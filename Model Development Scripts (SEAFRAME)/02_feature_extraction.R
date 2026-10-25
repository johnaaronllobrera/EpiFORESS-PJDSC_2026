# EpiFORESS Analysis Pipeline
# Script: 02_feature_extraction.R
# Project root: set EPIFORESS_PROJECT_DIR or run from the project data directory.
# Reproducibility: inspect all settings and verify input/model compatibility before reporting results.

# EpiFORESS: Supplementary Model-Development Script
# Feature extraction from disease-seeded simulated surveillance series
# Author: John Aaron B. Llobrera
# Purpose: Compute 49 candidate pre-forecasting features for model development.
#
# Reproducibility notes:
# - This script reads the simulation workbook and writes outputs to separate files.
# - Feature definitions are fixed here; any learned preprocessing belongs inside
#   the model resampling pipeline to prevent leakage.
# - Forecasting performance metrics are outcomes, not candidate predictors.
# - Review feature definitions and missingness summaries before model fitting.

# ============================================================
# 1. CONFIGURATION
# ============================================================

packages <- c("readxl", "dplyr", "purrr", "forecast", "openxlsx",
              "tibble", "TDA", "changepoint", "pracma")
missing_packages <- setdiff(packages, rownames(installed.packages()))
if (length(missing_packages)) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
invisible(lapply(packages, library, character.only = TRUE))

project_dir <- normalizePath(
  Sys.getenv("EPIFORESS_PROJECT_DIR", unset = getwd()),
  winslash = "/",
  mustWork = FALSE
)

if (!dir.exists(project_dir)) {
  stop("Project directory does not exist: ", project_dir, call. = FALSE)
}

workbook_path <- file.path(project_dir, "Disease_Seeded_MC_Simulation_REFINED.xlsx")
observed_sheet <- "Observed_Simulated_Data"
artifact_sheet <- "Artifact_Metadata"
season_frequency <- 52L
random_seed <- 2026L

# Separate outputs preserve the source simulation workbook.
output_dir <- dirname(workbook_path)
features_csv <- file.path(output_dir, "extracted_features.csv")
modeling_csv <- file.path(output_dir, "modeling_dataset.csv")
output_workbook <- file.path(output_dir, "EpiFORESS_feature_extraction_outputs.xlsx")

classifier_predictors <- c(
  # Distributional (7)
  "mean_cases", "dispersion_index", "coefficient_of_variation",
  "variance_cases", "skewness_cases", "kurtosis_cases", "gini_coefficient",
  # Temporal structure (9)
  "trend_strength", "trend_slope", "seasonality_strength",
  "lag1_autocorrelation", "lag2_autocorrelation", "lag4_autocorrelation",
  "lag52_autocorrelation", "acf_decay", "partial_acf_lag1",
  # Spectral and complexity (4)
  "spectral_entropy", "dominant_period", "sample_entropy", "persistence_entropy",
  # Data quality (9)
  "missingness_rate", "heaping_index", "repetition_index",
  "proportion_zero_counts", "longest_zero_run", "longest_nonzero_run",
  "intermittency", "run_length_cv", "zero_cluster_index",
  # Outbreak and structural (10)
  "outbreak_weeks", "outbreak_week_rate", "peak_to_mean_ratio",
  "level_shift_index", "change_point_count", "change_point_rate",
  "outbreak_dominance_index", "max_to_median_ratio", "upper_tail_weight",
  "seasonal_peak_timing_instability",
  # Signal quality and noise (6)
  "signal_to_noise_ratio", "stl_remainder_strength",
  "rolling_variance_instability", "rolling_mean_instability",
  "residual_acf_strength", "noise_to_signal_ratio",
  # Inter-annual stability (4)
  "yoy_mean_cv", "yoy_peak_cv", "yoy_variance_cv", "seasonal_subseries_cv"
)

# Fixed log1p transformation for nonnegative, right-skewed candidates.
# The transformation is applied once in this script. Do not repeat downstream.
skewed_features <- c(
  "mean_cases", "dispersion_index", "variance_cases", "peak_to_mean_ratio",
  "level_shift_index", "change_point_count", "outbreak_weeks",
  "longest_zero_run", "longest_nonzero_run", "max_to_median_ratio",
  "dominant_period", "rolling_variance_instability",
  "rolling_mean_instability", "signal_to_noise_ratio", "noise_to_signal_ratio"
)

if (!file.exists(workbook_path)) {
  stop("Input workbook not found: ", workbook_path, call. = FALSE)
}
if (length(classifier_predictors) != 49L) {
  stop("Expected 49 candidate predictors; found ", length(classifier_predictors), ".")
}

# ============================================================
# 2. GENERAL HELPERS
# ============================================================

finite_values <- function(x) {
  x <- as.numeric(x)
  x[is.finite(x)]
}

safe_var <- function(x) {
  x <- finite_values(x)
  if (length(x) < 2L) return(NA_real_)
  stats::var(x)
}

safe_sd <- function(x) {
  x <- finite_values(x)
  if (length(x) < 2L) return(NA_real_)
  stats::sd(x)
}

safe_cv <- function(x) {
  x <- finite_values(x)
  if (length(x) < 2L) return(NA_real_)
  m <- mean(x)
  s <- stats::sd(x)
  if (!is.finite(m) || !is.finite(s)) return(NA_real_)
  if (m == 0 && s == 0) return(0)
  if (m <= 0) return(NA_real_)
  s / m
}

first_unique_value <- function(x) {
  ux <- unique(x[!is.na(x)])
  if (!length(ux)) return(NA)
  if (length(ux) > 1L) warning("Multiple IDs found within a series group.")
  ux[[1L]]
}

# Run lengths are computed separately within contiguous finite segments.
# Missing/non-finite observations break runs and are never treated as zero.
run_lengths_by_state <- function(x, state_value) {
  x <- as.numeric(x)
  observed <- is.finite(x)
  if (!any(observed)) return(numeric(0))
  state_match <- if (identical(as.numeric(state_value), 0)) x == 0 else x > 0
  state <- ifelse(observed, state_match, NA)
  r <- rle(state)
  r$lengths[!is.na(r$values) & r$values]
}

longest_state_run <- function(x, state_value) {
  runs <- run_lengths_by_state(x, state_value)
  if (!length(runs)) return(0)
  max(runs)
}

# Fill missing values for algorithms requiring complete vectors. The original
# observed vector is retained for data-quality features.
interpolate_series <- function(x, frequency = season_frequency) {
  x <- as.numeric(x)
  if (sum(is.finite(x)) < 2L) return(NULL)
  # Treat all non-finite values as missing before interpolation.
  x[!is.finite(x)] <- NA_real_
  tryCatch(
    as.numeric(forecast::na.interp(stats::ts(x, frequency = frequency))),
    error = function(e) NULL
  )
}

# ============================================================
# 3. SPECTRAL AND COMPLEXITY FEATURES
# ============================================================

spectral_entropy_fe <- function(x) {
  x <- finite_values(x)
  if (length(x) < 10L) return(NA_real_)
  if (safe_sd(x) == 0) return(0)
  spec <- tryCatch(stats::spectrum(x, plot = FALSE)$spec,
                   error = function(e) NULL)
  if (is.null(spec)) return(NA_real_)
  spec <- spec[is.finite(spec) & spec >= 0]
  if (!length(spec) || sum(spec) <= 0) return(NA_real_)
  p <- spec / sum(spec)
  p <- p[p > 0]
  if (length(p) <= 1L) return(0)
  -sum(p * log(p)) / log(length(p))
}

dominant_period_fe <- function(x) {
  x <- finite_values(x)
  if (length(x) < 10L || safe_sd(x) == 0) return(NA_real_)
  sp <- tryCatch(stats::spectrum(x, plot = FALSE), error = function(e) NULL)
  if (is.null(sp) || !length(sp$freq)) return(NA_real_)
  positive <- which(is.finite(sp$freq) & sp$freq > 0 & is.finite(sp$spec))
  if (!length(positive)) return(NA_real_)
  idx <- positive[which.max(sp$spec[positive])]
  1 / sp$freq[idx]
}

sample_entropy_fe <- function(x, m = 2L, r_scale = 0.2) {
  x <- finite_values(x)
  if (length(x) < 20L) return(NA_real_)
  s <- safe_sd(x)
  if (!is.finite(s) || s == 0) return(0)
  xs <- as.numeric(scale(x))
  r <- r_scale * stats::sd(xs)
  out <- tryCatch(pracma::sample_entropy(xs, edim = m, r = r),
                  error = function(e) NA_real_)
  out <- as.numeric(out)
  if (length(out) != 1L || !is.finite(out)) NA_real_ else out
}

# Normalized entropy of positive persistence lifetimes from a Vietoris-Rips
# diagram. The diagram is truncated at the 30th percentile of pairwise
# embedding distances; interpret this feature as scale-dependent persistence
# entropy, not a general topological entropy measure.
persistence_entropy_fe <- function(x, m = 3L, tau = 1L, max_points = 120L) {
  x <- finite_values(x)
  if (length(x) < 50L) return(NA_real_)
  s <- safe_sd(x)
  if (!is.finite(s) || s == 0) return(0)
  xs <- as.numeric(scale(x))
  last_start <- length(xs) - (m - 1L) * tau
  if (last_start <= 5L) return(NA_real_)
  em <- do.call(cbind, lapply(seq_len(m), function(j) {
    start <- (j - 1L) * tau + 1L
    xs[start:(start + last_start - 1L)]
  }))
  if (nrow(em) > max_points) {
    keep <- unique(round(seq(1, nrow(em), length.out = max_points)))
    em <- em[keep, , drop = FALSE]
  }
  distances <- as.numeric(stats::dist(em))
  positive_distances <- distances[is.finite(distances) & distances > 0]
  if (!length(positive_distances)) return(0)
  max_scale <- as.numeric(stats::quantile(positive_distances, 0.30, na.rm = TRUE))
  if (!is.finite(max_scale) || max_scale <= 0) return(NA_real_)
  diag <- tryCatch(
    TDA::ripsDiag(X = em, maxdimension = 1, maxscale = max_scale,
                  dist = "euclidean", printProgress = FALSE)$diagram,
    error = function(e) NULL
  )
  if (is.null(diag) || !nrow(diag)) return(0)
  lifetimes <- diag[, 3] - diag[, 2]
  lifetimes <- lifetimes[is.finite(lifetimes) & lifetimes > 0]
  if (length(lifetimes) <= 1L) return(0)
  p <- lifetimes / sum(lifetimes)
  -sum(p * log(p)) / log(length(p))
}

# ============================================================
# 4. DISTRIBUTIONAL FEATURES
# ============================================================

gini_coefficient_fe <- function(x) {
  x <- finite_values(x)
  x <- x[x >= 0]
  if (!length(x)) return(NA_real_)
  if (sum(x) == 0) return(0)
  x <- sort(x); n <- length(x)
  (2 * sum(seq_len(n) * x) / (n * sum(x))) - ((n + 1) / n)
}

skewness_fe <- function(x) {
  x <- finite_values(x)
  if (length(x) < 3L) return(NA_real_)
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) return(NA_real_)
  mean(((x - mean(x)) / s)^3)
}

kurtosis_fe <- function(x) {
  x <- finite_values(x)
  if (length(x) < 4L) return(NA_real_)
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) return(NA_real_)
  mean(((x - mean(x)) / s)^4) - 3
}

# ============================================================
# 5. TEMPORAL AND SIGNAL FEATURES
# ============================================================

lag_acf_fe <- function(x, lag = 1L) {
  x <- finite_values(x)
  if (length(x) < lag + 2L || safe_sd(x) == 0) return(NA_real_)
  tryCatch(as.numeric(stats::acf(x, lag.max = lag, plot = FALSE)$acf[lag + 1L]),
           error = function(e) NA_real_)
}

partial_acf_lag1_fe <- function(x) {
  x <- finite_values(x)
  if (length(x) < 5L || safe_sd(x) == 0) return(NA_real_)
  tryCatch(as.numeric(stats::pacf(x, lag.max = 1L, plot = FALSE)$acf[1L]),
           error = function(e) NA_real_)
}

acf_decay_fe <- function(x, max_lag = 10L) {
  x <- finite_values(x)
  if (length(x) < 5L) return(NA_real_)
  if (safe_sd(x) == 0) return(0)
  max_lag <- min(max_lag, length(x) - 1L)
  av <- tryCatch(as.numeric(stats::acf(x, lag.max = max_lag, plot = FALSE)$acf)[-1L],
                 error = function(e) NULL)
  if (is.null(av)) return(NA_real_)
  lags <- seq_along(av)
  valid <- is.finite(av) & abs(av) > 1e-8
  if (sum(valid) < 2L) return(0)
  fit <- tryCatch(stats::lm(log(abs(av[valid])) ~ lags[valid]),
                  error = function(e) NULL)
  if (is.null(fit)) return(NA_real_)
  -as.numeric(stats::coef(fit)[2L])
}

get_signal_features <- function(x, freq = season_frequency) {
  na_out <- tibble::tibble(trend_strength = NA_real_, trend_slope = NA_real_,
                           seasonality_strength = NA_real_,
                           signal_to_noise_ratio = NA_real_)
  xf <- interpolate_series(x, freq)
  if (is.null(xf) || length(xf) < 20L) return(na_out)
  if (safe_sd(xf) == 0) {
    return(tibble::tibble(trend_strength = 0, trend_slope = 0,
                          seasonality_strength = 0, signal_to_noise_ratio = 0))
  }
  ti <- seq_along(xf)
  trend_fit <- tryCatch(stats::lm(xf ~ ti), error = function(e) NULL)
  trend_r2 <- if (is.null(trend_fit)) NA_real_ else summary(trend_fit)$r.squared
  trend_slope <- if (is.null(trend_fit)) NA_real_ else unname(stats::coef(trend_fit)[2L])
  if (freq > 1L && length(xf) >= 2L * freq) {
    seasonal_index <- factor(((seq_along(xf) - 1L) %% freq) + 1L)
    seasonal_fit <- tryCatch(stats::lm(xf ~ seasonal_index), error = function(e) NULL)
    full_fit <- tryCatch(stats::lm(xf ~ ti + seasonal_index), error = function(e) NULL)
    seasonal_r2 <- if (is.null(seasonal_fit)) NA_real_ else summary(seasonal_fit)$r.squared
  } else {
    full_fit <- trend_fit
    seasonal_r2 <- NA_real_
  }
  snr <- NA_real_
  if (!is.null(full_fit) && stats::df.residual(full_fit) > 0) {
    signal_var <- safe_var(stats::fitted(full_fit))
    residual_var <- safe_var(stats::residuals(full_fit))
    if (is.finite(signal_var) && is.finite(residual_var)) {
      snr <- if (residual_var > 0) signal_var / residual_var else NA_real_
    }
  }
  tibble::tibble(trend_strength = trend_r2, trend_slope = trend_slope,
                 seasonality_strength = seasonal_r2,
                 signal_to_noise_ratio = snr)
}

# ============================================================
# 6. DATA QUALITY AND RUN FEATURES
# ============================================================

heaping_index_fe <- function(x) {
  x <- as.numeric(x); x <- x[is.finite(x) & x > 0]
  if (!length(x)) return(NA_real_)
  rounded <- round(x)
  integer_like <- abs(x - rounded) < 1e-8
  mean(integer_like & rounded %% 5 == 0)
}

repetition_index_fe <- function(x) {
  x <- as.numeric(x)
  if (length(x) <= 1L) return(NA_real_)
  a <- x[-length(x)]; b <- x[-1L]
  ok <- is.finite(a) & is.finite(b)
  if (!any(ok)) return(NA_real_)
  mean(a[ok] == b[ok])
}

intermittency_fe <- function(x) {
  x <- as.numeric(x)
  if (length(x) <= 1L) return(NA_real_)
  a <- x[-length(x)]; b <- x[-1L]
  ok <- is.finite(a) & is.finite(b)
  if (!any(ok)) return(NA_real_)
  mean((a[ok] == 0) != (b[ok] == 0))
}

run_length_cv_fe <- function(x) {
  x <- as.numeric(x)
  if (!length(x)) return(NA_real_)
  observed <- is.finite(x)
  if (!any(observed)) return(NA_real_)
  # Missing values split segments. For each finite segment, collect both
  # zero-run and positive-run lengths, then summarize the combined lengths.
  segment_id <- cumsum(!observed)
  segment_lengths <- split(x[observed], segment_id[observed])
  runs <- unlist(lapply(segment_lengths, function(seg) rle(seg == 0)$lengths), use.names = FALSE)
  if (length(runs) < 2L) return(if (length(runs) == 1L) 0 else NA_real_)
  safe_cv(runs)
}

zero_cluster_index_fe <- function(x) {
  runs <- run_lengths_by_state(x, 0)
  if (!length(runs)) return(0)
  mean(runs)
}

# ============================================================
# 7. OUTBREAK AND STRUCTURAL FEATURES
# ============================================================

outbreak_threshold <- function(x) {
  x <- finite_values(x)
  if (!length(x)) return(NA_real_)
  threshold <- unname(stats::quantile(x, 0.75, na.rm = TRUE) + 1.5 * stats::IQR(x, na.rm = TRUE))
  if (!is.finite(threshold) || threshold <= 0) {
    threshold <- mean(x) + 2 * stats::sd(x)
  }
  if (!is.finite(threshold)) return(NA_real_)
  threshold
}

outbreak_weeks_fe <- function(x) {
  x <- finite_values(x)
  if (!length(x)) return(NA_real_)
  threshold <- outbreak_threshold(x)
  if (!is.finite(threshold)) return(0)
  sum(x > threshold)
}

outbreak_dominance_index_fe <- function(x) {
  x <- finite_values(x)
  if (!length(x) || sum(x) == 0) return(NA_real_)
  threshold <- outbreak_threshold(x)
  if (!is.finite(threshold)) return(0)
  sum(x[x > threshold]) / sum(x)
}

max_to_median_ratio_fe <- function(x) {
  xp <- finite_values(x); xp <- xp[xp > 0]
  if (!length(xp)) return(NA_real_)
  med <- stats::median(xp)
  if (!is.finite(med) || med <= 0) return(NA_real_)
  max(xp) / med
}

upper_tail_weight_fe <- function(x, p = 0.10) {
  x <- finite_values(x); x <- x[x >= 0]
  if (!length(x) || sum(x) == 0) return(NA_real_)
  k <- max(1L, ceiling(p * length(x)))
  sum(sort(x, decreasing = TRUE)[seq_len(k)]) / sum(x)
}

level_shift_fe <- function(x) {
  x <- finite_values(x)
  if (length(x) < 10L) return(NA_real_)
  if (safe_sd(x) == 0) return(0)
  w <- max(3L, min(26L, floor(length(x) / 10L)))
  rm <- as.numeric(stats::filter(x, rep(1 / w, w), sides = 1))
  diffs <- abs(diff(rm)); diffs <- diffs[is.finite(diffs)]
  if (!length(diffs)) return(0)
  max(diffs)
}

change_point_count_fe <- function(x) {
  x <- finite_values(x)
  if (length(x) < 20L || safe_sd(x) == 0) return(0)
  tryCatch({
    cp <- changepoint::cpt.mean(x, method = "PELT", penalty = "BIC", minseglen = 4L)
    length(changepoint::cpts(cp))
  }, error = function(e) NA_real_)
}

# Circular mean resultant length with zero-based week indexing. This makes the
# first and last seasonal weeks adjacent on the circle.
seasonal_peak_timing_instability_fe <- function(x, freq = season_frequency) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2L * freq) return(NA_real_)
  complete_n <- floor(n / freq) * freq
  x <- x[seq_len(complete_n)]
  year_idx <- (seq_along(x) - 1L) %/% freq
  years <- unique(year_idx)
  peak_weeks <- vapply(years, function(yr) {
    seg <- x[year_idx == yr]
    valid <- which(is.finite(seg))
    if (!length(valid)) return(NA_real_)
    # If ties occur, use the earliest peak week deterministically.
    valid[which.max(seg[valid])][1L] - 1L
  }, numeric(1))
  peak_weeks <- peak_weeks[is.finite(peak_weeks)]
  if (length(peak_weeks) < 2L) return(NA_real_)
  theta <- 2 * pi * peak_weeks / freq
  resultant <- sqrt(mean(cos(theta))^2 + mean(sin(theta))^2)
  max(0, min(1, 1 - resultant))
}

# ============================================================
# 8. SIGNAL QUALITY AND INTER-ANNUAL STABILITY
# ============================================================

stl_components <- function(x, freq = season_frequency) {
  xf <- interpolate_series(x, freq)
  if (is.null(xf) || length(xf) < 2L * freq || safe_sd(xf) == 0) return(NULL)
  tryCatch(stats::stl(stats::ts(xf, frequency = freq), s.window = "periodic", robust = TRUE),
           error = function(e) NULL)
}

stl_remainder_strength_fe <- function(x, freq = season_frequency) {
  fit <- stl_components(x, freq)
  if (is.null(fit)) return(NA_real_)
  remainder <- as.numeric(fit$time.series[, "remainder"])
  seasonal <- as.numeric(fit$time.series[, "seasonal"])
  vr <- safe_var(remainder); vrs <- safe_var(remainder + seasonal)
  if (!is.finite(vr) || !is.finite(vrs) || vrs <= 0) return(NA_real_)
  max(0, min(1, vr / vrs))
}

rolling_variance_instability_fe <- function(x, freq = season_frequency) {
  xf <- interpolate_series(x, freq)
  if (is.null(xf) || length(xf) < 2L * freq) return(NA_real_)
  if (safe_sd(xf) == 0) return(0)
  w <- freq
  vals <- vapply(seq_len(length(xf) - w + 1L), function(i) var(xf[i:(i + w - 1L)]), numeric(1))
  vals <- vals[is.finite(vals) & vals >= 0]
  if (length(vals) < 2L) return(NA_real_)
  m <- mean(vals)
  if (m == 0) return(0)
  stats::sd(vals) / m
}

rolling_mean_instability_fe <- function(x, freq = season_frequency) {
  xf <- interpolate_series(x, freq)
  if (is.null(xf) || length(xf) < 2L * freq) return(NA_real_)
  if (safe_sd(xf) == 0) return(0)
  w <- freq
  vals <- vapply(seq_len(length(xf) - w + 1L), function(i) mean(xf[i:(i + w - 1L)]), numeric(1))
  if (length(vals) < 2L) return(NA_real_)
  m <- mean(vals)
  if (!is.finite(m) || m <= 0) return(NA_real_)
  stats::sd(vals) / m
}

residual_acf_strength_fe <- function(x, freq = season_frequency) {
  fit <- stl_components(x, freq)
  if (is.null(fit)) return(NA_real_)
  residual <- as.numeric(fit$time.series[, "remainder"])
  if (safe_sd(residual) == 0) return(0)
  av <- tryCatch(as.numeric(stats::acf(residual, lag.max = 4L, plot = FALSE)$acf)[-1L],
                 error = function(e) NULL)
  if (is.null(av) || !length(av)) return(NA_real_)
  mean(abs(av), na.rm = TRUE)
}

noise_to_signal_ratio_fe <- function(snr) {
  if (length(snr) != 1L || !is.finite(snr) || snr <= 0) return(NA_real_)
  1 / snr
}

complete_year_series <- function(x, freq = season_frequency) {
  x <- as.numeric(x)
  complete_n <- floor(length(x) / freq) * freq
  if (complete_n < 2L * freq) return(NULL)
  x[seq_len(complete_n)]
}

annual_values <- function(x, freq = season_frequency, FUN) {
  x <- complete_year_series(x, freq)
  if (is.null(x)) return(numeric(0))
  year_idx <- (seq_along(x) - 1L) %/% freq
  vapply(unique(year_idx), function(yr) {
    seg <- x[year_idx == yr]
    valid <- is.finite(seg)
    # Avoid treating a year with very few observations as representative.
    if (sum(valid) < ceiling(0.8 * freq)) return(NA_real_)
    FUN(seg[valid])
  }, numeric(1))
}

yoy_mean_cv_fe <- function(x, freq = season_frequency) {
  vals <- annual_values(x, freq, mean); vals <- vals[is.finite(vals)]
  if (length(vals) < 2L) return(NA_real_)
  safe_cv(vals)
}

yoy_peak_cv_fe <- function(x, freq = season_frequency) {
  vals <- annual_values(x, freq, max); vals <- vals[is.finite(vals)]
  if (length(vals) < 2L) return(NA_real_)
  safe_cv(vals)
}

yoy_variance_cv_fe <- function(x, freq = season_frequency) {
  vals <- annual_values(x, freq, safe_var); vals <- vals[is.finite(vals)]
  if (length(vals) < 2L) return(NA_real_)
  safe_cv(vals)
}

seasonal_subseries_cv_fe <- function(x, freq = season_frequency) {
  x <- complete_year_series(x, freq)
  if (is.null(x)) return(NA_real_)
  week <- ((seq_along(x) - 1L) %% freq) + 1L
  cvs <- vapply(seq_len(freq), function(w) {
    vals <- x[week == w]
    vals <- vals[is.finite(vals)]
    if (length(vals) < 2L) return(NA_real_)
    safe_cv(vals)
  }, numeric(1))
  cvs <- cvs[is.finite(cvs)]
  if (!length(cvs)) NA_real_ else mean(cvs)
}

# ============================================================
# 9. FEATURE EXTRACTION FOR ONE SERIES
# ============================================================

extract_features_one_series <- function(data) {
  data <- dplyr::arrange(data, Time)
  x <- as.numeric(data$Observed_Cases)
  x[!is.finite(x)] <- NA_real_
  xn <- finite_values(x)
  if (!length(xn)) stop("A simulation series contains no finite observed counts.")

  mc <- mean(xn); vc <- safe_var(xn); cv <- safe_cv(xn)
  di <- if (is.finite(mc) && mc > 0) vc / mc else if (mc == 0 && is.finite(vc) && vc == 0) 0 else NA_real_
  xf <- interpolate_series(x, season_frequency)
  xt <- if (is.null(xf)) xn else xf
  sf <- get_signal_features(x, season_frequency)
  snr <- sf$signal_to_noise_ratio

  outbreak_count <- outbreak_weeks_fe(xn)
  cpc <- change_point_count_fe(xt)
  tibble::tibble(
    Global_Simulation_ID = first_unique_value(data$Global_Simulation_ID),
    Disease_Seed = as.character(first_unique_value(data$Disease_Seed)),
    Disease_Simulation_ID = first_unique_value(data$Disease_Simulation_ID),
    mean_cases = mc,
    dispersion_index = di,
    coefficient_of_variation = cv,
    variance_cases = vc,
    skewness_cases = skewness_fe(xn),
    kurtosis_cases = kurtosis_fe(xn),
    gini_coefficient = gini_coefficient_fe(xn),
    trend_strength = sf$trend_strength,
    trend_slope = sf$trend_slope,
    seasonality_strength = sf$seasonality_strength,
    lag1_autocorrelation = lag_acf_fe(xt, 1L),
    lag2_autocorrelation = lag_acf_fe(xt, 2L),
    lag4_autocorrelation = lag_acf_fe(xt, 4L),
    lag52_autocorrelation = lag_acf_fe(xt, season_frequency),
    acf_decay = acf_decay_fe(xt),
    partial_acf_lag1 = partial_acf_lag1_fe(xt),
    spectral_entropy = spectral_entropy_fe(xt),
    dominant_period = dominant_period_fe(xt),
    sample_entropy = sample_entropy_fe(xt),
    persistence_entropy = persistence_entropy_fe(xt),
    # Count all non-finite observations as unavailable in this quality feature.
    missingness_rate = mean(!is.finite(as.numeric(data$Observed_Cases))),
    heaping_index = heaping_index_fe(x),
    repetition_index = repetition_index_fe(x),
    proportion_zero_counts = mean(xn == 0),
    longest_zero_run = longest_state_run(x, 0),
    longest_nonzero_run = longest_state_run(x, 1),
    intermittency = intermittency_fe(x),
    run_length_cv = run_length_cv_fe(x),
    zero_cluster_index = zero_cluster_index_fe(x),
    outbreak_weeks = outbreak_count,
    outbreak_week_rate = if (is.finite(outbreak_count)) outbreak_count / length(xn) else NA_real_,
    peak_to_mean_ratio = if (mc > 0) max(xn) / mc else NA_real_,
    level_shift_index = level_shift_fe(xt),
    change_point_count = cpc,
    change_point_rate = if (is.finite(cpc)) cpc / length(xt) else NA_real_,
    outbreak_dominance_index = outbreak_dominance_index_fe(xn),
    max_to_median_ratio = max_to_median_ratio_fe(xn),
    upper_tail_weight = upper_tail_weight_fe(xn),
    seasonal_peak_timing_instability = seasonal_peak_timing_instability_fe(x, season_frequency),
    signal_to_noise_ratio = snr,
    stl_remainder_strength = stl_remainder_strength_fe(x, season_frequency),
    rolling_variance_instability = rolling_variance_instability_fe(x, season_frequency),
    rolling_mean_instability = rolling_mean_instability_fe(x, season_frequency),
    residual_acf_strength = residual_acf_strength_fe(x, season_frequency),
    noise_to_signal_ratio = noise_to_signal_ratio_fe(snr),
    yoy_mean_cv = yoy_mean_cv_fe(x, season_frequency),
    yoy_peak_cv = yoy_peak_cv_fe(x, season_frequency),
    yoy_variance_cv = yoy_variance_cv_fe(x, season_frequency),
    seasonal_subseries_cv = seasonal_subseries_cv_fe(x, season_frequency),
    n_total = length(x),
    n_observed = length(xn)
  )
}

# ============================================================
# 10. LOAD AND VALIDATE INPUTS
# ============================================================

observed <- readxl::read_excel(workbook_path, sheet = observed_sheet) %>%
  dplyr::mutate(
    Global_Simulation_ID = as.integer(Global_Simulation_ID),
    Disease_Seed = as.character(Disease_Seed),
    Disease_Simulation_ID = as.integer(Disease_Simulation_ID),
    Time = as.integer(Time),
    Week = as.integer(Week),
    Expected_Mean = as.numeric(Expected_Mean),
    Clean_Cases = as.numeric(Clean_Cases),
    Observed_Cases = as.numeric(Observed_Cases)
  )

required_observed <- c("Global_Simulation_ID", "Disease_Seed", "Disease_Simulation_ID", "Time", "Observed_Cases")
missing_observed <- setdiff(required_observed, names(observed))
if (length(missing_observed)) stop("Missing columns in observed data: ", paste(missing_observed, collapse = ", "))
if (!nrow(observed)) stop("Observed simulated data sheet is empty.")
if (anyNA(observed$Global_Simulation_ID) || anyNA(observed$Time)) stop("Simulation IDs and Time must not be missing.")
if (anyDuplicated(observed[c("Global_Simulation_ID", "Time")])) stop("Duplicate Global_Simulation_ID-Time rows found.")

series_key_check <- observed %>%
  dplyr::group_by(Global_Simulation_ID) %>%
  dplyr::summarise(
    n_disease = dplyr::n_distinct(Disease_Seed),
    n_local_id = dplyr::n_distinct(Disease_Simulation_ID),
    .groups = "drop"
  )
if (any(series_key_check$n_disease != 1L | series_key_check$n_local_id != 1L)) {
  stop("A Global_Simulation_ID maps to multiple disease or disease-simulation IDs.")
}

cat("\nExtracting features from", dplyr::n_distinct(observed$Global_Simulation_ID), "series...\n")
cat("Candidate predictor count:", length(classifier_predictors), "\n")

extracted_features <- observed %>%
  dplyr::group_by(Global_Simulation_ID) %>%
  dplyr::group_split(.keep = TRUE) %>%
  purrr::map_dfr(extract_features_one_series, .progress = TRUE)

if (anyDuplicated(extracted_features$Global_Simulation_ID)) {
  stop("Feature extraction produced duplicate simulation IDs.")
}
missing_predictors <- setdiff(classifier_predictors, names(extracted_features))
if (length(missing_predictors)) stop("Missing predictors: ", paste(missing_predictors, collapse = ", "))

# Convert non-finite feature results to NA, but preserve IDs and informational fields.
extracted_features <- extracted_features %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(classifier_predictors),
                             ~ ifelse(is.finite(.x), .x, NA_real_)))

# ============================================================
# 11. VALIDATE AND JOIN ARTIFACT METADATA
# ============================================================

artifact_metadata <- readxl::read_excel(workbook_path, sheet = artifact_sheet) %>%
  dplyr::mutate(
    Global_Simulation_ID = as.integer(Global_Simulation_ID),
    Disease_Seed = as.character(Disease_Seed),
    Disease_Simulation_ID = as.integer(Disease_Simulation_ID)
  )

join_keys <- c("Global_Simulation_ID", "Disease_Seed", "Disease_Simulation_ID")
required_artifact <- c(join_keys, "Artifact_Count", "Artifact_Burden_Score", "Artifact_Burden_Class")
missing_artifact <- setdiff(required_artifact, names(artifact_metadata))
if (length(missing_artifact)) {
  stop("Missing required artifact metadata columns: ", paste(missing_artifact, collapse = ", "))
}
if (anyDuplicated(artifact_metadata[join_keys])) {
  stop("Artifact metadata contains duplicate simulation keys; join would duplicate rows.")
}

unmatched_ids <- dplyr::anti_join(
  extracted_features %>% dplyr::select(dplyr::all_of(join_keys)),
  artifact_metadata %>% dplyr::select(dplyr::all_of(join_keys)),
  by = join_keys
)
if (nrow(unmatched_ids)) {
  stop(nrow(unmatched_ids), " extracted series have no matching artifact metadata record.")
}

modeling_dataset <- extracted_features %>%
  dplyr::left_join(artifact_metadata, by = join_keys)
if (nrow(modeling_dataset) != nrow(extracted_features)) {
  stop("Metadata join changed the number of feature rows.")
}

# ============================================================
# 12. TRANSFORM AND REPORT
# ============================================================

bad_log_features <- intersect(skewed_features, classifier_predictors)
bad_log_features <- bad_log_features[vapply(bad_log_features, function(nm) {
  any(is.finite(extracted_features[[nm]]) & extracted_features[[nm]] < 0)
}, logical(1))]
if (length(bad_log_features)) {
  stop("Cannot apply log1p to negative values in: ", paste(bad_log_features, collapse = ", "))
}

skewed_present <- intersect(skewed_features, names(extracted_features))
extracted_features <- extracted_features %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(skewed_present), log1p))

modeling_dataset <- extracted_features %>%
  dplyr::left_join(artifact_metadata, by = join_keys)

cat("\nFeature extraction summary\n")
cat("Series extracted:", nrow(extracted_features), "\n")
cat("Rows in modeling dataset:", nrow(modeling_dataset), "\n")
cat("Missing values by predictor (after transformation):\n")
print(colSums(is.na(extracted_features[, classifier_predictors, drop = FALSE])))
cat("\nArtifact burden distribution:\n")
print(table(modeling_dataset$Artifact_Burden_Class, useNA = "ifany"))

# ============================================================
# 13. SAVE OUTPUTS WITHOUT OVERWRITING THE INPUT WORKBOOK
# ============================================================

utils::write.csv(extracted_features, features_csv, row.names = FALSE, na = "")
utils::write.csv(modeling_dataset, modeling_csv, row.names = FALSE, na = "")

out_wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(out_wb, "Extracted_Features")
openxlsx::addWorksheet(out_wb, "Modeling_Dataset")
openxlsx::addWorksheet(out_wb, "Feature_Dictionary")
openxlsx::writeDataTable(out_wb, "Extracted_Features", extracted_features, withFilter = TRUE)
openxlsx::writeDataTable(out_wb, "Modeling_Dataset", modeling_dataset, withFilter = TRUE)
feature_dictionary <- tibble::tibble(
  predictor = classifier_predictors,
  domain = c(rep("Distributional Characteristics", 7),
             rep("Temporal Structure", 9),
             rep("Spectral and Complexity Measures", 4),
             rep("Data Quality Characteristics", 9),
             rep("Outbreak and Structural Characteristics", 10),
             rep("Signal Quality and Noise Characteristics", 6),
             rep("Inter-Annual Stability Characteristics", 4)),
  log1p_transformed = classifier_predictors %in% skewed_present,
  note = "See feature definitions and comments in R/03_feature_extraction.R"
)
openxlsx::writeDataTable(out_wb, "Feature_Dictionary", feature_dictionary, withFilter = TRUE)
openxlsx::freezePane(out_wb, "Extracted_Features", firstRow = TRUE)
openxlsx::freezePane(out_wb, "Modeling_Dataset", firstRow = TRUE)
openxlsx::freezePane(out_wb, "Feature_Dictionary", firstRow = TRUE)
openxlsx::saveWorkbook(out_wb, output_workbook, overwrite = TRUE)

cat("\n================================================\n")
cat("EPIFORESS FEATURE EXTRACTION COMPLETE\n")
cat("================================================\n")
cat("Series processed:", nrow(extracted_features), "\n")
cat("Candidate predictors:", length(classifier_predictors), "\n")
cat("Output CSV:", features_csv, "\n")
cat("Modeling CSV:", modeling_csv, "\n")
cat("Output workbook:", output_workbook, "\n")
cat("\nImportant: fixed log1p transformations have already been applied to the listed features.\n")
cat("Fit any data-dependent imputation, scaling, or feature selection within resampling folds.\n")
cat("================================================\n")
