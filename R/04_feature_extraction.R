# EpiFORESS | 04_feature_extraction.R
# Feature engineering and model-aligned feature transformations.
# Split from app_full_fixed.R; code logic preserved during file separation.

# 5. EMPIRICAL-VALIDATION-ALIGNED 49-FEATURE EXTRACTION
# ------------------------------------------------------------
skewness_fe <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 3) return(NA_real_)
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) return(0)
  mean(((x - mean(x)) / s)^3)
}
kurtosis_fe <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 4) return(NA_real_)
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) return(0)
  mean(((x - mean(x)) / s)^4) - 3
}
gini_fe <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  if (any(x < 0)) x <- x - min(x)
  if (sum(x) == 0) return(0)
  x <- sort(x)
  n <- length(x)
  sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
}
spectral_entropy_fe <- function(x) {
  x <- as.numeric(x[is.finite(x)])
  if (length(x) < 10) return(NA_real_)
  if (stats::sd(x) == 0) return(0)
  spec <- tryCatch(stats::spectrum(x, plot = FALSE)$spec, error = function(e) NULL)
  if (is.null(spec)) return(NA_real_)
  spec <- spec[is.finite(spec) & spec >= 0]
  if (length(spec) == 0 || sum(spec) <= 0) return(NA_real_)
  p <- spec / sum(spec)
  p <- p[p > 0]
  if (length(p) <= 1) return(0)
  -sum(p * log(p)) / log(length(p))
}
dominant_period_fe <- function(x, frequency = 52) {
  x <- as.numeric(x[is.finite(x)])
  if (length(x) < 10 || stats::sd(x) == 0) return(NA_real_)
  sp <- tryCatch(stats::spectrum(x, plot = FALSE), error = function(e) NULL)
  if (is.null(sp)) return(NA_real_)
  spec <- as.numeric(sp$spec)
  freq <- as.numeric(sp$freq)
  ok <- is.finite(spec) & is.finite(freq) & freq > 0
  if (sum(ok) == 0) return(NA_real_)
  dom_freq <- freq[ok][which.max(spec[ok])]
  if (!is.finite(dom_freq) || dom_freq <= 0) return(NA_real_)
  1 / dom_freq
}
sample_entropy_fe <- function(x, m = 2, r = NULL) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < m + 10) return(NA_real_)
  sx <- stats::sd(x)
  if (!is.finite(sx) || sx == 0) return(0)
  if (is.null(r)) r <- 0.2 * sx
  count_matches <- function(mm) {
    if (n <= mm) return(0)
    templates <- embed(x, mm)
    N <- nrow(templates)
    count <- 0
    if (N <= 1) return(0)
    for (i in seq_len(N - 1)) {
      dist_i <- apply(
        templates[(i + 1):N, , drop = FALSE],
        1,
        function(row) max(abs(row - templates[i, ]))
      )
      count <- count + sum(dist_i <= r, na.rm = TRUE)
    }
    count
  }
  B <- count_matches(m)
  A <- count_matches(m + 1)
  if (B == 0 || A == 0) return(NA_real_)
  -log(A / B)
}
heaping_index_fe <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x) & x > 0]
  if (length(x) == 0) return(NA_real_)
  mean(x %% 5 == 0)
}
repetition_index_fe <- function(x) {
  x <- as.numeric(x)
  if (length(x) <= 1) return(NA_real_)
  prev <- x[-length(x)]
  curr <- x[-1]
  ok <- !is.na(prev) & !is.na(curr)
  if (sum(ok) == 0) return(NA_real_)
  mean(curr[ok] == prev[ok])
}
intermittency_index_fe <- function(x) {
  x <- as.numeric(x[!is.na(x)])
  if (length(x) <= 1) return(NA_real_)
  mean(abs(diff(as.numeric(x == 0))) == 1)
}
longest_run_fe <- function(x, condition_fun) {
  x <- as.numeric(x[!is.na(x)])
  if (length(x) == 0) return(NA_real_)
  flag <- condition_fun(x)
  runs <- rle(flag)
  if (!any(runs$values)) return(0)
  max(runs$lengths[runs$values])
}
run_length_cv_fe <- function(x) {
  x <- as.numeric(x[!is.na(x)])
  if (length(x) == 0) return(NA_real_)
  runs <- rle(x == 0)$lengths
  if (length(runs) <= 1) return(0)
  safe_cv(runs)
}
zero_cluster_index_fe <- function(x) {
  x <- as.numeric(x[!is.na(x)])
  if (length(x) == 0) return(NA_real_)
  rz <- rle(x == 0)
  zero_runs <- rz$lengths[rz$values]
  if (length(zero_runs) == 0) return(0)
  mean(zero_runs^2) / mean(zero_runs)
}
outbreak_weeks_fe <- function(x) {
  x <- as.numeric(x[!is.na(x)])
  if (length(x) == 0) return(NA_real_)
  q3 <- safe_quantile(x, 0.75)
  iq <- stats::IQR(x, na.rm = TRUE)
  thr <- q3 + 1.5 * iq
  if (!is.finite(thr) || thr <= 0) {
    thr <- mean(x, na.rm = TRUE) + 2 * stats::sd(x, na.rm = TRUE)
  }
  if (!is.finite(thr)) return(0)
  sum(x > thr, na.rm = TRUE)
}
outbreak_dominance_index_fe <- function(x) {
  x <- as.numeric(x[!is.na(x)])
  if (length(x) == 0 || sum(x) <= 0) return(0)
  q90 <- safe_quantile(x, 0.90)
  if (!is.finite(q90)) return(0)
  sum(x[x >= q90], na.rm = TRUE) / sum(x, na.rm = TRUE)
}
upper_tail_weight_fe <- function(x) {
  outbreak_dominance_index_fe(x)
}
acf_at_lag_fe <- function(x, lag = 1) {
  x <- as.numeric(x[is.finite(x)])
  if (length(x) <= lag || stats::sd(x) == 0) return(0)
  out <- tryCatch(
    as.numeric(stats::acf(x, lag.max = lag, plot = FALSE)$acf[lag + 1]),
    error = function(e) NA_real_
  )
  ifelse(is.finite(out), out, NA_real_)
}
pacf_lag1_fe <- function(x) {
  x <- as.numeric(x[is.finite(x)])
  if (length(x) < 5 || stats::sd(x) == 0) return(0)
  out <- tryCatch(
    as.numeric(stats::pacf(x, lag.max = 1, plot = FALSE)$acf[1]),
    error = function(e) NA_real_
  )
  ifelse(is.finite(out), out, NA_real_)
}
acf_decay_fe <- function(x, max_lag = 10) {
  x <- as.numeric(x[is.finite(x)])
  if (length(x) < 5 || stats::sd(x) == 0) return(0)
  max_lag <- min(max_lag, length(x) - 1)
  av <- tryCatch(
    as.numeric(stats::acf(x, lag.max = max_lag, plot = FALSE)$acf)[-1],
    error = function(e) NULL
  )
  if (is.null(av)) return(NA_real_)
  lags <- seq_along(av)
  valid <- is.finite(av) & abs(av) > 1e-8
  if (sum(valid) < 2) return(0)
  fit <- tryCatch(stats::lm(log(abs(av[valid])) ~ lags[valid]), error = function(e) NULL)
  if (is.null(fit)) return(NA_real_)
  -as.numeric(stats::coef(fit)[2])
}
level_shift_fe <- function(x) {
  x <- as.numeric(x[is.finite(x)])
  if (length(x) < 10 || stats::sd(x) == 0) return(0)
  w <- max(3, min(26, floor(length(x) / 10)))
  rm <- as.numeric(stats::filter(x, rep(1 / w, w), sides = 1))
  diffs <- abs(diff(rm))
  diffs <- diffs[is.finite(diffs)]
  if (length(diffs) == 0) return(0)
  max(diffs, na.rm = TRUE)
}
change_point_count_fe <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 20 || stats::sd(x) == 0) return(0)
  if (has_changepoint) {
    out <- tryCatch({
      cp <- changepoint::cpt.meanvar(
        x,
        method = "PELT",
        penalty = "BIC",
        minseglen = 4,
        class = TRUE
      )
      length(changepoint::cpts(cp))
    }, error = function(e) NA_real_)
    if (is.finite(out)) return(out)
  }
  w <- max(4, min(13, floor(length(x) / 10)))
  rm <- as.numeric(stats::filter(x, rep(1 / w, w), sides = 1))
  d <- abs(diff(rm))
  d <- d[is.finite(d)]
  if (length(d) == 0) return(0)
  thr <- mean(d, na.rm = TRUE) + stats::sd(d, na.rm = TRUE)
  if (!is.finite(thr)) return(0)
  sum(d > thr, na.rm = TRUE)
}
rolling_stat_instability_fe <- function(x, window = 13, stat = c("mean", "variance")) {
  stat <- match.arg(stat)
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 2 * window) return(NA_real_)
  vals <- purrr::map_dbl(
    seq_len(length(x) - window + 1),
    function(i) {
      z <- x[i:(i + window - 1)]
      if (stat == "mean") mean(z, na.rm = TRUE) else stats::var(z, na.rm = TRUE)
    }
  )
  vals <- vals[is.finite(vals)]
  if (length(vals) <= 1) return(NA_real_)
  safe_cv(vals)
}
seasonal_peak_timing_instability_fe <- function(x, frequency = 52) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 2 * frequency) return(NA_real_)
  n_years <- floor(length(x) / frequency)
  mat <- matrix(x[seq_len(n_years * frequency)], ncol = frequency, byrow = TRUE)
  peak_weeks <- apply(mat, 1, which.max)
  if (length(unique(peak_weeks)) <= 1) return(0)
  stats::sd(peak_weeks, na.rm = TRUE) / frequency
}
seasonal_subseries_cv_fe <- function(x, frequency = 52) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 2 * frequency) return(NA_real_)
  n_years <- floor(length(x) / frequency)
  mat <- matrix(x[seq_len(n_years * frequency)], ncol = frequency, byrow = TRUE)
  cv_by_week <- apply(mat, 2, safe_cv)
  if (all(!is.finite(cv_by_week))) return(NA_real_)
  mean(cv_by_week, na.rm = TRUE)
}
yoy_features_fe <- function(x, frequency = 52) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  empty <- tibble::tibble(
    yoy_mean_cv = NA_real_,
    yoy_peak_cv = NA_real_,
    yoy_variance_cv = NA_real_
  )
  if (length(x) < 2 * frequency) return(empty)
  n_years <- floor(length(x) / frequency)
  mat <- matrix(x[seq_len(n_years * frequency)], ncol = frequency, byrow = TRUE)
  yearly_mean <- rowMeans(mat, na.rm = TRUE)
  yearly_peak <- apply(mat, 1, max, na.rm = TRUE)
  yearly_var <- apply(mat, 1, stats::var, na.rm = TRUE)
  tibble::tibble(
    yoy_mean_cv = safe_cv(yearly_mean),
    yoy_peak_cv = safe_cv(yearly_peak),
    yoy_variance_cv = safe_cv(yearly_var)
  )
}
get_signal_features_fe <- function(x, frequency = 52) {
  x <- as.numeric(x)
  out <- tibble::tibble(
    trend_strength = NA_real_,
    trend_slope = NA_real_,
    seasonality_strength = NA_real_,
    signal_to_noise_ratio = NA_real_,
    stl_remainder_strength = NA_real_,
    residual_acf_strength = NA_real_,
    noise_to_signal_ratio = NA_real_
  )
  if (sum(!is.na(x)) < 20) return(out)
  xf <- tryCatch(
    as.numeric(forecast::na.interp(stats::ts(x, frequency = frequency))),
    error = function(e) NULL
  )
  if (is.null(xf)) return(out)
  if (length(xf) < 2 || stats::sd(xf, na.rm = TRUE) == 0) {
    return(tibble::tibble(
      trend_strength = 0,
      trend_slope = 0,
      seasonality_strength = 0,
      signal_to_noise_ratio = 0,
      stl_remainder_strength = 0,
      residual_acf_strength = 0,
      noise_to_signal_ratio = 0
    ))
  }
  n <- length(xf)
  ti <- seq_len(n)
  tf <- tryCatch(stats::lm(xf ~ ti), error = function(e) NULL)
  trend_strength <- if (!is.null(tf)) summary(tf)$r.squared else NA_real_
  trend_slope <- if (!is.null(tf)) as.numeric(stats::coef(tf)[2]) else NA_real_
  if (frequency > 1 && n >= 2 * frequency) {
    si <- factor(rep(seq_len(frequency), length.out = n))
    sf <- tryCatch(stats::lm(xf ~ si), error = function(e) NULL)
    ff <- tryCatch(stats::lm(xf ~ ti + si), error = function(e) NULL)
    seasonality_strength <- if (!is.null(sf)) summary(sf)$r.squared else NA_real_
  } else {
    ff <- tf
    seasonality_strength <- NA_real_
  }
  signal_to_noise_ratio <- if (!is.null(ff) && stats::df.residual(ff) > 0) {
    sv <- safe_var(stats::fitted(ff))
    nv <- safe_var(stats::residuals(ff))
    if (!is.na(nv) && nv > 0) sv / nv else NA_real_
  } else {
    NA_real_
  }
  noise_to_signal_ratio <- if (!is.na(signal_to_noise_ratio) && signal_to_noise_ratio > 0) {
    1 / signal_to_noise_ratio
  } else {
    NA_real_
  }
  stl_remainder_strength <- NA_real_
  residual_acf_strength <- NA_real_
  if (frequency > 1 && n >= 2 * frequency) {
    stl_obj <- tryCatch(
      stats::stl(
        stats::ts(xf, frequency = frequency),
        s.window = "periodic",
        robust = TRUE
      ),
      error = function(e) NULL
    )
    if (!is.null(stl_obj)) {
      comps <- stl_obj$time.series
      rem <- as.numeric(comps[, "remainder"])
      seas <- as.numeric(comps[, "seasonal"])
      tr <- as.numeric(comps[, "trend"])
      total_var <- safe_var(xf)
      rem_var <- safe_var(rem)
      signal_var <- safe_var(seas + tr)
      stl_remainder_strength <- if (!is.na(total_var) && total_var > 0) rem_var / total_var else NA_real_
      residual_acf_strength <- acf_at_lag_fe(rem, lag = 1)
      if (is.na(signal_to_noise_ratio) && !is.na(rem_var) && rem_var > 0) {
        signal_to_noise_ratio <- signal_var / rem_var
        noise_to_signal_ratio <- if (!is.na(signal_var) && signal_var > 0) rem_var / signal_var else NA_real_
      }
    }
  }
  tibble::tibble(
    trend_strength = trend_strength,
    trend_slope = trend_slope,
    seasonality_strength = seasonality_strength,
    signal_to_noise_ratio = signal_to_noise_ratio,
    stl_remainder_strength = stl_remainder_strength,
    residual_acf_strength = residual_acf_strength,
    noise_to_signal_ratio = noise_to_signal_ratio
  )
}
make_delay_embedding <- function(x, m = 3, tau = 1) {
  x <- as.numeric(x)
  n <- length(x)
  last_start <- n - (m - 1) * tau
  if (last_start <= 5) return(NULL)
  do.call(
    cbind,
    lapply(seq_len(m), function(j) {
      x[((j - 1) * tau + 1):((j - 1) * tau + last_start)]
    })
  )
}
persistence_entropy_fe <- function(x, m = 3, tau = 1, max_points = 120) {
  if (!has_TDA) return(NA_real_)
  x <- as.numeric(x[is.finite(x)])
  if (length(x) < 50) return(NA_real_)
  if (stats::sd(x) == 0) return(0)
  xs <- as.numeric(scale(x))
  if (any(is.na(xs))) return(NA_real_)
  em <- make_delay_embedding(xs, m = m, tau = tau)
  if (is.null(em) || nrow(em) < 10) return(NA_real_)
  if (nrow(em) > max_points) {
    keep <- unique(round(seq(1, nrow(em), length.out = max_points)))
    em <- em[keep, , drop = FALSE]
  }
  pd <- stats::dist(em)
  ms <- tryCatch(stats::quantile(pd, 0.30, na.rm = TRUE), error = function(e) NA_real_)
  if (is.na(ms) || ms <= 0) return(NA_real_)
  diag <- tryCatch(
    TDA::ripsDiag(
      X = em,
      maxdimension = 1,
      maxscale = as.numeric(ms),
      dist = "euclidean",
      printProgress = FALSE
    )$diagram,
    error = function(e) NULL
  )
  if (is.null(diag) || nrow(diag) == 0) return(0)
  lt <- diag[, 3] - diag[, 2]
  lt <- lt[is.finite(lt) & lt > 0]
  if (length(lt) == 0) return(0)
  p <- lt / sum(lt)
  p <- p[p > 0]
  if (length(p) <= 1) return(0)
  -sum(p * log(p)) / log(length(p))
}
compute_raw_features_display <- function(y, frequency = 52, compute_persistence = TRUE) {
  y_raw <- as.numeric(y)
  frequency <- max(1L, as.integer(round(frequency)))
  yn <- y_raw[!is.na(y_raw)]
  if (length(yn) == 0) stop("No usable numeric count values found.")
  xf <- tryCatch(
    as.numeric(forecast::na.interp(stats::ts(y_raw, frequency = frequency))),
    error = function(e) y_raw
  )
  xf <- as.numeric(xf)
  mean_cases <- mean(yn, na.rm = TRUE)
  variance_cases <- safe_var(yn)
  n_obs <- length(yn)
  sig <- get_signal_features_fe(y_raw, frequency = frequency)
  yoy <- yoy_features_fe(y_raw, frequency = frequency)
  outbreak_weeks_val <- outbreak_weeks_fe(y_raw)
  change_point_count_val <- change_point_count_fe(xf)
  persistence_entropy_val <- if (isTRUE(compute_persistence)) {
    persistence_entropy_fe(xf)
  } else {
    NA_real_
  }
  feats_raw <- tibble::tibble(
    mean_cases = mean_cases,
    dispersion_index = if (!is.na(mean_cases) && mean_cases > 0) variance_cases / mean_cases else NA_real_,
    coefficient_of_variation = if (!is.na(mean_cases) && mean_cases > 0) stats::sd(yn, na.rm = TRUE) / mean_cases else NA_real_,
    variance_cases = variance_cases,
    skewness_cases = skewness_fe(yn),
    kurtosis_cases = kurtosis_fe(yn),
    gini_coefficient = gini_fe(yn),
    trend_strength = sig$trend_strength,
    trend_slope = sig$trend_slope,
    seasonality_strength = sig$seasonality_strength,
    lag1_autocorrelation = acf_at_lag_fe(xf, 1),
    lag2_autocorrelation = acf_at_lag_fe(xf, 2),
    lag4_autocorrelation = acf_at_lag_fe(xf, 4),
    lag52_autocorrelation = acf_at_lag_fe(xf, 52),
    acf_decay = acf_decay_fe(xf),
    partial_acf_lag1 = pacf_lag1_fe(xf),
    spectral_entropy = spectral_entropy_fe(xf),
    dominant_period = dominant_period_fe(xf, frequency = frequency),
    sample_entropy = sample_entropy_fe(xf),
    persistence_entropy = persistence_entropy_val,
    missingness_rate = mean(is.na(y_raw)),
    heaping_index = heaping_index_fe(y_raw),
    repetition_index = repetition_index_fe(y_raw),
    proportion_zero_counts = mean(yn == 0),
    longest_zero_run = longest_run_fe(y_raw, function(z) z == 0),
    longest_nonzero_run = longest_run_fe(y_raw, function(z) z != 0),
    intermittency = intermittency_index_fe(y_raw),
    run_length_cv = run_length_cv_fe(y_raw),
    zero_cluster_index = zero_cluster_index_fe(y_raw),
    outbreak_weeks = outbreak_weeks_val,
    outbreak_week_rate = if (n_obs > 0) outbreak_weeks_val / n_obs else NA_real_,
    peak_to_mean_ratio = if (!is.na(mean_cases) && mean_cases > 0) max(yn, na.rm = TRUE) / mean_cases else NA_real_,
    level_shift_index = level_shift_fe(xf),
    change_point_count = change_point_count_val,
    change_point_rate = if (length(xf) > 0) change_point_count_val / length(xf) else NA_real_,
    outbreak_dominance_index = outbreak_dominance_index_fe(y_raw),
    max_to_median_ratio = {
      med <- stats::median(yn, na.rm = TRUE)
      if (!is.na(med) && med > 0) max(yn, na.rm = TRUE) / med else NA_real_
    },
    upper_tail_weight = upper_tail_weight_fe(y_raw),
    seasonal_peak_timing_instability = seasonal_peak_timing_instability_fe(y_raw, frequency = frequency),
    signal_to_noise_ratio = sig$signal_to_noise_ratio,
    stl_remainder_strength = sig$stl_remainder_strength,
    rolling_variance_instability = rolling_stat_instability_fe(y_raw, window = 13, stat = "variance"),
    rolling_mean_instability = rolling_stat_instability_fe(y_raw, window = 13, stat = "mean"),
    residual_acf_strength = sig$residual_acf_strength,
    noise_to_signal_ratio = sig$noise_to_signal_ratio,
    yoy_mean_cv = yoy$yoy_mean_cv,
    yoy_peak_cv = yoy$yoy_peak_cv,
    yoy_variance_cv = yoy$yoy_variance_cv,
    seasonal_subseries_cv = seasonal_subseries_cv_fe(y_raw, frequency = frequency)
  )
  for (v in predictor_vars) {
    if (!v %in% names(feats_raw)) feats_raw[[v]] <- NA_real_
    feats_raw[[v]] <- as.numeric(feats_raw[[v]])
    feats_raw[[v]][is.infinite(feats_raw[[v]]) | is.nan(feats_raw[[v]])] <- NA_real_
  }
  feats_raw %>% dplyr::select(dplyr::all_of(predictor_vars))
}
transform_raw_features_for_model <- function(feats) {
  feats_model <- as.data.frame(feats)
  if (isTRUE(apply_log1p_to_model_features)) {
    for (v in intersect(log1p_features, names(feats_model))) {
      feats_model[[v]] <- log1p(pmax(as.numeric(feats_model[[v]]), 0))
    }
  }
  tibble::as_tibble(feats_model) %>% dplyr::select(dplyr::all_of(predictor_vars))
}
compute_model_features <- function(y, frequency = 52, compute_persistence = TRUE) {
  raw <- compute_raw_features_display(
    y = y,
    frequency = frequency,
    compute_persistence = compute_persistence
  )
  transform_raw_features_for_model(raw)
}
# ------------------------------------------------------------
