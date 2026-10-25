# EpiFORESS Analysis Pipeline
# Script: 07_empirical_validation.R
# Project root: set EPIFORESS_PROJECT_DIR or run from the project data directory.
# Reproducibility: inspect all settings and verify input/model compatibility before reporting results.

# ============================================================
# EpiFORESS: EMPIRICAL VALIDATION
# Script: 06_empirical_validation.R
#
# Purpose:
#   Generate disease-level forecasts and evaluate the deployed
#   EpiFORESS scoring pipeline against empirical forecasting performance.
#
# Main configuration:
#   - Empirical target: Ready if Best MASE < 1.00
#   - Predictor set: 49 features across seven feature domains
#   - Statistical and machine-learning components loaded from saved models
#   - Hybrid score based on the saved component weights when available
#   - XGBoost Platt calibration applied only when relevant and available
#   - Missing predictors imputed using saved training medians where available
#   - Score bands: 0-24, 25-49, 50-79, 80-89, and 90-100
#   - Forecasting roster: Seasonal Naive, STL-ETS, SARIMA,
#     nonseasonal ARIMA, and Negative Binomial regression
#
# Validation policy:
#   - The primary empirical analysis retains all diseases with valid results.
#   - Disease exclusions are used only in the optional sensitivity analysis.
#
# Score definition:
#   EpiFORESS = 100 * [1 - P_Hybrid(Not Ready)]
#
# Run from the project directory, or set EPIFORESS_PROJECT_DIR to the
# directory containing Raw_Diseases.xlsx and Saved_Models/.
# ============================================================

# ------------------------------------------------------------
# 0. LOAD PACKAGES
# ------------------------------------------------------------

packages <- c(
  "readxl", "openxlsx", "dplyr", "tibble", "purrr",
  "forecast", "ggplot2", "stringr", "tidyr",
  "pROC", "lmtest", "FSA", "broom",
  "caret", "xgboost", "clinfun", "MASS"
)

missing_packages <- setdiff(packages, rownames(installed.packages()))

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
  library(dplyr)
  library(tibble)
  library(purrr)
  library(forecast)
  library(ggplot2)
  library(stringr)
  library(tidyr)
  library(pROC)
  library(lmtest)
  library(FSA)
  library(broom)
  library(caret)
  library(xgboost)
  library(clinfun)
  library(MASS)
})

has_TDA <- requireNamespace("TDA", quietly = TRUE)
has_changepoint <- requireNamespace("changepoint", quietly = TRUE)

set.seed(2026)

# ------------------------------------------------------------
# 1. USER SETTINGS
# ------------------------------------------------------------

base_path <- normalizePath(
  Sys.getenv("EPIFORESS_PROJECT_DIR", unset = getwd()),
  winslash = "/",
  mustWork = FALSE
)

if (!dir.exists(base_path)) {
  stop("Project directory does not exist: ", base_path)
}

raw_workbook_path <- file.path(base_path, "Raw_Diseases.xlsx")
output_workbook_path <- file.path(base_path, "EpiFORESS_Forecast_Results.xlsx")
raw_sheet <- NULL

if (!file.exists(raw_workbook_path)) {
  stop(
    "Raw disease workbook not found: ", raw_workbook_path,
    "\\nPlace Raw_Diseases.xlsx in the project directory or set ",
    "EPIFORESS_PROJECT_DIR."
  )
}

model_output_dir <- file.path(base_path, "Saved_Models")
dir.create(model_output_dir, recursive = TRUE, showWarnings = FALSE)

stat_model_path <- file.path(
  model_output_dir,
  "best_statistical_forecast_readiness_model.rds"
)

ml_model_path <- file.path(
  model_output_dir,
  "best_machine_learning_model.rds"
)

xgb_calibration_path <- file.path(
  model_output_dir,
  "xgb_platt_calibration_model.rds"
)

epiforess_results_path <- file.path(
  model_output_dir,
  "final_epiforess_results.rds"
)

lodo_results_path <- file.path(
  model_output_dir,
  "lodo_validation_results.rds"
)

forecast_output_path <- file.path(
  base_path,
  "Raw_Disease_Forecasting_Evaluation.xlsx"
)

validation_output_path <- file.path(
  base_path,
  "EpiFORESS_End_to_End_Validation.xlsx"
)

score_output_path <- file.path(
  base_path,
  "EpiFORESS_Computed_Scores.xlsx"
)

plot_best_mase_path <- file.path(
  base_path,
  "Raw_Disease_Best_MASE.png"
)

seasonal_frequency <- 52
forecast_horizon   <- 13
initial_window     <- 156
rolling_step       <- 13
max_origins        <- 6

aggregate_weekly_duplicates <- TRUE
bootstrap_n <- 1000

# Sole empirical target used to train and validate EpiFORESS.
empirical_mase_threshold <- 1.00

# The main modeling script stores raw predictors with median imputation.
# Keep FALSE unless the final models are explicitly retrained on log1p features.
apply_log1p_to_model_features <- FALSE

# Fallback only. The saved validation-selected threshold should normally load.
# A Not-Ready probability threshold of 0.34 corresponds to a score cutoff of 66.
fallback_notready_probability_threshold <- 0.34

# ------------------------------------------------------------
# OPTIONAL DISEASE-EXCLUSION SENSITIVITY ANALYSIS
# ------------------------------------------------------------

run_exclusion_sensitivity <- TRUE

hard_empirical_diseases <- c(
  "Chikungunya",
  "Pertussis",
  "Acute Viral Hepatitis"
)

hard_empirical_exclusion_note <- paste(
  "Excluded only from the optional disease-exclusion sensitivity analysis;",
  "retained in the primary all-disease empirical validation,",
  "raw forecasting outputs, and EpiFORESS score tables."
)

# ------------------------------------------------------------
# 1A. FINAL 49 PREDICTORS
# ------------------------------------------------------------

predictor_vars <- c(
  # Domain 1: Distributional characteristics, 7
  "mean_cases",
  "dispersion_index",
  "coefficient_of_variation",
  "variance_cases",
  "skewness_cases",
  "kurtosis_cases",
  "gini_coefficient",

  # Domain 2: Temporal structure, 9
  "trend_strength",
  "trend_slope",
  "seasonality_strength",
  "lag1_autocorrelation",
  "lag2_autocorrelation",
  "lag4_autocorrelation",
  "lag52_autocorrelation",
  "acf_decay",
  "partial_acf_lag1",

  # Domain 3: Spectral and complexity, 4
  "spectral_entropy",
  "dominant_period",
  "sample_entropy",
  "persistence_entropy",

  # Domain 4: Data quality, 9
  "missingness_rate",
  "heaping_index",
  "repetition_index",
  "proportion_zero_counts",
  "longest_zero_run",
  "longest_nonzero_run",
  "intermittency",
  "run_length_cv",
  "zero_cluster_index",

  # Domain 5: Outbreak and structural instability, 10
  "outbreak_weeks",
  "outbreak_week_rate",
  "peak_to_mean_ratio",
  "level_shift_index",
  "change_point_count",
  "change_point_rate",
  "outbreak_dominance_index",
  "max_to_median_ratio",
  "upper_tail_weight",
  "seasonal_peak_timing_instability",

  # Domain 6: Signal quality and noise, 6
  "signal_to_noise_ratio",
  "stl_remainder_strength",
  "rolling_variance_instability",
  "rolling_mean_instability",
  "residual_acf_strength",
  "noise_to_signal_ratio",

  # Domain 7: Inter-annual stability, 4
  "yoy_mean_cv",
  "yoy_peak_cv",
  "yoy_variance_cv",
  "seasonal_subseries_cv"
)

stopifnot(length(predictor_vars) == 49)

log1p_features <- c(
  "mean_cases",
  "dispersion_index",
  "variance_cases",
  "peak_to_mean_ratio",
  "level_shift_index",
  "change_point_count",
  "outbreak_weeks",
  "longest_zero_run",
  "longest_nonzero_run",
  "max_to_median_ratio",
  "dominant_period",
  "rolling_variance_instability",
  "rolling_mean_instability",
  "signal_to_noise_ratio",
  "noise_to_signal_ratio"
)

excluded_sheet_patterns <- c(
  "^SpectralTDA", "^LODO", "^EpiFORESS", "^EpiFORES",
  "^Model", "^Forecast", "^Readiness", "^Settings",
  "^Results", "^Summary", "^Correlations", "^GroupTests",
  "^Validation", "^Performance", "^Predictions", "^Features",
  "^Importance", "^Calibration", "^Threshold", "^All_Model",
  "^Best_Model", "^Series_Metadata", "^Winning_Model", "^Overall",
  "^Empirical"
)

# ------------------------------------------------------------
# 2. GENERAL HELPERS
# ------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

get_list_value <- function(x, candidates, default = NULL) {
  if (is.null(x)) return(default)

  for (nm in candidates) {
    value <- x[[nm]]

    if (!is.null(value) && length(value) > 0 && !all(is.na(value))) {
      return(value)
    }
  }

  default
}

clean_colnames <- function(x) {
  stringr::str_trim(as.character(x))
}

normalize_name <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

safe_numeric <- function(x) {
  x <- stringr::str_replace_all(as.character(x), ",|%", "")
  out <- suppressWarnings(as.numeric(x))
  out[is.infinite(out) | is.nan(out)] <- NA_real_
  out
}

is_numeric_like <- function(x, min_nonmissing_prop = 0.50) {
  mean(!is.na(safe_numeric(x))) >= min_nonmissing_prop
}

find_column <- function(names_vec, candidates) {
  idx <- match(normalize_name(candidates), normalize_name(names_vec))
  idx <- idx[!is.na(idx)]

  if (length(idx) == 0) return(NA_character_)

  names_vec[idx[1]]
}

exclude_output_sheets <- function(sheets) {
  if (length(sheets) == 0) return(sheets)

  pattern <- paste(excluded_sheet_patterns, collapse = "|")

  sheets[!stringr::str_detect(
    sheets,
    stringr::regex(pattern, ignore_case = TRUE)
  )]
}

safe_sheet_name <- function(x) {
  out <- stringr::str_replace_all(
    as.character(x),
    "[:\\\\/?*\\[\\]]",
    "_"
  )

  out <- stringr::str_sub(out, 1, 31)

  ifelse(is.na(out) | out == "", "Sheet", out)
}

safe_var <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 1) return(NA_real_)
  stats::var(x)
}

safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 1) return(NA_real_)
  stats::sd(x)
}

safe_cv <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 1) return(NA_real_)

  m <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)

  if (!is.finite(m) || abs(m) < 1e-12 || !is.finite(s)) {
    return(NA_real_)
  }

  s / abs(m)
}

safe_quantile <- function(x, p) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, p, na.rm = TRUE, names = FALSE))
}

clamp_probability <- function(p, eps = 1e-6) {
  p <- as.numeric(p)
  p[!is.finite(p)] <- NA_real_
  pmin(pmax(p, eps), 1 - eps)
}

readiness_band <- function(score) {
  dplyr::case_when(
    is.na(score)  ~ NA_character_,
    score < 25    ~ "Very_Low",
    score < 50    ~ "Low",
    score < 80    ~ "Moderate",
    score < 90    ~ "High",
    TRUE          ~ "Very_High"
  )
}

possible_time_cols <- c(
  "Week", "week", "Date", "date", "Year", "year",
  "Time", "time", "Month", "month", "Period", "period",
  "Epiweek", "epiweek", "Epi_Week", "epi_week",
  "Row_Index", "row_index"
)

preferred_count_cols <- c(
  "Cases", "Count", "Observed_Cases", "Reported_Cases",
  "Case_Count", "Weekly_Cases", "cases", "count",
  "observed_cases", "reported_cases", "case_count", "weekly_cases"
)

disease_col_candidates <- c(
  "Disease", "disease", "Disease_Seed", "Disease_Name",
  "disease_seed", "disease_name"
)

year_col_candidates <- c("Year", "year")

week_col_candidates <- c(
  "Week", "week", "Epiweek", "epiweek", "Epi_Week", "epi_week"
)

detect_count_column <- function(dat) {
  nms <- names(dat)

  preferred <- find_column(nms, preferred_count_cols)
  if (!is.na(preferred)) return(preferred)

  time_present <- nms[normalize_name(nms) %in% normalize_name(possible_time_cols)]
  numeric_like <- nms[sapply(dat, is_numeric_like)]
  candidates <- setdiff(numeric_like, time_present)

  if (length(candidates) > 0) return(candidates[1])
  if (length(numeric_like) > 0) return(numeric_like[1])

  NA_character_
}

has_long_format_columns <- function(dat) {
  names(dat) <- clean_colnames(names(dat))

  !is.na(find_column(names(dat), disease_col_candidates)) &&
    !is.na(find_column(names(dat), year_col_candidates)) &&
    !is.na(find_column(names(dat), week_col_candidates)) &&
    !is.na(find_column(names(dat), preferred_count_cols))
}

safe_mase_denominator <- function(training_series, frequency = 1) {
  y <- as.numeric(training_series)
  y <- y[is.finite(y)]

  if (length(y) < 2) return(NA_real_)

  if (!is.na(frequency) && frequency > 1 && length(y) > frequency) {
    d <- mean(
      abs(
        y[(frequency + 1):length(y)] -
          y[1:(length(y) - frequency)]
      ),
      na.rm = TRUE
    )

    if (!is.na(d) && d > 0) return(d)
  }

  d <- mean(abs(diff(y)), na.rm = TRUE)
  if (!is.na(d) && d > 0) return(d)

  d <- mean(abs(y - mean(y, na.rm = TRUE)), na.rm = TRUE)
  if (!is.na(d) && d > 0) return(d)

  NA_real_
}

calculate_metrics <- function(actual,
                              predicted,
                              training_series = NULL,
                              frequency = 1,
                              mase_denominators = NULL) {
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)

  valid <- stats::complete.cases(actual, predicted)
  av <- actual[valid]
  pv <- predicted[valid]

  if (length(av) == 0) {
    return(tibble::tibble(
      MAE = NA_real_,
      RMSE = NA_real_,
      Bias = NA_real_,
      MASE = NA_real_,
      N_Evaluated = 0L
    ))
  }

  errors <- av - pv

  mae <- mean(abs(errors), na.rm = TRUE)
  rmse <- sqrt(mean(errors^2, na.rm = TRUE))
  bias <- mean(pv - av, na.rm = TRUE)

  if (!is.null(mase_denominators)) {
    dv <- as.numeric(mase_denominators)[valid]
    ok <- stats::complete.cases(errors, dv) & dv > 0

    mase <- if (sum(ok) > 0) {
      mean(abs(errors[ok]) / dv[ok], na.rm = TRUE)
    } else {
      NA_real_
    }
  } else {
    den <- safe_mase_denominator(training_series, frequency = frequency)
    mase <- if (!is.na(den) && den > 0) mae / den else NA_real_
  }

  tibble::tibble(
    MAE = mae,
    RMSE = rmse,
    Bias = bias,
    MASE = mase,
    N_Evaluated = length(av)
  )
}

make_classification_metrics <- function(actual, predicted) {
  actual <- factor(actual, levels = c("Ready", "Not_Ready"))
  predicted <- factor(predicted, levels = c("Ready", "Not_Ready"))

  valid <- !is.na(actual) & !is.na(predicted)
  actual <- actual[valid]
  predicted <- predicted[valid]

  raw_tbl <- table(Predicted = predicted, Actual = actual)

  cm <- matrix(
    0,
    2,
    2,
    dimnames = list(
      Predicted = c("Ready", "Not_Ready"),
      Actual = c("Ready", "Not_Ready")
    )
  )

  cm[rownames(raw_tbl), colnames(raw_tbl)] <- raw_tbl

  TP <- cm["Not_Ready", "Not_Ready"]
  TN <- cm["Ready", "Ready"]
  FP <- cm["Not_Ready", "Ready"]
  FN <- cm["Ready", "Not_Ready"]

  n <- sum(cm)

  acc <- if (n == 0) NA_real_ else (TP + TN) / n
  rec <- if ((TP + FN) == 0) NA_real_ else TP / (TP + FN)
  spec <- if ((TN + FP) == 0) NA_real_ else TN / (TN + FP)
  ba <- if (all(is.na(c(rec, spec)))) NA_real_ else mean(c(rec, spec), na.rm = TRUE)
  prec <- if ((TP + FP) == 0) NA_real_ else TP / (TP + FP)

  f1 <- if (is.na(prec) || is.na(rec) || (prec + rec) == 0) {
    NA_real_
  } else {
    2 * prec * rec / (prec + rec)
  }

  degenerate_flag <- dplyr::case_when(
    is.na(rec) || rec == 0 ~ "No_NotReady_Detected",
    is.na(spec) || spec == 0 ~ "No_Ready_Detected",
    TRUE ~ "OK"
  )

  selection_score <- if (degenerate_flag != "OK") {
    NA_real_
  } else {
    0.35 * ba +
      0.35 * f1 +
      0.20 * rec +
      0.10 * spec
  }

  list(
    confusion_table = cm,
    metrics = tibble::tibble(
      N = n,
      Accuracy = acc,
      Balanced_Accuracy = ba,
      Precision_NotReady = prec,
      Recall_NotReady = rec,
      Specificity_Ready = spec,
      F1_NotReady = f1,
      Selection_Score = selection_score,
      Degenerate_Flag = degenerate_flag,
      TP = as.integer(TP),
      TN = as.integer(TN),
      FP = as.integer(FP),
      FN = as.integer(FN)
    )
  )
}

compute_brier_score_empirical <- function(empirical_readiness, epiforess_score) {
  observed <- as.numeric(empirical_readiness == "Ready")
  predicted <- epiforess_score / 100

  valid <- stats::complete.cases(observed, predicted)
  if (sum(valid) == 0) return(NA_real_)

  mean((predicted[valid] - observed[valid])^2)
}

compute_empirical_auc_ci <- function(actual, score, boot_n = bootstrap_n) {
  actual <- factor(actual, levels = c("Not_Ready", "Ready"))

  valid <- !is.na(actual) & !is.na(score)
  actual <- actual[valid]
  score <- score[valid]

  if (length(unique(stats::na.omit(actual))) < 2) {
    return(tibble::tibble(
      AUC = NA_real_,
      AUC_CI_Lower = NA_real_,
      AUC_CI_Upper = NA_real_
    ))
  }

  tryCatch({
    ro <- pROC::roc(
      response = actual,
      predictor = score,
      levels = c("Not_Ready", "Ready"),
      direction = "<",
      quiet = TRUE
    )

    ci <- pROC::ci.auc(
      ro,
      method = "bootstrap",
      boot.n = boot_n,
      progress = "none"
    )

    tibble::tibble(
      AUC = as.numeric(pROC::auc(ro)),
      AUC_CI_Lower = as.numeric(ci[1]),
      AUC_CI_Upper = as.numeric(ci[3])
    )
  }, error = function(e) {
    tibble::tibble(
      AUC = NA_real_,
      AUC_CI_Lower = NA_real_,
      AUC_CI_Upper = NA_real_
    )
  })
}

write_tables_to_workbook <- function(tables, path) {
  wb <- openxlsx::createWorkbook()
  used <- character(0)

  for (sheet in names(tables)) {
    tab <- tables[[sheet]]
    if (is.null(tab)) next

    sn <- safe_sheet_name(sheet)
    base_sn <- sn
    suffix <- 1

    while (sn %in% used) {
      suffix <- suffix + 1
      sn <- safe_sheet_name(paste0(base_sn, "_", suffix))
    }

    used <- c(used, sn)

    openxlsx::addWorksheet(wb, sn)
    openxlsx::writeData(wb, sn, tab, rowNames = FALSE)
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

append_to_results_workbook <- function(tables, path) {
  if (!file.exists(path)) {
    warning("Results workbook not found. Creating new workbook: ", path)
    wb <- openxlsx::createWorkbook()
  } else {
    wb <- openxlsx::loadWorkbook(path)
  }

  for (sn in names(tables)) {
    tab <- tables[[sn]]
    if (is.null(tab)) next

    sn_safe <- safe_sheet_name(sn)

    if (sn_safe %in% openxlsx::sheets(wb)) {
      openxlsx::removeWorksheet(wb, sn_safe)
    }

    openxlsx::addWorksheet(wb, sn_safe)
    openxlsx::writeData(wb, sn_safe, tab, rowNames = FALSE)
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  cat("Results workbook updated:", path, "\n")
}

# ------------------------------------------------------------
# 3. FEATURE EXTRACTION HELPERS
# ------------------------------------------------------------

skewness_fe <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(NA_real_)

  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) return(0)

  mean(((x - mean(x)) / s)^3)
}

kurtosis_fe <- function(x) {
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

  spec <- tryCatch(
    stats::spectrum(x, plot = FALSE)$spec,
    error = function(e) NULL
  )

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

  sp <- tryCatch(
    stats::spectrum(x, plot = FALSE),
    error = function(e) NULL
  )

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

  fit <- tryCatch(
    stats::lm(log(abs(av[valid])) ~ lags[valid]),
    error = function(e) NULL
  )

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

rolling_stat_instability_fe <- function(x,
                                        window = 13,
                                        stat = c("mean", "variance")) {
  stat <- match.arg(stat)

  x <- as.numeric(x)
  x <- x[is.finite(x)]

  if (length(x) < 2 * window) return(NA_real_)

  vals <- purrr::map_dbl(seq_len(length(x) - window + 1), function(i) {
    z <- x[i:(i + window - 1)]

    if (stat == "mean") {
      mean(z, na.rm = TRUE)
    } else {
      stats::var(z, na.rm = TRUE)
    }
  })

  vals <- vals[is.finite(vals)]
  if (length(vals) <= 1) return(NA_real_)

  safe_cv(vals)
}

seasonal_peak_timing_instability_fe <- function(x, frequency = 52) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]

  if (length(x) < 2 * frequency) return(NA_real_)

  n_years <- floor(length(x) / frequency)
  mat <- matrix(
    x[seq_len(n_years * frequency)],
    ncol = frequency,
    byrow = TRUE
  )

  peak_weeks <- apply(mat, 1, which.max)
  if (length(unique(peak_weeks)) <= 1) return(0)

  stats::sd(peak_weeks, na.rm = TRUE) / frequency
}

seasonal_subseries_cv_fe <- function(x, frequency = 52) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]

  if (length(x) < 2 * frequency) return(NA_real_)

  n_years <- floor(length(x) / frequency)
  mat <- matrix(
    x[seq_len(n_years * frequency)],
    ncol = frequency,
    byrow = TRUE
  )

  cv_by_week <- apply(mat, 2, safe_cv)

  if (all(is.na(cv_by_week))) return(NA_real_)

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
  mat <- matrix(
    x[seq_len(n_years * frequency)],
    ncol = frequency,
    byrow = TRUE
  )

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

  noise_to_signal_ratio <- if (
    !is.na(signal_to_noise_ratio) && signal_to_noise_ratio > 0
  ) {
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

      stl_remainder_strength <- if (!is.na(total_var) && total_var > 0) {
        rem_var / total_var
      } else {
        NA_real_
      }

      residual_acf_strength <- acf_at_lag_fe(rem, lag = 1)

      if (is.na(signal_to_noise_ratio) && !is.na(rem_var) && rem_var > 0) {
        signal_to_noise_ratio <- signal_var / rem_var

        noise_to_signal_ratio <- if (!is.na(signal_var) && signal_var > 0) {
          rem_var / signal_var
        } else {
          NA_real_
        }
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

  ms <- tryCatch(
    stats::quantile(pd, 0.30, na.rm = TRUE),
    error = function(e) NA_real_
  )

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

compute_epiforess_features_49 <- function(y, frequency = 52) {
  y_raw <- as.numeric(y)
  yn <- y_raw[!is.na(y_raw)]

  if (length(yn) == 0) {
    stop("No usable numeric count values found.")
  }

  xf <- tryCatch(
    as.numeric(forecast::na.interp(stats::ts(y_raw, frequency = frequency))),
    error = function(e) NULL
  )

  if (is.null(xf)) {
    stop("Interpolation failed during feature extraction.")
  }

  mean_cases <- mean(yn, na.rm = TRUE)
  variance_cases <- safe_var(yn)
  n_obs <- length(yn)

  sig <- get_signal_features_fe(y_raw, frequency = frequency)
  yoy <- yoy_features_fe(y_raw, frequency = frequency)

  spectral_entropy_val <- spectral_entropy_fe(xf)
  dominant_period_val <- dominant_period_fe(xf, frequency = frequency)
  outbreak_weeks_val <- outbreak_weeks_fe(y_raw)
  change_point_count_val <- change_point_count_fe(xf)

  feats_raw <- tibble::tibble(
    mean_cases = mean_cases,
    dispersion_index = if (!is.na(mean_cases) && mean_cases > 0) {
      variance_cases / mean_cases
    } else {
      NA_real_
    },
    coefficient_of_variation = if (!is.na(mean_cases) && mean_cases > 0) {
      stats::sd(yn, na.rm = TRUE) / mean_cases
    } else {
      NA_real_
    },
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

    spectral_entropy = spectral_entropy_val,
    dominant_period = dominant_period_val,
    sample_entropy = sample_entropy_fe(xf),
    persistence_entropy = persistence_entropy_fe(xf),

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
    peak_to_mean_ratio = if (!is.na(mean_cases) && mean_cases > 0) {
      max(yn, na.rm = TRUE) / mean_cases
    } else {
      NA_real_
    },
    level_shift_index = level_shift_fe(xf),
    change_point_count = change_point_count_val,
    change_point_rate = if (length(xf) > 0) {
      change_point_count_val / length(xf)
    } else {
      NA_real_
    },
    outbreak_dominance_index = outbreak_dominance_index_fe(y_raw),
    max_to_median_ratio = {
      med <- stats::median(yn, na.rm = TRUE)

      if (!is.na(med) && med > 0) {
        max(yn, na.rm = TRUE) / med
      } else {
        NA_real_
      }
    },
    upper_tail_weight = upper_tail_weight_fe(y_raw),
    seasonal_peak_timing_instability = seasonal_peak_timing_instability_fe(
      y_raw,
      frequency = frequency
    ),

    signal_to_noise_ratio = sig$signal_to_noise_ratio,
    stl_remainder_strength = sig$stl_remainder_strength,
    rolling_variance_instability = rolling_stat_instability_fe(
      y_raw,
      window = 13,
      stat = "variance"
    ),
    rolling_mean_instability = rolling_stat_instability_fe(
      y_raw,
      window = 13,
      stat = "mean"
    ),
    residual_acf_strength = sig$residual_acf_strength,
    noise_to_signal_ratio = sig$noise_to_signal_ratio,

    yoy_mean_cv = yoy$yoy_mean_cv,
    yoy_peak_cv = yoy$yoy_peak_cv,
    yoy_variance_cv = yoy$yoy_variance_cv,
    seasonal_subseries_cv = seasonal_subseries_cv_fe(
      y_raw,
      frequency = frequency
    )
  )

  for (v in predictor_vars) {
    if (!v %in% names(feats_raw)) feats_raw[[v]] <- NA_real_

    feats_raw[[v]] <- as.numeric(feats_raw[[v]])
    feats_raw[[v]][is.infinite(feats_raw[[v]]) | is.nan(feats_raw[[v]])] <- NA_real_
  }

  feats_model <- feats_raw

  if (isTRUE(apply_log1p_to_model_features)) {
    for (v in intersect(log1p_features, names(feats_model))) {
      feats_model[[v]] <- log1p(pmax(as.numeric(feats_model[[v]]), 0))
    }
  }

  list(
    raw_features = feats_raw %>%
      dplyr::select(dplyr::all_of(predictor_vars)),
    model_features = feats_model %>%
      dplyr::select(dplyr::all_of(predictor_vars))
  )
}

# ------------------------------------------------------------
# 4. MODEL PREDICTION HELPERS
# ------------------------------------------------------------

unwrap_model_object <- function(obj) {
  if (
    inherits(obj, "glm") ||
    inherits(obj, "train") ||
    inherits(obj, "xgb.Booster") ||
    inherits(obj, "cv.glmnet") ||
    inherits(obj, "glmnet")
  ) {
    return(obj)
  }

  if (is.list(obj)) {
    for (nm in c(
      "model", "final_model", "best_model", "fitted_model",
      "caret_model", "xgb_model", "object", "fit", "final_fit",
      "finalModel", "final_model_object"
    )) {
      cand <- obj[[nm]]

      if (!is.null(cand) &&
          (
            inherits(cand, "glm") ||
            inherits(cand, "train") ||
            inherits(cand, "xgb.Booster") ||
            inherits(cand, "cv.glmnet") ||
            inherits(cand, "glmnet")
          )) {
        return(cand)
      }
    }
  }

  obj
}

get_model_predictor_names <- function(model_obj, default_predictors = predictor_vars) {
  obj <- model_obj

  if (is.list(obj)) {
    for (nm in c(
      "predictors", "predictor_vars", "features", "x_names",
      "final_predictor_cols", "predictor_cols", "model_predictors",
      "predictor_columns"
    )) {
      if (!is.null(obj[[nm]]) && length(obj[[nm]]) > 0) {
        out <- intersect(as.character(obj[[nm]]), default_predictors)
        if (length(out) > 0) return(out)
      }
    }
  }

  model <- unwrap_model_object(obj)

  if (inherits(model, "xgb.Booster")) {
    fn <- tryCatch(model$feature_names, error = function(e) NULL)

    if (!is.null(fn) && length(fn) > 0) {
      out <- intersect(as.character(fn), default_predictors)
      if (length(out) > 0) return(out)
    }
  }

  if (inherits(model, "train")) {
    if (!is.null(model$xNames)) {
      out <- intersect(model$xNames, default_predictors)
      if (length(out) > 0) return(out)
    }

    if (!is.null(model$trainingData)) {
      out <- setdiff(
        names(model$trainingData),
        c(".outcome", "y", "y_notready")
      )

      out <- intersect(out, default_predictors)
      if (length(out) > 0) return(out)
    }

    if (!is.null(model$finalModel) && !is.null(model$finalModel$xNames)) {
      out <- intersect(model$finalModel$xNames, default_predictors)
      if (length(out) > 0) return(out)
    }
  }

  if (inherits(model, "glm")) {
    terms_obj <- tryCatch(
      attr(stats::terms(model), "term.labels"),
      error = function(e) NULL
    )

    if (!is.null(terms_obj) && length(terms_obj) > 0) {
      out <- intersect(terms_obj, default_predictors)
      if (length(out) > 0) return(out)
    }
  }

  default_predictors
}

extract_imputation_medians <- function(epiforess_results, predictor_set) {
  med <- rep(0, length(predictor_set))
  names(med) <- predictor_set

  if (is.null(epiforess_results)) return(med)

  candidate_names <- c(
    "imputation_medians",
    "train_medians",
    "training_medians",
    "predictor_medians",
    "median_imputation_values",
    "final_imputation_medians",
    "preprocessing_medians"
  )

  for (nm in candidate_names) {
    obj <- epiforess_results[[nm]]
    if (is.null(obj)) next

    if (is.data.frame(obj)) {
      nms <- normalize_name(names(obj))

      var_col <- names(obj)[nms %in% c(
        "variable", "predictor", "feature", "name"
      )][1]

      med_col <- names(obj)[nms %in% c(
        "median_used", "median", "value", "imputation_value"
      )][1]

      if (!is.na(var_col) && !is.na(med_col)) {
        vars <- as.character(obj[[var_col]])
        vals <- safe_numeric(obj[[med_col]])
        names(vals) <- vars

        hit <- intersect(names(vals), predictor_set)
        med[hit] <- vals[hit]

        return(med)
      }
    }

    if (is.numeric(obj) && !is.null(names(obj))) {
      hit <- intersect(names(obj), predictor_set)
      med[hit] <- obj[hit]
      return(med)
    }
  }

  med
}

prepare_model_features <- function(model_features,
                                   predictor_set,
                                   medians = NULL) {
  mf <- as.data.frame(model_features)

  for (v in predictor_set) {
    if (!v %in% names(mf)) mf[[v]] <- NA_real_

    mf[[v]] <- as.numeric(mf[[v]])
    mf[[v]][is.infinite(mf[[v]]) | is.nan(mf[[v]])] <- NA_real_
  }

  if (is.null(medians)) {
    medians <- rep(0, length(predictor_set))
    names(medians) <- predictor_set
  }

  for (v in predictor_set) {
    med <- medians[[v]]

    if (is.null(med) || is.na(med) || !is.finite(med)) med <- 0

    mf[[v]][is.na(mf[[v]])] <- med
  }

  mf <- mf %>%
    dplyr::select(dplyr::all_of(predictor_set))

  bad_after <- names(mf)[sapply(
    mf,
    function(z) any(is.na(z) | is.nan(z) | is.infinite(z))
  )]

  if (length(bad_after) > 0) {
    stop("Bad values remain after imputation: ", paste(bad_after, collapse = ", "))
  }

  mf
}

predict_platt_probability <- function(platt_model, raw_p) {
  raw_p <- clamp_probability(raw_p)

  if (is.null(platt_model)) return(raw_p)

  model_terms <- tryCatch(
    attr(stats::terms(platt_model), "term.labels"),
    error = function(e) NULL
  )

  candidate_names <- unique(c(
    model_terms,
    "prob",
    "eta_oof",
    "p_raw",
    "raw_p",
    "raw_prob",
    "xgb_raw",
    "xgb_raw_prob",
    "xgb_prob",
    "p_ml_raw",
    "P_ML_NotReady_Raw"
  ))

  for (nm in candidate_names) {
    if (is.null(nm) || is.na(nm) || nm == "") next

    value <- if (nm == "eta_oof") qlogis(clamp_probability(raw_p)) else raw_p

    nd <- data.frame(value)
    names(nd) <- nm

    out <- tryCatch(
      as.numeric(stats::predict(platt_model, newdata = nd, type = "response")),
      error = function(e) NULL
    )

    if (!is.null(out) && all(is.finite(out))) {
      return(clamp_probability(out))
    }
  }

  warning("Platt calibration model could not be applied. Using raw ML probabilities.")
  raw_p
}

predict_not_ready <- function(model_obj,
                              newdata,
                              predictor_set = predictor_vars,
                              calibration_model = NULL) {
  model <- unwrap_model_object(model_obj)
  newdata <- as.data.frame(newdata)

  for (v in predictor_set) {
    if (!v %in% names(newdata)) newdata[[v]] <- 0
  }

  newdata <- newdata[, predictor_set, drop = FALSE]

  if (inherits(model, "cv.glmnet")) {
    p <- as.numeric(
      predict(
        model,
        newx = as.matrix(newdata),
        s = "lambda.min",
        type = "response"
      )
    )

    return(clamp_probability(p))
  }

  if (inherits(model, "glmnet")) {
    p <- as.numeric(
      predict(
        model,
        newx = as.matrix(newdata),
        type = "response"
      )
    )

    return(clamp_probability(p))
  }

  if (inherits(model, "glm")) {
    p <- as.numeric(
      stats::predict(
        model,
        newdata = newdata,
        type = "response"
      )
    )

    return(clamp_probability(p))
  }

  if (inherits(model, "train")) {
    needed_cols <- model$xNames

    if (!is.null(needed_cols)) {
      missing_needed <- setdiff(needed_cols, names(newdata))

      if (length(missing_needed) > 0) {
        stop(
          "New data missing required caret columns: ",
          paste(missing_needed, collapse = ", ")
        )
      }

      newdata <- newdata[, needed_cols, drop = FALSE]
    }

    prob <- predict(model, newdata = newdata, type = "prob")

    cn <- stringr::str_replace_all(tolower(colnames(prob)), "[^a-z0-9]", "")
    ni <- which(cn %in% c("notready", "notforecastready"))
    ri <- which(cn %in% c("ready", "forecastready"))

    if (length(ni) == 1) {
      p_notready <- clamp_probability(as.numeric(prob[, ni]))
    } else if (length(ri) == 1) {
      p_notready <- clamp_probability(1 - as.numeric(prob[, ri]))
    } else {
      stop("Could not identify Not_Ready probability column in caret model.")
    }

    if (!is.null(calibration_model)) {
      p_notready <- predict_platt_probability(
        platt_model = calibration_model,
        raw_p = p_notready
      )
    }

    return(clamp_probability(p_notready))
  }

  if (inherits(model, "xgb.Booster")) {
    mat <- as.matrix(newdata)
    colnames(mat) <- predictor_set

    xp_raw <- as.numeric(
      predict(
        model,
        newdata = xgboost::xgb.DMatrix(data = mat)
      )
    )

    xp_cal <- predict_platt_probability(calibration_model, xp_raw)
    return(clamp_probability(xp_cal))
  }

  stop("Unsupported model object: ", paste(class(model), collapse = ", "))
}

empty_score_result <- function(feature_error = TRUE,
                               prediction_error = TRUE,
                               message = NA_character_,
                               raw_features = NULL,
                               model_features = NULL) {
  list(
    EpiFORESS = NA_real_,
    P_Stat_NotReady = NA_real_,
    P_ML_NotReady = NA_real_,
    P_Hybrid_NotReady = NA_real_,
    P_Ensemble_Ready = NA_real_,
    Feature_Error = feature_error,
    Prediction_Error = prediction_error,
    Score_Status = "Failed",
    Score_Message = message,
    Raw_Features = raw_features,
    Model_Features = model_features
  )
}

compute_epiforess_score <- function(y,
                                    stat_model,
                                    ml_model,
                                    ml_calibration_model = NULL,
                                    frequency = 52,
                                    w_stat,
                                    w_ml,
                                    stat_predictors,
                                    ml_predictors,
                                    stat_medians = NULL,
                                    ml_medians = NULL) {
  feat_result <- tryCatch(
    compute_epiforess_features_49(y = y, frequency = frequency),
    error = function(e) {
      attr(e, "message") <- e$message
      NULL
    }
  )

  if (is.null(feat_result)) {
    return(empty_score_result(
      feature_error = TRUE,
      prediction_error = TRUE,
      message = "Feature extraction failed."
    ))
  }

  stat_x <- tryCatch(
    prepare_model_features(
      feat_result$model_features,
      predictor_set = stat_predictors,
      medians = stat_medians
    ),
    error = function(e) NULL
  )

  ml_x <- tryCatch(
    prepare_model_features(
      feat_result$model_features,
      predictor_set = ml_predictors,
      medians = ml_medians
    ),
    error = function(e) NULL
  )

  if (is.null(stat_x) || is.null(ml_x)) {
    return(empty_score_result(
      feature_error = FALSE,
      prediction_error = TRUE,
      message = "Feature preparation failed after extraction.",
      raw_features = feat_result$raw_features,
      model_features = feat_result$model_features
    ))
  }

  stat_error <- NULL
  ml_error <- NULL

  p_stat <- tryCatch(
    predict_not_ready(
      stat_model,
      stat_x,
      predictor_set = stat_predictors,
      calibration_model = NULL
    ),
    error = function(e) {
      stat_error <<- e$message
      NA_real_
    }
  )

  p_ml <- tryCatch(
    predict_not_ready(
      ml_model,
      ml_x,
      predictor_set = ml_predictors,
      calibration_model = ml_calibration_model
    ),
    error = function(e) {
      ml_error <<- e$message
      NA_real_
    }
  )

  p_stat <- clamp_probability(p_stat)[1]
  p_ml <- clamp_probability(p_ml)[1]

  if (is.na(p_stat) || is.na(p_ml)) {
    msg <- paste(
      "Prediction failed.",
      paste0("Statistical error: ", stat_error %||% "none"),
      paste0("ML error: ", ml_error %||% "none")
    )

    return(empty_score_result(
      feature_error = FALSE,
      prediction_error = TRUE,
      message = msg,
      raw_features = feat_result$raw_features,
      model_features = feat_result$model_features
    ))
  }

  w_sum <- w_stat + w_ml

  if (!is.finite(w_sum) || w_sum <= 0) {
    w_stat <- 0.50
    w_ml <- 0.50
  } else {
    w_stat <- w_stat / w_sum
    w_ml <- w_ml / w_sum
  }

  p_hybrid_notready <- clamp_probability(
    w_stat * p_stat + w_ml * p_ml
  )[1]

  p_ensemble_ready <- 1 - p_hybrid_notready

  list(
    EpiFORESS = 100 * p_ensemble_ready,
    P_Stat_NotReady = p_stat,
    P_ML_NotReady = p_ml,
    P_Hybrid_NotReady = p_hybrid_notready,
    P_Ensemble_Ready = p_ensemble_ready,
    Feature_Error = FALSE,
    Prediction_Error = FALSE,
    Score_Status = "OK",
    Score_Message = "Score computed successfully.",
    Raw_Features = feat_result$raw_features,
    Model_Features = feat_result$model_features
  )
}

# ------------------------------------------------------------
# 5. LOAD FINAL MODEL CONFIGURATION
# ------------------------------------------------------------

if (!file.exists(epiforess_results_path)) {
  stop(
    "final_epiforess_results.rds not found: ",
    epiforess_results_path,
    "\nRun the updated main modeling script first."
  )
}

epiforess_results <- readRDS(epiforess_results_path)

best_statistical_name <- as.character(get_list_value(
  epiforess_results,
  c("best_statistical_name", "selected_statistical_model", "best_stat_name"),
  default = NA_character_
))[1]

best_ml_name <- as.character(get_list_value(
  epiforess_results,
  c("best_ml_name", "selected_ml_model", "best_machine_learning_name"),
  default = NA_character_
))[1]

if (is.na(best_statistical_name) || best_statistical_name == "") {
  stop("Could not identify the selected statistical component from final_epiforess_results.rds.")
}

if (is.na(best_ml_name) || best_ml_name == "") {
  stop("Could not identify the selected machine-learning component from final_epiforess_results.rds.")
}

if (grepl("^Binary_Logistic_Regression$", best_statistical_name, ignore.case = TRUE)) {
  stop(
    "The saved results still select Binary_Logistic_Regression, which was removed. ",
    "Rerun the updated main modeling script before empirical validation."
  )
}

not_ready_threshold_hybrid <- as.numeric(get_list_value(
  epiforess_results,
  c("hybrid_threshold", "best_hybrid_threshold"),
  default = fallback_notready_probability_threshold
))[1]

if (!is.finite(not_ready_threshold_hybrid) ||
    not_ready_threshold_hybrid <= 0 ||
    not_ready_threshold_hybrid >= 1) {
  warning(
    "Invalid saved hybrid threshold. Using fallback Not-Ready probability threshold: ",
    fallback_notready_probability_threshold
  )

  not_ready_threshold_hybrid <- fallback_notready_probability_threshold
}

epiforess_ready_threshold <- 100 * (1 - not_ready_threshold_hybrid)

comparison_thresholds <- sort(unique(c(
  20,
  30,
  40,
  50,
  60,
  round(epiforess_ready_threshold),
  70,
  80,
  90
)))

# ------------------------------------------------------------
# 6. LOAD FINAL HYBRID WEIGHTS
# ------------------------------------------------------------

weight_stat <- as.numeric(get_list_value(
  epiforess_results,
  c("weight_statistical", "final_weight_stat", "weight_stat", "w_stat"),
  default = NA_real_
))[1]

weight_ml <- as.numeric(get_list_value(
  epiforess_results,
  c("weight_ml", "final_weight_ml", "w_ml"),
  default = NA_real_
))[1]

if (is.finite(weight_stat) && is.finite(weight_ml) &&
    weight_stat > 0 && weight_ml > 0) {
  weight_source <- "Main-model validation inverse-Brier weights from final_epiforess_results.rds"
} else {
  lodo_results <- tryCatch(
    readRDS(lodo_results_path),
    error = function(e) NULL
  )

  if (!is.null(lodo_results) &&
      !is.null(lodo_results$lodo_weights) &&
      all(c("Fold_Weight_Statistical", "Fold_Weight_ML") %in%
          names(lodo_results$lodo_weights))) {
    weight_stat <- mean(
      lodo_results$lodo_weights$Fold_Weight_Statistical,
      na.rm = TRUE
    )

    weight_ml <- mean(
      lodo_results$lodo_weights$Fold_Weight_ML,
      na.rm = TRUE
    )

    weight_source <- "Fallback mean fold-specific inverse-Brier weights from LODO RDS"
  } else {
    weight_stat <- 0.50
    weight_ml <- 0.50
    weight_source <- "Equal-weight fallback because saved hybrid weights were unavailable"
  }
}

if (!is.finite(weight_stat) || !is.finite(weight_ml) ||
    weight_stat <= 0 || weight_ml <= 0) {
  weight_stat <- 0.50
  weight_ml <- 0.50
  weight_source <- "Equal-weight fallback because loaded weights were invalid"
}

weight_sum <- weight_stat + weight_ml
weight_stat <- weight_stat / weight_sum
weight_ml <- weight_ml / weight_sum

cat("Final EpiFORESS configuration:\n")
cat("  Statistical component:", best_statistical_name, "\n")
cat("  Machine-learning component:", best_ml_name, "\n")
cat("  Readiness rule: Best MASE <", empirical_mase_threshold, "\n")
cat("  Hybrid Not-Ready probability threshold:", round(not_ready_threshold_hybrid, 4), "\n")
cat("  Equivalent EpiFORESS score cutoff:", round(epiforess_ready_threshold, 2), "\n")
cat("  Statistical weight:", round(weight_stat, 4), "\n")
cat("  ML weight:", round(weight_ml, 4), "\n")
cat("  Weight source:", weight_source, "\n\n")

# ------------------------------------------------------------
# 7. FORECASTING MODELS
# Prophet removed. Negative Binomial Regression retained.
# ------------------------------------------------------------

fit_seasonal_naive <- function(train_ts, h) {
  as.numeric(
    forecast::forecast(
      forecast::snaive(train_ts),
      h = h
    )$mean
  )
}

fit_ets <- function(train_ts, h) {
  freq <- stats::frequency(train_ts)

  if (freq > 24 && length(train_ts) >= 2 * freq) {
    fc <- tryCatch(
      forecast::stlf(train_ts, h = h, method = "ets", robust = TRUE),
      error = function(e) NULL
    )

    if (!is.null(fc)) return(as.numeric(fc$mean))
  }

  as.numeric(
    forecast::forecast(
      forecast::ets(train_ts, model = "ZZN"),
      h = h
    )$mean
  )
}

fit_sarima <- function(train_ts, h) {
  as.numeric(
    forecast::forecast(
      forecast::auto.arima(
        train_ts,
        seasonal = TRUE,
        stepwise = TRUE,
        approximation = TRUE,
        max.p = 2,
        max.q = 2,
        max.P = 1,
        max.Q = 1,
        max.d = 1,
        max.D = 1,
        ic = "aic"
      ),
      h = h
    )$mean
  )
}

fit_arima_nonseasonal <- function(train_ts, h) {
  as.numeric(
    forecast::forecast(
      forecast::auto.arima(
        train_ts,
        seasonal = FALSE,
        stepwise = TRUE,
        approximation = TRUE,
        max.p = 2,
        max.q = 2,
        max.d = 1,
        ic = "aic"
      ),
      h = h
    )$mean
  )
}

fit_negbin_regression <- function(train_ts, h) {
  y <- as.numeric(train_ts)
  y <- y[is.finite(y)]
  n <- length(y)
  freq <- stats::frequency(train_ts)

  if (n < 30 || stats::sd(y, na.rm = TRUE) == 0) {
    return(rep(mean(y, na.rm = TRUE), h))
  }

  K <- if (freq > 1 && n >= 2 * freq) {
    min(3, floor(freq / 2) - 1)
  } else {
    0
  }

  if (K > 0) {
    xreg_train <- as.data.frame(forecast::fourier(train_ts, K = K))
    xreg_future <- as.data.frame(forecast::fourier(train_ts, K = K, h = h))
  } else {
    xreg_train <- data.frame()
    xreg_future <- data.frame()
  }

  xreg_train$t <- seq_len(n)
  xreg_future$t <- (n + 1):(n + h)

  train_df <- data.frame(
    y = pmax(round(y), 0),
    xreg_train
  )

  fit <- suppressWarnings(
    tryCatch(
      MASS::glm.nb(
        y ~ .,
        data = train_df,
        control = glm.control(maxit = 100)
      ),
      error = function(e) NULL
    )
  )

  if (is.null(fit)) {
    return(rep(mean(y, na.rm = TRUE), h))
  }

  pred <- tryCatch(
    as.numeric(stats::predict(fit, newdata = xreg_future, type = "response")),
    error = function(e) rep(NA_real_, h)
  )

  pred <- pmax(pred, 0)

  cap <- max(
    max(y, na.rm = TRUE) * 10,
    stats::quantile(y, 0.99, na.rm = TRUE, names = FALSE) * 20,
    mean(y, na.rm = TRUE) + 10 * stats::sd(y, na.rm = TRUE),
    1,
    na.rm = TRUE
  )

  pred[!is.finite(pred)] <- NA_real_
  pred[pred > cap] <- cap

  if (any(is.na(pred))) {
    pred[is.na(pred)] <- mean(y, na.rm = TRUE)
  }

  pred
}

evaluate_model <- function(series,
                           model_name,
                           forecast_function,
                           horizon = 13,
                           initial_window = 156,
                           frequency = 52,
                           rolling_step = 13,
                           max_origins = 6) {
  series <- as.numeric(series)
  n <- length(series)

  if (n < initial_window + horizon) {
    return(tibble::tibble(
      Model = model_name,
      MAE = NA_real_,
      RMSE = NA_real_,
      Bias = NA_real_,
      MASE = NA_real_,
      N_Forecasted = 0L,
      N_Origins = 0L
    ))
  }

  origins <- seq(initial_window, n - horizon, by = rolling_step)

  if (length(origins) > max_origins) {
    origins <- tail(origins, max_origins)
  }

  actual_all <- numeric(0)
  predicted_all <- numeric(0)
  mase_denom_all <- numeric(0)

  for (origin in origins) {
    train_raw <- series[1:origin]
    test <- series[(origin + 1):(origin + horizon)]

    if (sum(is.finite(train_raw)) < 20) next

    train_ts <- tryCatch(
      forecast::na.interp(stats::ts(train_raw, frequency = frequency)),
      error = function(e) NULL
    )

    if (is.null(train_ts)) next

    pred <- tryCatch(
      forecast_function(train_ts, horizon),
      error = function(e) rep(NA_real_, horizon)
    )

    pred <- pmax(as.numeric(pred), 0)
    pred[!is.finite(pred)] <- NA_real_

    denom <- safe_mase_denominator(train_ts, frequency = frequency)

    actual_all <- c(actual_all, test)
    predicted_all <- c(predicted_all, pred)
    mase_denom_all <- c(mase_denom_all, rep(denom, length(test)))
  }

  metric_row <- calculate_metrics(
    actual = actual_all,
    predicted = predicted_all,
    frequency = frequency,
    mase_denominators = mase_denom_all
  )

  metric_row %>%
    dplyr::mutate(
      Model = model_name,
      N_Forecasted = N_Evaluated,
      N_Origins = length(origins)
    ) %>%
    dplyr::select(Model, MAE, RMSE, Bias, MASE, N_Forecasted, N_Origins)
}

evaluate_disease_series <- function(counts,
                                    frequency = 52,
                                    horizon = 13,
                                    initial_window = 156,
                                    rolling_step = 13,
                                    max_origins = 6) {
  counts <- safe_numeric(counts)

  if (length(counts) < initial_window + horizon) return(NULL)
  if (sum(is.finite(counts)) < initial_window) return(NULL)
  if (stats::sd(counts, na.rm = TRUE) == 0) return(NULL)

  results <- dplyr::bind_rows(
    evaluate_model(
      counts,
      "Seasonal_Naive",
      fit_seasonal_naive,
      horizon,
      initial_window,
      frequency,
      rolling_step,
      max_origins
    ),
    evaluate_model(
      counts,
      "STL_ETS",
      fit_ets,
      horizon,
      initial_window,
      frequency,
      rolling_step,
      max_origins
    ),
    evaluate_model(
      counts,
      "SARIMA",
      fit_sarima,
      horizon,
      initial_window,
      frequency,
      rolling_step,
      max_origins
    ),
    evaluate_model(
      counts,
      "Nonseasonal_ARIMA",
      fit_arima_nonseasonal,
      horizon,
      initial_window,
      frequency,
      rolling_step,
      max_origins
    ),
    evaluate_model(
      counts,
      "NegBin_Regression",
      fit_negbin_regression,
      horizon,
      initial_window,
      frequency,
      rolling_step,
      max_origins
    )
  )

  best <- results %>%
    dplyr::filter(!is.na(MASE)) %>%
    dplyr::arrange(MASE, RMSE, MAE) %>%
    dplyr::slice(1)

  if (nrow(best) == 0) {
    best <- tibble::tibble(
      Model = NA_character_,
      MAE = NA_real_,
      RMSE = NA_real_,
      Bias = NA_real_,
      MASE = NA_real_,
      N_Forecasted = NA_real_,
      N_Origins = NA_real_
    )
  }

  list(
    all_models = results,
    best_model = best
  )
}

# ------------------------------------------------------------
# 8. RAW WORKBOOK READING
# ------------------------------------------------------------

read_long_raw_sheet <- function(path, sheet_name) {
  dat <- readxl::read_excel(path, sheet = sheet_name)
  names(dat) <- clean_colnames(names(dat))

  dc <- find_column(names(dat), disease_col_candidates)
  yc <- find_column(names(dat), year_col_candidates)
  wc <- find_column(names(dat), week_col_candidates)
  cc <- find_column(names(dat), preferred_count_cols)

  if (any(is.na(c(dc, yc, wc, cc)))) {
    stop(
      "Long-format sheet must have Disease, Year, Week, Cases. Found: ",
      paste(names(dat), collapse = ", ")
    )
  }

  out <- dat %>%
    dplyr::transmute(
      Disease = as.character(.data[[dc]]),
      Year = as.integer(safe_numeric(.data[[yc]])),
      Week = as.integer(safe_numeric(.data[[wc]])),
      Count = safe_numeric(.data[[cc]]),
      Count_Column = cc
    ) %>%
    dplyr::filter(!is.na(Disease), !is.na(Year), !is.na(Week))

  if (aggregate_weekly_duplicates) {
    out <- out %>%
      dplyr::group_by(Disease, Year, Week) %>%
      dplyr::summarise(
        Count = if (all(is.na(Count))) NA_real_ else sum(Count, na.rm = TRUE),
        Count_Column = dplyr::first(Count_Column),
        .groups = "drop"
      )
  }

  out %>%
    dplyr::arrange(Disease, Year, Week) %>%
    dplyr::group_by(Disease) %>%
    dplyr::mutate(Row_Index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(Disease, Count_Column, Year, Week, Row_Index, Count)
}

read_raw_workbook_flexibly <- function(path, raw_sheet = NULL) {
  if (!file.exists(path)) stop("Raw disease workbook not found: ", path)

  all_sheets <- readxl::excel_sheets(path)
  valid_sheets <- exclude_output_sheets(all_sheets)

  cat("Sheets found:\n")
  print(all_sheets)

  cat("\nSheets after exclusion:\n")
  print(valid_sheets)

  if (length(valid_sheets) == 0) stop("No valid raw sheets found.")

  if (!is.null(raw_sheet)) {
    sel <- all_sheets[normalize_name(all_sheets) == normalize_name(raw_sheet)][1]

    if (is.na(sel)) stop("Specified raw_sheet not found: ", raw_sheet)

    return(read_long_raw_sheet(path, sel))
  }

  lf_sheets <- valid_sheets[sapply(valid_sheets, function(s) {
    d <- tryCatch(
      readxl::read_excel(path, sheet = s, n_max = 50),
      error = function(e) NULL
    )

    !is.null(d) && has_long_format_columns(d)
  })]

  if (length(lf_sheets) >= 1) {
    if (length(lf_sheets) > 1) {
      warning("Multiple long-format sheets found. Using first: ", lf_sheets[1])
    }

    cat("\nAuto-detected long-format sheet:", lf_sheets[1], "\n")

    return(read_long_raw_sheet(path, lf_sheets[1]))
  }

  if (length(valid_sheets) > 1) {
    cat("\nNo long-format sheet found. Assuming one disease per sheet.\n")

    out_list <- list()

    for (s in valid_sheets) {
      dat <- tryCatch(
        readxl::read_excel(path, sheet = s),
        error = function(e) NULL
      )

      if (is.null(dat) || nrow(dat) == 0) {
        warning("Skipped empty or unreadable sheet: ", s)
        next
      }

      names(dat) <- clean_colnames(names(dat))

      cc <- detect_count_column(dat)
      yc <- find_column(names(dat), year_col_candidates)
      wc <- find_column(names(dat), week_col_candidates)

      if (is.na(cc)) {
        warning("No count column detected in: ", s)
        next
      }

      out_list[[s]] <- dat %>%
        dplyr::mutate(
          Disease = s,
          Count_Column = cc,
          Year = if (!is.na(yc)) {
            as.integer(safe_numeric(.data[[yc]]))
          } else {
            NA_integer_
          },
          Week = if (!is.na(wc)) {
            as.integer(safe_numeric(.data[[wc]]))
          } else {
            dplyr::row_number()
          },
          Count = safe_numeric(.data[[cc]])
        ) %>%
        dplyr::arrange(dplyr::coalesce(Year, 0L), Week) %>%
        dplyr::mutate(Row_Index = dplyr::row_number()) %>%
        dplyr::select(Disease, Count_Column, Year, Week, Row_Index, Count)
    }

    return(dplyr::bind_rows(out_list))
  }

  cat("\nSingle sheet detected. Assuming diseases as columns.\n")

  dat <- readxl::read_excel(path, sheet = valid_sheets[1])
  names(dat) <- clean_colnames(names(dat))

  numeric_cols <- names(dat)[sapply(dat, is_numeric_like)]
  time_present <- names(dat)[normalize_name(names(dat)) %in% normalize_name(possible_time_cols)]
  disease_cols <- setdiff(numeric_cols, time_present)

  if (length(disease_cols) == 0) stop("No disease count columns detected.")

  dplyr::bind_rows(lapply(disease_cols, function(col) {
    tibble::tibble(
      Disease = col,
      Count_Column = col,
      Year = NA_integer_,
      Week = seq_len(nrow(dat)),
      Row_Index = seq_len(nrow(dat)),
      Count = safe_numeric(dat[[col]])
    )
  }))
}

# ------------------------------------------------------------
# 9. LOAD DATA AND MODELS
# ------------------------------------------------------------

raw_series_data <- read_raw_workbook_flexibly(
  path = raw_workbook_path,
  raw_sheet = raw_sheet
) %>%
  dplyr::filter(!is.na(Disease), !is.na(Row_Index))

if (nrow(raw_series_data) == 0) stop("No usable disease series found.")

cat("\nDiseases detected:", length(unique(raw_series_data$Disease)), "\n")
cat("Rows detected:", nrow(raw_series_data), "\n")
print(unique(raw_series_data$Disease))

if (!file.exists(stat_model_path)) {
  stop("Statistical model not found: ", stat_model_path)
}

if (!file.exists(ml_model_path)) {
  stop("Machine-learning model not found: ", ml_model_path)
}

stat_model_raw <- readRDS(stat_model_path)
ml_model_raw <- readRDS(ml_model_path)

stat_model <- unwrap_model_object(stat_model_raw)
ml_model <- unwrap_model_object(ml_model_raw)

# Detect stale or mismatched saved model files before scoring.
stat_method <- if (inherits(stat_model, "train")) stat_model$method else NA_character_
ml_method <- if (inherits(ml_model, "train")) ml_model$method else NA_character_

if (grepl("LASSO|Elastic|Ridge", best_statistical_name, ignore.case = TRUE)) {
  valid_stat_object <-
    inherits(stat_model, "cv.glmnet") ||
    inherits(stat_model, "glmnet") ||
    (inherits(stat_model, "train") && identical(stat_method, "glmnet"))

  if (!valid_stat_object) {
    stop(
      "Saved statistical model does not match selected component '",
      best_statistical_name,
      "'. Rerun the updated main modeling script to refresh Saved_Models."
    )
  }
}

if (grepl("Stepwise|Bayesian|LowVIF", best_statistical_name, ignore.case = TRUE)) {
  valid_stat_object <- inherits(stat_model, "glm") || inherits(stat_model, "train")

  if (!valid_stat_object) {
    stop(
      "Saved statistical model does not match selected component '",
      best_statistical_name,
      "'. Rerun the updated main modeling script to refresh Saved_Models."
    )
  }
}

if (grepl("Ranger", best_ml_name, ignore.case = TRUE)) {
  if (!(inherits(ml_model, "train") && identical(ml_method, "ranger"))) {
    stop(
      "The final configuration selects Ranger, but best_machine_learning_model.rds ",
      "is not a caret Ranger model. Rerun the updated main modeling script."
    )
  }
}

if (grepl("XGBoost", best_ml_name, ignore.case = TRUE)) {
  valid_xgb_object <-
    inherits(ml_model, "xgb.Booster") ||
    (inherits(ml_model, "train") && identical(ml_method, "xgbTree"))

  if (!valid_xgb_object) {
    stop(
      "The final configuration selects XGBoost, but best_machine_learning_model.rds ",
      "is not an XGBoost model. Rerun the updated main modeling script."
    )
  }
}

ml_calibration_model <- NULL

if (grepl("XGBoost", best_ml_name, ignore.case = TRUE)) {
  if (file.exists(xgb_calibration_path)) {
    ml_calibration_model <- readRDS(xgb_calibration_path)
  } else {
    warning(
      "XGBoost is the selected ML model, but its Platt calibration object was not found: ",
      xgb_calibration_path,
      ". Raw XGBoost probabilities will be used."
    )
  }
} else {
  cat(
    "Selected ML model is not XGBoost; XGBoost calibration will not be loaded or applied.\n"
  )
}

stat_predictors <- get_model_predictor_names(stat_model_raw, predictor_vars)
ml_predictors <- get_model_predictor_names(ml_model_raw, predictor_vars)

stat_predictors <- intersect(stat_predictors, predictor_vars)
ml_predictors <- intersect(ml_predictors, predictor_vars)

if (length(stat_predictors) == 0) stat_predictors <- predictor_vars
if (length(ml_predictors) == 0) ml_predictors <- predictor_vars

median_object_candidates <- c(
  "imputation_medians",
  "train_medians",
  "training_medians",
  "predictor_medians",
  "median_imputation_values",
  "final_imputation_medians",
  "preprocessing_medians"
)

median_source_name <- median_object_candidates[
  vapply(
    median_object_candidates,
    function(nm) !is.null(epiforess_results[[nm]]),
    logical(1)
  )
][1]

if (is.na(median_source_name) || length(median_source_name) == 0) {
  stop(
    "No training-set imputation medians were found in final_epiforess_results.rds. ",
    "Rerun the updated main modeling script before empirical scoring."
  )
}

stat_medians <- extract_imputation_medians(epiforess_results, stat_predictors)
ml_medians <- extract_imputation_medians(epiforess_results, ml_predictors)

model_configuration <- tibble::tibble(
  Item = c(
    "Best_Statistical_Model",
    "Best_Machine_Learning_Model",
    "Statistical_Model_Class",
    "ML_Model_Class",
    "ML_Calibration_Applied",
    "Empirical_MASE_Threshold",
    "Hybrid_NotReady_Threshold",
    "Equivalent_EpiFORESS_Cutoff",
    "Weight_Statistical",
    "Weight_ML",
    "Weight_Source",
    "Imputation_Median_Source",
    "Statistical_Predictor_Count",
    "ML_Predictor_Count",
    "Log1p_Applied"
  ),
  Value = as.character(c(
    best_statistical_name,
    best_ml_name,
    paste(class(stat_model), collapse = ", "),
    paste(class(ml_model), collapse = ", "),
    !is.null(ml_calibration_model),
    empirical_mase_threshold,
    not_ready_threshold_hybrid,
    epiforess_ready_threshold,
    weight_stat,
    weight_ml,
    weight_source,
    median_source_name,
    length(stat_predictors),
    length(ml_predictors),
    apply_log1p_to_model_features
  ))
)

cat("\nModels loaded.\n")
cat("Statistical model:", best_statistical_name, "\n")
cat("Statistical model class:", paste(class(stat_model), collapse = ", "), "\n")
cat("ML model:", best_ml_name, "\n")
cat("ML model class:", paste(class(ml_model), collapse = ", "), "\n")
cat("ML calibration loaded:", !is.null(ml_calibration_model), "\n")
cat("Statistical predictors:", length(stat_predictors), "\n")
cat("ML predictors:", length(ml_predictors), "\n")
cat("Hybrid weights: stat =", round(weight_stat, 4), "| ML =", round(weight_ml, 4), "\n")
cat("Log1p applied to empirical model features:", apply_log1p_to_model_features, "\n\n")

# ------------------------------------------------------------
# 10. RUN FORECASTING + EPIFORESS SCORING
# ------------------------------------------------------------

all_model_results <- list()
best_model_summary <- list()
series_metadata <- list()
epiforess_scores <- list()
raw_feature_rows <- list()
model_feature_rows <- list()

disease_names <- sort(unique(raw_series_data$Disease))

for (disease_name in disease_names) {
  cat("\nProcessing:", disease_name, "\n")

  disease_data <- raw_series_data %>%
    dplyr::filter(Disease == disease_name) %>%
    dplyr::arrange(Row_Index)

  counts <- disease_data$Count
  count_col <- disease_data$Count_Column[1]

  evaluation <- evaluate_disease_series(
    counts = counts,
    frequency = seasonal_frequency,
    horizon = forecast_horizon,
    initial_window = initial_window,
    rolling_step = rolling_step,
    max_origins = max_origins
  )

  if (is.null(evaluation)) {
    warning("Insufficient usable observations for: ", disease_name)
    next
  }

  disease_all <- evaluation$all_models %>%
    dplyr::mutate(
      Disease = disease_name,
      Count_Column = count_col,
      N_Total_Rows = length(counts),
      N_Observed = sum(!is.na(counts)),
      .before = 1
    )

  disease_best <- evaluation$best_model %>%
    dplyr::mutate(
      Disease = disease_name,
      Count_Column = count_col,
      N_Total_Rows = length(counts),
      N_Observed = sum(!is.na(counts)),
      .before = 1
    ) %>%
    dplyr::rename(
      Best_Model = Model,
      Best_MAE = MAE,
      Best_RMSE = RMSE,
      Best_Bias = Bias,
      Best_MASE = MASE
    )

  disease_meta <- tibble::tibble(
    Disease = disease_name,
    Count_Column = count_col,
    N_Total_Rows = length(counts),
    N_Observed = sum(!is.na(counts)),
    Mean_Cases = mean(counts, na.rm = TRUE),
    SD_Cases = stats::sd(counts, na.rm = TRUE),
    Min_Cases = if (all(is.na(counts))) NA_real_ else min(counts, na.rm = TRUE),
    Max_Cases = if (all(is.na(counts))) NA_real_ else max(counts, na.rm = TRUE),
    Proportion_Zero = mean(counts == 0, na.rm = TRUE),
    Missingness_Rate = mean(is.na(counts))
  )

  score_result <- tryCatch(
    compute_epiforess_score(
      y = counts,
      stat_model = stat_model_raw,
      ml_model = ml_model_raw,
      ml_calibration_model = ml_calibration_model,
      frequency = seasonal_frequency,
      w_stat = weight_stat,
      w_ml = weight_ml,
      stat_predictors = stat_predictors,
      ml_predictors = ml_predictors,
      stat_medians = stat_medians,
      ml_medians = ml_medians
    ),
    error = function(e) {
      empty_score_result(
        feature_error = TRUE,
        prediction_error = TRUE,
        message = e$message
      )
    }
  )

  disease_score <- tibble::tibble(
    Disease = disease_name,
    EpiFORESS = score_result$EpiFORESS,
    EpiFORESS_Band = readiness_band(score_result$EpiFORESS),
    Predicted_Readiness = dplyr::case_when(
      is.na(score_result$EpiFORESS) ~ NA_character_,
      score_result$EpiFORESS > epiforess_ready_threshold ~ "Ready",
      TRUE ~ "Not_Ready"
    ),
    EpiFORESS_Ready_Cutoff = epiforess_ready_threshold,
    Hybrid_NotReady_Probability_Cutoff = not_ready_threshold_hybrid,
    P_Stat_NotReady = score_result$P_Stat_NotReady,
    P_ML_NotReady = score_result$P_ML_NotReady,
    P_Hybrid_NotReady = score_result$P_Hybrid_NotReady,
    P_Ensemble_Ready = score_result$P_Ensemble_Ready,
    Statistical_Model = best_statistical_name,
    Machine_Learning_Model = best_ml_name,
    ML_Calibration_Applied = !is.null(ml_calibration_model),
    Weight_Statistical = weight_stat,
    Weight_ML = weight_ml,
    Weight_Method = weight_source,
    Score_Source = "Computed internally from the final 49-feature empirical extraction",
    Feature_Error = score_result$Feature_Error,
    Prediction_Error = score_result$Prediction_Error,
    Score_Status = score_result$Score_Status,
    Score_Message = score_result$Score_Message
  )

  if (!is.null(score_result$Raw_Features)) {
    raw_feature_rows[[disease_name]] <- score_result$Raw_Features %>%
      dplyr::mutate(Disease = disease_name, .before = 1)
  }

  if (!is.null(score_result$Model_Features)) {
    model_feature_rows[[disease_name]] <- score_result$Model_Features %>%
      dplyr::mutate(Disease = disease_name, .before = 1)
  }

  all_model_results[[disease_name]] <- disease_all
  best_model_summary[[disease_name]] <- disease_best
  series_metadata[[disease_name]] <- disease_meta
  epiforess_scores[[disease_name]] <- disease_score

  cat(
    "  Best model:", disease_best$Best_Model,
    "| MASE:", round(disease_best$Best_MASE, 4),
    "| EpiFORESS:", ifelse(
      is.na(score_result$EpiFORESS),
      "NA",
      round(score_result$EpiFORESS, 2)
    ),
    "| Score status:", score_result$Score_Status,
    "\n"
  )

  if (!is.na(score_result$Score_Message) && score_result$Score_Status != "OK") {
    cat("  Score message:", score_result$Score_Message, "\n")
  }
}

all_model_results <- dplyr::bind_rows(all_model_results)

best_model_summary <- dplyr::bind_rows(best_model_summary) %>%
  dplyr::select(
    Disease,
    Count_Column,
    N_Total_Rows,
    N_Observed,
    Best_Model,
    Best_MAE,
    Best_RMSE,
    Best_Bias,
    Best_MASE,
    N_Forecasted,
    N_Origins
  )

series_metadata <- dplyr::bind_rows(series_metadata)
epiforess_scores <- dplyr::bind_rows(epiforess_scores)
raw_feature_table <- dplyr::bind_rows(raw_feature_rows)
model_feature_table <- dplyr::bind_rows(model_feature_rows)

if (nrow(best_model_summary) == 0) {
  stop("No disease had enough observations for forecasting evaluation.")
}

# ------------------------------------------------------------
# 11. FORECASTING SUMMARY
# ------------------------------------------------------------

winning_model_distribution <- best_model_summary %>%
  dplyr::count(Best_Model, name = "Frequency") %>%
  dplyr::mutate(
    Percent = round(100 * Frequency / sum(Frequency), 2)
  ) %>%
  dplyr::arrange(dplyr::desc(Frequency))

overall_summary <- tibble::tibble(
  N_Diseases = nrow(best_model_summary),
  Mean_Best_MASE = mean(best_model_summary$Best_MASE, na.rm = TRUE),
  Median_Best_MASE = stats::median(best_model_summary$Best_MASE, na.rm = TRUE),
  SD_Best_MASE = stats::sd(best_model_summary$Best_MASE, na.rm = TRUE),
  Min_Best_MASE = suppressWarnings(min(best_model_summary$Best_MASE, na.rm = TRUE)),
  Max_Best_MASE = suppressWarnings(max(best_model_summary$Best_MASE, na.rm = TRUE)),
  Mean_Best_MAE = mean(best_model_summary$Best_MAE, na.rm = TRUE),
  Mean_Best_RMSE = mean(best_model_summary$Best_RMSE, na.rm = TRUE),
  Forecast_Ready_Count = sum(
    best_model_summary$Best_MASE < empirical_mase_threshold,
    na.rm = TRUE
  ),
  Not_Forecast_Ready_Count = sum(
    best_model_summary$Best_MASE >= empirical_mase_threshold,
    na.rm = TRUE
  )
) %>%
  dplyr::mutate(
    Min_Best_MASE = dplyr::if_else(is.infinite(Min_Best_MASE), NA_real_, Min_Best_MASE),
    Max_Best_MASE = dplyr::if_else(is.infinite(Max_Best_MASE), NA_real_, Max_Best_MASE)
  )

# ------------------------------------------------------------
# 11A. SAVE ALL FORECASTING MODEL METRICS
# ------------------------------------------------------------

all_forecast_model_metrics <- all_model_results %>%
  dplyr::mutate(
    Forecast_Readiness = dplyr::case_when(
      is.na(MASE) ~ NA_character_,
      MASE < empirical_mase_threshold ~ "Ready",
      TRUE ~ "Not_Ready"
    )
  ) %>%
  dplyr::arrange(Disease, MASE, RMSE, MAE)

forecast_model_rankings <- all_model_results %>%
  dplyr::group_by(Disease) %>%
  dplyr::arrange(MASE, RMSE, MAE, .by_group = TRUE) %>%
  dplyr::mutate(
    Model_Rank_By_MASE = dplyr::if_else(
      is.na(MASE),
      NA_integer_,
      dplyr::row_number()
    ),
    Is_Best_Model = !is.na(Model_Rank_By_MASE) & Model_Rank_By_MASE == 1
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Disease, Model_Rank_By_MASE)

forecast_model_summary_by_model <- all_model_results %>%
  dplyr::group_by(Model) %>%
  dplyr::summarise(
    N_Diseases_Evaluated = sum(!is.na(MASE)),
    N_Total_Diseases = dplyr::n_distinct(Disease),

    Mean_MAE = mean(MAE, na.rm = TRUE),
    Median_MAE = median(MAE, na.rm = TRUE),
    SD_MAE = stats::sd(MAE, na.rm = TRUE),

    Mean_RMSE = mean(RMSE, na.rm = TRUE),
    Median_RMSE = median(RMSE, na.rm = TRUE),
    SD_RMSE = stats::sd(RMSE, na.rm = TRUE),

    Mean_Bias = mean(Bias, na.rm = TRUE),
    Median_Bias = median(Bias, na.rm = TRUE),
    SD_Bias = stats::sd(Bias, na.rm = TRUE),

    Mean_MASE = mean(MASE, na.rm = TRUE),
    Median_MASE = median(MASE, na.rm = TRUE),
    SD_MASE = stats::sd(MASE, na.rm = TRUE),
    Min_MASE = suppressWarnings(min(MASE, na.rm = TRUE)),
    Max_MASE = suppressWarnings(max(MASE, na.rm = TRUE)),

    N_Forecast_Ready = sum(
      MASE < empirical_mase_threshold,
      na.rm = TRUE
    ),

    Forecast_Ready_Rate = dplyr::if_else(
      N_Diseases_Evaluated > 0,
      N_Forecast_Ready / N_Diseases_Evaluated,
      NA_real_
    ),

    Mean_N_Forecasted = mean(N_Forecasted, na.rm = TRUE),
    Mean_N_Origins = mean(N_Origins, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Min_MASE = dplyr::if_else(is.infinite(Min_MASE), NA_real_, Min_MASE),
    Max_MASE = dplyr::if_else(is.infinite(Max_MASE), NA_real_, Max_MASE)
  ) %>%
  dplyr::arrange(Mean_MASE)

forecast_model_summary_by_disease <- all_model_results %>%
  dplyr::group_by(Disease) %>%
  dplyr::group_modify(function(.x, .y) {
    valid <- .x %>%
      dplyr::filter(!is.na(MASE)) %>%
      dplyr::arrange(MASE, RMSE, MAE)

    if (nrow(valid) == 0) {
      return(tibble::tibble(
        N_Models_Evaluated = 0L,
        Best_Model = NA_character_,
        Best_MASE = NA_real_,
        Best_MAE = NA_real_,
        Best_RMSE = NA_real_,
        Best_Bias = NA_real_,
        Worst_Model = NA_character_,
        Worst_MASE = NA_real_,
        Mean_MASE_All_Models = NA_real_,
        Median_MASE_All_Models = NA_real_,
        SD_MASE_All_Models = NA_real_,
        Mean_MAE_All_Models = NA_real_,
        Median_MAE_All_Models = NA_real_,
        SD_MAE_All_Models = NA_real_,
        Mean_RMSE_All_Models = NA_real_,
        Median_RMSE_All_Models = NA_real_,
        SD_RMSE_All_Models = NA_real_,
        Mean_Bias_All_Models = NA_real_,
        Median_Bias_All_Models = NA_real_,
        SD_Bias_All_Models = NA_real_,
        MASE_Range = NA_real_
      ))
    }

    worst <- valid %>% dplyr::arrange(dplyr::desc(MASE)) %>% dplyr::slice(1)
    best <- valid %>% dplyr::slice(1)

    tibble::tibble(
      N_Models_Evaluated = nrow(valid),
      Best_Model = best$Model,
      Best_MASE = best$MASE,
      Best_MAE = best$MAE,
      Best_RMSE = best$RMSE,
      Best_Bias = best$Bias,
      Worst_Model = worst$Model,
      Worst_MASE = worst$MASE,
      Mean_MASE_All_Models = mean(valid$MASE, na.rm = TRUE),
      Median_MASE_All_Models = median(valid$MASE, na.rm = TRUE),
      SD_MASE_All_Models = stats::sd(valid$MASE, na.rm = TRUE),
      Mean_MAE_All_Models = mean(valid$MAE, na.rm = TRUE),
      Median_MAE_All_Models = median(valid$MAE, na.rm = TRUE),
      SD_MAE_All_Models = stats::sd(valid$MAE, na.rm = TRUE),
      Mean_RMSE_All_Models = mean(valid$RMSE, na.rm = TRUE),
      Median_RMSE_All_Models = median(valid$RMSE, na.rm = TRUE),
      SD_RMSE_All_Models = stats::sd(valid$RMSE, na.rm = TRUE),
      Mean_Bias_All_Models = mean(valid$Bias, na.rm = TRUE),
      Median_Bias_All_Models = median(valid$Bias, na.rm = TRUE),
      SD_Bias_All_Models = stats::sd(valid$Bias, na.rm = TRUE),
      MASE_Range = worst$MASE - best$MASE
    )
  }) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Best_MASE)

forecast_model_metrics_wide <- all_model_results %>%
  dplyr::select(
    Disease,
    Model,
    MAE,
    RMSE,
    Bias,
    MASE,
    N_Forecasted,
    N_Origins
  ) %>%
  tidyr::pivot_wider(
    names_from = Model,
    values_from = c(
      MAE,
      RMSE,
      Bias,
      MASE,
      N_Forecasted,
      N_Origins
    ),
    names_glue = "{Model}_{.value}"
  )

forecast_model_winner_diagnostics <- forecast_model_rankings %>%
  dplyr::filter(Is_Best_Model) %>%
  dplyr::count(Model, name = "Win_Count") %>%
  dplyr::mutate(
    Win_Percent = round(100 * Win_Count / sum(Win_Count), 2)
  ) %>%
  dplyr::arrange(dplyr::desc(Win_Count))

forecast_model_metric_dictionary <- tibble::tibble(
  Metric = c(
    "MAE",
    "RMSE",
    "Bias",
    "MASE",
    "N_Forecasted",
    "N_Origins",
    "Forecast_Readiness",
    "Model_Rank_By_MASE",
    "Is_Best_Model"
  ),
  Meaning = c(
    "Mean absolute error across observed rolling-origin forecast targets.",
    "Root mean squared error across observed rolling-origin forecast targets.",
    "Mean forecast bias, computed as predicted minus actual.",
    "Mean absolute scaled error using the seasonal or fallback naive denominator.",
    "Total observed forecast targets evaluated across rolling origins.",
    "Number of rolling-origin validation windows attempted.",
    paste0("Ready if model-specific MASE < ", empirical_mase_threshold, "."),
    "Within-disease ranking of candidate forecasting models by MASE, then RMSE, then MAE.",
    "TRUE if the model achieved the lowest valid MASE for that disease."
  )
)

# ------------------------------------------------------------
# 11B. BEST MASE PLOT
# ------------------------------------------------------------

plot_data <- best_model_summary %>%
  dplyr::arrange(Best_MASE) %>%
  dplyr::mutate(Disease = factor(Disease, levels = Disease))

best_mase_plot <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(
    x = Disease,
    y = Best_MASE,
    fill = Best_MASE < empirical_mase_threshold
  )
) +
  ggplot2::geom_col(show.legend = TRUE) +
  ggplot2::scale_fill_manual(
    values = c("TRUE" = "#087E8B", "FALSE" = "#B0BEC5"),
    labels = c(
      "TRUE" = paste0("MASE < ", empirical_mase_threshold),
      "FALSE" = paste0("MASE >= ", empirical_mase_threshold)
    ),
    name = NULL
  ) +
  ggplot2::geom_hline(
    yintercept = empirical_mase_threshold,
    linetype = "dashed",
    color = "#333333",
    linewidth = 0.7
  ) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Best Forecasting Performance by Disease",
    subtitle = paste0(
      "Forecast-ready if Best MASE < ",
      empirical_mase_threshold
    ),
    x = "Disease",
    y = "Best MASE"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  plot_best_mase_path,
  plot = best_mase_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 11C. SAVE FORECASTING AND SCORE OUTPUTS
# ------------------------------------------------------------

write_tables_to_workbook(
  tables = list(
    Model_Configuration = model_configuration,
    All_Model_Results = all_model_results,
    All_Model_Metrics = all_forecast_model_metrics,
    Model_Rankings = forecast_model_rankings,
    Model_Summary_By_Model = forecast_model_summary_by_model,
    Model_Summary_By_Disease = forecast_model_summary_by_disease,
    Model_Metrics_Wide = forecast_model_metrics_wide,
    Model_Winner_Diagnostics = forecast_model_winner_diagnostics,
    Model_Metric_Dictionary = forecast_model_metric_dictionary,
    Best_Model_Summary = best_model_summary,
    Series_Metadata = series_metadata,
    Winning_Model_Dist = winning_model_distribution,
    Overall_Summary = overall_summary
  ),
  path = forecast_output_path
)

write_tables_to_workbook(
  tables = list(
    Model_Configuration = model_configuration,
    EpiFORESS_Scores = epiforess_scores,
    Raw_Features_49 = raw_feature_table,
    Model_Features_49 = model_feature_table
  ),
  path = score_output_path
)

cat("\nForecasting output saved to:\n", forecast_output_path, "\n")
cat("EpiFORESS scores saved to:\n", score_output_path, "\n")
cat("Best MASE plot saved to:\n", plot_best_mase_path, "\n")

# ============================================================
# 12. EMPIRICAL VALIDATION
# ============================================================

validation_data_all <- best_model_summary %>%
  dplyr::mutate(Disease_Key = normalize_name(Disease)) %>%
  dplyr::left_join(
    epiforess_scores %>%
      dplyr::mutate(Disease_Key = normalize_name(Disease)) %>%
      dplyr::select(
        Disease_Key,
        EpiFORESS,
        EpiFORESS_Band,
        Predicted_Readiness,
        P_Ensemble_Ready,
        P_Stat_NotReady,
        P_ML_NotReady,
        P_Hybrid_NotReady,
        Statistical_Model,
        Machine_Learning_Model,
        Score_Status,
        Score_Message,
        Feature_Error,
        Prediction_Error
      ),
    by = "Disease_Key"
  ) %>%
  dplyr::filter(!is.na(Best_MASE), !is.na(EpiFORESS)) %>%
  dplyr::mutate(
    Disease = as.character(Disease),
    Best_MASE = as.numeric(Best_MASE),
    EpiFORESS = as.numeric(EpiFORESS),
    Excluded_From_Sensitivity = Disease %in% hard_empirical_diseases,
    Exclusion_Reason = dplyr::if_else(
      Excluded_From_Sensitivity,
      hard_empirical_exclusion_note,
      "Included in all empirical analyses"
    )
  ) %>%
  dplyr::select(-Disease_Key)

unmatched_or_failed_scores <- best_model_summary %>%
  dplyr::mutate(Disease_Key = normalize_name(Disease)) %>%
  dplyr::left_join(
    epiforess_scores %>%
      dplyr::mutate(Disease_Key = normalize_name(Disease)) %>%
      dplyr::select(
        Disease_Key,
        EpiFORESS,
        Score_Status,
        Score_Message,
        Feature_Error,
        Prediction_Error
      ),
    by = "Disease_Key"
  ) %>%
  dplyr::filter(is.na(EpiFORESS)) %>%
  dplyr::select(
    Disease,
    Best_Model,
    Best_MASE,
    EpiFORESS,
    Score_Status,
    Score_Message,
    Feature_Error,
    Prediction_Error
  )

excluded_empirical_diseases <- validation_data_all %>%
  dplyr::filter(Excluded_From_Sensitivity) %>%
  dplyr::select(
    Disease,
    Best_Model,
    Best_MASE,
    EpiFORESS,
    P_Stat_NotReady,
    P_ML_NotReady,
    P_Hybrid_NotReady,
    P_Ensemble_Ready,
    Exclusion_Reason
  ) %>%
  dplyr::arrange(dplyr::desc(Best_MASE))

validation_data_sensitivity <- if (run_exclusion_sensitivity) {
  validation_data_all %>%
    dplyr::filter(!Excluded_From_Sensitivity)
} else {
  tibble::tibble()
}

cat("\nEmpirical validation matching:\n")
cat("  Total matched diseases with valid EpiFORESS:", nrow(validation_data_all), "\n")
cat("  Failed or unmatched scores:", nrow(unmatched_or_failed_scores), "\n")
cat("  Optional disease-exclusion sensitivity enabled:", run_exclusion_sensitivity, "\n")
cat("  Named disease exclusions:", nrow(excluded_empirical_diseases), "\n")
cat(
  "  Diseases retained in optional disease-exclusion sensitivity:",
  nrow(validation_data_sensitivity),
  "\n\n"
)

if (nrow(unmatched_or_failed_scores) > 0) {
  cat("Diseases with failed or missing EpiFORESS scores:\n")
  print(unmatched_or_failed_scores)
}

if (nrow(excluded_empirical_diseases) > 0) {
  cat("Diseases marked for optional disease-exclusion sensitivity:\n")
  print(excluded_empirical_diseases)
}

run_validation_analysis <- function(data, dataset_label) {
  if (nrow(data) < 3) {
    warning("Fewer than 3 diseases available for: ", dataset_label)
    return(NULL)
  }

  analysis_data <- data %>%
    dplyr::mutate(
      Empirical_Readiness_Primary = factor(
        dplyr::if_else(
          Best_MASE < empirical_mase_threshold,
          "Ready",
          "Not_Ready"
        ),
        levels = c("Ready", "Not_Ready")
      ),
      Predicted_Readiness = factor(
        dplyr::if_else(
          EpiFORESS > epiforess_ready_threshold,
          "Ready",
          "Not_Ready"
        ),
        levels = c("Ready", "Not_Ready")
      ),
      EpiFORESS_Group = factor(
        readiness_band(EpiFORESS),
        levels = c("Very_Low", "Low", "Moderate", "High", "Very_High"),
        ordered = TRUE
      )
    )

  # ----------------------------------------------------------
  # A. CORRELATION AND LINEAR REGRESSION
  # ----------------------------------------------------------

  pearson_test <- tryCatch(
    stats::cor.test(
      analysis_data$EpiFORESS,
      analysis_data$Best_MASE,
      method = "pearson"
    ),
    error = function(e) NULL
  )

  spearman_test <- tryCatch(
    suppressWarnings(stats::cor.test(
      analysis_data$EpiFORESS,
      analysis_data$Best_MASE,
      method = "spearman",
      exact = FALSE
    )),
    error = function(e) NULL
  )

  correlation_summary <- tibble::tibble(
    Dataset = dataset_label,
    Method = c("Pearson", "Spearman"),
    Correlation = c(
      if (is.null(pearson_test)) NA_real_ else unname(pearson_test$estimate),
      if (is.null(spearman_test)) NA_real_ else unname(spearman_test$estimate)
    ),
    P_Value = c(
      if (is.null(pearson_test)) NA_real_ else pearson_test$p.value,
      if (is.null(spearman_test)) NA_real_ else spearman_test$p.value
    ),
    N = nrow(analysis_data),
    Expected_Direction =
      "Negative: higher EpiFORESS should correspond to lower Best_MASE"
  )

  linear_model <- tryCatch(
    stats::lm(Best_MASE ~ EpiFORESS, data = analysis_data),
    error = function(e) NULL
  )

  linear_model_summary <- if (!is.null(linear_model)) {
    broom::tidy(linear_model, conf.int = TRUE) %>%
      dplyr::mutate(Dataset = dataset_label, .before = 1)
  } else {
    tibble::tibble()
  }

  linear_model_fit <- if (!is.null(linear_model)) {
    broom::glance(linear_model) %>%
      dplyr::mutate(Dataset = dataset_label, .before = 1)
  } else {
    tibble::tibble()
  }

  shapiro_result <- if (!is.null(linear_model)) {
    tryCatch(
      stats::shapiro.test(stats::residuals(linear_model)),
      error = function(e) NULL
    )
  } else {
    NULL
  }

  bp_result <- if (!is.null(linear_model)) {
    tryCatch(lmtest::bptest(linear_model), error = function(e) NULL)
  } else {
    NULL
  }

  cooks_d <- if (!is.null(linear_model)) {
    stats::cooks.distance(linear_model)
  } else {
    rep(NA_real_, nrow(analysis_data))
  }

  assumption_summary <- tibble::tibble(
    Dataset = dataset_label,
    Test = c("Shapiro-Wilk", "Breusch-Pagan", "Maximum Cook's Distance"),
    Statistic = c(
      ifelse(is.null(shapiro_result), NA_real_, unname(shapiro_result$statistic)),
      ifelse(is.null(bp_result), NA_real_, unname(bp_result$statistic)),
      if (all(is.na(cooks_d))) NA_real_ else max(cooks_d, na.rm = TRUE)
    ),
    P_Value = c(
      ifelse(is.null(shapiro_result), NA_real_, shapiro_result$p.value),
      ifelse(is.null(bp_result), NA_real_, bp_result$p.value),
      NA_real_
    )
  )

  influence_summary <- analysis_data %>%
    dplyr::mutate(
      Dataset = dataset_label,
      Cooks_Distance = cooks_d,
      Potentially_Influential = cooks_d > (4 / nrow(analysis_data)),
      .before = 1
    ) %>%
    dplyr::arrange(dplyr::desc(Cooks_Distance))

  # ----------------------------------------------------------
  # B. READINESS CLASSIFICATION: MASE < 1.00 ONLY
  # ----------------------------------------------------------

  empirical_auc_ci_primary <- compute_empirical_auc_ci(
    analysis_data$Empirical_Readiness_Primary,
    analysis_data$EpiFORESS,
    boot_n = bootstrap_n
  )

  brier_primary <- compute_brier_score_empirical(
    analysis_data$Empirical_Readiness_Primary,
    analysis_data$EpiFORESS
  )

  build_class_summary <- function(readiness_col,
                                  auc_ci,
                                  brier_val,
                                  mase_thr,
                                  label) {
    cr <- make_classification_metrics(
      actual = analysis_data[[readiness_col]],
      predicted = analysis_data$Predicted_Readiness
    )

    cr$metrics %>%
      dplyr::mutate(
        Dataset = dataset_label,
        Criterion = label,
        MASE_Threshold = mase_thr,
        Hybrid_NotReady_Probability_Threshold = not_ready_threshold_hybrid,
        EpiFORESS_Threshold = epiforess_ready_threshold,
        Rule = paste0("Ready if EpiFORESS > ", round(epiforess_ready_threshold, 2)),
        AUC_ROC = auc_ci$AUC,
        AUC_CI_Lower_95 = auc_ci$AUC_CI_Lower,
        AUC_CI_Upper_95 = auc_ci$AUC_CI_Upper,
        Brier_Score = brier_val,
        Brier_Note = "Probability of Ready is EpiFORESS/100; lower Brier is better",
        .before = 1
      )
  }

  classification_summary <- build_class_summary(
    "Empirical_Readiness_Primary",
    empirical_auc_ci_primary,
    brier_primary,
    empirical_mase_threshold,
    paste0("Ready if Best MASE < ", empirical_mase_threshold)
  )

  auc_ci_summary <- empirical_auc_ci_primary %>%
    dplyr::mutate(
      Dataset = dataset_label,
      Criterion = paste0(
        "Ready if Best MASE < ",
        empirical_mase_threshold
      ),
      .before = 1
    )

  confusion_primary <- make_classification_metrics(
    analysis_data$Empirical_Readiness_Primary,
    analysis_data$Predicted_Readiness
  )$confusion_table

  # ----------------------------------------------------------
  # C. THRESHOLD SWEEP: MASE < 1.00 ONLY
  # ----------------------------------------------------------

  score_threshold_grid <- seq(0, 100, by = 1)

  sweep_fn <- function(readiness_col, criterion_label) {
    purrr::map_dfr(score_threshold_grid, function(thresh) {
      pred <- dplyr::if_else(
        analysis_data$EpiFORESS > thresh,
        "Ready",
        "Not_Ready"
      )

      make_classification_metrics(
        actual = analysis_data[[readiness_col]],
        predicted = pred
      )$metrics %>%
        dplyr::mutate(
          Dataset = dataset_label,
          Criterion = criterion_label,
          Threshold = thresh,
          .before = 1
        )
    })
  }

  threshold_sweep_primary <- sweep_fn(
    "Empirical_Readiness_Primary",
    paste0("Ready if Best MASE < ", empirical_mase_threshold)
  )

  best_threshold_primary <- threshold_sweep_primary %>%
    dplyr::filter(is.finite(Selection_Score)) %>%
    dplyr::arrange(
      dplyr::desc(Selection_Score),
      dplyr::desc(Balanced_Accuracy),
      dplyr::desc(F1_NotReady),
      abs(Threshold - epiforess_ready_threshold)
    ) %>%
    dplyr::slice(1)

  build_threshold_comparison <- function(readiness_col, criterion_label) {
    bs <- compute_brier_score_empirical(
      analysis_data[[readiness_col]],
      analysis_data$EpiFORESS
    )

    purrr::map_dfr(comparison_thresholds, function(thresh) {
      pred <- dplyr::if_else(
        analysis_data$EpiFORESS > thresh,
        "Ready",
        "Not_Ready"
      )

      make_classification_metrics(
        actual = analysis_data[[readiness_col]],
        predicted = pred
      )$metrics %>%
        dplyr::mutate(
          Dataset = dataset_label,
          Criterion = criterion_label,
          Threshold = thresh,
          Is_Simulation_Derived = abs(thresh - epiforess_ready_threshold) < 0.5,
          Brier_Score = bs,
          Brier_Note = "Threshold-independent; same across rows within criterion",
          N_Predicted_Ready = sum(pred == "Ready"),
          N_Predicted_NotReady = sum(pred == "Not_Ready"),
          .before = 1
        )
    })
  }

  threshold_comparison <- build_threshold_comparison(
    "Empirical_Readiness_Primary",
    paste0("Ready if Best MASE < ", empirical_mase_threshold)
  )

  # ----------------------------------------------------------
  # D. CALIBRATION AND ORDERED GROUP TESTS
  # ----------------------------------------------------------

  calibration_summary <- analysis_data %>%
    dplyr::group_by(EpiFORESS_Group) %>%
    dplyr::summarise(
      Dataset = dataset_label,
      N_Diseases = dplyr::n(),
      Mean_EpiFORESS = mean(EpiFORESS, na.rm = TRUE),
      Median_EpiFORESS = median(EpiFORESS, na.rm = TRUE),
      Mean_Best_MASE = mean(Best_MASE, na.rm = TRUE),
      Median_Best_MASE = median(Best_MASE, na.rm = TRUE),
      SD_Best_MASE = stats::sd(Best_MASE, na.rm = TRUE),
      Ready_Count_Primary = sum(
        Best_MASE < empirical_mase_threshold
      ),
      Ready_Rate_Primary = Ready_Count_Primary / N_Diseases,
      .groups = "drop"
    )

  kruskal_result <- tryCatch(
    stats::kruskal.test(Best_MASE ~ EpiFORESS_Group, data = analysis_data),
    error = function(e) NULL
  )

  kruskal_summary <- if (!is.null(kruskal_result)) {
    tibble::tibble(
      Dataset = dataset_label,
      Test = "Kruskal-Wallis",
      Statistic = unname(kruskal_result$statistic),
      DF = unname(kruskal_result$parameter),
      P_Value = kruskal_result$p.value
    )
  } else {
    tibble::tibble(
      Dataset = dataset_label,
      Test = "Kruskal-Wallis",
      Statistic = NA_real_,
      DF = NA_real_,
      P_Value = NA_real_
    )
  }

  dunn_posthoc <- tryCatch(
    FSA::dunnTest(
      Best_MASE ~ EpiFORESS_Group,
      data = analysis_data,
      method = "bh"
    )$res %>%
      tibble::as_tibble() %>%
      dplyr::mutate(Dataset = dataset_label, .before = 1),
    error = function(e) {
      tibble::tibble(
        Dataset = dataset_label,
        Comparison = NA_character_,
        Z = NA_real_,
        P.unadj = NA_real_,
        P.adj = NA_real_,
        Error = e$message
      )
    }
  )

  jt_data <- analysis_data %>%
    dplyr::filter(!is.na(Best_MASE), !is.na(EpiFORESS_Group)) %>%
    dplyr::mutate(
      EpiFORESS_Group = factor(
        EpiFORESS_Group,
        levels = c("Very_Low", "Low", "Moderate", "High", "Very_High"),
        ordered = TRUE
      ),
      EpiFORESS_Group_Order = as.numeric(EpiFORESS_Group)
    ) %>%
    droplevels()

  jonckheere_group_order <- jt_data %>%
    dplyr::group_by(EpiFORESS_Group) %>%
    dplyr::summarise(
      Dataset = dataset_label,
      Group_Order = dplyr::first(EpiFORESS_Group_Order),
      N_Diseases = dplyr::n(),
      Mean_EpiFORESS = mean(EpiFORESS, na.rm = TRUE),
      Median_EpiFORESS = median(EpiFORESS, na.rm = TRUE),
      Mean_Best_MASE = mean(Best_MASE, na.rm = TRUE),
      Median_Best_MASE = median(Best_MASE, na.rm = TRUE),
      SD_Best_MASE = stats::sd(Best_MASE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(Group_Order)

  jt_result <- tryCatch({
    if (nrow(jt_data) < 3) {
      stop("Fewer than 3 observations available for Jonckheere-Terpstra test.")
    }

    if (dplyr::n_distinct(jt_data$EpiFORESS_Group_Order) < 2) {
      stop("Fewer than 2 ordered EpiFORESS groups are observed.")
    }

    clinfun::jonckheere.test(
      x = jt_data$Best_MASE,
      g = jt_data$EpiFORESS_Group_Order,
      alternative = "decreasing"
    )
  }, error = function(e) e)

  jonckheere_summary <- if (inherits(jt_result, "htest")) {
    tibble::tibble(
      Dataset = dataset_label,
      Test = "Jonckheere-Terpstra",
      Ordered_Grouping = "Very_Low < Low < Moderate < High < Very_High",
      Response = "Best_MASE",
      Alternative = "Decreasing Best_MASE with increasing EpiFORESS group",
      Statistic = unname(jt_result$statistic),
      P_Value = jt_result$p.value,
      N = nrow(jt_data),
      N_Groups_Observed = dplyr::n_distinct(jt_data$EpiFORESS_Group_Order),
      Method = jt_result$method,
      Interpretation = dplyr::case_when(
        is.na(jt_result$p.value) ~ "No interpretable p-value returned.",
        jt_result$p.value < 0.05 ~
          "Evidence of an ordered decrease in Best_MASE across increasing EpiFORESS groups.",
        TRUE ~
          "No statistically significant ordered decrease detected at alpha = 0.05."
      )
    )
  } else {
    tibble::tibble(
      Dataset = dataset_label,
      Test = "Jonckheere-Terpstra",
      Ordered_Grouping = "Very_Low < Low < Moderate < High < Very_High",
      Response = "Best_MASE",
      Alternative = "Decreasing Best_MASE with increasing EpiFORESS group",
      Statistic = NA_real_,
      P_Value = NA_real_,
      N = nrow(jt_data),
      N_Groups_Observed = dplyr::n_distinct(jt_data$EpiFORESS_Group_Order),
      Method = NA_character_,
      Interpretation = paste(
        "Jonckheere-Terpstra test not computed:",
        conditionMessage(jt_result)
      )
    )
  }

  # ----------------------------------------------------------
  # E. EMPIRICAL LEAVE-ONE-DISEASE-OUT SENSITIVITY
  # ----------------------------------------------------------

  empirical_lodo_sensitivity <- purrr::map_dfr(
    sort(unique(analysis_data$Disease)),
    function(d) {
      td <- analysis_data %>%
        dplyr::filter(Disease != d)

      if (nrow(td) < 3) {
        return(tibble::tibble(
          Dataset = dataset_label,
          Removed_Disease = d,
          Pearson_r = NA_real_,
          Pearson_p = NA_real_,
          Spearman_rho = NA_real_,
          Spearman_p = NA_real_,
          Regression_Slope = NA_real_,
          R_Squared = NA_real_
        ))
      }

      pt <- tryCatch(
        suppressWarnings(stats::cor.test(
          td$EpiFORESS,
          td$Best_MASE,
          method = "pearson"
        )),
        error = function(e) NULL
      )

      st <- tryCatch(
        suppressWarnings(stats::cor.test(
          td$EpiFORESS,
          td$Best_MASE,
          method = "spearman",
          exact = FALSE
        )),
        error = function(e) NULL
      )

      lm_fit <- tryCatch(
        stats::lm(Best_MASE ~ EpiFORESS, data = td),
        error = function(e) NULL
      )

      tibble::tibble(
        Dataset = dataset_label,
        Removed_Disease = d,
        Pearson_r = if (is.null(pt)) NA_real_ else unname(pt$estimate),
        Pearson_p = if (is.null(pt)) NA_real_ else pt$p.value,
        Spearman_rho = if (is.null(st)) NA_real_ else unname(st$estimate),
        Spearman_p = if (is.null(st)) NA_real_ else st$p.value,
        Regression_Slope = if (is.null(lm_fit)) {
          NA_real_
        } else {
          as.numeric(stats::coef(lm_fit)["EpiFORESS"])
        },
        R_Squared = if (is.null(lm_fit)) {
          NA_real_
        } else {
          summary(lm_fit)$r.squared
        }
      )
    }
  )

  empirical_lodo_summary <- empirical_lodo_sensitivity %>%
    dplyr::summarise(
      Dataset = dataset_label,
      Mean_Pearson_r = mean(Pearson_r, na.rm = TRUE),
      Median_Pearson_r = median(Pearson_r, na.rm = TRUE),
      Min_Pearson_r = suppressWarnings(min(Pearson_r, na.rm = TRUE)),
      Max_Pearson_r = suppressWarnings(max(Pearson_r, na.rm = TRUE)),
      Mean_Spearman_rho = mean(Spearman_rho, na.rm = TRUE),
      Median_Spearman_rho = median(Spearman_rho, na.rm = TRUE),
      Min_Spearman_rho = suppressWarnings(min(Spearman_rho, na.rm = TRUE)),
      Max_Spearman_rho = suppressWarnings(max(Spearman_rho, na.rm = TRUE)),
      Mean_R_Squared = mean(R_Squared, na.rm = TRUE),
      Median_R_Squared = median(R_Squared, na.rm = TRUE),
      Min_R_Squared = suppressWarnings(min(R_Squared, na.rm = TRUE)),
      Max_R_Squared = suppressWarnings(max(R_Squared, na.rm = TRUE))
    ) %>%
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ dplyr::if_else(is.infinite(.x), NA_real_, .x)
      )
    )

  list(
    data = analysis_data,
    correlation_summary = correlation_summary,
    linear_model = linear_model,
    linear_model_summary = linear_model_summary,
    linear_model_fit = linear_model_fit,
    assumption_summary = assumption_summary,
    influence_summary = influence_summary,
    classification_summary = classification_summary,
    auc_ci_summary = auc_ci_summary,
    confusion_primary = confusion_primary,
    threshold_sweep_primary = threshold_sweep_primary,
    best_threshold_primary = best_threshold_primary,
    threshold_comparison = threshold_comparison,
    calibration_summary = calibration_summary,
    kruskal_summary = kruskal_summary,
    dunn_posthoc = dunn_posthoc,
    jonckheere_summary = jonckheere_summary,
    jonckheere_group_order = jonckheere_group_order,
    empirical_lodo_sensitivity = empirical_lodo_sensitivity,
    empirical_lodo_summary = empirical_lodo_summary,
    brier_primary = brier_primary
  )
}

all_disease_analysis <- run_validation_analysis(
  validation_data_all,
  "All-disease empirical validation"
)

exclusion_sensitivity_analysis <- if (
  run_exclusion_sensitivity && nrow(validation_data_sensitivity) >= 3
) {
  run_validation_analysis(
    validation_data_sensitivity,
    "Optional disease-exclusion sensitivity analysis"
  )
} else {
  NULL
}

if (is.null(all_disease_analysis)) {
  stop("All-disease empirical validation could not be computed.")
}

# ------------------------------------------------------------
# 13. EMPIRICAL VALIDATION PLOTS
# ------------------------------------------------------------

scatter_all_plot_path <- file.path(
  base_path,
  "EpiFORESS_vs_Best_MASE_All_Diseases.png"
)

threshold_plot_path <- file.path(
  base_path,
  "EpiFORESS_Threshold_Sweep_All_Diseases.png"
)

calibration_plot_path <- file.path(
  base_path,
  "EpiFORESS_Calibration_by_Group_All_Diseases.png"
)

residual_plot_path <- file.path(
  base_path,
  "EpiFORESS_Residual_Diagnostics_All_Diseases.png"
)

boxplot_path <- file.path(
  base_path,
  "Best_MASE_by_EpiFORESS_Group_All_Diseases.png"
)

comparison_plot_path <- file.path(
  base_path,
  "EpiFORESS_Threshold_Comparison_All_Diseases.png"
)

auc_ci_plot_path <- file.path(
  base_path,
  "EpiFORESS_AUC_CI_All_Diseases.png"
)

scatter_sensitivity_plot_path <- file.path(
  base_path,
  "EpiFORESS_vs_Best_MASE_Exclusion_Sensitivity.png"
)

all_plot_data <- all_disease_analysis$data

# Score vs Best MASE

ggplot2::ggsave(
  scatter_all_plot_path,
  width = 9,
  height = 6,
  dpi = 300,
  plot = ggplot2::ggplot(
    all_plot_data,
    ggplot2::aes(x = EpiFORESS, y = Best_MASE)
  ) +
    ggplot2::geom_point(
      ggplot2::aes(
        color = Empirical_Readiness_Primary
      ),
      size = 3
    ) +
    ggplot2::scale_color_manual(
      values = c("Ready" = "#087E8B", "Not_Ready" = "#B0BEC5"),
      name = paste0("Ready if MASE < ", empirical_mase_threshold)
    ) +
    ggplot2::geom_smooth(
      method = "lm",
      se = TRUE,
      color = "#333333",
      linewidth = 0.7
    ) +
    ggplot2::geom_hline(
      yintercept = empirical_mase_threshold,
      linetype = "dashed",
      color = "#333333"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = Disease),
      hjust = 0,
      vjust = 1,
      size = 2.8,
      check_overlap = TRUE
    ) +
    ggplot2::labs(
      title = "EpiFORESS Score vs Best MASE",
      subtitle = "Primary all-disease empirical validation",
      x = "EpiFORESS Score",
      y = "Best MASE"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")
)

# AUC confidence intervals

ggplot2::ggsave(
  auc_ci_plot_path,
  width = 8,
  height = 5,
  dpi = 300,
  plot = all_disease_analysis$auc_ci_summary %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = Criterion,
        y = AUC,
        ymin = AUC_CI_Lower,
        ymax = AUC_CI_Upper
      )
    ) +
    ggplot2::geom_point(size = 4, color = "#087E8B") +
    ggplot2::geom_errorbar(width = 0.2, color = "#087E8B") +
    ggplot2::geom_hline(
      yintercept = 0.5,
      linetype = "dashed",
      color = "gray50"
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      title = "All-Disease Empirical AUC-ROC with 95% Bootstrap CIs",
      subtitle = paste0("Bootstrap replicates: ", bootstrap_n),
      x = NULL,
      y = "AUC-ROC"
    ) +
    ggplot2::theme_minimal(base_size = 13)
)

# Threshold sweep

threshold_sweep_long <- all_disease_analysis$threshold_sweep_primary

ggplot2::ggsave(
  threshold_plot_path,
  width = 9,
  height = 5,
  dpi = 300,
  plot = ggplot2::ggplot(
    threshold_sweep_long,
    ggplot2::aes(
      x = Threshold,
      y = Balanced_Accuracy,
      color = Criterion
    )
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_vline(
      xintercept = epiforess_ready_threshold,
      linetype = "dashed",
      color = "gray50"
    ) +
    ggplot2::labs(
      title = "All-Disease Balanced Accuracy Across EpiFORESS Thresholds",
      subtitle = paste0(
        "Dashed = simulation-derived score cutoff: ",
        round(epiforess_ready_threshold, 2)
      ),
      x = "EpiFORESS Threshold",
      y = "Balanced Accuracy",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")
)

# Selected-threshold comparison

ggplot2::ggsave(
  comparison_plot_path,
  width = 9,
  height = 5,
  dpi = 300,
  plot = all_disease_analysis$threshold_comparison %>%
    dplyr::mutate(Threshold = factor(Threshold)) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = Threshold, y = F1_NotReady, fill = Criterion)
    ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(
      title = "All-Disease F1 Score for Not Ready at Selected Thresholds",
      x = "EpiFORESS Threshold",
      y = "F1 Score for Not Ready",
      fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")
)

# Group calibration summary

ggplot2::ggsave(
  calibration_plot_path,
  width = 8,
  height = 5,
  dpi = 300,
  plot = ggplot2::ggplot(
    all_disease_analysis$calibration_summary,
    ggplot2::aes(x = EpiFORESS_Group, y = Mean_Best_MASE)
  ) +
    ggplot2::geom_col(fill = "#087E8B") +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = pmax(Mean_Best_MASE - SD_Best_MASE, 0),
        ymax = Mean_Best_MASE + SD_Best_MASE
      ),
      width = 0.25,
      color = "#333333"
    ) +
    ggplot2::geom_hline(
      yintercept = empirical_mase_threshold,
      linetype = "dashed",
      color = "#333333"
    ) +
    ggplot2::labs(
      title = "Mean Best MASE by EpiFORESS Readiness Band",
      subtitle = "All diseases; error bars represent ±1 SD",
      x = "EpiFORESS Readiness Band",
      y = "Mean Best MASE"
    ) +
    ggplot2::theme_minimal(base_size = 13)
)

# Residual diagnostics

if (!is.null(all_disease_analysis$linear_model)) {
  residual_plot_data <- tibble::tibble(
    Fitted = stats::fitted(all_disease_analysis$linear_model),
    Residuals = stats::residuals(all_disease_analysis$linear_model)
  )

  ggplot2::ggsave(
    residual_plot_path,
    width = 8,
    height = 5,
    dpi = 300,
    plot = ggplot2::ggplot(
      residual_plot_data,
      ggplot2::aes(x = Fitted, y = Residuals)
    ) +
      ggplot2::geom_point(size = 3, color = "#087E8B") +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::labs(
        title = "Residual Plot for the All-Disease Linear Validation Model",
        x = "Fitted Values",
        y = "Residuals"
      ) +
      ggplot2::theme_minimal(base_size = 13)
  )
}

# Best MASE by readiness band

ggplot2::ggsave(
  boxplot_path,
  width = 8,
  height = 5,
  dpi = 300,
  plot = ggplot2::ggplot(
    all_plot_data,
    ggplot2::aes(x = EpiFORESS_Group, y = Best_MASE)
  ) +
    ggplot2::geom_boxplot(fill = "#E7F4F6", color = "#087E8B") +
    ggplot2::geom_jitter(
      width = 0.1,
      size = 2,
      ggplot2::aes(color = Empirical_Readiness_Primary)
    ) +
    ggplot2::scale_color_manual(
      values = c("Ready" = "#087E8B", "Not_Ready" = "#B77500"),
      name = paste0("Primary: MASE < ", empirical_mase_threshold)
    ) +
    ggplot2::geom_hline(
      yintercept = empirical_mase_threshold,
      linetype = "dashed",
      color = "#333333"
    ) +
    ggplot2::labs(
      title = "Best MASE Across EpiFORESS Readiness Bands",
      subtitle = "Primary all-disease empirical validation",
      x = "EpiFORESS Readiness Band",
      y = "Best MASE"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")
)

# Optional disease-exclusion sensitivity scatter

if (!is.null(exclusion_sensitivity_analysis)) {
  ggplot2::ggsave(
    scatter_sensitivity_plot_path,
    width = 9,
    height = 6,
    dpi = 300,
    plot = ggplot2::ggplot(
      exclusion_sensitivity_analysis$data,
      ggplot2::aes(x = EpiFORESS, y = Best_MASE)
    ) +
      ggplot2::geom_point(
        ggplot2::aes(color = Empirical_Readiness_Primary),
        size = 3
      ) +
      ggplot2::scale_color_manual(
        values = c("Ready" = "#087E8B", "Not_Ready" = "#B0BEC5"),
        name = paste0("Primary: MASE < ", empirical_mase_threshold)
      ) +
      ggplot2::geom_smooth(
        method = "lm",
        se = TRUE,
        color = "#333333",
        linewidth = 0.7
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = Disease),
        hjust = 0,
        vjust = 1,
        size = 2.8,
        check_overlap = TRUE
      ) +
      ggplot2::labs(
        title = "EpiFORESS Score vs Best MASE",
        subtitle = paste0(
          "Optional disease-exclusion sensitivity; excluded: ",
          paste(hard_empirical_diseases, collapse = ", ")
        ),
        x = "EpiFORESS Score",
        y = "Best MASE"
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(legend.position = "bottom")
  )
}

# ------------------------------------------------------------
# 14. SAVE EMPIRICAL VALIDATION OUTPUTS
# ------------------------------------------------------------

all_disease_confusion_primary <- as.data.frame.matrix(
  all_disease_analysis$confusion_primary
)

all_disease_confusion_primary$Predicted_Class <- rownames(
  all_disease_confusion_primary
)

rownames(all_disease_confusion_primary) <- NULL

all_disease_confusion_primary <- all_disease_confusion_primary %>%
  dplyr::relocate(Predicted_Class)

validation_tables <- list(
  Model_Configuration = model_configuration,
  Validation_Data_All = all_disease_analysis$data,
  Failed_or_Unmatched_Scores = unmatched_or_failed_scores,
  Exclusion_Sensitivity_Diseases = excluded_empirical_diseases,
  EpiFORESS_Scores = epiforess_scores,
  Raw_Features_49 = raw_feature_table,
  Model_Features_49 = model_feature_table,

  All_Model_Results = all_model_results,
  All_Model_Metrics = all_forecast_model_metrics,
  Model_Rankings = forecast_model_rankings,
  Model_Summary_By_Model = forecast_model_summary_by_model,
  Model_Summary_By_Disease = forecast_model_summary_by_disease,
  Model_Metrics_Wide = forecast_model_metrics_wide,
  Model_Winner_Diagnostics = forecast_model_winner_diagnostics,
  Model_Metric_Dictionary = forecast_model_metric_dictionary,

  All_AUC_CI = all_disease_analysis$auc_ci_summary,
  All_Correlation = all_disease_analysis$correlation_summary,
  All_Linear_Model_Coefs = all_disease_analysis$linear_model_summary,
  All_Linear_Model_Fit = all_disease_analysis$linear_model_fit,
  All_Assumptions = all_disease_analysis$assumption_summary,
  All_Influence = all_disease_analysis$influence_summary,
  All_Confusion_Primary = all_disease_confusion_primary,
  All_Classification = all_disease_analysis$classification_summary,
  All_Threshold_Sweep_Primary = all_disease_analysis$threshold_sweep_primary,
  All_Best_Threshold_Primary = all_disease_analysis$best_threshold_primary,
  All_Threshold_Comparison = all_disease_analysis$threshold_comparison,
  All_Calibration = all_disease_analysis$calibration_summary,
  All_Kruskal_Wallis = all_disease_analysis$kruskal_summary,
  All_Dunn_Posthoc = all_disease_analysis$dunn_posthoc,
  All_Jonckheere_Terpstra = all_disease_analysis$jonckheere_summary,
  All_JT_Group_Order = all_disease_analysis$jonckheere_group_order,
  All_Empirical_LODO = all_disease_analysis$empirical_lodo_sensitivity,
  All_Empirical_LODO_Summary = all_disease_analysis$empirical_lodo_summary
)

if (!is.null(exclusion_sensitivity_analysis)) {
  sensitivity_confusion_primary <- as.data.frame.matrix(
    exclusion_sensitivity_analysis$confusion_primary
  )

  sensitivity_confusion_primary$Predicted_Class <- rownames(
    sensitivity_confusion_primary
  )

  rownames(sensitivity_confusion_primary) <- NULL

  sensitivity_confusion_primary <- sensitivity_confusion_primary %>%
    dplyr::relocate(Predicted_Class)

  validation_tables <- c(
    validation_tables,
    list(
      Sensitivity_Data = exclusion_sensitivity_analysis$data,
      Sensitivity_AUC_CI = exclusion_sensitivity_analysis$auc_ci_summary,
      Sensitivity_Correlation = exclusion_sensitivity_analysis$correlation_summary,
      Sensitivity_Linear_Coefs = exclusion_sensitivity_analysis$linear_model_summary,
      Sensitivity_Linear_Fit = exclusion_sensitivity_analysis$linear_model_fit,
      Sensitivity_Assumptions = exclusion_sensitivity_analysis$assumption_summary,
      Sensitivity_Influence = exclusion_sensitivity_analysis$influence_summary,
      Sensitivity_Confusion_Primary = sensitivity_confusion_primary,
      Sensitivity_Classification = exclusion_sensitivity_analysis$classification_summary,
      Sensitivity_Threshold_Primary = exclusion_sensitivity_analysis$threshold_sweep_primary,
      Sensitivity_Best_Threshold_Primary = exclusion_sensitivity_analysis$best_threshold_primary,
      Sensitivity_Threshold_Comparison = exclusion_sensitivity_analysis$threshold_comparison,
      Sensitivity_Calibration = exclusion_sensitivity_analysis$calibration_summary,
      Sensitivity_Kruskal = exclusion_sensitivity_analysis$kruskal_summary,
      Sensitivity_Dunn = exclusion_sensitivity_analysis$dunn_posthoc,
      Sensitivity_JT = exclusion_sensitivity_analysis$jonckheere_summary,
      Sensitivity_JT_Groups = exclusion_sensitivity_analysis$jonckheere_group_order,
      Sensitivity_Empirical_LODO = exclusion_sensitivity_analysis$empirical_lodo_sensitivity,
      Sensitivity_Empirical_LODO_Summary = exclusion_sensitivity_analysis$empirical_lodo_summary
    )
  )
}

write_tables_to_workbook(
  tables = validation_tables,
  path = validation_output_path
)

results_tables <- list(
  Empirical_Model_Config = model_configuration,
  Empirical_Validation_All = all_disease_analysis$data,
  Empirical_FailedScores = unmatched_or_failed_scores,
  Empirical_Exclusion_List = excluded_empirical_diseases,
  Empirical_Classification = all_disease_analysis$classification_summary,
  Empirical_AUC_CI = all_disease_analysis$auc_ci_summary,
  Empirical_Threshold_Sweep = all_disease_analysis$threshold_sweep_primary,
  Empirical_Calibration = all_disease_analysis$calibration_summary,
  Empirical_Correlation = all_disease_analysis$correlation_summary,
  Empirical_Threshold_Comp = all_disease_analysis$threshold_comparison,
  Empirical_JT_Test = all_disease_analysis$jonckheere_summary,
  Empirical_JT_Groups = all_disease_analysis$jonckheere_group_order,

  Empirical_AllModelMetrics = all_forecast_model_metrics,
  Empirical_ModelRankings = forecast_model_rankings,
  Empirical_ModelByModel = forecast_model_summary_by_model,
  Empirical_ModelByDisease = forecast_model_summary_by_disease,
  Empirical_ModelWide = forecast_model_metrics_wide,
  Empirical_ModelWinners = forecast_model_winner_diagnostics
)

if (!is.null(exclusion_sensitivity_analysis)) {
  results_tables <- c(
    results_tables,
    list(
      Empirical_Sensitivity_Data = exclusion_sensitivity_analysis$data,
      Empirical_Sensitivity_Class = exclusion_sensitivity_analysis$classification_summary,
      Empirical_Sensitivity_AUC = exclusion_sensitivity_analysis$auc_ci_summary,
      Empirical_Sensitivity_Cor = exclusion_sensitivity_analysis$correlation_summary,
      Empirical_Sensitivity_JT = exclusion_sensitivity_analysis$jonckheere_summary
    )
  )
}

append_to_results_workbook(
  tables = results_tables,
  path = output_workbook_path
)

# ------------------------------------------------------------
# 15. SAVE EMPIRICAL VALIDATION RDS
# ------------------------------------------------------------

empirical_validation_rds <- list(
  model_configuration = model_configuration,
  all_disease_analysis = all_disease_analysis,
  exclusion_sensitivity_analysis = exclusion_sensitivity_analysis,
  validation_data_all = validation_data_all,
  validation_data_sensitivity = validation_data_sensitivity,
  unmatched_or_failed_scores = unmatched_or_failed_scores,
  excluded_empirical_diseases = excluded_empirical_diseases,
  epiforess_scores = epiforess_scores,
  raw_feature_table = raw_feature_table,
  model_feature_table = model_feature_table,
  all_model_results = all_model_results,
  best_model_summary = best_model_summary,
  forecast_model_summary_by_model = forecast_model_summary_by_model,
  forecast_model_summary_by_disease = forecast_model_summary_by_disease,
  best_statistical_name = best_statistical_name,
  best_ml_name = best_ml_name,
  weight_statistical = weight_stat,
  weight_ml = weight_ml,
  weight_source = weight_source,
  not_ready_threshold_hybrid = not_ready_threshold_hybrid,
  epiforess_ready_threshold = epiforess_ready_threshold,
  empirical_mase_threshold = empirical_mase_threshold,
  created_at = Sys.time()
)

empirical_validation_rds_path <- file.path(
  model_output_dir,
  "empirical_validation_results.rds"
)

saveRDS(empirical_validation_rds, empirical_validation_rds_path)

# ------------------------------------------------------------
# 16. CONSOLE SUMMARY
# ------------------------------------------------------------

cat("\n================================================\n")
cat("EpiFORESS END-TO-END EMPIRICAL VALIDATION COMPLETE\n")
cat("================================================\n")

cat("\nModel architecture:\n")
cat("  Statistical component:", best_statistical_name, "\n")
cat("  Machine-learning component:", best_ml_name, "\n")
cat("  XGBoost calibration applied:", !is.null(ml_calibration_model), "\n")

cat("\nReadiness definition:\n")
cat("  Ready if Best MASE <", empirical_mase_threshold, "\n")
cat(
  "  Operational score rule: Ready if EpiFORESS >",
  round(epiforess_ready_threshold, 2),
  "\n"
)
cat(
  "  Equivalent Not-Ready probability threshold:",
  round(not_ready_threshold_hybrid, 4),
  "\n"
)

cat("\nHybrid weighting:\n")
cat("  Statistical weight:", round(weight_stat, 4), "\n")
cat("  ML weight:", round(weight_ml, 4), "\n")
cat("  Source:", weight_source, "\n")

cat("\nEmpirical disease counts:\n")
cat("  Matched all-disease validation set:", nrow(validation_data_all), "\n")
cat("  Failed or unmatched scores:", nrow(unmatched_or_failed_scores), "\n")
cat("  Optional disease-exclusion sensitivity enabled:", run_exclusion_sensitivity, "\n")
cat("  Named disease exclusions:", nrow(excluded_empirical_diseases), "\n")

cat("\nAll-disease correlation summary:\n")
print(all_disease_analysis$correlation_summary)

cat("\nAll-disease classification summary:\n")
print(all_disease_analysis$classification_summary)

cat("\nAll-disease AUC-ROC with bootstrap confidence interval:\n")
print(all_disease_analysis$auc_ci_summary)

cat("\nAll-disease empirical readiness distribution:\n")
print(table(all_disease_analysis$data$Empirical_Readiness_Primary))

cat("\nAll-disease confusion matrix:\n")
print(all_disease_analysis$confusion_primary)

cat("\nAll-disease Brier score:\n")
cat(" ", round(all_disease_analysis$brier_primary, 4), "\n")

cat("\nAll-disease best empirical sweep threshold:\n")
print(all_disease_analysis$best_threshold_primary)

cat("\nAll-disease calibration summary:\n")
print(all_disease_analysis$calibration_summary)

cat("\nAll-disease Jonckheere-Terpstra ordered trend test:\n")
print(all_disease_analysis$jonckheere_summary)

cat("\nAll-disease empirical leave-one-disease-out robustness summary:\n")
print(all_disease_analysis$empirical_lodo_summary)

if (!is.null(exclusion_sensitivity_analysis)) {
  cat("\nOptional disease-exclusion sensitivity correlation summary:\n")
  print(exclusion_sensitivity_analysis$correlation_summary)

  cat("\nOptional disease-exclusion sensitivity classification summary:\n")
  print(exclusion_sensitivity_analysis$classification_summary)
}

cat("\nForecasting model summary by model:\n")
print(forecast_model_summary_by_model)

cat("\nForecasting model winner diagnostics:\n")
print(forecast_model_winner_diagnostics)

cat("\nEpiFORESS scoring status summary:\n")
print(
  epiforess_scores %>%
    dplyr::count(Score_Status, Feature_Error, Prediction_Error, name = "Frequency")
)

cat("\nSaved files:\n")
cat("  Forecasting workbook:", forecast_output_path, "\n")
cat("  Score workbook:", score_output_path, "\n")
cat("  Validation workbook:", validation_output_path, "\n")
cat("  Updated results workbook:", output_workbook_path, "\n")
cat("  Empirical validation RDS:", empirical_validation_rds_path, "\n")

cat("\nSaved plots:\n")
cat(" ", plot_best_mase_path, "\n")
cat(" ", scatter_all_plot_path, "\n")
cat(" ", auc_ci_plot_path, "\n")
cat(" ", threshold_plot_path, "\n")
cat(" ", comparison_plot_path, "\n")
cat(" ", calibration_plot_path, "\n")
cat(" ", residual_plot_path, "\n")
cat(" ", boxplot_path, "\n")

if (!is.null(exclusion_sensitivity_analysis)) {
  cat(" ", scatter_sensitivity_plot_path, "\n")
}

cat("\nForecasting roster used:\n")
cat("  Seasonal_Naive, STL_ETS, SARIMA, Nonseasonal_ARIMA, NegBin_Regression\n")

cat("\n================================================\n")
cat("SCRIPT COMPLETE\n")
cat("================================================\n")
