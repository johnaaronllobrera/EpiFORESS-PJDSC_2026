# EpiFORESS | 00_setup.R
# Setup, package loading, app settings, predictor definitions, and feature-domain mappings.
# Split from app_full_fixed.R; code logic preserved during file separation.


# ============================================================
options(shiny.sanitize.errors = FALSE)
cat("EpiFORESS app version: final-continuous-gradient-2026-07-16\n")
options(shiny.maxRequestSize = 1000 * 1024^2)  # 100 MB limit
# ------------------------------------------------------------
# 0. PACKAGES
# ------------------------------------------------------------
packages <- c(
  "shiny", "bslib", "readxl", "readr", "dplyr", "ggplot2",
  "forecast", "tibble", "DT", "caret", "xgboost", "stringr",
  "lubridate", "tseries", "scales", "htmltools", "glmnet",
  "purrr", "plotly", "shinycssloaders", "pracma", "TDA", "changepoint", "ranger", "later"
)
missing_packages <- setdiff(packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(readxl)
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(forecast)
  library(tibble)
  library(DT)
  library(caret)
  library(xgboost)
  library(stringr)
  library(lubridate)
  library(tseries)
  library(scales)
  library(htmltools)
  library(glmnet)
  library(purrr)
  library(plotly)
  library(shinycssloaders)
  library(pracma)
})
has_TDA <- requireNamespace("TDA", quietly = TRUE)
has_changepoint <- requireNamespace("changepoint", quietly = TRUE)
has_ranger <- requireNamespace("ranger", quietly = TRUE)
has_later <- requireNamespace("later", quietly = TRUE)
# ------------------------------------------------------------
# 1. SETTINGS
# ------------------------------------------------------------
resolve_epiforess_app_dir <- function() {
  candidate_dirs <- character(0)
  frame_files <- vapply(
    sys.frames(),
    function(frame) {
      ofile <- frame$ofile
      if (
        is.null(ofile) || length(ofile) == 0 ||
        is.na(ofile[1]) || !nzchar(as.character(ofile[1]))
      ) return(NA_character_)
      as.character(ofile[1])
    },
    character(1)
  )
  frame_files <- frame_files[!is.na(frame_files) & nzchar(frame_files)]
  if (length(frame_files) > 0) {
    candidate_dirs <- c(
      candidate_dirs,
      dirname(normalizePath(frame_files, winslash = "/", mustWork = FALSE))
    )
  }
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    candidate_dirs <- c(
      candidate_dirs,
      dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE))
    )
  }
  candidate_dirs <- unique(c(
    candidate_dirs,
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  ))
  matching_dir <- candidate_dirs[
    dir.exists(file.path(candidate_dirs, "Saved_Models"))
  ]
  if (length(matching_dir) > 0) return(matching_dir[1])
  candidate_dirs[1]
}
app_dir <- resolve_epiforess_app_dir()
model_output_dir <- file.path(app_dir, "Saved_Models")
stat_model_path <- file.path(model_output_dir, "best_statistical_forecast_readiness_model.rds")
ml_model_path <- file.path(model_output_dir, "best_machine_learning_model.rds")
ml_calibration_candidates <- c(
  file.path(model_output_dir, "ml_probability_calibration_model.rds"),
  file.path(model_output_dir, "xgb_platt_calibration_model.rds")
)
existing_ml_calibration <- ml_calibration_candidates[file.exists(ml_calibration_candidates)]
ml_calibration_path <- if (length(existing_ml_calibration) > 0L) {
  existing_ml_calibration[1]
} else {
  ml_calibration_candidates[1]
}
final_results_path <- file.path(model_output_dir, "final_epiforess_results.rds")
lodo_results_path <- file.path(model_output_dir, "lodo_validation_results.rds")
cat("Resolved model directory:", model_output_dir, "\n")
stat_glm_target_class <- "Not_Ready"
xgb_target_class <- "Not_Ready"
ml_target_class <- xgb_target_class
max_batch_series <- 1000L
apply_log1p_to_model_features <- FALSE
# ------------------------------------------------------------
# CONTINUOUS SCORE AND DESCRIPTIVE READINESS BANDS
# ------------------------------------------------------------
simulation_not_ready_threshold <- 0.66
simulation_score_boundary <- 100 * (1 - simulation_not_ready_threshold)
# Exact reference: P(Not Ready) = 0.66 <=> EpiFORESS = 34.
# Rounded descriptive bands: 0-19, 20-34, 35-59, 60-79, 80-100.
# The application does not issue a binary decision.
stopifnot(isTRUE(all.equal(simulation_score_boundary, 34)))

# ------------------------------------------------------------
# SCORE-TO-BEST-MASE CALIBRATION
#
# Validation-set calibration:
#   log(Best MASE) =
#     0.688744 - 0.145625 * (EpiFORESS / 10)
#
# Exponentiation gives the conditional median Best MASE.
# This is a secondary continuous output. It is not included in
# the EpiFORESS score and does not replace forecast validation.
# ------------------------------------------------------------

best_mase_calibration_intercept <- 0.688744
best_mase_calibration_slope_per_10 <- -0.145625

estimate_median_best_mase <- function(score) {

  score <- suppressWarnings(
    as.numeric(score)
  )

  output <- rep(
    NA_real_,
    length(score)
  )

  valid <- is.finite(score)

  output[valid] <- exp(
    best_mase_calibration_intercept +
      best_mase_calibration_slope_per_10 *
      (score[valid] / 10)
  )

  output
}

# ------------------------------------------------------------
# 1A. EXACT 49-PREDICTOR MODEL SPACE
# ------------------------------------------------------------
predictor_vars <- c(
  "mean_cases", "dispersion_index", "coefficient_of_variation",
  "variance_cases", "skewness_cases", "kurtosis_cases", "gini_coefficient",
  "trend_strength", "trend_slope", "seasonality_strength",
  "lag1_autocorrelation", "lag2_autocorrelation", "lag4_autocorrelation",
  "lag52_autocorrelation", "acf_decay", "partial_acf_lag1",
  "spectral_entropy", "dominant_period", "sample_entropy", "persistence_entropy",
  "missingness_rate", "heaping_index", "repetition_index",
  "proportion_zero_counts", "longest_zero_run", "longest_nonzero_run",
  "intermittency", "run_length_cv", "zero_cluster_index",
  "outbreak_weeks", "outbreak_week_rate", "peak_to_mean_ratio",
  "level_shift_index", "change_point_count", "change_point_rate",
  "outbreak_dominance_index", "max_to_median_ratio", "upper_tail_weight",
  "seasonal_peak_timing_instability",
  "signal_to_noise_ratio", "stl_remainder_strength",
  "rolling_variance_instability", "rolling_mean_instability",
  "residual_acf_strength", "noise_to_signal_ratio",
  "yoy_mean_cv", "yoy_peak_cv", "yoy_variance_cv", "seasonal_subseries_cv"
)
stopifnot(length(predictor_vars) == 49L, !anyDuplicated(predictor_vars))
log1p_features <- c(
  "mean_cases", "dispersion_index", "variance_cases", "peak_to_mean_ratio",
  "level_shift_index", "change_point_count", "outbreak_weeks",
  "longest_zero_run", "longest_nonzero_run", "max_to_median_ratio",
  "dominant_period", "rolling_variance_instability",
  "rolling_mean_instability", "signal_to_noise_ratio", "noise_to_signal_ratio"
)
raw_feature_label_map <- c(
  mean_cases = "Mean cases",
  dispersion_index = "Dispersion index",
  coefficient_of_variation = "Coefficient of variation",
  variance_cases = "Variance of cases",
  skewness_cases = "Skewness",
  kurtosis_cases = "Kurtosis",
  gini_coefficient = "Gini coefficient",
  trend_strength = "Linear trend R-squared",
  trend_slope = "Linear trend slope",
  seasonality_strength = "Week-of-year seasonality R-squared",
  lag1_autocorrelation = "Lag-1 autocorrelation",
  lag2_autocorrelation = "Lag-2 autocorrelation",
  lag4_autocorrelation = "Lag-4 autocorrelation",
  lag52_autocorrelation = "Lag-52 autocorrelation",
  acf_decay = "ACF decay",
  partial_acf_lag1 = "Partial ACF lag 1",
  spectral_entropy = "Spectral entropy",
  dominant_period = "Dominant period",
  sample_entropy = "Sample entropy",
  persistence_entropy = "Persistence entropy",
  missingness_rate = "Missingness rate",
  heaping_index = "Terminal-digit heaping index",
  repetition_index = "Repeated-value index",
  proportion_zero_counts = "Proportion of zero counts",
  longest_zero_run = "Longest zero run",
  longest_nonzero_run = "Longest nonzero run",
  intermittency = "Intermittency",
  run_length_cv = "Run-length CV",
  zero_cluster_index = "Zero-cluster index",
  outbreak_weeks = "Outbreak weeks",
  outbreak_week_rate = "Outbreak week rate",
  peak_to_mean_ratio = "Peak-to-mean ratio",
  level_shift_index = "Level-shift index",
  change_point_count = "Change-point count",
  change_point_rate = "Change-point rate",
  outbreak_dominance_index = "Outbreak dominance index",
  max_to_median_ratio = "Max-to-median ratio",
  upper_tail_weight = "Upper-tail weight",
  seasonal_peak_timing_instability = "Seasonal peak-timing instability",
  signal_to_noise_ratio = "Signal-to-noise ratio",
  stl_remainder_strength = "STL remainder strength",
  rolling_variance_instability = "Rolling variance instability",
  rolling_mean_instability = "Rolling mean instability",
  residual_acf_strength = "Residual ACF strength",
  noise_to_signal_ratio = "Noise-to-signal ratio",
  yoy_mean_cv = "Year-over-year mean CV",
  yoy_peak_cv = "Year-over-year peak CV",
  yoy_variance_cv = "Year-over-year variance CV",
  seasonal_subseries_cv = "Seasonal subseries CV"
)
model_feature_label_map <- raw_feature_label_map
feature_interpretation_map <- c(
  mean_cases = "Mean observed weekly case count.",
  dispersion_index = "Variance-to-mean ratio; values above 1 indicate overdispersion.",
  coefficient_of_variation = "Standard deviation relative to the absolute mean.",
  variance_cases = "Variance of observed weekly counts.",
  skewness_cases = "Third standardized moment, indicating distributional asymmetry.",
  kurtosis_cases = "Excess kurtosis, indicating tail heaviness and extreme observations.",
  gini_coefficient = "Concentration of total cases across weeks.",
  trend_strength = "Proportion of variation explained by a linear regression of the series on time.",
  trend_slope = "Estimated linear change in the observed count per week.",
  seasonality_strength = "Proportion of variation explained by week-of-year indicator variables.",
  lag1_autocorrelation = "Autocorrelation between adjacent weeks.",
  lag2_autocorrelation = "Autocorrelation at a two-week lag.",
  lag4_autocorrelation = "Autocorrelation at a four-week lag.",
  lag52_autocorrelation = "Autocorrelation at the annual seasonal lag.",
  acf_decay = "Estimated decay rate of the absolute autocorrelation function.",
  partial_acf_lag1 = "Lag-1 partial autocorrelation after accounting for intermediate dependence.",
  spectral_entropy = "Normalized entropy of the spectral power distribution.",
  dominant_period = "Period of the strongest spectral component, measured in weeks.",
  sample_entropy = "Irregularity of short sequential patterns.",
  persistence_entropy = "Normalized entropy of persistence lifetimes from the delay embedding.",
  missingness_rate = "Proportion of observations with missing counts.",
  heaping_index = "Proportion of positive counts divisible by five.",
  repetition_index = "Proportion of adjacent observed pairs with identical counts.",
  proportion_zero_counts = "Proportion of observed weeks with zero cases.",
  longest_zero_run = "Longest zero-count run after missing values are removed.",
  longest_nonzero_run = "Longest nonzero run after missing values are removed.",
  intermittency = "Rate of transitions between zero and nonzero observations.",
  run_length_cv = "Coefficient of variation of alternating zero and nonzero run lengths.",
  zero_cluster_index = "Length-weighted mean zero-run length: mean(L^2) divided by mean(L).",
  outbreak_weeks = "Number of observations above the Tukey upper fence, with a mean-plus-two-SD fallback.",
  outbreak_week_rate = "Outbreak-week count divided by the number of observed weeks.",
  peak_to_mean_ratio = "Maximum observed count relative to the mean count.",
  level_shift_index = "Maximum absolute change in a trailing rolling mean.",
  change_point_count = "Number of mean-variance change points detected by PELT with BIC, or by the fallback rule.",
  change_point_rate = "Detected change points divided by interpolated series length.",
  outbreak_dominance_index = "Share of total cases occurring at or above the 90th percentile.",
  max_to_median_ratio = "Maximum observed count relative to the median observed count.",
  upper_tail_weight = "Share of total cases occurring at or above the 90th percentile.",
  seasonal_peak_timing_instability = "Standard deviation of annual peak week divided by seasonal frequency.",
  signal_to_noise_ratio = "Variance of fitted trend and seasonal structure relative to residual variance.",
  stl_remainder_strength = "STL remainder variance divided by total series variance.",
  rolling_variance_instability = "Coefficient of variation of 13-week rolling variances.",
  rolling_mean_instability = "Coefficient of variation of 13-week rolling means.",
  residual_acf_strength = "Lag-1 autocorrelation of the STL remainder.",
  noise_to_signal_ratio = "Inverse of the signal-to-noise ratio.",
  yoy_mean_cv = "Coefficient of variation of annual means.",
  yoy_peak_cv = "Coefficient of variation of annual maxima.",
  yoy_variance_cv = "Coefficient of variation of annual variances.",
  seasonal_subseries_cv = "Mean coefficient of variation across week-of-year subseries."
)
stopifnot(setequal(names(feature_interpretation_map), predictor_vars))
feature_domain_map <- c(
  mean_cases = "Distributional Characteristics",
  dispersion_index = "Distributional Characteristics",
  coefficient_of_variation = "Distributional Characteristics",
  variance_cases = "Distributional Characteristics",
  skewness_cases = "Distributional Characteristics",
  kurtosis_cases = "Distributional Characteristics",
  gini_coefficient = "Distributional Characteristics",
  trend_strength = "Temporal Structure",
  trend_slope = "Temporal Structure",
  seasonality_strength = "Temporal Structure",
  lag1_autocorrelation = "Temporal Structure",
  lag2_autocorrelation = "Temporal Structure",
  lag4_autocorrelation = "Temporal Structure",
  lag52_autocorrelation = "Temporal Structure",
  acf_decay = "Temporal Structure",
  partial_acf_lag1 = "Temporal Structure",
  spectral_entropy = "Complexity and Irregularity",
  dominant_period = "Complexity and Irregularity",
  sample_entropy = "Complexity and Irregularity",
  persistence_entropy = "Complexity and Irregularity",
  missingness_rate = "Data Quality and Reporting Artifacts",
  heaping_index = "Data Quality and Reporting Artifacts",
  repetition_index = "Data Quality and Reporting Artifacts",
  proportion_zero_counts = "Data Quality and Reporting Artifacts",
  longest_zero_run = "Data Quality and Reporting Artifacts",
  longest_nonzero_run = "Data Quality and Reporting Artifacts",
  intermittency = "Data Quality and Reporting Artifacts",
  run_length_cv = "Data Quality and Reporting Artifacts",
  zero_cluster_index = "Data Quality and Reporting Artifacts",
  outbreak_weeks = "Outbreak and Structural Instability",
  outbreak_week_rate = "Outbreak and Structural Instability",
  peak_to_mean_ratio = "Outbreak and Structural Instability",
  level_shift_index = "Outbreak and Structural Instability",
  change_point_count = "Outbreak and Structural Instability",
  change_point_rate = "Outbreak and Structural Instability",
  outbreak_dominance_index = "Outbreak and Structural Instability",
  max_to_median_ratio = "Outbreak and Structural Instability",
  upper_tail_weight = "Outbreak and Structural Instability",
  seasonal_peak_timing_instability = "Outbreak and Structural Instability",
  signal_to_noise_ratio = "Signal Quality and Noise",
  stl_remainder_strength = "Signal Quality and Noise",
  rolling_variance_instability = "Signal Quality and Noise",
  rolling_mean_instability = "Signal Quality and Noise",
  residual_acf_strength = "Signal Quality and Noise",
  noise_to_signal_ratio = "Signal Quality and Noise",
  yoy_mean_cv = "Inter-Annual Stability",
  yoy_peak_cv = "Inter-Annual Stability",
  yoy_variance_cv = "Inter-Annual Stability",
  seasonal_subseries_cv = "Inter-Annual Stability"
)
feature_domain_order <- c(
  "Distributional Characteristics",
  "Temporal Structure",
  "Complexity and Irregularity",
  "Data Quality and Reporting Artifacts",
  "Outbreak and Structural Instability",
  "Signal Quality and Noise",
  "Inter-Annual Stability"
)
feature_domain_ids <- c(
  "Distributional Characteristics" = "distributional",
  "Temporal Structure" = "temporal",
  "Complexity and Irregularity" = "complexity",
  "Data Quality and Reporting Artifacts" = "dataquality",
  "Outbreak and Structural Instability" = "outbreak",
  "Signal Quality and Noise" = "signal",
  "Inter-Annual Stability" = "interannual"
)
stopifnot(
  setequal(names(raw_feature_label_map), predictor_vars),
  setequal(names(feature_domain_map), predictor_vars),
  setequal(feature_domain_order, unique(feature_domain_map)),
  setequal(names(feature_domain_ids), feature_domain_order)
)
# ------------------------------------------------------------
