# EpiFORESS | 02_model_config.R
# Load saved model artifacts and configure model predictors.
# Split from app_full_fixed.R; code logic preserved during file separation.

# 3. LOAD FINAL CONFIGURATION AND MODELS
# ------------------------------------------------------------
final_results <- tryCatch(
  readRDS(final_results_path),
  error = function(e) {
    warning("Could not load final_epiforess_results.rds: ", e$message)
    NULL
  }
)
lodo_results <- tryCatch(
  readRDS(lodo_results_path),
  error = function(e) {
    warning("Could not load lodo_validation_results.rds: ", e$message)
    NULL
  }
)
# Final main-model inverse-Brier weights
w_stat <- 0.310109318867921
w_ml   <- 0.689890681132079
weight_source <- "Final main-model inverse-Brier weights"
stopifnot(
  is.finite(w_stat),
  is.finite(w_ml),
  w_stat > 0,
  w_ml > 0,
  abs((w_stat + w_ml) - 1) < 1e-10
)
cat("Calibrated ensemble configuration loaded.\n\n")
stat_model_obj <- tryCatch(
  readRDS(stat_model_path),
  error = function(e) {
    warning("Statistical model could not be loaded: ", e$message)
    NULL
  }
)
ml_model_obj <- tryCatch(
  readRDS(ml_model_path),
  error = function(e) {
    warning("ML model could not be loaded: ", e$message)
    NULL
  }
)
ml_calibration_model_obj <- if (file.exists(ml_calibration_path)) {
  tryCatch(
    readRDS(ml_calibration_path),
    error = function(e) {
      warning("Machine-learning calibration model could not be loaded: ", e$message)
      NULL
    }
  )
} else {
  warning("Optional machine-learning calibration model was not found: ", ml_calibration_path)
  NULL
}
cat("Statistical model class:", paste(class(unwrap_model_object(stat_model_obj)), collapse = ", "), "\n")
cat("ML model class:", paste(class(unwrap_model_object(ml_model_obj)), collapse = ", "), "\n\n")
stat_predictor_vars <- get_model_predictor_names(stat_model_obj, predictor_vars)
ml_predictor_vars <- get_model_predictor_names(ml_model_obj, predictor_vars)
stat_medians <- extract_imputation_medians(final_results, stat_predictor_vars)
ml_medians <- extract_imputation_medians(final_results, ml_predictor_vars)
cat("Statistical predictors used:", length(stat_predictor_vars), "\n")
cat("ML predictors used:", length(ml_predictor_vars), "\n\n")
# ------------------------------------------------------------
