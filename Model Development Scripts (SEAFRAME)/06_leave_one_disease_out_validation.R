# EpiFORESS Analysis Pipeline
# Script: 06_leave_one_disease_out_validation.R
# Project root: set EPIFORESS_PROJECT_DIR or run from the project data directory.
# Reproducibility: inspect all settings and verify input/model compatibility before reporting results.

# ============================================================
# EPIFORESS: LEAVE-ONE-DISEASE-OUT VALIDATION
# ============================================================
# Run after feature extraction, forecasting, readiness labeling,
# and main EpiFORESS model selection.
#
# Primary readiness rule: Ready if Best_MASE < 1.00.
# This script performs leave-one-disease-out validation with
# disease-grouped internal validation, fold-specific preprocessing,
# fold-specific hybrid weights, and fold-specific thresholds.
#
# IMPORTANT:
# - Set EPIFORESS_PROJECT_DIR to the project data/model directory, or run from that directory.
# - The selected model names and Brier scores must match the
#   structure of final_epiforess_results.rds.
# - This script has not been executed in R in this environment.
# ============================================================

# ------------------------------------------------------------
# 0. PACKAGES
# ------------------------------------------------------------

packages <- c(
  "readxl", "dplyr", "caret", "xgboost", "MASS", "openxlsx",
  "tibble", "pROC", "PRROC", "purrr", "stringr", "glmnet",
  "arm", "ranger", "car"
)

missing_packages <- setdiff(packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(caret)
  library(xgboost)
  library(MASS)
  library(openxlsx)
  library(tibble)
  library(pROC)
  library(PRROC)
  library(purrr)
  library(stringr)
  library(glmnet)
  library(arm)
  library(ranger)
  library(car)
})

set.seed(2026)

# ------------------------------------------------------------
# 1. USER SETTINGS
# ------------------------------------------------------------

base_path <- normalizePath(Sys.getenv("EPIFORESS_PROJECT_DIR", unset = getwd()), winslash = "/", mustWork = FALSE)
if (!dir.exists(base_path)) stop("Project directory does not exist: ", base_path, call. = FALSE)

source_workbook_path <- file.path(
  base_path, "Disease_Seeded_MC_Simulation_REFINED.xlsx"
)
output_workbook_path <- file.path(
  base_path, "EpiFORESS_Forecast_Results.xlsx"
)
lodo_output_path <- file.path(
  base_path, "Leave_One_Disease_Out_Validation.xlsx"
)
model_output_dir <- file.path(base_path, "Saved_Models")
epiforess_results_path <- file.path(
  model_output_dir, "final_epiforess_results.rds"
)

if (!dir.exists(model_output_dir)) {
  dir.create(model_output_dir, recursive = TRUE)
}

ready_mase_threshold <- 1.00
internal_validation_disease_prop <- 0.20
internal_split_max_attempts <- 250
cv_folds_stat <- 5
cv_folds_ml <- 5
threshold_grid <- seq(0.01, 0.99, by = 0.01)

elastic_alpha_grid <- seq(0.05, 0.95, by = 0.10)
glmnet_lambda_grid <- 10^seq(-5, 1, length.out = 80)
vif_threshold <- 10

xgb_max_rounds <- 500
xgb_early_stopping <- 25
xgb_nthread <- max(1, parallel::detectCores() - 2)

ranger_num_trees <- 500
ranger_mtry_candidates <- c(3, 5, 7, 10, 15)
ranger_min_node_sizes <- c(5, 10, 20, 50)

# ------------------------------------------------------------
# 2. CANDIDATE PREDICTORS (49)
# ------------------------------------------------------------

predictor_vars <- c(
  # Distributional characteristics (7)
  "mean_cases", "dispersion_index", "coefficient_of_variation",
  "variance_cases", "skewness_cases", "kurtosis_cases", "gini_coefficient",
  # Temporal structure (9)
  "trend_strength", "trend_slope", "seasonality_strength",
  "lag1_autocorrelation", "lag2_autocorrelation", "lag4_autocorrelation",
  "lag52_autocorrelation", "acf_decay", "partial_acf_lag1",
  # Spectral and complexity measures (4)
  "spectral_entropy", "dominant_period", "sample_entropy", "persistence_entropy",
  # Data quality characteristics (9)
  "missingness_rate", "heaping_index", "repetition_index",
  "proportion_zero_counts", "longest_zero_run", "longest_nonzero_run",
  "intermittency", "run_length_cv", "zero_cluster_index",
  # Outbreak and structural characteristics (10)
  "outbreak_weeks", "outbreak_week_rate", "peak_to_mean_ratio",
  "level_shift_index", "change_point_count", "change_point_rate",
  "outbreak_dominance_index", "max_to_median_ratio", "upper_tail_weight",
  "seasonal_peak_timing_instability",
  # Signal quality and noise characteristics (6)
  "signal_to_noise_ratio", "stl_remainder_strength",
  "rolling_variance_instability", "rolling_mean_instability",
  "residual_acf_strength", "noise_to_signal_ratio",
  # Inter-annual stability characteristics (4)
  "yoy_mean_cv", "yoy_peak_cv", "yoy_variance_cv", "seasonal_subseries_cv"
)

stopifnot(length(predictor_vars) == 49)

# These features are already log1p-transformed during feature extraction.
log1p_features <- c(
  "mean_cases", "dispersion_index", "variance_cases",
  "peak_to_mean_ratio", "level_shift_index", "change_point_count",
  "outbreak_weeks", "longest_zero_run", "longest_nonzero_run",
  "max_to_median_ratio", "dominant_period", "rolling_variance_instability",
  "rolling_mean_instability", "signal_to_noise_ratio", "noise_to_signal_ratio"
)

# ------------------------------------------------------------
# 3. GENERAL HELPERS
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

clean_names <- function(x) {
  x <- stringr::str_to_lower(stringr::str_trim(as.character(x)))
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "_")
  x <- stringr::str_replace_all(x, "^_|_$", "")
  x[x == ""] <- "x"
  make.unique(x, sep = "_")
}

clean_values <- function(x) {
  x <- stringr::str_to_lower(stringr::str_trim(as.character(x)))
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "_")
  stringr::str_replace_all(x, "^_|_$", "")
}

safe_numeric <- function(x) {
  x <- stringr::str_replace_all(as.character(x), ",|%", "")
  out <- suppressWarnings(as.numeric(x))
  out[!is.finite(out)] <- NA_real_
  out
}

find_sheet <- function(available_sheets, candidates) {
  lookup <- tibble(original = available_sheets,
                   clean = clean_names(available_sheets))
  for (candidate in clean_names(candidates)) {
    idx <- which(lookup$clean == candidate)
    if (length(idx)) return(lookup$original[idx[1]])
  }
  NA_character_
}

standardize_readiness_text <- function(x) {
  y <- clean_values(x)
  dplyr::case_when(
    y %in% c("ready", "forecast_ready", "forecasting_ready",
             "yes", "true", "1") ~ "Ready",
    y %in% c("not_ready", "notready", "not_forecast_ready",
             "not_forecasting_ready", "no", "false", "0") ~ "Not_Ready",
    TRUE ~ NA_character_
  )
}

derive_readiness_class <- function(df, threshold = 1.00) {
  out <- rep(NA_character_, nrow(df))
  if ("best_mase" %in% names(df)) {
    bm <- safe_numeric(df$best_mase)
    valid <- !is.na(bm)
    out[valid] <- ifelse(bm[valid] < threshold, "Ready", "Not_Ready")
  }
  for (col in c("readiness_class", "readiness", "readiness_label", "ready_label")) {
    if (col %in% names(df)) {
      candidate <- standardize_readiness_text(df[[col]])
      idx <- is.na(out) & !is.na(candidate)
      out[idx] <- candidate[idx]
    }
  }
  if ("forecast_ready" %in% names(df)) {
    fr <- safe_numeric(df$forecast_ready)
    idx <- is.na(out) & !is.na(fr)
    out[idx] <- ifelse(fr[idx] == 1, "Ready",
                       ifelse(fr[idx] == 0, "Not_Ready", NA_character_))
  }
  if ("y_notready" %in% names(df)) {
    ynr <- safe_numeric(df$y_notready)
    idx <- is.na(out) & !is.na(ynr)
    out[idx] <- ifelse(ynr[idx] == 1, "Not_Ready",
                       ifelse(ynr[idx] == 0, "Ready", NA_character_))
  }
  out
}

stable_fold_seed <- function(label, base_seed = 2026L) {
  as.integer((base_seed + sum(utf8ToInt(enc2utf8(as.character(label)))) ) %%
               .Machine$integer.max)
}

clamp_prob <- function(p, eps = 1e-6) {
  pmin(pmax(as.numeric(p), eps), 1 - eps)
}

compute_auc_safe <- function(actual, probability_notready) {
  valid <- !is.na(actual) & !is.na(probability_notready)
  actual <- factor(actual[valid], levels = c("Ready", "Not_Ready"))
  probability_notready <- probability_notready[valid]
  if (length(unique(stats::na.omit(actual))) < 2) return(NA_real_)
  tryCatch(as.numeric(pROC::auc(pROC::roc(
    response = actual, predictor = probability_notready,
    levels = c("Ready", "Not_Ready"), quiet = TRUE
  ))), error = function(e) NA_real_)
}

compute_pr_auc_safe <- function(actual, probability_notready) {
  valid <- !is.na(actual) & !is.na(probability_notready)
  actual <- as.character(actual[valid])
  probability_notready <- probability_notready[valid]
  positive <- actual == "Not_Ready"
  if (length(unique(positive)) < 2) return(NA_real_)
  tryCatch(as.numeric(PRROC::pr.curve(
    scores.class0 = probability_notready[positive],
    scores.class1 = probability_notready[!positive],
    curve = FALSE
  )$auc.integral), error = function(e) NA_real_)
}

compute_brier_score <- function(actual, probability_notready) {
  valid <- !is.na(actual) & !is.na(probability_notready)
  if (!any(valid)) return(NA_real_)
  actual_bin <- as.integer(as.character(actual[valid]) == "Not_Ready")
  mean((probability_notready[valid] - actual_bin)^2)
}

selection_score_function <- function(balanced_accuracy, f1_notready,
                                     recall_notready, specificity_ready,
                                     degenerate_flag) {
  ifelse(degenerate_flag != "OK", -Inf,
         0.35 * balanced_accuracy + 0.35 * f1_notready +
           0.20 * recall_notready + 0.10 * specificity_ready)
}

make_confusion_metrics <- function(actual, probability_notready, threshold,
                                   model_name, heldout_disease) {
  valid <- !is.na(actual) & !is.na(probability_notready)
  actual <- factor(as.character(actual[valid]),
                   levels = c("Not_Ready", "Ready"))
  probability_notready <- probability_notready[valid]
  predicted <- factor(ifelse(probability_notready >= threshold,
                             "Not_Ready", "Ready"),
                      levels = c("Not_Ready", "Ready"))
  cm <- table(Predicted = predicted, Actual = actual)
  cm_full <- matrix(0, 2, 2,
                    dimnames = list(Predicted = c("Not_Ready", "Ready"),
                                    Actual = c("Not_Ready", "Ready")))
  cm_full[rownames(cm), colnames(cm)] <- cm
  cm <- cm_full
  TP <- cm["Not_Ready", "Not_Ready"]; TN <- cm["Ready", "Ready"]
  FP <- cm["Not_Ready", "Ready"]; FN <- cm["Ready", "Not_Ready"]
  n <- sum(cm)
  accuracy <- if (n == 0) NA_real_ else (TP + TN) / n
  recall <- if (TP + FN == 0) 0 else TP / (TP + FN)
  specificity <- if (TN + FP == 0) 0 else TN / (TN + FP)
  balanced_accuracy <- mean(c(recall, specificity))
  precision <- if (TP + FP == 0) 0 else TP / (TP + FP)
  f1 <- if (precision + recall == 0) 0 else 2 * precision * recall / (precision + recall)
  expected <- if (n == 0) NA_real_ else
    sum(rowSums(cm) * colSums(cm)) / n^2
  kappa <- if (is.na(expected) || 1 - expected == 0) NA_real_ else
    (accuracy - expected) / (1 - expected)
  mcc_den <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  mcc <- if (mcc_den == 0) 0 else (TP * TN - FP * FN) / mcc_den
  flag <- if (recall == 0) "No_NotReady_Detected" else
    if (specificity == 0) "No_Ready_Detected" else "OK"
  tibble(
    Preprocessing = "Fold-specific median imputation; fold-specific NZV removal",
    Heldout_Disease = heldout_disease, Model = model_name,
    Threshold = threshold, Score_Cutoff = 100 * (1 - threshold),
    N_Test = n, Accuracy = accuracy, Balanced_Accuracy = balanced_accuracy,
    Precision_NotReady = precision, Recall_NotReady = recall,
    Specificity_Ready = specificity, F1_NotReady = f1,
    Selection_Score = selection_score_function(balanced_accuracy, f1, recall, specificity, flag),
    AUC = compute_auc_safe(actual, probability_notready),
    PR_AUC = compute_pr_auc_safe(actual, probability_notready),
    Brier_Score = compute_brier_score(actual, probability_notready),
    Kappa = kappa, MCC = mcc, Degenerate_Flag = flag,
    TP = as.integer(TP), TN = as.integer(TN), FP = as.integer(FP), FN = as.integer(FN)
  )
}

select_best_threshold <- function(actual, probability_notready,
                                   model_name, heldout_disease,
                                   thresholds = threshold_grid) {
  tab <- purrr::map_dfr(thresholds, function(threshold) {
    make_confusion_metrics(actual, probability_notready, threshold,
                           model_name, heldout_disease)
  })
  eligible <- tab %>% filter(is.finite(Selection_Score))
  if (!nrow(eligible)) return(list(threshold = 0.50, threshold_table = tab))
  best <- eligible %>% arrange(desc(Selection_Score),
                               desc(Balanced_Accuracy), desc(F1_NotReady),
                               desc(Recall_NotReady), abs(Threshold - 0.50)) %>%
    slice(1)
  list(threshold = best$Threshold[1], threshold_table = tab)
}

safe_sheet_write <- function(wb, sheet_name, data) {
  if (sheet_name %in% openxlsx::sheets(wb)) {
    openxlsx::removeWorksheet(wb, sheet_name)
  }
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet = sheet_name, x = data, rowNames = FALSE)
  wb
}

# ------------------------------------------------------------
# 4. DISEASE-GROUPED INTERNAL SPLIT
# ------------------------------------------------------------

split_internal_by_disease <- function(train_all, validation_prop,
                                      fold_seed, max_attempts = 250) {
  diseases <- sort(unique(as.character(train_all$disease_seed)))
  if (length(diseases) < 3) {
    stop("At least three non-held-out diseases are required for grouped internal validation.")
  }
  n_val <- max(1, min(length(diseases) - 1,
                      round(length(diseases) * validation_prop)))
  set.seed(fold_seed)
  for (attempt in seq_len(max_attempts)) {
    val_diseases <- sample(diseases, n_val, replace = FALSE)
    val <- train_all %>% filter(.data$disease_seed %in% val_diseases)
    fit <- train_all %>% filter(!.data$disease_seed %in% val_diseases)
    fit_classes <- unique(as.character(fit$Readiness_Class))
    val_classes <- unique(as.character(val$Readiness_Class))
    if (nrow(fit) > 0 && nrow(val) > 0 &&
        all(c("Ready", "Not_Ready") %in% fit_classes) &&
        all(c("Ready", "Not_Ready") %in% val_classes)) {
      return(list(internal_fit = fit, internal_val = val,
                  validation_diseases = val_diseases, attempt = attempt))
    }
  }
  stop("Could not create grouped internal split with both classes in fit and validation sets.")
}

# ------------------------------------------------------------
# 5. PREPROCESSING
# ------------------------------------------------------------

preprocess_train_test <- function(train_df, test_df, predictor_set) {
  vars <- intersect(predictor_set, intersect(names(train_df), names(test_df)))
  if (length(vars) < 2) stop("Fewer than two predictors are available in this fold.")
  train_x <- as.data.frame(lapply(train_df[, vars, drop = FALSE], safe_numeric))
  test_x <- as.data.frame(lapply(test_df[, vars, drop = FALSE], safe_numeric))
  names(train_x) <- vars; names(test_x) <- vars

  imputation <- tibble(Variable = character(), Median_Used = numeric(),
                       Transformation = character())
  for (v in vars) {
    med <- median(train_x[[v]], na.rm = TRUE)
    if (!is.finite(med)) med <- 0
    train_x[[v]][is.na(train_x[[v]])] <- med
    test_x[[v]][is.na(test_x[[v]])] <- med
    imputation <- bind_rows(imputation, tibble(
      Variable = v, Median_Used = med,
      Transformation = ifelse(v %in% log1p_features,
                              "log1p_transformed_during_feature_extraction", "none")
    ))
  }
  nzv <- caret::nearZeroVar(train_x)
  removed <- character()
  if (length(nzv)) {
    removed <- names(train_x)[nzv]
    train_x <- train_x[, -nzv, drop = FALSE]
    test_x <- test_x[, -nzv, drop = FALSE]
  }
  if (ncol(train_x) < 2) stop("Fewer than two predictors remain after NZV removal.")
  list(train_x = train_x, test_x = test_x, predictors_used = names(train_x),
       predictors_removed_nzv = removed, imputation_table = imputation)
}

# ------------------------------------------------------------
# 6. STATISTICAL COMPONENTS
# ------------------------------------------------------------

make_stratified_foldid <- function(y_bin, nfolds) {
  nfolds <- min(nfolds, min(table(y_bin)))
  if (nfolds < 2) stop("At least two observations per class are needed for CV.")
  folds <- caret::createFolds(factor(y_bin, levels = c(0, 1)),
                              k = nfolds, returnTrain = FALSE)
  foldid <- integer(length(y_bin))
  for (i in seq_along(folds)) foldid[folds[[i]]] <- i
  foldid
}

select_low_vif_predictors <- function(train_x, y_bin, threshold = 10) {
  train_x <- as.data.frame(train_x)
  if (ncol(train_x) < 2) return(names(train_x))
  model_df <- bind_cols(tibble(y_notready = y_bin), train_x)
  fit <- tryCatch(suppressWarnings(glm(y_notready ~ ., data = model_df,
                                       family = binomial())), error = function(e) NULL)
  if (is.null(fit)) return(names(train_x))
  vif_values <- tryCatch(car::vif(fit), error = function(e) NULL)
  if (is.null(vif_values)) return(names(train_x))
  if (is.matrix(vif_values)) {
    if ("GVIF^(1/(2*Df))" %in% colnames(vif_values)) {
      vif_numeric <- vif_values[, "GVIF^(1/(2*Df))"]^2
    } else {
      vif_numeric <- vif_values[, 1]
    }
  } else vif_numeric <- vif_values
  keep <- intersect(names(vif_numeric)[is.finite(vif_numeric) & vif_numeric <= threshold],
                    names(train_x))
  if (length(keep) < 2) names(train_x) else keep
}

fit_stat_component <- function(train_x, train_y, model_type) {
  train_x <- as.data.frame(train_x)
  y_bin <- as.integer(as.character(train_y) == "Not_Ready")
  if (length(unique(y_bin)) < 2) stop("Statistical training data contain only one class.")

  if (model_type %in% c("lasso", "ridge", "elastic_net")) {
    alpha <- if (model_type == "lasso") 1 else if (model_type == "ridge") 0 else elastic_alpha_grid
    foldid <- make_stratified_foldid(y_bin, cv_folds_stat)
    fits <- lapply(alpha, function(a) glmnet::cv.glmnet(
      x = as.matrix(train_x), y = y_bin, family = "binomial", alpha = a,
      lambda = glmnet_lambda_grid, type.measure = "deviance",
      foldid = foldid, standardize = TRUE
    ))
    cv_min <- vapply(fits, function(f) min(f$cvm, na.rm = TRUE), numeric(1))
    best <- which.min(cv_min)
    return(list(type = model_type, fit = fits[[best]], alpha = alpha[best],
                predictors = names(train_x),
                tuning = paste0("alpha=", alpha[best],
                                "; lambda.min=", signif(fits[[best]]$lambda.min, 6))))
  }

  predictors <- names(train_x)
  if (model_type %in% c("lowvif_logistic", "lowvif_stepwise_bic")) {
    predictors <- select_low_vif_predictors(train_x, y_bin, vif_threshold)
  }
  model_df <- bind_cols(tibble(y_notready = y_bin),
                        train_x[, predictors, drop = FALSE])
  formula <- stats::reformulate(predictors, response = "y_notready")

  if (model_type == "bayesian") {
    fit <- arm::bayesglm(formula, data = model_df, family = binomial(),
                         prior.scale = 2.5, prior.df = 1, maxit = 100)
    return(list(type = model_type, fit = fit, predictors = predictors,
                tuning = "Weakly informative Bayesian logistic regression"))
  }

  if (model_type %in% c("stepwise_bic", "lowvif_stepwise_bic", "stepwise_aic")) {
    full <- suppressWarnings(glm(formula, data = model_df, family = binomial(),
                                 control = list(maxit = 100)))
    k <- if (model_type == "stepwise_aic") 2 else log(nrow(model_df))
    fit <- suppressWarnings(MASS::stepAIC(full, direction = "both", k = k, trace = FALSE))
    return(list(type = model_type, fit = fit, predictors = predictors,
                tuning = paste0(if (k == 2) "AIC" else "BIC",
                                " stepwise; initial predictors=", length(predictors))))
  }

  if (model_type == "lowvif_logistic") {
    fit <- suppressWarnings(glm(formula, data = model_df, family = binomial(),
                                 control = list(maxit = 100)))
    return(list(type = model_type, fit = fit, predictors = predictors,
                tuning = paste0("Low-VIF logistic; predictors=", length(predictors))))
  }
  stop("Unsupported statistical model type: ", model_type)
}

predict_stat_component <- function(model, new_x) {
  new_x <- as.data.frame(new_x)
  missing <- setdiff(model$predictors, names(new_x))
  if (length(missing)) stop("Prediction data missing predictors: ", paste(missing, collapse = ", "))
  new_x <- new_x[, model$predictors, drop = FALSE]
  if (model$type %in% c("lasso", "ridge", "elastic_net")) {
    return(clamp_prob(predict(model$fit, newx = as.matrix(new_x),
                              s = "lambda.min", type = "response")))
  }
  clamp_prob(predict(model$fit, newdata = new_x, type = "response"))
}

# ------------------------------------------------------------
# 7. MACHINE-LEARNING COMPONENTS
# ------------------------------------------------------------

fit_xgb_component <- function(train_x, train_y) {
  train_x <- as.data.frame(train_x)
  y <- as.integer(as.character(train_y) == "Not_Ready")
  if (length(unique(y)) < 2) stop("XGBoost training data contain only one class.")
  foldid <- make_stratified_foldid(y, cv_folds_ml)
  params <- list(objective = "binary:logistic", eval_metric = "logloss",
                 max_depth = 3, eta = 0.05, subsample = 0.85,
                 colsample_bytree = 0.85, min_child_weight = 1,
                 gamma = 0, lambda = 1, alpha = 0, nthread = xgb_nthread)
  dtrain <- xgboost::xgb.DMatrix(as.matrix(train_x), label = y)
  xgb_cv <- xgboost::xgb.cv(params = params, data = dtrain,
                            nrounds = xgb_max_rounds, nfold = length(unique(foldid)),
                            stratified = TRUE, early_stopping_rounds = xgb_early_stopping,
                            verbose = 0)
  best_rounds <- xgb_cv$best_iteration
  if (is.null(best_rounds) || !length(best_rounds) || is.na(best_rounds) ||
      !is.finite(best_rounds)) {
    best_rounds <- which.min(xgb_cv$evaluation_log$test_logloss_mean)
  }
  if (is.null(best_rounds) || !length(best_rounds) || is.na(best_rounds) ||
      !is.finite(best_rounds)) best_rounds <- 150

  # OOF predictions used to fit Platt calibration.
  set.seed(2026)
  folds <- caret::createFolds(factor(y, levels = c(0, 1)),
                              k = length(unique(foldid)), returnTrain = FALSE)
  oof <- rep(NA_real_, length(y))
  for (i in seq_along(folds)) {
    valid <- folds[[i]]
    fit_idx <- setdiff(seq_along(y), valid)
    d_fit <- xgboost::xgb.DMatrix(as.matrix(train_x[fit_idx, , drop = FALSE]),
                                  label = y[fit_idx])
    d_valid <- xgboost::xgb.DMatrix(as.matrix(train_x[valid, , drop = FALSE]))
    fold_fit <- xgboost::xgb.train(params = params, data = d_fit,
                                   nrounds = best_rounds, verbose = 0)
    oof[valid] <- predict(fold_fit, d_valid)
  }
  platt <- tryCatch({
    cal <- data.frame(y = y, eta = qlogis(clamp_prob(oof)))
    cal <- cal[is.finite(cal$eta) & !is.na(cal$y), , drop = FALSE]
    if (nrow(cal) < 10 || length(unique(cal$y)) < 2) NULL else
      glm(y ~ eta, data = cal, family = binomial())
  }, error = function(e) NULL)
  fit <- xgboost::xgb.train(params = params, data = dtrain,
                            nrounds = best_rounds, verbose = 0)
  list(type = "xgboost", fit = fit, platt_model = platt,
       best_nrounds = best_rounds, params = params, predictors = names(train_x),
       tuning = paste0("nrounds=", best_rounds))
}

predict_xgb_component <- function(model, new_x) {
  missing <- setdiff(model$predictors, names(new_x))
  if (length(missing)) stop("Prediction data missing XGBoost predictors.")
  dtest <- xgboost::xgb.DMatrix(as.matrix(new_x[, model$predictors, drop = FALSE]))
  raw <- clamp_prob(predict(model$fit, dtest))
  if (is.null(model$platt_model)) return(raw)
  calibrated <- tryCatch(as.numeric(predict(
    model$platt_model,
    newdata = data.frame(eta = qlogis(raw)), type = "response"
  )), error = function(e) raw)
  clamp_prob(calibrated)
}

fit_ranger_component <- function(train_x, train_y) {
  train_x <- as.data.frame(train_x)
  train_y <- factor(train_y, levels = c("Not_Ready", "Ready"))
  if (length(unique(train_y)) < 2) stop("Ranger training data contain only one class.")
  nfolds <- min(cv_folds_ml, min(table(train_y)))
  if (nfolds < 2) stop("Ranger CV needs at least two observations per class.")
  mtry <- unique(pmax(1, pmin(ncol(train_x),
                              c(ranger_mtry_candidates, floor(sqrt(ncol(train_x)))))))
  grid <- expand.grid(mtry = mtry, splitrule = "gini",
                      min.node.size = ranger_min_node_sizes)
  control <- caret::trainControl(method = "cv", number = nfolds,
                                 classProbs = TRUE,
                                 summaryFunction = caret::twoClassSummary,
                                 savePredictions = "final",
                                 allowParallel = TRUE)
  fit <- caret::train(x = train_x, y = train_y, method = "ranger",
                      metric = "ROC", trControl = control, tuneGrid = grid,
                      num.trees = ranger_num_trees, importance = "impurity")
  tuning <- paste0("mtry=", fit$bestTune$mtry,
                   "; splitrule=", fit$bestTune$splitrule,
                   "; min.node.size=", fit$bestTune$min.node.size,
                   "; num.trees=", ranger_num_trees)
  list(type = "ranger", fit = fit, predictors = names(train_x),
       best_tune = fit$bestTune, tuning = tuning)
}

predict_ranger_component <- function(model, new_x) {
  missing <- setdiff(model$predictors, names(new_x))
  if (length(missing)) stop("Prediction data missing Ranger predictors.")
  probs <- predict(model$fit, newdata = new_x[, model$predictors, drop = FALSE],
                   type = "prob")
  if (!"Not_Ready" %in% names(probs)) stop("Ranger did not return Not_Ready probabilities.")
  clamp_prob(probs$Not_Ready)
}

fit_ml_component <- function(train_x, train_y, model_type) {
  if (model_type == "xgboost") return(fit_xgb_component(train_x, train_y))
  if (model_type == "ranger") return(fit_ranger_component(train_x, train_y))
  stop("Unsupported ML model type: ", model_type)
}

predict_ml_component <- function(model, new_x) {
  if (model$type == "xgboost") return(predict_xgb_component(model, new_x))
  if (model$type == "ranger") return(predict_ranger_component(model, new_x))
  stop("Unsupported fitted ML model type: ", model$type)
}

# ------------------------------------------------------------
# 8. LOAD CONFIGURATION AND DATA
# ------------------------------------------------------------

if (!file.exists(source_workbook_path)) stop("Source workbook not found: ", source_workbook_path)
if (!file.exists(output_workbook_path)) stop("Results workbook not found: ", output_workbook_path)

epiforess_results <- tryCatch(readRDS(epiforess_results_path), error = function(e) {
  warning("Could not load final model RDS; using fallback Stepwise BIC + Ranger. ",
          e$message)
  NULL
})
if (!is.null(epiforess_results) && isTRUE(epiforess_results$setup_check_mode)) {
  stop("Saved model RDS is in setup_check_mode; rerun the main modeling script.")
}

best_stat_name <- get_list_value(epiforess_results,
  c("best_statistical_name", "selected_statistical_model", "best_stat_name"),
  "Stepwise_Logistic_BIC")
best_ml_name <- get_list_value(epiforess_results,
  c("best_ml_name", "selected_ml_model", "best_machine_learning_name"),
  "Ranger_Random_Forest")

stat_model_type <- dplyr::case_when(
  grepl("LowVIF.*Stepwise.*BIC|Low_VIF.*Stepwise.*BIC", best_stat_name, ignore.case = TRUE) ~ "lowvif_stepwise_bic",
  grepl("LowVIF.*Logistic|Low_VIF.*Logistic", best_stat_name, ignore.case = TRUE) ~ "lowvif_logistic",
  grepl("Elastic", best_stat_name, ignore.case = TRUE) ~ "elastic_net",
  grepl("LASSO", best_stat_name, ignore.case = TRUE) ~ "lasso",
  grepl("Ridge", best_stat_name, ignore.case = TRUE) ~ "ridge",
  grepl("Bayesian", best_stat_name, ignore.case = TRUE) ~ "bayesian",
  grepl("BIC", best_stat_name, ignore.case = TRUE) ~ "stepwise_bic",
  grepl("AIC", best_stat_name, ignore.case = TRUE) ~ "stepwise_aic",
  grepl("^Binary_Logistic_Regression$|^Binary Logistic Regression$", best_stat_name, ignore.case = TRUE) ~ "removed_binary_logistic",
  TRUE ~ "unsupported"
)
ml_model_type <- dplyr::case_when(
  grepl("Ranger|Random_Forest|Random Forest", best_ml_name, ignore.case = TRUE) ~ "ranger",
  grepl("XGBoost|XGB", best_ml_name, ignore.case = TRUE) ~ "xgboost",
  TRUE ~ "unsupported"
)
if (stat_model_type == "removed_binary_logistic") {
  stop("Saved configuration selects removed ordinary Binary Logistic Regression. Rerun main model selection.")
}
if (stat_model_type == "unsupported") stop("Unsupported statistical model name: ", best_stat_name)
if (ml_model_type == "unsupported") stop("Unsupported ML model name: ", best_ml_name)

main_stat_brier <- get_list_value(epiforess_results,
  c("stat_brier_validation", "final_stat_brier", "best_statistical_brier"), NA_real_)
main_ml_brier <- get_list_value(epiforess_results,
  c("ml_brier_validation", "final_ml_brier", "best_ml_brier"), NA_real_)
main_weight_stat <- get_list_value(epiforess_results,
  c("weight_statistical", "final_weight_stat", "weight_stat", "w_stat"), NA_real_)
main_weight_ml <- get_list_value(epiforess_results,
  c("weight_ml", "final_weight_ml", "weight_ml", "w_ml"), NA_real_)
main_hybrid_threshold <- get_list_value(epiforess_results,
  c("hybrid_threshold", "best_hybrid_threshold"), 0.50)

source_sheets <- openxlsx::getSheetNames(source_workbook_path)
results_sheets <- openxlsx::getSheetNames(output_workbook_path)
features_sheet <- find_sheet(source_sheets, c("Extracted_Features", "Modeling_Dataset"))
if (is.na(features_sheet)) stop("No Extracted_Features or Modeling_Dataset sheet in source workbook.")
features_raw <- readxl::read_excel(source_workbook_path, sheet = features_sheet)
names(features_raw) <- clean_names(names(features_raw))

readiness_sheet <- find_sheet(results_sheets, c(
  "Readiness_Labels", "Modeling_Dataset_Labeled", "Forecast_Metrics",
  "Forecasting_Metrics", "Raw_Forecasting_Evaluation",
  "Forecast_Evaluation", "Modeling_Dataset"
))
if (!is.na(readiness_sheet)) {
  readiness_raw <- readxl::read_excel(output_workbook_path, sheet = readiness_sheet)
} else {
  csv_path <- file.path(base_path, "forecast_metrics.csv")
  if (!file.exists(csv_path)) stop("No readiness sheet or forecast_metrics.csv found.")
  readiness_raw <- read.csv(csv_path, stringsAsFactors = FALSE)
}
names(readiness_raw) <- clean_names(names(readiness_raw))

id_cols <- c("global_simulation_id", "disease_seed", "disease_simulation_id")
if (length(setdiff(id_cols, names(features_raw)))) {
  stop("Feature data missing IDs: ", paste(setdiff(id_cols, names(features_raw)), collapse = ", "))
}
if (length(setdiff(id_cols, names(readiness_raw)))) {
  stop("Readiness data missing IDs: ", paste(setdiff(id_cols, names(readiness_raw)), collapse = ", "))
}

label_cols <- intersect(c("readiness_class", "readiness", "readiness_label",
                          "ready_label", "forecast_ready", "y_notready",
                          "best_mase", "best_model"), names(readiness_raw))
if (!length(label_cols)) stop("No Best_MASE or readiness-label field found.")
readiness_small <- readiness_raw %>% select(all_of(c(id_cols, label_cols))) %>% distinct()
duplicate_keys <- readiness_small %>% count(across(all_of(id_cols)), name = "n") %>% filter(n > 1)
if (nrow(duplicate_keys)) stop("Duplicate readiness keys found. Resolve before LODO.")

lodo_data <- features_raw %>% left_join(readiness_small, by = id_cols,
                                        suffix = c("", "_label"))
if (!"best_mase" %in% names(lodo_data) && "best_mase_label" %in% names(lodo_data)) {
  lodo_data$best_mase <- lodo_data$best_mase_label
}
if (!"best_model" %in% names(lodo_data) && "best_model_label" %in% names(lodo_data)) {
  lodo_data$best_model <- lodo_data$best_model_label
}
lodo_data$Readiness_Class <- derive_readiness_class(lodo_data, ready_mase_threshold)

cat("\nReadiness distribution before filtering:\n")
print(table(lodo_data$Readiness_Class, useNA = "ifany"))

lodo_data <- lodo_data %>%
  filter(!is.na(.data$disease_seed),
         !is.na(.data$Readiness_Class),
         .data$Readiness_Class %in% c("Ready", "Not_Ready")) %>%
  mutate(Readiness_Class = factor(.data$Readiness_Class,
                                  levels = c("Ready", "Not_Ready")))

if ("best_mase" %in% names(lodo_data)) {
  bm <- safe_numeric(lodo_data$best_mase)
  valid <- !is.na(bm)
  stopifnot(all(as.character(lodo_data$Readiness_Class[valid]) ==
                  ifelse(bm[valid] < ready_mase_threshold, "Ready", "Not_Ready")))
}

predictors_present <- intersect(predictor_vars, names(lodo_data))
missing_predictors <- setdiff(predictor_vars, predictors_present)
if (length(missing_predictors)) warning("Skipping absent predictors: ",
                                        paste(missing_predictors, collapse = ", "))
if (!nrow(lodo_data)) stop("No rows remain after readiness filtering.")
if (length(unique(lodo_data$Readiness_Class)) < 2) stop("Only one readiness class remains.")
if (dplyr::n_distinct(lodo_data$disease_seed) < 4) {
  stop("At least four disease seeds are required for one held-out disease plus grouped internal validation.")
}
if (length(predictors_present) < 2) stop("Fewer than two predictors are available.")

cat("\nRows:", nrow(lodo_data), "\nDiseases:", dplyr::n_distinct(lodo_data$disease_seed),
    "\nPredictors present:", length(predictors_present), "of", length(predictor_vars), "\n")

# ------------------------------------------------------------
# 9. ONE LODO FOLD
# ------------------------------------------------------------

run_lodo_fold <- function(heldout_disease) {
  message("\n--- LODO: ", heldout_disease, " ---")
  test_data <- lodo_data %>% filter(.data$disease_seed == heldout_disease)
  train_all <- lodo_data %>% filter(.data$disease_seed != heldout_disease)
  if (!nrow(test_data) || !nrow(train_all)) stop("Empty train/test split.")
  if (length(unique(train_all$Readiness_Class)) < 2) stop("Training diseases contain one class.")

  split <- split_internal_by_disease(train_all, internal_validation_disease_prop,
                                     stable_fold_seed(heldout_disease),
                                     internal_split_max_attempts)
  inner_fit <- split$internal_fit
  inner_val <- split$internal_val
  prep_inner <- preprocess_train_test(inner_fit, inner_val, predictors_present)

  stat_inner <- fit_stat_component(prep_inner$train_x, inner_fit$Readiness_Class,
                                   stat_model_type)
  stat_val <- predict_stat_component(stat_inner, prep_inner$test_x)
  ml_inner <- fit_ml_component(prep_inner$train_x, inner_fit$Readiness_Class,
                               ml_model_type)
  ml_val <- predict_ml_component(ml_inner, prep_inner$test_x)

  brier_stat <- compute_brier_score(inner_val$Readiness_Class, stat_val)
  brier_ml <- compute_brier_score(inner_val$Readiness_Class, ml_val)
  if (!is.finite(brier_stat) || !is.finite(brier_ml)) {
    w_stat <- 0.5; w_ml <- 0.5
    weight_method <- "Equal weights; internal Brier unavailable"
  } else {
    inv_stat <- 1 / max(brier_stat, 1e-8)
    inv_ml <- 1 / max(brier_ml, 1e-8)
    w_stat <- inv_stat / (inv_stat + inv_ml)
    w_ml <- inv_ml / (inv_stat + inv_ml)
    weight_method <- "Inverse internal-validation Brier score"
  }
  hybrid_val <- clamp_prob(w_stat * stat_val + w_ml * ml_val)

  stat_label <- paste0("Stat_", best_stat_name)
  ml_label <- best_ml_name
  stat_thr <- select_best_threshold(inner_val$Readiness_Class, stat_val,
                                    stat_label, paste0(heldout_disease, "_Internal"))$threshold
  ml_thr <- select_best_threshold(inner_val$Readiness_Class, ml_val,
                                  ml_label, paste0(heldout_disease, "_Internal"))$threshold
  hybrid_thr <- select_best_threshold(inner_val$Readiness_Class, hybrid_val,
                                      "Hybrid_EpiFORESS",
                                      paste0(heldout_disease, "_Internal"))$threshold

  prep_final <- preprocess_train_test(train_all, test_data, predictors_present)
  stat_final <- fit_stat_component(prep_final$train_x, train_all$Readiness_Class,
                                   stat_model_type)
  stat_test <- predict_stat_component(stat_final, prep_final$test_x)
  ml_final <- fit_ml_component(prep_final$train_x, train_all$Readiness_Class,
                               ml_model_type)
  ml_test <- predict_ml_component(ml_final, prep_final$test_x)
  hybrid_test <- clamp_prob(w_stat * stat_test + w_ml * ml_test)

  metrics <- bind_rows(
    make_confusion_metrics(test_data$Readiness_Class, stat_test, stat_thr,
                           stat_label, heldout_disease),
    make_confusion_metrics(test_data$Readiness_Class, ml_test, ml_thr,
                           ml_label, heldout_disease),
    make_confusion_metrics(test_data$Readiness_Class, hybrid_test, hybrid_thr,
                           "Hybrid_EpiFORESS", heldout_disease)
  )

  predictions <- test_data %>%
    select(global_simulation_id, disease_seed, disease_simulation_id,
           Readiness_Class, any_of(c("best_mase", "best_model"))) %>%
    mutate(
      Heldout_Disease = heldout_disease,
      Statistical_Model = best_stat_name, ML_Model = best_ml_name,
      Stat_Prob_NotReady = stat_test, ML_Prob_NotReady = ml_test,
      Hybrid_Prob_NotReady = hybrid_test,
      Stat_Threshold = stat_thr, ML_Threshold = ml_thr,
      Hybrid_Threshold = hybrid_thr,
      Stat_Predicted = ifelse(stat_test >= stat_thr, "Not_Ready", "Ready"),
      ML_Predicted = ifelse(ml_test >= ml_thr, "Not_Ready", "Ready"),
      Hybrid_Predicted = ifelse(hybrid_test >= hybrid_thr, "Not_Ready", "Ready"),
      EpiFORESS = 100 * (1 - hybrid_test),
      EpiFORESS_Band = case_when(
        EpiFORESS < 40 ~ "Very_Low",
        EpiFORESS < 60 ~ "Low",
        EpiFORESS < 80 ~ "Moderate",
        EpiFORESS < 90 ~ "High",
        TRUE ~ "Very_High"
      )
    )

  weights <- tibble(
    Heldout_Disease = heldout_disease,
    N_Train_All = nrow(train_all), N_Test = nrow(test_data),
    N_Internal_Fit = nrow(inner_fit), N_Internal_Validation = nrow(inner_val),
    Internal_Validation_Diseases = paste(sort(split$validation_diseases), collapse = "; "),
    Internal_Split_Attempt = split$attempt,
    Train_Ready = sum(train_all$Readiness_Class == "Ready"),
    Train_NotReady = sum(train_all$Readiness_Class == "Not_Ready"),
    Test_Ready = sum(test_data$Readiness_Class == "Ready"),
    Test_NotReady = sum(test_data$Readiness_Class == "Not_Ready"),
    Best_Statistical_Name = best_stat_name, Statistical_Model_Type = stat_model_type,
    Best_ML_Name = best_ml_name, ML_Model_Type = ml_model_type,
    Internal_Stat_Brier = brier_stat, Internal_ML_Brier = brier_ml,
    Fold_Weight_Statistical = w_stat, Fold_Weight_ML = w_ml,
    Fold_Weight_Method = weight_method,
    Fold_Stat_Threshold = stat_thr, Fold_ML_Threshold = ml_thr,
    Fold_Hybrid_Threshold = hybrid_thr,
    Fold_Hybrid_Score_Cutoff = 100 * (1 - hybrid_thr),
    Internal_Stat_Tuning = stat_inner$tuning %||% NA_character_,
    Final_Stat_Tuning = stat_final$tuning %||% NA_character_,
    Internal_ML_Tuning = ml_inner$tuning %||% NA_character_,
    Final_ML_Tuning = ml_final$tuning %||% NA_character_,
    N_Predictors_Used = length(prep_final$predictors_used),
    Predictors_Used = paste(prep_final$predictors_used, collapse = ", "),
    Predictors_Removed_NZV = paste(prep_final$predictors_removed_nzv, collapse = ", ")
  )
  imputation <- prep_final$imputation_table %>%
    mutate(Heldout_Disease = heldout_disease,
           Used_In_Final_Fold = Variable %in% prep_final$predictors_used,
           Removed_By_NZV = Variable %in% prep_final$predictors_removed_nzv) %>%
    relocate(Heldout_Disease)

  thresholds <- bind_rows(
    select_best_threshold(inner_val$Readiness_Class, stat_val, stat_label,
                          paste0(heldout_disease, "_Internal"))$threshold_table %>%
      mutate(Threshold_Source = "Internal_Statistical"),
    select_best_threshold(inner_val$Readiness_Class, ml_val, ml_label,
                          paste0(heldout_disease, "_Internal"))$threshold_table %>%
      mutate(Threshold_Source = paste0("Internal_", best_ml_name)),
    select_best_threshold(inner_val$Readiness_Class, hybrid_val, "Hybrid_EpiFORESS",
                          paste0(heldout_disease, "_Internal"))$threshold_table %>%
      mutate(Threshold_Source = "Internal_Hybrid")
  )
  list(metrics = metrics, predictions = predictions, weights = weights,
       imputation = imputation, threshold_scan = thresholds)
}

# ------------------------------------------------------------
# 10. RUN LODO FOLDS
# ------------------------------------------------------------

diseases <- sort(unique(as.character(lodo_data$disease_seed)))
stat_label <- paste0("Stat_", best_stat_name)
ml_label <- best_ml_name
lodo_results <- list()

for (disease in diseases) {
  lodo_results[[disease]] <- tryCatch(run_lodo_fold(disease), error = function(e) {
    warning("LODO fold failed for ", disease, ": ", conditionMessage(e))
    list(
      metrics = tibble(
        Preprocessing = "Fold failed",
        Heldout_Disease = disease,
        Model = c(stat_label, ml_label, "Hybrid_EpiFORESS"),
        Threshold = NA_real_, Score_Cutoff = NA_real_, N_Test = NA_integer_,
        Accuracy = NA_real_, Balanced_Accuracy = NA_real_,
        Precision_NotReady = NA_real_, Recall_NotReady = NA_real_,
        Specificity_Ready = NA_real_, F1_NotReady = NA_real_,
        Selection_Score = NA_real_, AUC = NA_real_, PR_AUC = NA_real_,
        Brier_Score = NA_real_, Kappa = NA_real_, MCC = NA_real_,
        Degenerate_Flag = paste0("Fold_Error: ", conditionMessage(e)),
        TP = NA_integer_, TN = NA_integer_, FP = NA_integer_, FN = NA_integer_
      ),
      predictions = tibble(),
      weights = tibble(Heldout_Disease = disease, Error = conditionMessage(e)),
      imputation = tibble(), threshold_scan = tibble()
    )
  })
}

lodo_metrics <- map_dfr(lodo_results, "metrics")
lodo_predictions <- map_dfr(lodo_results, "predictions")
lodo_weights <- map_dfr(lodo_results, "weights")
lodo_imputation <- map_dfr(lodo_results, "imputation")
lodo_threshold_scan <- map_dfr(lodo_results, "threshold_scan")

# ------------------------------------------------------------
# 11. SUMMARIES
# ------------------------------------------------------------

lodo_summary <- lodo_metrics %>%
  group_by(Model) %>%
  summarise(
    N_Folds = n(),
    N_Successful_Folds = sum(!grepl("^Fold_Error", Degenerate_Flag), na.rm = TRUE),
    Mean_Accuracy = mean(Accuracy, na.rm = TRUE),
    Median_Accuracy = median(Accuracy, na.rm = TRUE),
    Mean_Balanced_Accuracy = mean(Balanced_Accuracy, na.rm = TRUE),
    Median_Balanced_Accuracy = median(Balanced_Accuracy, na.rm = TRUE),
    Mean_F1_NotReady = mean(F1_NotReady, na.rm = TRUE),
    Median_F1_NotReady = median(F1_NotReady, na.rm = TRUE),
    Mean_Recall_NotReady = mean(Recall_NotReady, na.rm = TRUE),
    Mean_Specificity_Ready = mean(Specificity_Ready, na.rm = TRUE),
    Mean_AUC = mean(AUC, na.rm = TRUE),
    Median_AUC = median(AUC, na.rm = TRUE),
    Mean_PR_AUC = mean(PR_AUC, na.rm = TRUE),
    Mean_Brier = mean(Brier_Score, na.rm = TRUE),
    Median_Brier = median(Brier_Score, na.rm = TRUE),
    Mean_MCC = mean(MCC, na.rm = TRUE),
    Mean_Kappa = mean(Kappa, na.rm = TRUE),
    N_OK_Folds = sum(Degenerate_Flag == "OK", na.rm = TRUE),
    .groups = "drop"
  ) %>% arrange(desc(Mean_Balanced_Accuracy))

make_pooled_metrics <- function(actual, predicted, probability, model_name) {
  valid <- !is.na(actual) & !is.na(predicted) & !is.na(probability)
  actual <- factor(as.character(actual[valid]), levels = c("Not_Ready", "Ready"))
  predicted <- factor(as.character(predicted[valid]), levels = c("Not_Ready", "Ready"))
  probability <- probability[valid]
  cm <- table(Predicted = predicted, Actual = actual)
  full <- matrix(0, 2, 2,
                 dimnames = list(Predicted = c("Not_Ready", "Ready"),
                                 Actual = c("Not_Ready", "Ready")))
  full[rownames(cm), colnames(cm)] <- cm
  cm <- full
  TP <- cm["Not_Ready", "Not_Ready"]; TN <- cm["Ready", "Ready"]
  FP <- cm["Not_Ready", "Ready"]; FN <- cm["Ready", "Not_Ready"]
  n <- sum(cm)
  acc <- if (n == 0) NA_real_ else (TP + TN) / n
  rec <- if (TP + FN == 0) 0 else TP / (TP + FN)
  spec <- if (TN + FP == 0) 0 else TN / (TN + FP)
  ba <- mean(c(rec, spec))
  prec <- if (TP + FP == 0) 0 else TP / (TP + FP)
  f1 <- if (prec + rec == 0) 0 else 2 * prec * rec / (prec + rec)
  den <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  mcc <- if (den == 0) 0 else (TP * TN - FP * FN) / den
  flag <- if (rec == 0) "No_NotReady_Detected" else if (spec == 0) "No_Ready_Detected" else "OK"
  tibble(
    Preprocessing = "Pooled held-out predictions; fold-specific preprocessing",
    Heldout_Disease = "Pooled_LODO", Model = model_name,
    Threshold = NA_real_, Score_Cutoff = NA_real_, N_Test = n,
    Accuracy = acc, Balanced_Accuracy = ba, Precision_NotReady = prec,
    Recall_NotReady = rec, Specificity_Ready = spec, F1_NotReady = f1,
    Selection_Score = selection_score_function(ba, f1, rec, spec, flag),
    AUC = compute_auc_safe(actual, probability),
    PR_AUC = compute_pr_auc_safe(actual, probability),
    Brier_Score = compute_brier_score(actual, probability),
    Kappa = NA_real_, MCC = mcc, Degenerate_Flag = flag,
    TP = as.integer(TP), TN = as.integer(TN), FP = as.integer(FP), FN = as.integer(FN)
  )
}

if (nrow(lodo_predictions)) {
  lodo_pooled_performance <- bind_rows(
    make_pooled_metrics(lodo_predictions$Readiness_Class,
                        lodo_predictions$Stat_Predicted,
                        lodo_predictions$Stat_Prob_NotReady, stat_label),
    make_pooled_metrics(lodo_predictions$Readiness_Class,
                        lodo_predictions$ML_Predicted,
                        lodo_predictions$ML_Prob_NotReady, ml_label),
    make_pooled_metrics(lodo_predictions$Readiness_Class,
                        lodo_predictions$Hybrid_Predicted,
                        lodo_predictions$Hybrid_Prob_NotReady, "Hybrid_EpiFORESS")
  )
  disease_summary <- lodo_predictions %>%
    group_by(Heldout_Disease) %>%
    summarise(
      N = n(), Actual_Ready = sum(Readiness_Class == "Ready"),
      Actual_NotReady = sum(Readiness_Class == "Not_Ready"),
      Mean_EpiFORESS = mean(EpiFORESS, na.rm = TRUE),
      Median_EpiFORESS = median(EpiFORESS, na.rm = TRUE),
      Mean_Hybrid_Prob_NotReady = mean(Hybrid_Prob_NotReady, na.rm = TRUE),
      Median_Hybrid_Prob_NotReady = median(Hybrid_Prob_NotReady, na.rm = TRUE),
      Mean_Hybrid_Score_Cutoff = mean(100 * (1 - Hybrid_Threshold), na.rm = TRUE),
      .groups = "drop"
    ) %>% arrange(desc(Mean_EpiFORESS))
} else {
  lodo_pooled_performance <- tibble()
  disease_summary <- tibble()
}

lodo_config <- tibble(
  Item = c("Ready_MASE_Threshold", "Ready_Rule", "Candidate_Predictor_Count",
           "Predictors_Present_Count", "Missing_Predictor_Count",
           "Log1p_Feature_Count", "Best_Statistical_Name", "Statistical_Model_Type",
           "Best_ML_Name", "LODO_ML_Model_Type", "Hybrid_Weighting",
           "Threshold_Selection", "Internal_Validation_Unit",
           "Internal_Validation_Disease_Proportion", "Main_Stat_Brier",
           "Main_ML_Brier", "Main_Weight_Statistical", "Main_Weight_ML",
           "Main_Hybrid_Threshold", "Main_Hybrid_Score_Cutoff",
           "Source_Workbook", "Output_Workbook", "Model_Output_Directory"),
  Value = as.character(c(
    ready_mase_threshold, paste0("Ready if Best_MASE < ", ready_mase_threshold),
    length(predictor_vars), length(predictors_present), length(missing_predictors),
    length(log1p_features), best_stat_name, stat_model_type, best_ml_name,
    ml_model_type,
    "Fold-specific inverse internal-validation Brier score",
    "Grouped internal validation only; 0.35*BA + 0.35*F1_NotReady + 0.20*Recall_NotReady + 0.10*Specificity_Ready",
    "Whole disease seeds", internal_validation_disease_prop,
    main_stat_brier, main_ml_brier, main_weight_stat, main_weight_ml,
    main_hybrid_threshold, 100 * (1 - main_hybrid_threshold),
    source_workbook_path, output_workbook_path, model_output_dir
  ))
)

# ------------------------------------------------------------
# 12. SAVE WORKBOOKS AND RDS
# ------------------------------------------------------------

lodo_wb <- createWorkbook()
sheets_to_write <- list(
  LODO_Metrics = lodo_metrics,
  LODO_Summary = lodo_summary,
  LODO_Pooled_Perf = lodo_pooled_performance,
  LODO_Preds = lodo_predictions,
  LODO_Weights = lodo_weights,
  LODO_Imputation = lodo_imputation,
  LODO_Thresholds = lodo_threshold_scan,
  Disease_Summary = disease_summary,
  Predictors_Present = tibble(Predictor = predictors_present,
                              Log1p_Transformed_Previously = predictors_present %in% log1p_features),
  Missing_Predictors = tibble(Missing_Predictor = missing_predictors),
  LODO_Config = lodo_config
)
for (nm in names(sheets_to_write)) lodo_wb <- safe_sheet_write(lodo_wb, nm, sheets_to_write[[nm]])
saveWorkbook(lodo_wb, lodo_output_path, overwrite = TRUE)

results_wb <- loadWorkbook(output_workbook_path)
results_sheets_to_write <- list(
  LODO_Metrics = lodo_metrics,
  LODO_Summary = lodo_summary,
  LODO_Pooled_Perf = lodo_pooled_performance,
  LODO_Weights = lodo_weights,
  LODO_Config = lodo_config,
  LODO_Disease_Summary = disease_summary,
  LODO_Predictors = tibble(Predictor = predictors_present,
                           Log1p_Transformed_Previously = predictors_present %in% log1p_features),
  LODO_Missing_Preds = tibble(Missing_Predictor = missing_predictors)
)
for (nm in names(results_sheets_to_write)) {
  results_wb <- safe_sheet_write(results_wb, nm, results_sheets_to_write[[nm]])
}
saveWorkbook(results_wb, output_workbook_path, overwrite = TRUE)

lodo_rds <- list(
  lodo_metrics = lodo_metrics, lodo_summary = lodo_summary,
  lodo_pooled_performance = lodo_pooled_performance,
  lodo_predictions = lodo_predictions, lodo_weights = lodo_weights,
  lodo_imputation = lodo_imputation, lodo_threshold_scan = lodo_threshold_scan,
  disease_summary = disease_summary, lodo_config = lodo_config,
  predictor_vars = predictor_vars, predictors_present = predictors_present,
  missing_predictors = missing_predictors, log1p_features = log1p_features,
  best_statistical_name = best_stat_name, stat_model_type = stat_model_type,
  best_ml_name = best_ml_name, ml_model_type = ml_model_type,
  main_stat_brier = main_stat_brier, main_ml_brier = main_ml_brier,
  main_weight_stat = main_weight_stat, main_weight_ml = main_weight_ml,
  main_hybrid_threshold = main_hybrid_threshold,
  ready_mase_threshold = ready_mase_threshold,
  internal_validation_disease_prop = internal_validation_disease_prop,
  threshold_grid = threshold_grid, created_at = Sys.time()
)
saveRDS(lodo_rds, file.path(model_output_dir, "lodo_validation_results.rds"))
saveRDS(lodo_rds, file.path(model_output_dir, paste0(
  "lodo_validation_", clean_values(best_stat_name), "_",
  clean_values(best_ml_name), "_results.rds"
)))

cat("\nLODO VALIDATION COMPLETE\n")
cat("Readiness rule: Best MASE < ", ready_mase_threshold, "\n", sep = "")
cat("Statistical component: ", best_stat_name, " (", stat_model_type, ")\n", sep = "")
cat("ML component: ", best_ml_name, " (", ml_model_type, ")\n", sep = "")
cat("Folds: ", length(diseases), "\n", sep = "")
cat("Standalone workbook: ", lodo_output_path, "\n", sep = "")
cat("Updated results workbook: ", output_workbook_path, "\n", sep = "")
cat("Saved RDS: ", file.path(model_output_dir, "lodo_validation_results.rds"), "\n")
