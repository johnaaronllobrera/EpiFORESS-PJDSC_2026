# EpiFORESS Analysis Pipeline
# Script: 05_forecast_readiness_modeling.R
# Project root: set EPIFORESS_PROJECT_DIR or run from the project data directory.
# Reproducibility: inspect all settings and verify input/model compatibility before reporting results.

# ============================================================
# EPIFORESS: FORECAST-READINESS MODELING
# Model fitting, validation-based selection, final test evaluation,
# hybrid probability estimation, and exploratory significance checks.
# Author: John Aaron B. Llobrera
# ============================================================

# Pipeline position:
#   01_empirical_characterization_simulation.R
#   02_feature_extraction.R
#   03_forecasting_experiment.R
#   04_forecast_readiness_labeling.R
#   05_forecast_readiness_modeling.R
#
# Methodological safeguards:
# - Uses the primary label: Ready if Best_MASE < 1.00.
# - Uses explicit simulation identifiers for feature/label joins.
# - Splits data before imputation; imputation values come from training only.
# - Selects models, thresholds, calibration, and hybrid weights without
#   using the final test set.
# - Keeps test data untouched until final evaluation.
# - Significance tests are exploratory, not confirmatory, after feature
#   selection or regularization.
#
# IMPORTANT:
# Strict pre-forecast screening requires every predictor to be calculated
# from data available at the screening time, excluding the forecast horizon.
# The script cannot correct leakage already present in Extracted_Features.

# ------------------------------------------------------------
# 0. SETTINGS
# ------------------------------------------------------------

project_dir <- normalizePath(
  Sys.getenv("EPIFORESS_PROJECT_DIR", unset = getwd()),
  winslash = "/",
  mustWork = FALSE
)

if (!dir.exists(project_dir)) {
  stop("Project directory does not exist: ", project_dir, call. = FALSE)
}

# Update these paths if upstream scripts use different filenames.
feature_workbook <- file.path(
  project_dir,
  "EpiFORESS_feature_extraction_outputs.xlsx"
)
label_workbook <- file.path(
  project_dir,
  "EpiFORESS_Forecast_Results.xlsx"
)
output_workbook <- file.path(
  project_dir,
  "EpiFORESS_Modeling_Results.xlsx"
)
model_output_dir <- file.path(project_dir, "Saved_Models")

feature_sheet <- "Extracted_Features"
label_sheet <- "Readiness_Labels"

primary_mase_threshold <- 1.00
sensitivity_mase_threshold <- 0.50

train_fraction <- 0.60
validation_fraction <- 0.20
test_fraction <- 0.20

threshold_grid <- seq(0.01, 0.99, by = 0.01)
selection_weights <- c(BA = 0.35, F1 = 0.35, Recall = 0.20, Specificity = 0.10)

bootstrap_replicates <- 1000
vif_threshold <- 10
set.seed(2026)

if (!dir.exists(model_output_dir)) {
  dir.create(model_output_dir, recursive = TRUE)
}

# Seven-domain, 49-feature set used by the feature-extraction stage.
predictors <- c(
  # Distributional characteristics (7)
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
  # Outbreak and structural characteristics (10)
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

# ------------------------------------------------------------
# 1. PACKAGES AND HELPERS
# ------------------------------------------------------------

packages <- c(
  "readxl", "dplyr", "tibble", "caret", "glmnet", "MASS", "tidyr",
  "xgboost", "pROC", "PRROC", "openxlsx", "purrr", "stringr",
  "arm", "car", "ranger", "ggplot2"
)
missing_packages <- setdiff(packages, rownames(installed.packages()))
if (length(missing_packages)) install.packages(missing_packages)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tibble)
  library(caret)
  library(glmnet)
  library(MASS)
  library(xgboost)
  library(pROC)
  library(PRROC)
  library(openxlsx)
  library(purrr)
  library(stringr)
  library(arm)
  library(car)
  library(ranger)
  library(ggplot2)
})

clean_names <- function(x) {
  x <- stringr::str_to_lower(stringr::str_trim(as.character(x)))
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "_")
  x <- stringr::str_replace_all(x, "^_|_$", "")
  make.unique(x, sep = "_")
}

safe_numeric <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- stringr::str_replace_all(as.character(x), ",|%", "")
  suppressWarnings(as.numeric(x))
}

require_columns <- function(data, cols, label) {
  missing <- setdiff(cols, names(data))
  if (length(missing)) {
    stop(label, " is missing required columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
}

auc_value <- function(actual, prob) {
  actual <- factor(actual, levels = c("Ready", "Not_Ready"))
  if (length(unique(stats::na.omit(actual))) < 2) return(NA_real_)
  tryCatch(
    as.numeric(pROC::auc(pROC::roc(
      actual, prob, levels = c("Ready", "Not_Ready"), quiet = TRUE
    ))),
    error = function(e) NA_real_
  )
}

auc_ci <- function(actual, prob, boot_n = bootstrap_replicates) {
  actual <- factor(actual, levels = c("Ready", "Not_Ready"))
  if (length(unique(stats::na.omit(actual))) < 2) {
    return(tibble(AUC = NA_real_, AUC_CI_Lower_95 = NA_real_,
                  AUC_CI_Upper_95 = NA_real_))
  }
  tryCatch({
    roc_obj <- pROC::roc(actual, prob, levels = c("Ready", "Not_Ready"),
                         quiet = TRUE)
    ci <- pROC::ci.auc(roc_obj, method = "bootstrap", boot.n = boot_n,
                       progress = "none")
    tibble(AUC = as.numeric(pROC::auc(roc_obj)),
           AUC_CI_Lower_95 = as.numeric(ci[1]),
           AUC_CI_Upper_95 = as.numeric(ci[3]))
  }, error = function(e) {
    tibble(AUC = NA_real_, AUC_CI_Lower_95 = NA_real_,
           AUC_CI_Upper_95 = NA_real_)
  })
}

pr_auc_value <- function(actual, prob) {
  positive <- as.character(actual) == "Not_Ready"
  if (length(unique(positive)) < 2) return(NA_real_)
  tryCatch(
    PRROC::pr.curve(scores.class0 = prob[positive],
                    scores.class1 = prob[!positive],
                    curve = FALSE)$auc.integral,
    error = function(e) NA_real_
  )
}

brier_score <- function(actual, prob) {
  y <- as.integer(as.character(actual) == "Not_Ready")
  mean((prob - y)^2, na.rm = TRUE)
}

confusion_metrics <- function(actual, prob, threshold) {
  actual <- factor(actual, levels = c("Not_Ready", "Ready"))
  predicted <- factor(ifelse(prob >= threshold, "Not_Ready", "Ready"),
                      levels = c("Not_Ready", "Ready"))
  cm <- table(Predicted = predicted, Actual = actual)

  tp <- unname(cm["Not_Ready", "Not_Ready"])
  tn <- unname(cm["Ready", "Ready"])
  fp <- unname(cm["Not_Ready", "Ready"])
  fn <- unname(cm["Ready", "Not_Ready"])
  n <- sum(cm)

  recall <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
  specificity <- if ((tn + fp) == 0) 0 else tn / (tn + fp)
  precision <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
  f1 <- if ((precision + recall) == 0) 0 else
    2 * precision * recall / (precision + recall)
  accuracy <- if (n == 0) NA_real_ else (tp + tn) / n
  balanced_accuracy <- mean(c(recall, specificity))
  expected_agreement <- if (n == 0) NA_real_ else {
    sum(rowSums(cm) * colSums(cm)) / n^2
  }
  kappa <- if (is.na(expected_agreement) || expected_agreement == 1) NA_real_ else
    (accuracy - expected_agreement) / (1 - expected_agreement)
  mcc_denom <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  mcc <- if (mcc_denom == 0) 0 else (tp * tn - fp * fn) / mcc_denom

  tibble(
    Threshold = threshold, Accuracy = accuracy,
    Balanced_Accuracy = balanced_accuracy,
    Precision_NotReady = precision, Recall_NotReady = recall,
    Specificity_Ready = specificity, F1_NotReady = f1,
    Kappa = kappa, MCC = mcc,
    TP = tp, TN = tn, FP = fp, FN = fn
  )
}

selection_score <- function(ba, f1, recall, specificity) {
  selection_weights[["BA"]] * ba +
    selection_weights[["F1"]] * f1 +
    selection_weights[["Recall"]] * recall +
    selection_weights[["Specificity"]] * specificity
}

evaluate_thresholds <- function(actual, prob, model_name, model_group) {
  auc <- auc_value(actual, prob)
  pr_auc <- pr_auc_value(actual, prob)
  brier <- brier_score(actual, prob)

  sweep <- purrr::map_dfr(threshold_grid, function(thr) {
    confusion_metrics(actual, prob, thr)
  }) %>%
    mutate(
      Model = model_name, Model_Group = model_group,
      AUC = auc, PR_AUC = pr_auc, Brier_Score = brier,
      Selection_Score = selection_score(
        Balanced_Accuracy, F1_NotReady, Recall_NotReady, Specificity_Ready
      )
    )

  best <- sweep %>%
    filter(is.finite(Selection_Score)) %>%
    arrange(desc(Selection_Score), desc(Balanced_Accuracy),
            desc(F1_NotReady), desc(Recall_NotReady), desc(AUC)) %>%
    slice(1)
  list(sweep = sweep, best = best)
}

predict_notready <- function(model, newdata) {
  newdata <- as.data.frame(newdata)
  if (inherits(model, "train")) {
    needed <- model$xNames
    if (!is.null(needed)) {
      require_columns(newdata, needed, "Prediction data")
      newdata <- newdata[, needed, drop = FALSE]
    }
    probs <- predict(model, newdata = newdata, type = "prob")
    if (!"Not_Ready" %in% names(probs)) {
      stop("Caret model did not return a Not_Ready probability.")
    }
    return(as.numeric(probs$Not_Ready))
  }
  if (inherits(model, "glm") || inherits(model, "bayesglm")) {
    return(as.numeric(predict(model, newdata = newdata, type = "response")))
  }
  stop("Unsupported model class: ", paste(class(model), collapse = ", "))
}

impute_from_training <- function(train_x, other_x_list) {
  medians <- vapply(train_x, function(x) {
    m <- median(x, na.rm = TRUE)
    if (!is.finite(m)) 0 else m
  }, numeric(1))
  train_x <- as.data.frame(train_x)
  for (nm in names(train_x)) {
    train_x[[nm]][!is.finite(train_x[[nm]]) | is.na(train_x[[nm]])] <-
      medians[[nm]]
  }
  imputed_others <- lapply(other_x_list, function(dat) {
    dat <- as.data.frame(dat)
    for (nm in names(train_x)) {
      dat[[nm]][!is.finite(dat[[nm]]) | is.na(dat[[nm]])] <- medians[[nm]]
    }
    dat
  })
  c(list(train_x = train_x, medians = medians), imputed_others)
}

# ------------------------------------------------------------
# 2. LOAD AND VALIDATE INPUTS
# ------------------------------------------------------------

if (!file.exists(feature_workbook)) {
  stop("Feature workbook not found: ", feature_workbook, call. = FALSE)
}
if (!file.exists(label_workbook)) {
  stop("Label workbook not found: ", label_workbook,
       "\nRun the forecasting and readiness-labeling stages first.",
       call. = FALSE)
}

features <- readxl::read_excel(feature_workbook, sheet = feature_sheet)
labels <- readxl::read_excel(label_workbook, sheet = label_sheet)
names(features) <- clean_names(names(features))
names(labels) <- clean_names(names(labels))

keys <- c("global_simulation_id", "disease_seed", "disease_simulation_id")
require_columns(features, keys, "Extracted_Features")
require_columns(labels, c(keys, "best_mase"), "Readiness_Labels")

features <- features %>%
  mutate(
    global_simulation_id = as.integer(global_simulation_id),
    disease_seed = as.character(disease_seed),
    disease_simulation_id = as.integer(disease_simulation_id)
  )
labels <- labels %>%
  mutate(
    global_simulation_id = as.integer(global_simulation_id),
    disease_seed = as.character(disease_seed),
    disease_simulation_id = as.integer(disease_simulation_id),
    best_mase = safe_numeric(best_mase)
  )

if (anyDuplicated(features[keys])) {
  stop("Extracted_Features has duplicate simulation keys.", call. = FALSE)
}
if (anyDuplicated(labels[keys])) {
  stop("Readiness_Labels has duplicate simulation keys.", call. = FALSE)
}

# Keep the primary threshold consistent with the EpiFORESS conservative label.
labels <- labels %>%
  mutate(
    Readiness_Class = case_when(
      is.finite(best_mase) & best_mase < primary_mase_threshold ~ "Ready",
      is.finite(best_mase) ~ "Not_Ready",
      TRUE ~ NA_character_
    ),
    Readiness_Sensitivity = case_when(
      is.finite(best_mase) & best_mase < sensitivity_mase_threshold ~ "Ready",
      is.finite(best_mase) ~ "Not_Ready",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Readiness_Class))

label_fields <- labels %>%
  select(all_of(keys), best_mase, Readiness_Class, Readiness_Sensitivity)

model_data <- features %>%
  left_join(label_fields, by = keys, relationship = "one-to-one")

if (anyNA(model_data$Readiness_Class)) {
  warning(
    sum(is.na(model_data$Readiness_Class)),
    " feature rows did not receive a valid readiness label and will be excluded."
  )
}
model_data <- model_data %>%
  filter(!is.na(Readiness_Class)) %>%
  mutate(
    Readiness_Class = factor(Readiness_Class, levels = c("Not_Ready", "Ready")),
    Row_ID = row_number()
  )

predictors <- clean_names(predictors)
available_predictors <- intersect(predictors, names(model_data))
missing_predictors <- setdiff(predictors, available_predictors)
if (length(missing_predictors)) {
  warning("Predictors not found and skipped: ",
          paste(missing_predictors, collapse = ", "))
}
if (length(available_predictors) < 2) {
  stop("Fewer than two specified predictors are available.", call. = FALSE)
}

for (nm in available_predictors) {
  model_data[[nm]] <- safe_numeric(model_data[[nm]])
  model_data[[nm]][!is.finite(model_data[[nm]])] <- NA_real_
}

# Remove predictors that are entirely missing before split creation.
all_missing <- available_predictors[
  vapply(model_data[available_predictors], function(x) all(is.na(x)), logical(1))
]
available_predictors <- setdiff(available_predictors, all_missing)
if (length(available_predictors) < 2) {
  stop("Fewer than two non-empty predictors remain.", call. = FALSE)
}

# ------------------------------------------------------------
# 3. TRAIN / VALIDATION / TEST SPLIT
# ------------------------------------------------------------

if (abs(train_fraction + validation_fraction + test_fraction - 1) > 1e-8) {
  stop("Split fractions must sum to 1.", call. = FALSE)
}

set.seed(2026)
train_idx <- caret::createDataPartition(
  model_data$Readiness_Class, p = train_fraction, list = FALSE
)
train_data <- model_data[train_idx, , drop = FALSE]
remaining <- model_data[-train_idx, , drop = FALSE]

validation_share <- validation_fraction / (validation_fraction + test_fraction)
validation_idx <- caret::createDataPartition(
  remaining$Readiness_Class, p = validation_share, list = FALSE
)
validation_data <- remaining[validation_idx, , drop = FALSE]
test_data <- remaining[-validation_idx, , drop = FALSE]

x_train_raw <- train_data[, available_predictors, drop = FALSE]
x_validation_raw <- validation_data[, available_predictors, drop = FALSE]
x_test_raw <- test_data[, available_predictors, drop = FALSE]

imputed <- impute_from_training(
  x_train_raw,
  list(validation_x = x_validation_raw, test_x = x_test_raw)
)
x_train <- imputed$train_x
x_validation <- imputed$validation_x
x_test <- imputed$test_x
imputation_medians <- imputed$medians

nzv <- caret::nearZeroVar(x_train)
dropped_nzv <- if (length(nzv)) names(x_train)[nzv] else character()
if (length(nzv)) {
  x_train <- x_train[, -nzv, drop = FALSE]
  x_validation <- x_validation[, -nzv, drop = FALSE]
  x_test <- x_test[, -nzv, drop = FALSE]
}
predictor_names <- names(x_train)
if (length(predictor_names) < 2) {
  stop("Fewer than two predictors remain after near-zero variance filtering.")
}

y_train <- factor(train_data$Readiness_Class, levels = c("Not_Ready", "Ready"))
y_validation <- factor(validation_data$Readiness_Class, levels = c("Not_Ready", "Ready"))
y_test <- factor(test_data$Readiness_Class, levels = c("Not_Ready", "Ready"))

train_glm_data <- data.frame(
  y_notready = as.integer(y_train == "Not_Ready"),
  x_train,
  check.names = FALSE
)

# ------------------------------------------------------------
# 4. VIF DIAGNOSTIC AND LOW-VIF MODEL
# ------------------------------------------------------------

vif_model <- tryCatch(
  suppressWarnings(glm(y_notready ~ ., data = train_glm_data, family = binomial())),
  error = function(e) NULL
)
vif_table <- tibble(Predictor = character(), VIF = numeric(), Flag = character())

if (!is.null(vif_model)) {
  vif_raw <- tryCatch(car::vif(vif_model), error = function(e) NULL)
  if (!is.null(vif_raw)) {
    if (is.matrix(vif_raw)) {
      # For multi-df terms, use GVIF^(1/(2*Df)) as the comparable measure.
      vif_values <- vif_raw[, "GVIF"]^(1 / (2 * vif_raw[, "Df"]))
      names(vif_values) <- rownames(vif_raw)
    } else {
      vif_values <- vif_raw
    }
    vif_table <- tibble(
      Predictor = names(vif_values),
      VIF = as.numeric(vif_values),
      Flag = ifelse(vif_values > vif_threshold, "HIGH", "OK")
    ) %>% arrange(desc(VIF))
  }
}
low_vif_predictors <- if (nrow(vif_table)) {
  intersect(vif_table$Predictor[vif_table$VIF <= vif_threshold], predictor_names)
} else {
  predictor_names
}
if (length(low_vif_predictors) < 2) {
  warning("Low-VIF set has fewer than two predictors; using all retained predictors.")
  low_vif_predictors <- predictor_names
}

# ------------------------------------------------------------
# 5. MODEL TRAINING
# ------------------------------------------------------------

# caret::twoClassSummary treats the first factor level as the event class.
# Levels are deliberately ordered Not_Ready, Ready.
train_control <- caret::trainControl(
  method = "cv", number = 10,
  classProbs = TRUE,
  summaryFunction = caret::twoClassSummary,
  savePredictions = "final",
  allowParallel = TRUE
)

lambda_grid <- 10^seq(-5, 1, length.out = 40)
model_objects <- list()
model_groups <- c()

fit_caret <- function(name, x, y, method, group, ...) {
  message("Fitting ", name, "...")
  obj <- tryCatch(
    caret::train(
      x = x, y = y, method = method, trControl = train_control,
      metric = "ROC", ...
    ),
    error = function(e) {
      warning(name, " failed: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(obj)) {
    model_objects[[name]] <<- obj
    model_groups[name] <<- group
  }
}

fit_caret(
  "LowVIF_Logistic_Regression",
  x_train[, low_vif_predictors, drop = FALSE], y_train,
  "glm", "Statistical", family = binomial()
)
fit_caret(
  "LASSO_Logistic_Regression", x_train, y_train,
  "glmnet", "Statistical",
  tuneGrid = expand.grid(alpha = 1, lambda = lambda_grid)
)
fit_caret(
  "Elastic_Net_Logistic_Regression", x_train, y_train,
  "glmnet", "Statistical",
  tuneGrid = expand.grid(alpha = seq(0.1, 0.9, by = 0.2), lambda = lambda_grid)
)

step_aic <- tryCatch({
  full <- suppressWarnings(glm(y_notready ~ ., data = train_glm_data, family = binomial()))
  MASS::stepAIC(full, direction = "both", trace = FALSE, k = 2)
}, error = function(e) {
  warning("Stepwise AIC failed: ", conditionMessage(e)); NULL
})
if (!is.null(step_aic)) {
  model_objects$Stepwise_Logistic_AIC <- step_aic
  model_groups["Stepwise_Logistic_AIC"] <- "Statistical"
}

step_bic <- tryCatch({
  full <- suppressWarnings(glm(y_notready ~ ., data = train_glm_data, family = binomial()))
  MASS::stepAIC(full, direction = "both", trace = FALSE, k = log(nrow(train_glm_data)))
}, error = function(e) {
  warning("Stepwise BIC failed: ", conditionMessage(e)); NULL
})
if (!is.null(step_bic)) {
  model_objects$Stepwise_Logistic_BIC <- step_bic
  model_groups["Stepwise_Logistic_BIC"] <- "Statistical"
}

lowvif_train_data <- data.frame(
  y_notready = as.integer(y_train == "Not_Ready"),
  x_train[, low_vif_predictors, drop = FALSE],
  check.names = FALSE
)
lowvif_step_bic <- tryCatch({
  full <- suppressWarnings(glm(y_notready ~ ., data = lowvif_train_data, family = binomial()))
  MASS::stepAIC(full, direction = "both", trace = FALSE,
                k = log(nrow(lowvif_train_data)))
}, error = function(e) {
  warning("Low-VIF stepwise BIC failed: ", conditionMessage(e)); NULL
})
if (!is.null(lowvif_step_bic)) {
  model_objects$LowVIF_Stepwise_Logistic_BIC <- lowvif_step_bic
  model_groups["LowVIF_Stepwise_Logistic_BIC"] <- "Statistical"
}

bayes_logistic <- tryCatch(
  arm::bayesglm(y_notready ~ ., data = train_glm_data, family = binomial()),
  error = function(e) {
    warning("Weakly informative Bayesian logistic model failed: ",
            conditionMessage(e)); NULL
  }
)
if (!is.null(bayes_logistic)) {
  model_objects$Weakly_Informative_Bayesian_Logistic <- bayes_logistic
  model_groups["Weakly_Informative_Bayesian_Logistic"] <- "Statistical"
}

xgb_grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(2, 3),
  eta = c(0.03, 0.05, 0.10),
  gamma = c(0, 0.5),
  colsample_bytree = 0.75,
  min_child_weight = c(1, 5),
  subsample = 0.75
)
fit_caret(
  "XGBoost", x_train, y_train, "xgbTree", "Machine_Learning",
  tuneGrid = xgb_grid, verbose = FALSE
)

rf_grid <- expand.grid(
  mtry = unique(pmax(1, pmin(length(predictor_names),
                              c(3, 5, 7, floor(sqrt(length(predictor_names))))))),
  splitrule = "gini",
  min.node.size = c(5, 10, 20)
)
fit_caret(
  "Ranger_Random_Forest", x_train, y_train, "ranger", "Machine_Learning",
  tuneGrid = rf_grid, num.trees = 500, importance = "impurity"
)

if (!length(model_objects)) stop("No models fitted successfully.")

# ------------------------------------------------------------
# 6. OPTIONAL XGBOOST PLATT CALIBRATION FROM TRAINING CV PREDICTIONS
# ------------------------------------------------------------

xgb_calibration <- NULL
if ("XGBoost" %in% names(model_objects)) {
  cvp <- model_objects$XGBoost$pred
  best_tune <- model_objects$XGBoost$bestTune
  if (!is.null(cvp) && nrow(cvp)) {
    for (nm in intersect(names(best_tune), names(cvp))) {
      cvp <- cvp[cvp[[nm]] == best_tune[[nm]][1], , drop = FALSE]
    }
    if (all(c("rowIndex", "obs", "Not_Ready") %in% names(cvp))) {
      cvp <- cvp %>%
        group_by(rowIndex) %>%
        summarise(
          prob = mean(Not_Ready, na.rm = TRUE),
          obs = as.character(first(obs)),
          .groups = "drop"
        ) %>%
        filter(is.finite(prob), !is.na(obs))
      if (nrow(cvp) >= 10 && length(unique(cvp$obs)) == 2) {
        xgb_calibration <- tryCatch(
          glm(as.integer(obs == "Not_Ready") ~ prob,
              data = cvp, family = binomial()),
          error = function(e) NULL
        )
      }
    }
  }
}

calibrate_xgb <- function(prob) {
  if (is.null(xgb_calibration)) return(prob)
  p <- pmin(pmax(prob, 1e-6), 1 - 1e-6)
  as.numeric(predict(xgb_calibration, newdata = data.frame(prob = p),
                     type = "response"))
}

# ------------------------------------------------------------
# 7. PREDICT VALIDATION AND TEST SETS
# ------------------------------------------------------------

make_predictions <- function(split_name, split_data, x, y) {
  purrr::imap_dfr(model_objects, function(model, model_name) {
    if (inherits(model, "train")) {
      model_x <- x
      if (!is.null(model$xNames)) {
        require_columns(model_x, model$xNames, paste(model_name, "input"))
        model_x <- model_x[, model$xNames, drop = FALSE]
      }
      raw <- predict(model, newdata = model_x, type = "prob")$Not_Ready
      raw <- as.numeric(raw)
    } else {
      raw <- predict_notready(model, x)
    }
    prob <- if (model_name == "XGBoost") calibrate_xgb(raw) else raw
    prob <- pmin(pmax(prob, 0), 1)
    tibble(
      Dataset = split_name,
      Row_ID = split_data$Row_ID,
      Disease_Seed = as.character(split_data$disease_seed),
      Actual_Class = as.character(y),
      Model = model_name,
      Model_Group = unname(model_groups[[model_name]]),
      NotReady_Probability_Raw = raw,
      NotReady_Probability = prob,
      EpiFORESS_Score = 100 * (1 - prob)
    )
  })
}

validation_predictions <- make_predictions(
  "Validation", validation_data, x_validation, y_validation
)
test_predictions <- make_predictions("Test", test_data, x_test, y_test)

validation_sweeps <- validation_predictions %>%
  group_by(Model, Model_Group) %>%
  group_split() %>%
  map(function(df) evaluate_thresholds(
    df$Actual_Class, df$NotReady_Probability,
    unique(df$Model), unique(df$Model_Group)
  ))

validation_threshold_sweeps <- map_dfr(validation_sweeps, "sweep")
validation_performance <- map_dfr(validation_sweeps, "best") %>%
  arrange(desc(Selection_Score), desc(AUC), desc(Balanced_Accuracy))

if (!nrow(validation_performance)) {
  stop("No model produced a usable validation threshold.")
}

test_performance <- test_predictions %>%
  group_by(Model, Model_Group) %>%
  group_split() %>%
  map_dfr(function(df) {
    mn <- unique(df$Model)
    thr <- validation_performance %>% filter(Model == mn) %>% pull(Threshold)
    if (!length(thr) || !is.finite(thr[1])) thr <- 0.5
    bind_cols(
      tibble(Model = mn, Model_Group = unique(df$Model_Group)),
      confusion_metrics(df$Actual_Class, df$NotReady_Probability, thr[1]),
      tibble(
        AUC = auc_value(df$Actual_Class, df$NotReady_Probability),
        PR_AUC = pr_auc_value(df$Actual_Class, df$NotReady_Probability),
        Brier_Score = brier_score(df$Actual_Class, df$NotReady_Probability)
      )
    )
  })

test_auc_intervals <- test_predictions %>%
  group_by(Model, Model_Group) %>%
  group_split() %>%
  map_dfr(function(df) {
    ci <- auc_ci(df$Actual_Class, df$NotReady_Probability)
    tibble(Model = unique(df$Model), Model_Group = unique(df$Model_Group)) %>%
      bind_cols(ci)
  })

test_performance <- test_performance %>%
  left_join(test_auc_intervals, by = c("Model", "Model_Group"))

# ------------------------------------------------------------
# 8. SELECT COMPONENTS ON VALIDATION; FORM HYBRID
# ------------------------------------------------------------

best_statistical <- validation_performance %>%
  filter(Model_Group == "Statistical") %>%
  arrange(desc(Selection_Score), desc(AUC)) %>%
  slice(1)
best_ml <- validation_performance %>%
  filter(Model_Group == "Machine_Learning") %>%
  arrange(desc(Selection_Score), desc(AUC)) %>%
  slice(1)
best_overall <- validation_performance %>%
  arrange(desc(Selection_Score), desc(AUC)) %>%
  slice(1)

if (!nrow(best_statistical) || !nrow(best_ml)) {
  stop("At least one statistical and one ML model must fit to construct the hybrid.")
}
stat_name <- best_statistical$Model[1]
ml_name <- best_ml$Model[1]

validation_pair <- validation_predictions %>%
  filter(Model %in% c(stat_name, ml_name)) %>%
  select(Row_ID, Actual_Class, Model, NotReady_Probability) %>%
  tidyr::pivot_wider(names_from = Model, values_from = NotReady_Probability)

stat_brier <- brier_score(validation_pair$Actual_Class, validation_pair[[stat_name]])
ml_brier <- brier_score(validation_pair$Actual_Class, validation_pair[[ml_name]])

if (!is.finite(stat_brier) || !is.finite(ml_brier) ||
    stat_brier <= 0 || ml_brier <= 0) {
  weight_stat <- 0.5
  weight_ml <- 0.5
  weight_method <- "Equal weights: unusable validation Brier score"
} else {
  inv_stat <- 1 / stat_brier
  inv_ml <- 1 / ml_brier
  weight_stat <- inv_stat / (inv_stat + inv_ml)
  weight_ml <- inv_ml / (inv_stat + inv_ml)
  weight_method <- "Inverse validation Brier-score weighting"
}

validation_hybrid <- validation_pair %>%
  mutate(
    Dataset = "Validation",
    Hybrid_NotReady_Probability = weight_stat * .data[[stat_name]] +
      weight_ml * .data[[ml_name]],
    EpiFORESS_Score = 100 * (1 - Hybrid_NotReady_Probability)
  )

hybrid_validation_eval <- evaluate_thresholds(
  validation_hybrid$Actual_Class,
  validation_hybrid$Hybrid_NotReady_Probability,
  "Hybrid_EpiFORESS", "Hybrid"
)
hybrid_threshold <- hybrid_validation_eval$best$Threshold[1]

test_pair <- test_predictions %>%
  filter(Model %in% c(stat_name, ml_name)) %>%
  select(Row_ID, Actual_Class, Model, NotReady_Probability) %>%
  tidyr::pivot_wider(names_from = Model, values_from = NotReady_Probability)

test_hybrid <- test_pair %>%
  mutate(
    Dataset = "Test",
    Hybrid_NotReady_Probability = weight_stat * .data[[stat_name]] +
      weight_ml * .data[[ml_name]],
    EpiFORESS_Score = 100 * (1 - Hybrid_NotReady_Probability),
    Predicted_Class = if_else(
      Hybrid_NotReady_Probability >= hybrid_threshold, "Not_Ready", "Ready"
    ),
    EpiFORESS_Band = case_when(
      EpiFORESS_Score < 40 ~ "Very_Low",
      EpiFORESS_Score < 60 ~ "Low",
      EpiFORESS_Score < 80 ~ "Moderate",
      EpiFORESS_Score < 90 ~ "High",
      TRUE ~ "Very_High"
    )
  )

hybrid_test_metrics <- confusion_metrics(
  test_hybrid$Actual_Class, test_hybrid$Hybrid_NotReady_Probability,
  hybrid_threshold
) %>%
  mutate(
    Model = "Hybrid_EpiFORESS", Model_Group = "Hybrid",
    AUC = auc_value(test_hybrid$Actual_Class, test_hybrid$Hybrid_NotReady_Probability),
    PR_AUC = pr_auc_value(test_hybrid$Actual_Class, test_hybrid$Hybrid_NotReady_Probability),
    Brier_Score = brier_score(test_hybrid$Actual_Class, test_hybrid$Hybrid_NotReady_Probability)
  )

hybrid_ci <- auc_ci(test_hybrid$Actual_Class, test_hybrid$Hybrid_NotReady_Probability)
hybrid_test_metrics <- bind_cols(hybrid_test_metrics, hybrid_ci %>%
  select(AUC_CI_Lower_95, AUC_CI_Upper_95))

final_test_performance <- bind_rows(
  test_performance,
  hybrid_test_metrics
) %>%
  arrange(desc(Selection_Score), desc(AUC), desc(Balanced_Accuracy))

hybrid_weights <- tibble(
  Statistical_Model = stat_name,
  ML_Model = ml_name,
  Validation_Brier_Statistical = stat_brier,
  Validation_Brier_ML = ml_brier,
  Weight_Statistical = weight_stat,
  Weight_ML = weight_ml,
  Weight_Method = weight_method,
  Hybrid_Threshold = hybrid_threshold,
  Primary_Readiness_Rule = paste0("Ready if Best_MASE < ", primary_mase_threshold),
  Sensitivity_Readiness_Rule = paste0("Ready if Best_MASE < ", sensitivity_mase_threshold)
)

# ------------------------------------------------------------
# 9. EXPLORATORY LOGISTIC SIGNIFICANCE CHECKS
# ------------------------------------------------------------

# These p-values are exploratory. Stepwise selection and penalization make
# ordinary post-selection p-values optimistic and not confirmatory.
logistic_candidates <- list(
  LowVIF_Logistic_Regression = if ("LowVIF_Logistic_Regression" %in% names(model_objects))
    model_objects$LowVIF_Logistic_Regression$finalModel else NULL,
  Stepwise_Logistic_AIC = step_aic,
  Stepwise_Logistic_BIC = step_bic,
  LowVIF_Stepwise_Logistic_BIC = lowvif_step_bic,
  Weakly_Informative_Bayesian_Logistic = bayes_logistic
)
logistic_candidates <- logistic_candidates[!vapply(logistic_candidates, is.null, logical(1))]

overall_significance <- purrr::imap_dfr(logistic_candidates, function(mod, nm) {
  if (!inherits(mod, "glm")) return(tibble())
  null_mod <- tryCatch(update(mod, . ~ 1), error = function(e) NULL)
  if (is.null(null_mod)) return(tibble())
  ll_full <- as.numeric(logLik(mod))
  ll_null <- as.numeric(logLik(null_mod))
  df_lr <- attr(logLik(mod), "df") - attr(logLik(null_mod), "df")
  lr <- 2 * (ll_full - ll_null)
  tibble(
    Model = nm, N = nobs(mod), LR_ChiSquare = lr, LR_DF = df_lr,
    LR_P_Value = pchisq(lr, df = df_lr, lower.tail = FALSE),
    AIC = AIC(mod), BIC = BIC(mod),
    McFadden_R2 = 1 - ll_full / ll_null
  )
})

predictor_significance <- purrr::imap_dfr(logistic_candidates, function(mod, nm) {
  if (!inherits(mod, "glm")) return(tibble())
  co <- tryCatch(summary(mod)$coefficients, error = function(e) NULL)
  if (is.null(co) || !nrow(co)) return(tibble())
  tibble(
    Model = nm,
    Predictor = rownames(co),
    Coefficient = co[, "Estimate"],
    Std_Error = co[, "Std. Error"],
    Z_Value = co[, "z value"],
    P_Value = co[, "Pr(>|z|)"]
  ) %>%
    filter(Predictor != "(Intercept)") %>%
    mutate(
      Odds_Ratio = exp(Coefficient),
      OR_CI_Lower_95 = exp(Coefficient - 1.96 * Std_Error),
      OR_CI_Upper_95 = exp(Coefficient + 1.96 * Std_Error),
      Significant_0_05 = P_Value < 0.05
    ) %>%
    arrange(Model, P_Value)
})

drop1_tests <- purrr::imap_dfr(logistic_candidates, function(mod, nm) {
  if (!inherits(mod, "glm")) return(tibble())
  tryCatch(
    drop1(mod, test = "Chisq") %>%
      as.data.frame() %>%
      rownames_to_column("Predictor") %>%
      as_tibble() %>%
      filter(Predictor != "<none>") %>%
      mutate(Model = nm, .before = 1),
    error = function(e) tibble()
  )
})

# ------------------------------------------------------------
# 10. SUMMARIES, SAVED OBJECTS, AND OUTPUTS
# ------------------------------------------------------------

class_distribution <- model_data %>%
  count(Readiness_Class, name = "N") %>%
  mutate(Percent = round(100 * N / sum(N), 2))

split_distribution <- bind_rows(
  train_data %>% count(Readiness_Class, name = "N") %>% mutate(Split = "Train"),
  validation_data %>% count(Readiness_Class, name = "N") %>% mutate(Split = "Validation"),
  test_data %>% count(Readiness_Class, name = "N") %>% mutate(Split = "Test")
) %>%
  group_by(Split) %>%
  mutate(Percent = round(100 * N / sum(N), 2)) %>%
  ungroup() %>%
  select(Split, everything())

settings <- tibble(
  Setting = c(
    "Feature workbook", "Label workbook", "Output workbook",
    "Primary threshold", "Sensitivity threshold", "Training fraction",
    "Validation fraction", "Test fraction", "Threshold-selection score",
    "Hybrid weighting", "VIF threshold", "Bootstrap replicates",
    "Feature-window limitation"
  ),
  Value = c(
    feature_workbook, label_workbook, output_workbook,
    primary_mase_threshold, sensitivity_mase_threshold,
    train_fraction, validation_fraction, test_fraction,
    "0.35*Balanced Accuracy + 0.35*F1_NotReady + 0.20*Recall_NotReady + 0.10*Specificity_Ready",
    weight_method, vif_threshold, bootstrap_replicates,
    "Predictors must use only information available before the forecast horizon"
  )
)

data_summary <- tibble(
  Item = c(
    "Usable observations", "Training observations", "Validation observations",
    "Test observations", "Requested predictors", "Available predictors",
    "Entirely missing predictors", "Near-zero variance predictors dropped",
    "Retained predictors", "Low-VIF predictors"
  ),
  Value = c(
    nrow(model_data), nrow(train_data), nrow(validation_data), nrow(test_data),
    length(predictors), length(available_predictors), length(all_missing),
    length(dropped_nzv), length(predictor_names), length(low_vif_predictors)
  )
)

predictor_audit <- tibble(
  Predictor = predictors,
  Available = predictors %in% available_predictors,
  Entirely_Missing = predictors %in% all_missing,
  Dropped_NZV = predictors %in% dropped_nzv,
  Retained = predictors %in% predictor_names,
  Low_VIF_Set = predictors %in% low_vif_predictors
)

# Save models and preprocessing metadata. Deployment should use the exact
# saved preprocessing values and model-specific input columns.
saveRDS(model_objects, file.path(model_output_dir, "all_fitted_models.rds"))
saveRDS(
  list(
    model_objects = model_objects,
    xgb_calibration = xgb_calibration,
    predictor_names = predictor_names,
    low_vif_predictors = low_vif_predictors,
    imputation_medians = imputation_medians,
    primary_mase_threshold = primary_mase_threshold,
    sensitivity_mase_threshold = sensitivity_mase_threshold,
    stat_name = stat_name,
    ml_name = ml_name,
    weight_stat = weight_stat,
    weight_ml = weight_ml,
    hybrid_threshold = hybrid_threshold,
    weight_method = weight_method,
    validation_performance = validation_performance,
    final_test_performance = final_test_performance
  ),
  file.path(model_output_dir, "epiforess_model_bundle.rds")
)

# Feature importance tables
statistical_coefficients <- purrr::imap_dfr(model_objects, function(mod, nm) {
  if (inherits(mod, "train") && mod$method == "glmnet") {
    cf <- as.matrix(coef(mod$finalModel, s = mod$bestTune$lambda))
    tibble(Model = nm, Predictor = rownames(cf), Coefficient = as.numeric(cf[, 1])) %>%
      filter(Predictor != "(Intercept)", Coefficient != 0) %>%
      mutate(Odds_Ratio = exp(Coefficient))
  } else if (inherits(mod, "glm")) {
    cf <- coef(mod)
    tibble(Model = nm, Predictor = names(cf), Coefficient = as.numeric(cf)) %>%
      filter(Predictor != "(Intercept)") %>%
      mutate(Odds_Ratio = exp(Coefficient))
  } else {
    tibble()
  }
})

ml_importance <- tibble()
if ("XGBoost" %in% names(model_objects)) {
  ml_importance <- tryCatch(
    xgboost::xgb.importance(model = model_objects$XGBoost$finalModel) %>%
      as_tibble(),
    error = function(e) tibble()
  )
}
if ("Ranger_Random_Forest" %in% names(model_objects)) {
  rf_imp <- tryCatch({
    vi <- caret::varImp(model_objects$Ranger_Random_Forest)$importance
    tibble(Predictor = rownames(vi), Importance = vi$Overall)
  }, error = function(e) tibble())
  if (nrow(rf_imp)) {
    rf_imp$Model <- "Ranger_Random_Forest"
    ml_importance <- bind_rows(ml_importance, rf_imp)
  }
}

output_tables <- list(
  Modeling_Settings = settings,
  Modeling_Data_Summary = data_summary,
  Predictor_Audit = predictor_audit,
  Class_Distribution = class_distribution,
  Split_Distribution = split_distribution,
  VIF_Diagnostic = vif_table,
  Validation_Performance = validation_performance,
  Validation_Threshold_Sweeps = validation_threshold_sweeps,
  Final_Test_Performance = final_test_performance,
  Test_AUC_Intervals = test_auc_intervals,
  Hybrid_Weights = hybrid_weights,
  Validation_Predictions = validation_predictions,
  Test_Predictions = test_predictions,
  Validation_Hybrid = validation_hybrid,
  Test_Hybrid = test_hybrid,
  Logistic_Overall_Tests = overall_significance,
  Logistic_Predictor_Tests = predictor_significance,
  Logistic_Drop1_Tests = drop1_tests,
  Statistical_Coefficients = statistical_coefficients,
  ML_Feature_Importance = ml_importance
)

wb <- if (file.exists(output_workbook)) {
  openxlsx::loadWorkbook(output_workbook)
} else {
  openxlsx::createWorkbook()
}
for (sheet_name in names(output_tables)) {
  if (sheet_name %in% openxlsx::sheets(wb)) {
    openxlsx::removeWorksheet(wb, sheet_name)
  }
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeDataTable(
    wb, sheet = sheet_name, x = output_tables[[sheet_name]],
    withFilter = TRUE
  )
  openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
}
openxlsx::saveWorkbook(wb, output_workbook, overwrite = TRUE)

# ------------------------------------------------------------
# 11. CONSOLE SUMMARY
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("EPIFORESS FORECAST-READINESS MODELING COMPLETE\n")
cat("============================================================\n")
cat("Feature workbook: ", feature_workbook, "\n")
cat("Label workbook:   ", label_workbook, "\n")
cat("Output workbook:  ", output_workbook, "\n")
cat("Primary rule: Ready if Best_MASE <", primary_mase_threshold, "\n")
cat("Sensitivity rule: Ready if Best_MASE <", sensitivity_mase_threshold, "\n")
cat("Training / validation / test:", nrow(train_data), "/",
    nrow(validation_data), "/", nrow(test_data), "\n")
cat("Selected statistical model:", stat_name, "\n")
cat("Selected ML model:", ml_name, "\n")
cat("Hybrid weights:", round(weight_stat, 4), "and", round(weight_ml, 4), "\n")
cat("Hybrid threshold:", round(hybrid_threshold, 4), "\n\n")
cat("Validation performance:\n")
print(validation_performance)
cat("\nFinal test performance:\n")
print(final_test_performance)
cat("\nModel files saved in:", model_output_dir, "\n")
cat("============================================================\n")
