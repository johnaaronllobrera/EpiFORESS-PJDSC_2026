# EpiFORESS Analysis Pipeline
# Script: 04_forecast_readiness_labeling.R
# Project root: set EPIFORESS_PROJECT_DIR or run from the project data directory.
# Reproducibility: inspect all settings and verify input/model compatibility before reporting results.

# ============================================================
# EPIFORESS: FORECAST-READINESS LABELING
# Purpose: Derive and audit readiness labels from forecast metrics,
#          then merge them with the feature-extraction modeling data.
# Author: John Aaron B. Llobrera
# ============================================================

# IMPORTANT:
# - Run after 01_empirical_characterization_simulation.R,
#   02_feature_extraction.R, and 03_forecasting_experiment.R.
# - The primary readiness rule is Best_MASE < 1.00, consistent with
#   the primary EpiFORESS labeling criterion.
# - A sensitivity label using Best_MASE < 0.50 is also generated.
# - Failed or invalid evaluations are not labeled Ready or Not_Ready.
# - Source files are read-only; outputs are written separately.

# ------------------------------------------------------------
# 0. USER SETTINGS
# ------------------------------------------------------------

project_dir <- normalizePath(
  Sys.getenv("EPIFORESS_PROJECT_DIR", unset = getwd()),
  winslash = "/",
  mustWork = FALSE
)

if (!dir.exists(project_dir)) {
  stop("Project directory does not exist: ", project_dir, call. = FALSE)
}

forecast_workbook <- file.path(
  project_dir,
  "EpiFORESS_Forecasting_Results.xlsx"
)

feature_workbook <- file.path(
  project_dir,
  "EpiFORESS_feature_extraction_outputs.xlsx"
)

output_workbook <- file.path(
  project_dir,
  "EpiFORESS_Forecast_Results.xlsx"
)

forecast_metrics_sheet <- "Forecast_Metrics"
modeling_dataset_sheet <- "Modeling_Dataset"

primary_mase_threshold <- 1.00
sensitivity_mase_threshold <- 0.50

# ------------------------------------------------------------
# 1. PACKAGES
# ------------------------------------------------------------

packages <- c("readxl", "dplyr", "openxlsx", "tibble")
missing_packages <- setdiff(packages, rownames(installed.packages()))

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(openxlsx)
  library(tibble)
})

# ------------------------------------------------------------
# 2. FILE, SHEET, AND KEY VALIDATION
# ------------------------------------------------------------

if (!file.exists(forecast_workbook)) {
  stop(
    "Forecast workbook not found: ", forecast_workbook,
    "\nRun 03_forecasting_experiment.R first.",
    call. = FALSE
  )
}

if (!file.exists(feature_workbook)) {
  stop(
    "Feature-extraction workbook not found: ", feature_workbook,
    "\nRun 02_feature_extraction.R first.",
    call. = FALSE
  )
}

forecast_sheets <- openxlsx::getSheetNames(forecast_workbook)
feature_sheets <- openxlsx::getSheetNames(feature_workbook)

if (!forecast_metrics_sheet %in% forecast_sheets) {
  stop(
    "Sheet '", forecast_metrics_sheet,
    "' not found in ", forecast_workbook,
    call. = FALSE
  )
}

if (!modeling_dataset_sheet %in% feature_sheets) {
  stop(
    "Sheet '", modeling_dataset_sheet,
    "' not found in ", feature_workbook,
    call. = FALSE
  )
}

join_keys <- c(
  "Global_Simulation_ID",
  "Disease_Seed",
  "Disease_Simulation_ID"
)

# ------------------------------------------------------------
# 3. LOAD FORECAST METRICS
# ------------------------------------------------------------

forecast_metrics <- readxl::read_excel(
  forecast_workbook,
  sheet = forecast_metrics_sheet
)

required_forecast_cols <- c(
  join_keys,
  "Best_Model",
  "Best_MASE",
  "Best_MAE",
  "Best_RMSE",
  "Evaluation_Status"
)

missing_forecast_cols <- setdiff(
  required_forecast_cols,
  names(forecast_metrics)
)

if (length(missing_forecast_cols) > 0) {
  stop(
    "Missing required columns in Forecast_Metrics: ",
    paste(missing_forecast_cols, collapse = ", "),
    call. = FALSE
  )
}

forecast_metrics <- forecast_metrics %>%
  dplyr::mutate(
    Global_Simulation_ID = as.integer(Global_Simulation_ID),
    Disease_Seed = as.character(Disease_Seed),
    Disease_Simulation_ID = as.integer(Disease_Simulation_ID),
    Best_Model = as.character(Best_Model),
    Best_MASE = as.numeric(Best_MASE),
    Best_MAE = as.numeric(Best_MAE),
    Best_RMSE = as.numeric(Best_RMSE),
    Evaluation_Status = as.character(Evaluation_Status)
  )

if (anyNA(forecast_metrics[, join_keys])) {
  stop("Forecast_Metrics contains missing values in join keys.", call. = FALSE)
}

duplicate_forecast_keys <- forecast_metrics %>%
  dplyr::count(dplyr::across(dplyr::all_of(join_keys)), name = "n") %>%
  dplyr::filter(n > 1)

if (nrow(duplicate_forecast_keys) > 0) {
  stop(
    "Forecast_Metrics contains duplicate series keys. Resolve duplicates before labeling.",
    call. = FALSE
  )
}

# Preserve the upstream labels for audit when present.
if (!"Ready_Binary" %in% names(forecast_metrics)) {
  forecast_metrics$Ready_Binary <- NA_integer_
}
if (!"Ready_Label" %in% names(forecast_metrics)) {
  forecast_metrics$Ready_Label <- NA_character_
}
if (!"N_Origins" %in% names(forecast_metrics)) {
  forecast_metrics$N_Origins <- NA_integer_
}
if (!"Error_Message" %in% names(forecast_metrics)) {
  forecast_metrics$Error_Message <- NA_character_
}

forecast_metrics <- forecast_metrics %>%
  dplyr::mutate(
    Original_Ready_Binary = suppressWarnings(as.integer(Ready_Binary)),
    Original_Ready_Label = as.character(Ready_Label),
    N_Origins = suppressWarnings(as.integer(N_Origins)),
    Error_Message = as.character(Error_Message)
  )

# ------------------------------------------------------------
# 4. CREATE PRIMARY AND SENSITIVITY LABELS
# ------------------------------------------------------------

# Keep all evaluation records in an audit table. Assign labels only
# to successfully evaluated series with finite Best_MASE.
readiness_audit <- forecast_metrics %>%
  dplyr::mutate(
    Valid_Evaluation = Evaluation_Status == "Evaluated" &
      is.finite(Best_MASE) & !is.na(Best_MASE),
    Forecast_Ready = dplyr::case_when(
      Valid_Evaluation & Best_MASE < primary_mase_threshold ~ 1L,
      Valid_Evaluation ~ 0L,
      TRUE ~ NA_integer_
    ),
    Readiness = factor(
      dplyr::case_when(
        Forecast_Ready == 1L ~ "Ready",
        Forecast_Ready == 0L ~ "Not_Ready",
        TRUE ~ NA_character_
      ),
      levels = c("Not_Ready", "Ready")
    ),
    Forecast_Ready_Sensitivity = dplyr::case_when(
      Valid_Evaluation & Best_MASE < sensitivity_mase_threshold ~ 1L,
      Valid_Evaluation ~ 0L,
      TRUE ~ NA_integer_
    ),
    Readiness_Sensitivity = factor(
      dplyr::case_when(
        Forecast_Ready_Sensitivity == 1L ~ "Ready",
        Forecast_Ready_Sensitivity == 0L ~ "Not_Ready",
        TRUE ~ NA_character_
      ),
      levels = c("Not_Ready", "Ready")
    ),
    Label_Check = dplyr::case_when(
      !Valid_Evaluation ~ "Not evaluated",
      is.na(Original_Ready_Binary) ~ "No original binary label",
      Original_Ready_Binary == Forecast_Ready ~ "Matches primary threshold",
      TRUE ~ "Mismatch"
    ),
    Label_Check_Sensitivity = dplyr::case_when(
      !Valid_Evaluation ~ "Not evaluated",
      is.na(Original_Ready_Binary) ~ "No original binary label",
      Original_Ready_Binary == Forecast_Ready_Sensitivity ~
        "Matches sensitivity threshold",
      TRUE ~ "Differs from sensitivity threshold"
    )
  )

readiness_labels <- readiness_audit %>%
  dplyr::filter(Valid_Evaluation) %>%
  dplyr::select(
    dplyr::all_of(join_keys),
    Best_Model,
    Best_MASE,
    Best_MAE,
    Best_RMSE,
    Forecast_Ready,
    Readiness,
    Forecast_Ready_Sensitivity,
    Readiness_Sensitivity,
    Original_Ready_Binary,
    Original_Ready_Label,
    Label_Check,
    Label_Check_Sensitivity,
    N_Origins,
    Evaluation_Status,
    Error_Message
  )

label_check_summary <- readiness_audit %>%
  dplyr::count(Label_Check, name = "Frequency") %>%
  dplyr::mutate(
    Percent = if (sum(Frequency) > 0) {
      round(100 * Frequency / sum(Frequency), 2)
    } else {
      NA_real_
    }
  )

label_check_sensitivity_summary <- readiness_audit %>%
  dplyr::count(Label_Check_Sensitivity, name = "Frequency") %>%
  dplyr::mutate(
    Percent = if (sum(Frequency) > 0) {
      round(100 * Frequency / sum(Frequency), 2)
    } else {
      NA_real_
    }
  )

if (any(readiness_audit$Label_Check == "Mismatch", na.rm = TRUE)) {
  warning(
    "Primary labels disagree with labels in Forecast_Metrics. ",
    "Inspect Label_Check_Summary and confirm the upstream threshold."
  )
}

# ------------------------------------------------------------
# 5. SUMMARY TABLES
# ------------------------------------------------------------

make_class_summary <- function(data, label_col, threshold) {
  data %>%
    dplyr::filter(!is.na(.data[[label_col]])) %>%
    dplyr::count(.data[[label_col]], name = "Frequency") %>%
    dplyr::rename(Readiness = dplyr::all_of(label_col)) %>%
    dplyr::mutate(
      Percent = round(100 * Frequency / sum(Frequency), 2),
      Threshold_Rule = paste0("Ready if Best_MASE < ", threshold)
    )
}

readiness_summary <- make_class_summary(
  readiness_labels, "Readiness", primary_mase_threshold
)

readiness_sensitivity_summary <- make_class_summary(
  readiness_labels, "Readiness_Sensitivity", sensitivity_mase_threshold
)

class_balance <- readiness_labels %>%
  dplyr::count(Readiness, name = "N") %>%
  dplyr::mutate(
    Proportion = round(N / sum(N), 4),
    Percent = round(100 * N / sum(N), 2)
  )

model_summary <- readiness_labels %>%
  dplyr::count(Best_Model, Readiness, name = "Frequency") %>%
  dplyr::group_by(Best_Model) %>%
  dplyr::mutate(
    Percent_Within_Model = round(100 * Frequency / sum(Frequency), 2)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Best_Model, Readiness)

disease_summary <- readiness_labels %>%
  dplyr::count(Disease_Seed, Readiness, name = "Frequency") %>%
  dplyr::group_by(Disease_Seed) %>%
  dplyr::mutate(
    Percent_Within_Disease = round(100 * Frequency / sum(Frequency), 2)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Disease_Seed, Readiness)

mase_summary <- readiness_labels %>%
  dplyr::group_by(Readiness) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean_Best_MASE = mean(Best_MASE, na.rm = TRUE),
    median_Best_MASE = median(Best_MASE, na.rm = TRUE),
    sd_Best_MASE = stats::sd(Best_MASE, na.rm = TRUE),
    min_Best_MASE = min(Best_MASE, na.rm = TRUE),
    max_Best_MASE = max(Best_MASE, na.rm = TRUE),
    mean_Best_MAE = mean(Best_MAE, na.rm = TRUE),
    median_Best_MAE = median(Best_MAE, na.rm = TRUE),
    mean_Best_RMSE = mean(Best_RMSE, na.rm = TRUE),
    median_Best_RMSE = median(Best_RMSE, na.rm = TRUE),
    .groups = "drop"
  )

model_performance_summary <- readiness_labels %>%
  dplyr::group_by(Best_Model) %>%
  dplyr::summarise(
    N_Best = dplyr::n(),
    Ready_Count = sum(Forecast_Ready == 1L),
    NotReady_Count = sum(Forecast_Ready == 0L),
    Ready_Percent = round(100 * mean(Forecast_Ready == 1L), 2),
    Mean_Best_MASE = mean(Best_MASE, na.rm = TRUE),
    Median_Best_MASE = median(Best_MASE, na.rm = TRUE),
    Mean_Best_MAE = mean(Best_MAE, na.rm = TRUE),
    Median_Best_MAE = median(Best_MAE, na.rm = TRUE),
    Mean_Best_RMSE = mean(Best_RMSE, na.rm = TRUE),
    Median_Best_RMSE = median(Best_RMSE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(N_Best))

disease_performance_summary <- readiness_labels %>%
  dplyr::group_by(Disease_Seed) %>%
  dplyr::summarise(
    N_Series = dplyr::n(),
    Ready_Count = sum(Forecast_Ready == 1L),
    NotReady_Count = sum(Forecast_Ready == 0L),
    Ready_Percent = round(100 * mean(Forecast_Ready == 1L), 2),
    Mean_Best_MASE = mean(Best_MASE, na.rm = TRUE),
    Median_Best_MASE = median(Best_MASE, na.rm = TRUE),
    SD_Best_MASE = stats::sd(Best_MASE, na.rm = TRUE),
    Most_Common_Best_Model = {
      counts <- table(Best_Model)
      if (length(counts) == 0) NA_character_ else names(counts)[which.max(counts)]
    },
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(Ready_Percent))

evaluation_status_summary <- readiness_audit %>%
  dplyr::count(Evaluation_Status, name = "Frequency") %>%
  dplyr::mutate(
    Percent = round(100 * Frequency / sum(Frequency), 2)
  ) %>%
  dplyr::arrange(dplyr::desc(Frequency))

readiness_metadata <- tibble::tibble(
  Field = c(
    "Forecast_Workbook",
    "Feature_Workbook",
    "Output_Workbook",
    "Forecast_Metrics_Sheet",
    "Modeling_Dataset_Sheet",
    "Primary_Readiness_Threshold",
    "Primary_Threshold_Rule",
    "Sensitivity_Readiness_Threshold",
    "Sensitivity_Threshold_Rule",
    "Forecast_Records",
    "Successfully_Labeled_Series",
    "Created_By"
  ),
  Value = c(
    forecast_workbook,
    feature_workbook,
    output_workbook,
    forecast_metrics_sheet,
    modeling_dataset_sheet,
    as.character(primary_mase_threshold),
    paste0("Ready if Best_MASE < ", primary_mase_threshold),
    as.character(sensitivity_mase_threshold),
    paste0("Ready if Best_MASE < ", sensitivity_mase_threshold),
    as.character(nrow(readiness_audit)),
    as.character(nrow(readiness_labels)),
    "EpiFORESS forecast-readiness labeling script"
  )
)

# ------------------------------------------------------------
# 6. MERGE LABELS WITH FEATURE-EXTRACTION DATA
# ------------------------------------------------------------

modeling_dataset <- readxl::read_excel(
  feature_workbook,
  sheet = modeling_dataset_sheet
) %>%
  dplyr::mutate(
    Global_Simulation_ID = as.integer(Global_Simulation_ID),
    Disease_Seed = as.character(Disease_Seed),
    Disease_Simulation_ID = as.integer(Disease_Simulation_ID)
  )

missing_model_keys <- setdiff(join_keys, names(modeling_dataset))
if (length(missing_model_keys) > 0) {
  stop(
    "Missing join keys in Modeling_Dataset: ",
    paste(missing_model_keys, collapse = ", "),
    call. = FALSE
  )
}

if (anyNA(modeling_dataset[, join_keys])) {
  stop("Modeling_Dataset contains missing values in join keys.", call. = FALSE)
}

duplicate_modeling_keys <- modeling_dataset %>%
  dplyr::count(dplyr::across(dplyr::all_of(join_keys)), name = "n") %>%
  dplyr::filter(n > 1)

if (nrow(duplicate_modeling_keys) > 0) {
  stop(
    "Modeling_Dataset contains duplicate series keys. Resolve duplicates before joining.",
    call. = FALSE
  )
}

label_keys <- readiness_labels %>%
  dplyr::select(dplyr::all_of(join_keys))

duplicate_label_keys <- label_keys %>%
  dplyr::count(dplyr::across(dplyr::all_of(join_keys)), name = "n") %>%
  dplyr::filter(n > 1)

if (nrow(duplicate_label_keys) > 0) {
  stop(
    "Readiness labels contain duplicate series keys. Resolve duplicates before joining.",
    call. = FALSE
  )
}

modeling_dataset_labeled <- modeling_dataset %>%
  dplyr::left_join(
    readiness_labels %>%
      dplyr::select(
        dplyr::all_of(join_keys),
        Best_Model,
        Best_MASE,
        Best_MAE,
        Best_RMSE,
        Forecast_Ready,
        Readiness,
        Forecast_Ready_Sensitivity,
        Readiness_Sensitivity,
        N_Origins,
        Evaluation_Status
      ),
    by = join_keys
  )

n_unlabeled <- sum(is.na(modeling_dataset_labeled$Forecast_Ready))

join_summary <- tibble::tibble(
  Metric = c(
    "Modeling_Dataset_Rows",
    "Forecast_Records",
    "Successfully_Labeled_Series",
    "Labeled_Modeling_Rows",
    "Modeling_Rows_Without_Valid_Label",
    "Duplicate_Modeling_Keys",
    "Duplicate_Label_Keys"
  ),
  Value = c(
    nrow(modeling_dataset),
    nrow(forecast_metrics),
    nrow(readiness_labels),
    sum(!is.na(modeling_dataset_labeled$Forecast_Ready)),
    n_unlabeled,
    nrow(duplicate_modeling_keys),
    nrow(duplicate_label_keys)
  )
)

# ------------------------------------------------------------
# 7. SAVE WORKBOOK OUTPUTS
# ------------------------------------------------------------

if (file.exists(output_workbook)) {
  wb <- openxlsx::loadWorkbook(output_workbook)
} else {
  wb <- openxlsx::createWorkbook()
}

sheet_data <- list(
  Readiness_Labels = readiness_labels,
  Readiness_Audit_All = readiness_audit,
  Readiness_Summary = readiness_summary,
  Readiness_Sensitivity = readiness_sensitivity_summary,
  Readiness_By_Model = model_summary,
  Readiness_By_Disease = disease_summary,
  MASE_By_Readiness = mase_summary,
  Class_Balance = class_balance,
  Model_Performance_Summary = model_performance_summary,
  Disease_Performance_Summary = disease_performance_summary,
  Label_Check_Summary = label_check_summary,
  Label_Check_Sensitivity = label_check_sensitivity_summary,
  Evaluation_Status = evaluation_status_summary,
  Join_Summary = join_summary,
  Readiness_Metadata = readiness_metadata,
  Modeling_Dataset_Labeled = modeling_dataset_labeled
)

for (sheet_name in names(sheet_data)) {
  if (sheet_name %in% openxlsx::sheets(wb)) {
    openxlsx::removeWorksheet(wb, sheet_name)
  }
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeDataTable(
    wb,
    sheet = sheet_name,
    x = sheet_data[[sheet_name]],
    withFilter = TRUE
  )
  openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
}

openxlsx::saveWorkbook(wb, output_workbook, overwrite = TRUE)

# ------------------------------------------------------------
# 8. SAVE CSV OUTPUTS
# ------------------------------------------------------------

output_dir <- dirname(output_workbook)

utils::write.csv(
  readiness_labels,
  file.path(output_dir, "readiness_labels.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  readiness_audit,
  file.path(output_dir, "readiness_audit_all.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  class_balance,
  file.path(output_dir, "class_balance.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  modeling_dataset_labeled,
  file.path(output_dir, "modeling_dataset_labeled.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------
# 9. CONSOLE SUMMARY
# ------------------------------------------------------------

cat("\n================================================\n")
cat("EPIFORESS FORECAST-READINESS LABELING COMPLETE\n")
cat("================================================\n")
cat("Forecast workbook:", forecast_workbook, "\n")
cat("Feature workbook:", feature_workbook, "\n")
cat("Output workbook:", output_workbook, "\n\n")
cat("Primary rule: Ready if Best_MASE <", primary_mase_threshold, "\n")
cat("Sensitivity rule: Ready if Best_MASE <", sensitivity_mase_threshold, "\n")
cat("Forecast records:", nrow(readiness_audit), "\n")
cat("Successfully labeled series:", nrow(readiness_labels), "\n")
cat("Modeling rows without a valid label:", n_unlabeled, "\n\n")

cat("Primary readiness summary:\n")
print(readiness_summary)

cat("\nSensitivity readiness summary:\n")
print(readiness_sensitivity_summary)

cat("\nClass balance:\n")
print(class_balance)

cat("\nPrimary label audit:\n")
print(label_check_summary)

cat("\nEvaluation status:\n")
print(evaluation_status_summary)

cat("\nJoin summary:\n")
print(join_summary)

cat("\n================================================\n")
