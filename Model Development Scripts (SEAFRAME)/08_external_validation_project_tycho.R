# EpiFORESS Analysis Pipeline
# Script: 08_external_validation_project_tycho.R
# Project root: set EPIFORESS_PROJECT_DIR or run from the project data directory.
# Reproducibility: inspect all settings and verify input/model compatibility before reporting results.

# ============================================================
# EPIFORESS EXTERNAL EMPIRICAL VALIDATION:
# PROJECT TYCHO LEVEL 1, LOCATION-STRATIFIED DESIGN
# FINAL FIXED VERSION: TESTSERVER CAPTURE + SAFE FALLBACK
# ============================================================
#
# Purpose
# -------
# Externally validate EpiFORESS using Project Tycho Level 1
# weekly notifiable-disease surveillance data while preserving
# the geographic stratification present in the source data.
#
# Validation unit
# ---------------
# Each distinct Disease x Location x Location_Type combination
# is treated as a separate surveillance series. Counts are not
# pooled across cities, states, or other geographic units.
#
# Core design
# -----------
# 1. Read the Project Tycho long-format CSV:
#      Year, Week, Location, loc_type, Disease, Cases.
#
# 2. Collapse only true duplicate rows within the same disease,
#    location, location type, year, and week.
#
# 3. Complete the weekly calendar separately for every
#    Disease x Location x Location_Type series. Absent source
#    rows remain missing and are never converted to zero.
#
# 4. Construct 260-week windows within each location-stratified
#    series.
#
# 5. Primary external validation:
#      latest eligible 260-week window per location-stratified
#      disease series.
#
# 6. Extended temporal validation:
#      all eligible recent-aligned, non-overlapping 260-week
#      windows per location-stratified disease series.
#
# 7. Evaluate the same five forecasting candidates:
#      Seasonal Naive, STL-ETS, nonseasonal ARIMA, SARIMA,
#      and Negative Binomial regression with Fourier terms.
#
# 8. Define observed forecast-readiness solely as:
#      Best MASE < 1.00.
#    This outcome criterion does not impose a binary cutoff on
#    the continuous EpiFORESS score itself.
#
# 9. Treat EpiFORESS principally as a continuous gradient:
#      - Pearson and Spearman association
#      - cluster-bootstrap correlation intervals
#      - robust log-linear regression
#      - quantile regression
#      - ROC-AUC for Best MASE < 1.00
#      - descriptive readiness-band summaries
#      - Jonckheere-Terpstra ordered trend test
#      - score-calibrated median Best MASE validation
#
# Dependence adjustment
# ---------------------
# Primary analysis:
#   two-way cluster-robust covariance by Disease and geographic
#   location stratum, because several observations may share a
#   disease or a location.
#
# Extended analysis:
#   cluster-robust covariance by Series_ID, because repeated
#   non-overlapping windows come from the same location-specific
#   disease series.
#
# EpiFORESS:
#   100 * [1 - P(Not Ready)]
#
# Descriptive readiness bands:
#   0-19   Very Low
#   20-34  Low
#   35-59  Moderate
#   60-79  High
#   80-100 Very High
# ============================================================


# ------------------------------------------------------------
# 0. PACKAGES
# ------------------------------------------------------------

required_packages <- c(
  "data.table", "dplyr", "tidyr", "purrr", "stringr",
  "tibble", "forecast", "MASS", "openxlsx", "ggplot2",
  "pROC", "broom", "lmtest", "sandwich", "quantreg",
  "clinfun", "shiny"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))

if (length(missing_packages) > 0L) {
  install.packages(missing_packages, dependencies = TRUE)
}

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(forecast)
  library(MASS)
  library(openxlsx)
  library(ggplot2)
  library(pROC)
  library(broom)
  library(lmtest)
  library(sandwich)
  library(quantreg)
  library(clinfun)
  library(shiny)
})

set.seed(2027)


# ------------------------------------------------------------
# 1. PATHS AND USER SETTINGS
# ------------------------------------------------------------

base_path <- normalizePath(
  Sys.getenv("EPIFORESS_PROJECT_DIR", unset = getwd()),
  winslash = "/",
  mustWork = FALSE
)

if (!dir.exists(base_path)) {
  stop("Project directory does not exist: ", base_path, call. = FALSE)
}

tycho_csv_path <- file.path(
  base_path,
  "ProjectTycho_Level1_v.1.0.0",
  "ProjectTycho_Level1_v1.0.0.csv"
)

app_candidates <- c(
  file.path(base_path, "app_full_fixed.R"),
  file.path(base_path, "EpiFORESS_app_gradient_revised_bands_full_score_FINAL_FIXED.R")
)

app_file <- app_candidates[file.exists(app_candidates)][1]

model_dir <- file.path(base_path, "Saved_Models")
stat_model_path <- file.path(model_dir, "best_statistical_forecast_readiness_model.rds")
ml_model_path <- file.path(model_dir, "best_machine_learning_model.rds")

output_dir <- file.path(
  base_path,
  "ProjectTycho_EpiFORESS_Location_Stratified_Validation"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

validation_workbook_path <- file.path(
  output_dir,
  "ProjectTycho_EpiFORESS_Location_Stratified_External_Validation.xlsx"
)

validation_rds_path <- file.path(
  output_dir,
  "ProjectTycho_EpiFORESS_Location_Stratified_External_Validation.rds"
)

primary_scatter_path <- file.path(
  output_dir,
  "Primary_Location_Stratified_EpiFORESS_vs_Best_MASE.png"
)

extended_scatter_path <- file.path(
  output_dir,
  "Extended_Location_Stratified_EpiFORESS_vs_Best_MASE.png"
)

band_plot_path <- file.path(
  output_dir,
  "Best_MASE_by_EpiFORESS_Band_Location_Stratified.png"
)

calibration_plot_path <- file.path(
  output_dir,
  "Observed_vs_Score_Calibrated_Best_MASE_Location_Stratified.png"
)

coverage_plot_path <- file.path(
  output_dir,
  "ProjectTycho_Location_Strata_Eligibility.png"
)


# ------------------------------------------------------------
# 1A. OPTIONAL SOURCE FILTERS
# ------------------------------------------------------------

# NULL means retain all values found in the source data.
# Matching is case-insensitive after whitespace normalization.
# Examples:
#   disease_filter <- c("MEASLES", "MUMPS")
#   location_filter <- c("ALABAMA", "ATLANTA", "CALIFORNIA")
#   location_type_filter <- c("STATE", "CITY")

disease_filter <- NULL
location_filter <- NULL
location_type_filter <- NULL

# When TRUE, series with an empty or unrecognized location label
# are removed. The supplied Project Tycho location list is not
# hard-coded; all valid source locations are retained by default.
require_nonempty_location <- TRUE

# Location stratification can create millions of weekly rows.
# The complete weekly data and windows are always retained in
# the RDS. Set this to TRUE only when those large tables must
# also be written to the Excel workbook.
include_large_weekly_tables_in_workbook <- FALSE


# ------------------------------------------------------------
# 1B. SERIES-CONSTRUCTION SETTINGS
# ------------------------------------------------------------

seasonal_frequency <- 52L

# Primary policy: exclude epidemiologic Week 53 so every retained
# calendar year contributes exactly 52 positions, matching the
# seasonal frequency used during EpiFORESS development.
#
# Sensitivity option: "retain_as_observed" completes Weeks 1-52
# in every year and retains Week 53 only in source years where it
# appears. The time-series frequency remains 52, so this option
# should be interpreted as a sensitivity analysis rather than the
# main specification.
week_53_policy <- "exclude"
valid_week_53_policies <- c("exclude", "retain_as_observed")

if (!week_53_policy %in% valid_week_53_policies) {
  stop(
    paste0(
      "week_53_policy must be one of: ",
      paste(valid_week_53_policies, collapse = ", ")
    )
  )
}
window_length <- 260L
primary_window_mode <- "latest"
extended_window_mode <- "non_overlapping"

# A 260-week window must contain at least this proportion of
# observed weekly counts. Missing source rows remain NA.
minimum_window_observed_fraction <- 0.85

# Each rolling-origin training sample must contain at least this
# proportion of observed values before within-training-sample
# interpolation is attempted.
minimum_training_observed_fraction <- 0.85

# At least this many finite values must exist in a candidate
# 260-week window. The 85% criterion is stricter under the
# current settings, but this explicit floor protects future
# sensitivity runs with a lower completeness requirement.
minimum_window_observed_weeks <- 156L

# Remove impossible negative counts but do not replace them with
# zero. They become missing.
drop_negative_cases <- TRUE

# Primary empirical forecast-readiness outcome criterion.
empirical_mase_threshold <- 1.00


# ------------------------------------------------------------
# 1C. FORECASTING SETTINGS
# ------------------------------------------------------------

forecast_horizon <- 13L
initial_window <- 156L
rolling_step <- 13L
max_origins <- 6L

# Cluster-bootstrap confidence intervals. Reduce temporarily for
# code testing; retain 2000 for the final reported analysis.
bootstrap_replicates <- 2000L


# ------------------------------------------------------------
# 1D. CURRENT CONTINUOUS SCORE CALIBRATION
# ------------------------------------------------------------

best_mase_calibration_intercept <- 0.688744
best_mase_calibration_slope_per_10 <- -0.145625

estimate_median_best_mase <- function(score) {
  score <- suppressWarnings(as.numeric(score))
  out <- rep(NA_real_, length(score))
  ok <- is.finite(score)

  out[ok] <- exp(
    best_mase_calibration_intercept +
      best_mase_calibration_slope_per_10 * (score[ok] / 10)
  )

  out
}


# ------------------------------------------------------------
# 2. GENERAL HELPERS
# ------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

normalize_name <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

normalize_label <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_squish() %>%
    stringr::str_to_upper()
}

safe_numeric <- function(x) {
  out <- suppressWarnings(
    as.numeric(stringr::str_replace_all(as.character(x), ",|%", ""))
  )
  out[is.nan(out) | is.infinite(out)] <- NA_real_
  out
}

readiness_band <- function(score) {
  dplyr::case_when(
    !is.finite(score) ~ NA_character_,
    score < 20 ~ "Very_Low",
    score < 35 ~ "Low",
    score < 60 ~ "Moderate",
    score < 80 ~ "High",
    TRUE ~ "Very_High"
  )
}

safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 1L) return(NA_real_)
  stats::sd(x)
}

safe_mase_denominator <- function(training_series, frequency = 52L) {
  y <- as.numeric(training_series)
  y <- y[is.finite(y)]

  if (length(y) < 2L) return(NA_real_)

  if (frequency > 1L && length(y) > frequency) {
    seasonal_denominator <- mean(
      abs(y[(frequency + 1L):length(y)] - y[1L:(length(y) - frequency)]),
      na.rm = TRUE
    )

    if (is.finite(seasonal_denominator) && seasonal_denominator > 0) {
      return(seasonal_denominator)
    }
  }

  first_difference_denominator <- mean(abs(diff(y)), na.rm = TRUE)

  if (is.finite(first_difference_denominator) && first_difference_denominator > 0) {
    return(first_difference_denominator)
  }

  dispersion_denominator <- mean(abs(y - mean(y, na.rm = TRUE)), na.rm = TRUE)

  if (is.finite(dispersion_denominator) && dispersion_denominator > 0) {
    return(dispersion_denominator)
  }

  NA_real_
}

safe_sheet_name <- function(x) {
  out <- stringr::str_replace_all(as.character(x), "[:\\\\/?*\\[\\]]", "_")
  out <- stringr::str_sub(out, 1L, 31L)
  ifelse(is.na(out) | out == "", "Sheet", out)
}

write_tables_to_workbook <- function(
    tables,
    path,
    max_data_rows_per_sheet = 900000L
) {
  wb <- openxlsx::createWorkbook()

  title_style <- openxlsx::createStyle(
    fontSize = 12,
    fontColour = "#FFFFFF",
    fgFill = "#1D4E9E",
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    border = "Bottom",
    borderColour = "#143A6B"
  )

  body_style <- openxlsx::createStyle(fontSize = 10, valign = "top")
  used_names <- character(0)

  unique_sheet_name <- function(proposed_name) {
    safe_name <- safe_sheet_name(proposed_name)
    original_name <- safe_name
    suffix <- 1L

    while (safe_name %in% used_names) {
      suffix <- suffix + 1L
      safe_name <- safe_sheet_name(paste0(original_name, "_", suffix))
    }

    used_names <<- c(used_names, safe_name)
    safe_name
  }

  write_one_sheet <- function(tab, proposed_name) {
    safe_name <- unique_sheet_name(proposed_name)

    openxlsx::addWorksheet(wb, safe_name, gridLines = FALSE)
    openxlsx::writeData(
      wb,
      safe_name,
      tab,
      rowNames = FALSE,
      headerStyle = title_style
    )

    if (nrow(tab) > 0L) {
      openxlsx::addStyle(
        wb,
        safe_name,
        body_style,
        rows = 2L:(nrow(tab) + 1L),
        cols = seq_len(ncol(tab)),
        gridExpand = TRUE,
        stack = TRUE
      )
    }

    openxlsx::freezePane(wb, safe_name, firstRow = TRUE)
    openxlsx::addFilter(
      wb,
      safe_name,
      row = 1L,
      cols = seq_len(max(1L, ncol(tab)))
    )
    openxlsx::setColWidths(
      wb,
      safe_name,
      cols = seq_len(max(1L, ncol(tab))),
      widths = "auto"
    )

    # Width estimation uses a bounded sample so very large
    # location-stratified tables remain practical to export.
    if (ncol(tab) > 0L) {
      width_rows <- seq_len(min(nrow(tab), 10000L))

      for (j in seq_len(ncol(tab))) {
        values <- if (length(width_rows) > 0L) {
          as.character(tab[[j]][width_rows])
        } else {
          character(0)
        }

        max_chars <- suppressWarnings(
          max(nchar(values), nchar(names(tab)[j]), na.rm = TRUE)
        )

        if (is.finite(max_chars) && max_chars > 42) {
          openxlsx::setColWidths(wb, safe_name, cols = j, widths = 42)
        }
      }
    }
  }

  for (sheet_name in names(tables)) {
    tab <- tables[[sheet_name]]
    if (is.null(tab)) next

    tab <- as.data.frame(tab)

    if (ncol(tab) == 0L) {
      tab <- data.frame(
        Note = "No fields were returned for this output table.",
        stringsAsFactors = FALSE
      )
    }

    if (nrow(tab) <= max_data_rows_per_sheet) {
      write_one_sheet(tab, sheet_name)
    } else {
      chunk_count <- ceiling(nrow(tab) / max_data_rows_per_sheet)

      for (chunk_index in seq_len(chunk_count)) {
        first_row <- (chunk_index - 1L) * max_data_rows_per_sheet + 1L
        last_row <- min(chunk_index * max_data_rows_per_sheet, nrow(tab))

        chunk <- tab[first_row:last_row, , drop = FALSE]
        chunk_name <- paste0(
          sheet_name,
          "_",
          sprintf("%02d", chunk_index)
        )

        write_one_sheet(chunk, chunk_name)
      }
    }
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

recursive_find_scalar <- function(object, candidate_names) {
  candidate_keys <- normalize_name(candidate_names)

  walk <- function(x) {
    if (is.null(x)) return(NULL)

    if (is.data.frame(x)) {
      matched <- which(normalize_name(names(x)) %in% candidate_keys)

      if (length(matched) > 0L) {
        for (index in matched) {
          value <- x[[index]][1]
          if (length(value) > 0L && !is.na(value)) return(value)
        }
      }
    }

    if (is.list(x)) {
      nms <- names(x)

      if (!is.null(nms)) {
        matched <- which(normalize_name(nms) %in% candidate_keys)

        if (length(matched) > 0L) {
          for (index in matched) {
            value <- x[[index]]
            if (is.atomic(value) && length(value) > 0L && !is.na(value[1])) {
              return(value[1])
            }
          }
        }
      }

      for (element in x) {
        result <- walk(element)
        if (!is.null(result)) return(result)
      }
    }

    NULL
  }

  walk(object)
}

recursive_find_feature_table <- function(object, predictor_count_target = 49L) {
  walk <- function(x) {
    if (is.null(x)) return(NULL)

    if (is.data.frame(x)) {
      normalized_columns <- normalize_name(names(x))

      if (
        ncol(x) >= predictor_count_target ||
        (
          any(normalized_columns %in% c("variable", "feature", "predictor")) &&
          any(normalized_columns %in% c("value", "raw_value", "feature_value"))
        )
      ) {
        return(x)
      }
    }

    if (is.list(x)) {
      for (element in x) {
        result <- walk(element)
        if (!is.null(result)) return(result)
      }
    }

    NULL
  }

  walk(object)
}


# ------------------------------------------------------------
# 3. VALIDATE REQUIRED FILES
# ------------------------------------------------------------

if (!file.exists(tycho_csv_path)) {
  stop(paste0("Project Tycho CSV not found:\n", tycho_csv_path))
}

if (length(app_file) == 0L || is.na(app_file) || !file.exists(app_file)) {
  stop(
    paste0(
      "No EpiFORESS application file was found.\nExpected one of:\n",
      paste(app_candidates, collapse = "\n")
    )
  )
}

if (!file.exists(stat_model_path)) {
  stop(paste0("Statistical model not found:\n", stat_model_path))
}

if (!file.exists(ml_model_path)) {
  stop(paste0("Machine-learning model not found:\n", ml_model_path))
}


# ------------------------------------------------------------
# 4. LOAD THE APP SCORING ENGINE
# ------------------------------------------------------------

cat("\nLoading EpiFORESS scoring engine from:\n", app_file, "\n")

old_working_directory <- getwd()
setwd(base_path)

app_environment <- new.env(parent = globalenv())
source_problem <- NULL

tryCatch(
  {
    sys.source(app_file, envir = app_environment, keep.source = FALSE)
  },
  error = function(e) {
    source_problem <<- e
  },
  finally = {
    setwd(old_working_directory)
  }
)

if (!is.null(source_problem)) {
  stop(
    paste0(
      "The EpiFORESS app could not be sourced from the project root.\n\n",
      conditionMessage(source_problem)
    )
  )
}

# score_one_series() may be defined either at app scope or inside
# server(). Prefer capturing the actual closure from a Shiny test
# session. If server initialization cannot run in testServer(), use
# a guarded syntax-tree fallback that traverses function bodies but
# never traverses formal-argument pairlists.
extract_server_functions <- function(server_function, parent_environment) {
  extracted_environment <- new.env(parent = parent_environment)
  extracted_names <- character(0)

  safe_language_element <- function(x, index) {
    tryCatch(x[[index]], error = function(e) NULL)
  }

  walk_expression <- function(expression) {
    if (is.null(expression)) return(invisible(NULL))

    call_head <- if (is.call(expression) && length(expression) >= 1L) {
      safe_language_element(expression, 1L)
    } else {
      NULL
    }

    is_function_assignment <-
      is.call(expression) &&
      length(expression) >= 3L &&
      (
        identical(call_head, as.name("<-")) ||
          identical(call_head, as.name("="))
      ) &&
      is.symbol(safe_language_element(expression, 2L))

    if (is_function_assignment) {
      assignment_name <- as.character(expression[[2]])
      assignment_value <- safe_language_element(expression, 3L)

      if (
        is.call(assignment_value) &&
        identical(assignment_value[[1]], as.name("function"))
      ) {
        evaluation_result <- tryCatch(
          {
            eval(expression, envir = extracted_environment)
            TRUE
          },
          error = function(e) FALSE
        )

        if (
          isTRUE(evaluation_result) &&
          exists(assignment_name, envir = extracted_environment, inherits = FALSE) &&
          is.function(
            get(assignment_name, envir = extracted_environment, inherits = FALSE)
          )
        ) {
          extracted_names <<- unique(c(extracted_names, assignment_name))
        }

        # Function structure: function(formals) body. Recurse only
        # through the body because missing formal defaults cannot be
        # safely assigned to a loop variable.
        if (length(assignment_value) >= 3L) {
          walk_expression(safe_language_element(assignment_value, 3L))
        }

        return(invisible(NULL))
      }
    }

    if (
      is.call(expression) &&
      length(expression) >= 1L &&
      identical(expression[[1]], as.name("function"))
    ) {
      if (length(expression) >= 3L) {
        walk_expression(safe_language_element(expression, 3L))
      }
      return(invisible(NULL))
    }

    if (is.expression(expression) || is.call(expression)) {
      for (index in seq_along(expression)) {
        element <- safe_language_element(expression, index)
        if (is.call(element) || is.expression(element)) {
          walk_expression(element)
        }
      }
    }

    invisible(NULL)
  }

  walk_expression(body(server_function))
  attr(extracted_environment, "extracted_names") <- extracted_names
  extracted_environment
}

scoring_capture_method <- NA_character_
scoring_capture_note <- NA_character_

if (exists("score_one_series", envir = app_environment, inherits = FALSE)) {
  score_one_series_app <- get(
    "score_one_series",
    envir = app_environment,
    inherits = FALSE
  )
  scoring_capture_method <- "App-scope function"
  scoring_capture_note <- "score_one_series() was defined directly in the sourced app environment."
} else {
  server_candidates <- c("server", "app_server", "epiforess_server")

  available_server <- server_candidates[
    vapply(
      server_candidates,
      function(candidate) {
        exists(candidate, envir = app_environment, inherits = FALSE) &&
          is.function(
            get(candidate, envir = app_environment, inherits = FALSE)
          )
      },
      logical(1)
    )
  ][1]

  if (length(available_server) == 0L || is.na(available_server)) {
    stop(
      paste0(
        "The app loaded, but neither score_one_series() nor a server function was found.\n\n",
        "Available app objects include:\n",
        paste(head(ls(app_environment, all.names = TRUE), 100L), collapse = ", ")
      )
    )
  }

  server_function <- get(
    available_server,
    envir = app_environment,
    inherits = FALSE
  )

  scoring_holder <- new.env(parent = emptyenv())
  test_server_problem <- NULL

  tryCatch(
    {
      shiny::testServer(
        server_function,
        {
          if (!exists("score_one_series", inherits = TRUE)) {
            stop("score_one_series() was not created in the active server environment.")
          }

          scoring_holder$score_one_series <- get(
            "score_one_series",
            inherits = TRUE
          )
        }
      )
    },
    error = function(e) {
      test_server_problem <<- e
    }
  )

  if (
    exists("score_one_series", envir = scoring_holder, inherits = FALSE) &&
    is.function(scoring_holder$score_one_series)
  ) {
    score_one_series_app <- scoring_holder$score_one_series
    scoring_capture_method <- "shiny::testServer"
    scoring_capture_note <- "Captured the actual server-local closure."
  } else {
    scoring_environment <- extract_server_functions(
      server_function = server_function,
      parent_environment = app_environment
    )

    if (
      !exists(
        "score_one_series",
        envir = scoring_environment,
        inherits = FALSE
      )
    ) {
      extracted_names <- attr(
        scoring_environment,
        "extracted_names"
      ) %||% character(0)

      stop(
        paste0(
          "score_one_series() could not be captured from server().\n\n",
          "testServer() problem:\n",
          if (is.null(test_server_problem)) {
            "No function was captured, but testServer() returned no explicit error."
          } else {
            conditionMessage(test_server_problem)
          },
          "\n\nFunctions exposed by the guarded fallback:\n",
          if (length(extracted_names) == 0L) {
            "(none)"
          } else {
            paste(extracted_names, collapse = ", ")
          }
        )
      )
    }

    score_one_series_app <- get(
      "score_one_series",
      envir = scoring_environment,
      inherits = FALSE
    )
    scoring_capture_method <- "Guarded server-body extraction fallback"
    scoring_capture_note <- if (is.null(test_server_problem)) {
      "testServer() returned without capturing the function."
    } else {
      paste0("testServer() failed: ", conditionMessage(test_server_problem))
    }
  }
}

if (!is.function(score_one_series_app)) {
  stop("The captured score_one_series object is not callable.")
}

statistical_model <- readRDS(stat_model_path)
machine_learning_model <- readRDS(ml_model_path)

cat(
  "\nScoring engine loaded successfully.\n",
  "Resolved Saved_Models directory:\n  ", model_dir, "\n",
  "Statistical model class: ", paste(class(statistical_model), collapse = ", "), "\n",
  "Machine-learning model class: ", paste(class(machine_learning_model), collapse = ", "), "\n",
  "Scoring-function capture: ", scoring_capture_method, "\n",
  sep = ""
)


# ------------------------------------------------------------
# 5. READ AND STANDARDIZE PROJECT TYCHO
# ------------------------------------------------------------

cat("\nReading Project Tycho CSV...\n")

tycho_raw <- data.table::fread(
  tycho_csv_path,
  na.strings = c("", "NA", "N/A", "NULL", "."),
  showProgress = TRUE
) %>%
  tibble::as_tibble()

original_column_names <- names(tycho_raw)
normalized_column_names <- normalize_name(original_column_names)

find_tycho_column <- function(candidates) {
  matched <- match(normalize_name(candidates), normalized_column_names)
  matched <- matched[!is.na(matched)]
  if (length(matched) == 0L) return(NA_character_)
  original_column_names[matched[1]]
}

year_column <- find_tycho_column(c("Year"))
week_column <- find_tycho_column(c("Week", "Epiweek", "Epi_Week"))
location_column <- find_tycho_column(c("Location", "State", "City"))
location_type_column <- find_tycho_column(c("loc_type", "Location_Type", "LocationType"))
disease_column <- find_tycho_column(c("Disease", "Condition"))
cases_column <- find_tycho_column(c("Cases", "Case_Count", "Count"))

required_columns <- c(
  Year = year_column,
  Week = week_column,
  Location = location_column,
  Location_Type = location_type_column,
  Disease = disease_column,
  Cases = cases_column
)

missing_required <- names(required_columns)[is.na(required_columns)]

if (length(missing_required) > 0L) {
  stop(
    paste0(
      "The following required Project Tycho fields were not found:\n",
      paste(missing_required, collapse = ", "),
      "\n\nAvailable columns:\n",
      paste(original_column_names, collapse = ", ")
    )
  )
}

tycho <- tycho_raw %>%
  dplyr::transmute(
    Year = as.integer(safe_numeric(.data[[year_column]])),
    Week = as.integer(safe_numeric(.data[[week_column]])),
    Location = normalize_label(.data[[location_column]]),
    Location_Type = normalize_label(.data[[location_type_column]]),
    Disease = normalize_label(.data[[disease_column]]),
    Cases = safe_numeric(.data[[cases_column]])
  ) %>%
  dplyr::filter(
    !is.na(Year),
    !is.na(Week),
    Week >= 1L,
    Week <= 53L,
    !is.na(Disease),
    Disease != "",
    !is.na(Location_Type),
    Location_Type != ""
  )

if (isTRUE(require_nonempty_location)) {
  tycho <- tycho %>%
    dplyr::filter(!is.na(Location), Location != "")
}

if (isTRUE(drop_negative_cases)) {
  tycho <- tycho %>%
    dplyr::mutate(Cases = dplyr::if_else(Cases < 0, NA_real_, Cases))
}

if (!is.null(disease_filter)) {
  tycho <- tycho %>%
    dplyr::filter(Disease %in% normalize_label(disease_filter))
}

if (!is.null(location_filter)) {
  tycho <- tycho %>%
    dplyr::filter(Location %in% normalize_label(location_filter))
}

if (!is.null(location_type_filter)) {
  tycho <- tycho %>%
    dplyr::filter(Location_Type %in% normalize_label(location_type_filter))
}

source_week_53_rows <- sum(tycho$Week == 53L, na.rm = TRUE)

if (identical(week_53_policy, "exclude")) {
  tycho <- tycho %>%
    dplyr::filter(Week <= seasonal_frequency)
}

if (nrow(tycho) == 0L) {
  stop("No Project Tycho rows remained after standardization and optional filtering.")
}

# Collapse duplicate source rows only within the exact same
# disease-location-location-type week.
tycho_location_week <- tycho %>%
  dplyr::group_by(Disease, Location_Type, Location, Year, Week) %>%
  dplyr::summarise(
    Cases = if (all(is.na(Cases))) NA_real_ else sum(Cases, na.rm = TRUE),
    Source_Rows = dplyr::n(),
    .groups = "drop"
  )

# Construct a regular epidemiologic-week calendar. Under the
# primary policy, every source year contributes Weeks 1-52. Under
# the sensitivity policy, Week 53 is added only to source years in
# which at least one retained record contains it.
source_years <- seq.int(
  min(tycho_location_week$Year, na.rm = TRUE),
  max(tycho_location_week$Year, na.rm = TRUE)
)

if (identical(week_53_policy, "exclude")) {
  calendar <- tidyr::expand_grid(
    Year = source_years,
    Week = seq_len(seasonal_frequency)
  )
} else {
  week_53_years <- tycho_location_week %>%
    dplyr::filter(Week == 53L) %>%
    dplyr::distinct(Year) %>%
    dplyr::pull(Year)

  calendar <- purrr::map_dfr(
    source_years,
    function(year_value) {
      maximum_week <- if (year_value %in% week_53_years) 53L else 52L
      tibble::tibble(
        Year = year_value,
        Week = seq_len(maximum_week)
      )
    }
  )
}

calendar <- calendar %>%
  dplyr::arrange(Year, Week) %>%
  dplyr::mutate(Global_Week_ID = dplyr::row_number())

tycho_location_week <- tycho_location_week %>%
  dplyr::left_join(calendar, by = c("Year", "Week"))

# Deterministic geographic series index. Location_Type is part of
# the identity so an identically named city and state cannot mix.
series_index <- tycho_location_week %>%
  dplyr::distinct(Disease, Location_Type, Location) %>%
  dplyr::arrange(Disease, Location_Type, Location) %>%
  dplyr::mutate(
    Series_Number = dplyr::row_number(),
    Series_ID = sprintf("TYCHO_%05d", Series_Number),
    Geographic_Location_ID = paste(Location_Type, Location, sep = " | "),
    Series_Label = paste(Disease, Location_Type, Location, sep = " | ")
  )

tycho_location_week <- tycho_location_week %>%
  dplyr::left_join(
    series_index,
    by = c("Disease", "Location_Type", "Location")
  )


# ------------------------------------------------------------
# 6. BUILD LOCATION-STRATIFIED WEEKLY SERIES
# ------------------------------------------------------------

build_location_stratified_series <- function(series_rows, calendar_table) {
  series_id_value <- series_rows$Series_ID[1]
  disease_value <- series_rows$Disease[1]
  location_type_value <- series_rows$Location_Type[1]
  location_value <- series_rows$Location[1]
  geographic_location_id_value <- series_rows$Geographic_Location_ID[1]
  series_label_value <- series_rows$Series_Label[1]

  series_week_range <- range(series_rows$Global_Week_ID, na.rm = TRUE)

  series_calendar <- calendar_table %>%
    dplyr::filter(
      Global_Week_ID >= series_week_range[1],
      Global_Week_ID <= series_week_range[2]
    )

  completed <- series_calendar %>%
    dplyr::left_join(
      series_rows %>%
        dplyr::select(Global_Week_ID, Cases, Source_Rows),
      by = "Global_Week_ID"
    ) %>%
    dplyr::mutate(
      Series_ID = series_id_value,
      Series_Label = series_label_value,
      Disease = disease_value,
      Location_Type = location_type_value,
      Location = location_value,
      Geographic_Location_ID = geographic_location_id_value,
      Report_Present = !is.na(Source_Rows),
      Observed_Case = is.finite(Cases),
      Count = Cases,
      .before = 1
    ) %>%
    dplyr::arrange(Global_Week_ID)

  profile <- completed %>%
    dplyr::summarise(
      Series_ID = dplyr::first(Series_ID),
      Series_Label = dplyr::first(Series_Label),
      Disease = dplyr::first(Disease),
      Location_Type = dplyr::first(Location_Type),
      Location = dplyr::first(Location),
      Geographic_Location_ID = dplyr::first(Geographic_Location_ID),
      Start_Year = dplyr::first(Year),
      Start_Week = dplyr::first(Week),
      End_Year = dplyr::last(Year),
      End_Week = dplyr::last(Week),
      Calendar_Weeks = dplyr::n(),
      Weeks_With_Source_Row = sum(Report_Present),
      Weeks_With_Observed_Cases = sum(Observed_Case),
      Row_Coverage = mean(Report_Present),
      Observed_Case_Coverage = mean(Observed_Case),
      Mean_Count = mean(Count, na.rm = TRUE),
      SD_Count = stats::sd(Count, na.rm = TRUE),
      Rows_With_Source_But_Missing_Cases = sum(Report_Present & !Observed_Case)
    )

  list(series = completed, profile = profile)
}

series_results <- tycho_location_week %>%
  split(.$Series_ID) %>%
  purrr::map(
    ~ build_location_stratified_series(
      series_rows = .x,
      calendar_table = calendar
    )
  )

location_series_data <- purrr::map_dfr(series_results, "series")
location_series_profile <- purrr::map_dfr(series_results, "profile")

if (nrow(location_series_data) == 0L) {
  stop("No location-stratified disease series could be constructed.")
}


# ------------------------------------------------------------
# 7. CONSTRUCT 260-WEEK VALIDATION WINDOWS
# ------------------------------------------------------------

window_is_eligible <- function(window_data) {
  if (nrow(window_data) != window_length) return(FALSE)

  observed_fraction <- mean(is.finite(window_data$Count))
  observed_counts <- window_data$Count[is.finite(window_data$Count)]

  observed_fraction >= minimum_window_observed_fraction &&
    length(observed_counts) >= minimum_window_observed_weeks &&
    is.finite(safe_sd(observed_counts)) &&
    safe_sd(observed_counts) > 0
}

latest_eligible_window <- function(series_data) {
  n <- nrow(series_data)
  if (n < window_length) return(NULL)

  possible_ends <- seq(from = n, to = window_length, by = -1L)

  for (end_index in possible_ends) {
    start_index <- end_index - window_length + 1L
    candidate <- series_data[start_index:end_index, , drop = FALSE]

    if (window_is_eligible(candidate)) return(candidate)
  }

  NULL
}

nonoverlapping_eligible_windows <- function(series_data) {
  n <- nrow(series_data)
  if (n < window_length) return(list())

  end_indices <- seq(from = n, to = window_length, by = -window_length)
  windows <- list()

  for (end_index in end_indices) {
    start_index <- end_index - window_length + 1L
    candidate <- series_data[start_index:end_index, , drop = FALSE]

    if (window_is_eligible(candidate)) {
      windows[[length(windows) + 1L]] <- candidate
    }
  }

  rev(windows)
}

primary_windows <- list()
extended_windows <- list()
window_eligibility_rows <- list()

for (series_id_value in sort(unique(location_series_data$Series_ID))) {
  series_data <- location_series_data %>%
    dplyr::filter(Series_ID == series_id_value) %>%
    dplyr::arrange(Global_Week_ID)

  primary_window <- latest_eligible_window(series_data)
  extended_for_series <- nonoverlapping_eligible_windows(series_data)

  window_eligibility_rows[[series_id_value]] <- tibble::tibble(
    Series_ID = series_id_value,
    Series_Label = series_data$Series_Label[1],
    Disease = series_data$Disease[1],
    Location_Type = series_data$Location_Type[1],
    Location = series_data$Location[1],
    Geographic_Location_ID = series_data$Geographic_Location_ID[1],
    Calendar_Weeks = nrow(series_data),
    Observed_Weeks = sum(is.finite(series_data$Count)),
    Observed_Fraction = mean(is.finite(series_data$Count)),
    Primary_Eligible = !is.null(primary_window),
    Extended_Eligible_Windows = length(extended_for_series)
  )

  if (!is.null(primary_window)) {
    primary_window <- primary_window %>%
      dplyr::mutate(
        Window_ID = paste0(series_id_value, "__latest"),
        Window_Type = "Primary_Latest_260_Weeks_Per_Location_Stratum",
        Window_Number = 1L,
        .before = 1
      )

    primary_windows[[series_id_value]] <- primary_window
  }

  if (length(extended_for_series) > 0L) {
    for (window_index in seq_along(extended_for_series)) {
      current_window <- extended_for_series[[window_index]] %>%
        dplyr::mutate(
          Window_ID = paste0(
            series_id_value,
            "__window_",
            sprintf("%03d", window_index)
          ),
          Window_Type = "Extended_Nonoverlapping_260_Weeks_Per_Location_Stratum",
          Window_Number = window_index,
          .before = 1
        )

      extended_windows[[paste0(series_id_value, "__", window_index)]] <- current_window
    }
  }
}

window_eligibility_table <- dplyr::bind_rows(window_eligibility_rows)
primary_window_data <- dplyr::bind_rows(primary_windows)
extended_window_data <- dplyr::bind_rows(extended_windows)

if (nrow(primary_window_data) == 0L) {
  stop("No Disease x Location x Location_Type series had an eligible primary 260-week window.")
}


# ------------------------------------------------------------
# 8. FORECASTING CANDIDATE MODELS
# ------------------------------------------------------------

fit_seasonal_naive <- function(train_ts, horizon) {
  as.numeric(forecast::forecast(forecast::snaive(train_ts), h = horizon)$mean)
}

fit_stl_ets <- function(train_ts, horizon) {
  frequency_value <- stats::frequency(train_ts)

  if (frequency_value > 1 && length(train_ts) >= 2L * frequency_value) {
    stlf_result <- tryCatch(
      forecast::stlf(train_ts, h = horizon, method = "ets", robust = TRUE),
      error = function(e) NULL
    )

    if (!is.null(stlf_result)) return(as.numeric(stlf_result$mean))
  }

  as.numeric(
    forecast::forecast(
      forecast::ets(train_ts, model = "ZZN"),
      h = horizon
    )$mean
  )
}

fit_nonseasonal_arima <- function(train_ts, horizon) {
  as.numeric(
    forecast::forecast(
      forecast::auto.arima(
        train_ts,
        seasonal = FALSE,
        stepwise = TRUE,
        approximation = TRUE,
        max.p = 2L,
        max.q = 2L,
        max.d = 1L,
        ic = "aic"
      ),
      h = horizon
    )$mean
  )
}

fit_sarima <- function(train_ts, horizon) {
  as.numeric(
    forecast::forecast(
      forecast::auto.arima(
        train_ts,
        seasonal = TRUE,
        stepwise = TRUE,
        approximation = TRUE,
        max.p = 2L,
        max.q = 2L,
        max.P = 1L,
        max.Q = 1L,
        max.d = 1L,
        max.D = 1L,
        ic = "aic"
      ),
      h = horizon
    )$mean
  )
}

fit_negbin_regression <- function(train_ts, horizon) {
  y <- as.numeric(train_ts)
  y <- y[is.finite(y)]
  n <- length(y)
  frequency_value <- stats::frequency(train_ts)

  if (n < 30L || !is.finite(safe_sd(y)) || safe_sd(y) == 0) {
    return(rep(mean(y, na.rm = TRUE), horizon))
  }

  fourier_k <- if (frequency_value > 1L && n >= 2L * frequency_value) {
    min(3L, floor(frequency_value / 2L) - 1L)
  } else {
    0L
  }

  if (fourier_k > 0L) {
    xreg_train <- as.data.frame(forecast::fourier(train_ts, K = fourier_k))
    xreg_future <- as.data.frame(forecast::fourier(train_ts, K = fourier_k, h = horizon))
  } else {
    xreg_train <- data.frame()
    xreg_future <- data.frame()
  }

  xreg_train$t <- seq_len(n)
  xreg_future$t <- (n + 1L):(n + horizon)

  training_data <- data.frame(y = pmax(round(y), 0), xreg_train)

  model <- suppressWarnings(
    tryCatch(
      MASS::glm.nb(
        y ~ .,
        data = training_data,
        control = stats::glm.control(maxit = 100L)
      ),
      error = function(e) NULL
    )
  )

  if (is.null(model)) return(rep(mean(y, na.rm = TRUE), horizon))

  predictions <- tryCatch(
    as.numeric(stats::predict(model, newdata = xreg_future, type = "response")),
    error = function(e) rep(NA_real_, horizon)
  )

  predictions <- pmax(predictions, 0)

  prediction_cap <- max(
    max(y, na.rm = TRUE) * 10,
    stats::quantile(y, 0.99, na.rm = TRUE, names = FALSE) * 20,
    mean(y, na.rm = TRUE) + 10 * stats::sd(y, na.rm = TRUE),
    1,
    na.rm = TRUE
  )

  predictions[!is.finite(predictions)] <- NA_real_
  predictions[predictions > prediction_cap] <- prediction_cap
  predictions[is.na(predictions)] <- mean(y, na.rm = TRUE)
  predictions
}

forecast_candidates <- list(
  Seasonal_Naive = fit_seasonal_naive,
  STL_ETS = fit_stl_ets,
  Nonseasonal_ARIMA = fit_nonseasonal_arima,
  SARIMA = fit_sarima,
  NegBin_Regression = fit_negbin_regression
)

evaluate_candidate_model <- function(counts, model_name, forecasting_function) {
  counts <- as.numeric(counts)
  n <- length(counts)

  origins <- seq(
    from = initial_window,
    to = n - forecast_horizon,
    by = rolling_step
  )

  if (length(origins) > max_origins) origins <- tail(origins, max_origins)

  actual_values <- numeric(0)
  predicted_values <- numeric(0)
  denominator_values <- numeric(0)
  successful_origins <- 0L
  attempted_origins <- length(origins)

  for (origin in origins) {
    training_raw <- counts[seq_len(origin)]
    test_values <- counts[(origin + 1L):(origin + forecast_horizon)]

    minimum_observed_training <- ceiling(
      length(training_raw) * minimum_training_observed_fraction
    )

    if (sum(is.finite(training_raw)) < minimum_observed_training) next

    training_ts <- tryCatch(
      forecast::na.interp(
        stats::ts(training_raw, frequency = seasonal_frequency)
      ),
      error = function(e) NULL
    )

    if (is.null(training_ts)) next

    predictions <- tryCatch(
      forecasting_function(training_ts, forecast_horizon),
      error = function(e) rep(NA_real_, forecast_horizon)
    )

    predictions <- pmax(as.numeric(predictions), 0)
    predictions[!is.finite(predictions)] <- NA_real_

    denominator <- safe_mase_denominator(
      training_ts,
      frequency = seasonal_frequency
    )

    actual_values <- c(actual_values, test_values)
    predicted_values <- c(predicted_values, predictions)
    denominator_values <- c(
      denominator_values,
      rep(denominator, length(test_values))
    )

    successful_origins <- successful_origins + 1L
  }

  valid <- stats::complete.cases(
    actual_values,
    predicted_values,
    denominator_values
  ) & denominator_values > 0

  if (sum(valid) == 0L) {
    return(
      tibble::tibble(
        Model = model_name,
        MAE = NA_real_,
        RMSE = NA_real_,
        Bias = NA_real_,
        MASE = NA_real_,
        N_Forecasted = 0L,
        N_Origins = successful_origins,
        N_Origins_Attempted = attempted_origins
      )
    )
  }

  errors <- actual_values[valid] - predicted_values[valid]

  tibble::tibble(
    Model = model_name,
    MAE = mean(abs(errors), na.rm = TRUE),
    RMSE = sqrt(mean(errors^2, na.rm = TRUE)),
    Bias = mean(predicted_values[valid] - actual_values[valid], na.rm = TRUE),
    MASE = mean(abs(errors) / denominator_values[valid], na.rm = TRUE),
    N_Forecasted = sum(valid),
    N_Origins = successful_origins,
    N_Origins_Attempted = attempted_origins
  )
}

evaluate_window_forecasts <- function(window_data) {
  counts <- window_data$Count

  all_models <- purrr::imap_dfr(
    forecast_candidates,
    ~ evaluate_candidate_model(
      counts = counts,
      model_name = .y,
      forecasting_function = .x
    )
  )

  best_model <- all_models %>%
    dplyr::filter(is.finite(MASE)) %>%
    dplyr::arrange(MASE, RMSE, MAE) %>%
    dplyr::slice(1L)

  if (nrow(best_model) == 0L) {
    best_model <- tibble::tibble(
      Model = NA_character_,
      MAE = NA_real_,
      RMSE = NA_real_,
      Bias = NA_real_,
      MASE = NA_real_,
      N_Forecasted = 0L,
      N_Origins = 0L,
      N_Origins_Attempted = 0L
    )
  }

  list(all_models = all_models, best_model = best_model)
}


# ------------------------------------------------------------
# 9. SCORE WINDOWS USING THE DEPLOYED APP ENGINE
# ------------------------------------------------------------

score_window_with_app <- function(window_data) {
  series_name <- paste0(
    window_data$Disease[1],
    " | ",
    window_data$Location_Type[1],
    ": ",
    window_data$Location[1],
    " | ",
    window_data$Year[1],
    "-W",
    sprintf("%02d", window_data$Week[1]),
    " to ",
    window_data$Year[nrow(window_data)],
    "-W",
    sprintf("%02d", window_data$Week[nrow(window_data)])
  )

  app_series_data <- window_data %>%
    dplyr::transmute(
      Year = Year,
      Week = Week,
      Epiweek = paste0(Year, sprintf("%02d", Week)),
      Date = as.Date("1970-01-01") +
        7L * (Global_Week_ID - min(Global_Week_ID)),
      Row_Index = dplyr::row_number(),
      Count = Count,
      Count_Selected = Count
    )

  score_result <- tryCatch(
    score_one_series_app(
      series_name = series_name,
      series_data = app_series_data,
      frequency_value = seasonal_frequency,
      stat_model_loaded = statistical_model,
      ml_model_loaded = machine_learning_model
    ),
    error = function(e) {
      structure(
        list(Error_Message = conditionMessage(e)),
        class = "epiforess_score_error"
      )
    }
  )

  if (inherits(score_result, "epiforess_score_error")) {
    return(
      list(
        summary = tibble::tibble(
          EpiFORESS = NA_real_,
          Readiness_Level = NA_character_,
          Deployment_Action = NA_character_,
          Estimated_Median_Best_MASE = NA_real_,
          Score_Status = "Failed",
          Score_Message = score_result$Error_Message
        ),
        feature_table = NULL,
        raw_object = score_result
      )
    )
  }

  score <- safe_numeric(
    recursive_find_scalar(
      score_result,
      c("EpiFORESS", "EpiFORESS_Score", "Score")
    )
  )[1]

  readiness_level <- as.character(
    recursive_find_scalar(
      score_result,
      c("Readiness_Level", "Readiness_Band", "Forecast_Readiness")
    )
  )[1]

  deployment_action <- as.character(
    recursive_find_scalar(
      score_result,
      c("Deployment_Action", "Recommendation", "Recommended_Action")
    )
  )[1]

  app_estimated_mase <- safe_numeric(
    recursive_find_scalar(
      score_result,
      c(
        "Estimated_Median_Best_MASE",
        "Estimated_Best_MASE",
        "Expected_Attainable_MASE"
      )
    )
  )[1]

  score_status <- as.character(
    recursive_find_scalar(score_result, c("Score_Status", "Status"))
  )[1]

  if (is.na(score_status) || score_status == "") {
    score_status <- if (is.finite(score)) "OK" else "Failed"
  }

  feature_table <- recursive_find_feature_table(score_result)

  list(
    summary = tibble::tibble(
      EpiFORESS = score,
      Readiness_Level = readiness_level %||% readiness_band(score),
      Deployment_Action = deployment_action,
      Estimated_Median_Best_MASE = if (is.finite(app_estimated_mase)) {
        app_estimated_mase
      } else {
        estimate_median_best_mase(score)
      },
      Score_Status = score_status,
      Score_Message = if (is.finite(score)) {
        "Scored with deployed application engine."
      } else {
        "The application returned no finite score."
      }
    ),
    feature_table = feature_table,
    raw_object = score_result
  )
}


# ------------------------------------------------------------
# 10. EVALUATE AND SCORE ALL WINDOWS
# ------------------------------------------------------------

evaluate_window_collection <- function(window_table, collection_label) {
  if (nrow(window_table) == 0L) {
    return(
      list(
        metadata = tibble::tibble(),
        all_models = tibble::tibble(),
        best_models = tibble::tibble(),
        scores = tibble::tibble(),
        features = tibble::tibble()
      )
    )
  }

  window_ids <- unique(window_table$Window_ID)
  metadata_rows <- list()
  all_model_rows <- list()
  best_model_rows <- list()
  score_rows <- list()
  feature_rows <- list()

  for (index in seq_along(window_ids)) {
    window_id <- window_ids[index]

    current_window <- window_table %>%
      dplyr::filter(Window_ID == window_id) %>%
      dplyr::arrange(Global_Week_ID)

    series_id_value <- current_window$Series_ID[1]
    disease_name <- current_window$Disease[1]
    location_name <- current_window$Location[1]
    location_type_value <- current_window$Location_Type[1]

    cat(
      "\n[", collection_label, "] ",
      index, "/", length(window_ids), ": ",
      disease_name, " | ", location_type_value, " | ", location_name,
      " | ", window_id, "\n",
      sep = ""
    )

    forecast_result <- evaluate_window_forecasts(current_window)
    score_result <- score_window_with_app(current_window)

    metadata_row <- tibble::tibble(
      Dataset = collection_label,
      Window_ID = window_id,
      Series_ID = series_id_value,
      Series_Label = current_window$Series_Label[1],
      Disease = disease_name,
      Location_Type = location_type_value,
      Location = location_name,
      Geographic_Location_ID = current_window$Geographic_Location_ID[1],
      Window_Type = current_window$Window_Type[1],
      Window_Number = current_window$Window_Number[1],
      Start_Year = current_window$Year[1],
      Start_Week = current_window$Week[1],
      End_Year = current_window$Year[nrow(current_window)],
      End_Week = current_window$Week[nrow(current_window)],
      N_Weeks = nrow(current_window),
      N_Source_Rows = sum(current_window$Report_Present),
      N_Observed = sum(is.finite(current_window$Count)),
      Observed_Fraction = mean(is.finite(current_window$Count)),
      Mean_Count = mean(current_window$Count, na.rm = TRUE),
      SD_Count = stats::sd(current_window$Count, na.rm = TRUE)
    )

    all_model_row <- forecast_result$all_models %>%
      dplyr::mutate(
        Dataset = collection_label,
        Window_ID = window_id,
        Series_ID = series_id_value,
        Disease = disease_name,
        Location_Type = location_type_value,
        Location = location_name,
        .before = 1
      )

    best_model_row <- forecast_result$best_model %>%
      dplyr::transmute(
        Dataset = collection_label,
        Window_ID = window_id,
        Series_ID = series_id_value,
        Disease = disease_name,
        Location_Type = location_type_value,
        Location = location_name,
        Best_Model = Model,
        Best_MAE = MAE,
        Best_RMSE = RMSE,
        Best_Bias = Bias,
        Best_MASE = MASE,
        N_Forecasted = N_Forecasted,
        N_Origins = N_Origins,
        N_Origins_Attempted = N_Origins_Attempted
      )

    score_row <- score_result$summary %>%
      dplyr::mutate(
        Dataset = collection_label,
        Window_ID = window_id,
        Series_ID = series_id_value,
        Disease = disease_name,
        Location_Type = location_type_value,
        Location = location_name,
        Geographic_Location_ID = current_window$Geographic_Location_ID[1],
        EpiFORESS_Band = readiness_band(EpiFORESS),
        .before = 1
      )

    if (!is.null(score_result$feature_table)) {
      feature_rows[[window_id]] <- as.data.frame(score_result$feature_table) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(
          Dataset = collection_label,
          Window_ID = window_id,
          Series_ID = series_id_value,
          Disease = disease_name,
          Location_Type = location_type_value,
          Location = location_name,
          .before = 1
        )
    }

    metadata_rows[[window_id]] <- metadata_row
    all_model_rows[[window_id]] <- all_model_row
    best_model_rows[[window_id]] <- best_model_row
    score_rows[[window_id]] <- score_row

    cat(
      "  Best model: ", best_model_row$Best_Model,
      " | Best MASE: ", round(best_model_row$Best_MASE, 4),
      " | EpiFORESS: ",
      ifelse(is.finite(score_row$EpiFORESS), round(score_row$EpiFORESS, 2), NA),
      " | Score status: ", score_row$Score_Status,
      "\n",
      sep = ""
    )
  }

  list(
    metadata = dplyr::bind_rows(metadata_rows),
    all_models = dplyr::bind_rows(all_model_rows),
    best_models = dplyr::bind_rows(best_model_rows),
    scores = dplyr::bind_rows(score_rows),
    features = dplyr::bind_rows(feature_rows)
  )
}

primary_results <- evaluate_window_collection(
  primary_window_data,
  "Primary_Latest_Window_Per_Location_Stratum"
)

extended_results <- evaluate_window_collection(
  extended_window_data,
  "Extended_Nonoverlapping_Windows_Per_Location_Stratum"
)


# ------------------------------------------------------------
# 11. ASSEMBLE VALIDATION DATA
# ------------------------------------------------------------

assemble_validation_data <- function(result_object) {
  if (
    is.null(result_object$metadata) ||
    nrow(result_object$metadata) == 0L
  ) {
    return(tibble::tibble())
  }

  result_object$metadata %>%
    dplyr::left_join(
      result_object$best_models,
      by = c(
        "Dataset", "Window_ID", "Series_ID", "Disease",
        "Location_Type", "Location"
      )
    ) %>%
    dplyr::left_join(
      result_object$scores,
      by = c(
        "Dataset", "Window_ID", "Series_ID", "Disease",
        "Location_Type", "Location", "Geographic_Location_ID"
      )
    ) %>%
    dplyr::mutate(
      Observed_Readiness = dplyr::case_when(
        !is.finite(Best_MASE) ~ NA_character_,
        Best_MASE < empirical_mase_threshold ~ "Ready",
        TRUE ~ "Not_Ready"
      ),
      Observed_Readiness = factor(
        Observed_Readiness,
        levels = c("Not_Ready", "Ready")
      ),
      Score10 = EpiFORESS / 10,
      Log_Best_MASE = dplyr::if_else(
        is.finite(Best_MASE) & Best_MASE >= 0,
        log(Best_MASE),
        NA_real_
      ),
      Calibration_Predicted_Median_MASE = estimate_median_best_mase(EpiFORESS),
      Calibration_Error = Best_MASE - Calibration_Predicted_Median_MASE
    ) %>%
    dplyr::filter(
      is.finite(Best_MASE),
      Best_MASE >= 0,
      is.finite(EpiFORESS)
    )
}

primary_validation_data <- assemble_validation_data(primary_results)
extended_validation_data <- assemble_validation_data(extended_results)


# ------------------------------------------------------------
# 12. STATISTICAL VALIDATION FUNCTIONS
# ------------------------------------------------------------

bootstrap_correlation_by_cluster <- function(
    data,
    cluster_column,
    method = "spearman",
    bootstrap_replicates = 2000L
) {
  if (!cluster_column %in% names(data)) {
    return(
      tibble::tibble(
        Cluster = cluster_column,
        Method = method,
        Estimate = NA_real_,
        CI_Lower = NA_real_,
        CI_Upper = NA_real_,
        Clusters = NA_integer_,
        Bootstrap_Replicates = bootstrap_replicates,
        Valid_Bootstrap_Replicates = 0L
      )
    )
  }

  clusters <- unique(data[[cluster_column]])
  clusters <- clusters[!is.na(clusters)]

  estimate <- suppressWarnings(
    stats::cor(
      data$EpiFORESS,
      data$Best_MASE,
      method = method,
      use = "complete.obs"
    )
  )

  if (length(clusters) < 2L || nrow(data) < 3L) {
    return(
      tibble::tibble(
        Cluster = cluster_column,
        Method = method,
        Estimate = estimate,
        CI_Lower = NA_real_,
        CI_Upper = NA_real_,
        Clusters = length(clusters),
        Bootstrap_Replicates = bootstrap_replicates,
        Valid_Bootstrap_Replicates = 0L
      )
    )
  }

  bootstrap_values <- replicate(
    bootstrap_replicates,
    {
      sampled_clusters <- sample(clusters, size = length(clusters), replace = TRUE)

      sampled_data <- purrr::map2_dfr(
        sampled_clusters,
        seq_along(sampled_clusters),
        function(cluster_value, replicate_id) {
          data[data[[cluster_column]] == cluster_value, , drop = FALSE] %>%
            dplyr::mutate(.Bootstrap_Cluster = paste0(cluster_value, "__", replicate_id))
        }
      )

      suppressWarnings(
        stats::cor(
          sampled_data$EpiFORESS,
          sampled_data$Best_MASE,
          method = method,
          use = "complete.obs"
        )
      )
    }
  )

  bootstrap_values <- bootstrap_values[is.finite(bootstrap_values)]

  tibble::tibble(
    Cluster = cluster_column,
    Method = method,
    Estimate = estimate,
    CI_Lower = if (length(bootstrap_values) > 0L) {
      as.numeric(stats::quantile(bootstrap_values, 0.025, na.rm = TRUE))
    } else {
      NA_real_
    },
    CI_Upper = if (length(bootstrap_values) > 0L) {
      as.numeric(stats::quantile(bootstrap_values, 0.975, na.rm = TRUE))
    } else {
      NA_real_
    },
    Clusters = length(clusters),
    Bootstrap_Replicates = bootstrap_replicates,
    Valid_Bootstrap_Replicates = length(bootstrap_values)
  )
}

compute_auc_value <- function(data) {
  valid <- data %>%
    dplyr::filter(!is.na(Observed_Readiness), is.finite(EpiFORESS))

  if (nrow(valid) < 3L || dplyr::n_distinct(valid$Observed_Readiness) < 2L) {
    return(NA_real_)
  }

  roc_object <- tryCatch(
    pROC::roc(
      response = valid$Observed_Readiness,
      predictor = valid$EpiFORESS,
      levels = c("Not_Ready", "Ready"),
      direction = "<",
      quiet = TRUE
    ),
    error = function(e) NULL
  )

  if (is.null(roc_object)) return(NA_real_)
  as.numeric(pROC::auc(roc_object))
}

compute_auc_summary <- function(
    data,
    cluster_column = NULL,
    bootstrap_replicates = 2000L
) {
  valid <- data %>%
    dplyr::filter(!is.na(Observed_Readiness), is.finite(EpiFORESS))

  point_auc <- compute_auc_value(valid)

  basic_counts <- tibble::tibble(
    AUC = point_auc,
    N = nrow(valid),
    Ready = sum(valid$Observed_Readiness == "Ready"),
    Not_Ready = sum(valid$Observed_Readiness == "Not_Ready")
  )

  if (!is.finite(point_auc)) {
    return(
      basic_counts %>%
        dplyr::mutate(
          CI_Lower = NA_real_,
          CI_Upper = NA_real_,
          Bootstrap_Cluster = cluster_column %||% "Observation",
          Clusters = NA_integer_,
          Bootstrap_Replicates = bootstrap_replicates,
          Valid_Bootstrap_Replicates = 0L
        )
    )
  }

  if (is.null(cluster_column)) {
    cluster_values <- seq_len(nrow(valid))
    valid$.AUC_Cluster <- cluster_values
    cluster_column_internal <- ".AUC_Cluster"
    cluster_label <- "Observation"
  } else {
    cluster_column_internal <- cluster_column
    cluster_label <- cluster_column
  }

  if (!cluster_column_internal %in% names(valid)) {
    return(
      basic_counts %>%
        dplyr::mutate(
          CI_Lower = NA_real_,
          CI_Upper = NA_real_,
          Bootstrap_Cluster = cluster_label,
          Clusters = NA_integer_,
          Bootstrap_Replicates = bootstrap_replicates,
          Valid_Bootstrap_Replicates = 0L
        )
    )
  }

  clusters <- unique(valid[[cluster_column_internal]])
  clusters <- clusters[!is.na(clusters)]

  if (length(clusters) < 2L) {
    return(
      basic_counts %>%
        dplyr::mutate(
          CI_Lower = NA_real_,
          CI_Upper = NA_real_,
          Bootstrap_Cluster = cluster_label,
          Clusters = length(clusters),
          Bootstrap_Replicates = bootstrap_replicates,
          Valid_Bootstrap_Replicates = 0L
        )
    )
  }

  bootstrap_auc <- replicate(
    bootstrap_replicates,
    {
      sampled_clusters <- sample(clusters, size = length(clusters), replace = TRUE)

      sampled_data <- purrr::map2_dfr(
        sampled_clusters,
        seq_along(sampled_clusters),
        function(cluster_value, replicate_id) {
          valid[valid[[cluster_column_internal]] == cluster_value, , drop = FALSE] %>%
            dplyr::mutate(.AUC_Bootstrap_Cluster = paste0(cluster_value, "__", replicate_id))
        }
      )

      compute_auc_value(sampled_data)
    }
  )

  bootstrap_auc <- bootstrap_auc[is.finite(bootstrap_auc)]

  basic_counts %>%
    dplyr::mutate(
      CI_Lower = if (length(bootstrap_auc) > 0L) {
        as.numeric(stats::quantile(bootstrap_auc, 0.025, na.rm = TRUE))
      } else {
        NA_real_
      },
      CI_Upper = if (length(bootstrap_auc) > 0L) {
        as.numeric(stats::quantile(bootstrap_auc, 0.975, na.rm = TRUE))
      } else {
        NA_real_
      },
      Bootstrap_Cluster = cluster_label,
      Clusters = length(clusters),
      Bootstrap_Replicates = bootstrap_replicates,
      Valid_Bootstrap_Replicates = length(bootstrap_auc)
    )
}

robust_covariance_for_design <- function(model, data, covariance_mode) {
  fallback <- list(
    matrix = sandwich::vcovHC(model, type = "HC3"),
    label = "HC3 fallback"
  )

  if (covariance_mode == "two_way_disease_location") {
    if (
      dplyr::n_distinct(data$Disease) >= 2L &&
      dplyr::n_distinct(data$Geographic_Location_ID) >= 2L
    ) {
      candidate <- tryCatch(
        sandwich::vcovCL(
          model,
          cluster = data.frame(
            Disease = data$Disease,
            Geographic_Location = data$Geographic_Location_ID
          ),
          type = "HC1"
        ),
        error = function(e) NULL
      )

      if (!is.null(candidate) && all(is.finite(candidate))) {
        return(
          list(
            matrix = candidate,
            label = "Two-way clustered HC1: Disease + Geographic Location"
          )
        )
      }
    }
  }

  if (covariance_mode == "series_cluster") {
    if (dplyr::n_distinct(data$Series_ID) >= 2L) {
      candidate <- tryCatch(
        sandwich::vcovCL(
          model,
          cluster = data$Series_ID,
          type = "HC1"
        ),
        error = function(e) NULL
      )

      if (!is.null(candidate) && all(is.finite(candidate))) {
        return(
          list(
            matrix = candidate,
            label = "Series_ID-clustered HC1"
          )
        )
      }
    }
  }

  fallback
}

leave_one_group_out_sensitivity <- function(data, group_column, dataset_label) {
  groups <- sort(unique(data[[group_column]]))
  groups <- groups[!is.na(groups)]

  purrr::map_dfr(
    groups,
    function(removed_group) {
      reduced_data <- data[data[[group_column]] != removed_group, , drop = FALSE]

      if (nrow(reduced_data) < 3L) {
        return(
          tibble::tibble(
            Dataset = dataset_label,
            Group_Type = group_column,
            Removed_Group = removed_group,
            N = nrow(reduced_data),
            Pearson_r = NA_real_,
            Spearman_rho = NA_real_,
            LogLinear_Slope_per_10 = NA_real_,
            Percent_Change_per_10 = NA_real_,
            R_Squared = NA_real_
          )
        )
      }

      model <- stats::lm(Log_Best_MASE ~ Score10, data = reduced_data)
      slope <- as.numeric(stats::coef(model)["Score10"])

      tibble::tibble(
        Dataset = dataset_label,
        Group_Type = group_column,
        Removed_Group = removed_group,
        N = nrow(reduced_data),
        Pearson_r = suppressWarnings(
          stats::cor(
            reduced_data$EpiFORESS,
            reduced_data$Best_MASE,
            method = "pearson",
            use = "complete.obs"
          )
        ),
        Spearman_rho = suppressWarnings(
          stats::cor(
            reduced_data$EpiFORESS,
            reduced_data$Best_MASE,
            method = "spearman",
            use = "complete.obs"
          )
        ),
        LogLinear_Slope_per_10 = slope,
        Percent_Change_per_10 = 100 * (exp(slope) - 1),
        R_Squared = summary(model)$r.squared
      )
    }
  )
}

analyze_validation_dataset <- function(
    data,
    dataset_label,
    covariance_mode,
    bootstrap_cluster_columns
) {
  if (nrow(data) < 3L) {
    warning("Fewer than three valid observations for ", dataset_label)
    return(NULL)
  }

  pearson_test <- tryCatch(
    stats::cor.test(data$EpiFORESS, data$Best_MASE, method = "pearson"),
    error = function(e) NULL
  )

  spearman_test <- tryCatch(
    suppressWarnings(
      stats::cor.test(
        data$EpiFORESS,
        data$Best_MASE,
        method = "spearman",
        exact = FALSE
      )
    ),
    error = function(e) NULL
  )

  correlation_summary <- tibble::tibble(
    Dataset = dataset_label,
    Method = c("Pearson", "Spearman"),
    Correlation = c(
      if (is.null(pearson_test)) NA_real_ else unname(pearson_test$estimate),
      if (is.null(spearman_test)) NA_real_ else unname(spearman_test$estimate)
    ),
    P_Value_Unadjusted = c(
      if (is.null(pearson_test)) NA_real_ else pearson_test$p.value,
      if (is.null(spearman_test)) NA_real_ else spearman_test$p.value
    ),
    N = nrow(data)
  )

  cluster_bootstrap_correlations <- purrr::map_dfr(
    bootstrap_cluster_columns,
    function(cluster_column) {
      dplyr::bind_rows(
        bootstrap_correlation_by_cluster(
          data,
          cluster_column = cluster_column,
          method = "pearson",
          bootstrap_replicates = bootstrap_replicates
        ),
        bootstrap_correlation_by_cluster(
          data,
          cluster_column = cluster_column,
          method = "spearman",
          bootstrap_replicates = bootstrap_replicates
        )
      ) %>%
        dplyr::mutate(Dataset = dataset_label, .before = 1)
    }
  )

  log_linear_model <- stats::lm(Log_Best_MASE ~ Score10, data = data)
  covariance_result <- robust_covariance_for_design(
    log_linear_model,
    data,
    covariance_mode
  )

  robust_coefficient_test <- lmtest::coeftest(
    log_linear_model,
    vcov. = covariance_result$matrix
  )

  robust_coefficient_table <- tibble::tibble(
    Dataset = dataset_label,
    Term = rownames(robust_coefficient_test),
    Estimate = robust_coefficient_test[, "Estimate"],
    Robust_SE = robust_coefficient_test[, "Std. Error"],
    Statistic = robust_coefficient_test[, 3],
    P_Value = robust_coefficient_test[, 4],
    Covariance = covariance_result$label
  ) %>%
    dplyr::mutate(
      Percent_Change_per_10_Points = dplyr::if_else(
        Term == "Score10",
        100 * (exp(Estimate) - 1),
        NA_real_
      )
    )

  model_fit_summary <- broom::glance(log_linear_model) %>%
    dplyr::mutate(Dataset = dataset_label, .before = 1)

  quantile_models <- purrr::map_dfr(
    c(0.50, 0.75),
    function(tau_value) {
      model <- tryCatch(
        quantreg::rq(
          Log_Best_MASE ~ Score10,
          tau = tau_value,
          data = data
        ),
        error = function(e) NULL
      )

      if (is.null(model)) {
        return(
          tibble::tibble(
            Dataset = dataset_label,
            Tau = tau_value,
            Intercept = NA_real_,
            Score10_Slope = NA_real_,
            Percent_Change_per_10_Points = NA_real_,
            MASE_1_Crossing_Score = NA_real_,
            Inference = "Point estimates only; no cluster-adjusted standard errors"
          )
        )
      }

      coefficients <- stats::coef(model)
      intercept <- as.numeric(coefficients["(Intercept)"])
      slope <- as.numeric(coefficients["Score10"])

      crossing_score <- if (
        is.finite(intercept) && is.finite(slope) && abs(slope) > 1e-12
      ) {
        -intercept / slope * 10
      } else {
        NA_real_
      }

      tibble::tibble(
        Dataset = dataset_label,
        Tau = tau_value,
        Intercept = intercept,
        Score10_Slope = slope,
        Percent_Change_per_10_Points = 100 * (exp(slope) - 1),
        MASE_1_Crossing_Score = crossing_score,
        Inference = "Point estimates only; no cluster-adjusted standard errors"
      )
    }
  )

  auc_summaries <- purrr::map_dfr(
    c(list(NULL), as.list(bootstrap_cluster_columns)),
    function(cluster_column) {
      compute_auc_summary(
        data,
        cluster_column = cluster_column,
        bootstrap_replicates = bootstrap_replicates
      )
    }
  ) %>%
    dplyr::mutate(
      Dataset = dataset_label,
      Criterion = "Ready if Best MASE < 1.00",
      .before = 1
    )

  calibration_accuracy <- tibble::tibble(
    Dataset = dataset_label,
    N = nrow(data),
    Series = dplyr::n_distinct(data$Series_ID),
    Diseases = dplyr::n_distinct(data$Disease),
    Geographic_Locations = dplyr::n_distinct(data$Geographic_Location_ID),
    MAE = mean(abs(data$Calibration_Error), na.rm = TRUE),
    RMSE = sqrt(mean(data$Calibration_Error^2, na.rm = TRUE)),
    Median_Absolute_Error = stats::median(abs(data$Calibration_Error), na.rm = TRUE),
    Mean_Error = mean(data$Calibration_Error, na.rm = TRUE),
    Spearman_Observed_vs_Predicted = suppressWarnings(
      stats::cor(
        data$Best_MASE,
        data$Calibration_Predicted_Median_MASE,
        method = "spearman",
        use = "complete.obs"
      )
    )
  )

  calibration_regression <- tryCatch(
    stats::lm(
      log(Best_MASE) ~ log(Calibration_Predicted_Median_MASE),
      data = data
    ),
    error = function(e) NULL
  )

  calibration_regression_table <- if (is.null(calibration_regression)) {
    tibble::tibble()
  } else {
    calibration_covariance <- robust_covariance_for_design(
      calibration_regression,
      data,
      covariance_mode
    )

    calibration_test <- lmtest::coeftest(
      calibration_regression,
      vcov. = calibration_covariance$matrix
    )

    tibble::tibble(
      Dataset = dataset_label,
      Term = rownames(calibration_test),
      Estimate = calibration_test[, "Estimate"],
      Robust_SE = calibration_test[, "Std. Error"],
      Statistic = calibration_test[, 3],
      P_Value = calibration_test[, 4],
      CI_Lower = calibration_test[, "Estimate"] -
        stats::qnorm(0.975) * calibration_test[, "Std. Error"],
      CI_Upper = calibration_test[, "Estimate"] +
        stats::qnorm(0.975) * calibration_test[, "Std. Error"],
      Covariance = calibration_covariance$label
    )
  }

  band_summary <- data %>%
    dplyr::mutate(
      EpiFORESS_Band = factor(
        readiness_band(EpiFORESS),
        levels = c("Very_Low", "Low", "Moderate", "High", "Very_High"),
        ordered = TRUE
      )
    ) %>%
    dplyr::group_by(EpiFORESS_Band) %>%
    dplyr::summarise(
      Dataset = dataset_label,
      N = dplyr::n(),
      Series = dplyr::n_distinct(Series_ID),
      Diseases = dplyr::n_distinct(Disease),
      Geographic_Locations = dplyr::n_distinct(Geographic_Location_ID),
      Mean_Score = mean(EpiFORESS, na.rm = TRUE),
      Median_Score = stats::median(EpiFORESS, na.rm = TRUE),
      Mean_Best_MASE = mean(Best_MASE, na.rm = TRUE),
      Median_Best_MASE = stats::median(Best_MASE, na.rm = TRUE),
      Q1_Best_MASE = as.numeric(stats::quantile(Best_MASE, 0.25, na.rm = TRUE)),
      Q3_Best_MASE = as.numeric(stats::quantile(Best_MASE, 0.75, na.rm = TRUE)),
      Ready_Count = sum(Best_MASE < empirical_mase_threshold, na.rm = TRUE),
      Ready_Rate = Ready_Count / N,
      .groups = "drop"
    )

  kruskal_result <- tryCatch(
    stats::kruskal.test(
      Best_MASE ~ factor(
        readiness_band(EpiFORESS),
        levels = c("Very_Low", "Low", "Moderate", "High", "Very_High"),
        ordered = TRUE
      ),
      data = data
    ),
    error = function(e) NULL
  )

  kruskal_summary <- tibble::tibble(
    Dataset = dataset_label,
    Test = "Kruskal-Wallis; unadjusted for clustering",
    Statistic = if (is.null(kruskal_result)) NA_real_ else unname(kruskal_result$statistic),
    DF = if (is.null(kruskal_result)) NA_real_ else unname(kruskal_result$parameter),
    P_Value = if (is.null(kruskal_result)) NA_real_ else kruskal_result$p.value
  )

  jt_data <- data %>%
    dplyr::mutate(
      Band = factor(
        readiness_band(EpiFORESS),
        levels = c("Very_Low", "Low", "Moderate", "High", "Very_High"),
        ordered = TRUE
      ),
      Band_Order = as.numeric(Band)
    ) %>%
    dplyr::filter(!is.na(Band_Order), is.finite(Best_MASE))

  jt_result <- tryCatch(
    {
      if (nrow(jt_data) < 3L || dplyr::n_distinct(jt_data$Band_Order) < 2L) {
        stop("Insufficient observed readiness bands.")
      }

      clinfun::jonckheere.test(
        x = jt_data$Best_MASE,
        g = jt_data$Band_Order,
        alternative = "decreasing"
      )
    },
    error = function(e) e
  )

  jt_summary <- if (inherits(jt_result, "htest")) {
    tibble::tibble(
      Dataset = dataset_label,
      Test = "Jonckheere-Terpstra; unadjusted for clustering",
      Alternative = "Best MASE decreases as readiness band increases",
      Statistic = unname(jt_result$statistic),
      P_Value = jt_result$p.value,
      N = nrow(jt_data),
      Bands_Observed = dplyr::n_distinct(jt_data$Band_Order)
    )
  } else {
    tibble::tibble(
      Dataset = dataset_label,
      Test = "Jonckheere-Terpstra; unadjusted for clustering",
      Alternative = "Best MASE decreases as readiness band increases",
      Statistic = NA_real_,
      P_Value = NA_real_,
      N = nrow(jt_data),
      Bands_Observed = dplyr::n_distinct(jt_data$Band_Order),
      Error = conditionMessage(jt_result)
    )
  }

  leave_one_disease_out <- leave_one_group_out_sensitivity(
    data,
    group_column = "Disease",
    dataset_label = dataset_label
  )

  leave_one_location_out <- leave_one_group_out_sensitivity(
    data,
    group_column = "Geographic_Location_ID",
    dataset_label = dataset_label
  )

  disease_clusters <- dplyr::n_distinct(data$Disease)
  location_clusters <- dplyr::n_distinct(data$Geographic_Location_ID)
  series_clusters <- dplyr::n_distinct(data$Series_ID)

  inference_diagnostics <- tibble::tibble(
    Dataset = dataset_label,
    N = nrow(data),
    Diseases = disease_clusters,
    Geographic_Locations = location_clusters,
    Series = series_clusters,
    Requested_Covariance = covariance_mode,
    Applied_Covariance = covariance_result$label,
    Small_Cluster_Caution = dplyr::case_when(
      covariance_mode == "two_way_disease_location" && disease_clusters < 30L ~
        "Disease-cluster count is below 30; treat asymptotic cluster-robust p-values cautiously.",
      covariance_mode == "series_cluster" && series_clusters < 30L ~
        "Series-cluster count is below 30; treat asymptotic cluster-robust p-values cautiously.",
      TRUE ~ "No automatic small-cluster warning triggered."
    )
  )

  list(
    data = data,
    inference_diagnostics = inference_diagnostics,
    correlation = correlation_summary,
    cluster_bootstrap_correlations = cluster_bootstrap_correlations,
    robust_loglinear_coefficients = robust_coefficient_table,
    loglinear_fit = model_fit_summary,
    quantile_regression = quantile_models,
    auc = auc_summaries,
    calibration_accuracy = calibration_accuracy,
    calibration_regression = calibration_regression_table,
    band_summary = band_summary,
    kruskal = kruskal_summary,
    jonckheere = jt_summary,
    leave_one_disease_out = leave_one_disease_out,
    leave_one_location_out = leave_one_location_out
  )
}

primary_analysis <- analyze_validation_dataset(
  primary_validation_data,
  dataset_label = "Primary latest 260-week window per Disease x Location x Location Type",
  covariance_mode = "two_way_disease_location",
  bootstrap_cluster_columns = c("Disease", "Geographic_Location_ID")
)

extended_analysis <- if (nrow(extended_validation_data) >= 3L) {
  analyze_validation_dataset(
    extended_validation_data,
    dataset_label = "Extended non-overlapping 260-week windows per location-stratified series",
    covariance_mode = "series_cluster",
    bootstrap_cluster_columns = c("Series_ID", "Disease", "Geographic_Location_ID")
  )
} else {
  NULL
}


# ------------------------------------------------------------
# 13. DESCRIPTIVE SUMMARIES
# ------------------------------------------------------------

data_profile <- tibble::tibble(
  Item = c(
    "Raw source rows",
    "Rows after standardization and optional filters",
    "Diseases in retained source",
    "Geographic locations in retained source",
    "Location types in retained source",
    "Disease x Location x Location Type series",
    "Completed location-stratified weekly rows",
    "Primary eligible location-stratified windows",
    "Extended eligible location-stratified windows",
    "Minimum window observed fraction",
    "Minimum training observed fraction",
    "Window length",
    "Seasonal frequency",
    "Forecast horizon",
    "Initial rolling-origin window",
    "Rolling step",
    "Maximum origins",
    "Bootstrap replicates",
    "Week 53 policy",
    "Source Week 53 rows before policy",
    "Scoring-function capture",
    "Observed readiness criterion"
  ),
  Value = as.character(
    c(
      nrow(tycho_raw),
      nrow(tycho),
      dplyr::n_distinct(tycho$Disease),
      dplyr::n_distinct(paste(tycho$Location_Type, tycho$Location)),
      dplyr::n_distinct(tycho$Location_Type),
      nrow(series_index),
      nrow(location_series_data),
      dplyr::n_distinct(primary_window_data$Window_ID),
      dplyr::n_distinct(extended_window_data$Window_ID),
      minimum_window_observed_fraction,
      minimum_training_observed_fraction,
      window_length,
      seasonal_frequency,
      forecast_horizon,
      initial_window,
      rolling_step,
      max_origins,
      bootstrap_replicates,
      week_53_policy,
      source_week_53_rows,
      scoring_capture_method,
      "Ready if Best MASE < 1.00"
    )
  )
)

location_type_summary <- window_eligibility_table %>%
  dplyr::group_by(Location_Type) %>%
  dplyr::summarise(
    Candidate_Series = dplyr::n(),
    Diseases = dplyr::n_distinct(Disease),
    Locations = dplyr::n_distinct(Location),
    Primary_Eligible_Series = sum(Primary_Eligible),
    Primary_Eligibility_Rate = mean(Primary_Eligible),
    Extended_Eligible_Windows = sum(Extended_Eligible_Windows),
    Median_Observed_Fraction = stats::median(Observed_Fraction, na.rm = TRUE),
    .groups = "drop"
  )

disease_location_summary <- window_eligibility_table %>%
  dplyr::group_by(Disease) %>%
  dplyr::summarise(
    Candidate_Location_Strata = dplyr::n(),
    Distinct_Geographic_Locations = dplyr::n_distinct(Geographic_Location_ID),
    Location_Types = paste(sort(unique(Location_Type)), collapse = ", "),
    Primary_Eligible_Series = sum(Primary_Eligible),
    Primary_Eligibility_Rate = mean(Primary_Eligible),
    Extended_Eligible_Windows = sum(Extended_Eligible_Windows),
    Median_Observed_Fraction = stats::median(Observed_Fraction, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(Primary_Eligible_Series), Disease)

location_performance_summary <- dplyr::bind_rows(
  primary_validation_data %>% dplyr::mutate(Analysis = "Primary"),
  extended_validation_data %>% dplyr::mutate(Analysis = "Extended")
) %>%
  dplyr::group_by(Analysis, Geographic_Location_ID, Location_Type, Location) %>%
  dplyr::summarise(
    N_Windows = dplyr::n(),
    Diseases = dplyr::n_distinct(Disease),
    Mean_EpiFORESS = mean(EpiFORESS, na.rm = TRUE),
    Median_EpiFORESS = stats::median(EpiFORESS, na.rm = TRUE),
    Mean_Best_MASE = mean(Best_MASE, na.rm = TRUE),
    Median_Best_MASE = stats::median(Best_MASE, na.rm = TRUE),
    Ready_Rate = mean(Best_MASE < empirical_mase_threshold, na.rm = TRUE),
    .groups = "drop"
  )

forecast_model_summary <- dplyr::bind_rows(
  primary_results$all_models,
  extended_results$all_models
) %>%
  dplyr::group_by(Dataset, Model) %>%
  dplyr::summarise(
    N_Windows = sum(is.finite(MASE)),
    Mean_MASE = mean(MASE, na.rm = TRUE),
    Median_MASE = stats::median(MASE, na.rm = TRUE),
    SD_MASE = stats::sd(MASE, na.rm = TRUE),
    Mean_MAE = mean(MAE, na.rm = TRUE),
    Mean_RMSE = mean(RMSE, na.rm = TRUE),
    Mean_Bias = mean(Bias, na.rm = TRUE),
    Ready_Rate = mean(MASE < empirical_mase_threshold, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Dataset, Mean_MASE)

winning_model_distribution <- dplyr::bind_rows(
  primary_results$best_models,
  extended_results$best_models
) %>%
  dplyr::count(Dataset, Best_Model, name = "Win_Count") %>%
  dplyr::group_by(Dataset) %>%
  dplyr::mutate(Win_Percent = 100 * Win_Count / sum(Win_Count)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Dataset, dplyr::desc(Win_Count))


# ------------------------------------------------------------
# 14. PLOTS
# ------------------------------------------------------------

if (!is.null(primary_analysis)) {
  primary_plot <- ggplot2::ggplot(
    primary_analysis$data,
    ggplot2::aes(x = EpiFORESS, y = Best_MASE)
  ) +
    ggplot2::geom_hline(
      yintercept = empirical_mase_threshold,
      linetype = "dashed",
      linewidth = 0.7,
      color = "#6B7280"
    ) +
    ggplot2::geom_smooth(
      method = "lm",
      se = TRUE,
      color = "#143A6B",
      fill = "#DCE6F5",
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        color = Observed_Readiness,
        shape = Location_Type
      ),
      size = 2.7,
      alpha = 0.78
    ) +
    ggplot2::scale_color_manual(
      values = c(Not_Ready = "#B77500", Ready = "#1D4E9E"),
      drop = FALSE
    ) +
    ggplot2::labs(
      title = "Project Tycho Location-Stratified External Validation",
      subtitle = "Latest eligible 260-week window per Disease x Location x Location Type series",
      x = "EpiFORESS score",
      y = "Observed Best MASE",
      color = "Best MASE < 1.00",
      shape = "Location type"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold")
    )

  ggplot2::ggsave(
    primary_scatter_path,
    primary_plot,
    width = 10,
    height = 6.5,
    dpi = 300
  )
}

if (!is.null(extended_analysis)) {
  extended_plot <- ggplot2::ggplot(
    extended_analysis$data,
    ggplot2::aes(
      x = EpiFORESS,
      y = Best_MASE,
      group = Series_ID
    )
  ) +
    ggplot2::geom_hline(
      yintercept = empirical_mase_threshold,
      linetype = "dashed",
      linewidth = 0.7,
      color = "#6B7280"
    ) +
    ggplot2::geom_line(alpha = 0.16, color = "#64748B") +
    ggplot2::geom_point(
      ggplot2::aes(
        color = Observed_Readiness,
        shape = Location_Type
      ),
      size = 2,
      alpha = 0.72
    ) +
    ggplot2::geom_smooth(
      ggplot2::aes(group = 1),
      method = "lm",
      se = TRUE,
      color = "#143A6B",
      fill = "#DCE6F5",
      linewidth = 0.8
    ) +
    ggplot2::scale_color_manual(
      values = c(Not_Ready = "#B77500", Ready = "#1D4E9E"),
      drop = FALSE
    ) +
    ggplot2::labs(
      title = "Extended Location-Stratified Project Tycho Validation",
      subtitle = "Non-overlapping windows; lines connect windows from the same Disease x Location series",
      x = "EpiFORESS score",
      y = "Observed Best MASE",
      color = "Best MASE < 1.00",
      shape = "Location type"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold")
    )

  ggplot2::ggsave(
    extended_scatter_path,
    extended_plot,
    width = 10,
    height = 6.5,
    dpi = 300
  )
}

combined_validation_for_bands <- dplyr::bind_rows(
  primary_validation_data %>% dplyr::mutate(Analysis = "Primary"),
  extended_validation_data %>% dplyr::mutate(Analysis = "Extended")
)

if (nrow(combined_validation_for_bands) > 0L) {
  combined_validation_for_bands <- combined_validation_for_bands %>%
    dplyr::mutate(
      EpiFORESS_Band = factor(
        readiness_band(EpiFORESS),
        levels = c("Very_Low", "Low", "Moderate", "High", "Very_High"),
        ordered = TRUE
      )
    )

  band_plot <- ggplot2::ggplot(
    combined_validation_for_bands,
    ggplot2::aes(x = EpiFORESS_Band, y = Best_MASE, fill = Analysis)
  ) +
    ggplot2::geom_hline(
      yintercept = empirical_mase_threshold,
      linetype = "dashed",
      color = "#6B7280"
    ) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.70) +
    ggplot2::geom_jitter(
      ggplot2::aes(color = Analysis),
      width = 0.12,
      alpha = 0.35,
      size = 1.2
    ) +
    ggplot2::scale_fill_manual(values = c(Primary = "#1D4E9E", Extended = "#C99A2E")) +
    ggplot2::scale_color_manual(values = c(Primary = "#143A6B", Extended = "#9C7818")) +
    ggplot2::labs(
      title = "Observed Best MASE Across EpiFORESS Readiness Bands",
      subtitle = "Location-stratified Project Tycho series",
      x = "Descriptive readiness band",
      y = "Best MASE",
      fill = "Analysis",
      color = "Analysis"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold")
    )

  ggplot2::ggsave(
    band_plot_path,
    band_plot,
    width = 9,
    height = 6,
    dpi = 300
  )

  calibration_plot <- ggplot2::ggplot(
    combined_validation_for_bands,
    ggplot2::aes(
      x = Calibration_Predicted_Median_MASE,
      y = Best_MASE,
      color = Analysis,
      shape = Location_Type
    )
  ) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      color = "#6B7280"
    ) +
    ggplot2::geom_point(size = 2.1, alpha = 0.65) +
    ggplot2::scale_color_manual(values = c(Primary = "#1D4E9E", Extended = "#C99A2E")) +
    ggplot2::labs(
      title = "External Check of the Score-Calibrated Median Best MASE",
      subtitle = "Location-stratified series; dashed line represents perfect agreement",
      x = "Score-calibrated median Best MASE",
      y = "Observed Best MASE",
      color = "Analysis",
      shape = "Location type"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold")
    )

  ggplot2::ggsave(
    calibration_plot_path,
    calibration_plot,
    width = 9,
    height = 6,
    dpi = 300
  )
}

coverage_plot_data <- disease_location_summary %>%
  dplyr::arrange(Primary_Eligible_Series) %>%
  dplyr::mutate(Disease = factor(Disease, levels = Disease))

coverage_plot <- ggplot2::ggplot(
  coverage_plot_data,
  ggplot2::aes(x = Disease, y = Primary_Eligible_Series)
) +
  ggplot2::geom_col(fill = "#1D4E9E") +
  ggplot2::geom_text(
    ggplot2::aes(
      label = paste0(
        Primary_Eligible_Series,
        "/",
        Candidate_Location_Strata
      )
    ),
    hjust = -0.08,
    size = 3.1
  ) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Eligible Location-Stratified Project Tycho Series",
    subtitle = "Labels show primary-eligible series divided by candidate location strata",
    x = "Disease",
    y = "Primary eligible location-stratified series"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

ggplot2::ggsave(
  coverage_plot_path,
  coverage_plot,
  width = 10,
  height = 7,
  dpi = 300
)


# ------------------------------------------------------------
# 15. OUTPUT TABLES
# ------------------------------------------------------------

output_tables <- list(
  README = tibble::tibble(
    Section = c(
      "Validation unit",
      "Primary validation",
      "Extended validation",
      "Observed readiness",
      "Location stratification",
      "Missing reporting",
      "Duplicate rows",
      "Week 53 policy",
      "Score engine",
      "Scoring capture",
      "Primary inference",
      "Extended inference",
      "Cluster bootstrap",
      "Large weekly tables",
      "Readiness bands"
    ),
    Description = c(
      "One series is one distinct Disease x Location x Location Type combination.",
      "The latest eligible 260-week window for every location-stratified disease series.",
      "All eligible recent-aligned non-overlapping 260-week windows within each location-stratified disease series.",
      "Ready when Best MASE is strictly below 1.00; the EpiFORESS score remains continuous.",
      "Counts are never summed across different locations or location types.",
      "Absent location-week rows remain missing, not zero. Window eligibility requires the configured observed fraction.",
      "Rows are summed only when they duplicate the exact same disease, location type, location, year, and week.",
      "The primary specification excludes Week 53 to preserve a regular 52-week calendar. retain_as_observed is available as a sensitivity setting.",
      "Scores are generated by score_one_series() sourced from the deployed EpiFORESS app.",
      paste0("Capture method: ", scoring_capture_method, ". ", scoring_capture_note),
      "Log-linear inference uses two-way clustered HC1 covariance by Disease and Geographic Location, with HC3 fallback.",
      "Log-linear inference uses Series_ID-clustered HC1 covariance for repeated windows, with HC3 fallback.",
      "Correlation and AUC intervals are recomputed using disease, geographic-location, or series-level cluster resampling as appropriate.",
      "Complete weekly and window-level data are always saved in the RDS. Excel inclusion is controlled by include_large_weekly_tables_in_workbook and oversized sheets are split automatically.",
      "Very Low 0-19; Low 20-34; Moderate 35-59; High 60-79; Very High 80-100."
    )
  ),
  Data_Profile = data_profile,
  Series_Index = series_index,
  Location_Series_Profile = location_series_profile,
  Window_Eligibility = window_eligibility_table,
  Location_Type_Summary = location_type_summary,
  Disease_Location_Summary = disease_location_summary,
  Location_Performance = location_performance_summary,
  Primary_Metadata = primary_results$metadata,
  Primary_All_Models = primary_results$all_models,
  Primary_Best_Models = primary_results$best_models,
  Primary_Scores = primary_results$scores,
  Primary_Features = primary_results$features,
  Primary_Validation = primary_validation_data,
  Extended_Metadata = extended_results$metadata,
  Extended_All_Models = extended_results$all_models,
  Extended_Best_Models = extended_results$best_models,
  Extended_Scores = extended_results$scores,
  Extended_Features = extended_results$features,
  Extended_Validation = extended_validation_data,
  Forecast_Model_Summary = forecast_model_summary,
  Winning_Model_Distribution = winning_model_distribution
)

if (isTRUE(include_large_weekly_tables_in_workbook)) {
  output_tables <- c(
    output_tables,
    list(
      Location_Series_Data = location_series_data,
      Primary_Window_Data = primary_window_data,
      Extended_Window_Data = extended_window_data
    )
  )
}

if (!is.null(primary_analysis)) {
  output_tables <- c(
    output_tables,
    list(
      Primary_Inference_Diagnostics = primary_analysis$inference_diagnostics,
      Primary_Correlation = primary_analysis$correlation,
      Primary_Cluster_Bootstrap = primary_analysis$cluster_bootstrap_correlations,
      Primary_LogLinear = primary_analysis$robust_loglinear_coefficients,
      Primary_LogLinear_Fit = primary_analysis$loglinear_fit,
      Primary_Quantile_Regression = primary_analysis$quantile_regression,
      Primary_AUC = primary_analysis$auc,
      Primary_Calibration_Accuracy = primary_analysis$calibration_accuracy,
      Primary_Calibration_Regression = primary_analysis$calibration_regression,
      Primary_Band_Summary = primary_analysis$band_summary,
      Primary_Kruskal = primary_analysis$kruskal,
      Primary_Jonckheere = primary_analysis$jonckheere,
      Primary_LODO_Sensitivity = primary_analysis$leave_one_disease_out,
      Primary_LOLO_Sensitivity = primary_analysis$leave_one_location_out
    )
  )
}

if (!is.null(extended_analysis)) {
  output_tables <- c(
    output_tables,
    list(
      Extended_Inference_Diagnostics = extended_analysis$inference_diagnostics,
      Extended_Correlation = extended_analysis$correlation,
      Extended_Cluster_Bootstrap = extended_analysis$cluster_bootstrap_correlations,
      Extended_LogLinear = extended_analysis$robust_loglinear_coefficients,
      Extended_LogLinear_Fit = extended_analysis$loglinear_fit,
      Extended_Quantile_Regression = extended_analysis$quantile_regression,
      Extended_AUC = extended_analysis$auc,
      Extended_Calibration_Accuracy = extended_analysis$calibration_accuracy,
      Extended_Calibration_Regression = extended_analysis$calibration_regression,
      Extended_Band_Summary = extended_analysis$band_summary,
      Extended_Kruskal = extended_analysis$kruskal,
      Extended_Jonckheere = extended_analysis$jonckheere,
      Extended_LODO_Sensitivity = extended_analysis$leave_one_disease_out,
      Extended_LOLO_Sensitivity = extended_analysis$leave_one_location_out
    )
  )
}

write_tables_to_workbook(output_tables, validation_workbook_path)


# ------------------------------------------------------------
# 16. SAVE COMPLETE R OBJECT
# ------------------------------------------------------------

validation_object <- list(
  settings = list(
    base_path = base_path,
    tycho_csv_path = tycho_csv_path,
    app_file = app_file,
    stat_model_path = stat_model_path,
    ml_model_path = ml_model_path,
    disease_filter = disease_filter,
    location_filter = location_filter,
    location_type_filter = location_type_filter,
    include_large_weekly_tables_in_workbook = include_large_weekly_tables_in_workbook,
    validation_unit = "Disease x Location x Location_Type",
    week_53_policy = week_53_policy,
    source_week_53_rows_before_policy = source_week_53_rows,
    scoring_capture_method = scoring_capture_method,
    scoring_capture_note = scoring_capture_note,
    minimum_window_observed_fraction = minimum_window_observed_fraction,
    minimum_training_observed_fraction = minimum_training_observed_fraction,
    minimum_window_observed_weeks = minimum_window_observed_weeks,
    window_length = window_length,
    seasonal_frequency = seasonal_frequency,
    forecast_horizon = forecast_horizon,
    initial_window = initial_window,
    rolling_step = rolling_step,
    max_origins = max_origins,
    bootstrap_replicates = bootstrap_replicates,
    empirical_mase_threshold = empirical_mase_threshold
  ),
  series_index = series_index,
  location_series_profile = location_series_profile,
  window_eligibility = window_eligibility_table,
  location_type_summary = location_type_summary,
  disease_location_summary = disease_location_summary,
  location_performance_summary = location_performance_summary,
  location_series_data = location_series_data,
  primary_window_data = primary_window_data,
  extended_window_data = extended_window_data,
  primary_results = primary_results,
  extended_results = extended_results,
  primary_validation_data = primary_validation_data,
  extended_validation_data = extended_validation_data,
  primary_analysis = primary_analysis,
  extended_analysis = extended_analysis,
  forecast_model_summary = forecast_model_summary,
  winning_model_distribution = winning_model_distribution,
  created_at = Sys.time()
)

saveRDS(validation_object, validation_rds_path)


# ------------------------------------------------------------
# 17. CONSOLE SUMMARY
# ------------------------------------------------------------

cat(
  "\n============================================================\n",
  "PROJECT TYCHO LOCATION-STRATIFIED EPIFORESS VALIDATION COMPLETE\n",
  "============================================================\n",
  sep = ""
)

cat("\nSource CSV:\n  ", tycho_csv_path, "\n", sep = "")
cat("\nEpiFORESS app scoring engine:\n  ", app_file, "\n", sep = "")
cat("Scoring-function capture: ", scoring_capture_method, "\n", sep = "")
cat("Week 53 policy: ", week_53_policy, "\n", sep = "")

cat(
  "\nLocation-stratified series constructed: ",
  dplyr::n_distinct(location_series_data$Series_ID),
  "\n",
  sep = ""
)

cat(
  "Diseases represented: ",
  dplyr::n_distinct(location_series_data$Disease),
  "\n",
  sep = ""
)

cat(
  "Geographic locations represented: ",
  dplyr::n_distinct(location_series_data$Geographic_Location_ID),
  "\n",
  sep = ""
)

cat(
  "Primary eligible windows: ",
  dplyr::n_distinct(primary_window_data$Window_ID),
  "\n",
  sep = ""
)

cat(
  "Extended eligible windows: ",
  dplyr::n_distinct(extended_window_data$Window_ID),
  "\n",
  sep = ""
)

if (!is.null(primary_analysis)) {
  cat("\nPRIMARY LOCATION-STRATIFIED CONTINUOUS VALIDATION\n")
  print(primary_analysis$inference_diagnostics)
  print(primary_analysis$correlation)

  cat("\nPrimary cluster-bootstrap correlations:\n")
  print(primary_analysis$cluster_bootstrap_correlations)

  cat("\nPrimary two-way cluster-robust log-linear coefficients:\n")
  print(primary_analysis$robust_loglinear_coefficients)

  cat("\nPrimary quantile-regression calibration:\n")
  print(primary_analysis$quantile_regression)

  cat("\nPrimary AUC for Best MASE < 1.00:\n")
  print(primary_analysis$auc)

  cat("\nPrimary score-calibrated Best MASE accuracy:\n")
  print(primary_analysis$calibration_accuracy)
}

if (!is.null(extended_analysis)) {
  cat("\nEXTENDED LOCATION-STRATIFIED TEMPORAL VALIDATION\n")
  print(extended_analysis$inference_diagnostics)
  print(extended_analysis$correlation)

  cat("\nExtended cluster-bootstrap correlations:\n")
  print(extended_analysis$cluster_bootstrap_correlations)

  cat("\nExtended Series_ID-clustered log-linear coefficients:\n")
  print(extended_analysis$robust_loglinear_coefficients)

  cat("\nExtended quantile-regression calibration:\n")
  print(extended_analysis$quantile_regression)

  cat("\nExtended AUC for Best MASE < 1.00:\n")
  print(extended_analysis$auc)
}

cat("\nOutput workbook:\n  ", validation_workbook_path, "\n", sep = "")
cat("\nOutput RDS:\n  ", validation_rds_path, "\n", sep = "")

cat(
  "\nPlots:\n",
  "  ", primary_scatter_path, "\n",
  "  ", extended_scatter_path, "\n",
  "  ", band_plot_path, "\n",
  "  ", calibration_plot_path, "\n",
  "  ", coverage_plot_path, "\n",
  sep = ""
)

cat(
  "\n============================================================\n",
  "SCRIPT COMPLETE\n",
  "============================================================\n",
  sep = ""
)
