# EpiFORESS Analysis Pipeline
# Script: 03_forecasting_experiment.R
# Project root: set EPIFORESS_PROJECT_DIR or run from the project data directory.
# Reproducibility: inspect all settings and verify input/model compatibility before reporting results.

# ============================================================
# EPIFORESS: FORECASTING EXPERIMENT AND READINESS LABELS
# Purpose: Evaluate candidate forecasting methods using rolling-origin
#          validation and generate series-level forecast-readiness labels.
# Author: John Aaron B. Llobrera
# ============================================================

# IMPORTANT:
# - This script evaluates forecast performance; it does not train the
#   EpiFORESS readiness classifier.
# - A series is labeled only when at least one valid MASE is available.
# - Input simulation data are never overwritten. Outputs are saved separately.
# - Prophet is optional and disabled for the primary 10,000-series run.

# ------------------------------------------------------------
# 0. USER SETTINGS
# ------------------------------------------------------------

run_prophet <- FALSE

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
output_dir <- dirname(workbook_path)
output_workbook <- file.path(output_dir, "EpiFORESS_Forecasting_Results.xlsx")

freq <- 52L
h <- 13L
initial_window <- 156L
rolling_step <- 13L
max_origins <- 6L
ready_mase_threshold <- 1.00  # Primary criterion; 0.50 is evaluated as a sensitivity label downstream.
series_timeout <- 120
negbin_fourier_order <- 3L
prophet_start_date <- as.Date("2000-01-03")
prophet_fourier_order <- 5L
random_seed <- 2026L

set.seed(random_seed)

# ------------------------------------------------------------
# 1. PACKAGES
# ------------------------------------------------------------

packages <- c("readxl", "dplyr", "purrr", "forecast", "openxlsx", "tibble", "R.utils", "MASS")
if (isTRUE(run_prophet)) packages <- c(packages, "prophet")
missing_packages <- setdiff(packages, rownames(installed.packages()))
if (length(missing_packages)) install.packages(missing_packages)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(purrr)
  library(forecast)
  library(openxlsx)
  library(tibble)
  library(R.utils)
  library(MASS)
  if (isTRUE(run_prophet)) library(prophet)
})

# ------------------------------------------------------------
# 2. LOAD AND VALIDATE INPUT
# ------------------------------------------------------------

if (!file.exists(workbook_path)) stop("Input workbook does not exist: ", workbook_path)

observed_series <- readxl::read_excel(workbook_path, sheet = observed_sheet) %>%
  dplyr::mutate(
    Global_Simulation_ID = as.integer(Global_Simulation_ID),
    Disease_Seed = as.character(Disease_Seed),
    Disease_Simulation_ID = as.integer(Disease_Simulation_ID),
    Time = as.integer(Time),
    Week = as.integer(Week),
    Observed_Cases = as.numeric(Observed_Cases)
  ) %>%
  dplyr::arrange(Global_Simulation_ID, Time)

required_cols <- c("Global_Simulation_ID", "Disease_Seed", "Disease_Simulation_ID", "Time", "Observed_Cases")
missing_cols <- setdiff(required_cols, names(observed_series))
if (length(missing_cols)) stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
if (nrow(observed_series) == 0L) stop("The observed simulated-data sheet is empty.")
if (anyNA(observed_series$Global_Simulation_ID) || anyNA(observed_series$Time)) {
  stop("Simulation IDs and Time must not contain missing values.")
}
if (anyDuplicated(observed_series[c("Global_Simulation_ID", "Time")])) {
  stop("Duplicate Global_Simulation_ID-Time combinations found.")
}

series_length_check <- observed_series %>%
  dplyr::group_by(Global_Simulation_ID) %>%
  dplyr::summarise(
    N_Weeks = dplyr::n(),
    N_Finite = sum(is.finite(Observed_Cases)),
    .groups = "drop"
  )

cat("\nSeries count:", nrow(series_length_check), "\n")
cat("Series-length summary:\n")
print(summary(series_length_check$N_Weeks))
cat("Required minimum length:", initial_window + h, "weeks\n")

if (all(series_length_check$N_Weeks < initial_window + h)) {
  stop("All series are shorter than initial_window + h. Adjust the evaluation settings.")
}

# ------------------------------------------------------------
# 3. METRIC HELPERS
# ------------------------------------------------------------

safe_mase_denom <- function(train, seasonal_period = freq) {
  train <- as.numeric(train)
  if (length(train) > seasonal_period) {
    a <- train[(seasonal_period + 1L):length(train)]
    b <- train[seq_len(length(train) - seasonal_period)]
    ok <- is.finite(a) & is.finite(b)
    if (any(ok)) {
      d <- mean(abs(a[ok] - b[ok]))
      if (is.finite(d) && d > 0) return(d)
    }
  }
  if (length(train) > 1L) {
    a <- train[-1L]
    b <- train[-length(train)]
    ok <- is.finite(a) & is.finite(b)
    if (any(ok)) {
      d <- mean(abs(a[ok] - b[ok]))
      if (is.finite(d) && d > 0) return(d)
    }
  }
  NA_real_
}

safe_mae <- function(actual, predicted) {
  ok <- is.finite(actual) & is.finite(predicted)
  if (!any(ok)) return(NA_real_)
  mean(abs(actual[ok] - predicted[ok]))
}

safe_rmse <- function(actual, predicted) {
  ok <- is.finite(actual) & is.finite(predicted)
  if (!any(ok)) return(NA_real_)
  sqrt(mean((actual[ok] - predicted[ok])^2))
}

# ------------------------------------------------------------
# 4. FORECASTING METHODS
# ------------------------------------------------------------

fit_seasonal_naive <- function(train_ts, horizon) {
  as.numeric(forecast::forecast(forecast::snaive(train_ts), h = horizon)$mean)
}

fit_stl_ets <- function(train_ts, horizon) {
  period <- stats::frequency(train_ts)
  if (period > 24L && length(train_ts) >= 2L * period) {
    ans <- tryCatch(
      forecast::stlf(train_ts, h = horizon, method = "ets", robust = TRUE),
      error = function(e) NULL
    )
    if (!is.null(ans)) return(as.numeric(ans$mean))
  }
  as.numeric(forecast::forecast(forecast::ets(train_ts, model = "ZZN"), h = horizon)$mean)
}

fit_sarima <- function(train_ts, horizon) {
  fit <- forecast::auto.arima(
    train_ts, seasonal = TRUE, stepwise = TRUE, approximation = TRUE,
    max.p = 2, max.q = 2, max.P = 1, max.Q = 1, max.d = 1, max.D = 1,
    ic = "aicc"
  )
  as.numeric(forecast::forecast(fit, h = horizon)$mean)
}

fit_arima_nonseasonal <- function(train_ts, horizon) {
  fit <- forecast::auto.arima(
    train_ts, seasonal = FALSE, stepwise = TRUE, approximation = TRUE,
    max.p = 2, max.q = 2, max.d = 1, ic = "aicc"
  )
  as.numeric(forecast::forecast(fit, h = horizon)$mean)
}

make_fourier_df <- function(index, period, K) {
  out <- data.frame(t = index)
  for (k in seq_len(K)) {
    out[[paste0("sin", k)]] <- sin(2 * pi * k * index / period)
    out[[paste0("cos", k)]] <- cos(2 * pi * k * index / period)
  }
  out
}

fit_negbin_regression <- function(train_ts, horizon) {
  y <- as.numeric(train_ts)
  if (length(y) < 20L || any(!is.finite(y))) return(rep(NA_real_, horizon))
  if (stats::sd(y) == 0) return(rep(mean(y), horizon))
  # The model is count-based; interpolated training values are rounded and truncated.
  y_count <- round(pmax(y, 0))
  n <- length(y_count)
  train_df <- make_fourier_df(seq_len(n), freq, negbin_fourier_order)
  train_df$y <- y_count
  future_df <- make_fourier_df((n + 1L):(n + horizon), freq, negbin_fourier_order)

  nb <- tryCatch(
    suppressWarnings(MASS::glm.nb(y ~ ., data = train_df, control = stats::glm.control(maxit = 50))),
    error = function(e) NULL
  )
  if (!is.null(nb)) {
    pred <- tryCatch(as.numeric(stats::predict(nb, newdata = future_df, type = "response")), error = function(e) rep(NA_real_, horizon))
    if (length(pred) == horizon && any(is.finite(pred))) return(pred)
  }
  pois <- tryCatch(
    suppressWarnings(stats::glm(y ~ ., data = train_df, family = stats::poisson(link = "log"), control = stats::glm.control(maxit = 50))),
    error = function(e) NULL
  )
  if (!is.null(pois)) {
    pred <- tryCatch(as.numeric(stats::predict(pois, newdata = future_df, type = "response")), error = function(e) rep(NA_real_, horizon))
    if (length(pred) == horizon && any(is.finite(pred))) return(pred)
  }
  rep(NA_real_, horizon)
}

fit_prophet_weekly <- function(train_ts, horizon) {
  y <- as.numeric(train_ts)
  if (length(y) < 20L || any(!is.finite(y))) return(rep(NA_real_, horizon))
  if (stats::sd(y) == 0) return(rep(mean(y), horizon))
  dates <- seq.Date(prophet_start_date, by = "week", length.out = length(y))
  train_df <- data.frame(ds = dates, y = pmax(y, 0))
  model <- tryCatch({
    m <- prophet::prophet(
      yearly.seasonality = FALSE, weekly.seasonality = FALSE, daily.seasonality = FALSE,
      seasonality.mode = "additive", changepoint.prior.scale = 0.05,
      seasonality.prior.scale = 10, uncertainty.samples = 0
    )
    m <- prophet::add_seasonality(m, name = "annual_52week", period = 364, fourier.order = prophet_fourier_order)
    suppressMessages(suppressWarnings(prophet::fit.prophet(m, train_df)))
  }, error = function(e) NULL)
  if (is.null(model)) return(rep(NA_real_, horizon))
  future <- data.frame(ds = seq.Date(max(dates) + 7, by = "week", length.out = horizon))
  fc <- tryCatch(suppressMessages(suppressWarnings(prophet::predict(model, future))), error = function(e) NULL)
  if (is.null(fc) || !"yhat" %in% names(fc)) return(rep(NA_real_, horizon))
  as.numeric(fc$yhat)
}

model_fns <- list(
  Seasonal_Naive = fit_seasonal_naive,
  STL_ETS = fit_stl_ets,
  SARIMA = fit_sarima,
  Nonseasonal_ARIMA = fit_arima_nonseasonal,
  NegBin_Regression = fit_negbin_regression
)
if (isTRUE(run_prophet)) model_fns$Prophet <- fit_prophet_weekly
model_names <- names(model_fns)

# ------------------------------------------------------------
# 5. RESULT STRUCTURE
# ------------------------------------------------------------

make_empty_result <- function(series_id, disease_seed, disease_sim_id, status, n_origins = 0L, error_message = NA_character_) {
  out <- tibble::tibble(
    Global_Simulation_ID = as.integer(series_id),
    Disease_Seed = as.character(disease_seed),
    Disease_Simulation_ID = as.integer(disease_sim_id)
  )
  for (metric in c("MASE", "MAE", "RMSE")) {
    for (model in model_names) out[[paste0(model, "_", metric)]] <- NA_real_
  }
  out$Best_Model <- NA_character_
  out$Best_MASE <- NA_real_
  out$Best_MAE <- NA_real_
  out$Best_RMSE <- NA_real_
  out$Ready_Label <- NA_character_
  out$Ready_Binary <- NA_integer_
  out$N_Origins <- as.integer(n_origins)
  out$Evaluation_Status <- as.character(status)
  out$Error_Message <- as.character(error_message)
  out
}

# ------------------------------------------------------------
# 6. ROLLING-ORIGIN EVALUATION
# ------------------------------------------------------------

evaluate_one_series <- function(data) {
  data <- dplyr::arrange(data, Time)
  series_id <- data$Global_Simulation_ID[1]
  disease_seed <- data$Disease_Seed[1]
  disease_sim_id <- data$Disease_Simulation_ID[1]
  x <- as.numeric(data$Observed_Cases)
  n <- length(x)

  if (n < initial_window + h) return(make_empty_result(series_id, disease_seed, disease_sim_id, "Insufficient length"))
  if (sum(is.finite(x)) < 2L || stats::sd(x, na.rm = TRUE) == 0) {
    return(make_empty_result(series_id, disease_seed, disease_sim_id, "Constant or near-constant series"))
  }

  # Use the latest available origins, capped at max_origins, as in the original protocol.
  origins <- seq.int(initial_window, n - h, by = rolling_step)
  if (length(origins) > max_origins) origins <- tail(origins, max_origins)
  if (!length(origins)) return(make_empty_result(series_id, disease_seed, disease_sim_id, "No valid origins"))

  actual_by_model <- setNames(vector("list", length(model_names)), model_names)
  pred_by_model <- setNames(vector("list", length(model_names)), model_names)
  denom_by_model <- setNames(vector("list", length(model_names)), model_names)
  n_origins_evaluated <- 0L

  for (origin in origins) {
    train_raw <- x[seq_len(origin)]
    test <- x[(origin + 1L):(origin + h)]
    denom <- safe_mase_denom(train_raw, freq)
    train_ts <- tryCatch(
      forecast::na.interp(stats::ts(train_raw, frequency = freq)),
      error = function(e) NULL
    )
    if (is.null(train_ts) || length(train_ts) != origin || any(!is.finite(train_ts))) next
    n_origins_evaluated <- n_origins_evaluated + 1L

    for (model in model_names) {
      pred <- tryCatch({
        p <- as.numeric(model_fns[[model]](train_ts, h))
        if (length(p) != h) p <- rep(NA_real_, h)
        p[!is.finite(p)] <- NA_real_
        pmax(p, 0)
      }, error = function(e) rep(NA_real_, h))
      actual_by_model[[model]] <- c(actual_by_model[[model]], test)
      pred_by_model[[model]] <- c(pred_by_model[[model]], pred)
      denom_by_model[[model]] <- c(denom_by_model[[model]], rep(denom, h))
    }
  }

  if (n_origins_evaluated == 0L) return(make_empty_result(series_id, disease_seed, disease_sim_id, "All origins failed"))

  mase_vals <- mae_vals <- rmse_vals <- setNames(rep(NA_real_, length(model_names)), model_names)
  for (model in model_names) {
    actual <- actual_by_model[[model]]
    pred <- pred_by_model[[model]]
    denom <- denom_by_model[[model]]
    ok <- is.finite(actual) & is.finite(pred) & is.finite(denom) & denom > 0
    if (any(ok)) mase_vals[model] <- mean(abs(actual[ok] - pred[ok]) / denom[ok])
    mae_vals[model] <- safe_mae(actual, pred)
    rmse_vals[model] <- safe_rmse(actual, pred)
  }

  # Do not label a series if every candidate model lacks a valid MASE.
  if (!any(is.finite(mase_vals))) {
    return(make_empty_result(series_id, disease_seed, disease_sim_id, "All MASE values missing", n_origins_evaluated))
  }

  best_model <- names(which.min(replace(mase_vals, !is.finite(mase_vals), Inf)))
  best_mase <- unname(mase_vals[best_model])
  best_mae <- unname(mae_vals[best_model])
  best_rmse <- unname(rmse_vals[best_model])
  ready_binary <- as.integer(best_mase < ready_mase_threshold)
  ready_label <- if (ready_binary == 1L) "Ready" else "Not_Ready"

  out <- make_empty_result(series_id, disease_seed, disease_sim_id, "Evaluated", n_origins_evaluated)
  for (model in model_names) {
    out[[paste0(model, "_MASE")]] <- unname(mase_vals[model])
    out[[paste0(model, "_MAE")]] <- unname(mae_vals[model])
    out[[paste0(model, "_RMSE")]] <- unname(rmse_vals[model])
  }
  out$Best_Model <- best_model
  out$Best_MASE <- best_mase
  out$Best_MAE <- best_mae
  out$Best_RMSE <- best_rmse
  out$Ready_Label <- ready_label
  out$Ready_Binary <- ready_binary
  out
}

# ------------------------------------------------------------
# 7. RUN EVALUATION WITH PER-SERIES TIMEOUT
# ------------------------------------------------------------

series_list <- observed_series %>%
  dplyr::group_by(Global_Simulation_ID) %>%
  dplyr::group_split()

cat("\nStarting EpiFORESS forecasting experiment\n")
cat("Series:", length(series_list), " | Models:", paste(model_names, collapse = ", "), "\n")
cat("Origins: latest up to", max_origins, " | Initial window:", initial_window,
    " | Horizon:", h, " | Step:", rolling_step, "\n")
cat("Ready if Best MASE <", ready_mase_threshold, " | Prophet:", run_prophet, "\n\n")

forecast_metrics <- purrr::map_dfr(seq_along(series_list), function(i) {
  dat <- series_list[[i]]
  if (i == 1L || i %% 50L == 0L || i == length(series_list)) {
    cat("Evaluating series", i, "of", length(series_list), "\n")
  }
  tryCatch(
    R.utils::withTimeout(evaluate_one_series(dat), timeout = series_timeout, onTimeout = "error"),
    TimeoutException = function(e) make_empty_result(
      dat$Global_Simulation_ID[1], dat$Disease_Seed[1], dat$Disease_Simulation_ID[1],
      "Timeout", error_message = conditionMessage(e)
    ),
    error = function(e) make_empty_result(
      dat$Global_Simulation_ID[1], dat$Disease_Seed[1], dat$Disease_Simulation_ID[1],
      "Error", error_message = conditionMessage(e)
    )
  )
}, .progress = TRUE)

# ------------------------------------------------------------
# 8. SUMMARIES
# ------------------------------------------------------------

safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_median <- function(x) if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE)
safe_sd_summary <- function(x) if (sum(is.finite(x)) < 2L) NA_real_ else stats::sd(x, na.rm = TRUE)

forecast_error_summary <- forecast_metrics %>% dplyr::summarise(
  Total_Series = dplyr::n(),
  Evaluated_Series = sum(Evaluation_Status == "Evaluated", na.rm = TRUE),
  Mean_Best_MASE = safe_mean(Best_MASE),
  Median_Best_MASE = safe_median(Best_MASE),
  SD_Best_MASE = safe_sd_summary(Best_MASE),
  Mean_Best_MAE = safe_mean(Best_MAE),
  Median_Best_MAE = safe_median(Best_MAE),
  Mean_Best_RMSE = safe_mean(Best_RMSE),
  Median_Best_RMSE = safe_median(Best_RMSE),
  Ready_Count = sum(Ready_Binary == 1L, na.rm = TRUE),
  NotReady_Count = sum(Ready_Binary == 0L, na.rm = TRUE),
  Labeled_Series = sum(!is.na(Ready_Binary)),
  Ready_Percent_Among_Labeled = if (sum(!is.na(Ready_Binary)) > 0L) round(100 * mean(Ready_Binary == 1L, na.rm = TRUE), 2) else NA_real_,
  Timeout_Count = sum(Evaluation_Status == "Timeout", na.rm = TRUE),
  Error_Count = sum(Evaluation_Status == "Error", na.rm = TRUE)
)

model_error_summary <- tibble::tibble(
  Model = model_names,
  Mean_MASE = vapply(paste0(model_names, "_MASE"), function(nm) safe_mean(forecast_metrics[[nm]]), numeric(1)),
  Median_MASE = vapply(paste0(model_names, "_MASE"), function(nm) safe_median(forecast_metrics[[nm]]), numeric(1)),
  Mean_MAE = vapply(paste0(model_names, "_MAE"), function(nm) safe_mean(forecast_metrics[[nm]]), numeric(1)),
  Median_MAE = vapply(paste0(model_names, "_MAE"), function(nm) safe_median(forecast_metrics[[nm]]), numeric(1)),
  Mean_RMSE = vapply(paste0(model_names, "_RMSE"), function(nm) safe_mean(forecast_metrics[[nm]]), numeric(1)),
  Median_RMSE = vapply(paste0(model_names, "_RMSE"), function(nm) safe_median(forecast_metrics[[nm]]), numeric(1))
) %>% dplyr::arrange(Mean_MASE)

best_model_summary <- forecast_metrics %>%
  dplyr::filter(!is.na(Best_Model)) %>%
  dplyr::count(Best_Model, name = "Frequency") %>%
  dplyr::mutate(Percent = round(100 * Frequency / sum(Frequency), 2)) %>%
  dplyr::arrange(dplyr::desc(Frequency))

readiness_summary <- forecast_metrics %>%
  dplyr::filter(!is.na(Ready_Label)) %>%
  dplyr::count(Ready_Label, name = "Frequency") %>%
  dplyr::mutate(Percent = round(100 * Frequency / sum(Frequency), 2))

evaluation_status_summary <- forecast_metrics %>%
  dplyr::count(Evaluation_Status, name = "Frequency") %>%
  dplyr::mutate(Percent = round(100 * Frequency / sum(Frequency), 2)) %>%
  dplyr::arrange(dplyr::desc(Frequency))

readiness_by_disease <- forecast_metrics %>%
  dplyr::filter(!is.na(Ready_Binary)) %>%
  dplyr::group_by(Disease_Seed) %>%
  dplyr::summarise(
    N_Series = dplyr::n(),
    Ready_Count = sum(Ready_Binary == 1L),
    NotReady_Count = sum(Ready_Binary == 0L),
    Ready_Rate_Percent = round(100 * mean(Ready_Binary == 1L), 2),
    Mean_Best_MASE = safe_mean(Best_MASE),
    Median_Best_MASE = safe_median(Best_MASE),
    SD_Best_MASE = safe_sd_summary(Best_MASE),
    Most_Common_Best_Model = {
      tab <- table(Best_Model)
      if (length(tab)) names(tab)[which.max(tab)] else NA_character_
    },
    .groups = "drop"
  ) %>% dplyr::arrange(dplyr::desc(Ready_Rate_Percent))

status_by_disease <- forecast_metrics %>%
  dplyr::count(Disease_Seed, Evaluation_Status, name = "Frequency") %>%
  dplyr::group_by(Disease_Seed) %>%
  dplyr::mutate(Percent = round(100 * Frequency / sum(Frequency), 2)) %>%
  dplyr::ungroup()

# ------------------------------------------------------------
# 9. SAVE SEPARATE OUTPUTS
# ------------------------------------------------------------

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
write.csv(forecast_metrics, file.path(output_dir, "forecast_metrics.csv"), row.names = FALSE)
write.csv(forecast_error_summary, file.path(output_dir, "forecast_error_summary.csv"), row.names = FALSE)
write.csv(model_error_summary, file.path(output_dir, "model_error_summary.csv"), row.names = FALSE)

output_sheets <- list(
  Forecast_Metrics = forecast_metrics,
  Forecast_Error_Summary = forecast_error_summary,
  Model_Error_Summary = model_error_summary,
  Best_Model_Summary = best_model_summary,
  Readiness_Summary = readiness_summary,
  Evaluation_Status = evaluation_status_summary,
  Readiness_By_Disease = readiness_by_disease,
  Status_By_Disease = status_by_disease
)

wb <- openxlsx::createWorkbook()
for (sheet_name in names(output_sheets)) {
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, output_sheets[[sheet_name]], withFilter = TRUE)
  openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
}
openxlsx::saveWorkbook(wb, output_workbook, overwrite = TRUE)

# ------------------------------------------------------------
# 10. FINAL LOG
# ------------------------------------------------------------

cat("\n================================================\n")
cat("EPIFORESS FORECASTING EXPERIMENT COMPLETE\n")
cat("================================================\n")
cat("Total series:", forecast_error_summary$Total_Series, "\n")
cat("Evaluated series:", forecast_error_summary$Evaluated_Series, "\n")
cat("Labeled series:", forecast_error_summary$Labeled_Series, "\n")
cat("Ready:", forecast_error_summary$Ready_Count, "\n")
cat("Not ready:", forecast_error_summary$NotReady_Count, "\n")
cat("Timeouts:", forecast_error_summary$Timeout_Count, "\n")
cat("Errors:", forecast_error_summary$Error_Count, "\n")
cat("Output workbook:", output_workbook, "\n")
cat("================================================\n")
