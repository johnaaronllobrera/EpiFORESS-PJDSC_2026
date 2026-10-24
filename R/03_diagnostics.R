# EpiFORESS | 03_diagnostics.R
# Year/date handling, stationarity checks, and STL helpers.
# Split from app_full_fixed.R; code logic preserved during file separation.

# 4. YEAR, STATIONARITY, AND STL HELPERS
# ------------------------------------------------------------
extract_year_safe <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(lubridate::year(x))
  }
  if (is.numeric(x) || is.integer(x)) {
    xn <- as.numeric(x)
    if (all(is.na(xn) | (xn >= 1900 & xn <= 2100))) return(xn)
    if (all(is.na(xn) | (xn > 20000 & xn < 80000))) {
      return(lubridate::year(as.Date(xn, origin = "1899-12-30")))
    }
    return(xn)
  }
  xc <- as.character(x)
  for (fn in list(lubridate::ymd, lubridate::mdy, lubridate::dmy)) {
    pd <- suppressWarnings(fn(xc))
    if (sum(!is.na(pd)) > 0) return(lubridate::year(pd))
  }
  suppressWarnings(as.numeric(stringr::str_extract(xc, "\\b(19|20)\\d{2}\\b")))
}
make_stationarity_stl_table <- function(y, frequency = 52, alpha = 0.05) {
  y <- as.numeric(y)
  frequency <- max(1L, as.integer(round(frequency)))
  valid_y <- y[!is.na(y)]
  empty_rows <- tibble::tibble(
    Component = c("Stationarity", "STL trend", "STL seasonality", "STL residual"),
    Test = c("Augmented Dickey-Fuller test", rep("STL decomposition", 3)),
    `P-Value` = rep("NA", 4),
    Decision = rep("Not evaluated", 4),
    Metric_Label = c(
      "ADF p-value", "STL trend strength", "STL seasonal strength",
      "Residual standard deviation"
    ),
    Metric_Value = rep("\u2014", 4),
    Interpretation = rep("At least 20 valid numeric observations are required.", 4),
    Recommendation = rep("At least 20 valid numeric observations are required.", 4)
  )
  if (length(valid_y) < 20L) return(empty_rows)
  series_sd <- stats::sd(valid_y, na.rm = TRUE)
  if (!is.finite(series_sd) || series_sd == 0) {
    empty_rows$Decision <- c("Constant series", rep("Not meaningful", 3))
    empty_rows$Interpretation <- rep(
      "The series is constant, so temporal components cannot be interpreted meaningfully.",
      4
    )
    empty_rows$Recommendation <- empty_rows$Interpretation
    return(empty_rows)
  }
  y_filled <- tryCatch(
    as.numeric(forecast::na.interp(stats::ts(y, frequency = frequency))),
    error = function(e) y
  )
  adf_test <- tryCatch(
    suppressWarnings(tseries::adf.test(y_filled, alternative = "stationary")),
    error = function(e) NULL
  )
  adf_p <- if (!is.null(adf_test)) adf_test$p.value else NA_real_
  adf_decision <- if (!is.finite(adf_p)) {
    "Not evaluated"
  } else if (adf_p < alpha) {
    "Passed"
  } else {
    "Flagged"
  }
  adf_interpretation <- dplyr::case_when(
    adf_decision == "Passed" ~ "No strong evidence of a unit root was detected.",
    adf_decision == "Flagged" ~ "Possible unit-root non-stationarity was detected.",
    TRUE ~ "Stationarity could not be evaluated reliably."
  )
  adf_recommendation <- dplyr::case_when(
    adf_decision == "Passed" ~ "Proceed with model validation while still checking for structural changes.",
    adf_decision == "Flagged" ~ paste(
      "Consider differencing, detrending, transformation,",
      "or a model that explicitly represents trend."
    ),
    TRUE ~ "Review the series and its temporal ordering before model development."
  )
  stationarity_row <- tibble::tibble(
    Component = "Stationarity",
    Test = "Augmented Dickey-Fuller test",
    `P-Value` = format_p_value(adf_p),
    Decision = adf_decision,
    Metric_Label = "ADF p-value",
    Metric_Value = if (is.finite(adf_p)) format_p_value(adf_p) else "\u2014",
    Interpretation = adf_interpretation,
    Recommendation = adf_recommendation
  )
  stl_fit <- if (frequency > 1L && length(y_filled) >= 2L * frequency) {
    tryCatch(
      stats::stl(
        stats::ts(y_filled, frequency = frequency),
        s.window = "periodic",
        robust = TRUE
      ),
      error = function(e) NULL
    )
  } else {
    NULL
  }
  if (is.null(stl_fit)) {
    stl_rows <- tibble::tibble(
      Component = c("STL trend", "STL seasonality", "STL residual"),
      Test = rep("STL decomposition", 3),
      `P-Value` = rep("NA", 3),
      Decision = rep("Not evaluated", 3),
      Metric_Label = c(
        "STL trend strength", "STL seasonal strength",
        "Residual standard deviation"
      ),
      Metric_Value = rep("\u2014", 3),
      Interpretation = rep("STL requires at least two complete seasonal cycles.", 3),
      Recommendation = rep(
        "Provide more observations or use a lower seasonal frequency when scientifically appropriate.",
        3
      )
    )
    return(dplyr::bind_rows(stationarity_row, stl_rows))
  }
  components <- stl_fit$time.series
  trend_component <- as.numeric(components[, "trend"])
  seasonal_component <- as.numeric(components[, "seasonal"])
  remainder_component <- as.numeric(components[, "remainder"])
  stl_trend_strength <- tryCatch({
    denominator <- stats::var(trend_component + remainder_component, na.rm = TRUE)
    numerator <- stats::var(remainder_component, na.rm = TRUE)
    if (!is.finite(denominator) || denominator <= 0 || !is.finite(numerator)) {
      NA_real_
    } else {
      max(0, min(1, 1 - numerator / denominator))
    }
  }, error = function(e) NA_real_)
  stl_seasonal_strength <- tryCatch({
    denominator <- stats::var(seasonal_component + remainder_component, na.rm = TRUE)
    numerator <- stats::var(remainder_component, na.rm = TRUE)
    if (!is.finite(denominator) || denominator <= 0 || !is.finite(numerator)) {
      NA_real_
    } else {
      max(0, min(1, 1 - numerator / denominator))
    }
  }, error = function(e) NA_real_)
  residual_sd <- tryCatch(stats::sd(remainder_component, na.rm = TRUE), error = function(e) NA_real_)
  trend_decision <- if (!is.finite(stl_trend_strength)) {
    "Not evaluated"
  } else if (stl_trend_strength >= 0.20) {
    "Present"
  } else {
    "Weak"
  }
  seasonal_decision <- if (!is.finite(stl_seasonal_strength)) {
    "Not evaluated"
  } else if (stl_seasonal_strength >= 0.20) {
    "Present"
  } else {
    "Weak"
  }
  residual_decision <- if (is.finite(residual_sd)) "Extracted" else "Not evaluated"
  stl_rows <- tibble::tibble(
    Component = c("STL trend", "STL seasonality", "STL residual"),
    Test = rep("STL decomposition", 3),
    `P-Value` = rep("NA", 3),
    Decision = c(trend_decision, seasonal_decision, residual_decision),
    Metric_Label = c(
      "STL trend strength", "STL seasonal strength",
      "Residual standard deviation"
    ),
    Metric_Value = c(
      if (is.finite(stl_trend_strength)) format_audit_number(stl_trend_strength, 2) else "\u2014",
      if (is.finite(stl_seasonal_strength)) format_audit_number(stl_seasonal_strength, 2) else "\u2014",
      if (is.finite(residual_sd)) format_audit_number(residual_sd, 2) else "\u2014"
    ),
    Interpretation = c(
      if (trend_decision == "Present") {
        "The STL decomposition identifies a meaningful smooth long-term component."
      } else {
        "The STL decomposition identifies little stable smooth long-term movement."
      },
      if (seasonal_decision == "Present") {
        "The STL decomposition identifies a repeatable seasonal component."
      } else {
        "The STL decomposition identifies little stable seasonal structure."
      },
      "The STL remainder contains variation not explained by the estimated trend and seasonal components."
    ),
    Recommendation = c(
      if (trend_decision == "Present") {
        "Include candidate models that can represent trend and assess whether the estimated movement remains stable."
      } else {
        "Do not impose a strong trend component unless out-of-sample validation supports it."
      },
      if (seasonal_decision == "Present") {
        "Include a seasonal benchmark or seasonal model during formal model comparison."
      } else {
        "Compare seasonal and nonseasonal specifications during validation."
      },
      "Inspect residual autocorrelation and unusual residual spikes before operational deployment."
    )
  )
  dplyr::bind_rows(stationarity_row, stl_rows)
}
make_stl_decomposition_data <- function(y, frequency = 52) {
  y <- as.numeric(y)
  frequency <- max(1L, as.integer(round(frequency)))
  valid_y <- y[!is.na(y)]
  empty_table <- tibble::tibble(
    Time_Index = numeric(0),
    Component = character(0),
    Value = numeric(0)
  )
  valid_sd <- if (length(valid_y) > 1L) stats::sd(valid_y, na.rm = TRUE) else NA_real_
  if (
    length(valid_y) < 20L || !is.finite(valid_sd) || valid_sd == 0 ||
    frequency <= 1L || length(valid_y) < 2L * frequency
  ) return(empty_table)
  y_filled <- tryCatch(
    as.numeric(forecast::na.interp(stats::ts(y, frequency = frequency))),
    error = function(e) NULL
  )
  if (is.null(y_filled)) return(empty_table)
  stl_fit <- tryCatch(
    stats::stl(
      stats::ts(y_filled, frequency = frequency),
      s.window = "periodic",
      robust = TRUE
    ),
    error = function(e) NULL
  )
  if (is.null(stl_fit)) return(empty_table)
  components <- as.data.frame(stl_fit$time.series)
  n <- length(y_filled)
  component_levels <- c("Original series", "STL trend", "STL seasonality", "STL residual")
  tibble::tibble(
    Time_Index = rep(seq_len(n), times = 4),
    Component = factor(
      rep(component_levels, each = n),
      levels = component_levels
    ),
    Value = c(
      y_filled,
      as.numeric(components$trend),
      as.numeric(components$seasonal),
      as.numeric(components$remainder)
    )
  )
}
# ------------------------------------------------------------
