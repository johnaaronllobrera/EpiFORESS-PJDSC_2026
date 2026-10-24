# EpiFORESS | 05_scoring.R
# Model prediction, calibration, score interpretation, and readiness scoring.
# Split from app_full_fixed.R; code logic preserved during file separation.

# 6. MODEL PREDICTION AND INTERPRETATION
# ------------------------------------------------------------
predict_platt_probability <- function(platt_model, raw_p) {
  raw_p <- clamp_probability(raw_p)
  if (is.null(platt_model)) return(raw_p)
  model_terms <- tryCatch(
    attr(stats::terms(platt_model), "term.labels"),
    error = function(e) NULL
  )
  candidate_names <- unique(c(
    model_terms,
    "prob", "p_raw", "raw_p", "raw_prob", "ml_raw", "ml_raw_prob",
    "xgb_raw", "xgb_raw_prob", "xgb_prob", "p_ml_raw", "P_ML_NotReady_Raw"
  ))
  for (nm in candidate_names) {
    if (is.null(nm) || is.na(nm) || nm == "") next
    nd <- data.frame(raw_p)
    names(nd) <- nm
    out <- tryCatch(
      as.numeric(stats::predict(platt_model, newdata = nd, type = "response")),
      error = function(e) NULL
    )
    if (!is.null(out) && all(is.finite(out))) return(clamp_probability(out))
  }
  warning(
    "The optional machine-learning probability calibration model could not be applied. ",
    "The uncalibrated model probability was used instead."
  )
  raw_p
}
predict_not_ready <- function(
    model_obj,
    newdata,
    predictor_set = predictor_vars,
    calibration_model = NULL
) {
  model <- unwrap_model_object(model_obj)
  newdata <- as.data.frame(newdata)
  for (v in predictor_set) {
    if (!v %in% names(newdata)) newdata[[v]] <- 0
  }
  newdata <- newdata[, predictor_set, drop = FALSE]
  if (inherits(model, "cv.glmnet")) {
    p <- as.numeric(predict(model, newx = as.matrix(newdata), s = "lambda.min", type = "response"))
    return(clamp_probability(p))
  }
  if (inherits(model, "glmnet")) {
    p <- as.numeric(predict(model, newx = as.matrix(newdata), type = "response"))
    return(clamp_probability(p))
  }
  if (inherits(model, "glm")) {
    gp <- as.numeric(stats::predict(model, newdata = newdata, type = "response"))
    if (stat_glm_target_class %in% c("Not_Ready", "NotReady", "Not Ready")) {
      return(clamp_probability(gp))
    }
    if (stat_glm_target_class %in% c("Ready", "Forecast_Ready", "Forecast Ready")) {
      return(clamp_probability(1 - gp))
    }
    stop("Could not identify the GLM response class.")
  }
  if (inherits(model, "train")) {
    needed_cols <- model$xNames
    if (!is.null(needed_cols)) {
      missing_needed <- setdiff(needed_cols, names(newdata))
      if (length(missing_needed) > 0) {
        stop("New data are missing required caret columns: ", paste(missing_needed, collapse = ", "))
      }
      newdata <- newdata[, needed_cols, drop = FALSE]
    }
    prob <- predict(model, newdata = newdata, type = "prob")
    cn <- stringr::str_replace_all(tolower(colnames(prob)), "[^a-z0-9]", "")
    ni <- which(cn %in% c("notready", "notforecastready"))
    ri <- which(cn %in% c("ready", "forecastready"))
    if (length(ni) == 1L) {
      p_notready <- clamp_probability(as.numeric(prob[, ni]))
    } else if (length(ri) == 1L) {
      p_notready <- clamp_probability(1 - as.numeric(prob[, ri]))
    } else {
      stop("Could not identify the Not_Ready probability column in the caret model.")
    }
    if (!is.null(calibration_model)) {
      p_notready <- predict_platt_probability(calibration_model, p_notready)
    }
    return(clamp_probability(p_notready))
  }
  if (inherits(model, "ranger")) {
    if (!requireNamespace("ranger", quietly = TRUE)) {
      stop("Package ranger is required to score the saved random-forest model.")
    }
    ranger_pred <- ranger::predict.ranger(model, data = newdata)$predictions
    if (is.null(dim(ranger_pred))) {
      stop(
        "The saved ranger model does not return class probabilities. ",
        "Retrain or save it with probability = TRUE."
      )
    }
    cn <- stringr::str_replace_all(tolower(colnames(ranger_pred)), "[^a-z0-9]", "")
    ni <- which(cn %in% c("notready", "notforecastready"))
    ri <- which(cn %in% c("ready", "forecastready"))
    if (length(ni) == 1L) {
      p_notready <- clamp_probability(as.numeric(ranger_pred[, ni]))
    } else if (length(ri) == 1L) {
      p_notready <- clamp_probability(1 - as.numeric(ranger_pred[, ri]))
    } else {
      stop("Could not identify the Not_Ready probability column in the ranger model.")
    }
    if (!is.null(calibration_model)) {
      p_notready <- predict_platt_probability(calibration_model, p_notready)
    }
    return(clamp_probability(p_notready))
  }
  if (inherits(model, "xgb.Booster")) {
    mat <- as.matrix(newdata)
    colnames(mat) <- predictor_set
    p_raw <- as.numeric(predict(model, newdata = xgboost::xgb.DMatrix(data = mat)))
    if (ml_target_class %in% c("Ready", "Forecast_Ready", "Forecast Ready")) {
      p_raw <- 1 - p_raw
    }
    p_cal <- predict_platt_probability(calibration_model, p_raw)
    return(clamp_probability(p_cal))
  }
  stop("Unsupported model object: ", paste(class(model), collapse = ", "))
}
score_band_metadata <- function(score) {
  if (!is_valid_number(score)) {
    return(list(
      key = "unclassified",
      short = "Unclassified",
      label = "Unclassified",
      range = "Not available",
      note = "The series could not be assigned to a readiness band.",
      action = "Cannot assess this series yet; check the input data first"
    ))
  }
  if (score < 20) {
    return(list(
      key = "very-low",
      short = "Very Low",
      label = "Very Low Forecast-Readiness",
      range = "0-19",
      note = "Very limited support for routine forecast development.",
      action = "Do not prioritize routine forecasting with the current series"
    ))
  }
  if (score < 35) {
    return(list(
      key = "low",
      short = "Low",
      label = "Low Forecast-Readiness",
      range = "20-34",
      note = "Forecasting should remain exploratory while limitations are addressed.",
      action = "Keep forecasting exploratory while improving the data and reviewing instability"
    ))
  }
  if (score < 60) {
    return(list(
      key = "moderate",
      short = "Moderate",
      label = "Moderate Forecast-Readiness",
      range = "35-59",
      note = "Cautious candidate-model comparison may be considered.",
      action = "Proceed only with cautious candidate-model comparison"
    ))
  }
  if (score < 80) {
    return(list(
      key = "high",
      short = "High",
      label = "High Forecast-Readiness",
      range = "60-79",
      note = "Favorable candidate for model fitting and rolling-origin validation.",
      action = "Proceed to candidate-model fitting and rolling-origin validation"
    ))
  }
  list(
    key = "very-high",
    short = "Very High",
    label = "Very High Forecast-Readiness",
    range = "80-100",
    note = "Strong candidate, pending comprehensive out-of-sample validation.",
    action = "Proceed to full forecast validation, followed by a final operational review"
  )
}
score_label <- function(score) {
  score_band_metadata(score)$label
}
score_band_short <- function(score) {
  score_band_metadata(score)$short
}
score_band_key <- function(score) {
  score_band_metadata(score)$key
}
deployment_action_label <- function(score) {
  score_band_metadata(score)$action
}
identify_supportive_indicators_vec <- function(raw_features, max_indicators = 5L) {
  f <- raw_features[1, ]
  indicators <- character(0)
  if (is_valid_number(f$missingness_rate) && f$missingness_rate < 0.05) {
    indicators <- c(indicators, "most weeks have a usable reported count")
  }
  if (is_valid_number(f$seasonality_strength) && f$seasonality_strength >= 0.60) {
    indicators <- c(indicators, "a strong repeating yearly pattern")
  } else if (is_valid_number(f$seasonality_strength) && f$seasonality_strength >= 0.35) {
    indicators <- c(indicators, "a noticeable repeating yearly pattern")
  }
  if (is_valid_number(f$lag1_autocorrelation) && f$lag1_autocorrelation >= 0.40) {
    indicators <- c(indicators, "strong continuity from one week to the next")
  } else if (is_valid_number(f$lag1_autocorrelation) && f$lag1_autocorrelation >= 0.25) {
    indicators <- c(indicators, "some useful week-to-week continuity")
  }
  if (is_valid_number(f$trend_strength) && f$trend_strength >= 0.40) {
    indicators <- c(indicators, "a clear linear long-term direction")
  }
  if (is_valid_number(f$signal_to_noise_ratio) && f$signal_to_noise_ratio >= 1.0) {
    indicators <- c(indicators, "more learnable structure than unexplained noise")
  }
  if (is_valid_number(f$spectral_entropy) && f$spectral_entropy <= 0.55) {
    indicators <- c(indicators, "a frequency pattern that is not highly noise-like")
  }
  if (is_valid_number(f$proportion_zero_counts) && f$proportion_zero_counts < 0.20) {
    indicators <- c(indicators, "few zero-count weeks")
  }
  if (is_valid_number(f$repetition_index) && f$repetition_index < 0.15) {
    indicators <- c(indicators, "few repeated carry-forward values")
  }
  if (is_valid_number(f$rolling_mean_instability) && f$rolling_mean_instability < 0.20) {
    indicators <- c(indicators, "a fairly stable average level over time")
  }
  if (is_valid_number(f$seasonal_peak_timing_instability) && f$seasonal_peak_timing_instability < 0.20) {
    indicators <- c(indicators, "yearly peaks that occur at fairly consistent times")
  }
  indicators <- unique(indicators)
  head(indicators, max(0L, as.integer(max_indicators)))
}
identify_potential_concerns_vec <- function(raw_features, max_notes = 4L) {
  f <- raw_features[1, ]
  notes <- tibble::tibble(note = character(0), severity = integer(0), priority = integer(0))
  add_note <- function(note, severity, priority) {
    notes <<- dplyr::bind_rows(
      notes,
      tibble::tibble(note = as.character(note), severity = as.integer(severity), priority = as.integer(priority))
    )
  }
  if (is_valid_number(f$missingness_rate) && f$missingness_rate >= 0.15) {
    add_note("many surveillance weeks are missing", 3L, 1L)
  } else if (is_valid_number(f$missingness_rate) && f$missingness_rate >= 0.08) {
    add_note("some surveillance weeks are missing", 2L, 1L)
  }
  very_sparse <- is_valid_number(f$mean_cases) && is_valid_number(f$proportion_zero_counts) &&
    f$mean_cases < 1 && f$proportion_zero_counts >= 0.40
  if (very_sparse) {
    add_note("counts are very low and sparse", 3L, 2L)
  } else if (is_valid_number(f$proportion_zero_counts) && f$proportion_zero_counts >= 0.60) {
    add_note("most observed weeks contain zero cases", 3L, 2L)
  } else if (is_valid_number(f$proportion_zero_counts) && f$proportion_zero_counts >= 0.40) {
    add_note("zero-count weeks are frequent", 2L, 2L)
  }
  high_dispersion <- is_valid_number(f$dispersion_index) && f$dispersion_index >= 10
  extreme_dispersion <- is_valid_number(f$dispersion_index) && f$dispersion_index >= 20
  large_spikes <- is_valid_number(f$peak_to_mean_ratio) && f$peak_to_mean_ratio >= 10
  extreme_spikes <- is_valid_number(f$peak_to_mean_ratio) && f$peak_to_mean_ratio >= 18
  if (extreme_dispersion && extreme_spikes) {
    add_note("weekly counts are extremely unstable and contain very large peaks", 3L, 3L)
  } else if (high_dispersion && large_spikes) {
    add_note("weekly counts are unstable and contain large peaks", 2L, 3L)
  } else if (extreme_spikes) {
    add_note("the largest week is unusually high compared with the baseline", 3L, 3L)
  } else if (extreme_dispersion) {
    add_note("weekly counts vary extremely widely", 3L, 3L)
  }
  weak_signal <- is_valid_number(f$trend_strength) && f$trend_strength < 0.10 &&
    is_valid_number(f$seasonality_strength) && f$seasonality_strength < 0.15 &&
    is_valid_number(f$lag1_autocorrelation) && abs(f$lag1_autocorrelation) < 0.15
  near_random <- is_valid_number(f$spectral_entropy) && f$spectral_entropy >= 0.88
  if (near_random && weak_signal) {
    add_note("the series has little stable pattern for a forecasting model to learn", 3L, 4L)
  } else if (near_random) {
    add_note("the series is highly irregular over time", 2L, 4L)
  } else if (weak_signal) {
    add_note("linear trend, week-of-year seasonality, and week-to-week continuity are all weak", 2L, 4L)
  }
  if (is_valid_number(f$rolling_mean_instability) && f$rolling_mean_instability >= 0.50) {
    add_note("the average level changes substantially across the series", 3L, 5L)
  } else if (is_valid_number(f$rolling_mean_instability) && f$rolling_mean_instability >= 0.30) {
    add_note("the average level changes noticeably across the series", 2L, 5L)
  }
  if (is_valid_number(f$rolling_variance_instability) && f$rolling_variance_instability >= 1.0) {
    add_note("the amount of variability changes strongly over time", 3L, 6L)
  } else if (is_valid_number(f$rolling_variance_instability) && f$rolling_variance_instability >= 0.60) {
    add_note("the amount of variability is not stable over time", 2L, 6L)
  }
  if (is_valid_number(f$stl_remainder_strength) && f$stl_remainder_strength >= 0.75) {
    add_note("most of the variation remains unexplained after seasonal decomposition", 2L, 7L)
  }
  if (is_valid_number(f$seasonal_peak_timing_instability) && f$seasonal_peak_timing_instability >= 0.60) {
    add_note("the timing of the yearly peak changes greatly from year to year", 2L, 8L)
  }
  strong_heaping <- is_valid_number(f$heaping_index) && f$heaping_index >= 0.45
  strong_repetition <- is_valid_number(f$repetition_index) && f$repetition_index >= 0.45
  if (strong_heaping && strong_repetition) {
    add_note("rounded and repeated values suggest possible reporting artifacts", 3L, 9L)
  } else if (strong_heaping) {
    add_note("many counts are rounded to multiples of five", 2L, 9L)
  } else if (strong_repetition) {
    add_note("the same count is repeated across many adjacent weeks", 2L, 9L)
  }
  if (nrow(notes) == 0) return(character(0))
  notes %>%
    dplyr::distinct(note, .keep_all = TRUE) %>%
    dplyr::arrange(dplyr::desc(severity), priority) %>%
    dplyr::slice_head(n = max(0L, as.integer(max_notes))) %>%
    dplyr::pull(note)
}
make_diagnostic_profile <- function(raw_features) {
  f <- raw_features[1, ]
  fmt_num <- function(x, digits = 2) {
    if (!is_valid_number(x)) return("Not estimable")
    formatC(x, format = "f", digits = digits)
  }
  fmt_pct <- function(x, accuracy = 0.1) {
    if (!is_valid_number(x)) return("Not estimable")
    scales::percent(x, accuracy = accuracy)
  }
  make_row <- function(area, status, status_key, explanation) {
    tibble::tibble(
      Area = area,
      Status = status,
      Status_Key = status_key,
      Explanation = explanation
    )
  }
  completeness <- if (!is_valid_number(f$missingness_rate)) {
    make_row("Data completeness", "Not estimable", "neutral", "Missingness could not be estimated from the selected series.")
  } else if (f$missingness_rate < 0.05) {
    make_row("Data completeness", "Adequate", "good", paste0("Missingness rate = ", fmt_pct(f$missingness_rate), "."))
  } else if (f$missingness_rate < 0.10) {
    make_row(
      "Data completeness", "Minor concern", "warn",
      paste0("Missingness rate = ", fmt_pct(f$missingness_rate), "; verify the temporal sequence before forecasting.")
    )
  } else {
    make_row(
      "Data completeness", "Elevated concern", "danger",
      paste0("Missingness rate = ", fmt_pct(f$missingness_rate), "; substantial gaps may weaken temporal estimation.")
    )
  }
  structure_values <- c(
    if (is_valid_number(f$seasonality_strength)) f$seasonality_strength else NA_real_,
    if (is_valid_number(f$trend_strength)) f$trend_strength else NA_real_,
    if (is_valid_number(f$lag1_autocorrelation)) abs(f$lag1_autocorrelation) else NA_real_
  )
  structure_values <- structure_values[is.finite(structure_values)]
  structure_score <- if (length(structure_values) > 0) max(structure_values) else NA_real_
  structure_status <- dplyr::case_when(
    !is_valid_number(structure_score) ~ "Not estimable",
    structure_score >= 0.40 ~ "Strong",
    structure_score >= 0.20 ~ "Moderate",
    TRUE ~ "Weak"
  )
  structure_key <- dplyr::case_when(
    structure_status == "Strong" ~ "good",
    structure_status == "Moderate" ~ "warn",
    structure_status == "Weak" ~ "danger",
    TRUE ~ "neutral"
  )
  temporal_structure <- make_row(
    "Temporal structure",
    structure_status,
    structure_key,
    paste0(
      "Linear trend R-squared = ", fmt_num(f$trend_strength),
      "; week-of-year seasonality R-squared = ", fmt_num(f$seasonality_strength),
      "; |lag-1 ACF| = ", fmt_num(abs(f$lag1_autocorrelation)), "."
    )
  )
  mean_instability <- if (is_valid_number(f$rolling_mean_instability)) f$rolling_mean_instability else NA_real_
  variance_instability <- if (is_valid_number(f$rolling_variance_instability)) f$rolling_variance_instability else NA_real_
  stability_status <- dplyr::case_when(
    !is_valid_number(mean_instability) && !is_valid_number(variance_instability) ~ "Not estimable",
    (!is_valid_number(mean_instability) || mean_instability < 0.20) &&
      (!is_valid_number(variance_instability) || variance_instability < 0.50) ~ "Stable",
    (!is_valid_number(mean_instability) || mean_instability < 0.40) &&
      (!is_valid_number(variance_instability) || variance_instability < 0.90) ~ "Variable",
    TRUE ~ "Unstable"
  )
  stability_key <- dplyr::case_when(
    stability_status == "Stable" ~ "good",
    stability_status == "Variable" ~ "warn",
    stability_status == "Unstable" ~ "danger",
    TRUE ~ "neutral"
  )
  temporal_stability <- make_row(
    "Temporal stability",
    stability_status,
    stability_key,
    paste0(
      "13-week mean instability = ", fmt_num(mean_instability),
      "; 13-week variance instability = ", fmt_num(variance_instability), "."
    )
  )
  intermittency_status <- dplyr::case_when(
    !is_valid_number(f$proportion_zero_counts) ~ "Not estimable",
    f$proportion_zero_counts < 0.20 ~ "Low",
    f$proportion_zero_counts < 0.40 ~ "Moderate",
    TRUE ~ "High"
  )
  intermittency_key <- dplyr::case_when(
    intermittency_status == "Low" ~ "good",
    intermittency_status == "Moderate" ~ "warn",
    intermittency_status == "High" ~ "danger",
    TRUE ~ "neutral"
  )
  count_intermittency <- make_row(
    "Count intermittency",
    intermittency_status,
    intermittency_key,
    paste0(
      "Zero-count proportion = ", fmt_pct(f$proportion_zero_counts),
      "; transition rate = ", fmt_num(f$intermittency), "."
    )
  )
  reporting_status <- dplyr::case_when(
    !is_valid_number(f$heaping_index) && !is_valid_number(f$repetition_index) ~ "Not estimable",
    (!is_valid_number(f$heaping_index) || f$heaping_index < 0.30) &&
      (!is_valid_number(f$repetition_index) || f$repetition_index < 0.30) ~ "No major flag",
    (!is_valid_number(f$heaping_index) || f$heaping_index < 0.45) &&
      (!is_valid_number(f$repetition_index) || f$repetition_index < 0.45) ~ "Review",
    TRUE ~ "Elevated concern"
  )
  reporting_key <- dplyr::case_when(
    reporting_status == "No major flag" ~ "good",
    reporting_status == "Review" ~ "warn",
    reporting_status == "Elevated concern" ~ "danger",
    TRUE ~ "neutral"
  )
  reporting_quality <- make_row(
    "Reporting quality",
    reporting_status,
    reporting_key,
    paste0(
      "Heaping index = ", fmt_pct(f$heaping_index),
      "; repetition index = ", fmt_pct(f$repetition_index), "."
    )
  )
  extreme_status <- dplyr::case_when(
    !is_valid_number(f$peak_to_mean_ratio) && !is_valid_number(f$outbreak_week_rate) ~ "Not estimable",
    (!is_valid_number(f$peak_to_mean_ratio) || f$peak_to_mean_ratio < 5) &&
      (!is_valid_number(f$outbreak_week_rate) || f$outbreak_week_rate < 0.10) ~ "Limited",
    !is_valid_number(f$peak_to_mean_ratio) || f$peak_to_mean_ratio < 10 ~ "Moderate",
    TRUE ~ "High"
  )
  extreme_key <- dplyr::case_when(
    extreme_status == "Limited" ~ "good",
    extreme_status == "Moderate" ~ "warn",
    extreme_status == "High" ~ "danger",
    TRUE ~ "neutral"
  )
  extreme_concentration <- make_row(
    "Extreme-event concentration",
    extreme_status,
    extreme_key,
    paste0(
      "Peak-to-mean ratio = ", fmt_num(f$peak_to_mean_ratio),
      "; outbreak-week rate = ", fmt_pct(f$outbreak_week_rate), "."
    )
  )
  dplyr::bind_rows(
    completeness,
    temporal_structure,
    temporal_stability,
    count_intermittency,
    reporting_quality,
    extreme_concentration
  )
}
secondary_note_label <- function(potential_concerns) {
  n <- length(unique(potential_concerns))
  if (n == 0) return("No major issue flagged")
  if (n == 1) return("One item to review")
  if (n == 2) return("Two items to review")
  "Several items to review"
}
epiforess_icon <- function(name, class = NULL) {
  paths <- switch(
    name,
    warning = tagList(
      tags$path(d = "M10.29 3.86 1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z"),
      tags$line(x1 = "12", y1 = "9", x2 = "12", y2 = "13"),
      tags$line(x1 = "12", y1 = "17", x2 = "12.01", y2 = "17")
    ),
    calendar = tagList(
      tags$rect(x = "3", y = "4", width = "18", height = "18", rx = "2", ry = "2"),
      tags$line(x1 = "16", y1 = "2", x2 = "16", y2 = "6"),
      tags$line(x1 = "8", y1 = "2", x2 = "8", y2 = "6"),
      tags$line(x1 = "3", y1 = "10", x2 = "21", y2 = "10")
    ),
    activity = tags$polyline(points = "22 12 18 12 15 21 9 3 6 12 2 12"),
    trend = tagList(
      tags$polyline(points = "3 17 9 11 13 15 21 7"),
      tags$polyline(points = "14 7 21 7 21 14")
    ),
    check = tagList(
      tags$circle(cx = "12", cy = "12", r = "9"),
      tags$polyline(points = "8 12 11 15 16 9")
    ),
    model = tagList(
      tags$circle(cx = "7", cy = "7", r = "3"),
      tags$circle(cx = "17", cy = "17", r = "3"),
      tags$line(x1 = "9.2", y1 = "9.2", x2 = "14.8", y2 = "14.8")
    ),
    info = tagList(
      tags$circle(cx = "12", cy = "12", r = "9"),
      tags$line(x1 = "12", y1 = "11", x2 = "12", y2 = "16"),
      tags$line(x1 = "12", y1 = "8", x2 = "12.01", y2 = "8")
    ),
    chart = tagList(
      tags$line(x1 = "4", y1 = "20", x2 = "4", y2 = "10"),
      tags$line(x1 = "10", y1 = "20", x2 = "10", y2 = "4"),
      tags$line(x1 = "16", y1 = "20", x2 = "16", y2 = "13"),
      tags$line(x1 = "22", y1 = "20", x2 = "22", y2 = "7")
    ),
    download = tagList(
      tags$path(d = "M12 3v12"),
      tags$polyline(points = "7 10 12 15 17 10"),
      tags$path(d = "M5 19h14")
    ),
    document = tagList(
      tags$path(d = "M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"),
      tags$polyline(points = "14 2 14 8 20 8"),
      tags$line(x1 = "9", y1 = "13", x2 = "15", y2 = "13"),
      tags$line(x1 = "9", y1 = "17", x2 = "15", y2 = "17")
    ),
    tags$circle(cx = "12", cy = "12", r = "9")
  )
  tags$svg(
    class = paste("ef-svg-icon", class %||% ""),
    xmlns = "http://www.w3.org/2000/svg",
    viewBox = "0 0 24 24",
    fill = "none",
    stroke = "currentColor",
    `stroke-width` = "2",
    `stroke-linecap` = "round",
    `stroke-linejoin` = "round",
    `aria-hidden` = "true",
    paths
  )
}
format_action_step <- function(x) {
  x <- stringr::str_squish(as.character(x))
  if (is.na(x) || !nzchar(x)) return(x)
  x <- paste0(stringr::str_to_upper(stringr::str_sub(x, 1, 1)), stringr::str_sub(x, 2))
  if (!stringr::str_detect(x, "[.!?]$")) x <- paste0(x, ".")
  x
}
concern_action_text <- function(note) {
  note_lower <- tolower(note)
  dplyr::case_when(
    stringr::str_detect(note_lower, "missing") ~ "check and document missing weeks before fitting models",
    stringr::str_detect(note_lower, "zero|sparse") ~ "consider temporal aggregation or methods designed for intermittent counts",
    stringr::str_detect(note_lower, "variability|instability|peaks|unexplained|average level") ~
      "verify extreme weeks and test whether performance remains stable across rolling origins",
    stringr::str_detect(note_lower, "trend|season|temporal|irregular") ~
      "compare seasonal and nonseasonal benchmarks before selecting a model",
    stringr::str_detect(note_lower, "heaping|repeated|reporting|rounded|multiples of five") ~
      "review rounding, repeated entries, and carry-forward reporting",
    TRUE ~ "review the affected feature before forecasting"
  )
}
compute_recommended_action <- function(score, concerns) {
  concerns <- unique(concerns %||% character(0))
  band_key <- score_band_key(score)
  action_heading <- switch(
    band_key,
    "very-low" = "Do Not Prioritize Routine Forecasting",
    "low" = "Keep Forecasting Exploratory",
    "moderate" = "Proceed with Cautious Model Exploration",
    "high" = "Begin Candidate-Model Validation",
    "very-high" = "Proceed to Full Forecast Validation",
    "Assessment Unavailable"
  )
  action_summary <- switch(
    band_key,
    "very-low" = paste(
      "The current feature profile provides very limited support for reliable routine forecasting.",
      "Prioritize descriptive surveillance summaries, data improvement, or an alternative analytical approach."
    ),
    "low" = paste(
      "The series contains limited potentially useful temporal information.",
      "Forecasting should remain exploratory while its principal limitations are investigated."
    ),
    "moderate" = paste(
      "The series demonstrates moderate forecast-readiness.",
      "Cautious candidate-model comparison is reasonable, but forecast performance must first be confirmed out of sample."
    ),
    "high" = paste(
      "The series has a favorable forecast-readiness profile.",
      "Proceed to candidate-model fitting and confirm performance through rolling-origin validation."
    ),
    "very-high" = paste(
      "The series has a strongly favorable forecast-readiness profile.",
      "Proceed to benchmarked model fitting and comprehensive rolling-origin validation before operational use."
    ),
    paste(
      "The series could not be assessed.",
      "Review the input data and rerun the screening."
    )
  )
  concern_steps <- if (length(concerns) > 0) {
    unique(vapply(concerns, concern_action_text, character(1)))
  } else {
    character(0)
  }
  core_steps <- switch(
    band_key,
    "very-low" = c(
      "document the principal data-quality and structural limitations",
      "improve the series or use descriptive surveillance summaries before rescreening"
    ),
    "low" = c(
      "treat any forecasts as exploratory rather than operational",
      "address the identified limitations and rerun the screening"
    ),
    "moderate" = c(
      "compare a limited set of candidate models with a seasonal-naive benchmark",
      "evaluate error, bias, and stability across rolling forecast origins"
    ),
    "high" = c(
      "compare candidate models with a seasonal-naive benchmark using rolling-origin validation",
      "confirm that forecast performance and residual diagnostics remain stable"
    ),
    "very-high" = c(
      "conduct comprehensive benchmarked rolling-origin validation",
      "confirm performance, residual behavior, and operational assumptions before deployment"
    ),
    c(
      "verify that the series contains at least 20 valid nonnegative observations",
      "confirm the selected count, year, week, and seasonal-frequency fields"
    )
  )
  next_steps <- head(unique(c(concern_steps, core_steps)), 3L)
  next_steps <- vapply(next_steps, format_action_step, character(1), USE.NAMES = FALSE)
  list(
    heading = action_heading,
    summary = action_summary,
    next_steps = next_steps
  )
}
make_screening_summary <- function(
    score,
    supportive_indicators,
    potential_concerns
) {
  if (!is_valid_number(score)) {
    return(
      "The screening could not be completed because the selected series does not contain enough usable information."
    )
  }
  supportive_indicators <- unique(
    stringr::str_squish(supportive_indicators[!is.na(supportive_indicators)])
  )
  supportive_indicators <- supportive_indicators[supportive_indicators != ""]
  potential_concerns <- unique(
    stringr::str_squish(potential_concerns[!is.na(potential_concerns)])
  )
  potential_concerns <- potential_concerns[potential_concerns != ""]
  support_text <- if (length(supportive_indicators) > 0) {
    paste(head(supportive_indicators, 2L), collapse = " and ")
  } else {
    ""
  }
  concern_text <- if (length(potential_concerns) > 0) {
    paste(head(potential_concerns, 2L), collapse = " and ")
  } else {
    ""
  }
  gradient_text <- switch(
    score_band_key(score),
    "very-low" = "The series is positioned in the very low range of forecast-readiness.",
    "low" = "The series is positioned in the low range of forecast-readiness.",
    "moderate" = "The series is positioned in the moderate range of forecast-readiness.",
    "high" = "The series is positioned in the high range of forecast-readiness.",
    "very-high" = "The series is positioned in the very high range of forecast-readiness.",
    "The series could not be assigned to a readiness band."
  )
  if (nzchar(support_text) && nzchar(concern_text)) {
    return(paste0(
      gradient_text,
      " Supportive structure includes ", support_text,
      ", while the main considerations are ", concern_text, "."
    ))
  }
  if (nzchar(concern_text)) {
    return(paste0(gradient_text, " The main considerations are ", concern_text, "."))
  }
  if (nzchar(support_text)) {
    return(paste0(gradient_text, " The clearest supportive evidence is ", support_text, "."))
  }
  paste0(
    gradient_text,
    " No single diagnostic feature dominates the result; the score reflects the complete 49-feature profile."
  )
}
forecast_readiness_message <- function(
    score,
    supportive_indicators,
    potential_concerns
) {
  if (!is_valid_number(score)) {
    return(
      "The series could not be scored. Verify that the selected count column contains sufficient valid observations."
    )
  }
  class_label <- score_label(score)
  action_label <- deployment_action_label(score)

  estimated_best_mase <-
    estimate_median_best_mase(score)[1]

  interpretation <- switch(
    score_band_key(score),
    "very-low" = "The extracted feature profile provides very limited evidence of stable forecastable structure.",
    "low" = "The extracted feature profile contains limited temporal information, and substantial limitations remain.",
    "moderate" = "The extracted feature profile contains moderate forecastable structure, but model development should remain cautious and validation-driven.",
    "high" = "The extracted feature profile is favorable for forecast-model development and rolling-origin validation.",
    "very-high" = "The extracted feature profile is strongly favorable for forecast-model development and comprehensive validation.",
    "The extracted feature profile could not be interpreted reliably."
  )
  evidence_text <- if (length(potential_concerns) > 0) {
    paste0(
      " Primary diagnostic concern",
      if (length(potential_concerns) == 1) ": " else "s: ",
      bold_sentence_list(potential_concerns),
      "."
    )
  } else if (length(supportive_indicators) > 0) {
    paste0(" Supportive indicators include ", bold_sentence_list(supportive_indicators), ".")
  } else {
    " No single diagnostic feature dominates the result; the score reflects the complete 49-feature profile."
  }
  paste0(
    "This series received an EpiFORESS score of <strong>",
    formatC(score, format = "f", digits = 1),
    " out of 100</strong>, corresponding to <strong>",
    class_label,
    "</strong>.",
    interpretation,
    evidence_text,
    " Based on the validation-set score calibration, this score corresponds ",
    "to an estimated median Best MASE of <strong>",
    formatC(
      estimated_best_mase,
      format = "f",
      digits = 2
    ),
    "</strong>. This secondary estimate does not replace direct candidate-model ",
    "fitting and rolling-origin validation.",
    " Recommended action: <strong>",
    action_label,
    "</strong>."
  )
}
