# EpiFORESS Analysis Pipeline
# Script: 01_empirical_characterization_simulation.R
# Project root: set EPIFORESS_PROJECT_DIR or run from the project data directory.
# Reproducibility: inspect all settings and verify input/model compatibility before reporting results.

# =============================================================================
# EPIFORESS: SUPPLEMENTARY MODEL-DEVELOPMENT DATA GENERATION
# Empirical characterization and disease-seeded Monte Carlo simulation
# Author: John Aaron B. Llobrera | Random seed: 2026
#
# Input columns: Disease, Year, Week, Cases
# Outputs: simulation settings, disease parameters, seasonal profiles, clean
# series, artifact-injected series, artifact metadata, and burden summaries.
# =============================================================================

# 0. PACKAGES -----------------------------------------------------------------

required_packages <- c(
  "readxl", "dplyr", "tidyr", "purrr", "forecast", "openxlsx", "tibble"
)

missing_packages <- setdiff(
  required_packages,
  rownames(utils::installed.packages())
)

if (length(missing_packages)) {
  stop(
    "Install these packages before running the script: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(forecast)
  library(openxlsx)
  library(tibble)
})

set.seed(2026)


# 1. USER SETTINGS ------------------------------------------------------------

# Update these paths for your computer.
project_dir <- normalizePath(
  Sys.getenv("EPIFORESS_PROJECT_DIR", unset = getwd()),
  winslash = "/",
  mustWork = FALSE
)

if (!dir.exists(project_dir)) {
  stop("Project directory does not exist: ", project_dir, call. = FALSE)
}

input_path <- file.path(project_dir, "External_Validation_Seed.xlsx")
output_path <- file.path(project_dir, "Disease_Seeded_MC_Simulation_REFINED.xlsx")

n_sim_per_disease <- 500L
series_length <- 260L
season_frequency <- 52L

artifact_prob <- list(
  missingness = 0.30,
  underreporting = 0.35,
  heaping = 0.25,
  delay = 0.25,
  zero_inflation = 0.30,
  repetition = 0.15,
  outbreak = 0.35,
  level_shift = 0.20
)

max_missingness_rate <- 0.25
max_underreporting_rate <- 0.50
max_outbreak_multiplier <- 10.0
max_artifacts_per_series <- 4L
trend_drift_sd <- 0.005

if (!file.exists(input_path)) {
  stop("Input file not found. Update `input_path`:\n", input_path, call. = FALSE)
}

output_dir <- dirname(output_path)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}
if (!dir.exists(output_dir)) {
  stop("Could not create output directory: ", output_dir, call. = FALSE)
}


# 2. LOAD AND VALIDATE DATA ---------------------------------------------------

required_columns <- c("Disease", "Year", "Week", "Cases")
seed_data <- readxl::read_excel(input_path)

missing_columns <- setdiff(required_columns, names(seed_data))
if (length(missing_columns)) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

seed_data <- seed_data %>%
  dplyr::mutate(
    Disease = as.character(.data$Disease),
    Year = suppressWarnings(as.integer(.data$Year)),
    Week = suppressWarnings(as.integer(.data$Week)),
    Cases = suppressWarnings(as.numeric(.data$Cases))
  ) %>%
  dplyr::filter(
    !is.na(.data$Disease),
    trimws(.data$Disease) != "",
    !is.na(.data$Year),
    !is.na(.data$Week),
    !is.na(.data$Cases),
    is.finite(.data$Cases),
    .data$Cases >= 0,
    .data$Week >= 1L,
    .data$Week <= season_frequency
  ) %>%
  dplyr::arrange(.data$Disease, .data$Year, .data$Week)

if (!nrow(seed_data)) {
  stop("No valid observations remain after input validation.", call. = FALSE)
}

duplicate_keys <- seed_data %>%
  dplyr::count(.data$Disease, .data$Year, .data$Week, name = "N") %>%
  dplyr::filter(.data$N > 1L)

if (nrow(duplicate_keys)) {
  stop(
    "Duplicate Disease-Year-Week records found. Resolve them before ",
    "simulation; duplicates are not aggregated automatically.",
    call. = FALSE
  )
}

message(
  "Loaded ", nrow(seed_data), " observations across ",
  dplyr::n_distinct(seed_data$Disease), " disease series."
)


# 3. EMPIRICAL CHARACTERIZATION -----------------------------------------------

characterize_disease <- function(df, freq = 52L) {
  df <- df %>% dplyr::arrange(.data$Year, .data$Week)
  y <- df$Cases
  n <- length(y)

  mean_incidence <- mean(y, na.rm = TRUE)
  variance_cases <- stats::var(y, na.rm = TRUE)

  if (!is.finite(mean_incidence) || mean_incidence <= 0) {
    mean_incidence <- 0.01
  }
  if (!is.finite(variance_cases)) {
    variance_cases <- 0
  }

  dispersion_index <- variance_cases / mean_incidence

  theta <- if (variance_cases > mean_incidence) {
    mean_incidence^2 / (variance_cases - mean_incidence)
  } else {
    1000
  }
  theta <- min(max(theta, 0.01), 500)

  acf_values <- tryCatch(
    stats::acf(
      y, plot = FALSE, lag.max = min(26L, n - 1L),
      na.action = na.pass
    )$acf[-1L],
    error = function(e) NA_real_
  )

  pacf_values <- tryCatch(
    stats::pacf(
      y, plot = FALSE, lag.max = min(26L, n - 1L),
      na.action = na.pass
    )$acf,
    error = function(e) NA_real_
  )

  acf_lag1 <- if (length(acf_values)) as.numeric(acf_values[1L]) else NA_real_
  pacf_lag1 <- if (length(pacf_values)) as.numeric(pacf_values[1L]) else NA_real_

  time_index <- seq_along(y)
  trend_slope <- tryCatch(
    unname(stats::coef(stats::lm(y ~ time_index))[2L]),
    error = function(e) NA_real_
  )

  # Complete the week-of-year profile to ensure every simulated week matches.
  weekly_profile <- df %>%
    dplyr::group_by(.data$Week) %>%
    dplyr::summarise(
      Weekly_Mean = mean(.data$Cases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::complete(
      Week = seq_len(freq),
      fill = list(Weekly_Mean = NA_real_)
    ) %>%
    dplyr::arrange(.data$Week) %>%
    dplyr::mutate(
      Seasonal_Multiplier = .data$Weekly_Mean / mean_incidence,
      Seasonal_Multiplier = dplyr::if_else(
        is.na(.data$Seasonal_Multiplier) |
          !is.finite(.data$Seasonal_Multiplier) |
          .data$Seasonal_Multiplier <= 0,
        1,
        .data$Seasonal_Multiplier
      ),
      Disease = as.character(df$Disease[1L])
    )

  seasonal_values <- weekly_profile$Seasonal_Multiplier
  smoothed_values <- as.numeric(
    stats::filter(seasonal_values, rep(1 / 3, 3), sides = 2)
  )
  bad_smooth <- is.na(smoothed_values) | !is.finite(smoothed_values)
  smoothed_values[bad_smooth] <- seasonal_values[bad_smooth]
  weekly_profile$Seasonal_Multiplier <- smoothed_values

  stl_fit <- tryCatch(
    stats::stl(stats::ts(y, frequency = freq),
               s.window = "periodic", robust = TRUE),
    error = function(e) NULL
  )

  if (!is.null(stl_fit)) {
    seasonal <- as.numeric(stl_fit$time.series[, "seasonal"])
    trend <- as.numeric(stl_fit$time.series[, "trend"])
    remainder <- as.numeric(stl_fit$time.series[, "remainder"])

    residual_variance <- stats::var(remainder, na.rm = TRUE)
    seasonal_denominator <- stats::var(seasonal + remainder, na.rm = TRUE)
    trend_denominator <- stats::var(trend + remainder, na.rm = TRUE)

    seasonal_strength <- if (
      is.finite(seasonal_denominator) && seasonal_denominator > 0
    ) {
      max(0, 1 - residual_variance / seasonal_denominator)
    } else {
      NA_real_
    }

    trend_strength <- if (is.finite(trend_denominator) && trend_denominator > 0) {
      max(0, 1 - residual_variance / trend_denominator)
    } else {
      NA_real_
    }
  } else {
    seasonal_strength <- NA_real_
    trend_strength <- NA_real_
  }

  list(
    summary = tibble::tibble(
      Disease = as.character(df$Disease[1L]),
      N_Observed = n,
      Mean_Incidence = mean_incidence,
      Variance = variance_cases,
      Dispersion_Index = dispersion_index,
      NB_Size_Theta = theta,
      Trend_Slope_Descriptive = trend_slope,
      Seasonal_Strength = seasonal_strength,
      Trend_Strength = trend_strength,
      ACF_Lag1 = acf_lag1,
      PACF_Lag1 = pacf_lag1,
      Prop_Zero_Seed = mean(y == 0, na.rm = TRUE)
    ),
    seasonal_profile = weekly_profile
  )
}

characterization <- seed_data %>%
  dplyr::group_by(.data$Disease) %>%
  dplyr::group_split() %>%
  purrr::map(characterize_disease, freq = season_frequency)

disease_parameters <- purrr::map_dfr(characterization, "summary")

seasonal_profiles <- purrr::map_dfr(characterization, "seasonal_profile") %>%
  dplyr::select(
    .data$Disease, .data$Week,
    .data$Weekly_Mean, .data$Seasonal_Multiplier
  )


# 4. SIMULATION INDEX AND CLEAN SERIES ----------------------------------------

simulation_index <- tidyr::expand_grid(
  Disease_Seed = disease_parameters$Disease,
  Disease_Simulation_ID = seq_len(n_sim_per_disease)
) %>%
  dplyr::mutate(Global_Simulation_ID = dplyr::row_number()) %>%
  dplyr::select(
    .data$Global_Simulation_ID,
    .data$Disease_Seed,
    .data$Disease_Simulation_ID
  )

simulate_clean_series <- function(
    Global_Simulation_ID, Disease_Seed, Disease_Simulation_ID) {

  params <- disease_parameters %>%
    dplyr::filter(.data$Disease == Disease_Seed)

  season_profile <- seasonal_profiles %>%
    dplyr::filter(.data$Disease == Disease_Seed) %>%
    dplyr::arrange(.data$Week)

  if (nrow(params) != 1L) {
    stop("Expected one parameter row for seed: ", Disease_Seed, call. = FALSE)
  }

  time_index <- seq_len(series_length)
  weeks <- rep(seq_len(season_frequency), length.out = series_length)

  seasonal_multiplier <- season_profile$Seasonal_Multiplier[
    match(weeks, season_profile$Week)
  ]
  seasonal_multiplier[
    is.na(seasonal_multiplier) | !is.finite(seasonal_multiplier)
  ] <- 1

  drift_slope <- stats::rnorm(1L, mean = 0, sd = trend_drift_sd)
  trend_multiplier <- exp(drift_slope * time_index)
  mu <- pmax(params$Mean_Incidence * seasonal_multiplier * trend_multiplier, 0.01)

  cases <- stats::rnbinom(
    n = series_length,
    size = params$NB_Size_Theta,
    mu = mu
  )

  tibble::tibble(
    Global_Simulation_ID = Global_Simulation_ID,
    Disease_Seed = Disease_Seed,
    Disease_Simulation_ID = Disease_Simulation_ID,
    Time = time_index,
    Week = weeks,
    Expected_Mean = as.numeric(mu),
    Clean_Cases = as.integer(cases),
    Simulated_Trend_Slope = drift_slope
  )
}

clean_simulated_data <- purrr::pmap_dfr(
  simulation_index,
  simulate_clean_series
)


# 5. ASSIGN ARTIFACTS ----------------------------------------------------------

assign_artifacts <- function(
    Global_Simulation_ID, Disease_Seed, Disease_Simulation_ID) {

  params <- disease_parameters %>%
    dplyr::filter(.data$Disease == Disease_Seed)

  zero_inflation_probability <- min(
    artifact_prob$zero_inflation +
      0.15 * as.integer(params$Prop_Zero_Seed > 0.20),
    0.60
  )

  fired <- c(
    missingness = stats::rbinom(1L, 1L, artifact_prob$missingness),
    underreporting = stats::rbinom(1L, 1L, artifact_prob$underreporting),
    heaping = stats::rbinom(1L, 1L, artifact_prob$heaping),
    delay = stats::rbinom(1L, 1L, artifact_prob$delay),
    zero_inflation = stats::rbinom(1L, 1L, zero_inflation_probability),
    repetition = stats::rbinom(1L, 1L, artifact_prob$repetition),
    outbreak = stats::rbinom(1L, 1L, artifact_prob$outbreak),
    level_shift = stats::rbinom(1L, 1L, artifact_prob$level_shift)
  )

  # Cap co-occurrence at four artifact types. If the cap is exceeded, retain a
  # random subset so that a particular artifact is not always favored.
  fired_names <- names(fired)[fired == 1L]
  if (length(fired_names) > max_artifacts_per_series) {
    retained <- sample(fired_names, max_artifacts_per_series)
    fired[] <- 0L
    fired[retained] <- 1L
  }

  missingness_rate <- if (fired["missingness"] == 1L) {
    sample(
      c(0.05, 0.10, 0.15, 0.20, max_missingness_rate),
      1L, prob = c(0.30, 0.30, 0.20, 0.15, 0.05)
    )
  } else 0

  underreporting_rate <- if (fired["underreporting"] == 1L) {
    sample(
      c(0.10, 0.20, 0.30, 0.40, max_underreporting_rate),
      1L, prob = c(0.30, 0.30, 0.20, 0.15, 0.05)
    )
  } else 0

  heaping_level <- if (fired["heaping"] == 1L) {
    sample(c("mild", "moderate", "severe"), 1L,
           prob = c(0.55, 0.35, 0.10))
  } else "none"

  delay_weeks <- if (fired["delay"] == 1L) {
    sample(c(1L, 2L, 4L, 8L), 1L, prob = c(0.45, 0.30, 0.15, 0.10))
  } else 0L

  zero_inflation_rate <- if (fired["zero_inflation"] == 1L) {
    sample(c(0.05, 0.10, 0.15, 0.20, 0.30), 1L,
           prob = c(0.30, 0.30, 0.20, 0.15, 0.05))
  } else 0

  repetition_rate <- if (fired["repetition"] == 1L) {
    sample(c(0.05, 0.10, 0.15, 0.20, 0.30), 1L,
           prob = c(0.35, 0.30, 0.20, 0.10, 0.05))
  } else 0

  outbreak_level <- if (fired["outbreak"] == 1L) {
    sample(c("mild", "moderate", "severe", "extreme"), 1L,
           prob = c(0.40, 0.35, 0.20, 0.05))
  } else "none"

  level_shift <- if (fired["level_shift"] == 1L) {
    sample(c("down", "up"), 1L, prob = c(0.55, 0.45))
  } else "none"

  tibble::tibble(
    Global_Simulation_ID = Global_Simulation_ID,
    Disease_Seed = Disease_Seed,
    Disease_Simulation_ID = Disease_Simulation_ID,
    Missingness_Rate = missingness_rate,
    Underreporting_Rate = underreporting_rate,
    Heaping_Level = heaping_level,
    Reporting_Delay_Weeks = delay_weeks,
    Zero_Inflation_Rate = zero_inflation_rate,
    Repetition_Rate = repetition_rate,
    Outbreak_Level = outbreak_level,
    Level_Shift = level_shift
  )
}

artifact_metadata <- purrr::pmap_dfr(simulation_index, assign_artifacts)


# 6. ARTIFACT INJECTION FUNCTIONS ---------------------------------------------

inject_underreporting <- function(y, rate) {
  round(y * (1 - rate))
}

inject_zero_inflation <- function(y, rate) {
  eligible <- which(!is.na(y) & y > 0)
  n_to_zero <- floor(length(eligible) * rate)
  if (n_to_zero > 0L) y[sample(eligible, n_to_zero)] <- 0
  y
}

inject_missingness <- function(y, rate) {
  eligible <- which(!is.na(y))
  n_to_remove <- floor(length(eligible) * rate)
  if (n_to_remove > 0L) y[sample(eligible, n_to_remove)] <- NA_real_
  y
}

inject_heaping <- function(y, level) {
  if (level == "none") return(y)

  eligible <- which(!is.na(y) & y > 0)
  if (!length(eligible)) return(y)

  if (level == "mild") {
    proportion <- 0.10
    base <- 5
  } else if (level == "moderate") {
    proportion <- 0.25
    base <- sample(c(5, 10), 1L)
  } else {
    proportion <- 0.40
    base <- sample(c(10, 50), 1L)
  }

  n_to_heap <- floor(length(eligible) * proportion)
  if (n_to_heap > 0L) {
    idx <- sample(eligible, n_to_heap)
    y[idx] <- round(y[idx] / base) * base
  }
  y
}

inject_reporting_delay <- function(y, delay_weeks) {
  if (delay_weeks <= 0L) return(y)

  # Transfer a portion of each eligible original count forward. The original
  # vector is used to prevent already-transferred counts from being delayed
  # again later in this same pass.
  original_y <- y
  y_new <- y
  eligible <- which(!is.na(original_y) & original_y > 0)
  eligible <- eligible[eligible + delay_weeks <= length(original_y)]

  for (i in eligible) {
    delayed_cases <- round(original_y[i] * stats::runif(1L, 0.20, 0.60))
    target <- i + delay_weeks
    y_new[i] <- y_new[i] - delayed_cases
    if (!is.na(y_new[target])) {
      y_new[target] <- y_new[target] + delayed_cases
    }
  }
  y_new
}

inject_repetition <- function(y, rate) {
  possible_idx <- which(!is.na(y))
  possible_idx <- possible_idx[possible_idx > 1L]
  n_to_repeat <- min(floor(length(y) * rate), length(possible_idx))

  if (n_to_repeat > 0L) {
    idx <- sample(possible_idx, n_to_repeat)
    for (i in idx) y[i] <- y[i - 1L]
  }
  y
}

inject_outbreak <- function(y, level) {
  if (level == "none") return(y)

  duration <- switch(
    level,
    mild = sample(2:3, 1L),
    moderate = sample(3:5, 1L),
    severe = sample(4:8, 1L),
    extreme = sample(6:10, 1L),
    3L
  )
  duration <- min(duration, length(y) - 1L)
  onset <- sample(seq_len(length(y) - duration + 1L), 1L)
  idx <- onset:(onset + duration - 1L)

  multiplier <- switch(
    level,
    mild = stats::runif(1L, 1.5, 2.5),
    moderate = stats::runif(1L, 2.5, 5.0),
    severe = stats::runif(1L, 5.0, 8.0),
    extreme = stats::runif(1L, 8.0, max_outbreak_multiplier),
    1
  )

  y[idx] <- round(y[idx] * multiplier)
  y
}

inject_level_shift <- function(y, level_shift) {
  if (level_shift == "none") return(y)

  onset_min <- max(1L, floor(0.20 * length(y)))
  onset_max <- max(onset_min, floor(0.75 * length(y)))
  onset <- sample(onset_min:onset_max, 1L)

  multiplier <- if (level_shift == "down") {
    stats::runif(1L, 0.35, 0.70)
  } else {
    stats::runif(1L, 1.30, 2.50)
  }

  y[onset:length(y)] <- round(y[onset:length(y)] * multiplier)
  y
}


# 7. APPLY ARTIFACTS -----------------------------------------------------------

apply_artifacts_to_series <- function(df, meta) {
  y <- df$Clean_Cases

  # Structural changes -> reporting changes -> count artifacts -> missingness.
  y <- inject_level_shift(y, meta$Level_Shift[[1L]])
  y <- inject_outbreak(y, meta$Outbreak_Level[[1L]])
  y <- inject_underreporting(y, meta$Underreporting_Rate[[1L]])
  y <- inject_reporting_delay(y, meta$Reporting_Delay_Weeks[[1L]])
  y <- inject_zero_inflation(y, meta$Zero_Inflation_Rate[[1L]])
  y <- inject_heaping(y, meta$Heaping_Level[[1L]])
  y <- inject_repetition(y, meta$Repetition_Rate[[1L]])
  y <- inject_missingness(y, meta$Missingness_Rate[[1L]])

  dplyr::mutate(df, Observed_Cases = as.numeric(y))
}

# Split once by simulation ID rather than repeatedly filtering the full metadata
# table for every series.
clean_by_id <- split(clean_simulated_data, clean_simulated_data$Global_Simulation_ID)
metadata_by_id <- split(artifact_metadata, artifact_metadata$Global_Simulation_ID)

observed_simulated_data <- purrr::map_dfr(names(clean_by_id), function(id) {
  df <- clean_by_id[[id]]
  meta <- metadata_by_id[[id]]

  if (is.null(meta) || nrow(meta) != 1L) {
    stop("Missing or duplicated artifact metadata for simulation ID ", id,
         call. = FALSE)
  }

  apply_artifacts_to_series(df, meta)
})


# 8. ARTIFACT BURDEN -----------------------------------------------------------

artifact_metadata <- artifact_metadata %>%
  dplyr::mutate(
    Missingness_Burden = .data$Missingness_Rate,
    Underreporting_Burden = .data$Underreporting_Rate,
    Zero_Inflation_Burden = .data$Zero_Inflation_Rate,
    Repetition_Burden = .data$Repetition_Rate,

    Heaping_Burden = dplyr::case_when(
      .data$Heaping_Level == "none" ~ 0,
      .data$Heaping_Level == "mild" ~ 0.10,
      .data$Heaping_Level == "moderate" ~ 0.25,
      .data$Heaping_Level == "severe" ~ 0.40,
      TRUE ~ 0
    ),

    Delay_Burden = .data$Reporting_Delay_Weeks / 8,

    Outbreak_Burden = dplyr::case_when(
      .data$Outbreak_Level == "none" ~ 0,
      .data$Outbreak_Level == "mild" ~ 0.20,
      .data$Outbreak_Level == "moderate" ~ 0.45,
      .data$Outbreak_Level == "severe" ~ 0.70,
      .data$Outbreak_Level == "extreme" ~ 1.00,
      TRUE ~ 0
    ),

    Level_Shift_Burden = dplyr::case_when(
      .data$Level_Shift == "none" ~ 0,
      .data$Level_Shift %in% c("down", "up") ~ 0.75,
      TRUE ~ 0
    ),

    Artifact_Count =
      as.integer(.data$Missingness_Rate > 0) +
      as.integer(.data$Underreporting_Rate > 0) +
      as.integer(.data$Heaping_Level != "none") +
      as.integer(.data$Reporting_Delay_Weeks > 0) +
      as.integer(.data$Zero_Inflation_Rate > 0) +
      as.integer(.data$Repetition_Rate > 0) +
      as.integer(.data$Outbreak_Level != "none") +
      as.integer(.data$Level_Shift != "none"),

    Artifact_Burden_Score =
      .data$Missingness_Burden +
      .data$Underreporting_Burden +
      .data$Zero_Inflation_Burden +
      .data$Repetition_Burden +
      .data$Heaping_Burden +
      .data$Delay_Burden +
      .data$Outbreak_Burden +
      .data$Level_Shift_Burden,

    Artifact_Burden_Class = dplyr::case_when(
      .data$Artifact_Count == 0L ~ "Clean",
      .data$Artifact_Burden_Score <= 0.50 ~ "Low",
      .data$Artifact_Burden_Score <= 1.25 ~ "Moderate",
      TRUE ~ "High"
    )
  )

surveillance_quality_profile <- artifact_metadata %>%
  dplyr::group_by(.data$Artifact_Burden_Class, .data$Artifact_Count) %>%
  dplyr::summarise(
    Number_of_Series = dplyr::n(),
    Mean_Burden_Score = mean(.data$Artifact_Burden_Score),
    .groups = "drop"
  ) %>%
  dplyr::arrange(.data$Artifact_Count)

artifact_distribution <- artifact_metadata %>%
  dplyr::count(.data$Artifact_Burden_Class, name = "Frequency") %>%
  dplyr::mutate(
    Percent = round(100 * .data$Frequency / sum(.data$Frequency), 2)
  )


# 9. RECORD SETTINGS -----------------------------------------------------------

simulation_settings <- tibble::tibble(
  Setting = c(
    "n_sim_per_disease", "series_length", "season_frequency",
    "trend_drift_sd", "max_artifacts_per_series",
    "max_missingness_rate", "max_underreporting_rate",
    "max_outbreak_multiplier",
    "artifact_prob_missingness", "artifact_prob_underreporting",
    "artifact_prob_heaping", "artifact_prob_delay",
    "artifact_prob_zero_inflation", "artifact_prob_repetition",
    "artifact_prob_outbreak", "artifact_prob_level_shift",
    "input_path", "output_path", "random_seed"
  ),
  Value = as.character(c(
    n_sim_per_disease, series_length, season_frequency,
    trend_drift_sd, max_artifacts_per_series,
    max_missingness_rate, max_underreporting_rate,
    max_outbreak_multiplier,
    artifact_prob$missingness, artifact_prob$underreporting,
    artifact_prob$heaping, artifact_prob$delay,
    artifact_prob$zero_inflation, artifact_prob$repetition,
    artifact_prob$outbreak, artifact_prob$level_shift,
    input_path, output_path, 2026
  ))
)


# 10. EXPORT TO EXCEL ----------------------------------------------------------

sheets <- list(
  Simulation_Settings = simulation_settings,
  Disease_Parameters = disease_parameters,
  Seasonal_Profiles = seasonal_profiles,
  Clean_Simulated_Data = clean_simulated_data,
  Observed_Simulated_Data = observed_simulated_data,
  Artifact_Metadata = artifact_metadata,
  Quality_Profile = surveillance_quality_profile,
  Artifact_Distribution = artifact_distribution
)

workbook <- openxlsx::createWorkbook()

for (sheet_name in names(sheets)) {
  openxlsx::addWorksheet(workbook, sheet_name)
  openxlsx::writeData(
    workbook, sheet = sheet_name, x = sheets[[sheet_name]],
    withFilter = TRUE
  )
  openxlsx::freezePane(workbook, sheet = sheet_name, firstRow = TRUE)
}

openxlsx::saveWorkbook(workbook, output_path, overwrite = TRUE)


# 11. RUN SUMMARY --------------------------------------------------------------

cat("\n", strrep("=", 72), "\n", sep = "")
cat("EPIFORESS MODEL-DEVELOPMENT DATA GENERATION COMPLETE\n")
cat(strrep("=", 72), "\n")
cat("Output file: ", output_path, "\n", sep = "")
cat("Disease seeds: ", dplyr::n_distinct(disease_parameters$Disease), "\n", sep = "")
cat("Simulations per disease: ", n_sim_per_disease, "\n", sep = "")
cat("Total simulated series: ", nrow(artifact_metadata), "\n", sep = "")
cat("Series length: ", series_length, " observations\n", sep = "")
cat("Total rows in observed-series data: ", nrow(observed_simulated_data), "\n", sep = "")
cat("\nArtifact burden distribution:\n")
print(artifact_distribution)
cat(
  "\nMean artifact count per series: ",
  round(mean(artifact_metadata$Artifact_Count), 2), "\n", sep = ""
)
cat(
  "Proportion of clean series: ",
  round(mean(artifact_metadata$Artifact_Count == 0L), 3), "\n", sep = ""
)
cat(strrep("=", 72), "\n")
