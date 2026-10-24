# EpiFORESS | 08_server.R
# Shiny server logic, outputs, observers, and downloads.
# Split from app_full_fixed.R; code logic preserved during file separation.

server <- function(input, output, session) {
  has_data <- reactive({
    !is.null(input$file)
  })
  
  # Set to TRUE by the Cancel button while a batch is being scored.
  # Checked between series inside batch_results() so a run in progress
  # can stop early without waiting for every remaining series.
  cancel_requested <- reactiveVal(FALSE)
  batch_processing <- reactiveVal(FALSE)

  # Final completed or cancelled batch shown by the app.
  batch_results_value <- reactiveVal(NULL)

  # Internal mutable state for the currently active batch.
  batch_job_state <- reactiveVal(NULL)

  # Invalidates callbacks left over from a previous Run click.
  batch_run_token <- reactiveVal(0L)

  batch_results <- reactive({
    batch_results_value()
  })

  observeEvent(input$cancel_scoring, {

    if (!isTRUE(batch_processing())) {
      return()
    }

    cancel_requested(TRUE)

    session$sendCustomMessage(
      "epiforessUpdateOverlay",
      list(
        text = "Cancellation requested. Finishing the current series...",
        percent = NULL
      )
    )

    showNotification(
      paste(
        "Cancellation requested.",
        "The current series will finish and all remaining series will be skipped."
      ),
      type = "warning",
      duration = 5
    )

  }, ignoreInit = TRUE)

  observeEvent(input$fullscreen_series_plot, {
    session$sendCustomMessage(
      type = "toggleFullscreen",
      message = "series_plot_fullscreen_container"
    )
  })
  
  observeEvent(input$run, {
    req(has_data())
    bslib::nav_select(id = "main_nav", selected = "Screening Results", session = session)
  }, ignoreInit = TRUE)
  
  stat_model <- reactive({
    validate(need(
      !is.null(stat_model_obj),
      paste0("The statistical model could not be loaded from: ", stat_model_path)
    ))
    stat_model_obj
  })
  
  ml_model <- reactive({
    validate(need(
      !is.null(ml_model_obj),
      paste0("The machine-learning model could not be loaded from: ", ml_model_path)
    ))
    ml_model_obj
  })
  
  output$sheet_selector <- renderUI({
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    if (!ext %in% c("xlsx", "xls")) return(NULL)
    sheets <- readxl::excel_sheets(input$file$datapath)
    tagList(
      selectizeInput(
        "sheet_name",
        "Sheet(s)",
        choices = sheets,
        selected = sheets[1],
        multiple = TRUE,
        width = "100%",
        options = list(placeholder = "Select one or more sheets")
      ),
      div(
        class = "select-all-row",
        actionLink("select_all_sheets", "Select all"),
        span(class = "select-all-sep", "\u00b7"),
        actionLink("clear_all_sheets", "Clear")
      )
    )
  })
  
  observeEvent(input$select_all_sheets, {
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    req(ext %in% c("xlsx", "xls"))
    sheets <- readxl::excel_sheets(input$file$datapath)
    updateSelectizeInput(session, "sheet_name", selected = sheets)
  })
  
  observeEvent(input$clear_all_sheets, {
    updateSelectizeInput(session, "sheet_name", selected = character(0))
  })
  
  uploaded_data <- reactive({
    req(input$file)
    
    ext <- tolower(
      tools::file_ext(input$file$name)
    )
    
    if (ext == "csv") {
      return(
        readr::read_csv(
          input$file$datapath,
          show_col_types = FALSE
        )
      )
    }
    
    if (ext %in% c("xlsx", "xls")) {
      req(input$sheet_name)
      
      sheets_to_read <- unique(
        as.character(input$sheet_name)
      )
      
      validate(need(
        length(sheets_to_read) > 0,
        "Select at least one sheet to load."
      ))
      
      data_list <- lapply(
        sheets_to_read,
        function(sh) {
          readxl::read_excel(
            path = input$file$datapath,
            sheet = sh
          )
        }
      )
      
      return(
        dplyr::bind_rows(data_list)
      )
    }
    
    validate(need(
      FALSE,
      "Please upload a CSV or Excel file."
    ))
  })
  
  output$column_selector <- renderUI({
    req(uploaded_data())
    data <- uploaded_data()
    numeric_columns <- names(data)[sapply(data, function(x) is.numeric(x) || is.integer(x))]
    selected_column <- dplyr::case_when(
      "Count" %in% names(data) ~ "Count",
      "Cases" %in% names(data) ~ "Cases",
      "Observed_Cases" %in% names(data) ~ "Observed_Cases",
      "Reported_Cases" %in% names(data) ~ "Reported_Cases",
      length(numeric_columns) > 0 ~ numeric_columns[1],
      TRUE ~ names(data)[1]
    )
    
    selectInput(
      "count_column",
      "Select disease count column",
      choices = names(data),
      selected = selected_column,
      width = "100%"
    )
  })
  
  output$disease_selector <- renderUI({
    req(uploaded_data())
    data <- uploaded_data()
    default_disease_column <- dplyr::case_when(
      "Disease" %in% names(data) ~ "Disease",
      "disease" %in% names(data) ~ "disease",
      "Disease_Seed" %in% names(data) ~ "Disease_Seed",
      "Disease_Name" %in% names(data) ~ "Disease_Name",
      TRUE ~ "None"
    )
    
    tagList(
      selectInput(
        "disease_column",
        "Disease or series column",
        choices = c("None", names(data)),
        selected = default_disease_column,
        width = "100%"
      ),
      uiOutput("disease_value_selector")
    )
  })
  
  output$disease_value_selector <- renderUI({
    req(uploaded_data(), input$disease_column)
    if (input$disease_column == "None") return(NULL)
    data <- uploaded_data()
    disease_values <- sort(unique(as.character(data[[input$disease_column]])))
    disease_values <- disease_values[!is.na(disease_values) & disease_values != ""]
    
    if (length(disease_values) == 0) {
      return(p(class = "small-note", "No valid disease or series values were detected."))
    }
    
    tagList(
      selectizeInput(
        "selected_diseases",
        "Select series to screen",
        choices = disease_values,
        selected = head(disease_values, 1),
        multiple = TRUE,
        width = "100%",
        options = list(
          maxItems = max_batch_series,
          placeholder = paste0("Choose up to ", max_batch_series, " series")
        )
      ),
      div(
        class = "select-all-row",
        actionLink("select_all_series", "Select all"),
        span(class = "select-all-sep", "\u00b7"),
        actionLink("clear_all_series", "Clear"),
        if (length(disease_values) > max_batch_series) {
          span(
            class = "select-all-note",
            paste0(
              "Only the first ", max_batch_series,
              " of ", length(disease_values), " series will be selected."
            )
          )
        }
      )
    )
  })
  
  observeEvent(input$select_all_series, {
    req(uploaded_data(), input$disease_column)
    req(input$disease_column != "None")
    data <- uploaded_data()
    disease_values <- sort(unique(as.character(data[[input$disease_column]])))
    disease_values <- disease_values[!is.na(disease_values) & disease_values != ""]
    req(length(disease_values) > 0)
    updateSelectizeInput(
      session,
      "selected_diseases",
      selected = head(disease_values, max_batch_series)
    )
  })
  
  observeEvent(input$clear_all_series, {
    updateSelectizeInput(session, "selected_diseases", selected = character(0))
  })
  
  # --------------------------------------------------------
  # OPTIONAL SECOND GROUPING VARIABLE
  # (e.g., state, province, facility). When set, each series
  # screened is a combination of the primary series value and
  # one value of this second grouping variable.
  # --------------------------------------------------------
  output$group2_selector <- renderUI({
    req(uploaded_data(), input$disease_column)
    if (input$disease_column == "None") return(NULL)
    data <- uploaded_data()
    candidate_columns <- setdiff(names(data), input$disease_column)
    default_group2_column <- dplyr::case_when(
      "State" %in% candidate_columns ~ "State",
      "Province" %in% candidate_columns ~ "Province",
      "Facility" %in% candidate_columns ~ "Facility",
      "Region" %in% candidate_columns ~ "Region",
      "Site" %in% candidate_columns ~ "Site",
      TRUE ~ "None"
    )
    
    tagList(
      selectInput(
        "group2_column",
        "Additional grouping variable (optional)",
        choices = c("None", candidate_columns),
        selected = default_group2_column,
        width = "100%"
      ),
      uiOutput("group2_value_selector")
    )
  })
  
  output$group2_value_selector <- renderUI({
    req(uploaded_data(), input$group2_column)
    if (is.null(input$group2_column) || input$group2_column == "None") return(NULL)
    data <- uploaded_data()
    req(input$group2_column %in% names(data))
    group2_values <- sort(unique(as.character(data[[input$group2_column]])))
    group2_values <- group2_values[!is.na(group2_values) & group2_values != ""]
    
    if (length(group2_values) == 0) {
      return(p(class = "small-note", "No valid values were detected for the additional grouping variable."))
    }
    
    tagList(
      selectizeInput(
        "selected_group2_values",
        "Select values to include",
        choices = group2_values,
        selected = head(group2_values, 1),
        multiple = TRUE,
        width = "100%",
        options = list(
          placeholder = "Choose one or more values"
        )
      ),
      div(
        class = "select-all-row",
        actionLink("select_all_group2", "Select all"),
        span(class = "select-all-sep", "\u00b7"),
        actionLink("clear_all_group2", "Clear")
      ),
      p(
        class = "small-note",
        paste0(
          "Each combination of the series and this grouping variable is screened as its own series ",
          "(for example, \"Measles \u2014 Region IV-A\"). The total number of combinations cannot exceed ",
          max_batch_series, "."
        )
      )
    )
  })
  
  observeEvent(input$select_all_group2, {
    req(uploaded_data(), input$group2_column)
    req(input$group2_column != "None")
    data <- uploaded_data()
    group2_values <- sort(unique(as.character(data[[input$group2_column]])))
    group2_values <- group2_values[!is.na(group2_values) & group2_values != ""]
    req(length(group2_values) > 0)
    updateSelectizeInput(session, "selected_group2_values", selected = group2_values)
  })
  
  observeEvent(input$clear_all_group2, {
    updateSelectizeInput(session, "selected_group2_values", selected = character(0))
  })
  
  output$year_filter_selector <- renderUI({
    req(uploaded_data())
    data <- uploaded_data()
    default_year_column <- dplyr::case_when(
      "Year" %in% names(data) ~ "Year",
      "year" %in% names(data) ~ "year",
      "Epi_Year" %in% names(data) ~ "Epi_Year",
      "Reporting_Year" %in% names(data) ~ "Reporting_Year",
      "Date" %in% names(data) ~ "Date",
      "date" %in% names(data) ~ "date",
      TRUE ~ "None"
    )
    default_week_column <- dplyr::case_when(
      "Week" %in% names(data) ~ "Week",
      "week" %in% names(data) ~ "week",
      "Epi_Week" %in% names(data) ~ "Epi_Week",
      "Reporting_Week" %in% names(data) ~ "Reporting_Week",
      TRUE ~ "None"
    )
    
    tagList(
      selectInput(
        "year_column",
        "Year column",
        choices = c("None", names(data)),
        selected = default_year_column,
        width = "100%"
      ),
      uiOutput("year_range_selector"),
      selectInput(
        "week_column",
        "Week column",
        choices = c("None", names(data)),
        selected = default_week_column,
        width = "100%"
      ),
      uiOutput("week_range_selector")
    )
  })
  
  output$year_range_selector <- renderUI({
    req(uploaded_data(), input$year_column)
    if (input$year_column == "None") return(NULL)
    data <- uploaded_data()
    
    if (
      !is.null(input$disease_column) && input$disease_column != "None" &&
      input$disease_column %in% names(data) &&
      !is.null(input$selected_diseases) && length(input$selected_diseases) > 0
    ) {
      data <- data %>%
        dplyr::filter(as.character(.data[[input$disease_column]]) %in% input$selected_diseases)
    }
    
    if (
      !is.null(input$group2_column) && input$group2_column != "None" &&
      input$group2_column %in% names(data) &&
      !is.null(input$selected_group2_values) && length(input$selected_group2_values) > 0
    ) {
      data <- data %>%
        dplyr::filter(as.character(.data[[input$group2_column]]) %in% input$selected_group2_values)
    }
    
    validate(need(
      input$year_column %in% names(data),
      "Selected year column was not found in the loaded data."
    ))
    
    extracted_years <- extract_year_safe(data[[input$year_column]])
    valid_years <- sort(unique(extracted_years[is.finite(extracted_years)]))
    if (length(valid_years) == 0) {
      return(p(class = "small-note", "No valid years detected for the selected data."))
    }
    
    y_min <- floor(min(valid_years))
    y_max <- ceiling(max(valid_years))
    sliderInput(
      "year_range",
      "Years covered",
      min = y_min,
      max = y_max,
      value = c(y_min, y_max),
      step = 1,
      sep = "",
      width = "100%"
    )
  })
  
  output$week_range_selector <- renderUI({
    req(uploaded_data(), input$week_column)
    if (input$week_column == "None") return(NULL)
    data <- uploaded_data()
    extracted_weeks <- suppressWarnings(as.numeric(data[[input$week_column]]))
    valid_weeks <- sort(unique(extracted_weeks[!is.na(extracted_weeks)]))
    if (length(valid_weeks) == 0) {
      return(p(class = "small-note", "No valid weeks detected."))
    }
    
    sliderInput(
      "week_range",
      "Weeks covered",
      min = min(valid_weeks),
      max = max(valid_weeks),
      value = c(min(valid_weeks), max(valid_weeks)),
      step = 1,
      sep = "",
      width = "100%"
    )
  })
  
  apply_time_filters <- function(data) {
    if (
      !is.null(input$year_column) && input$year_column != "None" &&
      input$year_column %in% names(data) && !is.null(input$year_range)
    ) {
      extracted_years <- extract_year_safe(data[[input$year_column]])
      data <- data %>%
        dplyr::mutate(.EpiFORESS_Year = extracted_years) %>%
        dplyr::filter(
          !is.na(.EpiFORESS_Year),
          .EpiFORESS_Year >= input$year_range[1],
          .EpiFORESS_Year <= input$year_range[2]
        )
    }
    
    if (
      !is.null(input$week_column) && input$week_column != "None" &&
      input$week_column %in% names(data)
    ) {
      data <- data %>%
        dplyr::mutate(.EpiFORESS_Week = suppressWarnings(as.numeric(.data[[input$week_column]])))
      if (!is.null(input$week_range)) {
        data <- data %>%
          dplyr::filter(
            !is.na(.EpiFORESS_Week),
            .EpiFORESS_Week >= input$week_range[1],
            .EpiFORESS_Week <= input$week_range[2]
          )
      }
    }
    data
  }
  
  series_inputs <- reactive({
    req(uploaded_data(), input$count_column)
    data <- uploaded_data()
    validate(need(
      input$count_column %in% names(data),
      "Selected count column was not found in the loaded data."
    ))
    
    if (is.null(input$disease_column) || input$disease_column == "None") {
      data_one <- apply_time_filters(data) %>%
        dplyr::mutate(Count_Selected = safe_numeric(.data[[input$count_column]]))
      validate(need(
        sum(!is.na(data_one$Count_Selected)) >= 20,
        "Selected series must contain at least 20 valid numeric observations."
      ))
      return(list("Loaded Series" = data_one))
    }
    
    validate(need(
      input$disease_column %in% names(data),
      "Selected disease or series column was not found in the loaded data."
    ))
    
    selected_g1 <- unique(as.character(input$selected_diseases))
    validate(need(length(selected_g1) >= 1, "Select at least one series to screen."))
    
    use_group2 <- !is.null(input$group2_column) && input$group2_column != "None"
    
    # ------------------------------------------------------
    # Primary grouping variable combined with an optional
    # second grouping variable (e.g., state, province, facility).
    # Each combination of the two becomes its own screened series.
    # ------------------------------------------------------
    if (use_group2) {
      validate(need(
        input$group2_column %in% names(data),
        "Selected additional grouping column was not found in the loaded data."
      ))
      selected_g2 <- unique(as.character(input$selected_group2_values))
      validate(need(
        length(selected_g2) >= 1,
        "Select at least one value for the additional grouping variable."
      ))
      
      combo_data <- data %>%
        dplyr::filter(
          as.character(.data[[input$disease_column]]) %in% selected_g1,
          as.character(.data[[input$group2_column]]) %in% selected_g2
        ) %>%
        apply_time_filters()
      
      combo_keys <- combo_data %>%
        dplyr::mutate(
          .g1 = as.character(.data[[input$disease_column]]),
          .g2 = as.character(.data[[input$group2_column]])
        ) %>%
        dplyr::distinct(.g1, .g2) %>%
        dplyr::mutate(Series_Key = paste0(.g1, " \u2014 ", .g2)) %>%
        dplyr::arrange(.g1, .g2)
      
      validate(need(
        nrow(combo_keys) >= 1,
        "No combinations of the selected series and grouping values were found."
      ))
      validate(need(
        nrow(combo_keys) <= max_batch_series,
        paste0(
          "The selected combination of grouping variables produces ", nrow(combo_keys),
          " series, exceeding the limit of ", max_batch_series, ". Narrow your selection."
        )
      ))
      
      out <- purrr::pmap(
        list(combo_keys$.g1, combo_keys$.g2, combo_keys$Series_Key),
        function(g1_val, g2_val, series_key) {
          z <- combo_data %>%
            dplyr::filter(
              as.character(.data[[input$disease_column]]) == g1_val,
              as.character(.data[[input$group2_column]]) == g2_val
            ) %>%
            dplyr::mutate(Count_Selected = safe_numeric(.data[[input$count_column]]))
          if (all(c(".EpiFORESS_Year", ".EpiFORESS_Week") %in% names(z))) {
            z <- dplyr::arrange(z, .EpiFORESS_Year, .EpiFORESS_Week)
          } else if (".EpiFORESS_Year" %in% names(z)) {
            z <- dplyr::arrange(z, .EpiFORESS_Year)
          } else if (".EpiFORESS_Week" %in% names(z)) {
            z <- dplyr::arrange(z, .EpiFORESS_Week)
          }
          z
        }
      )
      names(out) <- combo_keys$Series_Key
      return(out)
    }
    
    # ------------------------------------------------------
    # Primary grouping variable only (no second grouping variable).
    # ------------------------------------------------------
    validate(need(
      length(selected_g1) <= max_batch_series,
      paste0("You may screen up to ", max_batch_series, " series at the same time.")
    ))
    
    data <- data %>%
      dplyr::filter(as.character(.data[[input$disease_column]]) %in% selected_g1) %>%
      apply_time_filters()
    
    out <- lapply(selected_g1, function(series_name) {
      z <- data %>%
        dplyr::filter(as.character(.data[[input$disease_column]]) == series_name) %>%
        dplyr::mutate(Count_Selected = safe_numeric(.data[[input$count_column]]))
      
      if (all(c(".EpiFORESS_Year", ".EpiFORESS_Week") %in% names(z))) {
        z <- dplyr::arrange(z, .EpiFORESS_Year, .EpiFORESS_Week)
      } else if (".EpiFORESS_Year" %in% names(z)) {
        z <- dplyr::arrange(z, .EpiFORESS_Year)
      } else if (".EpiFORESS_Week" %in% names(z)) {
        z <- dplyr::arrange(z, .EpiFORESS_Week)
      }
      z
    })
    names(out) <- selected_g1
    out
  })
  
  score_one_series <- function(
    series_name,
    series_data,
    frequency_value,
    stat_model_loaded,
    ml_model_loaded
  ) {
    y <- series_data$Count_Selected
    n_valid <- sum(!is.na(y))
    fail_summary <- function(message) {
      tibble::tibble(
        Series_Name = series_name,
        Rank = NA_integer_,
        N_Observations = n_valid,
        EpiFORESS = NA_real_,
        Estimated_Median_Best_MASE = NA_real_,
        Readiness_Level = "Unclassified",
        Deployment_Action = "Cannot assess this series yet; check the input data first",
        Supportive_Indicators = NA_character_,
        Secondary_Notes = NA_character_,
        Secondary_Screening_Notes = NA_character_,
        Score_Status = message
      )
    }
    if (n_valid < 20) {
      return(list(
        summary = fail_summary(
          "Failed: fewer than 20 valid numeric observations."
        ),
        detail = NULL
      ))
    }
    if (any(is.finite(y) & y < 0)) {
      return(list(
        summary = fail_summary(
          "Failed: count data cannot contain negative values."
        ),
        detail = NULL
      ))
    }
    feature_error <- NULL
    raw_features <- tryCatch(
      compute_raw_features_display(
        y,
        frequency_value
      ),
      error = function(e) {
        feature_error <<- conditionMessage(e)
        NULL
      }
    )
    if (is.null(raw_features)) {
      return(list(
        summary = fail_summary(
          paste0(
            "Failed during feature extraction: ",
            feature_error %||% "unknown error"
          )
        ),
        detail = NULL
      ))
    }
    transform_error <- NULL
    model_features <- tryCatch(
      transform_raw_features_for_model(
        raw_features
      ),
      error = function(e) {
        transform_error <<- conditionMessage(e)
        NULL
      }
    )
    if (is.null(model_features)) {
      return(list(
        summary = fail_summary(
          paste0(
            "Failed while preparing extracted features: ",
            transform_error %||% "unknown error"
          )
        ),
        detail = NULL
      ))
    }
    stat_prepare_error <- NULL
    ml_prepare_error <- NULL
    stat_x <- tryCatch(
      prepare_model_features(
        model_features = model_features,
        predictor_set = stat_predictor_vars,
        medians = stat_medians
      ),
      error = function(e) {
        stat_prepare_error <<- conditionMessage(e)
        NULL
      }
    )
    ml_x <- tryCatch(
      prepare_model_features(
        model_features = model_features,
        predictor_set = ml_predictor_vars,
        medians = ml_medians
      ),
      error = function(e) {
        ml_prepare_error <<- conditionMessage(e)
        NULL
      }
    )
    if (is.null(stat_x) || is.null(ml_x)) {
      failure_parts <- c(
        if (!is.null(stat_prepare_error)) {
          paste0(
            "statistical feature preparation: ",
            stat_prepare_error
          )
        },
        if (!is.null(ml_prepare_error)) {
          paste0(
            "machine-learning feature preparation: ",
            ml_prepare_error
          )
        }
      )
      return(list(
        summary = fail_summary(
          paste0(
            "Failed while preparing model inputs",
            if (length(failure_parts) > 0) {
              paste0(
                ": ",
                paste(failure_parts, collapse = " | ")
              )
            } else {
              "."
            }
          )
        ),
        detail = NULL
      ))
    }
    stat_prediction_error <- NULL
    ml_prediction_error <- NULL
    p_stat_notready <- tryCatch(
      clamp_probability(
        predict_not_ready(
          model_obj = stat_model_loaded,
          newdata = stat_x,
          predictor_set = stat_predictor_vars,
          calibration_model = NULL
        )
      )[1],
      error = function(e) {
        stat_prediction_error <<- conditionMessage(e)
        NA_real_
      }
    )
    p_ml_notready <- tryCatch(
      clamp_probability(
        predict_not_ready(
          model_obj = ml_model_loaded,
          newdata = ml_x,
          predictor_set = ml_predictor_vars,
          calibration_model = ml_calibration_model_obj
        )
      )[1],
      error = function(e) {
        ml_prediction_error <<- conditionMessage(e)
        NA_real_
      }
    )
    if (!is.finite(p_stat_notready) || !is.finite(p_ml_notready)) {
      failure_parts <- c(
        if (!is.finite(p_stat_notready)) {
          paste0(
            "statistical model: ",
            stat_prediction_error %||% "invalid internal output"
          )
        },
        if (!is.finite(p_ml_notready)) {
          paste0(
            "machine-learning model: ",
            ml_prediction_error %||% "invalid internal output"
          )
        }
      )
      return(list(
        summary = fail_summary(
          paste0(
            "Prediction failed: ",
            paste(failure_parts, collapse = " | ")
          )
        ),
        detail = NULL
      ))
    }
    p_hybrid_notready <- clamp_probability(
      w_stat * p_stat_notready +
        w_ml * p_ml_notready
    )[1]
    p_hybrid_ready <- 1 - p_hybrid_notready
    epiforess_score <- 100 * p_hybrid_ready

    estimated_median_best_mase <-
      estimate_median_best_mase(
        epiforess_score
      )[1]

    supportive_vec <- identify_supportive_indicators_vec(
      raw_features
    )
    concerns_vec <- identify_potential_concerns_vec(
      raw_features
    )
    supportive_text <- if (length(supportive_vec) == 0) {
      paste(
        "No dominant single supportive indicator was detected.",
        "The score reflects the combined model-based assessment across all extracted features."
      )
    } else {
      collapse_clean(
        lower_initial(supportive_vec)
      )
    }
    concern_text <- if (length(concerns_vec) == 0) {
      "No major secondary screening notes were detected."
    } else {
      collapse_clean(
        lower_initial(concerns_vec)
      )
    }
    note_label <- secondary_note_label(
      concerns_vec
    )
    readiness_level <- score_label(
      epiforess_score
    )
    action_class <- deployment_action_label(
      epiforess_score
    )
    unified_message <- forecast_readiness_message(
      score = epiforess_score,
      supportive_indicators = supportive_vec,
      potential_concerns = concerns_vec
    )
    score_table <- tibble::tibble(
      Series_Name = series_name,
      EpiFORESS = epiforess_score,
      Estimated_Median_Best_MASE =
        estimated_median_best_mase,
      Readiness_Level = readiness_level,
      Readiness_Band = score_band_short(epiforess_score),
      Deployment_Action = action_class,
      Secondary_Screening_Notes = note_label,
      Readiness_Interpretation = unified_message,
      Recommendation = unified_message,
      Supportive_Indicators = supportive_text,
      Secondary_Notes = concern_text,
      N_Observations = n_valid,
      Score_Status = "OK"
    )
    detail <- list(
      series_name = series_name,
      score = score_table,
      raw_features = raw_features,
      model_features = model_features,
      feature_table = make_feature_value_table(
        raw_features,
        raw_feature_label_map
      ),
      model_feature_table = make_feature_value_table(
        model_features,
        model_feature_label_map
      ),
      score_detail_table = make_score_detail_table(
        score_table
      ),
      stationarity_stl_table = make_stationarity_stl_table(
        y,
        frequency_value,
        0.05
      ),
      stl_decomposition_data = make_stl_decomposition_data(
        y,
        frequency_value
      ),
      series = y,
      selected_data = series_data,
      frequency = frequency_value,
      supportive_indicators_vec = supportive_vec,
      potential_concerns_vec = concerns_vec
    )
    summary_row <- score_table %>%
      dplyr::transmute(
        Series_Name,
        Rank = NA_integer_,
        N_Observations,
        EpiFORESS,
        Estimated_Median_Best_MASE,
        Readiness_Level,
        Deployment_Action,
        Supportive_Indicators,
        Secondary_Notes,
        Secondary_Screening_Notes,
        Score_Status
      )
    list(
      summary = summary_row,
      detail = detail
    )
  }
  
  # --------------------------------------------------------
    # RESPONSIVE ONE-SERIES-AT-A-TIME BATCH PROCESSING
    #
    # One callback scores one series. The callback then returns
    # control to Shiny before scheduling the next series. This is
    # what allows the browser's Cancel click to be processed.
    # --------------------------------------------------------

    make_unscored_result <- function(
        series_name,
        series_data,
        status_message,
        action_message = "Not scored"
    ) {

      n_valid <- if (
        !is.null(series_data) &&
        "Count_Selected" %in% names(series_data)
      ) {
        sum(!is.na(series_data$Count_Selected))
      } else {
        NA_integer_
      }

      list(
        summary = tibble::tibble(
          Series_Name = series_name,
          Rank = NA_integer_,
          N_Observations = n_valid,
          EpiFORESS = NA_real_,
          Readiness_Level = "Unclassified",
          Deployment_Action = action_message,
          Supportive_Indicators = NA_character_,
          Secondary_Notes = NA_character_,
          Secondary_Screening_Notes = NA_character_,
          Score_Status = status_message
        ),
        detail = NULL
      )
    }


    finalize_batch_job <- function(
        state,
        was_cancelled = FALSE
    ) {

      if (is.null(state)) {
        return(invisible(NULL))
      }

      current_token <- isolate(batch_run_token())

      # Ignore a callback belonging to an older Run request.
      if (!identical(state$token, current_token)) {
        return(invisible(NULL))
      }

      # Mark every unstarted series as skipped after cancellation.
      if (
        isTRUE(was_cancelled) &&
        state$index < state$n_series
      ) {

        remaining_indices <- seq.int(
          from = state$index + 1L,
          to = state$n_series
        )

        for (remaining_index in remaining_indices) {

          remaining_name <-
            state$series_names[remaining_index]

          remaining_data <-
            state$series_list[[remaining_name]]

          state$result_list[[remaining_name]] <-
            make_unscored_result(
              series_name = remaining_name,
              series_data = remaining_data,
              status_message = paste(
                "Skipped: screening was cancelled before",
                "this series was reached."
              ),
              action_message =
                "Not scored: screening was cancelled"
            )
        }
      }

      # Defensive fallback for any unexpected empty result.
      empty_result_indices <- which(
        vapply(
          state$result_list,
          is.null,
          logical(1)
        )
      )

      if (length(empty_result_indices) > 0L) {

        for (empty_index in empty_result_indices) {

          empty_name <- state$series_names[empty_index]
          empty_data <- state$series_list[[empty_name]]

          state$result_list[[empty_name]] <-
            make_unscored_result(
              series_name = empty_name,
              series_data = empty_data,
              status_message =
                "Skipped because batch processing ended unexpectedly.",
              action_message = "Not scored"
            )
        }
      }

      scored <- state$result_list

      ranked <- purrr::map_dfr(
        scored,
        "summary"
      ) %>%
        dplyr::select(
          -dplyr::any_of("Rank")
        ) %>%
        dplyr::arrange(
          is.na(EpiFORESS),
          dplyr::desc(EpiFORESS),
          Series_Name
        ) %>%
        dplyr::mutate(
          Rank = dplyr::if_else(
            is.na(EpiFORESS),
            NA_integer_,
            dplyr::row_number()
          ),
          .before = 1
        )

      details <- purrr::map(
        scored,
        "detail"
      )

      details <- details[
        !purrr::map_lgl(
          details,
          is.null
        )
      ]

      failed_rows <- ranked %>%
        dplyr::filter(
          Score_Status != "OK"
        )

      if (nrow(failed_rows) > 0L) {

        cat(
          "\nEpiFORESS scoring failures or skipped series:\n"
        )

        print(
          failed_rows %>%
            dplyr::select(
              Series_Name,
              N_Observations,
              Score_Status
            )
        )
      }

      final_result <- list(
        ranked = ranked,
        details = details,
        n_screened = state$n_series,
        n_success = length(details),
        cancelled = isTRUE(was_cancelled),
        n_scored_before_cancel = state$index
      )

      batch_results_value(final_result)
      batch_processing(FALSE)
      batch_job_state(NULL)
      cancel_requested(FALSE)

      session$sendCustomMessage(
        "epiforessHideOverlay",
        list()
      )

      if (isTRUE(was_cancelled)) {

        showNotification(
          paste0(
            "Screening cancelled after ",
            state$index,
            " of ",
            state$n_series,
            " series were processed."
          ),
          type = "warning",
          duration = 6
        )

      } else {

        showNotification(
          paste0(
            "Screening completed for ",
            state$n_series,
            if (state$n_series == 1L) {
              " series."
            } else {
              " series."
            }
          ),
          type = "message",
          duration = 4
        )
      }

      invisible(final_result)
    }


    process_next_batch_series <- function(token) {

      current_token <- isolate(batch_run_token())

      # A newer screening run superseded this callback.
      if (!identical(token, current_token)) {
        return(invisible(NULL))
      }

      state <- isolate(batch_job_state())

      if (is.null(state)) {
        return(invisible(NULL))
      }

      if (state$index >= state$n_series) {

        finalize_batch_job(
          state,
          was_cancelled = FALSE
        )

        return(invisible(NULL))
      }

      # Cancellation is evaluated before starting another series.
      if (isTRUE(isolate(cancel_requested()))) {

        finalize_batch_job(
          state,
          was_cancelled = TRUE
        )

        return(invisible(NULL))
      }

      next_index <- state$index + 1L
      series_name <- state$series_names[next_index]
      series_data <- state$series_list[[series_name]]

      progress_text <- paste0(
        'Scoring "',
        series_name,
        '" (',
        next_index,
        " of ",
        state$n_series,
        ")"
      )

      session$sendCustomMessage(
        "epiforessUpdateOverlay",
        list(
          text = progress_text,
          percent = round(
            100 * (next_index - 1L) /
              state$n_series
          )
        )
      )

      # The active series is synchronous. A cancellation request
      # takes effect immediately after it completes and before the
      # next series starts.
      series_result <- tryCatch(
        score_one_series(
          series_name = series_name,
          series_data = series_data,
          frequency_value = state$frequency_value,
          stat_model_loaded =
            state$statistical_model,
          ml_model_loaded =
            state$machine_learning_model
        ),
        error = function(e) {

          make_unscored_result(
            series_name = series_name,
            series_data = series_data,
            status_message = paste0(
              "Unexpected scoring error: ",
              conditionMessage(e)
            ),
            action_message =
              "Scoring could not be completed"
          )
        }
      )

      state$result_list[[series_name]] <-
        series_result

      state$index <- next_index
      batch_job_state(state)

      session$sendCustomMessage(
        "epiforessUpdateOverlay",
        list(
          text = paste0(
            "Completed ",
            next_index,
            " of ",
            state$n_series,
            " series."
          ),
          percent = round(
            100 * next_index /
              state$n_series
          )
        )
      )

      # Return control to Shiny, then start the next series.
      later::later(
      function() {
        shiny::withReactiveDomain(
          session,
          process_next_batch_series(token)
        )
      },
      delay = 0.10
    )

      invisible(NULL)
    }


    observeEvent(input$run, {

      validate(need(
        has_later,
        paste(
          "The later package is required for responsive batch",
          "processing and cancellation."
        )
      ))

      # Invalidate scheduled callbacks from an earlier run.
      new_token <- isolate(batch_run_token()) + 1L
      batch_run_token(new_token)

      cancel_requested(FALSE)
      batch_results_value(NULL)
      batch_processing(TRUE)
      batch_job_state(NULL)

      setup <- tryCatch({

        series_list <- isolate(
          series_inputs()
        )

        validate(need(
          length(series_list) >= 1L,
          "No series were selected for screening."
        ))

        validate(need(
          length(series_list) <= max_batch_series,
          paste0(
            "You may screen up to ",
            max_batch_series,
            " series at the same time."
          )
        ))

        frequency_value <- max(
          1L,
          as.integer(
            round(input$frequency)
          )
        )

        statistical_model <- stat_model()
        machine_learning_model <- ml_model()

        list(
          series_list = series_list,
          frequency_value = frequency_value,
          statistical_model = statistical_model,
          machine_learning_model =
            machine_learning_model
        )

      }, error = function(e) {

        batch_processing(FALSE)
        batch_job_state(NULL)

        session$sendCustomMessage(
          "epiforessHideOverlay",
          list()
        )

        showNotification(
          paste0(
            "Screening could not begin: ",
            conditionMessage(e)
          ),
          type = "error",
          duration = 8
        )

        NULL
      })

      if (is.null(setup)) {
        return()
      }

      series_names <- names(
        setup$series_list
      )

      if (
        is.null(series_names) ||
        any(is.na(series_names)) ||
        any(!nzchar(series_names))
      ) {

        series_names <- paste0(
          "Series ",
          seq_along(setup$series_list)
        )

        names(setup$series_list) <-
          series_names
      }

      n_series <- length(
        setup$series_list
      )

      result_list <- stats::setNames(
        vector(
          "list",
          n_series
        ),
        series_names
      )

      state <- list(
        token = new_token,
        series_list = setup$series_list,
        series_names = series_names,
        n_series = n_series,
        index = 0L,
        result_list = result_list,
        frequency_value =
          setup$frequency_value,
        statistical_model =
          setup$statistical_model,
        machine_learning_model =
          setup$machine_learning_model
      )

      batch_job_state(state)

      if (!has_TDA) {

        showNotification(
          paste(
            "TDA package is unavailable;",
            "persistence entropy will use its saved",
            "imputation value."
          ),
          type = "warning",
          duration = 5
        )
      }

      session$sendCustomMessage(
        "epiforessShowOverlay",
        list(
          text = paste0(
            "Preparing to screen ",
            n_series,
            if (n_series == 1L) {
              " series..."
            } else {
              " series..."
            }
          ),
          percent = 0
        )
      )

      later::later(
      function() {
        shiny::withReactiveDomain(
          session,
          process_next_batch_series(new_token)
        )
      },
      delay = 0.05
    )

    }, ignoreInit = TRUE)

    results <- reactive({
    batch <- batch_results()
    req(batch)
    detail_names <- names(batch$details)
    
    failure_message <- if (length(detail_names) == 0) {
      failure_reasons <- unique(
        batch$ranked$Score_Status[
          !is.na(batch$ranked$Score_Status) & batch$ranked$Score_Status != "OK"
        ]
      )
      if (length(failure_reasons) > 0) paste(failure_reasons, collapse = " | ") else "No series could be scored successfully."
    } else {
      ""
    }
    
    validate(need(length(detail_names) > 0, failure_message))
    selected <- input$detail_series
    if (is.null(selected) || !selected %in% detail_names) selected <- detail_names[1]
    batch$details[[selected]]
  })
  
  output$detail_series_selector <- renderUI({

      batch <- batch_results()
      req(batch)

      available_series <- batch$ranked %>%
        dplyr::filter(
          Score_Status == "OK",
          Series_Name %in% names(batch$details)
        ) %>%
        dplyr::arrange(Rank)

      if (nrow(available_series) == 0L) {

        return(
          p(
            class = "small-note",
            paste(
              "No successfully scored series are",
              "available for detailed viewing."
            )
          )
        )
      }

      choice_labels <- paste0(
        "#",
        available_series$Rank,
        "  ·  ",
        available_series$Series_Name,
        "  ·  Score ",
        formatC(
          available_series$EpiFORESS,
          format = "f",
          digits = 1
        ),
        "  ·  ",
        available_series$Readiness_Level
      )

      choices <- stats::setNames(
        available_series$Series_Name,
        choice_labels
      )

      current_selection <- isolate(
        input$detail_series
      )

      if (
        is.null(current_selection) ||
        !current_selection %in%
          available_series$Series_Name
      ) {
        current_selection <-
          available_series$Series_Name[1]
      }

      selectizeInput(
        "detail_series",
        label = NULL,
        choices = choices,
        selected = current_selection,
        multiple = FALSE,
        width = "100%",
        options = list(
          create = FALSE,
          closeAfterSelect = TRUE,
          placeholder =
            "Select a series to inspect",
          searchField = c(
            "label",
            "value"
          )
        )
      )
    })

    batch_report_series <- reactiveVal(NULL)
  
  output$batch_summary_table <- renderDT({
    req(batch_results())
    
    out <- batch_results()$ranked %>%
      dplyr::mutate(
        EpiFORESS = round(EpiFORESS, 2),

        Estimated_Median_Best_MASE = round(
          Estimated_Median_Best_MASE,
          3
        ),

        Report = dplyr::if_else(
          Score_Status == "OK",
          paste0(
            '<button type="button" class="btn btn-sm btn-outline-primary ef-table-report-btn" ',
            'onclick="Shiny.setInputValue(\'batch_report_row\', ',
            dplyr::row_number(),
            ', {priority: \'event\'});">Download</button>'
          ),
          '<span class="text-muted">Unavailable</span>'
        )
      ) %>%
      dplyr::select(
        Rank,
        Series_Name,
        N_Observations,
        EpiFORESS,
        Estimated_Median_Best_MASE,
        Readiness_Level,
        Deployment_Action,
        Report
      )
    
    table_options <- professional_dt_options(
      page_length = 10,
      scroll_x = TRUE
    )
    
    table_options$columnDefs <- c(
      table_options$columnDefs,
      list(
        list(
          orderable = FALSE,
          searchable = FALSE,
          className = "dt-center",
          targets = 7
        )
      )
    )
    
    DT::datatable(
      out,
      class = "stripe hover",
      width = "100%",
      options = table_options,
      rownames = FALSE,
      escape = FALSE,
      colnames = c(
        "Rank",
        "Series Name",
        "N",
        "Score",
        "Estimated Median Best MASE",
        "Readiness Band",
        "Recommended Action",
        "Report"
      )
    )
  })
  
  observeEvent(input$batch_report_row, {
    req(batch_results())
    
    row_index <- suppressWarnings(
      as.integer(input$batch_report_row)
    )
    
    ranked <- batch_results()$ranked
    
    validate(need(
      is.finite(row_index) &&
        row_index >= 1L &&
        row_index <= nrow(ranked),
      "The selected report row is invalid."
    ))
    
    series_name <- as.character(
      ranked$Series_Name[row_index]
    )
    
    validate(need(
      series_name %in% names(batch_results()$details),
      "A report is unavailable because this series was not scored successfully."
    ))
    
    batch_report_series(series_name)
    
    session$sendCustomMessage(
      "triggerBatchReportDownload",
      list(series = series_name)
    )
  }, ignoreInit = TRUE)
  
  
  output$download_batch_summary <- downloadHandler(
    filename = function() {
      paste0("epiforess_batch_screening_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(batch_results())
      out <- batch_results()$ranked %>%
        dplyr::mutate(
          EpiFORESS = round(
            EpiFORESS,
            2
          ),
          Estimated_Median_Best_MASE = round(
            Estimated_Median_Best_MASE,
            3
          )
        ) %>%
        dplyr::select(
          Rank,
          `Series Name` = Series_Name,
          N_Observations,
          `EpiFORESS Score` = EpiFORESS,
          `Estimated Median Best MASE` =
            Estimated_Median_Best_MASE,
          `Readiness Band` = Readiness_Level,
          `Recommended Action` = Deployment_Action,
          `Supportive Indicators` = Supportive_Indicators,
          `Secondary Notes` = Secondary_Notes,
          `Screening Status` = Score_Status
        )
      readr::write_csv(out, file, na = "NA")
    }
  )
  
  output$failure_summary_table <- renderDT({
    req(batch_results())
    out <- batch_results()$ranked %>%
      dplyr::filter(Score_Status != "OK") %>%
      dplyr::select(Series_Name, N_Observations, Score_Status)
    
    DT::datatable(
      out,
      class = "stripe hover",
      width = "100%",
      rownames = FALSE,
      options = professional_dt_options(10, scroll_x = TRUE),
      colnames = c("Series", "Valid Observations", "Reason Scoring Failed")
    )
  })
  
  selected_disease_label <- reactive({
    req(results())
    results()$series_name %||% "Selected Series"
  })
  
  selected_years_label <- reactive({
    req(results())
    selected_data <- results()$selected_data
    if (".EpiFORESS_Year" %in% names(selected_data)) {
      years <- suppressWarnings(as.numeric(selected_data$.EpiFORESS_Year))
      years <- years[is.finite(years)]
      if (length(years) > 0) {
        year_min <- min(years)
        year_max <- max(years)
        if (year_min == year_max) return(as.character(year_min))
        return(paste0(year_min, "\u2013", year_max))
      }
    }
    "Selected Years"
  })
  
  output$time_series_title <- renderText({
    paste0(selected_disease_label(), " Cases (", selected_years_label(), ")")
  })
  
  output$overview_section_title <- renderText({
    paste0("Forecast-Readiness Summary: ", selected_disease_label(), " (", selected_years_label(), ")")
  })
  
  output$stl_tab_title <- renderText({
    paste0(selected_disease_label(), " STL Decomposition (", selected_years_label(), ")")
  })
  
  output$extracted_features_title <- renderText({
    paste0(selected_disease_label(), " Extracted Features (", selected_years_label(), ")")
  })
  
  output$selected_data_title <- renderText({
    paste0(selected_disease_label(), " Selected Data (", selected_years_label(), ")")
  })
  
  output$results_panel <- renderUI({
    if (!has_data()) {
      return(div(
        class = "app-card placeholder-card",
        h3("No Data Loaded Yet"),
        p("Upload a CSV or Excel file on the Upload Data page to begin."),
        p(
          "After loading data, select the count column, disease or series column, optional additional grouping variable, year/week coverage, and seasonal frequency. Then click Run EpiFORESS Screening."
        )
      ))
    }
    
    if (is.null(input$run) || input$run == 0) {
      return(div(
        class = "app-card placeholder-card",
        h3("Data Loaded"),
        p("Configure the inputs on the Upload Data page, then click Run EpiFORESS Screening."),
        p(
          class = "small-note",
          paste0(
            "Up to ", max_batch_series, " series can be screened per run."
          )
        )
      ))
    }
    
    req(batch_results())
    batch_state <- batch_results()
    
    if (is.null(batch_state$n_success) || batch_state$n_success == 0) {
      all_have_minimum <- all(batch_state$ranked$N_Observations >= 20, na.rm = TRUE)
      return(div(
        class = "tab-section ef-screening-failure-page",
        div(class = "section-title", "Screening Could Not Be Completed"),
        div(
          class = "section-subtitle",
          if (isTRUE(batch_state$cancelled)) {
            "Screening was cancelled before any series finished scoring."
          } else if (isTRUE(all_have_minimum)) {
            paste(
              "The selected series passed the minimum observation check,",
              "but scoring failed at a later stage. Review the exact reason below."
            )
          } else {
            paste(
              "At least one selected series did not pass the input checks,",
              "or scoring failed at a later stage. Review the exact reason below."
            )
          }
        ),
        div(
          class = "clean-panel ef-screening-failure-panel",
          h4("Scoring Diagnostics"),
          p(class = "small-note", paste0("Resolved model directory: ", model_output_dir)),
          shinycssloaders::withSpinner(
            DTOutput("failure_summary_table", width = "100%"),
            type = 4,
            color = "#1D4E9E",
            size = 0.6
          )
        )
      ))
    }
    
    n_chosen <- as.integer(batch_state$n_screened %||% 1L)
    if (length(n_chosen) == 0 || is.na(n_chosen)) n_chosen <- 1L
    
    ranked_results_tabs <- if (n_chosen > 1) {
      list(
        bslib::nav_panel(
          "Ranked Results",
          div(
            class = "tab-section",
            div(class = "section-title", "Ranked EpiFORESS Screening Results"),
            div(
              class = "section-subtitle",
              if (isTRUE(batch_state$cancelled)) {
                paste0(
                  "Screening was cancelled after ", batch_state$n_scored_before_cancel,
                  " of ", batch_state$n_screened, " series were scored. Remaining series are listed as skipped."
                )
              } else {
                " "
              }
            ),
            div(
              class = "clean-panel",
              div(
                class = "panel-heading-row",
                h4("Batch Screening Summary"),
                downloadButton(
                  "download_batch_summary",
                  label = tagList(epiforess_icon("download", "ef-btn-icon"), span("Export Results \u00b7 CSV")),
                  class = "btn-primary export-btn"
                )
              ),
              shinycssloaders::withSpinner(
                DTOutput("batch_summary_table", width = "100%"),
                type = 4,
                color = "#1D4E9E",
                size = 0.6
              ),
              div(
                style = paste(
                  "position: absolute;",
                  "width: 1px;",
                  "height: 1px;",
                  "overflow: hidden;",
                  "clip: rect(0 0 0 0);",
                  "white-space: nowrap;"
                ),
                downloadButton(
                  "download_batch_series_report",
                  "Download"
                )
              )
            )
          )
        )
      )
    } else {
      list()
    }
    
    remaining_tabs <- list(
      bslib::nav_panel(
        "Overview",
        div(
          class = "tab-section",
          div(
            class = "section-title-row",
            div(class = "section-title", textOutput("overview_section_title", inline = TRUE)),
            downloadButton(
              "download_series_report",
              label = tagList(epiforess_icon("document", "ef-btn-icon"), span("Download")),
              class = "btn-primary export-btn export-btn-hero"
            )
          ),
          div(class = "section-subtitle", " "),
          div(
            class = "ef-overview-layout",
            style = "grid-template-columns: minmax(0, 1fr); width: 100%;",
            shinycssloaders::withSpinner(
              uiOutput("score_hero_content"),
              type = 4,
              color = "#1D4E9E",
              size = 0.6,
              proxy.height = "300px"
            )
          ),
          div(
            class = "clean-panel ef-interpretation-panel",
            div(
              class = "ef-panel-heading",
              div(class = "ef-panel-heading-copy", h4("Interpretation"), p(""))
            ),
            div(
              class = "interpretation-box",
              shinycssloaders::withSpinner(
                uiOutput("readiness_interpretation"),
                type = 4,
                color = "#1D4E9E",
                size = 0.6
              )
            )
          )
        )
      ),
      
      bslib::nav_panel(
        "Recommended Action",
        div(
          class = "tab-section ef-action-tab",
          shinycssloaders::withSpinner(
            uiOutput("recommended_action_panel"),
            type = 4,
            color = "#1D4E9E",
            size = 0.6
          ),
          shinycssloaders::withSpinner(
            uiOutput("why_classified_panel"),
            type = 4,
            color = "#1D4E9E",
            size = 0.6
          )
        )
      ),
      
      bslib::nav_panel(
        "Time Series and Diagnostics",
        div(
          class = "tab-section",
          div(class = "section-title", "Time Series and Diagnostics"),
          div(
            class = "section-subtitle",
            "Review the full series first, then examine baseline stationarity, STL trend, STL seasonality, and residual variation."
          ),
          div(
            class = "plot-card",
            div(
              class = "plot-card-heading",
              h4(textOutput("time_series_title", inline = TRUE)),
              p(
                class = "plot-source",
                em("Source: uploaded dataset")
              )
            ),
            div(class = "plot-toolbar", actionButton("fullscreen_series_plot", "Fullscreen", class = "fullscreen-btn")),
            div(
              id = "series_plot_fullscreen_container",
              class = "fullscreen-plot-container",
              shinycssloaders::withSpinner(
                plotlyOutput("series_plot", height = "570px", width = "100%"),
                type = 4,
                color = "#1D4E9E",
                size = 0.6
              )
            )
          ),
          div(
            class = "clean-panel",
            h4("Diagnostic Checks"),
            p(
              class = "small-note",
              "These checks summarize stationarity and STL-based trend, seasonality, and unexplained variation."
            ),
            shinycssloaders::withSpinner(
              uiOutput("stationarity_stl_cards"),
              type = 4,
              color = "#1D4E9E",
              size = 0.6
            )
          )
        )
      ),
      
      bslib::nav_panel(
        "STL Decomposition",
        div(
          class = "tab-section",
          div(class = "section-title", "STL Decomposition"),
          div(
            class = "section-subtitle",
            "This tab displays the original series and its STL trend, STL seasonal, and STL residual components."
          ),
          div(
            class = "plot-card",
            div(
              class = "plot-card-heading",
              h4(textOutput("stl_tab_title", inline = TRUE)),
              p(class = "plot-source", em("Source: selected time-series data"))
            ),
            shinycssloaders::withSpinner(
              plotOutput("stl_decomposition_plot", height = "680px", width = "100%"),
              type = 4,
              color = "#1D4E9E",
              size = 0.6
            )
          )
        )
      ),
      
      bslib::nav_panel(
        "Extracted Features",
        div(
          class = "tab-section",
          div(class = "section-title", "What EpiFORESS Measured"),
          div(
            class = "section-subtitle",
            "The 49 extracted features are grouped below by domain, with their definitions and the values calculated from the selected series."
          ),
          div(
            class = "clean-panel",
            div(
              class = "panel-heading-row",
              h4(textOutput("extracted_features_title", inline = TRUE)),
              div(
                class = "export-btn-group",
                downloadButton(
                  "download_series_features",
                  label = tagList(epiforess_icon("download", "ef-btn-icon"), span("Export This Series \u00b7 CSV")),
                  class = "btn-primary export-btn"
                ),
                downloadButton(
                  "download_all_features",
                  label = tagList(epiforess_icon("download", "ef-btn-icon"), span("Export All Screened Series \u00b7 CSV")),
                  class = "btn-secondary export-btn export-btn-secondary"
                )
              )
            ),
            uiOutput("feature_tables_by_domain")
          )
        )
      ),
      
      bslib::nav_panel(
        "Selected Data",
        div(
          class = "tab-section",
          div(class = "section-title", "Selected Input Data"),
          div(
            class = "section-subtitle",
            "This tab shows the filtered rows used for feature extraction and scoring."
          ),
          div(
            class = "clean-panel",
            h4(textOutput("selected_data_title", inline = TRUE)),
            shinycssloaders::withSpinner(
              DTOutput("selected_data_table", width = "100%"),
              type = 4,
              color = "#1D4E9E",
              size = 0.6
            )
          )
        )
      ),
      
      bslib::nav_panel(
        "Interpretation Guide",
        div(
          class = "tab-section",
          div(class = "section-title", "Interpretation Guide"),
          div(
            class = "section-subtitle",
            "This tab documents the readiness bands, hybrid formula, and diagnostic definitions."
          ),
          uiOutput("interpretation_guide_content")
        )
      )
    )

    persistent_series_switcher <- div(
      class = "ef-global-series-switcher",

      div(
        class = "ef-global-series-switcher-copy",

        div(
          class = "ef-global-series-switcher-eyebrow",
          "Detailed Results"
        ),

        h4(
          "Series Being Viewed"
        ),

        p(
          if (
            isTRUE(batch_state$n_success > 1L)
          ) {
            paste(
              "Switch the disease or series shown in",
              "Overview, Recommended Action, diagnostics,",
              "STL decomposition, extracted features,",
              "selected data, and the downloadable report."
            )
          } else {
            paste(
              "The successfully screened series shown",
              "throughout the detailed result tabs."
            )
          }
        )
      ),

      div(
        class =
          "ef-global-series-switcher-control",

        uiOutput(
          "detail_series_selector"
        )
      )
    )

    result_tabs <- do.call(
      bslib::navset_card_tab,
      c(
        list(
          title =
            "EpiFORESS Screening Outputs",
          header =
            persistent_series_switcher
        ),
        ranked_results_tabs,
        remaining_tabs
      )
    )

    result_tabs
  })
  
  output$interpretation_guide_content <- renderUI({
    band_definitions <- list(
      list(
        range = "0-19",
        name = "Very Low",
        note = "Very limited support for routine forecast development.",
        color = "#94A3AB"
      ),
      list(
        range = "20-34",
        name = "Low",
        note = "Forecasting should remain exploratory while limitations are addressed.",
        color = "#6FA8B3"
      ),
      list(
        range = "35-59",
        name = "Moderate",
        note = "Cautious candidate-model comparison may be considered.",
        color = "#3F98A3"
      ),
      list(
        range = "60-79",
        name = "High",
        note = "Favorable candidate for model fitting and rolling-origin validation.",
        color = "#0E8A96"
      ),
      list(
        range = "80-100",
        name = "Very High",
        note = "Strong candidate, pending comprehensive out-of-sample validation.",
        color = "#1D4E9E"
      )
    )
    band_rows <- lapply(
      band_definitions,
      function(band) {
        div(
          class = "guide-band-row",
          style = paste0("border-left-color:", band$color, ";"),
          div(class = "guide-band-range", band$range),
          div(class = "guide-band-name", band$name),
          div(class = "guide-band-note", band$note)
        )
      }
    )
    div(
      class = "guide-grid",
      div(
        class = "guide-panel guide-panel-wide",
        div(
          class = "guide-merged-grid",
          div(
            class = "guide-merged-col",
            h4("Readiness Bands"),
            p(
              class = "guide-lead",
              "The 0-100 score is grouped into five descriptive readiness bands for interpretation."
            ),
            div(class = "guide-band-list", band_rows)
          ),
          div(
            class = "guide-merged-col guide-merged-col-divider",
            h4("How to Read the Score"),
            div(
              class = "guide-stat-callout",
              div(class = "guide-stat-value", "0-100"),
              div(
                class = "guide-stat-label",
                "Higher scores indicate stronger predicted forecast-readiness"
              )
            ),
            p(
              class = "guide-lead",
              "EpiFORESS does not issue a binary Forecast-Ready or Not Forecast-Ready decision. The score, band, diagnostics, and recommended action should be interpreted together."
            ),
            p(
              class = "guide-lead",
              paste0(
                "Reference point: P(Not Ready) = ",
                formatC(simulation_not_ready_threshold, format = "f", digits = 2),
                " corresponds to a score of ",
                formatC(simulation_score_boundary, format = "f", digits = 0),
                ". Band boundaries are rounded for interpretation."
              )
            ),
            p(
              class = "guide-lead",
              paste0(
                "Up to ", max_batch_series,
                " series can be screened per run and ranked by score. An optional second grouping variable ",
                "(such as state, province, or facility) lets each series/location combination be screened separately."
              )
            )
          )
        )
      ),
      div(
        class = "guide-panel",
        h4("How It Is Scored"),
        p(
          class = "guide-lead",
          "EpiFORESS combines statistical and machine-learning components into a calibrated forecast-readiness score."
        )
      ),
      div(
        class = "guide-panel",
        h4("Indicators and Screening Notes"),
        tags$ul(
          class = "guide-simple-list",
          tags$li("Supporting indicators summarize clearly favorable patterns in the selected series."),
          tags$li("Screening notes identify conditions that deserve review during forecast development.")
        )
      ),
      div(
        class = "guide-panel guide-panel-wide",
        h4("Score Production Formula"),
        p(
          class = "guide-lead",
          paste(
            "The statistical and machine-learning components estimate",
            "forecast non-readiness internally. Their calibrated outputs",
            "are combined using inverse-Brier weights and converted",
            "to a 0-100 readiness score."
          )
        ),
        div(
          class = "ef-formula-stack",
          div(
            class = "ef-formula-row",
            div(class = "ef-formula-symbol", HTML("P<sub>Hybrid</sub>(Not Ready)")),
            div(class = "ef-formula-equals", "="),
            div(
              class = "ef-formula-expression",
              HTML(
                paste0(
                  formatC(w_stat, format = "f", digits = 3),
                  " x P<sub>Statistical</sub>(Not Ready) + ",
                  formatC(w_ml, format = "f", digits = 3),
                  " x P<sub>Machine Learning</sub>(Not Ready)"
                )
              )
            )
          ),
          div(
            class = "ef-formula-row ef-formula-row-primary",
            div(class = "ef-formula-symbol", "EpiFORESS Score"),
            div(class = "ef-formula-equals", "="),
            div(
              class = "ef-formula-expression",
              HTML("100 x [1 - P<sub>Hybrid</sub>(Not Ready)]")
            )
          ),

          div(
            class = "ef-formula-row",

            div(
              class = "ef-formula-symbol",
              "Estimated Median Best MASE"
            ),

            div(
              class = "ef-formula-equals",
              "="
            ),

            div(
              class = "ef-formula-expression",
              HTML(
                "exp[0.688744 - 0.145625 x (EpiFORESS Score / 10)]"
              )
            )
          )
        )
      )
    )
  })
  
  output$score_hero_content <- renderUI({
    req(results())
    score_table <- results()$score
    score <- as.numeric(score_table$EpiFORESS)
    band_key <- score_band_key(score)
    band_label <- score_label(score)
    div(
      class = paste("ef-score-card", paste0("ef-band-", band_key)),
      style = paste0("--ef-score-width:", max(0, min(100, score)), "%;"),
      div(class = "ef-score-eyebrow", "Forecast-Readiness Score"),
      div(
        class = "ef-score-line",
        span(class = "ef-score-main", formatC(score, format = "f", digits = 1)),
        span(class = "ef-score-denominator", "/ 100")
      ),
      div(class = "ef-score-band", band_label),

      div(
        class = "ef-secondary-mase-estimate",

        div(
          class = "ef-secondary-mase-copy",

          span(
            class = "ef-secondary-mase-label",
            "Estimated Median Best MASE"
          ),

          span(
            class = "ef-secondary-mase-note",
            "Validation-set score calibration"
          )
        ),

        div(
          class = "ef-secondary-mase-value",
          formatC(
            estimate_median_best_mase(score),
            format = "f",
            digits = 2
          )
        ),

        div(
          class = "ef-secondary-mase-footnote",
          paste(
            "Lower values indicate better expected performance",
            "from the best candidate model across conventional forecasting models."
          )
        )
      ),

      div(
        class = "ef-readiness-scale",
        div(
          class = "ef-readiness-track",
          div(class = "ef-readiness-fill")
        ),
        div(
          class = "ef-threshold-labels",
          span("0"),
          span("20"),
          span("35"),
          span("60"),
          span("80"),
          span("100")
        ),
        div(
          class = "ef-threshold-caption",
        )
      )
    )
  })
  
  
  
  output$readiness_interpretation <- renderUI({
    req(results())
    htmltools::HTML(results()$score$Readiness_Interpretation)
  })
  
  output$why_classified_panel <- renderUI({
    req(results())
    result <- results()
    score_table <- result$score
    score <- as.numeric(
      score_table$EpiFORESS
    )
    supportive <- unique(
      result$supportive_indicators_vec %||% character(0)
    )
    concerns <- unique(
      result$potential_concerns_vec %||% character(0)
    )
    profile <- make_diagnostic_profile(
      result$raw_features
    )
    supportive_profile <- profile %>%
      dplyr::filter(
        Status_Key == "good"
      )
    concern_profile <- profile %>%
      dplyr::filter(
        Status_Key %in% c(
          "warn",
          "danger"
        )
      )
    neutral_profile <- profile %>%
      dplyr::filter(
        Status_Key == "neutral"
      )
    screening_summary <- make_screening_summary(
      score = score,
      supportive_indicators = supportive,
      potential_concerns = concerns
    )
    render_indicator_list <- function(
    items,
    empty_text = NULL
    ) {
      items <- items[!is.na(items)]
      items <- unique(
        stringr::str_squish(
          items
        )
      )
      items <- items[items != ""]
      if (length(items) == 0) {
        if (is.null(empty_text)) {
          return(NULL)
        }
        return(
          p(
            class = "ef-empty-message",
            empty_text
          )
        )
      }
      tags$ul(
        class = "ef-natural-indicator-list",
        lapply(
          items,
          tags$li
        )
      )
    }
    render_profile_rows <- function(
    profile_data
    ) {
      if (nrow(profile_data) == 0) {
        return(NULL)
      }
      lapply(
        seq_len(
          nrow(profile_data)
        ),
        function(i) {
          row <- profile_data[i, ]
          div(
            class = paste(
              "ef-profile-row",
              paste0(
                "profile-",
                as.character(
                  row$Status_Key
                )
              )
            ),
            div(
              class = "ef-profile-row-main",
              div(
                class = "ef-profile-row-title",
                as.character(
                  row$Area
                )
              ),
              span(
                class = "ef-profile-status",
                as.character(
                  row$Status
                )
              )
            ),
            div(
              class = "ef-profile-row-text",
              as.character(
                row$Explanation
              )
            )
          )
        }
      )
    }
    profile_column <- function(
    title,
    profile_data,
    indicators,
    column_class,
    empty_profile_text
    ) {
      cleaned_indicators <- unique(
        stringr::str_squish(
          indicators[!is.na(indicators)]
        )
      )
      has_indicators <- length(
        cleaned_indicators[
          cleaned_indicators != ""
        ]
      ) > 0
      div(
        class = paste(
          "ef-profile-column",
          column_class
        ),
        div(
          class = "ef-profile-column-heading",
          h5(title)
        ),
        div(
          class = "ef-profile-column-rows",
          if (nrow(profile_data) > 0) {
            render_profile_rows(
              profile_data
            )
          } else {
            p(
              class = "ef-empty-message",
              empty_profile_text
            )
          }
        ),
        if (has_indicators) {
          div(
            class = "ef-profile-column-notes",
            h6("Other Findings"),
            render_indicator_list(
              indicators
            )
          )
        }
      )
    }
    div(
      class = "clean-panel ef-why-panel",
      div(
        class = "ef-panel-heading",
        div(
          class = "ef-panel-heading-copy",
          h4("Screening Rationale")
        )
      ),
      div(
        class = "ef-plain-summary",
        screening_summary
      ),
      div(
        class = "ef-profile-split",
        profile_column(
          title = "Supportive Findings",
          profile_data = supportive_profile,
          indicators = supportive,
          column_class = "ef-profile-column-positive",
          empty_profile_text = "No core diagnostic check was clearly favorable."
        ),
        profile_column(
          title = "Concerns to Review",
          profile_data = concern_profile,
          indicators = concerns,
          column_class = "ef-profile-column-negative",
          empty_profile_text = "No core diagnostic check raised a material concern."
        )
      ),
      if (nrow(neutral_profile) > 0) {
        div(
          class = "ef-neutral-profile",
          h6("Not Estimable"),
          render_profile_rows(
            neutral_profile
          )
        )
      }
    )
  })
  
  output$recommended_action_panel <- renderUI({
    req(results())
    result <- results()
    score_table <- result$score
    score <- as.numeric(
      score_table$EpiFORESS
    )
    concerns <- unique(
      result$potential_concerns_vec %||% character(0)
    )
    action <- compute_recommended_action(
      score,
      concerns
    )
    next_steps <- action$next_steps[
      !grepl(
        "^Review the affected feature",
        action$next_steps,
        ignore.case = TRUE
      )
    ]
    div(
      class = "clean-panel ef-action-panel ef-action-panel-compact",
      div(
        class = "ef-action-title",
        action$heading
      ),
      p(
        class = "ef-action-context",
        action$summary
      ),
      if (length(next_steps) > 0) {
        div(
          class = "ef-action-steps",
          h5("Next Steps:"),
          tags$ul(
            class = "ef-next-step-list ef-next-step-list-compact",
            lapply(
              next_steps,
              tags$li
            )
          )
        )
      }
    )
  })
  
  output$series_plot <- plotly::renderPlotly({
    req(results())
    
    result <- results()
    plot_data <- result$selected_data %>%
      dplyr::mutate(
        Count = suppressWarnings(as.numeric(Count_Selected)),
        Time_Index = dplyr::row_number()
      )
    
    has_year <- ".EpiFORESS_Year" %in% names(plot_data) &&
      any(is.finite(suppressWarnings(as.numeric(plot_data$.EpiFORESS_Year))))
    
    has_week <- ".EpiFORESS_Week" %in% names(plot_data) &&
      any(is.finite(suppressWarnings(as.numeric(plot_data$.EpiFORESS_Week))))
    
    xaxis_settings <- list(title = "Time index")
    
    if (has_year && has_week) {
      frequency_value <- max(
        1L,
        as.integer(round(result$frequency %||% input$frequency %||% 52))
      )
      
      plot_data <- plot_data %>%
        dplyr::mutate(
          .EpiFORESS_Year = suppressWarnings(as.numeric(.EpiFORESS_Year)),
          .EpiFORESS_Week = suppressWarnings(as.numeric(.EpiFORESS_Week))
        ) %>%
        dplyr::filter(
          is.finite(.EpiFORESS_Year),
          is.finite(.EpiFORESS_Week)
        ) %>%
        dplyr::mutate(
          Plot_X = .EpiFORESS_Year +
            ((.EpiFORESS_Week - 1) / frequency_value),
          Hover_Label = paste0(
            "Year: ", .EpiFORESS_Year,
            "<br>Week: ", .EpiFORESS_Week,
            "<br>Count: ", Count
          )
        ) %>%
        dplyr::arrange(Plot_X)
      
      validate(need(
        nrow(plot_data) > 0,
        "No valid year-week observations are available for plotting."
      ))
      
      year_breaks <- seq(
        floor(min(plot_data$.EpiFORESS_Year, na.rm = TRUE)),
        ceiling(max(plot_data$.EpiFORESS_Year, na.rm = TRUE)),
        by = 1
      )
      
      xaxis_settings <- list(
        title = "Year",
        tickmode = "array",
        tickvals = year_breaks,
        ticktext = as.character(year_breaks)
      )
    } else if (has_year) {
      plot_data <- plot_data %>%
        dplyr::mutate(
          .EpiFORESS_Year = suppressWarnings(as.numeric(.EpiFORESS_Year))
        ) %>%
        dplyr::filter(is.finite(.EpiFORESS_Year)) %>%
        dplyr::mutate(
          Plot_X = Time_Index,
          Hover_Label = paste0(
            "Year: ", .EpiFORESS_Year,
            "<br>Count: ", Count
          )
        ) %>%
        dplyr::arrange(Plot_X)
      
      validate(need(
        nrow(plot_data) > 0,
        "No valid year observations are available for plotting."
      ))
      
      year_breaks <- plot_data %>%
        dplyr::group_by(.EpiFORESS_Year) %>%
        dplyr::summarise(Break_Position = min(Plot_X), .groups = "drop")
      
      xaxis_settings <- list(
        title = "Year",
        tickmode = "array",
        tickvals = year_breaks$Break_Position,
        ticktext = as.character(year_breaks$.EpiFORESS_Year)
      )
    } else {
      plot_data <- plot_data %>%
        dplyr::mutate(
          Plot_X = Time_Index,
          Hover_Label = paste0(
            "Time index: ", Time_Index,
            "<br>Count: ", Count
          )
        ) %>%
        dplyr::arrange(Plot_X)
    }
    
    plot_data <- plot_data %>%
      dplyr::filter(
        is.finite(Plot_X),
        is.finite(Count)
      )
    
    validate(need(
      nrow(plot_data) > 0,
      "No valid numeric count observations are available for plotting."
    ))
    
    plotly::plot_ly(
      data = plot_data,
      x = ~Plot_X,
      y = ~Count,
      type = "scatter",
      mode = "lines+markers",
      text = ~Hover_Label,
      hoverinfo = "text",
      line = list(width = 2, color = "#12275C"),
      marker = list(size = 6, color = "#1D4E9E")
    ) %>%
      plotly::layout(
        dragmode = "zoom",
        hovermode = "closest",
        xaxis = xaxis_settings,
        yaxis = list(title = "Disease count"),
        margin = list(l = 65, r = 30, b = 65, t = 20),
        paper_bgcolor = "white",
        plot_bgcolor = "white"
      ) %>%
      plotly::config(
        displaylogo = FALSE,
        responsive = TRUE,
        scrollZoom = TRUE,
        modeBarButtonsToRemove = c("lasso2d", "select2d")
      )
  })
  
  output$stationarity_stl_cards <- renderUI({
    req(results())
    
    diagnostic_table <- results()$stationarity_stl_table
    
    status_class <- function(decision) {
      decision <- tolower(as.character(decision))
      
      if (decision %in% c("passed", "present", "extracted")) {
        return("status-good")
      }
      
      if (decision == "flagged") return("status-danger")
      if (decision == "weak") return("status-warn")
      
      "status-neutral"
    }
    
    diagnostic_icon <- function(component, decision) {
      component_lower <- tolower(as.character(component))
      decision_lower <- tolower(as.character(decision))
      
      if (component_lower == "stationarity") {
        return(if (decision_lower == "passed") "check" else "warning")
      }
      
      if (component_lower %in% c("stl trend", "stl seasonality")) {
        return("trend")
      }
      
      if (component_lower == "stl residual") {
        return("activity")
      }
      
      "info"
    }
    
    component_label <- function(component) {
      dplyr::recode(
        as.character(component),
        Stationarity = "Stationarity",
        `STL trend` = "STL trend",
        `STL seasonality` = "STL seasonality",
        `STL residual` = "STL residual variation",
        .default = as.character(component)
      )
    }
    
    decision_label <- function(decision) {
      dplyr::recode(
        as.character(decision),
        Passed = "Looks stable",
        Flagged = "Needs review",
        Present = "Detected",
        Weak = "Weak",
        Extracted = "Available",
        `Not evaluated` = "Not available",
        `Constant series` = "Constant",
        `Not meaningful` = "Not meaningful",
        .default = as.character(decision)
      )
    }
    
    metric_label <- function(component, label) {
      component <- as.character(component)
      
      if (component == "Stationarity") return("ADF p-value")
      if (component == "STL trend") return("STL trend strength")
      if (component == "STL seasonality") return("STL seasonal strength")
      if (component == "STL residual") return("Residual standard deviation")
      
      as.character(label)
    }
    
    div(
      class = "diagnostic-summary-grid",
      
      lapply(seq_len(nrow(diagnostic_table)), function(i) {
        row <- diagnostic_table[i, , drop = FALSE]
        component <- as.character(row$Component)
        decision <- as.character(row$Decision)
        
        div(
          class = paste(
            "diagnostic-check-card",
            status_class(decision)
          ),
          div(
            class = "diagnostic-card-top",
            div(
              class = "diagnostic-component-wrap",
              epiforess_icon(
                diagnostic_icon(component, decision)
              ),
              span(
                class = "diagnostic-component",
                component_label(component)
              )
            ),
            span(
              class = "diagnostic-pill",
              decision_label(decision)
            )
          ),
          div(
            class = "diagnostic-metric",
            div(
              class = "diagnostic-metric-label",
              metric_label(component, row$Metric_Label)
            ),
            div(
              class = "diagnostic-metric-value",
              as.character(row$Metric_Value)
            )
          ),
          div(
            class = "diagnostic-interpretation",
            as.character(row$Interpretation)
          ),
          div(
            class = "diagnostic-recommendation",
            strong("Forecasting implication: "),
            as.character(row$Recommendation)
          ),
          div(
            class = "diagnostic-technical",
            paste0("Basis: ", as.character(row$Test))
          )
        )
      })
    )
  })
  
  output$stl_decomposition_plot <- renderPlot({
    req(results())
    
    result <- results()
    decomposition_data <- result$stl_decomposition_data
    
    validate(need(
      nrow(decomposition_data) > 0,
      paste0(
        "STL decomposition is unavailable. Use a seasonal frequency greater than 1 ",
        "and select at least two complete seasonal cycles. For weekly data with ",
        "frequency 52, this means at least 104 valid observations."
      )
    ))
    
    axis_data <- result$selected_data %>%
      dplyr::mutate(Row_Position = dplyr::row_number())
    
    has_year <- ".EpiFORESS_Year" %in% names(axis_data) &&
      any(is.finite(suppressWarnings(as.numeric(axis_data$.EpiFORESS_Year))))
    
    has_week <- ".EpiFORESS_Week" %in% names(axis_data) &&
      any(is.finite(suppressWarnings(as.numeric(axis_data$.EpiFORESS_Week))))
    
    if (has_year && has_week) {
      frequency_value <- max(
        1L,
        as.integer(round(result$frequency %||% input$frequency %||% 52))
      )
      
      axis_data <- axis_data %>%
        dplyr::mutate(
          .EpiFORESS_Year = suppressWarnings(as.numeric(.EpiFORESS_Year)),
          .EpiFORESS_Week = suppressWarnings(as.numeric(.EpiFORESS_Week)),
          Plot_X = .EpiFORESS_Year +
            ((.EpiFORESS_Week - 1) / frequency_value)
        )
      
      year_breaks <- seq(
        floor(min(axis_data$.EpiFORESS_Year, na.rm = TRUE)),
        ceiling(max(axis_data$.EpiFORESS_Year, na.rm = TRUE)),
        by = 1
      )
      
      x_scale <- ggplot2::scale_x_continuous(
        breaks = year_breaks,
        labels = year_breaks
      )
      
      x_label <- "Year"
    } else if (has_year) {
      axis_data <- axis_data %>%
        dplyr::mutate(
          .EpiFORESS_Year = suppressWarnings(as.numeric(.EpiFORESS_Year)),
          Plot_X = Row_Position
        )
      
      year_breaks <- axis_data %>%
        dplyr::filter(is.finite(.EpiFORESS_Year)) %>%
        dplyr::group_by(.EpiFORESS_Year) %>%
        dplyr::summarise(Break_Position = min(Row_Position), .groups = "drop")
      
      x_scale <- ggplot2::scale_x_continuous(
        breaks = year_breaks$Break_Position,
        labels = year_breaks$.EpiFORESS_Year
      )
      
      x_label <- "Year"
    } else {
      axis_data <- axis_data %>%
        dplyr::mutate(Plot_X = Row_Position)
      
      x_scale <- ggplot2::scale_x_continuous()
      x_label <- "Time index"
    }
    
    plot_data <- decomposition_data %>%
      dplyr::mutate(Row_Position = Time_Index) %>%
      dplyr::left_join(
        axis_data %>%
          dplyr::select(Row_Position, Plot_X),
        by = "Row_Position"
      ) %>%
      dplyr::filter(
        is.finite(Plot_X),
        is.finite(Value)
      )
    
    validate(need(
      nrow(plot_data) > 0,
      "STL decomposition could not be plotted."
    ))
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = Plot_X, y = Value)
    ) +
      ggplot2::geom_line(
        linewidth = 0.62,
        color = "#12275C"
      ) +
      ggplot2::facet_wrap(
        ~Component,
        ncol = 1,
        scales = "free_y"
      ) +
      x_scale +
      ggplot2::labs(
        x = x_label,
        y = NULL
      ) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(4, 10, 4, 10),
        panel.spacing.y = grid::unit(0.85, "lines"),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line(color = "#12275C14"),
        panel.grid.major.y = ggplot2::element_line(color = "#12275C14"),
        strip.background = ggplot2::element_rect(fill = "#E9F0FB", color = NA),
        strip.text = ggplot2::element_text(
          face = "bold",
          size = 12,
          color = "#143A6B"
        ),
        axis.title.x = ggplot2::element_text(
          face = "bold",
          color = "#42616B",
          margin = ggplot2::margin(t = 8)
        ),
        axis.text.x = ggplot2::element_text(
          color = "#42616B",
          size = 11
        ),
        axis.text.y = ggplot2::element_text(
          color = "#627780",
          size = 10
        )
      )
  })
  
  output$feature_tables_by_domain <- renderUI({
    req(results())
    
    lapply(feature_domain_order, function(domain_name) {
      domain_id <- feature_domain_ids[[domain_name]]
      output_id <- paste0("feature_table_", domain_id)
      
      local({
        this_domain <- domain_name
        this_output_id <- output_id
        
        output[[this_output_id]] <- renderDT({
          req(results())
          
          domain_rows <- results()$feature_table %>%
            dplyr::filter(Domain == this_domain) %>%
            dplyr::select(-Domain) %>%
            dplyr::mutate(
              Value = round(Value, 4)
            )
          
          DT::datatable(
            domain_rows,
            class = "stripe hover compact",
            width = "100%",
            options = professional_dt_options(
              page_length = max(1L, nrow(domain_rows)),
              scroll_x = TRUE,
              show_pagination = FALSE
            ),
            rownames = FALSE
          )
        })
      })
      
      div(
        class = "ef-domain-feature-block",
        h5(
          class = "ef-domain-feature-heading",
          domain_name
        ),
        shinycssloaders::withSpinner(
          DTOutput(output_id, width = "100%"),
          type = 4,
          color = "#1D4E9E",
          size = 0.6
        )
      )
    })
  })
  
  output$download_series_features <- downloadHandler(
    filename = function() {
      series_slug <- gsub(
        "[^A-Za-z0-9]+",
        "_",
        selected_disease_label()
      )
      
      paste0(
        "epiforess_features_",
        series_slug,
        "_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".csv"
      )
    },
    content = function(file) {
      req(results())
      
      output_table <- results()$feature_table %>%
        dplyr::mutate(Value = round(Value, 4)) %>%
        dplyr::rename(
          `What It Captures` = `What it captures`
        )
      
      readr::write_csv(
        output_table,
        file,
        na = "NA"
      )
    }
  )
  
  output$download_all_features <- downloadHandler(
    filename = function() {
      paste0(
        "epiforess_all_series_features_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".csv"
      )
    },
    content = function(file) {
      batch <- batch_results()
      req(batch)
      
      details <- batch$details
      
      validate(need(
        length(details) > 0,
        "No successfully scored series are available to export."
      ))
      
      rows <- purrr::imap(details, function(detail, series_name) {
        features <- as.data.frame(detail$raw_features)
        
        for (variable in predictor_vars) {
          if (!variable %in% names(features)) {
            features[[variable]] <- NA_real_
          }
        }
        
        features <- features[
          ,
          predictor_vars,
          drop = FALSE
        ]
        
        tibble::tibble(
          Series_Name = series_name,
          N_Observations = as.integer(detail$score$N_Observations),
          EpiFORESS_Score = round(
            as.numeric(
              detail$score$EpiFORESS
            ),
            2
          ),

          Estimated_Median_Best_MASE = round(
            as.numeric(
              detail$score$Estimated_Median_Best_MASE
            ),
            3
          ),

          Readiness_Band =
            detail$score$Readiness_Level
        ) %>%
          dplyr::bind_cols(features)
      })
      
      readr::write_csv(
        dplyr::bind_rows(rows),
        file,
        na = "NA"
      )
    }
  )
  
  output$download_batch_series_report <- downloadHandler(
    filename = function() {
      req(batch_report_series())
      
      series_slug <- gsub(
        "[^A-Za-z0-9]+",
        "_",
        batch_report_series()
      )
      
      paste0(
        "epiforess_report_",
        series_slug,
        "_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".pdf"
      )
    },
    content = function(file) {
      req(batch_results(), batch_report_series())
      
      series_name <- batch_report_series()
      detail <- batch_results()$details[[series_name]]
      
      validate(need(
        !is.null(detail),
        "The selected series report is unavailable."
      ))
      
      selected_data <- detail$selected_data
      
      years_label <- if (
        !is.null(selected_data) &&
        ".EpiFORESS_Year" %in% names(selected_data)
      ) {
        years <- suppressWarnings(
          as.numeric(selected_data$.EpiFORESS_Year)
        )
        years <- years[is.finite(years)]
        
        if (length(years) == 0L) {
          "Selected Years"
        } else if (min(years) == max(years)) {
          as.character(min(years))
        } else {
          paste0(min(years), "\u2013", max(years))
        }
      } else {
        "Selected Years"
      }
      
      render_epiforess_pdf_report(
        file = file,
        res = detail,
        series_label = series_name,
        years_label = years_label
      )
    },
    contentType = "application/pdf"
  )
  
  
  outputOptions(
    output,
    "download_batch_series_report",
    suspendWhenHidden = FALSE
  )
  
  output$download_series_report <- downloadHandler(
    filename = function() {
      series_slug <- gsub(
        "[^A-Za-z0-9]+",
        "_",
        selected_disease_label()
      )
      
      paste0(
        "epiforess_report_",
        series_slug,
        "_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".pdf"
      )
    },
    content = function(file) {
      req(results())
      
      render_epiforess_pdf_report(
        file = file,
        res = results(),
        series_label = selected_disease_label(),
        years_label = selected_years_label()
      )
    },
    contentType = "application/pdf"
  )
  
  output$selected_data_table <- renderDT({
    req(results())
    
    DT::datatable(
      make_selected_data_display_table(
        results()$selected_data
      ),
      class = "stripe hover compact",
      width = "100%",
      options = professional_dt_options(
        page_length = 12,
        scroll_x = TRUE
      ),
      rownames = FALSE
    )
  })
  
  observe({
    req(has_data())
    
    outputOptions(
      output,
      "results_panel",
      suspendWhenHidden = FALSE
    )
  })
}
