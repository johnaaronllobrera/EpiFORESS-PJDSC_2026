# EpiFORESS | 07_ui.R
# Shiny user interface.
# Split from app_full_fixed.R; code logic preserved during file separation.

# 7. USER INTERFACE
# ------------------------------------------------------------
navbar_brand <- span(class = "ef-navbar-brand-name-only", "EpiFORESS")
ui <- tagList(
  tags$head(
    tags$script(src = "navbar_series_selector.js")
  ),
  bslib::page_navbar(
  id = "main_nav",
  title = navbar_brand,
  theme = bslib::bs_theme(
    version = 5,
    primary = "#1D4E9E",
    bg = "#F8FBFC",
    fg = "#102A33",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter")
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('triggerBatchReportDownload', function(message) {
         var attempts = 0;
         function startDownload() {
           var link = document.getElementById('download_batch_series_report');
           var href = link ? link.getAttribute('href') : null;
           if (link && href && href !== '#') {
             var downloadLink = document.createElement('a');
             downloadLink.href = href;
             downloadLink.setAttribute('download', '');
             downloadLink.style.display = 'none';
             document.body.appendChild(downloadLink);
             downloadLink.click();
             document.body.removeChild(downloadLink);
             return;
           }
           attempts += 1;
           if (attempts < 50) {
             window.setTimeout(startDownload, 100);
           }
         }
         window.setTimeout(startDownload, 100);
       });"
    ))
  ),
  header = div(
    id = "epiforess_loading_overlay",
    div(class = "epiforess-spinner-ring"),
    div(class = "epiforess-loader-title", "Screening series for forecast-readiness"),
    div(id = "epiforess_loading_detail", class = "epiforess-loader-subtitle", "Preparing batch..."),
    div(
      class = "epiforess-loader-progress-track",
      div(id = "epiforess_loading_progress_bar", class = "epiforess-loader-progress-fill")
    ),
    div(
      class = "epiforess-loader-cancel-row",
      style = "margin-top:18px; text-align:center;",
      actionButton(
        "cancel_scoring",
        "Cancel Screening",
        class = "btn-cancel-scoring",
        style = paste(
          "background:#B3261E; color:#fff; border:none; border-radius:6px;",
          "padding:8px 22px; font-weight:600; cursor:pointer;",
          "box-shadow:0 1px 3px rgba(0,0,0,0.25);"
        )
      )
    )
  ),
  tags$script(src = "script.js"),
  
  bslib::nav_panel(
    "Upload Data",
    value = "Upload Data",
    div(
      class = "hero",
      h1("EpiFORESS"),
      h3("Epidemiological Forecasting-Readiness Screening System")
    ),
    
    bslib::layout_columns(
      col_widths = c(7, 5),
      gap = "18px",
      
      div(
        class = "app-card purpose-card",
        h3("Purpose"),
        p(
          "EpiFORESS screens epidemiological ",
          tags$a(
            href = "https://en.wikipedia.org/wiki/Time_series",
            target = "_blank",
            "time-series"
          ),
          " data according to their predicted ability to support reliable forecasting. A high score indicates stronger forecast-readiness. A low score suggests that the data may need cleaning, further review, or another analytical approach."
        ),
        p("In practical terms, EpiFORESS asks:"),
        tags$blockquote(strong(
          "Before I spend time fitting models, is this dataset likely to yield reliable forecasts?"
        )),
        p(
          "EpiFORESS is optimized for weekly epidemiological surveillance counts. Preliminary cross-domain exploration suggests possible applications to other time-series settings, but those uses remain under validation."
        ),
        hr(),
        h4("Recommended Input"),
        tags$ul(
          tags$li("Disease or event-name column"),
          tags$li("Numeric disease-count column"),
          tags$li("Year or date column"),
          tags$li("Week column for weekly surveillance data"),
          tags$li("At least 20 valid numeric observations per series")
        )
      ),
      
      div(
        class = "app-card sidebar-card",
        h3("Upload and Configure Data"),
        div(
          class = "clean-file-input",
          fileInput(
            "file",
            "Upload data file",
            buttonLabel = "Choose file",
            placeholder = "No file selected",
            accept = c(".csv", ".xlsx", ".xls"),
            width = "100%"
          )
        ),
        
        uiOutput("sheet_selector"),
        uiOutput("column_selector"),
        uiOutput("disease_selector"),
        uiOutput("group2_selector"),
        uiOutput("year_filter_selector"),
        numericInput(
          "frequency",
          "Seasonal frequency",
          value = 52,
          min = 1,
          width = "100%"
        ),
        actionButton(
          "run",
          "Run EpiFORESS Screening",
          class = "btn-primary"
        ),
        hr(),
        p(
          class = "small-note",
          paste0(
            "Up to ", max_batch_series, " series can be screened per run, ranked by score."
          )
        )
      )
    )
  ),
  
  bslib::nav_panel(
    "Results",
    value = "Screening Results",
    div(class = "results-shell", uiOutput("results_panel"))
  )
)
# ------------------------------------------------------------
# 8. SERVER
# ------------------------------------------------------------
)
