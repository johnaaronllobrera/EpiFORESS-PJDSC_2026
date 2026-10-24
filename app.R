# EpiFORESS Shiny application entry point
# Run from this folder with: shiny::runApp()

options(shiny.sanitize.errors = FALSE)

app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(app_dir, "R", "00_setup.R"))) {
  stop("Run the app from the EpiFORESS project folder containing R/00_setup.R.")
}

script_files <- c(
  "00_setup.R",
  "01_helpers.R",
  "02_model_config.R",
  "03_diagnostics.R",
  "04_feature_extraction.R",
  "05_scoring.R",
  "06_report_pdf.R",
  "07_ui.R",
  "08_server.R"
)

for (script in script_files) {
  source(file.path(app_dir, "R", script), local = .GlobalEnv)
}

shiny::shinyApp(ui = ui, server = server)
