# EpiFORESS (cleanly separated R scripts)

## Project layout

- `app.R`: application entry point.
- `R/00_setup.R`: package loading, configuration, predictor definitions, and feature-domain mappings.
- `R/01_helpers.R`: shared utility functions.
- `R/02_model_config.R`: loading and configuring saved model artifacts.
- `R/03_diagnostics.R`: date/year handling, stationarity checks, and STL helpers.
- `R/04_feature_extraction.R`: feature extraction and model-aligned feature transformations.
- `R/05_scoring.R`: model prediction, calibration, and readiness scoring.
- `R/06_report_pdf.R`: PDF report generation.
- `R/07_ui.R`: Shiny UI.
- `R/08_server.R`: Shiny server logic.

## Run

1. Keep the `Saved_Models/` directory with the model `.rds` files expected by the application.
2. Keep the original front-end assets in `www/` if present (for example `styles.css`, `script.js`, and `navbar_series_selector.js`).
3. Open this project folder as the working directory in R/RStudio.
4. Run `shiny::runApp()` or open `app.R` and click **Run App**.

## Important notes

- This is a structural split of the supplied `app_full_fixed.R`, not a redesign of the statistical methods.
- Function bodies and server/UI logic were preserved in their original order to retain dependencies.
- R was not available in the generation environment, so the application could not be parsed or launched here. Please test in an R environment with the required packages and the `Saved_Models/` directory.
- The app setup attempts to install missing packages automatically, matching the original script's behavior.
