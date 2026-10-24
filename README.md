# EpiFORESS
### A Forecast-Readiness Screening System for Epidemiological Time Series

> **Before forecasting, ask whether the data are ready to be forecast.**

Epidemiological surveillance data are not automatically suitable for reliable forecasting. Missing reports, underreporting, repeated values, excess zeros, outbreaks, structural changes, and unstable seasonal patterns can complicate model fitting and weaken out-of-sample performance.

**EpiFORESS** (Epidemiological Forecasting-Readiness Screening System) is an R/Shiny application designed to help users examine these data-quality and time-series characteristics *before* committing to a forecasting workflow. It converts a series-level feature profile into a continuous forecast-readiness score, provides diagnostic evidence to support interpretation, and generates a downloadable PDF report.

EpiFORESS is intended to **support prioritization and methodological judgment**, not to certify that a forecast will be accurate or replace formal forecast validation.

---

## Why this matters

Forecasting workflows often begin by choosing a model. But for surveillance data, the more consequential first question may be whether the observed series contains a sufficiently informative and stable signal for forecasting to be useful.

EpiFORESS brings that pre-modeling question to the foreground:

- **Screen first.** Summarize characteristics that may affect forecasting performance before fitting candidate models.
- **Explain the score.** Present diagnostics and feature information alongside the overall readiness assessment.
- **Make the assessment actionable.** Help users identify when a series may warrant data review, cautious experimentation, or full out-of-sample validation.
- **Document the decision.** Export a PDF report and feature tables for review, reproducibility, and communication.

## What the application does

### 1. Accepts epidemiological time-series data
The application supports individual-series and batch workflows through the available data-input interface. It can use year/week metadata when supplied and allows users to inspect the selected observations.

### 2. Extracts a 49-feature profile
EpiFORESS organizes predictors across complementary analytical domains, including:

- Distribution and variability
- Trend and seasonality
- Serial dependence
- Entropy and complexity
- Surveillance artifacts and reporting irregularities
- Zeros, intermittency, and run structure
- Outbreak behavior and extreme values
- Change points and level shifts
- Residual and rolling-window instability

The exact feature definitions and model-aligned transformations are implemented in the R source files. Feature availability can depend on the properties and length of the input series.

### 3. Examines key time-series diagnostics
The application provides stationarity and STL-based diagnostic summaries, including trend, seasonal, and residual behavior when the series supports decomposition. For seasonal frequency \(m\), STL decomposition requires sufficient observations; the interface advises at least two complete seasonal cycles.

These diagnostics are evidence for interpretation, not standalone guarantees of forecastability.

### 4. Produces a continuous readiness score
The score is defined in the application as:

EpiFORESS Score = 100 × [1 − P(Hybrid Not Ready)]

The scoring workflow combines statistical and machine-learning model outputs using the saved model artifacts and calibration components configured by the application. The score is presented on a 0–100 scale, with higher values indicating greater estimated forecast-readiness under the fitted system.

The application also reports an estimated median Best MASE using the implemented validation-set calibration:

Estimated Median Best MASE =
    exp(0.688744 − 0.145625 × (EpiFORESS Score / 10))

This is a **secondary calibrated estimate**, not a substitute for evaluating forecasts against held-out observations.

### 5. Generates reports and exports
Depending on the selected workflow, users can access diagnostic cards, STL decomposition plots, feature tables, selected-data views, CSV feature exports, batch feature exports, and downloadable PDF reports.

---

## Interpreting the score

The application uses the following descriptive bands:

| Score | Descriptive band | Suggested interpretation |
|---:|---|---|
| 0–19 | Very Low | Routine forecasting has limited support; review data limitations first. |
| 20–34 | Low | Keep forecasting exploratory while investigating instability and data-quality concerns. |
| 35–59 | Moderate | Consider cautious candidate-model comparison. |
| 60–79 | High | Proceed to candidate-model fitting and rolling-origin validation. |
| 80–100 | Very High | Stronger readiness signal, but comprehensive out-of-sample validation is still required. |

**Important:** These bands are descriptive guidance, not universal epidemiological standards. A high score does not guarantee accurate forecasts, and a low score does not mean forecasting is impossible. Interpretation should account for the disease, surveillance system, observation window, intended use, and consequences of forecast error.

---

## Getting started

### Requirements

- R (a recent version is recommended)
- RStudio (recommended, but not required)
- The application’s required R packages
- The matching trained model artifacts in `Saved_Models/`
- Any front-end assets required by the application in `www/`

The app source lists its R package dependencies and attempts to install missing packages at startup. For a controlled or offline deployment, install dependencies in advance and follow your institution’s software-management requirements.

### Project structure

```text
EpiFORESS/
├── app.R
├── R/
│   ├── 00_setup.R
│   ├── 01_helpers.R
│   ├── 02_model_config.R
│   ├── 03_diagnostics.R
│   ├── 04_feature_extraction.R
│   ├── 05_scoring.R
│   ├── 06_report_pdf.R
│   ├── 07_ui.R
│   └── 08_server.R
├── Saved_Models/       # Required model artifacts; obtain from the project owner
├── www/                # CSS, JavaScript, and other UI assets, if used
└── README.md
```

The model files and any required `www/` assets may not be included in a source-code bundle. Keep the directory names and expected filenames consistent with the application configuration.

### Run locally

1. Download and extract the project folder.
2. Confirm that `Saved_Models/` contains the model artifacts expected by `R/02_model_config.R`.
3. Restore any required files under `www/`.
4. Open the project folder in RStudio.
5. Run:

```r
shiny::runApp()
```

Alternatively, open `app.R` and use **Run App** in RStudio.

### Input preparation

For the most interpretable results:

- Provide a single, clearly identified observation series for each disease or indicator.
- Ensure observations are ordered chronologically.
- Use a consistent time interval throughout the selected series.
- Represent missing observations explicitly when they are genuinely missing; do not silently convert them to zero.
- If year and week fields are available, use consistent year/week definitions and verify that they align with the count observations.
- Select a seasonal frequency appropriate to the data-generating and reporting interval. For weekly annual seasonality, the code uses a frequency of 52 where applicable.
- Review warnings and diagnostic messages before interpreting the score.

Input requirements should be checked against the application's current validation logic before using the system with a new data source.

---

## Reproducibility and responsible use

EpiFORESS is a screening aid in a broader forecasting process. A responsible workflow is:

1. Inspect and validate the surveillance data.
2. Review the extracted features and diagnostics.
3. Interpret the readiness score in its data and public-health context.
4. Fit appropriate candidate forecasting models.
5. Evaluate performance using time-ordered, out-of-sample validation.
6. Document uncertainty, limitations, and operational implications before using forecasts to inform decisions.

Do not use the score alone to allocate public-health resources, make clinical decisions, or claim that an outbreak will or will not occur. Forecasts and readiness assessments should be reviewed by qualified subject-matter and statistical experts.

### Known limitations

- The score reflects the trained models, features, calibration, and data conditions represented during model development and validation.
- New diseases, surveillance systems, reporting practices, time scales, and countries may require additional validation or recalibration.
- Some diagnostics and features require sufficient observations or variation; they may be unavailable or less informative for short, constant, or highly incomplete series.
- The estimated median Best MASE is a calibration-based summary and should not be interpreted as a guaranteed future error.
- A source-code split does not, by itself, establish that the application has been fully tested or validated in a deployment environment.

---

## Technical overview

| Component | Implementation |
|---|---|
| Application | R and Shiny |
| Interface and styling | `bslib`, `htmltools`, application CSS/JavaScript assets where present |
| Data manipulation | `dplyr`, `tibble`, `readr`, `readxl`, `purrr`, `stringr` |
| Visualization | `ggplot2`, `plotly`, `scales` |
| Time-series analysis | `forecast`, `tseries`, `lubridate` |
| Model and feature workflow | Saved statistical and machine-learning model objects, model-aligned feature transformations |
| Feature methods | Distributional, temporal, artifact, complexity, and stability-related measures; optional packages may support selected features |
| Tables and reports | `DT`, PDF report-generation functions |

The package list in `R/00_setup.R` is the authoritative reference for the current application. Optional components are enabled only when the relevant package and implementation are available.

---

## Project files

- `app.R` — application entry point
- `R/00_setup.R` — package setup, configuration, feature definitions, and mappings
- `R/01_helpers.R` — shared utility functions
- `R/02_model_config.R` — saved-model loading and model configuration
- `R/03_diagnostics.R` — stationarity, date handling, and STL diagnostic helpers
- `R/04_feature_extraction.R` — feature extraction and model-aligned transformations
- `R/05_scoring.R` — prediction, calibration, and readiness scoring
- `R/06_report_pdf.R` — PDF report generation
- `R/07_ui.R` — Shiny interface
- `R/08_server.R` — server logic, outputs, and downloads

## Citation and attribution

If you use EpiFORESS in a research project, presentation, or evaluation, cite the associated project documentation or manuscript supplied by the project authors. Do not cite this README as evidence of model validity. Until a formal citation is provided, describe it as **EpiFORESS (Epidemiological Forecasting-Readiness Screening System)** and identify the version used.

## Development status

This README describes the application structure and scoring behavior visible in the supplied source code. The separated scripts should be tested together with the required model artifacts and UI assets before a live demonstration or deployment. Report reproducible bugs with the R version, package versions, input-data structure, error message, and steps to reproduce.

---

### The central idea

**Reliable forecasting begins before model selection.** EpiFORESS makes forecast-readiness an explicit, inspectable step in the epidemiological forecasting workflow, pairing a continuous screening score with diagnostic context and a report that can be reviewed rather than treated as a black-box verdict.
