<div align="center">

# EpiFORESS
### Epidemiological Forecasting-Readiness Screening System

**Screen the series before selecting the model.**

*A pre-forecasting decision-support application for epidemiological time series*

[![Built with R](https://img.shields.io/badge/Built%20with-R-276DC3?logo=r)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/App-Shiny-1F78B4)](https://shiny.posit.co/)
[![Focus](https://img.shields.io/badge/Domain-Public%20Health%20Forecasting-167D6A)](https://www.who.int/)

</div>

---

## The problem

Epidemiological surveillance data are collected to monitor public health, but a recorded time series is not automatically suitable for reliable forecasting. Missing reports, underreporting, heaping, repeated values, excess zeros, outbreaks, changing levels, and unstable seasonal patterns can affect the information available to a forecasting model.

Forecasting workflows commonly focus on *which model to fit*. EpiFORESS foregrounds an earlier question:

> **Does this series exhibit characteristics associated with forecast-readiness, and what should be examined before forecasting proceeds?**

EpiFORESS (Epidemiological Forecasting-Readiness Screening System) is an R/Shiny decision-support application that extracts a feature profile from an epidemiological time series, applies a trained hybrid statistical and machine-learning scoring workflow, displays diagnostic context, and produces exportable results.

The intended contribution is a **pre-modeling screening step** that helps analysts prioritize data review and forecasting validation. EpiFORESS does not guarantee forecast accuracy, replace epidemiological judgment, or remove the need for out-of-sample evaluation.

## Why EpiFORESS?

- **Readiness before model selection:** evaluates series characteristics before a user commits to a forecasting model.
- **A multi-feature view:** summarizes temporal structure, distributional behavior, complexity, and possible surveillance artifacts.
- **Interpretable diagnostics:** presents supporting indicators and STL decomposition when the data support it.
- **Operational outputs:** provides feature tables, CSV exports, batch scoring outputs, and PDF reports.
- **A validation-first philosophy:** treats the score as a screening signal, not as proof that a future forecast will be accurate.

---

## How it works

```text
Epidemiological time series
            |
            v
Data checks and time-index preparation
            |
            v
Feature extraction and diagnostic assessment
            |
            v
Trained statistical + machine-learning models
            |
            v
Hybrid forecast-readiness score
            |
            v
Diagnostic review, feature exports, and PDF report
            |
            v
Candidate forecasting + time-ordered validation
```

### 1. Input and preparation

The application supports individual-series and batch workflows as implemented in the repository. Year/week metadata can be used to present a meaningful time axis. Users should verify chronological order, consistent reporting intervals, valid counts, and the meaning of missing observations before interpreting any score.

**A missing report is not necessarily a zero count.** Missingness should only be converted to zero when the surveillance definition and data-generation process justify that interpretation.

### 2. Feature extraction

EpiFORESS uses a **49-feature predictor profile** organized around characteristics relevant to time-series forecasting. The implemented feature set spans domains such as:

- Distribution and variability
- Trend and seasonal structure
- Serial dependence
- Entropy and complexity
- Missingness and reporting artifacts
- Zero counts and intermittency
- Repetition and heaping
- Peaks, outbreaks, and extreme observations
- Level shifts and temporal instability

The precise feature definitions, transformations, and availability rules are defined in the R source. Some features may be unavailable or uninformative for short, constant, or highly incomplete series.

### 3. Diagnostic assessment

The interface includes stationarity-related diagnostics and STL decomposition summaries for trend, seasonal, and residual components when decomposition is supported by the series. For seasonal frequency \(m\), the application advises at least two complete seasonal cycles before STL decomposition; for weekly data with annual seasonality, this is approximately 104 observations at frequency 52.

These diagnostics describe properties of the observed series. They do not, individually, establish forecastability.

### 4. Hybrid readiness scoring

The application combines outputs from saved statistical and machine-learning models. The score is expressed on a 0–100 scale:

```text
EpiFORESS Score = 100 × [1 − P(Hybrid Not Ready)]
```

Higher scores indicate a greater estimated probability of readiness under the trained scoring system. The result depends on the saved models, feature-processing pipeline, and calibration used by the application.

The application also reports a calibrated estimate of median Best MASE:

```text
Estimated Median Best MASE =
    exp(0.688744 − 0.145625 × (EpiFORESS Score / 10))
```

This estimate is a secondary model-based summary. It is **not a guarantee of future forecast error** and should not replace direct evaluation on held-out observations.

### 5. Reporting and export

Depending on the workflow, the application provides:

- Series-level score and readiness band
- Diagnostic summary cards
- STL decomposition plot
- Feature tables grouped by domain
- Selected-data inspection
- CSV export of series features
- Batch feature export
- Downloadable PDF reports

---

## Findings and evaluation

The project’s development workflow uses simulated time series to evaluate forecast-readiness prediction, with epidemiological series informing simulation design and supporting empirical assessment. The reported results below should be interpreted in light of the study design and validation sample.

| Evaluation component | Reported result | Interpretation |
|---|---:|---|
| Simulated datasets | 10,000 | 500 simulated series for each of 20 disease seeds |
| Primary readiness definition | Best MASE < 0.50 | Conservative operational labeling criterion used in the primary analysis |
| Simulated series labeled Ready | 4,006 (40.06%) | Proportion classified Ready under the primary criterion |
| Simulated series labeled Not Ready | 5,994 (59.94%) | Proportion classified Not Ready under the primary criterion |
| Empirical validation sample | 17 series | Small validation sample; findings should be treated as preliminary |
| Association with Best MASE | Pearson \(r=-0.695\), \(p=0.00195\) | Higher scores were associated with lower observed Best MASE in the analyzed sample |
| Discrimination under the primary criterion | AUC = 0.904; bootstrap 95% CI: 0.731–1.000 | Promising discrimination in the analyzed sample, with substantial uncertainty due to sample size |

**Interpretation guardrail:** These are project-reported results, not a claim of universal performance. The empirical sample is small, and the confidence interval is wide. Independent validation, assessment across additional diseases and surveillance systems, and careful calibration checks are needed before operational use. Results may change when data definitions, observation windows, model versions, or readiness thresholds change.

### Forecast evaluation approach

Forecasting performance in the development workflow is evaluated using time-ordered rolling-origin validation rather than randomly shuffling temporal observations. This is important because future observations must not leak into the information used to fit forecasts. Best MASE is used as the primary performance summary, with additional error metrics retained for context.

MASE is useful for comparing forecasting performance across series with different scales, but its calculation and interpretation still require attention to the benchmark and the properties of each series (Hyndman & Koehler, 2006).

---

## Interpreting the readiness bands

The application displays the following descriptive score bands:

| Score | Band | Practical next step |
|---:|---|---|
| 0–19 | Very Low | Review data quality and the series definition before routine forecasting. |
| 20–34 | Low | Investigate instability and reporting artifacts; treat model results as exploratory. |
| 35–59 | Moderate | Compare candidate models cautiously and document limitations. |
| 60–79 | High | Proceed to candidate-model fitting and time-ordered validation. |
| 80–100 | Very High | A stronger readiness signal; still validate out of sample before operational use. |

These bands are application-level guidance, not universal epidemiological thresholds. A high score does not guarantee accurate forecasts, and a low score does not mean forecasting is impossible. Interpret scores in relation to the disease, surveillance system, time window, forecast horizon, and consequences of error.

---

## Data sources and data management

The repository includes a workbook named `DOH PIDSR DATA (2019-2023).xlsx`, which is identified in the project as Philippine Department of Health PIDSR data for 2019–2023. The workbook is the project’s local analysis input; its inclusion in the repository should not be interpreted as an endorsement by the Department of Health or as confirmation that all redistribution permissions have been satisfied.

For transparent and responsible use:

1. Record the original source, extraction date, reporting period, variable definitions, and any transformations.
2. Retain the original source separately from cleaned or derived data.
3. Document how missing reports, zero counts, duplicates, and revisions are handled.
4. Check data-sharing permissions and applicable privacy, institutional, and government requirements before redistributing or deploying the workbook.
5. Do not treat an observed zero as confirmed absence of disease without considering surveillance and detection processes.

For new datasets, confirm that the data structure matches the application's input and validation logic. New diseases, countries, reporting systems, and time intervals may require recalibration and independent validation.

---

## Repository structure

```text
EpiFORESS-PJDSC_2026/
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
├── saved_models/
├── www/
├── DOH PIDSR DATA (2019-2023).xlsx
└── README.md
```

The `R/` directory separates application setup, helpers, model configuration, diagnostics, feature extraction, scoring, PDF generation, user interface, and server logic. `app.R` is the application entry point. The `saved_models/` directory contains the trained model artifacts required by the scoring workflow, while `www/` holds web assets such as styling or JavaScript where applicable.

Keep model filenames and directory names consistent with the paths expected by the source code. Do not publish credentials, private data, or model artifacts whose redistribution is not authorized.

---

## Installation and execution

### Prerequisites

- A recent version of [R](https://www.r-project.org/)
- [RStudio Desktop](https://posit.co/download/rstudio-desktop/) (recommended)
- The R packages required by the project
- The model artifacts in `saved_models/`
- The UI assets in `www/`, if required by the application

The package setup code is the authoritative source for the application's current dependencies. For reproducible deployment, record the R version and package versions used. A dependency lockfile (for example, one generated with `renv`) is recommended for future releases.

### Run the application

1. Clone or download this repository.
2. Open the repository root as the working directory in RStudio.
3. Verify that the model artifacts and required web assets are present.
4. Install any missing dependencies according to the setup script.
5. Run the application:

```r
shiny::runApp()
```

Alternatively, open `app.R` in RStudio and select **Run App**.

### Reproducibility checklist

Before presenting or deploying the application:

- [ ] Confirm that all required model artifacts load successfully.
- [ ] Confirm that the feature names, ordering, transformations, and model expectations agree.
- [ ] Run the app on a known example series and inspect the outputs.
- [ ] Test short, constant, incomplete, and seasonal series.
- [ ] Verify the score, readiness band, feature table, CSV exports, and PDF report.
- [ ] Record R and package versions.
- [ ] Keep a known-good input and expected output for regression testing.
- [ ] Confirm permissions for any real surveillance data included or shared.

---

## Challenges and mitigation

| Challenge | Why it matters | Mitigation |
|---|---|---|
| Imperfect surveillance reporting | Observed counts may reflect reporting processes as well as disease occurrence. | Inspect missingness, repeated values, zeros, heaping, and other artifacts; document assumptions. |
| Heterogeneous diseases and scales | A feature or model can behave differently across diseases and surveillance systems. | Validate across disease groups and recalibrate when moving to a new domain. |
| Limited empirical validation sample | Performance estimates can be uncertain and may not generalize. | Expand external validation and report uncertainty intervals. |
| Temporal leakage | Random splits can use future information to evaluate forecasts. | Use chronological splits and rolling-origin evaluation. |
| Score misinterpretation | A readiness score can be mistaken for a guarantee of forecast accuracy. | Pair the score with diagnostics, clear caveats, and subsequent out-of-sample validation. |
| Reproducibility across machines | Different package versions or missing model files can prevent execution. | Document dependencies, preserve expected paths, and use a dependency lockfile. |

---

## Real-world application and future directions

EpiFORESS is designed to support analysts who need to decide where to invest time in data review and forecasting experiments. Potential uses include:

- **Surveillance analytics:** flag series that may need additional data-quality investigation.
- **Forecasting workflow triage:** prioritize series for candidate-model evaluation.
- **Research and teaching:** demonstrate why forecast performance depends on series characteristics as well as model choice.
- **Reporting and review:** provide a consistent diagnostic summary for analysts and collaborators.

These are intended applications, not evidence that the tool has been prospectively validated for routine public-health decision-making.

### Priority next steps

1. **External validation:** evaluate on additional diseases, years, regions, and surveillance systems not used in model development.
2. **Threshold sensitivity:** report how conclusions change under alternative readiness definitions, including Best MASE < 1.00.
3. **Calibration assessment:** compare predicted readiness probabilities with observed outcomes in independent data.
4. **Subgroup performance:** investigate whether performance differs by disease, prevalence, zero frequency, or artifact burden.
5. **Reproducible deployment:** pin dependencies, add automated checks, and document tested software environments.
6. **Human-centered evaluation:** assess whether the diagnostics and reports help intended users make better-informed workflow decisions.

---

## Technical methods and rationale

- **Feature-based screening:** compresses multiple temporal and reporting characteristics into a structured series-level representation.
- **Hybrid statistical and machine-learning scoring:** combines complementary model outputs using the model configuration saved with the application.
- **Rolling-origin forecast evaluation:** preserves temporal order and evaluates predictions on later observations, consistent with established time-series validation guidance (Hyndman & Athanasopoulos, 2021).
- **MASE-based performance labeling:** uses a scaled error measure for comparability across series, with the caveat that the chosen readiness threshold is a project-specific operational definition rather than a universal standard (Hyndman & Koehler, 2006).
- **Diagnostic reporting:** supports review of the feature profile and underlying series instead of presenting the score as an unexplained verdict.

The contribution is not that one score can settle forecastability for every epidemiological series. It is that forecast-readiness is treated as an explicit, testable pre-modeling question and linked to a documented analytical workflow.

---

## Limitations and responsible use

EpiFORESS is a research and decision-support tool. Its outputs depend on the training data, labels, feature definitions, fitted models, calibration, and input quality. New settings may require recalibration. The reported empirical evaluation is limited in sample size and should not be treated as definitive evidence of broad generalizability.

Do not use the readiness score alone to make clinical decisions, declare that an outbreak will or will not occur, or allocate public-health resources. Review the result with qualified statistical and public-health professionals, and validate any forecasting model using data that were not used to fit or select it.

## References

Hyndman, R. J., & Athanasopoulos, G. (2021). *Forecasting: Principles and practice* (3rd ed.). OTexts. https://otexts.com/fpp3/

Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. *International Journal of Forecasting, 22*(4), 679–688. https://doi.org/10.1016/j.ijforecast.2006.03.001

World Health Organization. (2017). *Data quality review: A toolkit for facility data quality assessment. Module 1: Framework and metrics*. https://www.who.int/data/data-collection-tools/health-service-data/data-quality-review

---

<div align="center">

**EpiFORESS — Reliable forecasting begins before model selection.**

</div>
