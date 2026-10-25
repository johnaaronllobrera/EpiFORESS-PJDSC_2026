<div align="center">

# EpiFORESS
### Epidemiological Forecasting-Readiness Screening System

**Screen the series before selecting the model.**

*A pre-forecasting decision-support application for epidemiological time series*

[![Built with R](https://img.shields.io/badge/Built%20with-R-276DC3?logo=r)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/App-Shiny-1F78B4)](https://shiny.posit.co/)

</div>

---

## Overview

Epidemiological surveillance data are not automatically suitable for reliable forecasting. Missing reports, underreporting, excess zeros, outbreaks, and structural changes can affect forecasting performance.

EpiFORESS is an R Shiny application that screens epidemiological time series before model selection. It extracts 49 predictors, applies a hybrid statistical and machine-learning scoring workflow, and provides readiness scores, diagnostic summaries, and exportable results.

The goal is to help analysts prioritize data review and forecasting experiments. EpiFORESS supports decision-making but does not guarantee forecast accuracy or replace out-of-sample validation.

## How it works

```text
Epidemiological time series
            |
            v
Input preparation and diagnostics
            |
            v
Feature extraction (49 predictors)
            |
            v
BIC Stepwise Logistic Regression + Random Forest
            |
            v
Inverse-Brier-weighted hybrid score
            |
            v
Diagnostic review and report export
            |
            v
Candidate forecasting and rolling-origin validation
```

## Methodology

EpiFORESS operationalizes the project's **SEAFRAME** workflow: Seeding, Emulation, Assessment, Forecast-Readiness Labeling, Representation, Algorithmic Modeling, and Evaluation.

### Simulation and forecasting assessment

Weekly Philippine Integrated Disease Surveillance and Response (PIDSR) data from 2019–2023 for 20 disease seeds informed the simulation design. A negative binomial data-generating process generated **10,000 synthetic series**, with 500 simulations per seed and 260 weekly observations per series. Simulated surveillance imperfections included missingness, underreporting, heaping, reporting delays, excess zeros, repeated observations, outbreaks, and level shifts.

Each series underwent rolling-origin evaluation using five candidate forecasting methods:

- Seasonal naïve
- STL-ETS
- Seasonal ARIMA (SARIMA)
- Nonseasonal ARIMA
- Negative binomial time-series regression

The evaluation used an initial window of 156 observations, a rolling step of 13, up to six forecast origins, and a 13-observation forecast horizon.

Mean Absolute Scaled Error (MASE) was the primary performance metric. The lowest MASE across candidate models was retained as **Best MASE**.

**Primary readiness criterion:** Ready if Best MASE < 1.00; otherwise, Not Ready.

### Feature extraction and hybrid modeling

The 49 predictors span seven domains:

1. Distributional characteristics
2. Temporal structure
3. Spectral and complexity measures
4. Data quality characteristics
5. Outbreak and structural characteristics
6. Signal quality and noise characteristics
7. Inter-annual stability characteristics

The hybrid scoring system combines two classifiers:

- **BIC-selected stepwise logistic regression**, representing the statistical component.
- **Random Forest**, capturing nonlinear relationships and interactions among predictors.

Their predicted probabilities of forecast non-readiness are combined using inverse-Brier-score weighting.

## Scoring and interpretation

The application combines the component probabilities using the configured weights:

$$
P_H(\text{Not Ready}) = 0.310 \times P_{\text{Stepwise}}(\text{Not Ready}) + 0.690 \times P_{\text{RF}}(\text{Not Ready})
$$

The hybrid probability is converted into a continuous readiness score:

$$
\text{EpiFORESS Score} = 100 \times \left[1 - P_H(\text{Not Ready})\right]
$$

Higher scores indicate stronger predicted forecast-readiness under the trained scoring system.

The application also estimates median Best MASE:

$$
\text{Estimated Median Best MASE} = \exp\left[0.688744 - 0.145625 \times \frac{\text{EpiFORESS Score}}{10}\right]
$$

This is a model-based estimate, not a guarantee of future forecasting error.

### Readiness bands

| Score | Band | Interpretation |
|---:|---|---|
| 0–19 | Very Low | Very limited support for routine forecasting. |
| 20–34 | Low | Keep forecasting exploratory while addressing limitations. |
| 35–59 | Moderate | Consider cautious candidate-model comparison. |
| 60–79 | High | Favorable candidate for model fitting and rolling-origin validation. |
| 80–100 | Very High | Strong candidate, pending comprehensive out-of-sample validation. |

The bands provide descriptive screening guidance, not universal epidemiological thresholds. A reference point of $P(\text{Not Ready}) = 0.66$ corresponds to a score of 34. EpiFORESS does not independently issue a binary readiness decision. Scores should be interpreted alongside diagnostic indicators and screening notes.

## Evaluation results

### Simulated-data performance

| Metric | Held-out test | Leave-one-dataset-out |
|---|---:|---:|
| Balanced accuracy | 0.91 | 0.90 |
| PR-AUC | 0.94 | 0.94 |
| F1 score | 0.88 | 0.85† |
| Matthews correlation coefficient | 0.82 | 0.77 |
| Brier score | 0.07 | 0.08 |

<sub>† The leave-one-dataset-out F1 score refers to the Not Ready class; the held-out test F1 is the reported overall score.</sub>

The held-out evaluation indicates strong predictive performance on unseen simulated series. Leave-one-dataset-out (LODO) evaluation assesses performance when an entire disease dataset is excluded from model development.

### Empirical validation

In Philippine surveillance data, EpiFORESS scores were inversely associated with Best MASE (Spearman's $\rho = -0.74$, $p < 0.0001$). Linear regression estimated a score coefficient of −0.05 ($p < 0.0001$).

At an empirical score threshold of 33, classification achieved a balanced accuracy of 0.88 and an F1 score of 0.86 for Not Ready. The Jonckheere–Terpstra test supported an ordered trend in forecasting performance across readiness groups (statistic = 11, $p < 0.0001$).

### External and cross-domain evaluation

External evaluation using United States disease series yielded a Spearman correlation of −0.52 between EpiFORESS scores and Best MASE. Each 10-point increase in score was associated with an estimated 24.6% reduction in Best MASE. The ROC-AUC for the Best MASE < 1.00 criterion was 0.84.

Exploratory analyses also found inverse associations between EpiFORESS scores and forecasting error in weather series ($\rho = -0.54$, $p = 0.014$) and electricity-load series ($\rho = -0.44$, $p = 0.014$).

These findings motivate further independent validation and domain-specific recalibration. Results remain specific to the evaluated datasets and validation designs.

## Application features

- Individual-series and batch screening
- Screening of up to 1,000 series per run
- Optional grouping by location or another variable
- Automated extraction of 49 predictors
- Hybrid readiness scoring and descriptive bands
- Diagnostic summaries and STL decomposition when supported
- Ranked series, feature tables, and screening notes
- CSV exports and downloadable PDF reports

## Repository structure

```text
EpiFORESS-PJDSC_2026/
├── app.R
├── Model Development Scripts (SEAFRAME)/
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

`app.R` launches the application. The `R/` directory contains the application modules, `saved_models/` stores trained model artifacts, and `www/` contains supporting web assets. The SEAFRAME development scripts document the simulation and model-development workflow.

## Installation and execution

### Requirements

- A recent version of [R](https://www.r-project.org/)
- [RStudio Desktop](https://posit.co/download/rstudio-desktop/) (recommended)
- Required R packages
- Trained model artifacts in `saved_models/`
- Supporting assets in `www/`, where required

### Run the application

1. Clone or download the repository.
2. Open the repository root in RStudio.
3. Install dependencies specified by the setup scripts.
4. Verify that the required model artifacts and assets are present.
5. Run:

```r
shiny::runApp()
```

Alternatively, open `app.R` in RStudio and select **Run App**.

For reproducibility, record the R and package versions used. A dependency lockfile, such as one generated with `renv`, is recommended.

## Data management and responsible use

The repository includes `DOH PIDSR DATA (2019-2023).xlsx`, identified as Philippine Department of Health PIDSR data. Its inclusion does not imply official endorsement or confirm redistribution permissions.

Document data sources, reporting intervals, transformations, and the treatment of missing reports and zero counts. Verify applicable data-sharing permissions before redistribution or deployment. A missing report should not automatically be interpreted as zero disease occurrence.

EpiFORESS is a research and decision-support tool. New diseases, surveillance systems, and reporting frequencies may require recalibration and independent validation. Do not use the score alone for clinical decisions, outbreak declarations, or public-health resource allocation. Any selected forecasting model must still be evaluated on observations not used to fit or select it.

## References

Hyndman, R. J., & Athanasopoulos, G. (2021). *Forecasting: Principles and practice* (3rd ed.). OTexts. https://otexts.com/fpp3/

Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. *International Journal of Forecasting, 22*(4), 679–688. https://doi.org/10.1016/j.ijforecast.2006.03.001

World Health Organization. (2017). *Data quality review: A toolkit for facility data quality assessment. Module 1: Framework and metrics*. https://www.who.int/data/data-collection-tools/health-service-data/data-quality-review

---

<div align="center">

**EpiFORESS | Epidemiological Forecasting-Readiness Screening System**

</div>
