# EpiFORESS: Model Development and Validation

This folder contains the R scripts behind the development and evaluation of **EpiFORESS (Epidemiological Forecast-Readiness Screening System)**. The scripts follow the analysis from simulated surveillance data to tests on empirical data and an external dataset from Project Tycho.

The intended reader may be reviewing the project or trying to reproduce part of the analysis. The guide below explains what each script does, when to run it, and what to check before interpreting its results.

## Start here

These scripts are designed to be read and run in numerical order for the main development workflow. The later scripts are validation studies and depend on earlier outputs, saved models, or the scoring function used by the EpiFORESS application.

| Script | What it does | Why it matters |
|---|---|---|
| `01_empirical_characterization_simulation.R` | Characterizes surveillance data and generates simulated time series. | Provides the development data used to study how surveillance-series properties relate to forecasting performance. |
| `02_feature_extraction.R` | Calculates the 49 candidate features from each series or analysis window. | Converts time-series information into predictors for the readiness models. |
| `03_forecasting_experiment.R` | Compares forecasting methods using rolling-origin evaluation. | Measures how well candidate methods forecast held-out observations. |
| `04_forecast_readiness_labeling.R` | Combines the feature data with forecasting results and assigns readiness labels. | Defines the outcome the readiness models are trained to predict. |
| `05_forecast_readiness_modeling.R` | Fits and compares statistical and machine-learning classifiers. | Selects and saves the models used to produce EpiFORESS scores. |
| `06_leave_one_disease_out_validation.R` | Repeats evaluation while holding out a disease from model development. | Examines whether performance carries over to a disease not represented in model training. |
| `07_empirical_validation.R` | Tests EpiFORESS against empirical surveillance series and their observed forecasting performance. | Checks whether scores are associated with forecasting performance beyond the simulated development data. |
| `08_external_validation_project_tycho.R` | Evaluates the scoring workflow on Project Tycho Level 1 data, keeping disease and geographic strata separate. | Examines performance on a separate external data source and a different surveillance context. |

## How the pieces fit together

Scripts 01–05 form the main model-development sequence: create the development data, extract features, evaluate forecasts, define the readiness outcome, and train the readiness models. Scripts 06–08 evaluate the resulting system using different validation designs. They are complementary rather than interchangeable: holding out a disease, evaluating empirical series, and testing an external dataset answer different questions.

The Shiny application uses the fitted models and scoring logic to score a surveillance series. The application itself is not included in this folder. Validation scripts that call the app's scoring function need the relevant app file and saved model artifacts in the locations expected by their settings.

## Before running the scripts

1. **Use the full EpiFORESS project directory.** This folder is only one part of the project; it does not contain all source data, intermediate files, saved models, or the app.
2. **Check each script's settings first.** Confirm its project path, input filenames, expected workbook sheets and columns, output directory, and required model files. Do not assume that setting the working directory alone will fix every path.
3. **Run in a fresh R session.** Install the packages listed by the script and avoid relying on objects left in the workspace by earlier interactive work.
4. **Run scripts 01–05 in order** when rebuilding model development. Run 06–08 only after their required inputs and scoring artifacts have been created or supplied.
5. **Keep a record of the run.** Record the R version, package versions, data versions, random seeds, model files, and any exclusions or failed fits. Keep the generated outputs so results can be traced back to the run that produced them.

If a script supports the `EPIFORESS_PROJECT_DIR` environment variable, set it to the root of the EpiFORESS project, not to this scripts folder. Other scripts may have their own path settings, so inspect them individually.

Example in R:

```r
Sys.setenv(EPIFORESS_PROJECT_DIR = "D:/path/to/EpiFORESS-project")
```

## Inputs and outputs

The exact files required depend on the script and the options selected. In particular, the validation scripts may require empirical-data workbooks, the Project Tycho Level 1 CSV, the saved statistical and machine-learning models, and an app file that exposes the scoring function. The scripts should be treated as parts of a larger project, not as eight standalone programs.

Each script defines or creates its own outputs. Review the output paths in the settings before running it, and do not overwrite the only copy of a prior analysis. Where outputs include tables, figures, model objects, or workbooks, preserve them with enough run information to identify the inputs and settings used.

## Methodological points to keep consistent

- **Readiness definition:** the label is based on observed forecasting error, not on the EpiFORESS score itself. Confirm the primary `Best MASE` threshold and any sensitivity threshold are consistent across the labeling, modeling, validation scripts, app, and written report.
- **Feature timing:** predictors used to score a series should only use information available at the intended screening or forecast-origin time. Future observations and evaluation errors must not enter the feature calculation.
- **Model evaluation:** preprocessing, feature selection, tuning, and score-threshold selection should use training or resampling data only, not the final test data.
- **Missing reports:** a missing source row is not automatically a reported zero. Follow the data source's reporting conventions and document how missing values are handled.
- **Project Tycho strata:** the external-validation script treats each disease × location × location-type series separately. Do not interpret its results as if all locations had been pooled into one disease series.
- **Interpretation:** EpiFORESS is a screening aid for assessing forecast readiness. A score does not guarantee that a forecast will be accurate or that a public-health decision will succeed.

## Data access and sharing

Check the terms of use and attribution requirements for the source surveillance data before sharing them. Do not upload restricted or non-redistributable datasets, confidential records, credentials, or local configuration files to a public repository. If the source data cannot be shared, document how an authorized user can obtain them and provide a data dictionary or synthetic example where useful.

Also confirm that saved model artifacts can be redistributed before committing them. A public repository should make it clear which files are available, which must be generated locally, and which require separate authorized access.

## Status and reporting

Having the scripts organized does not by itself confirm that the complete analysis runs or that the reported results are reproducible. Before presenting results, run the relevant scripts in the intended R environment, review warnings and failed fits, confirm that outputs were generated as expected, and reconcile the final thresholds and model artifacts with the application and report.

For reviewers, the key evidence is not only the final score or model metric, but also the data eligibility rules, number of series analyzed, exclusions, failure rates, uncertainty estimates, and validation design used to obtain it.
