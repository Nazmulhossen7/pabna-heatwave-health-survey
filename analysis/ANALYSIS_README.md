# Analysis scripts

This folder contains the reproducible R workflow for **Impact of Heatwaves on Human Health: A Case Study in Pabna**.

## Files

- `00_run_all.R` — runs the complete analysis pipeline in order.
- `01_data_cleaning_and_preparation.R` — cleans and validates the anonymized 300-respondent survey dataset, parses numeric fields, calculates BMI, and writes a cleaned dataset plus data-quality summary.
- `02_descriptive_analysis.R` — creates demographic and heatwave-outcome frequency tables, numerical summaries, and descriptive figures.
- `03_correlation_analysis.R` — calculates Pearson correlations among BMI, age, height, weight, and working time, with pairwise p-values and a heatmap.
- `04_chi_square_analysis.R` — evaluates the three categorical associations emphasized in the original report using Pearson chi-square, likelihood-ratio chi-square, Cramer's V, and sparse-cell diagnostics.
- `05_logistic_regression.R` — provides a transparent reproducible binary logistic-regression extension using sleep impact as the outcome and age, sex, work hours, BMI, and monthly-income effect as predictors.

## How to run

From the repository root in R or RStudio:

```r
source("analysis/00_run_all.R")
```

The scripts read from:

```text
data/heatwave_health_survey_anonymized.csv
```

and generate:

```text
data/heatwave_health_survey_cleaned.csv
results/tables/
results/figures/
```

## Reproducibility note

The original academic report analyzed the survey using SPSS and Excel. The R workflow is a reproducible portfolio implementation based on the anonymized public dataset. The model-fitting paragraph in the original report contains inconsistent variable labels, so `05_logistic_regression.R` explicitly defines its outcome and predictors rather than claiming an exact reconstruction of that SPSS model.
