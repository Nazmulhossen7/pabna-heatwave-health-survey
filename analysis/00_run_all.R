# ============================================================
# 00_run_all.R
# Project: Impact of Heatwaves on Human Health: A Case Study in Pabna
# Purpose: Run the complete reproducible R analysis pipeline.
# Run this file from the repository root.
# ============================================================

scripts <- c(
  "analysis/01_data_cleaning_and_preparation.R",
  "analysis/02_descriptive_analysis.R",
  "analysis/03_correlation_analysis.R",
  "analysis/04_chi_square_analysis.R",
  "analysis/05_logistic_regression.R"
)

for (script in scripts) {
  cat("\n============================================================\n")
  cat("Running:", script, "\n")
  cat("============================================================\n")
  source(script, echo = FALSE)
}

cat("\nAll analyses completed successfully.\n")
cat("Generated tables: results/tables/\n")
cat("Generated figures: results/figures/\n")
