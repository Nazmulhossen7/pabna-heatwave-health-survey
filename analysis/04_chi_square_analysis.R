# ============================================================
# 04_chi_square_analysis.R
# Project: Impact of Heatwaves on Human Health: A Case Study in Pabna
# Purpose: Evaluate associations between categorical variables with
#          Pearson chi-square tests, likelihood-ratio tests, Cramer's V,
#          and sparse-cell diagnostics.
#
# The original report examined:
#   1) Working time vs monthly-income effect
#   2) Eating habit vs monthly-income effect
#   3) Cooling access vs physical-health issues
#
# The public cleaned data include a few missing/irregular entries. To
# avoid treating many nearly-empty work-hour values as separate groups,
# work hours are grouped into four interpretable categories below.
# ============================================================

# Packages ---------------------------------------------------
required_packages <- c("readr", "dplyr", "tidyr")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) install.packages(missing_packages)

library(readr)
library(dplyr)
library(tidyr)

# Paths ------------------------------------------------------
input_file <- "data/heatwave_health_survey_cleaned.csv"
table_dir <- "results/tables"

if (!dir.exists(table_dir)) dir.create(table_dir, recursive = TRUE)
if (!file.exists(input_file)) {
  stop("Cleaned dataset not found. Run analysis/01_data_cleaning_and_preparation.R first.")
}

survey <- read_csv(input_file, show_col_types = FALSE) %>%
  mutate(
    work_hours_group = case_when(
      is.na(work_hours) ~ NA_character_,
      work_hours <= 6 ~ "<=6 hours",
      work_hours <= 8 ~ "7-8 hours",
      work_hours <= 10 ~ "9-10 hours",
      TRUE ~ ">=11 hours"
    ),
    work_hours_group = factor(
      work_hours_group,
      levels = c("<=6 hours", "7-8 hours", "9-10 hours", ">=11 hours")
    )
  )

# Helper: likelihood-ratio (G-test) statistic ---------------
likelihood_ratio_test <- function(tab) {
  expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  positive <- tab > 0 & expected > 0
  g2 <- 2 * sum(tab[positive] * log(tab[positive] / expected[positive]))
  df <- (nrow(tab) - 1) * (ncol(tab) - 1)
  p <- pchisq(g2, df = df, lower.tail = FALSE)
  list(statistic = g2, df = df, p_value = p)
}

# Helper: run one association analysis ----------------------
run_association <- function(data, row_var, col_var, test_name, file_stub) {
  dat <- data %>%
    transmute(
      row = as.character(.data[[row_var]]),
      col = as.character(.data[[col_var]])
    ) %>%
    filter(!is.na(row), row != "", !is.na(col), col != "")

  tab <- table(dat$row, dat$col)
  pearson <- suppressWarnings(chisq.test(tab, correct = FALSE))
  lr <- likelihood_ratio_test(tab)

  n <- sum(tab)
  min_dim <- min(nrow(tab) - 1, ncol(tab) - 1)
  cramers_v <- if (min_dim > 0) sqrt(as.numeric(pearson$statistic) / (n * min_dim)) else NA_real_

  expected_lt5 <- sum(pearson$expected < 5)
  expected_pct_lt5 <- 100 * expected_lt5 / length(pearson$expected)
  min_expected <- min(pearson$expected)

  # Monte Carlo chi-square p-value is useful when expected counts are sparse.
  set.seed(2026)
  monte_carlo_p <- if (expected_lt5 > 0) {
    suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = 10000)$p.value)
  } else {
    NA_real_
  }

  result <- tibble(
    test = test_name,
    n = n,
    rows = nrow(tab),
    columns = ncol(tab),
    pearson_chi_square = as.numeric(pearson$statistic),
    df = as.numeric(pearson$parameter),
    pearson_p_value = pearson$p.value,
    likelihood_ratio_chi_square = lr$statistic,
    likelihood_ratio_df = lr$df,
    likelihood_ratio_p_value = lr$p_value,
    cramers_v = cramers_v,
    cells_expected_below_5 = expected_lt5,
    percent_cells_expected_below_5 = expected_pct_lt5,
    minimum_expected_count = min_expected,
    monte_carlo_p_value_if_sparse = monte_carlo_p
  )

  write_csv(
    as.data.frame.matrix(tab) %>% tibble::rownames_to_column(row_var),
    file.path(table_dir, paste0(file_stub, "_contingency_table.csv"))
  )

  result
}

# 1) Working time vs monthly-income effect ------------------
res1 <- run_association(
  survey,
  "work_hours_group",
  "monthly_income_effect",
  "Working time group vs monthly-income effect",
  "chi_square_01_work_hours_vs_income"
)

# 2) Eating habits vs monthly-income effect -----------------
res2 <- run_association(
  survey,
  "eating_habit_change",
  "monthly_income_effect",
  "Eating habit vs monthly-income effect",
  "chi_square_02_eating_vs_income"
)

# 3) Cooling access vs physical-health issues ---------------
res3 <- run_association(
  survey,
  "cooling_access",
  "physical_health_issue",
  "Cooling access vs physical-health issue",
  "chi_square_03_cooling_vs_physical_health"
)

chi_square_results <- bind_rows(res1, res2, res3) %>%
  mutate(
    across(
      c(
        pearson_chi_square, pearson_p_value,
        likelihood_ratio_chi_square, likelihood_ratio_p_value,
        cramers_v, percent_cells_expected_below_5,
        minimum_expected_count, monte_carlo_p_value_if_sparse
      ),
      ~ round(.x, 4)
    ),
    interpretation = if_else(
      pearson_p_value < 0.05,
      "Evidence of association at alpha = 0.05",
      "No evidence of association at alpha = 0.05"
    )
  )

write_csv(
  chi_square_results,
  file.path(table_dir, "chi_square_test_summary.csv")
)

cat("\nChi-square analysis complete.\n")
cat("Summary: results/tables/chi_square_test_summary.csv\n")
cat("Contingency tables were saved separately in results/tables/.\n\n")
print(chi_square_results)
