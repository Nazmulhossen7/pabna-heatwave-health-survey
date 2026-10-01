# ============================================================
# 05_logistic_regression.R
# Project: Impact of Heatwaves on Human Health: A Case Study in Pabna
# Purpose: Fit a transparent binary logistic-regression model and
#          report odds ratios, an omnibus likelihood-ratio test,
#          Hosmer-Lemeshow goodness-of-fit, and a forest plot.
#
# IMPORTANT REPRODUCIBILITY NOTE
# The original field-survey report's model-fitting paragraph uses
# inconsistent labels for the dependent and independent variables
# (it mentions "Having Dining Meal", sleeping pattern, and monthly
# income in conflicting roles). Therefore this script DOES NOT claim
# to reproduce that exact SPSS model. Instead, it defines a clear,
# reproducible extension that stays close to the report's variables:
#
# Outcome: whether heatwaves impacted sleeping patterns (Yes = 1).
# Predictors: age, sex, working hours, BMI, and monthly-income effect.
# ============================================================

# Packages ---------------------------------------------------
required_packages <- c("readr", "dplyr", "tidyr", "ggplot2", "broom")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) install.packages(missing_packages)

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)

# Paths ------------------------------------------------------
input_file <- "data/heatwave_health_survey_cleaned.csv"
table_dir <- "results/tables"
figure_dir <- "results/figures"

if (!dir.exists(table_dir)) dir.create(table_dir, recursive = TRUE)
if (!dir.exists(figure_dir)) dir.create(figure_dir, recursive = TRUE)
if (!file.exists(input_file)) {
  stop("Cleaned dataset not found. Run analysis/01_data_cleaning_and_preparation.R first.")
}

survey <- read_csv(input_file, show_col_types = FALSE)

# Prepare model data ----------------------------------------
model_data <- survey %>%
  transmute(
    respondent_id,
    sleep_impact_binary = case_when(
      sleep_impact == "Yes" ~ 1,
      sleep_impact == "No" ~ 0,
      TRUE ~ NA_real_
    ),
    age = age,
    sex = factor(sex),
    work_hours = work_hours,
    bmi = bmi,
    monthly_income_effect = factor(monthly_income_effect)
  ) %>%
  filter(complete.cases(.)) %>%
  mutate(
    sex = relevel(sex, ref = "Female"),
    monthly_income_effect = relevel(monthly_income_effect, ref = "Remaining same")
  )

if (nrow(model_data) < 50) {
  stop("Too few complete observations for the planned logistic-regression model.")
}

# Fit full and null models ----------------------------------
full_model <- glm(
  sleep_impact_binary ~ age + sex + work_hours + bmi + monthly_income_effect,
  data = model_data,
  family = binomial(link = "logit")
)

null_model <- glm(
  sleep_impact_binary ~ 1,
  data = model_data,
  family = binomial(link = "logit")
)

# Coefficients and odds ratios ------------------------------
coef_table <- broom::tidy(full_model) %>%
  mutate(
    odds_ratio = exp(estimate),
    conf_low_95 = exp(estimate - 1.96 * std.error),
    conf_high_95 = exp(estimate + 1.96 * std.error),
    significance = case_when(
      p.value < 0.001 ~ "p < .001",
      p.value < 0.01 ~ "p < .01",
      p.value < 0.05 ~ "p < .05",
      TRUE ~ "Not significant"
    )
  ) %>%
  select(
    term, estimate, std.error, statistic, p.value,
    odds_ratio, conf_low_95, conf_high_95, significance
  )

write_csv(
  coef_table,
  file.path(table_dir, "logistic_regression_odds_ratios.csv")
)

# Omnibus likelihood-ratio test -----------------------------
lr_chi_square <- deviance(null_model) - deviance(full_model)
lr_df <- df.residual(null_model) - df.residual(full_model)
lr_p <- pchisq(lr_chi_square, df = lr_df, lower.tail = FALSE)

omnibus <- tibble(
  n = nrow(model_data),
  null_deviance = deviance(null_model),
  model_deviance = deviance(full_model),
  likelihood_ratio_chi_square = lr_chi_square,
  df = lr_df,
  p_value = lr_p,
  AIC = AIC(full_model)
)

write_csv(
  omnibus,
  file.path(table_dir, "logistic_regression_model_fit.csv")
)

# Hosmer-Lemeshow test --------------------------------------
# Implemented directly to avoid a package dependency.
# Predicted probabilities are split into up to 10 groups.
model_data <- model_data %>%
  mutate(
    predicted_probability = predict(full_model, type = "response"),
    hl_group = ntile(predicted_probability, 10)
  )

hl_table <- model_data %>%
  group_by(hl_group) %>%
  summarise(
    n = n(),
    observed_yes = sum(sleep_impact_binary),
    observed_no = n - observed_yes,
    expected_yes = sum(predicted_probability),
    expected_no = n - expected_yes,
    .groups = "drop"
  )

small <- 1e-10
hl_chi_square <- sum(
  (hl_table$observed_yes - hl_table$expected_yes)^2 / pmax(hl_table$expected_yes, small) +
    (hl_table$observed_no - hl_table$expected_no)^2 / pmax(hl_table$expected_no, small)
)
hl_df <- max(nrow(hl_table) - 2, 1)
hl_p <- pchisq(hl_chi_square, df = hl_df, lower.tail = FALSE)

hl_summary <- tibble(
  groups = nrow(hl_table),
  chi_square = hl_chi_square,
  df = hl_df,
  p_value = hl_p,
  interpretation = if_else(
    hl_p >= 0.05,
    "No evidence of lack of fit at alpha = 0.05",
    "Evidence of lack of fit at alpha = 0.05"
  )
)

write_csv(hl_table, file.path(table_dir, "hosmer_lemeshow_groups.csv"))
write_csv(hl_summary, file.path(table_dir, "hosmer_lemeshow_summary.csv"))

# Save respondent-level predictions -------------------------
prediction_output <- model_data %>%
  select(respondent_id, sleep_impact_binary, predicted_probability)
write_csv(
  prediction_output,
  file.path(table_dir, "logistic_regression_predictions.csv")
)

# Odds-ratio forest plot ------------------------------------
plot_data <- coef_table %>%
  filter(term != "(Intercept)") %>%
  mutate(term = gsub("monthly_income_effect", "Income: ", term),
         term = gsub("sex", "Sex: ", term),
         term = gsub("work_hours", "Work hours", term),
         term = gsub("age", "Age", term),
         term = gsub("bmi", "BMI", term))

p <- ggplot(
  plot_data,
  aes(x = odds_ratio, y = reorder(term, odds_ratio))
) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf_low_95, xmax = conf_high_95), height = 0.2) +
  geom_point(size = 2.5) +
  scale_x_log10() +
  labs(
    title = "Logistic Regression: Factors Associated with Sleep Impact",
    subtitle = "Odds ratios with 95% Wald confidence intervals",
    x = "Odds ratio (log scale)",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(figure_dir, "07_logistic_regression_odds_ratios.png"),
  p,
  width = 8.5,
  height = 5.5,
  dpi = 300
)

cat("\nLogistic regression complete.\n")
cat("Complete-case model N:", nrow(model_data), "\n")
cat("Odds ratios: results/tables/logistic_regression_odds_ratios.csv\n")
cat("Model fit: results/tables/logistic_regression_model_fit.csv\n")
cat("Hosmer-Lemeshow: results/tables/hosmer_lemeshow_summary.csv\n")
cat("Forest plot: results/figures/07_logistic_regression_odds_ratios.png\n\n")
print(omnibus)
print(hl_summary)
