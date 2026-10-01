# ============================================================
# 02_descriptive_analysis.R
# Project: Impact of Heatwaves on Human Health: A Case Study in Pabna
# Purpose: Produce reproducible descriptive statistics, frequency
#          tables, and publication-ready figures from the cleaned
#          300-respondent survey dataset.
# ============================================================

# Packages ---------------------------------------------------
required_packages <- c("readr", "dplyr", "tidyr", "stringr", "ggplot2", "scales")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)

# Paths ------------------------------------------------------
input_file <- "data/heatwave_health_survey_cleaned.csv"
table_dir <- "results/tables"
figure_dir <- "results/figures"

if (!dir.exists(table_dir)) dir.create(table_dir, recursive = TRUE)
if (!dir.exists(figure_dir)) dir.create(figure_dir, recursive = TRUE)

# Read cleaned data -----------------------------------------
if (!file.exists(input_file)) {
  stop(
    "Cleaned dataset not found. Run analysis/01_data_cleaning_and_preparation.R first."
  )
}

survey <- read_csv(input_file, show_col_types = FALSE)

cat("Respondents:", nrow(survey), "\n")
cat("Variables:", ncol(survey), "\n")

# Helper functions ------------------------------------------
frequency_table <- function(data, variable, variable_label) {
  data %>%
    transmute(category = .data[[variable]]) %>%
    mutate(
      category = as.character(category),
      category = str_squish(category),
      category = if_else(is.na(category) | category == "", "Missing", category)
    ) %>%
    count(category, name = "n") %>%
    mutate(
      variable = variable_label,
      percent = round(100 * n / sum(n), 2)
    ) %>%
    select(variable, category, n, percent)
}

numeric_summary <- function(data, variable, variable_label) {
  x <- data[[variable]]
  tibble(
    variable = variable_label,
    valid_n = sum(!is.na(x)),
    missing_n = sum(is.na(x)),
    mean = round(mean(x, na.rm = TRUE), 2),
    sd = round(sd(x, na.rm = TRUE), 2),
    median = round(median(x, na.rm = TRUE), 2),
    q1 = round(quantile(x, 0.25, na.rm = TRUE, names = FALSE), 2),
    q3 = round(quantile(x, 0.75, na.rm = TRUE, names = FALSE), 2),
    min = round(min(x, na.rm = TRUE), 2),
    max = round(max(x, na.rm = TRUE), 2)
  )
}

# 1. Demographic frequency tables --------------------------
demographic_summary <- bind_rows(
  frequency_table(survey, "sex", "Sex"),
  frequency_table(survey, "education", "Education"),
  frequency_table(survey, "marital_status", "Marital status"),
  frequency_table(survey, "socioeconomic_status", "Socio-economic status"),
  frequency_table(survey, "bmi_category", "BMI category")
)

write_csv(
  demographic_summary,
  file.path(table_dir, "demographic_frequency_summary.csv")
)

# 2. Key heatwave-related outcomes --------------------------
heatwave_outcome_summary <- bind_rows(
  frequency_table(survey, "health_change", "Health changes during heatwaves"),
  frequency_table(survey, "physical_health_issue", "Physical health issues"),
  frequency_table(survey, "mental_health_change", "Mental health changes"),
  frequency_table(survey, "sleep_impact", "Sleep pattern impacted"),
  frequency_table(survey, "eating_habit_change", "Eating habit change"),
  frequency_table(survey, "productivity_decrease", "Work productivity decrease"),
  frequency_table(survey, "monthly_income_effect", "Monthly income effect"),
  frequency_table(survey, "financial_challenges", "Financial challenges"),
  frequency_table(survey, "financially_prepared", "Financial preparedness"),
  frequency_table(survey, "cooling_access", "Access to cooling facilities")
)

write_csv(
  heatwave_outcome_summary,
  file.path(table_dir, "heatwave_outcome_frequency_summary.csv")
)

# 3. Numeric descriptive statistics ------------------------
numeric_descriptive_summary <- bind_rows(
  numeric_summary(survey, "age", "Age (years)"),
  numeric_summary(survey, "work_hours", "Work time (hours/day)"),
  numeric_summary(survey, "weight_kg_valid", "Weight (kg)"),
  numeric_summary(survey, "height_inches", "Height (inches)"),
  numeric_summary(survey, "bmi", "BMI")
)

write_csv(
  numeric_descriptive_summary,
  file.path(table_dir, "numeric_descriptive_summary.csv")
)

# 4. Multi-response fields: preserve original combinations --
# These tables summarize the combinations exactly as recorded.
# They do not attempt to infer or recode ambiguous free-text responses.
physical_issue_combinations <- frequency_table(
  survey,
  "physical_health_issue_types",
  "Physical health issue combination"
) %>%
  arrange(desc(n))

mental_issue_combinations <- frequency_table(
  survey,
  "mental_health_issue_types",
  "Mental health issue combination"
) %>%
  arrange(desc(n))

write_csv(
  physical_issue_combinations,
  file.path(table_dir, "physical_health_issue_combinations.csv")
)
write_csv(
  mental_issue_combinations,
  file.path(table_dir, "mental_health_issue_combinations.csv")
)

# 5. Figures -------------------------------------------------
# Figure 1: Sex distribution
sex_plot_data <- demographic_summary %>%
  filter(variable == "Sex", category != "Missing")

p1 <- ggplot(sex_plot_data, aes(x = reorder(category, -percent), y = percent)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(percent, "%")), vjust = -0.35, size = 4) +
  scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Sex Distribution of Survey Respondents",
    x = NULL,
    y = "Respondents (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank())

ggsave(
  file.path(figure_dir, "01_sex_distribution.png"),
  p1,
  width = 7,
  height = 5,
  dpi = 300
)

# Figure 2: Socio-economic status
ses_order <- c("Poorest", "Poor", "Middle", "Rich", "Richest")
ses_plot_data <- demographic_summary %>%
  filter(variable == "Socio-economic status", category != "Missing") %>%
  mutate(category = factor(category, levels = ses_order))

p2 <- ggplot(ses_plot_data, aes(x = category, y = percent)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(percent, "%")), vjust = -0.35, size = 3.8) +
  scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Socio-economic Status of Respondents",
    x = NULL,
    y = "Respondents (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank())

ggsave(
  file.path(figure_dir, "02_socioeconomic_status.png"),
  p2,
  width = 7.5,
  height = 5,
  dpi = 300
)

# Figure 3: Percentage reporting selected heatwave impacts
key_yes_variables <- tibble(
  variable = c(
    "Health changes during heatwaves",
    "Physical health issues",
    "Mental health changes",
    "Sleep pattern impacted",
    "Access to cooling facilities"
  ),
  display_label = c(
    "Health changes",
    "Physical health issues",
    "Mental health changes",
    "Sleep disruption",
    "Cooling access"
  )
)

key_yes_plot_data <- heatwave_outcome_summary %>%
  inner_join(key_yes_variables, by = "variable") %>%
  filter(str_to_lower(category) == "yes") %>%
  select(display_label, percent)

p3 <- ggplot(
  key_yes_plot_data,
  aes(x = reorder(display_label, percent), y = percent)
) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(percent, "%")), hjust = -0.15, size = 3.8) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 105),
    breaks = seq(0, 100, 20),
    labels = label_percent(scale = 1)
  ) +
  labs(
    title = "Selected Heatwave-related Survey Outcomes",
    x = NULL,
    y = "Respondents reporting 'Yes' (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

ggsave(
  file.path(figure_dir, "03_key_heatwave_outcomes.png"),
  p3,
  width = 8,
  height = 5.5,
  dpi = 300
)

# Figure 4: Eating habits during heatwaves
eating_plot_data <- heatwave_outcome_summary %>%
  filter(variable == "Eating habit change", category != "Missing") %>%
  arrange(percent)

p4 <- ggplot(
  eating_plot_data,
  aes(x = reorder(category, percent), y = percent)
) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(percent, "%")), hjust = -0.15, size = 3.8) +
  coord_flip() +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Changes in Eating Habits During Heatwaves",
    x = NULL,
    y = "Respondents (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

ggsave(
  file.path(figure_dir, "04_eating_habits.png"),
  p4,
  width = 8,
  height = 5,
  dpi = 300
)

# Figure 5: Monthly income effect
income_plot_data <- heatwave_outcome_summary %>%
  filter(variable == "Monthly income effect", category != "Missing") %>%
  arrange(percent)

p5 <- ggplot(
  income_plot_data,
  aes(x = reorder(category, percent), y = percent)
) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(percent, "%")), hjust = -0.15, size = 3.8) +
  coord_flip() +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Reported Effect of Heatwaves on Monthly Income",
    x = NULL,
    y = "Respondents (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

ggsave(
  file.path(figure_dir, "05_monthly_income_effect.png"),
  p5,
  width = 7.5,
  height = 5,
  dpi = 300
)

# Console summary -------------------------------------------
cat("\nDescriptive analysis complete.\n")
cat("Tables saved to:", table_dir, "\n")
cat("Figures saved to:", figure_dir, "\n\n")

cat("Key respondent counts from the cleaned dataset:\n")
print(
  heatwave_outcome_summary %>%
    filter(
      variable %in% c(
        "Health changes during heatwaves",
        "Physical health issues",
        "Mental health changes",
        "Sleep pattern impacted"
      ),
      category == "Yes"
    ) %>%
    select(variable, n, percent)
)
