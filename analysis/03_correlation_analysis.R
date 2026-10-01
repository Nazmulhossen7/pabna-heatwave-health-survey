# ============================================================
# 03_correlation_analysis.R
# Project: Impact of Heatwaves on Human Health: A Case Study in Pabna
# Purpose: Reproduce the quantitative-variable correlation analysis
#          using the cleaned public dataset.
# ============================================================

# Packages ---------------------------------------------------
required_packages <- c("readr", "dplyr", "tidyr", "ggplot2")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) install.packages(missing_packages)

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

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

# Quantitative variables used in the original report --------
quantitative <- survey %>%
  transmute(
    BMI = bmi,
    Age = age,
    Height = height_inches,
    Weight = weight_kg_valid,
    Working_Time = work_hours
  )

# Pearson correlation matrix (pairwise complete observations)
cor_matrix <- cor(quantitative, use = "pairwise.complete.obs", method = "pearson")
write_csv(
  as.data.frame(cor_matrix) %>% tibble::rownames_to_column("variable"),
  file.path(table_dir, "pearson_correlation_matrix.csv")
)

# Pairwise correlations with sample size and p-value --------
vars <- names(quantitative)
pairs <- combn(vars, 2, simplify = FALSE)

pairwise_results <- lapply(pairs, function(pair) {
  x <- quantitative[[pair[1]]]
  y <- quantitative[[pair[2]]]
  ok <- complete.cases(x, y)

  if (sum(ok) < 3 || sd(x[ok]) == 0 || sd(y[ok]) == 0) {
    return(tibble(
      variable_1 = pair[1], variable_2 = pair[2],
      n = sum(ok), r = NA_real_, p_value = NA_real_
    ))
  }

  test <- cor.test(x[ok], y[ok], method = "pearson")
  tibble(
    variable_1 = pair[1],
    variable_2 = pair[2],
    n = sum(ok),
    r = unname(test$estimate),
    p_value = test$p.value
  )
}) %>% bind_rows() %>%
  mutate(
    r = round(r, 3),
    p_value = signif(p_value, 4),
    significance = case_when(
      is.na(p_value) ~ NA_character_,
      p_value < 0.001 ~ "p < .001",
      p_value < 0.01 ~ "p < .01",
      p_value < 0.05 ~ "p < .05",
      TRUE ~ "Not significant"
    ),
    strength = case_when(
      is.na(r) ~ NA_character_,
      abs(r) < 0.10 ~ "Negligible",
      abs(r) < 0.30 ~ "Weak",
      abs(r) < 0.50 ~ "Moderate",
      abs(r) < 0.70 ~ "Strong",
      TRUE ~ "Very strong"
    )
  )

write_csv(pairwise_results, file.path(table_dir, "pearson_correlation_pairwise.csv"))

# Heatmap ----------------------------------------------------
cor_long <- as.data.frame(cor_matrix) %>%
  tibble::rownames_to_column("variable_1") %>%
  pivot_longer(-variable_1, names_to = "variable_2", values_to = "r") %>%
  mutate(
    variable_1 = factor(variable_1, levels = vars),
    variable_2 = factor(variable_2, levels = rev(vars))
  )

p <- ggplot(cor_long, aes(x = variable_1, y = variable_2, fill = r)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", r)), size = 4) +
  scale_fill_gradient2(limits = c(-1, 1), midpoint = 0, name = "Pearson r") +
  labs(
    title = "Pearson Correlation Matrix",
    subtitle = "Quantitative variables in the Pabna heatwave-health survey",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

ggsave(
  file.path(figure_dir, "06_pearson_correlation_heatmap.png"),
  p,
  width = 8,
  height = 6.5,
  dpi = 300
)

cat("\nCorrelation analysis complete.\n")
cat("Correlation matrix: results/tables/pearson_correlation_matrix.csv\n")
cat("Pairwise tests: results/tables/pearson_correlation_pairwise.csv\n")
cat("Heatmap: results/figures/06_pearson_correlation_heatmap.png\n\n")
print(pairwise_results)
