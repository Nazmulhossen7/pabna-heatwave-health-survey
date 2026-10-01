# ============================================================
# 01_data_cleaning_and_preparation.R
# Project: Impact of Heatwaves on Human Health: A Case Study in Pabna
# Purpose: Clean, standardize, validate, and prepare the anonymized
#          300-respondent survey dataset for statistical analysis.
# ============================================================

# Packages ---------------------------------------------------
required_packages <- c("readr", "dplyr", "stringr")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(readr)
library(dplyr)
library(stringr)

# Paths ------------------------------------------------------
input_file <- "data/heatwave_health_survey_anonymized.csv"
clean_file <- "data/heatwave_health_survey_cleaned.csv"
quality_file <- "results/tables/data_quality_summary.csv"

# Create output folders when needed
if (!dir.exists("results")) dir.create("results")
if (!dir.exists("results/tables")) dir.create("results/tables", recursive = TRUE)

# Read data --------------------------------------------------
raw <- read_csv(input_file, show_col_types = FALSE)

cat("Rows:", nrow(raw), "\n")
cat("Columns:", ncol(raw), "\n")

# Helper functions ------------------------------------------
clean_text <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x[x == ""] <- NA_character_
  x
}

# Convert age entries such as '36m', '45 h', etc. to their numeric part.
# Values outside the study's intended 15-80 year range are flagged, not silently corrected.
parse_age <- function(x) {
  suppressWarnings(parse_number(as.character(x)))
}

# Convert height entries to total inches.
# Handles formats such as 5'5", 5 feet 5 inch, 5.7 (treated as 5 ft 7 in),
# plain 5 (treated as 5 ft), and 75 (treated as 75 inches).
parse_height_inches <- function(x) {
  s <- tolower(str_squish(as.character(x)))
  s[s %in% c("", "na", "nan")] <- NA_character_

  out <- rep(NA_real_, length(s))

  for (i in seq_along(s)) {
    z <- s[i]
    if (is.na(z)) next

    # Fix common spelling variants
    z <- str_replace_all(z, "feeet|fgeet", "feet")
    z <- str_replace_all(z, "inches", "inch")

    # Format: 5 feet 7 inch / 5 feet 7 / 5'7\" / 5'7 / 5'2''
    if (str_detect(z, "feet|foot|'") ) {
      nums <- str_extract_all(z, "\\d+(?:\\.\\d+)?")[[1]]
      if (length(nums) >= 1) {
        ft <- as.numeric(nums[1])
        inch <- ifelse(length(nums) >= 2, as.numeric(nums[2]), 0)
        out[i] <- ft * 12 + inch
      }
      next
    }

    # Numeric-like values
    num <- suppressWarnings(as.numeric(str_replace_all(z, '"', "")))
    if (is.na(num)) next

    # 48-84 is a plausible total-inch entry (e.g., 75 inches)
    if (num >= 48 && num <= 84) {
      out[i] <- num
    } else if (num >= 4 && num <= 7) {
      # Human-entered 5.7 generally means 5 ft 7 in in this survey file
      ft <- floor(num)
      decimal_part <- round((num - ft) * 10)
      if (decimal_part <= 11) {
        out[i] <- ft * 12 + decimal_part
      } else {
        out[i] <- num * 12
      }
    }
  }

  out
}

parse_weight <- function(x) {
  s <- str_replace_all(as.character(x), "\\s+", "")
  s <- str_replace_all(s, "(?i)kg", "")
  s <- str_replace_all(s, "(?<=\\d)\\.(?=\\s*\\d)", ".")
  suppressWarnings(parse_number(s))
}

# Parse work hours, using the midpoint for a range such as 6-7.
parse_work_hours <- function(x) {
  s <- tolower(str_squish(as.character(x)))
  s[s %in% c("", "na", "nan", "none")] <- NA_character_

  out <- rep(NA_real_, length(s))
  for (i in seq_along(s)) {
    z <- s[i]
    if (is.na(z)) next

    nums <- suppressWarnings(as.numeric(str_extract_all(z, "\\d+(?:\\.\\d+)?")[[1]]))
    nums <- nums[!is.na(nums)]

    if (length(nums) >= 2 && str_detect(z, "-")) {
      out[i] <- mean(nums[1:2])
    } else if (length(nums) >= 1) {
      out[i] <- nums[1]
    }
  }
  out
}

standardize_education <- function(x) {
  z <- str_to_lower(clean_text(x))
  case_when(
    is.na(z) ~ NA_character_,
    str_detect(z, "illiterate") ~ "Illiterate",
    z == "primary" ~ "Primary",
    z == "secondary" ~ "Secondary",
    str_detect(z, "higher secondary") ~ "Higher Secondary",
    str_detect(z, "under\\s*graduate|undergraduate") ~ "Undergraduate",
    z == "graduate" ~ "Graduate",
    str_detect(z, "degree") ~ "Degree",
    TRUE ~ str_to_title(z)
  )
}

# Clean and rename ------------------------------------------
clean <- raw %>%
  mutate(across(where(is.character), clean_text)) %>%
  transmute(
    respondent_id,
    sex = str_to_title(sex),
    age_raw = age_in_year,
    age = parse_age(age_in_year),
    height_raw = height_feet,
    height_inches = parse_height_inches(height_feet),
    weight_raw = weight_in_kg,
    weight_kg = parse_weight(weight_in_kg),
    education = standardize_education(level_of_education),
    marital_status = str_to_title(marital_status),
    occupation,
    socioeconomic_status = str_to_title(socio_economic_status),
    work_hours_raw = how_many_time_do_you_work_at_your_work_place_in_hours,
    work_hours = parse_work_hours(how_many_time_do_you_work_at_your_work_place_in_hours),
    health_change = do_you_have_any_health_changes_for_heatwaves_than_from_normal_weather,
    physical_health_issue = have_you_experienced_any_physical_health_issues_during_heatwaves,
    physical_health_issue_types = physical_health_issues_you_have_experienced_during_heatwaves,
    mental_health_change = have_you_noticed_any_changes_in_your_mental_health_during_heatwaves,
    mental_health_issue_types = mental_health_issues_you_have_experienced_during_heatwaves,
    sleep_impact = did_the_heatwaves_impact_your_sleeping_patterns,
    eating_habit_change = do_you_experience_changes_in_your_eating_habits_during_heatwaves,
    foods_consumed_more = types_of_food_consume_more_frequently,
    water_consumption = how_much_water_you_consume_during_heatwaves,
    hydration_priority = do_you_prioritize_hydration_and_consume_more_fluids_during_heatwaves,
    work_performance_effect = how_does_the_heatwave_affect_your_work_performance,
    productivity_decrease = have_you_observed_a_decrease_in_your_work_productivity_during_heatwaves,
    employer_support = do_you_receive_any_support_or_accommodations_from_your_employer_to_cope_with_heatwaves_and_maintain_work_performance,
    monthly_income_effect = did_heatwaves_effect_your_monthly_income,
    financial_challenges = do_you_face_any_financial_challenges_during_heatwaves,
    financially_prepared = are_you_financially_prepared_to_handle_unexpected_expenses_related_to_heatwaves,
    cooling_access = did_you_have_access_to_colling_facilities,
    cooling_facilities = if_yes_which_coolling_facilities_you_have_used
  ) %>%
  mutate(
    # Validation flags: preserve raw entries and flag implausible values.
    age_flag = is.na(age) | age < 15 | age > 80,
    height_flag = is.na(height_inches) | height_inches < 48 | height_inches > 84,
    weight_flag = is.na(weight_kg) | weight_kg < 25 | weight_kg > 250,
    work_hours_flag = !is.na(work_hours) & (work_hours < 0 | work_hours > 24),

    # Use only validated measurements to calculate BMI.
    height_m = if_else(!height_flag, height_inches * 0.0254, NA_real_),
    weight_kg_valid = if_else(!weight_flag, weight_kg, NA_real_),
    bmi = if_else(
      !is.na(height_m) & !is.na(weight_kg_valid) & height_m > 0,
      weight_kg_valid / (height_m^2),
      NA_real_
    ),
    bmi_category = case_when(
      is.na(bmi) ~ NA_character_,
      bmi < 18.5 ~ "Underweight",
      bmi < 25.0 ~ "Normal",
      bmi < 30.0 ~ "Overweight",
      TRUE ~ "Obesity"
    )
  )

# Data quality summary --------------------------------------
quality_summary <- tibble(
  check = c(
    "Total rows",
    "Duplicate respondent_id",
    "Missing values (all cells)",
    "Age values flagged",
    "Height values flagged",
    "Weight values flagged",
    "Work-hour values flagged"
  ),
  count = c(
    nrow(clean),
    sum(duplicated(clean$respondent_id)),
    sum(is.na(clean)),
    sum(clean$age_flag, na.rm = TRUE),
    sum(clean$height_flag, na.rm = TRUE),
    sum(clean$weight_flag, na.rm = TRUE),
    sum(clean$work_hours_flag, na.rm = TRUE)
  )
)

# Save outputs ----------------------------------------------
write_csv(clean, clean_file, na = "")
write_csv(quality_summary, quality_file)

# Console summary -------------------------------------------
cat("\nCleaning complete.\n")
cat("Clean dataset:", clean_file, "\n")
cat("Data-quality summary:", quality_file, "\n\n")
print(quality_summary)
