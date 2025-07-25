# Load necessary libraries
library(tidyverse)
library(janitor)  # for clean_names()

# Load dataset
df <- read_csv("C:/Users/erach/Downloads/Dataset_2_demographic.csv")

# Clean column names to lowercase and ensure uniqueness
df <- df %>% janitor::clean_names()

# View column names to confirm
print(names(df))

# Remove duplicated columns 
df <- df %>% select(-which(duplicated(names(.))))

# Check what columns exist and recode 
print("Available columns after cleaning:")
print(names(df))

# Create age groups 
if("age" %in% names(df)) {
  df <- df %>%
    mutate(
      age_group = case_when(
        age >= 18 & age <= 40 ~ "Young Age",
        age >= 41 & age <= 65 ~ "Middle Age", 
        age >= 66 & age <= 90 ~ "Old Age",
        TRUE ~ "Other"
      )
    )
}

# Convert categorical variables to factors 
if("sex" %in% names(df)) {
  df <- df %>% mutate(sex = factor(sex, levels = c(0, 1), labels = c("Female", "Male")))
}

if("socioeconomic" %in% names(df)) {
  df <- df %>% mutate(socioeconomic = factor(socioeconomic, levels = c(1, 2, 3),
                                             labels = c("Lower class", "Middle class", "Upper class")))
}

if("education" %in% names(df)) {
  df <- df %>% mutate(education = factor(education, levels = c(0, 1, 2, 3),
                                         labels = c("Uneducated", "Primary", "Secondary", "Tertiary")))
}

if("smoking_status" %in% names(df)) {
  df <- df %>% mutate(smoking_status = factor(smoking_status, levels = c(0, 1, 2),
                                              labels = c("Non-Smoker", "Occasional", "Chainsmoker")))
}

if("drinking_status" %in% names(df)) {
  df <- df %>% mutate(drinking_status = factor(drinking_status, levels = c(0, 1, 2),
                                               labels = c("Non-Drinker", "Casual", "Heavy")))
}

# 5. Descriptive statistics 
available_cols <- names(df)

summary_stats <- df %>%
  summarise(
    mean_age = if("age" %in% available_cols) mean(age, na.rm = TRUE) else NA,
    median_bmi = if("bmi" %in% available_cols) median(bmi, na.rm = TRUE) else NA,
    sd_literacy = if("health_literacy_score" %in% available_cols) sd(health_literacy_score, na.rm = TRUE) else NA,
    min_activity = if("physical_activity_hours_week" %in% available_cols) min(physical_activity_hours_week, na.rm = TRUE) else NA,
    max_satisfaction = if("patient_satisfaction_score" %in% available_cols) max(patient_satisfaction_score, na.rm = TRUE) else NA
  )
print(summary_stats)

# Visualizations 
# Histogram of BMI
if("bmi" %in% names(df)) {
  p1 <- ggplot(df, aes(x = bmi)) +
    geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
    theme_minimal() +
    labs(title = "BMI Distribution", x = "BMI", y = "Count")
  print(p1)
}

# Bar chart of Drinking Status
if("drinking_status" %in% names(df)) {
  p4 <- ggplot(df, aes(x = drinking_status, fill = drinking_status)) +
    geom_bar() +
    theme_minimal() +
    labs(title = "Drinking Status Distribution")
  print(p4)
}

# Bar chart of Age Group Distribution
if("age_group" %in% names(df)) {
  p5 <- ggplot(df, aes(x = age_group, fill = age_group)) +
    geom_bar() +
    theme_minimal() +
    labs(title = "Age Group Distribution", x = "Age Group", y = "Count")
  print(p5)
}

#Statistical Test Correlation 
#Remove NA values
df_clean <- df[, c("physical_activity_hours_week", "bmi")]
df_clean$physical_activity_hours_week <- as.numeric(df_clean$physical_activity_hours_week)
df_clean$bmi <- as.numeric(df_clean$bmi)
df_clean <- na.omit(df_clean)

# Pearson correlation 
cor_test <- cor.test(df$physical_activity_hours_week,df$bmi)
print(cor.test)


