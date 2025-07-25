# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Load dataset
df <- read_csv("C:/Users/erach/Downloads/Dataset_1_Odd.csv")

# Clean & preprocess (column types and factors)
df <- df %>%
  mutate(
    Sex = factor(Sex, levels = c(0, 1), labels = c("Female", "Male")),
    Smoking_Status = factor(Smoking_Status, levels = c(0, 1, 2),
                            labels = c("Non-Smoker", "Occasional", "Chainsmoker")),
    Hypertension = factor(Hypertension),
    Elevated_Risk = factor(`Elevated Risk`)
  )

# Descriptive Statistics
summary_stats <- df %>%
  summarise(
    mean_age = mean(Age),
    median_bmi = median(BMI),
    sd_heart_rate = sd(Heart_rate),
    max_glucose = max(`Glucose_mg/dL`),
    min_sleep = min(Daily_Sleeping_hours)
  )
print(summary_stats)

# Data Visualizations

# Histogram of Age
ggplot(df, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Age Distribution", x = "Age", y = "Count")

# Boxplot of BMI by Sex
ggplot(df, aes(x = Sex, y = BMI, fill = Sex)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "BMI by Sex", x = "Sex", y = "BMI")

# Scatter plot of Glucose vs. Cholesterol
ggplot(df, aes(x = `Glucose_mg/dL`, y = `Cholesterol_mg/dL`, color = Elevated_Risk)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Glucose vs. Cholesterol", x = "Glucose (mg/dL)", y = "Cholesterol (mg/dL)")

# Bar plot of Smoking Status
ggplot(df, aes(x = Smoking_Status, fill = Smoking_Status)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Smoking Status Distribution")

# Correlation between BMI and Systolic BP
cor_test <- cor.test(df$BMI, df$Systolic_BP)
print(cor_test)


