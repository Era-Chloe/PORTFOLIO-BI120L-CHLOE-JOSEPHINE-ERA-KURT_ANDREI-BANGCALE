data <- read.csv("C:/Users/CHRISPOGI/Downloads/1_Vital_signs_diagnosis_data_Group_017.csv", stringsAsFactors = FALSE)

#Installing necessary packages
library(MASS)
library(Hmisc)
library(psych)
library(modeest)
library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyr)


#Inspect the data
str(data)
summary(data)

#data cleaning
which(data$BMI < 10 | data$BMI > 70)
colSums(is.na(data))
data <- data %>%
  select(-starts_with("Unnamed"))
data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
data<- data %>%
  filter(Height_cm > 0 & Weight_kg > 0 & Systolic_BP > 0 & Diastolic_BP > 0)

#Convert data types to appropriate formats
data$Smoking_status_Legend <- factor(data$Smoking_status_Legend, 
                                           levels = c("0 - Non-Smoker", "1 - Occasional", "2 - Chainsmoker"),
                                           labels = c("Non-Smoker", "Occasional", "Chainsmoker"))
data$Hypertension_Legend <- factor(data$Hypertension_Legend, 
                                         levels = c("0 - Hypotensive", "1 - Normal", "2 - Elevated", 
                                                    "3 - Hypertension Stage 1", "4 - Hypertension Stage 2"),
                                         labels = c("Hypotensive", "Normal", "Elevated", "Hypertension Stage 1", 
                                                    "Hypertension Stage 2"))


#Descriptive statistics
descriptive_stats <- data %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    SD_Age = sd(Age, na.rm = TRUE),
    Mean_BMI = mean(BMI, na.rm = TRUE),
    Median_BMI = median(BMI, na.rm = TRUE),
    SD_BMI = sd(BMI, na.rm = TRUE),
    Mean_Systolic_BP = mean(Systolic_BP, na.rm = TRUE),
    SD_Systolic_BP = sd(Systolic_BP, na.rm = TRUE),
    Mean_Heart_rate = mean(Heart_rate, na.rm = TRUE),
    SD_Heart_rate = sd(Heart_rate, na.rm = TRUE)
  )

print(descriptive_stats)

#Histogram for Age
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")

#Histogram for Heart rate
ggplot(data, aes(x = Heart_rate)) +
  geom_histogram(binwidth = 5, fill = "pink", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Heart Rate", x = "Heart Rate", y = "Frequency")

#Plot for Age vs Heart rate
ggplot(data, aes(x = Age, y = Heart_rate)) +
  geom_point(color = "green", alpha = 0.6) +
  theme_minimal() +
  labs(title = "Age vs. Heart Rate", x = "Age", y = "Heart Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Plot for Stress level
ggplot(data, aes(x = factor(Stress_Level))) +
  geom_bar(fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(title = "Stress Level Distribution", x = "Stress Level", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Plot for Age vs stress level
ggplot(data, aes(x = factor(Stress_Level), y = Age)) +
  geom_boxplot(fill = "yellow", color = "black") +
  theme_minimal() +
  labs(title = "Age vs. Stress Level", x = "Stress Level", y = "Age") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Pearson/Spearman Correlation Coefficient tests for age vs heart rate/stress level
cor.test(data$Age, data$Heart_rate, method = "pearson")
cor.test(data$Age, data$Stress_Level, method = "spearman", exact = FALSE)

#Simple Linear/Logistic Regression for Age vs heart rate/Stress level

Simple_Linear_Age <- lm(Heart_rate ~ Age, data = data)
print(Simple_Linear_Age)
summary(Simple_Linear_Age)
ggplot(data, aes(x = Age, y = Heart_rate)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear regression line
  theme_minimal() +
  labs(title = "Age vs. Heart Rate", 
       x = "Age", y = "Heart Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model_age_stress <- polr(factor(Stress_Level) ~ Age, data = data, method = "logistic")
summary(model_age_stress)
age_seq <- seq(min(data$Age), max(data$Age), length.out = 100)
predicted_probs <- predict(model_age_stress, newdata = data.frame(Age = age_seq), type = "probs")
predicted_df <- data.frame(Age = age_seq, predicted_probs)
predicted_df <- gather(predicted_df, key = "Stress_Level", value = "Probability", -Age)
ggplot(predicted_df, aes(x = Age, y = Probability, color = Stress_Level)) +
  geom_line(size = 1) + 
  theme_minimal() +
  labs(title = "Stress Level prediction with age", x = "Age", y = "Predicted Probability of Stress") +
  scale_color_brewer(palette = "Set1", name = "Stress Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

