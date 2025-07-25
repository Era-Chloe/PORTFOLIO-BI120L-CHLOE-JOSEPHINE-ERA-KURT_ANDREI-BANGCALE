data <- read.csv("C:/Users/CHRISPOGI/Downloads/3_Nutritional_Dietary_data_Group_017.csv", stringsAsFactors = FALSE)

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
data <- data %>%
  filter(BMI > 10 & BMI < 50,
         Protein_intake_g > 10 & Protein_intake_g < 300,
         Daily_Caloric_Intake_kcal > 1000 & Daily_Caloric_Intake_kcal < 5000)

#Convert data types to appropriate formats
data$Patient_ID <- as.integer(data$Patient.ID)
data$Body_Fat_Percent <- as.numeric(data$Body_Fat_percent)
data$Muscle_Mass_kg <- as.numeric(data$Muscle_Mass_kg)
data$BMI <- as.numeric(data$BMI)
data$Physical_Activity_Hours_Week <- as.numeric(data$Physical_Activity_Hours_Week)
data$Daily_Caloric_Intake_kcal <- as.numeric(data$Daily_Caloric_Intake_kcal)
data$Protein_Intake_g <- as.numeric(data$Protein_intake_g)
data$Fat_Intake_g <- as.numeric(data$Fat_intake_g)
data$Carbohydrate_Intake_g <- as.numeric(data$Carbohydrate_intake_g)
data$Vitamin_C_mg <- as.numeric(data$Vitamin_C_mg)
data$Iron_mg <- as.numeric(data$Iron_mg)
data$Water_Intake_ml <- as.numeric(data$Water_intake_ml)


#Descriptive statistics
descriptive_stats <- data %>%
  summarise(
    Mean_Body_Fat_Percent = mean(Body_Fat_percent, na.rm = TRUE),
    Median_Body_Fat_Percent = median(Body_Fat_percent, na.rm = TRUE),
    SD_Body_Fat_Percent = sd(Body_Fat_percent, na.rm = TRUE),
    Mean_Muscle_Mass_kg = mean(Muscle_Mass_kg, na.rm = TRUE),
    Median_Muscle_Mass_kg = median(Muscle_Mass_kg, na.rm = TRUE),
    SD_Muscle_Mass_kg = sd(Muscle_Mass_kg, na.rm = TRUE),
    Mean_BMI = mean(BMI, na.rm = TRUE),
    Median_BMI = median(BMI, na.rm = TRUE),
    SD_BMI = sd(BMI, na.rm = TRUE),
    Mean_Caloric_Intake_kcal = mean(Daily_Caloric_Intake_kcal, na.rm = TRUE),
    Median_Caloric_Intake_kcal = median(Daily_Caloric_Intake_kcal, na.rm = TRUE),
    SD_Caloric_Intake_kcal = sd(Daily_Caloric_Intake_kcal, na.rm = TRUE),
    Mean_Protein_Intake_g = mean(Protein_intake_g, na.rm = TRUE),
    Median_Protein_Intake_g = median(Protein_intake_g, na.rm = TRUE),
    SD_Protein_Intake_g = sd(Protein_intake_g, na.rm = TRUE),
    Mean_Fat_Intake_g = mean(Fat_intake_g, na.rm = TRUE),
    Median_Fat_Intake_g = median(Fat_intake_g, na.rm = TRUE),
    SD_Fat_Intake_g = sd(Fat_intake_g, na.rm = TRUE)
  )
summary(descriptive_stats)

#Plot for Body Fat %
ggplot(data, aes(x = Body_Fat_Percent)) + 
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Body Fat Percentage", x = "Body Fat Percentage", y = "Frequency")

#Plot for Muscle mass (kg)
ggplot(data, aes(x = Muscle_Mass_kg)) + 
  geom_histogram(binwidth = 5, fill = "orange", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Muscle Mass (kg)", x = "Muscle Mass (kg)", y = "Frequency")


#Plot for Physical Activity Hours per Week
ggplot(data, aes(x = Physical_Activity_Hours_Week)) + 
  geom_histogram(binwidth = 2, fill = "lightgreen", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Physical Activity Hours per Week", x = "Physical Activity Hours per Week", y = "Frequency")


#Pearson Correlation Coefficient tests for physical activity hours

cor.test(data$Physical_Activity_Hours_Week, data$Body_Fat_percent, method = "pearson")
cor.test(data$Physical_Activity_Hours_Week, data$Muscle_Mass_kg, method = "pearson")

ggplot(data, aes(x = Physical_Activity_Hours_Week, y = Body_Fat_Percent)) +
  geom_point(color = "purple", alpha = 0.7) + # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Linear regression line (Pearson's correlation line)
  theme_minimal() +
  labs(title = "Physical Activity Hours per Week vs Body Fat Percentage (Pearson's Correlation)", 
       x = "Physical Activity Hours per Week", 
       y = "Body Fat Percentage")

ggplot(data, aes(x = Physical_Activity_Hours_Week, y = Muscle_Mass_kg)) +
  geom_point(color = "blue", alpha = 0.7) + # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Linear regression line (Pearson's correlation line)
  theme_minimal() +
  labs(title = "Physical Activity Hours per Week vs Muscle Mass (kg) (Pearson's Correlation)", 
       x = "Physical Activity Hours per Week", 
       y = "Muscle Mass (kg)")

#Simple Linear Regression for physical activity hours per week vs body fat/muscle mass

linear_model_body_fat <- lm(Body_Fat_Percent ~ Physical_Activity_Hours_Week, data = data)
summary(linear_model_body_fat)

linear_model_muscle_mass <- lm(Muscle_Mass_kg ~ Physical_Activity_Hours_Week, data = data)
summary(linear_model_muscle_mass)


