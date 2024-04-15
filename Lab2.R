# Load necessary libraries
library(readr)
library(caret)
library(dplyr)

# Choose the file path for the students dataset
students_file_path <- file.choose("Choose the students dataset file")

# Read the students dataset
students_data <- read_csv(students_file_path)

# Data preprocessing for students dataset
students_data <- students_data %>%
  mutate(
    code_module = as.factor(code_module),
    code_presentation = as.factor(code_presentation),
    gender = as.factor(gender),
    region = as.factor(region),
    highest_education = as.factor(highest_education),
    imd_band = as.factor(imd_band),
    age_band = as.factor(age_band),
    num_of_prev_attempts = as.integer(as.character(num_of_prev_attempts)),
    disability = as.factor(disability),
    final_result = as.factor(final_result)
  ) %>%
  na.omit()

# Choose the file path for the assessments dataset
assessments_file_path <- file.choose("Choose the assessments dataset file")

# Read the assessments dataset
assessments_data <- read_csv(assessments_file_path)

# Data preprocessing for assessments dataset
assessments_data <- assessments_data %>%
  mutate(
    code_module = as.factor(code_module),
    code_presentation = as.factor(code_presentation),
    assessment_type = as.factor(assessment_type)
  )

# Merge assessment scores with student data
merged_data <- left_join(students_data, assessments_data, by = c("id_student", "code_module", "code_presentation"))

# Train the classification model (logistic regression)
model_classification <- glm(final_result ~ ., data = students_data, family = binomial)

# Train the regression model (linear regression)
model_regression <- lm(score ~ ., data = merged_data)

# Interpret the model coefficients
summary(model_regression)
summary(model_classification)
