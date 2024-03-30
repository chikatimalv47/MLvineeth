# Load necessary libraries
library(readr)
library(caret)
library(dplyr)

# Read the dataset
file_path <- file.choose("/Users/vineethkumar/Desktop/oulad-students.csv")
data <- read_csv(file_path)

# Data preprocessing
data <- data %>%
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

# Split the data into training and testing sets (80% training, 20% testing)
set.seed(123) # For reproducibility
train_index <- createDataPartition(data$final_result, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train the classification model (logistic regression)
model <- glm(final_result ~ ., data = train_data, family = binomial)

# Make predictions on test data
predictions <- predict(model, newdata = test_data, type = "response")
predictions <- ifelse(predictions > 0.5, "Pass", "Fail")

# Ensure factor levels are consistent
predictions <- factor(predictions, levels = levels(test_data$final_result))

# Evaluate the model
confusionMatrix(predictions, test_data$final_result)

