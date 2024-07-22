# Load necessary libraries
library(ggplot2)
# Load the data
all  <- read.csv(file.choose())

# Data Preprocessing
data <- data %>%
  filter(!is.na(first_innings_score)) %>%
  mutate_if(is.character, as.factor)

# Splitting the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$first_innings_score, p = 0.8, list = FALSE)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]

# Building the Linear Regression Model
model <- lm(first_innings_score ~ ., data = trainData)

# Summary of the model
summary(model)

# Making predictions on the test set
predictions <- predict(model, testData)

# Evaluating the model
MAE <- mean(abs(predictions - testData$first_innings_score))
RMSE <- sqrt(mean((predictions - testData$first_innings_score)^2))

print(paste("MAE: ", MAE))
print(paste("RMSE: ", RMSE))

# Predicting for new data
new_data <- data.frame(...) # add your new data here
predicted_score <- predict(model, new_data)
print(predicted_score)
