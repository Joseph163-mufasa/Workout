## load required packages in rstudio
library(tidyverse)
library(readr)
# load in results and stats data
renv::status()
tidyverse::tidyverse_packages()
library(readr)
results <- read_csv("data/results.csv")
View(results)

stats <- read_csv("data/stats.csv")
View(stats)



# Load required libraries
library(dplyr)
library(ggplot2)
library(caret)

# Import historical data
data <- read.csv("premier_league_data.csv")

# Preprocess data (adjust as needed based on your dataset)
data <- data %>%
  filter(!is.na(Goals)) %>%
  mutate(Win = ifelse(Result == "Win", 1, 0))

# Split data into training and testing sets
set.seed(123)
splitIndex <- createDataPartition(data$Win, p = 0.8, list = FALSE)
training <- data[splitIndex, ]
testing <- data[-splitIndex, ]

# Build a predictive model (e.g., logistic regression)
model <- train(
  Win ~ Goals + Shots + Possession + PassAccuracy,
  data = training,
  method = "glm",
  family = "binomial"
)

# Predict outcomes for the testing set
predictions <- predict(model, testing)
confusionMatrix(predictions, testing$Win)

# Visualize important features (e.g., using ggplot2)
important_features <- varImp(model)
print(important_features)

# Save the model for future predictions
saveRDS(model, file = "premier_league_model.rds")