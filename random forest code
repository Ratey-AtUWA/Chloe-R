#ML
install.packages("randomForest")
install.packages("caret")
install.packages("tidyverse")

library(dplyr)
library(readxl)
library(randomForest)
library(caret)
library(tidyverse)

library(randomForest)
library(dplyr)
library(caret)


file_path <- "C:/Users/chloe/Desktop/KF1.csv"
data <- read.csv(file_path)


data <- data %>%
  mutate(
    Lag_1 = lag(device.69, 1),
    Lag_2 = lag(device.69, 2),
    Lag_3 = lag(device.69, 3)
  )

library(dplyr)


data <- data %>%
  mutate(
    num_col1 = if_else(is.na(num_col1), mean(num_col1, na.rm = TRUE), num_col1),
    num_col2 = if_else(is.na(num_col2), median(num_col2, na.rm = TRUE), num_col2)
  )

set.seed(123) 
train_size <- floor(0.8 * nrow(data))

train_indices <- sample(seq_len(nrow(data)), size = train_size)

trainData <- data[train_indices, ]
testData <- data[-train_indices, ]
trainData <- na.omit(trainData)
print(nrow(trainData))
print(length(trainData$Lag_1))
print(length(trainData$Lag_2))
print(length(trainData$Lag_3))
trainDataClean <- na.omit(trainData)
testDataClean <- na.omit(testData)

sum(is.na(trainDataClean)) # should be zero

# train random forest model
# uese randomForest
X_train <- trainDataClean[, c("Lag_1", "Lag_2", "Lag_3")] 
y_train <- as.numeric(trainDataClean$`device.69`)
trainDataClean$`device.69` <- as.numeric(as.character(trainDataClean$`device.69`))
trainDataClean <- na.omit(trainDataClean)
X_train <- trainDataClean[, c("Lag_1", "Lag_2", "Lag_3")]
y_train <- trainDataClean$`device.69`  #  no NA
# use matrix and vetor tain model 
model_rf <- randomForest(x = X_train, y = y_train)
print(model_rf)
