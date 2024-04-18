

#ML RANDOM FOREST( SIMPLE WAY WORKING NOW)
# 1. 
file_path <- "C:/Users/chloe/Desktop/KF2.csv"
data <- read.csv(file_path)



# 2. 

data <- data.frame(lapply(data, function(x) {
  if (is.numeric(x)) {
    # 对数值型数据填充缺失值为该列的均值
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  } else {
    # 对非数值型数据，使用众数或默认值填充
    freq <- table(x)
    if (length(freq) == 0) {
      # 如果列完全由NA组成，使用一个默认值
      x[is.na(x)] <- "Unknown"  # 选择一个合适的默认值
    } else {
      # 使用众数填充
      mode_value <- names(which.max(freq))
      x[is.na(x)] <- mode_value
    }
  }
  return(x)
}))


# 3. 
set.seed(123)
train_index <- sample(1:nrow(data), 0.8 * nrow(data), replace = FALSE)
trainData <- data[train_index, ]
testData <- data[-train_index, ]

# 4. 
model_rf <- randomForest(device.81 ~ ., data = trainData, ntree = 500)

# 5. 
predictions <- predict(model_rf, newdata = testData)

# 6. evaluation
actuals <- testData$device.81
MAE <- mean(abs(predictions - actuals))
MSE <- mean((predictions - actuals)^2)
R2 <- 1 - sum((actuals - predictions)^2) / sum((actuals - mean(actuals))^2)


cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("R-squared (R2):", R2, "\n")
 

#improve onlast model
library(randomForest)
library(caret)


tuneGrid <- expand.grid(.mtry = c(2, sqrt(ncol(trainData)-1), ncol(trainData)-1))
control <- trainControl(method="cv", number=5, search="grid")

model_rf <- train(
  device.81 ~ ., 
  data = trainData, 
  method = "rf", 
  ntree = 500, 
  tuneGrid = tuneGrid, 
  trControl = control
)


print(model_rf$bestTune)


predictions <- predict(model_rf, newdata = testData)


actuals <- testData$device.81
MAE <- mean(abs(predictions - actuals))
MSE <- mean((predictions - actuals)^2)
R2 <- 1 - sum((actuals - predictions)^2) / sum((actuals - mean(actuals))^2)

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("R-squared (R2):", R2, "\n")


#ML- multiple variables using lag feature (not working now)

file_path <- "C:/Users/chloe/Desktop/KF11.csv"
data <- read.csv(file_path)

# 生成滞后特征
data <- data %>%
  mutate(
    Lag_1_device1 = lag(device.70, 1),
    Lag_2_device1 = lag(device.70, 2),
    Lag_3_device1 = lag(device.70, 3),
    Lag_1_device2 = lag(device.81, 1),
    Lag_2_device2 = lag(device.81, 2),
    Lag_3_device2 = lag(device.81, 3),
    Lag_1_device3 = lag(device.84, 1),
    Lag_2_device3 = lag(device.84, 2),
    Lag_3_device3 = lag(device.84, 3),
    Lag_1_device4 = lag(device.91, 1),
    Lag_2_device4 = lag(device.91, 2),
    Lag_3_device4 = lag(device.91, 3),
    Lag_1_device5 = lag(device.105, 1),
    Lag_2_device5 = lag(device.105, 2),
    Lag_3_device5 = lag(device.105, 3)
  ) %>%
  
  na.omit()  # 删除因滞后产生的NA行

set.seed(123)
train_index <- sample(1:nrow(data), 0.8 * nrow(data), replace = FALSE)
trainData <- data[train_index, ]
testData <- data[-train_index, ]

model_rf <- randomForest(device.70 ~ ., data = trainData, ntree = 500)

actuals <- testData$device.70
MAE <- mean(abs(predictions - actuals))
MSE <- mean((predictions - actuals)^2)
R2 <- 1 - sum((actuals - predictions)^2) / sum((actuals - mean(actuals))^2)

# 输出模型的性能评估
cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("R-squared (R2):", R2, "\n")

