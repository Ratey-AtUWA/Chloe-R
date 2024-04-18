#seasonal boxplot
file_path <- "C:/Users/chloe/Desktop/XXtotal.csv"
data <- read.csv(file_path)
#  
data$time <- as.Date(data$time, format="%Y/%m/%d")
data$month <- format(data$time, "%Y-%m")

# Converts the device's reading column to numeric format
data[2:(ncol(data)-1)] <- sapply(data[2:(ncol(data)-1)], as.numeric)

# 
data$daily_mean <- rowMeans(data[2:(ncol(data)-2)], na.rm = TRUE)

# 
ggplot(data, aes(x=month, y=daily_mean)) +
  geom_boxplot() +
  xlab("Month") +
  ylab("Average Value") +
  ggtitle("Data represents average daily readings across Kaifeng") +
  theme_minimal()


#correlation voc/temp/humid

library(readxl)
library(dplyr)
data <- read_excel("F:/Honour Dissertation/Raw Data/2023KF/2023 KF.xlsx")
data$time <- as.Date(data$time, format="%Y-%m-%d %H:%M:%S")
# Filter the data for January 2023
may_data <- data %>% 
  filter(time >= as.Date("2023-05-01") & time < as.Date("2023-05-05"))
# Perform correlation tests
cor_test_voc_temp <- cor.test(may_data$VOCvalue,may_data$temp)
cor_test_voc_humid <- cor.test(may_data$VOCvalue, may_data$humid)
cor_test_temp_humid <- cor.test(may_data$temp, may_data$humid)
list(
  voc_temp = cor_test_voc_temp,
  voc_humid = cor_test_voc_humid,
  temp_humid = cor_test_temp_humid)
plot(may_data$humid, may_data$VOCvalue)
plot(may_data$temp, may_data$VOCvalue)

#corre matric(need to do spitial corre)

library(readxl)
install.packages("corrplot")
library(corrplot)
data <- read.csv("/mnt/data/KFtotal.csv")

data <- data.frame(lapply(data, function(x) as.numeric(as.character(x))))
#or
data <- data.frame(lapply(data, function(x) {
  if (is.numeric(x)) {

    x[is.na(x)] <- mean(x, na.rm = TRUE)
  } else {
    # 
    freq <- table(x)
    if (length(freq) == 0) {
    
      x[is.na(x)] <- "Unknown"  # 选择一个合适的默认值
    } else {
      # 使用众数填充
      mode_value <- names(which.max(freq))
      x[is.na(x)] <- mode_value
    }
  }
  return(x)
})) 
cor_matrix <- cor(data, use = "pairwise.complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", 
         number.cex = 0.3, 
         tl.cex = 0.3, 
         diag = FALSE) 


