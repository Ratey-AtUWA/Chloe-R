library(httr)
library(jsonlite)
library(lubridate)

url <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/"

secret <- readLines("../visual-crossing-API-key.txt")

# see https://www.visualcrossing.com/resources/documentation/weather-api/timeline-weather-api/
# for full list of options

# set start time
start <- as.POSIXct("2022-03-01 00:00:00 CST")
# set end date
end <- as.POSIXct("2022-03-10 23:59:59 CST")

# XinXiang 35.3, 113.927
# ZhouKou  33.589717, 114.656714
# KaiFeng  34.732160, 114.400189

lat = 35.3
long = 115.803
# request parameters
params <- paste(c("datetimeEpoch","tzoffset","temp",
                  "humidity","precip","preciptype","pressure",
                  "visibility","winddir","windgust","windspeed",
                  "stations"),
                 collapse = ",")

# Make the API request
response <- GET(paste0(url,
        lat,",",
        long,"/",
        as.numeric(start),"/",
        as.numeric(end),
        "?key=",secret,
        "&options=csv",
        "&include=obs%2Chours%2Calerts",
        "&elements=",params))

# Convert response to JSON
json_data <- fromJSON(content(response, as="text", encoding="UTF-8"))

# Process and save response data.

weather <- as.data.frame(json_data$days$hours[[1]])
for(i in 2:length(json_data$days$hours)){
  weather <- rbind(weather, as.data.frame(json_data$days$hours[[i]]))
  }
weather$datetime <- as.POSIXct(weather$datetimeEpoch,
                               origin=as.POSIXct("1970-01-01 08:00:00"))
weather$tempC <- (5/9)*(weather$temp - 32)
weather$precipmm <- weather$precip * 25.4
weather$windgustkmh <- weather$windgust * 1.609344
weather$windspeedkmh <- weather$windspeed * 1.609344
weather$visibilitykm <- weather$visibility * 1.609344
stationsUsed <- weather$stations
weather$stations <- NULL

write.csv(weather, file="XinXiang-vc-20220301-20220310.csv", row.names = F)

with(weather, plot(tempC ~ datetime, pch=20, type="o", col=2))
