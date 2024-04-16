library(httr)
library(jsonlite)
library(lubridate)

secret <- readLines("../stormglass-API-key.txt")

# set start time
start <- as.POSIXct("2022-03-01 00:00:00")

# set end date
end <- as.POSIXct("2022-03-10 23:59:59")

# API endpoint and parameters
url <- "https://api.stormglass.io/v2/weather/point"
#      # XinXiang 35.3, 113.927
#      # ZhouKou  33.589717, 114.656714
#      # KaiFeng  34.732160, 114.400189
params <- list(
  lat = 35.288,
  lng = 113.894,
  params = paste(c("pressure", "precipitation", "airTemperature",
                   "humidity", "windDirection","windSpeed","gust", "cloudCover"),
                 collapse = ","),
  start = as.numeric(start),
  end = as.numeric(end)
)

# API headers
headers <- c("Authorization" = secret)

# Make the API request
response <- GET(url, query = params, add_headers(.headers=headers))

# Convert response to JSON
json_data <- fromJSON(content(response, "text"))

# Do something with response data.
# (Note: You may need to adjust the data processing steps based on the structure of the JSON response)
print(json_data)

weather <- as.data.frame(do.call(cbind, json_data$hours))
weather$time <- as.POSIXct(gsub("T"," ",weather$time), timezone="UTC")

write.csv(weather, file="XinXiang-sg-20220301-20220310.csv", row.names = F)

with(weather, plot(airTemperature.noaa ~ time, pch=20, type="o", col=4))
