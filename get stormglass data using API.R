library(httr)
library(jsonlite)
library(lubridate)

secret <- readLines("../stormglass-API-key.txt")

# set start time
start <- as.POSIXct("2022-06-10 00:00:00")

# set end date
end <- as.POSIXct("2022-06-30 23:59:59")

# API endpoint and parameters
url <- "https://api.stormglass.io/v2/weather/point"
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

write.csv(weather, file="weather20220610-20220630.csv", row.names = F)

with(weather, plot(airTemperature.noaa ~ time, pch=20, type="o"))
