# Load the necessary libraries
library(tidyverse)
library(lubridate)
library(fable)
library(tsibble)
library(openair)  # R package specifically designed for the analysis of air quality data

# Install the openair package if not already installed
if (!requireNamespace("openair", quietly = TRUE)) {
  install.packages("openair")
}

# Read the CSV file
aqi <- read.csv("aqi.csv")
str(aqi)

# Convert the date column to Date format
aqi$date <- as.Date(aqi$date)

# Visualize the data using timePlot from the openair package
timePlot(aqi, pollutant = "AQI", avg.time = "month")

# Calculate monthly means for each year
aqi$month <- floor_date(aqi$date, "month")
aqi_mean <- aqi %>% group_by(month) %>% summarize(AQI = mean(AQI, na.rm = TRUE))

# Changing the date format and converting into tsibble
aqi_monthly <- aqi_mean %>% mutate(Date = yearmonth(as.character(month))) %>% as_tsibble(index = Date)

# Fitting the model
aqi_models <- aqi_monthly %>% model(
  ARIMA = ARIMA(AQI),
  ETS = ETS(AQI ~ season(method = "A"))
) %>%
  mutate(AVERAGE = (ARIMA + ETS) / 2)

# Forecast data
forecast_aqi <- aqi_models %>% forecast(bootstrap = TRUE, times = 100, h = 12)  # Forecast for 12 months (1 year)

# Plotting the Forecast data
autoplot(forecast_aqi) + autolayer(aqi_monthly, series = "Forecasts") + theme_minimal()