library(forecast)

data = read.csv("portal_timeseries.csv")
data$date = as.Date(data$date, format = "%m/%d/%Y")
NDVI_ts = ts(data$NDVI, start = c(1992,3),
             end = c(2014,11), frequency = 12)

plot(NDVI_ts)
acf(NDVI_ts) #Correlation function

avg_model = Arima(NDVI_ts, c(0,0,0))
str(avg_model) #Structure

avg_forecast = forecast(avg_model)
str(avg_forecast)
avg_forecast$mean
avg_forecast = forecast(avg_model, 48, level = c(50, 95)) #forecasting for 48 months, level is the prediction interval
avg_forecast$mean

plot(NDVI_ts)
lines(avg_forecast$mean, col = "pink")

plot(avg_forecast)
autoplot(avg_forecast) #Long-term average NDVI, blue color section - information on uncertainly

##Plot is used for create scatter plots, line plots, histograms. It is part of the base graphics system in R.
##Autoplot is used for create time series data, linear models. It is a function from 'ggplot2' package.
##80% of points fall in the dark blue box, 95% of points fall in the light blue box (prediction interval)

arima_model = auto.arima(NDVI_ts, seasonal = FALSE)
arima_forecast = forecast(arima_model)
plot(arima_model)

seasonal_arima_model = auto.arima(NDVI_ts)
seasonal_arima_forecast = forecast(seasonal_arima_model,
                                   h = 36,
                                   level = c(80, 99)) #Predict for 36 months (3 years); Prediction Interval of 80% and 99%
plot(seasonal_arima_forecast)