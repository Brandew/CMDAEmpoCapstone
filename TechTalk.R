rm(list = ls())
library(datasets)
library(ggplot2)
library(forecast)
library(tseries)


# Read in .csv file and clean up problem entries
tempsOriginal <- read.csv('~/Downloads/daily-minimum-temperatures-in-me.csv', header = T)
tempsOriginal$Date <- as.Date(tempsOriginal$Date)

colnames(tempsOriginal) <- c('Date', 'minTemps')
tempsOriginal = tempsOriginal[1:3650,]
tempsOriginal$minTemps <- as.character(tempsOriginal$minTemps)
#Choosing colors for plot 
tempsOriginal[566,2] = 0.2 
tempsOriginal[567,2] = 0.8
tempsOriginal[1291,2] = 0.1
tempsOriginal$minTemps <- as.numeric(tempsOriginal$minTemps)

View(tempsOriginal)

# Examining the data, step #1
# Visualize plotted model by line graph
dev.off()
ggplot(tempsOriginal, aes(Date, minTemps)) + 
  geom_line(size = 0.3, color = "#0099CC") + 
  scale_x_date('Year')  +
  ylab("Minimum Daily Temperature (Celsius)") + 
  xlab("") + 
  ggtitle("Minimum Temperatures in Australia (1981 - 1990)")



# Create smaller subset of data to use for this demonstration
# 5 years: January 1, 1985 to December 31, 1989
# Visualize via line graph
# Use ts() to create time series of data
# Then use tsclean() function to remove outliers 
# and adding in missing values

temps <- tempsOriginal[1461:3285,]
temps_ts = ts(temps[, c('minTemps')])

ggplot(temps, aes(Date, minTemps)) + 
  geom_line(size = 0.3, color = "#0099CC") + 
  scale_x_date('Year')  + 
  ylab("Minimum Daily Temperature (Celsius)") + 
  xlab("") + 
  ggtitle("Minimum Temperatures in Australia (1985 - 1989)")


# We now find the monthly and weekly moving average 
# and graph with daily values

temps$clean_temps <- tsclean(temps_ts)
temps$temp_ma <- ma(temps$clean_temps, order = 7)
temps$temp_ma30 <- ma(temps$clean_temps, order = 30)


options(warn=-1)

ggplot() + 
  geom_line(data = temps, aes(x=Date, y = clean_temps, color = "Daily Temperature")) + 
  geom_line(data = temps, aes(x=Date, y = temp_ma, color = "Weekly Moving Average"), size = 1) +
  geom_line(data = temps, aes(x=Date, y = temp_ma30, color = "Monthly Moving Average"), size = 1.2) +
  ylab("Temperature (Celsius)") + xlab("Year") + ggtitle("Comparing Moving Averages")

options(warn=0)


# Corresponding to step #2
# Now we can decompose the time series into data,
# seasonal, trend, and remainder.

temps_ma = ts(na.omit(temps$temp_ma), frequency=30)
decomp = stl(temps_ma, s.window="periodic")

# Returns seasonally adjusted data constructed 
# by removing the seasonal component.

deseasonal_temps <- seasadj(decomp)
plot(decomp)


# Corresponding to step #3
# Using adf.test, we can quickly determine if the moving average
# of the time series is stationary.
# This tests the null hypothesis that a unit root is 
# present in a time series sample. This is present in 
# nonseasonal data. 


adf.test(temps_ma, alternative = "stationary")

# Step #4
# Easy function to see the Auto Correlation Function and 
# Partial ACF


Acf(temps_ma, main='')
# ACF plots display correlation between a series and its lags.
# The fact that the lines are well above the blue dotted 
# lines indicate that the current data correlates 
# highly with previous data. 


Pacf(temps_ma, main='')
# Partial ACF plots display correlation between a 
# variable and its lags that is not 
# explained by previous lags.
# There is an oscillation around 0
# We see highly significant spikes at lag 1, 2  and lag 7.
# This suggests using a MA components of order 1,2 or 7.

# Differencing a series involves subtracting 
# its current and previous values, d times. 
# Typically differencing is used to stabilize the series 
# when the stationarity assumption is not met. 



temp_d1 = diff(deseasonal_temps, differences = 1)
plot(temp_d1, main = 'Plot of Differenced Series') 
#difference model
adf.test(temp_d1, alternative = "stationary")

# We see an oscillating pattern around 0 when plotting
# the differenced series. This suggests that differencing
# of order 1 terms is sufficient. 

# Finally Fitting the Model

# Use function auto.arima to fit...  
# But results might be more clear manually


fit <- auto.arima(deseasonal_temps, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')


# Step 6
# Since the PACF graph indicates a spike at around 7,
# we use 7 for our d value... Much better


fit2 = arima(deseasonal_temps, order=c(1,1,7))
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

#Finally, we can get a model forecast of future values

fcast <- forecast(fit2, h=30)
plot(fcast)
summary(fcast)


