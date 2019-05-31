                            # CA4 - Predictive Modelling
                                # Sabin K Babu
                            # MSc in Big Data Analytics
                        # Letterkenny Institute of Technology
          

# Importing the required libraries 
# and loading the data to the R frame
library(forecast)
library(tseries)
library(ggplot2)
rainfall_data <- read.csv("monthly_data.csv", header = TRUE, stringsAsFactors = FALSE)
# Examinining the structue of the data
head(rainfall_data,5)
str(rainfall_data)

# Creating a times series of the data using ts() function
# available in package tseries 
# data consists of rainfall amount from 1955 - 2018
# Since it is monthly data the frequency is given as 12
new_data <- ts(rainfall_data$X70.1, frequency = 12, start = c(1955, 1))

# Verifying the start of time series
# end of the series and the frequency
start(new_data)
end(new_data)
frequency(new_data)

# plotting the data and
# and smoothing it to remove significant error components
# using centered moving average or ma() funtion
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
ylim <- c(min(new_data), max(new_data))
plot(new_data, main="Raw time series")
# ma() function used to smooth the Nile time series
plot(ma(new_data, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(new_data, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
plot(ma(new_data, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
par(opar)

# Decomposing the data
# time series analysis consists of seasonality, trend and cycle
# stl () fucntion can be used to calculate the seasonal component
# by default it assumes additive model structure
log_of_new_data <- log(new_data)
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
plot(new_data)
plot(log_of_new_data)
par(opar)

seasonal_decomp <- stl(log_of_new_data, s.window="period")
plot(seasonal_decomp)
seasonal_decomp$time.series

# Examining the seasonal effects suggests that the amount of 
# precipitation fall increased by 36% in July (a multiplier of 1.36) 
# and decreased by 33% in November (with a multiplier of .67).

converted_new_data <- exp(seasonal_decomp$time.series)
converted_new_data

deseasonal_data <- seasadj(seasonal_decomp)
plot(deseasonal_data)

seasonplot(new_data, 12, col = rainbow(12), year.labels = TRUE, main = "Seasonal plot: Precipitation fall")

# seasonal frequency set as 12 for monthly data.
# Removed the seasonal element
seasonplot(deseasonal_data, 12, col = rainbow(12), year.labels = TRUE, main = "Seasonal element removed: Precipiatation fall")

# To check the stationarity of the series
# if the p-value < 0.05 then the data is stationary
# adf.test() and kpss.test can be used to check the stationarity
# the results showing that p-value is lesser than 0.05
# therefor the series is stationary
adf.test(deseasonal_data)
kpss.test(deseasonal_data)

# ACF plots are useful in choosing the order parameters for ARIMA model (MA(q))
# PACF plots helps to determine the order of AR(p)
par(mfrow = c(1, 2))
Acf_result <- Acf(deseasonal_data)
Pacf_result <- Pacf(deseasonal_data)

# eventhough the series is stationary it may contains trend
# checking the trend of the series
ndiffs(deseasonal_data)

# Since there is a trend, the series is differenced 
# once (lag=1 is the default) and saved as diff_data
# Note we had to difference the data by 1
diff_data <- diff(deseasonal_data, lag = 1)

# plotiing both raw data and diff_data
plot(deseasonal_data)
plot(diff_data)

# examining the trend of the data again
# p-value shows that series is stationary
ndiffs(diff_data)
adf.test(diff_data)

# evaluating autocaorrelation and 
# partial ACF of of diff_data
# ACF plot shows a significant correlation at lag 1
# PACF shows significant correlation correlation at lag 1 and lag 11
Acf(diff_data, main = "autocorrelation plot for the differenced data time series")
Pacf(diff_data, main = "autocorrelation plot for the differenced data time series")

par(opar)
# A manual model is created from the details obtained
# from the ACF and PACF plots 
# from the plots (p,q) can be examined as (0,1), (1,0) and (1,1)
# the series shows one differenecing to become stationary so, d= 1 
# so for orginal time series we can propose 3 ARIMA models
# ARIMA (0,1,1), ARIMA (1,1,0) and ARIMA (1,1,1)

arima_model1 <- Arima(deseasonal_data, order = c(0, 1, 1))
arima_model1
arima_model2 <- Arima(deseasonal_data, order = c(1, 1, 0))
arima_model2
arima_model3 <- Arima(deseasonal_data, order = c(1, 1, 1))
arima_model3

# Accuracy measures
# The mean absolute percentage error (MAPE)
# measures prediction of accuracy
# so this is the forecast accuracy of the error
accuracy(arima_model1)
accuracy(arima_model2)
accuracy(arima_model3)

# model fit can be examined using Q-Q plot 
# QQ plot is produced using a qqnorm() function
# and qqline build a normal line that fits the data.
# ans is clear arima_model2 fits the data than other two models
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,3))

qqnorm(arima_model1$residuals)
qqline(arima_model1$residuals)

qqnorm(arima_model2$residuals)
qqline(arima_model2$residuals)

qqnorm(arima_model3$residuals)
qqline(arima_model3$residuals)

par(opar)
# Box.test() function provides a test that autocorrelations 
# are all zero (H0). The results are significant, suggesting 
# the autocorrelations don ’t differ from zero.
# This ARIMA model appears to fit the data well.
Box.test(arima_model1$residuals, type = "Ljung-Box")
Box.test(arima_model2$residuals, type = "Ljung-Box")
Box.test(arima_model3$residuals, type = "Ljung-Box")
# Forecast 5 years ahead for rainfall time series
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,3))
forecast(arima_model1, 5)
# Plot function shows the forecast. Point estimates are
# given by the blue dots, 80 % and 95 % confidence bands 
# are represented by dark and light bands, respectively
plot(forecast(arima_model1, 5), xlab = "Year", ylab = "monthly fall")

forecast(arima_model2, 5)
plot(forecast(arima_model2, 5), xlab = "Year", ylab = "monthly fall")

forecast(arima_model3, 5)
plot(forecast(arima_model3, 5), xlab = "Year", ylab = "monthly fall")

par(opar)
# ARIMA forecasting (automated)
# Comparing the automated ARIMA test against the manual method
library(forecast)
auto_arima_model <- auto.arima(deseasonal_data)
auto_arima_model
accuracy(auto_arima_model)
# Auto ARIMA model returns a model similair to arima_model2 
# Compare with manual selected model
accuracy((arima_model2))
qqnorm(auto_arima_model$residuals)
qqline(auto_arima_model$residuals)
# We apply the Box-Ljung test to the residuals from the ARIMA(1,1,0) 
# model fit to determine whether residuals are random or not. In this job, for the suggested ARIMA model and auto ARIMA 
# the Box-Ljung test shows that autocorrelations among the residuals are zero (p-value = 0.00003), 
# indicating that the residuals are random and that the model provides an adequate fit to the data.
Box.test(auto_arima_model$residuals, type = "Ljung-Box")
Box.test(arima_model2$residuals, type = "Ljung-Box")
forecast(auto_arima_model)
plot(forecast(auto_arima_model, 5), xlab = "Year", ylab = "Annual Flow")

#----------------------------------------------------------------------------------------------

