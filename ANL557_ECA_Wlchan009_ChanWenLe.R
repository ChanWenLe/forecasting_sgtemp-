library(sjPlot)
library(ggplot2)
library(dplyr)
library(forecast)
library(openxlsx)
library(tidyr)

# Reading the Excel file into a data frame
df = read.xlsx("C:\\Users\\Chan Wen Le\\Documents\\SUSS\\ANL557 Applied Forecasting\\ECA\\SG_Enviroment_TemperatureRainfall.xlsx", rows = 10:20, cols = 1:64)

# Convert all columns (except the first one) to numeric
df[,-1] <- lapply(df[,-1], function(x) as.numeric(as.character(x)))

# Replace na as 0
df[is.na(df)] <- 0

# Pivoting the data frame to longer format
df_long = pivot_longer(df, cols = -1, names_to = "year", values_to = "value")

# Pivoting the data frame to wider format
df_wide <- pivot_wider(df_long, names_from = 'Data.Series', values_from = value)

# year is stored as character class
class(df_wide$year)

# Arranging year in ascending order after converting it back to numeric data type
df_wide <- df_wide %>%
  arrange(as.numeric(year))

# Establishing index number to the dataset
df_wide$index <- 1:nrow(df_wide)



# Plotting of trend line
df_wide_trend <-lm(`Air Temperature Means Daily Maximum (Degree Celsius)`~index, data=df_wide)
print(df_wide_trend)
summary(df_wide_trend)

# The values for trend line
df_wide_trend$fitted.values

# Plotting out of trend line
plot(df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`,type ='l',ylab = "Temperature(Degree Celcius)")
abline(a=df_wide_trend$coefficients[1],b=df_wide_trend$coefficients[2],col='red')

# EDA of the data
hist(df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`)
boxplot(df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`)

# Convert the Air Temperature Means Daily Maximum (Degree Celsius) variable to a time series
df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)` = ts(df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`,start = c(1960,1),frequency=1)

#time series has no or less than 2 periods
#df_wide_decompose <- decompose(df_wide_ts, type ='additive')

# Moving Average Decompose

autoplot(df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`, series="RAW") +
  autolayer(ma(df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`,3), series="3-MA") +
  xlab("Year") + ylab("Temp") +
  scale_colour_manual(values=c("RAW"="grey50","3-MA"="red"),
                      breaks=c("RAW","3-MA")) +
  ggtitle("Time Series Plot")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()


# Double ESM
df_wide.desm <- holt(df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`, h=10)
print(df_wide.desm)
df_wide.desm$model

#Model Evaluation
# Compute the Mean Absolute Percentage Error (MAPE)
mean(abs(df_wide.desm$residuals) / df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`) * 100
# Compute the Mean Absolute Deviance (or Error) (MAD)
mean(abs(df_wide.desm$residuals))
# Compute the Mean Square Deviance (or Error) (MSD)
mean(df_wide.desm$residuals ^ 2)

#Alpha (α = 0.2621): This value suggests that the model puts moderate weight on recent observations in adjusting the level. 
#Roughly 26% of the weight is on the current observation, while the rest is on the past level estimates.

#Beta (β = 1e-04):  very small value close to zero suggests that the model is putting very little weight on recent changes in the trend. 
#It is assuming that the trend is relatively stable over time and doesn't adjust much based on recent observations.

# Combining smooth values and forecasted values
df_wide.desm_fc <- ts(c(df_wide.desm$fitted, df_wide.desm$mean), start = c(2000, 1), frequency = 1)


# Plot out DESM with forecasted value
plot(x = 1:63, y = df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`, type = "l", lwd = 2)
lines(x = 1:73, y = df_wide.desm_fc, col = "red", lwd = 3)



# Linear Regression
df_wide.lm <- lm(`Air Temperature Means Daily Maximum (Degree Celsius)` ~ index, data = df_wide)
summary(df_wide.lm)
# P values is 1.35e-10, we reject the slope is 0, there is slope
# Multiple R-squared:  0.494,How good the linear line can fit in the time series plot, around 49% of good fit

# Predicting using linear regression model
df_wide.lm.pred <- predict(df_wide.lm)
print(df_wide.lm.pred)

# Compute the Mean Absolute Deviance (or Error) (MAD)
mean(abs(df_wide.lm$residuals))

# Plot the series and add the predicted values on it
plot(x = df_wide$year, y = df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`, type = "l", xlab = "Year", ylab = "Temperature")
lines(x = df_wide$year, y = df_wide.lm.pred, col = "red")

# Extract the residuals of the linear regression
df_wide.lm.res <- residuals(df_wide.lm)

# Plot the predicted values against the residuals to check the pattern of the residuals
plot(x = df_wide.lm.pred, y = df_wide.lm.res)
# Create a qq-plot for the model residuals
qqnorm(y = df_wide.lm.res)

# Add a 45-degree line to it. If the residuals are lying closely on the line, the residuals
# of the linear regression are most likely normally distributed.
qqline(df_wide.lm.res, col = "red")


# Generate a histogram to have another check on the distribution of the residuals
hist(df_wide.lm.res)

# The Shapiro-Wilk test is most common for it.
shapiro.test(df_wide.lm.res)
# null hypothesis is saying the data are normal,p-value = 0.2093
# means that we cant reject, it is normal distributed


# Check on the independence of the residuals by looking at the ACF
acf(df_wide.lm.res)
# to check the residual is independant or not

# We can run a Durbin Watson test on the residuals to see whether statistically, we can
# support whether the residuals are independent on one another or not.

library(car)
durbinWatsonTest(df_wide.lm)
# rho not equal to 0, means correlation exist
#With a p-value of 0.004, which is less than the common significance level of 0.05, the test result is statistically significant. This means that you can reject the null hypothesis of no autocorrelation in favor of the alternative hypothesis that there is autocorrelation (ρ != 0).
# forecasting using linear regression might not a good way


### linear regression is not significant, but the below part is the forecasted value using lm 
## Predict future values
# To generate future forecast, we need to create a "Data Frame" which contains the
# future values of the INDEPENDENT variables, i.e., the date index and the factor 
# indicating the season to which the observation belongs. The date variable is not
# a must, but it is good to have it already since we may need it for plotting later.
# Firstly, we generate a sequence of future months (12 here)
# Generating a sequence of future dates, from 2023 to 2032, by 1 year increments
newdate <- c(2023,2024,2025,2026,2027,2028,2029,2030,2031,2032)


# Using the length of vector with the new date, generate a sequence of future time index
# by adding 1 to the length of newdate to the last index of the original dataset
newindex <- 1:length(newdate) + tail(df_wide$index, 1)

# Append new dates.
x_val <- c(df_wide$year, newdate)

# Since the number of values on the x-axis must match the number of values on the y-axis,
# we must extend the original series with missing data as well. The number of missing data NA
# corresponds to the number of new dates stored in the variable "newdate". By generating
# multiple NAs using the rep() function, we can then append them to the Close variable using
# the c() function.

y_val <- c(df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`, rep(NA, length(newdate)))
# concatenate all the predicted values from the past with the
# forecasted values for the future in one vector for plotting later.
pred_val <- c(df_wide.lm.pred, df_wide.lm.fc[, 1])

# First, plot the original series with the extended time line. Remember to use x_val and
# y_val for plotting here instead of the original dataset since the number of values is
# now larger.
plot(x = x_val, y = y_val, type = "l", xlab = "Date", ylab = "Temp")

# Then we can overlay the graph with the predicted and forecasted values stored in pred_val.
lines(x = x_val, y = pred_val, col = "red")






# Check the ARIMA orders of the series using the ACF and PACF plot.
df_wide.acf <- acf(df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`, lag.max = 24)
df_wide.pacf <- pacf(df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`, lag.max = 24)


# Apply simple differencing of order 1
df_wide.d1 <- diff(df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`, lag = 1)
#plot(df_wide.d1, type ='l') #to check volatility

# Check the ACF of the differenced series
df_wide.d1.acf <- acf(df_wide.d1, lag.max = 25)
df_wide.d1.pacf <- pacf(df_wide.d1, lag.max = 25)
print(df_wide.d1.acf)
# the dotted line is the test whether the correlation is 0 or nots,
# if it goes out, it means it is diff from 0
# within the interval range, lag 2 is not different from 0
# if the lag line is out by a bit, it is so called marginal significant
# if we adjust the confidence interval, it no longer significant anymore

#try ARIMA(0,1,1)
df_wide.d1.ma1 <- arima(df_wide.d1, order = c(0, 0,1))

print(df_wide.d1.ma1)

# Extract the coefficient estimates
param <- df_wide.d1.ma1$coef

# Extract the covariance matrix of the coefficient estimates
covmat <- df_wide.d1.ma1$var.coef

# The standard errors of the coefficient estimates are square root of the diagonal elements
se <- sqrt(diag(covmat))

# the t-test statistics 
ttest <- abs(param) / se # the abs() function is to convert all values to positive numbers.

# The p-values 
pt(ttest, df = length(df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`) - length(param), lower.tail = FALSE) * 2


# evaluation values like MAD, MAPE, MSD, AIC, etc.
summary(df_wide.d1.ma1)


#try ARIMA(1,1,1)
df_wide.d1.arima11 <- arima(df_wide.d1, order = c(1, 0, 1))
print(df_wide.d1.arima11)

# Extract the coefficient estimates
param <- df_wide.d1.arima11$coef

# Extract the covariance matrix of the coefficient estimates
covmat <- df_wide.d1.arima11$var.coef

# The standard errors 
se <- sqrt(diag(covmat))

# the t-test statistics
ttest <- abs(param) / se # the abs() function is to convert all values to positive numbers.

# The p-values 
pt(ttest, df = length(df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`) - length(param), lower.tail = FALSE) * 2

# the summary() function 
summary(df_wide.d1.arima11)




# Extract the residuals of the model
df_wide.d1.arima11.res <- residuals(df_wide.d1.arima11)
# Model diagnosis 1 - Independence of residuals
acf(df_wide.d1.arima11.res) # If all lags (except lag 0) are insignificant, the model is safe.
# All residual are not correlated to each other

# Model diagnosis 2 - Normality
hist(df_wide.d1.arima11.res)
qqnorm(df_wide.d1.arima11.res)
qqline(df_wide.d1.arima11.res, col = "red") # If residual are normal distributed, it should stick on the red line
shapiro.test(df_wide.d1.arima11.res) # Since it is significant, normality assumption is violated. But it is most likely caused by the two outliers

#null hypothesis is assuming the data is normal, we cannot reject null hypothesis

# Model diagnosis 3 - Zero mean and constant variance
df_wide.d1.arima11.pred <-  df_wide.d1- df_wide.d1.arima11.res
plot(x = df_wide.d1.arima11.pred, y = df_wide.d1.arima11.res) # There are three points lying away from the rest. The others are fluctuating around 0 within a small range.

## Forecast future values ##
# Generate the future forecasts using the predict function.
# The n.ahead parameter controls the number of periods for forecasting.
fc_step <- 10 #28 for (28 periods)
df_wide.d1.arima11.fc <- predict(df_wide.d1.arima11, n.ahead = fc_step)


# Revert the 1-time differencing effect from the forecasted value. First, append the forecasted value 
# to the logged-differenced series with which the ARIMA models were fitted.
fcval <- c(df_wide.d1, df_wide.d1.arima11.fc$pred)
fcval <- ts(fcval, start = c(1961, 2), frequency = 1)

# Then we accumulate the series values since differencing means to subtract the previous value from the 
# current one. To revert the effect, we must compute the accumulated sum of all the previous differenced 
# values until t and then add the value of the first observation to the result. Reminder: append the first
# observation at the beginning (see explanation in the seminar)!
newval <- c(df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`[1], df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`[1] + cumsum(fcval))
newval <- ts(newval, start = c(1960, 1), frequency = 1)

summary(newval)
## Plot the series value and forecasted future values as a red line. ##
plot(newval, type = "l", col = "red",xlab = "Year",  ylab = "Temperature (°C)",  main = "Forecasted Air Temperature Means Daily Maximum (°C)")
# Overlay the original series value as a black line. But we must append the same number of missing values
# to the original series as the forecasting steps. As a result, the original series appears as black time
# series line, proceeded by a red line representing the forecasted values.
lines(df_wide$`Air Temperature Means Daily Maximum (Degree Celsius)`, col = "black", ylim = c(0, 5000))

print(newval)

