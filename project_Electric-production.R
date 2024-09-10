library(forecast)

# Set working directory for locating files.
setwd("C:/Users/azmat/Downloads")

# Create data frame.
Electric_p.data <- read.csv("Electric_Production.csv")
Electric_p.data

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
Electric_p.ts <- ts(Electric_p.data$IPG2211A2N, 
                      start = c(1985, 1), end = c(2017, 12), freq = 12)

## Use plot() to plot time series data 
plot(Electric_p.ts, 
     xlab = "Time", ylab = "Electric (in kw)", 
     ylim = c(10, 200), xaxt = 'n',
     main = "Electricity production")

# Establish x-axis scale interval for time in months.
axis(1, at = seq(1985, 2017, 1), labels = format(seq(1985, 2017, 1)))

# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
Electric_p.stl <- stl(Electric_p.ts, s.window = "periodic")
autoplot(Electric_p.stl, main = "Electricity Production")

## TEST PREDICTABILITY OF Electricity Production.

# Use Arima() function to fit AR(1) model 
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
Electric_p.ar1<- Arima(Electric_p.ts, order = c(1,0,0))
summary(Electric_p.ar1)

# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.8734
s.e. <- 0.0245
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}


# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags.
autocor <- Acf(Electric_p.ts, lag.max = 12, 
               main = "Autocorrelation for Electricity Production")

# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
# Total number of period length(Electric_p.ts) = 96.
# nvalid = 96 months (8 years), from January 2010 to December 2017.
# nTrain = 300 months (25 years), from January 1985 to December 2009.

nValid <- 96 
nTrain <- length(Electric_p.ts) - nValid
train.ts <- window(Electric_p.ts, start = c(1985, 1), end = c(1985, nTrain))
valid.ts <- window(Electric_p.ts, start = c(1985, nTrain + 1), 
                   end = c(1985, nTrain + nValid))

# FIT REGRESSION MODEL WITH (1) LINEAR TREND (2) QUADRATIC (POLYNOMIAL) TREND, 
## (3) SEASONALITY, (4) LINEAR TREND AND SEASONALITY, AND
## (5) QUADRATIC TREND AND SEASONALITY.
## IDENTIFY FORECAST FOR VALIDATION PERIOD FOR EACH MODEL.

## (1) LINEAR TREND MODEL.
# Use tslm() function to create linear trend model.
train.lin <- tslm(train.ts ~ trend)

# See summary of quadratic trend model and associated parameters.
summary(train.lin)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
train.lin.pred

## (2) QUADRATIC TREND MODEL.
# Use tslm() function to create quadratic (polynomial) trend model.
train.quad <- tslm(train.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and associated parameters.
summary(train.quad)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
train.quad.pred

## (3) SEASONALITY MODEL.
# Use tslm() function to create seasonal model.
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)

# Apply forecast() function to make predictions for ts with 
# seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred

## (4) LINEAR TREND AND SEASONALITY MODEL.
# Use tslm() function to create linear trend and seasonal model.
train.lin.trend.season <- tslm(train.ts ~ trend  + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.lin.trend.season.pred <- 
  forecast(train.lin.trend.season, h = nValid, level = 0)
train.lin.trend.season.pred


## (5) QUADRATIC TREND AND SEASONALITY MODEL.
# Use tslm() function to create quadratic trend and seasonal model.
train.quad.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.trend.season.pred <- 
  forecast(train.quad.trend.season, h = nValid, level = 0)
train.quad.trend.season.pred

# Use accuracy() function to identify common accuracy measures
# for the developed forecast in the validation period.
round(accuracy(train.lin.pred$mean, valid.ts),3)
round(accuracy(train.quad.pred$mean, valid.ts),3)
round(accuracy(train.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.trend.season.pred$mean, valid.ts),3)
round(accuracy(train.quad.trend.season.pred$mean, valid.ts),3)

# Use tslm() function to create regression model with linear trend 
# and seasonality for entire data set
lin.season <- tslm(Electric_p.ts ~ trend + season)

# See summary of linear trend and seasonality equation and
# associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in 24 future periods.
lin.season.pred <- forecast(lin.season, h = 24, level = 0)
lin.season.pred

# Use tslm() function to create linear trend model.
lin.trend <- tslm(Electric_p.ts ~ trend)

# Apply forecast() function to make predictions for ts with 
# linear trend  data in 24 future periods.
lin.trend.pred <- forecast(lin.trend, h = 24, level = 0)

# Use tslm() function to create regression model with quadratic trend 
# and seasonality for entire data set
quad.season <- tslm(Electric_p.ts ~ trend + I(trend^2) + season)

# See summary of linear trend and seasonality equation and associated parameters.
summary(quad.season)

# Apply forecast() function to make predictions for ts with 
# quadratic trend and seasonality data in 24 future periods.
quad.season.pred <- forecast(quad.season, h = 24, level = 0)
quad.season.pred

## COMPARE ACCURACY MEASURES OF REGRESSION FORECAST WITH LINEAR TREND AND
# SEASONALITY, LINEAR TREND AND QUADRATIC TREND SEASONALITY FOR THE
# ENTIRE DATA SET WITH ACCURACY MEASURES OF NAIVE FORECAST AND 
#SEASONAL NAIVE FORECAST FOR ENTIRE DATA SET.

# Use accuracy() function to identify common accuracy measures
round(accuracy(lin.season.pred$fitted, Electric_p.ts),3)
round(accuracy(lin.trend.pred$fitted, Electric_p.ts),3)
round(accuracy(quad.season.pred$fitted, Electric_p.ts),3)
round(accuracy((naive(Electric_p.ts))$fitted, Electric_p.ts), 3)
round(accuracy((snaive(Electric_p.ts))$fitted, Electric_p.ts), 3)

## TEST PREDICTABILITY OF Electricity Production.

# Use Arima() function to fit AR(1) model 
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
Electric_p.ar1<- Arima(Electric_p.ts, order = c(1,0,0))
summary(Electric_p.ar1)

# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.8734
s.e. <- 0.0245
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

## FIT AUTO ARIMA MODEL.

# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals
    of Training Data")

plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "Electric Production (in kw)", 
     ylim = c(10,300 ), xaxt = "n", 
     bty = "l", xlim = c(1985, 2022), 
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(1985, 2022, 1), labels = format(seq(1985, 2022, 1)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(1985,300, legend = c("Electric Production Time Series", 
                            "Auto ARIMA Forecast for Training Period",
                            "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2010, 2010), c(0, 200))
lines(c(2018, 2018), c(0, 200))
text(1994,200, "Training")
text(2012, 200, "Validation")
text(2020, 200, "Future")
arrows(1985, 180, 2010, 180, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2010, 180, 2018, 180, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2018, 180, 2020.3, 180, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
auto.arima <- auto.arima(Electric_p.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 24 periods. 
auto.arima.pred <- forecast(auto.arima, h = 24, level = 0)
auto.arima.pred

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals
    for entire dataset")

# Plot historical data, predictions for historical data, and seasonal 
# ARIMA forecast for 24 future periods.
plot(Electric_p.ts, 
     xlab = "Time", ylab = "Electric production (in Kw)", 
     ylim = c(10, 300), xaxt = "n", 
     bty = "l", xlim = c(1985, 2022), 
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(1985, 2022, 1), labels = format(seq(1985, 2022, 1)))
lines(auto.arima$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(1985,300, legend = c("Electric Production Time Series", 
                            "Auto ARIMA Forecast for Entire Dataset",
                            "Auto ARIMA Forecast for Future 24 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
lines(c(2018, 2018), c(0, 200))
text(1994,200, "data set")
text(2018, 200, "Future")
arrows(1985, 180, 2018, 180, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2018, 180, 2020, 180, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.

# Use accuracy() function to identify common accuracy measures for:

# (1) Auto ARIMA Model,
# (2) Seasonal naive forecast, and
# (3) Naive forecast.

round(accuracy(auto.arima.pred$fitted, Electric_p.ts), 3)
round(accuracy((snaive(Electric_p.ts))$fitted, Electric_p.ts), 3)
round(accuracy((naive(Electric_p.ts))$fitted, Electric_p.ts), 3)


