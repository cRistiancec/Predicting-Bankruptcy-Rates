setwd('/Users/sheringuyen/Desktop/TS/project')

library(forecast)
library(lmtest)
library(lawstat)
train <- read.csv('train.csv', header = T)

# Create a Train + Validation set from train data
# Train: Jan 1987 - Dec 2008
# Validation: Jan 2009 - Dec 2010
train.set <- train[1:264, 1:ncol(train)]
val.set <- train[265:nrow(train), 1:ncol(train)]

# Convert to Time Series objects
train.bank.rate <- ts(train.set[,4], frequency = 12, start = c(1987, 1))
val.bank.rate <- ts(val.set[,4], frequency = 12, start = c(2009, 1))

residual_plots <- function(model){
  par(mfrow = c(3,1))
  plot(model$residuals)
  acf(model$residuals, lag.max = 48)
  pacf(model$residuals, lag.max = 48)
}

MSE <- function(predictions, observed){
  residuals_squared <- (predictions - observed) ^ 2
  return (sum(residuals_squared)/length(predictions))
}

# Preliminary trends 
par(mfrow = c(2,1))
plot(train.bank.rate) # linear trend, no seasonality
plot(log(train.bank.rate))

ndiffs(train.bank.rate) # 1
nsdiffs(train.bank.rate) # 0

ndiffs(log(train.bank.rate)) # 1 
nsdiffs(log(train.bank.rate)) # 0 

par(mfrow = c(3,1))
diff.train.BR <- diff(train.bank.rate)
plot(diff.train.BR)
acf(diff.train.BR, lag.max = 48)
pacf(diff.train.BR, lag.max = 48)

# Residuals for log are less heteroskedastic, will use log of BR
diff.log.train.BR <- diff(log(train.bank.rate))
plot(diff.log.train.BR)
acf(diff.log.train.BR, lag.max = 48)
pacf(diff.log.train.BR, lag.max = 48)

# q = 1,2,3, MAYBE 4
# Q = 1, maybe 2/3
# p = 1,2,4
# P = 1

##############################################################
########################## ARIMA #############################
##############################################################

log.train.BR <- log(train.bank.rate)
log.val.BR <- log(val.bank.rate)

auto.arima(log.train.BR)
# ARIMA(3,1,2)(1,0,0)[12]   
# No suggested seasonality, so SARIMA may not be necessary
m1 <- arima(log.train.BR, order = c(3, 1, 2), seasonal = list(order = c(1,0,0), period = 12))
residual_plots(m1)
tsdiag(m1) # correlated

m2 <- arima(log.train.BR, order = c(3, 1, 2))
residual_plots(m2) # definitely need P = 1
tsdiag(m2) # correlated

m3 <- arima(log.train.BR, order = c(4, 1, 2), seasonal = list(order = c(1,0,0), period = 12))
residual_plots(m3)
tsdiag(m3) # correlated residuals

m4 <- arima(log.train.BR, order = c(5, 1, 2), seasonal = list(order = c(1,0,0), period = 12))
residual_plots(m4)
tsdiag(m4)

m5 <- arima(log.train.BR, order = c(5, 1, 1), seasonal = list(order = c(1,0,0), period = 12))
residual_plots(m5)
tsdiag(m5) # uncorrelated

m6 <- arima(log.train.BR, order = c(4, 1, 1), seasonal = list(order = c(1,0,0), period = 12))
residual_plots(m6)
tsdiag(m6) # correlated


# Final models: m4 and m5
summary(m4) # alternative
summary(m5) # null

# Results from m4 are slightly better, but is it worth adding another q?

D <- -2*(m5$loglik - m4$loglik)
pval <- 1-pchisq(D,1)
# Reject null that null model and alternative model is equally good
# Select alternative

# Final model: m4

# Residual Diagnostic Tests:

# No correlated residuals : pass
tsdiag(m4)

# Normality : pass
par(mfrow = c(1,1))
qqnorm(m4$residuals)
qqline(m4$residuals)
shapiro.test(m4$residuals) 

# Zero-mean : pass
t.test(m4$residuals)

# Homoskedasticity : pass
group = c(rep(1, 66), rep(2, 66), rep(3, 66), rep(4, 66))
levene.test(m4$residuals, group)

predictions <- forecast(m5, h = 24, level = 95)
log_arima_rmse <- sqrt(MSE(predictions$mean, log.val.BR))
# 0.2380469

arima_rmse <- sqrt(MSE(exp(predictions$mean), exp(log.val.BR)))
# 0.009123579

plot(m4)
plot(predictions)

##############################################################
#################### HoltWinters #############################
##############################################################

hw1 <- HoltWinters(log.train.BR, gamma = NULL, seasonal = 'additive')
hw_fcast1 <- forecast(hw1, h = 24, level = 95)
plot(h1)
plot(hw_fcast1)

hw2 <- HoltWinters(log.train.BR, gamma = NULL, seasonal = 'multiplicative')
hw_fcast2 <- forecast(hw2, h = 24, level = 95)
plot(hw2)
plot(hw_fcast2)

log_hw_rmse1 <- sqrt(MSE(hw_fcast1$mean, log.val.BR)) # 0.3806596
hw_rmse1 <- sqrt(MSE(exp(hw_fcast1$mean), exp(log.val.BR))) # 0.01655722

log_hw_rmse2 <- sqrt(MSE(hw_fcast2$mean, log.val.BR)) # 0.3609931
hw_rmse2 <- sqrt(MSE(exp(hw_fcast2$mean), exp(log.val.BR))) # 0.01540406
