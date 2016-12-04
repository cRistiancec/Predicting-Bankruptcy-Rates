setwd('/Users/sheringuyen/Desktop/TS/project')

library(forecast)
library(lmtest)
library(lawstat)
train <- read.csv('train.csv', header = T)

train.set <- train[1:264, 1:ncol(train)]
val.set <- train[265:nrow(train), 1:ncol(train)]

# br := bankruptcy rate
# ur := unemployment rate
# pop := population
# hpi := house price index

# TRAINING
br.train <- ts(train.set[,4], frequency = 12, start = c(1987, 1))
ur.train <- ts(train.set[,2], frequency = 12 , start = c(1987, 1))
pop.train <- ts(train.set[,3], frequency = 12, start = c(1987, 1))
hpi.train <- ts(train.set[,5], frequency = 12, start = c(1987, 1))

# VALIDATION
br.val <- ts(val.set[,4], frequency = 12, start = c(2009, 1))
ur.val <- ts(val.set[,2], frequency = 12 , start = c(2009, 1))
pop.val <- ts(val.set[,3], frequency = 12, start = c(2009, 1))
hpi.val <- ts(val.set[,5], frequency = 12, start = c(2009, 1))



##############################################################
##################### PRELIMINARY ############################
##############################################################
# Show relative possible trends and seasonality per Time Series
par(mfrow = c(4,1))
plot(log(br.train)) # plot: linear trend, no seasonality
ndiffs(log(br.train)) # 1 
nsdiffs(log(br.train)) # 0

plot(ur.train, col = 'red') # plot: no trend, no seasonality
plot(pop.train, col = 'green') # strong linear trend
plot(hpi.train, col = 'blue') # linnear trend, no seasonality

# Return all residual plot diagnostics
residual_plots <- function(model){
  par(mfrow = c(3,1))
  plot(model$residuals)
  acf(model$residuals, lag.max = 48)
  pacf(model$residuals, lag.max = 48)
}

residual_tests <- function(model){
  # No correlation
  tsdiag(model)
  
  # Normality
  #qqnorm(model$residuals)
  #qqline(model$residuals)
  shapirotest <- shapiro.test(model$residuals)
  print (shapirotest)
  
  # Zero-mean
  ttest <- t.test(model$residuals)
  print (ttest)
  
  # Homoskedasticity
  group = c(rep(1, 66), rep(2, 66), rep(3, 66), rep(4, 66))
  ltest <- levene.test(model$residuals, group)
  print (ltest)
}

MSE <- function(predictions, observed){
  residuals_squared <- (predictions - observed) ^ 2
  return (sum(residuals_squared)/length(predictions))
}

log.br.train <- log(br.train)
log.br.val <- log(br.val)
par(mfrow = c(3,1))
plot(diff(log.br.train))
acf(diff(log.br.train), lag.max = 48)
pacf(diff(log.br.train), lag.max = 48)

# q = 1,2,3, MAYBE 4
# Q = 1, maybe 2/3
# p = 1,2,4
# P = 1

##############################################################
######################## ARIMAX ##############################
##############################################################

auto.arima(log.br.train, xreg = data.frame(ur.train, pop.train, hpi.train))
# ARIMA(4,0,3)(2,0,0)[12]

############################## MODELS WITH ALL THREE VARIABLES #######################

m1 <- arima(log.br.train, order = c(4,0,3), seasonal = list(order = c(2,0,0), period = 12), xreg = data.frame(ur.train, pop.train, hpi.train))
residual_plots(m1)
tsdiag(m1) # fails
residual_tests(m1) # pass

# NEXT ROUND
m2 <- arima(log.br.train, order = c(5, 1, 2), seasonal = list(order = c(1,0,0), period = 12), xreg = data.frame(ur.train, pop.train, hpi.train))
tsdiag(m2) # pass
residual_tests(m2) # pass

# NEXT ROUND
m3 <- arima(log.br.train, order = c(4,1,3), seasonal = list(order = c(1,0,0), period = 12), xreg = data.frame(ur.train, pop.train, hpi.train))
tsdiag(m3) # pass
residual_tests(m3) # pass

# NEXT ROUND
m4 <- arima(log.br.train, order = c(4,1,3), seasonal = list(order = c(2,0,0), period = 12), xreg = data.frame(ur.train, pop.train, hpi.train))
tsdiag(m4) # pass
residual_tests(m4) # pass

m5 <- arima(log.br.train, order = c(4,1,2), seasonal = list(order = c(2,0,0), period = 12), xreg = data.frame(ur.train, pop.train, hpi.train))
tsdiag(m5) # fails
residual_tests(m5) # pass

sigma2 <- c(m2$sigma2, m3$sigma2, m4$sigma2)
loglik <- c(m2$loglik, m3$loglik, m4$loglik)
AIC <- c(m2$aic, m3$aic, m4$aic)
pdq_PDQ <- c('(5,1,2) x (1,0,0)', '(4,1,3) x (1,0,0)', '(4,1,3) x (2,0,0)')
criterion.arimax_3vars<- data.frame(pdq_PDQ, sigma2, loglik, AIC)
# Model 3 'wins'

arimax_3var_fcast <- forecast(m3, h = 24, level = 95, xreg = data.frame(ur.val, pop.val, hpi.val))

arimax_rmse <- sqrt(MSE(exp(arimax_3var_fcast$mean), exp(log.br.val))) #0.0117701

######################################## 2 Variables ########################################

auto.arima(log.br.train, xreg = data.frame(ur.train, pop.train)) 
# ARIMA(2,1,2)(1,0,0)[12]
# NaNs produced for pop.train. Using pop.train along with ur.train not necessary

auto.arima(log.br.train, xreg = data.frame(ur.train, hpi.train)) 
# ARIMA(1,1,3)(1,0,0)[12], with drift?
# 0 contained in ur.train confidence interval... ur.train with hpi.train not neccessary

auto.arima(log.br.train, xreg = data.frame(pop.train, hpi.train)) # ARIMA(2,1,2)(1,0,0)[12], NaN produced (pop.train)
# ARIMA(2,1,2)(1,0,0)[12]
# NaN for pop.train no need for pop.train

# conclusions: 
# for single exog model : ur.train
# for single exog model : hpi.train
# pop.train provides no valuable input when used alongside each of the other two variables
# no valuable exploration in 2 exog variable models

######################################## 1 Variable ########################################
auto.arima(log.br.train, xreg = data.frame(ur.train))
# ARIMA(3,1,1)(1,0,0)[12]

# JUST UR.TRAIN
m1_1 <- arima(log.br.train, xreg = data.frame(ur.train), order = c(3,1,1), seasonal = list(order = c(1,0,0), period = 12))
residual_tests(m1_1) # passes all but autocorrelation
residual_plots(m1_1) 

# NEXT ROUND
m2_1 <- arima(log.br.train, xreg = data.frame(ur.train), order = c(4,1,1), seasonal = list(order = c(1,0,0), period = 12))
residual_tests(m2_1) # pass
residual_plots(m2_1) # pass

# NEXT ROUND
m3_1 <- arima(log.br.train, xreg = data.frame(ur.train), order = c(5,1,1), seasonal = list(order = c(1,0,0), period = 12))
residual_tests(m3_1) # pass
residual_plots(m3_1) # pass

m4_1 <- arima(log.br.train, xreg = data.frame(ur.train), order = c(4,1,2), seasonal = list(order = c(1,0,0), period = 12))
residual_tests(m4_1) # fails autocorr
residual_plots(m4_1) # pass

m5_1 <- arima(log.br.train, xreg = data.frame(ur.train), order = c(4,1,3), seasonal = list(order = c(1,0,0), period = 12))
residual_tests(m5_1) # fails autocorr
residual_plots(m5_1) # pass

# NEXT ROUND
m6_1 <- arima(log.br.train, xreg = data.frame(ur.train), order = c(5,1,2), seasonal = list(order = c(1,0,0), period = 12))
residual_tests(m6_1) # pass
residual_plots(m6_1) # pass

sigma2_1a <- c(m2_1$sigma2, m3_1$sigma2, m6_1$sigma2)
loglik_1a <- c(m2_1$loglik, m3_1$loglik, m6_1$loglik)
AIC_1a <- c(m2_1$aic, m3_1$aic, m6_1$aic)
pdq_PDQ_1a <- c('(4,1,1) x (1,0,0)', '(5,1,1) x (1,0,0)', '(5,1,2) x (1,0,0)')
criterion.arimax_1a<- data.frame(pdq_PDQ_1a, sigma2_1a, loglik_1a, AIC_1a)
# Model 6 'wins'

arimax_fcast1a <- forecast(m6_1, h = 24, level = 95, xreg = data.frame(ur.val))

arimax_rmse_1a <- sqrt(MSE(exp(arimax_fcast1a$mean), exp(log.br.val))) #0.009108259

# JUST HPI.TRAIN
auto.arima(log.br.train, xreg = data.frame(hpi.train))
# ARIMA(1,1,3)(1,0,0)[12] with drift <- what the heck is this

m1_2 <- arima(log.br.train, xreg = data.frame(hpi.train), order = c(1,1,3), seasonal = list(order = c(1,0,0), period = 12))
residual_plots(m1_2)
residual_tests(m1_2) # fails autocorr

# NEXT ROUND
m2_2 <- arima(log.br.train, xreg = data.frame(hpi.train), order = c(5,1,2), seasonal = list(order = c(1,0,0), period = 12))
residual_plots(m2_2)
residual_tests(m2_2) # pass

m3_2 <- arima(log.br.train, xreg = data.frame(hpi.train), order = c(4,1,1), seasonal = list(order = c(1,0,0), period = 12))
residual_plots(m3_2)
residual_tests(m3_2) # too close to call -- ... fail

# NEXT ROUND
m4_2 <- arima(log.br.train, xreg = data.frame(hpi.train), order = c(5,1,1), seasonal = list(order = c(1,0,0), period = 12))
residual_plots(m4_2)
residual_tests(m4_2) # pass

sigma2_1b <- c(m2_2$sigma2, m4_2$sigma2)
loglik_1b <- c(m2_2$loglik, m4_2$loglik)
AIC_1b <- c(m2_2$aic, m4_2$aic)
pdq_PDQ_1b <- c('(5,1,2) x (1,0,0)', '(5,1,1) x (1,0,0)')
criterion.arimax_1b<- data.frame(pdq_PDQ_1b, sigma2_1b, loglik_1b, AIC_1b)

# Model 2_2 wins .. but is it significanntly better than 4_2? 
D <- -2*(m4_2$loglik - m2_2$loglik)
pval <- 1-pchisq(D,1) # 0.006341492
# Reject null that null model and alternative model is equally good
# Select alternative

# Conclusion: Model 2_2 wins
arimax_fcast1b <- forecast(m2_2, h = 24, level = 95, xreg = data.frame(hpi.val))

arimax_rmse_1b <- sqrt(MSE(exp(arimax_fcast1b$mean), exp(log.br.val))) # 0.009679471

################################ FINAL ARIMAX CONCLUSIONS ###############################
# Best ARIMAX model is with single exogenous variable ur.train
#m6_1 <- arima(log.br.train, xreg = data.frame(ur.train), order = c(5,1,2), seasonal = list(order = c(1,0,0), period = 12))
#residual_tests(m6_1) # pass
#residual_plots(m6_1)
