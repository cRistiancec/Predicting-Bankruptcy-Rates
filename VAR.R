setwd('/Users/sheringuyen/Desktop/TS/project')

library(forecast)
library(vars)
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

MSE <- function(predictions, observed){
  residuals_squared <- (predictions - observed) ^ 2
  return (sum(residuals_squared)/length(predictions))
}

log.br.train <- log(br.train)
log.br.val <- log(br.val)

##############################################################
########################### VAR ##############################
##############################################################
VARselect(y = data.frame(log.br.train,ur.train, pop.train, hpi.train), type = 'trend')
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#10      8      6     10  

# NEXT ROUND (seems like the smallest values...we want the least parameters!)
VARselect(y = data.frame(log.br.train,ur.train, hpi.train), type = 'trend')
# AIC(n)  HQ(n)  SC(n) FPE(n) 
#6      5      3      6 

VARselect(y = data.frame(log.br.train,pop.train, hpi.train), type = 'trend')
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#10     10      8     10 

VARselect(y = data.frame(log.br.train,pop.train, ur.train), type = 'trend')
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#10     10      8     10 

VARselect(y = data.frame(log.br.train,ur.train), type = 'trend')
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#10      6      5     10

VARselect(y = data.frame(log.br.train,pop.train), type = 'trend')
# AIC(n)  HQ(n)  SC(n) FPE(n) 
#10     10     10     10 

VARselect(y = data.frame(log.br.train,hpi.train), type = 'trend')
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#10      5      5     10 


# try p = 3, 5, 6
m1_3 <- VAR(y = data.frame(log.br.train,ur.train, hpi.train), p = 3, type = 'trend')
pred1_3 <- predict(m1_3, n.ahead = 24, ci = 0.95)
pred1_3.rmse <- sqrt(MSE(exp(pred1_3$fcst$log.br.train[,1]), exp(log.br.val))) 
# 0.004476595

m2_3 <- VAR(y = data.frame(log.br.train,ur.train, hpi.train), p = 5, type = 'trend')
pred2_3 <- predict(m2_3, n.ahead = 24, ci = 0.95)
pred2_3.rmse <- sqrt(MSE(exp(pred2_3$fcst$log.br.train[,1]), exp(log.br.val)))
# 0.005096225

m3_3 <- VAR(y = data.frame(log.br.train,ur.train, hpi.train), p = 6, type = 'trend')
pred3_3 <- predict(m3_3, n.ahead = 24, ci = 0.95)
pred3_3.rmse <- sqrt(MSE(exp(pred3_3$fcst$log.br.train[,1]), exp(log.br.val)))
#0.006566534

# CONCLUSION
# VAR with bankruptcy, unemployment rate, and housing price index with p = 3 is the best model
