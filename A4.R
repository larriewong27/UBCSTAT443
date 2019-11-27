## this is the R script for STAT443 A4

#q1

dtrain <- ts(furnace[1:60,]$furntemp)
dholdout <- ts(furnace[61:80,]$furntemp)

ar1 <- arima(dtrain, c(1, 0, 0), method = "CSS")
ar2 <- arima(dtrain, c(2, 0, 0), method = "CSS")
ar3 <- arima(dtrain, c(3, 0, 0), method = "CSS")

# 2-step predict

pred1 <- predict(ar1, n.ahead = 2)
pred2 <- predict(ar2, n.ahead = 2)
pred3 <- predict(ar3, n.ahead = 2)

# d

##The holdout set out-of-sample forecast rmse's are

p <- 3
est <- arima(dtrain, c(p, 0, 0), method = "CSS")
n <- length(dtrain)
n_hold <- length(dholdout)
y <- c(dtrain[(60 - p + 1):60], dholdout)
y <- y - est$coef[p+1]
mse <- 0

for (i in 1:n_hold) {
  u <- rev(y[i:(i+p-1)])
  fc <- est$coef[p+1] + sum(u*est$coef[1:p])
  zt <- dholdout[i]
  fcerror <- zt- fc
  mse <- mse + fcerror^2
}

rmse <- sqrt(mse/n_hold)
rmse

#q2

arim <- arima.sim(n = 10, list(ar = c(1.23, -0.7)), sd = sqrt(1))

acf_plot <- acf(arim)

plot.ts(arim)

acf <- acf(arim, plot = FALSE)

plot(acf$lag, acf$acf)


