## This is the R script for STAT443 Lab7

# Part a)


# orginal acf and pacf for fxrate
acf(eurofx$fxrate)
pacf(eurofx$fxrate)

# plot diff
dfx <- ts(diff(eurofx$fxrate))
acf(dfx)
pacf(dfx)

# This is for Diff

# AR(2)
ar2 <- arima(dfx, order = c(2, 0, 0), method = "CSS")
ar2

# in-sample RMSE

sqrt(ar2$sigma2)

# MA(2)

ma2 <- arima(dfx, order = c(0, 0, 2), method = "CSS")
ma2

# in-sample RMSE

sqrt(ma2$sigma2)

# ARMA(1,1)

arma1 <- arima(dfx, order = c(1, 0, 1), method = "CSS")
arma1

# in-sample RMSE

sqrt(arma1$sigma2)

# This is for fxrate

fxrate <- eurofx$fxrate


# ARIMA(2,1,0)

ar21 <- arima(fxrate, order = c(2, 1, 0), method = "CSS")
ar21
sqrt(ar21$sigma2)

# ARIMA(0,1,2)

ma21 <- arima(fxrate, order = c(0, 1, 2))
ma21
sqrt(ma21$sigma2)

# ARIMA(1,1,1)

arma11<- arima(fxrate, order = c(1, 1, 1), method = "CSS")
arma11
sqrt(arma11$sigma2)

# This is for prediction

pred1 <- predict(ar2, n.ahead = 2)
pred1

pred2 <- predict(ar21, n.ahead = 2)
pred2
