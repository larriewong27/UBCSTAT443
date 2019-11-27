## This is the R script for STAT 443 Lab8

zt <- ts(c(3.89,8.04,10.26,10.72,10.69,12.50,16.43,20.15,22.38,22.45,21.91,24.06,28.03,32.35,34.47,34.47,34.98,36.36,39.86,43.57))
ts(zt)

# differencing

zt_1 <- diff(zt)

acfdiff <- acf(zt_1, plot = F)

# difference again (in order to remove the sesonal effect)

zt_11 <- diff(zt_1, lag = 6)

# plot acf
acf(zt_1)
acf(zt_11)

# period = 6

# Using arima(0,1,0)

arima010 <- arima(zt, order = c(0, 1, 0), seasonal = list(order = c(0, 1, 0), period = 6), method = "CSS")

arima010
