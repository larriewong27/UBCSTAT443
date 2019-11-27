## This is the R scrpit for STAT 443 Lab 6

pressure.train <- ts(pressure$pressure[1:108])
pressure.test <- ts(pressure$pressure[109:120])

acf(pressure.train)
pacf(pressure.train)

fitma1 <- arima(pressure.train, c(0, 0, 1), method = "CSS")
fitma1

fitma2 <- arima(pressure.train, c(0, 0, 2), method = "CSS")
fitma2

tsdiag(fitma1)

## one step and two steps forecast for MA(1):

pred1 <- predict(fitma1, n.ahead = 2)
pred1

## one to 3 steps forecast of MA(2):

pred2 <- predict(fitma2, n.ahead = 3)
pred2
