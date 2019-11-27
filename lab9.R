## This is the R script for STAT 443 Lab 9

dat <- read.csv("data-lab9.csv")

## This is for Part A
## Generate 200 observations for model ARMA(1,1,0)

sim200 <- arima.sim(n = 200, list(order = c(1, 1, 0), ar = 0.5), sd = 1)

dts <- diff(sim200)

ddts <- dts + 3

sum_ddts <- cumsum(ddts)

## plot above 3 different series:

plot.ts(sim200)
plot.ts(dts)
plot.ts(sum_ddts)

## add linear trend

y_t <- sim200

for (i in 1:200){
  y_t[i] <- y_t[i] + 3*i
}

plot.ts(y_t)

## This is for part B



# ARMAX for original data

dat <- ts(dat)

fitarmax11.css <- arima(dat[,2], order = c(1,0,1), method = "CSS", xreg = dat[,1])
fitarmax11.css


# arma(1,1) for differenced data

diffdat <- diff(dat[,2], lag = 1, differences = 1)

firarma11.css <- arima(diffdat, order = c(1,0,1), method = "CSS")
firarma11.css


# Predictions:

pred1 <- predict(firarma11.css,n.ahead = 2)
pred1

pred2 <- predict(fitarmax11.css, n.ahead = 2, newxreg = c(201,202))
pred2

## for the arma11 prediction:

dat[200,2] + 2.363662
dat[200,2] + 2.363662 + 3.371002
