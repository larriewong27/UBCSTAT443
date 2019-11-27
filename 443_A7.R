## Test script

# define model first
arma.model <- arima(ts(rnorm(100), frequency = 12), order = c(1, 0, 0), seasonal = list(order = c(1, 0, 0), period = 12), method = "CSS")

new.sim <- arima.sim(n = 200, model = list(order = c(1, 0, 0), ar = arma.model$coef[1]))

summary(arma.model)

arma.model

vtemp <- read.csv("vtempprec.csv")
mean_temp <- vtemp$meantemp

plot.ts(ts(mean_temp))
acf(mean_temp)
pacf(mean_temp)
library(forecast)
auto.arima(mean_temp)
set.seed(1)

fit24 <- arima(ts(rnorm(100), frequency = 10), order = c(24, 0, 0))
sim24 <- arima.sim(n = 828, model = list(order = c(24, 0, 0), ar = c(fit24$coef[1:24])))
plot.ts(sim24)
acf(sim24)
pacf(sim24)

sim.data <- arima.sim(n = 828, model = list(order = c(5, 0, 1), ar = c(1.122, -0.3081, -0.0966, -0.1523, -0.1034), ma = c(-0.4561)), mean = 10.0390, sd = sqrt(2.522))
plot.ts(sim.data) 
acf(sim.data)
pacf(sim.data)


## for q4:

lag24 <- acf(mean_temp, plot = F)[1:24]
lag24 <- c(lag24$acf)

a <- c(1, lag24[1:23])
b <- c()

for (i in 2:23){
  b <- c(1, lag24[1:(24-i)])
  b <- c(rev(lag24[1:(i-1)]), b)
  a <- c(a, b)
}

diagonal <- c(rev(lag24[23:1]), 1)
a <- c(a,diagonal)
topletiz <- matrix(a, nrow = 24, ncol = 24)

ar24 <- solve(topletiz) %*% lag24
set.seed(1)
sim24 <- arima.sim(n=828, list(ar = ar24))
acf(sim24)
plot(sim24)
ar24
