### this is for lab5

# this is for part 1:
arim <- arima.sim(n = 500, list(ar = c(0.8, -1/3, 0.6/sqrt(3))), sd = sqrt(0.8))

acf_plot <- acf(arim)

pacf_plot <- pacf(arim)

# for part 2:


set.seed(1234)

arim <- arima.sim(n = 500, list(ar = c(0.8, -1/3, 0.6/sqrt(3))), sd = sqrt(0.8))

arima_1234 <- arima(arim, order = c(3, 0, 0), include.mean = FALSE)
arima_1234

arima_method <- arima(arim, order = c(3, 0, 0), include.mean = FALSE, method = "CSS")
arima_method
