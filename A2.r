## this is the r scrpt for A2

train <- c(chem$conc[1:150])
holdout <- c(chem$conc[151:170])

# simple linear exponential smoothing

z <- ts(train)

hw <- HoltWinters(z, beta = FALSE, gamma = FALSE)

alpha <- 0.2860687

v_n <- 16.94087

sse <- 0
fc <- v_n

zt <- holdout[1]

newfc <- fc

fcerror <- zt - fc

sse <- sse + fcerror^2

vprev <- v_n

for (i in 2:20){
  vnew <- alpha*holdout[i-1] + (1-alpha)*fc
  fc <- vnew
  zt <- holdout[i]
  fcerror <- zt - fc
  sse <- sse + fcerror^2
  vprev <- vnew
}

rmse <- sqrt(sse/20)

rmse

## this is for linear exponential smoothing

hw <- HoltWinters(z, gamma = FALSE)
alpha <- 0.5148245
beta <- 0.1857282
v_n <- 16.964614229
b_n <- 0.008581887

sse <- 0
fc <- v_n
zt <- holdout[1]
newfc <- fc
fcerror <- zt - fc
sse <- sse + fcerror^2
vprev <- v_n
bprev <- b_n
for (i in 2:20){
  vnew <- alpha*holdout[i-1] + (1-alpha)*fc
  bnew <- beta*(vnew - vprev) + (1-beta)*bprev
  fc <- vnew + bnew
  zt <- holdout[i]
  fcerror <- zt-fc
  sse <- sse + fcerror^2
  vprev <- vnew
  bprev <- bnew
}

rmse <- sqrt(sse/20)
rmse

# in-sample prediction
# holt linear

z <- ts(train, start = 1, frequency = 1)
hlfit <- HoltWinters(z,gamma=F)
esfit <- HoltWinters(z, beta = FALSE, gamma = FALSE)
hlfit
pred=predict(hlfit,n.ahead=12,prediction.interval=T);
pred

sse <- 0

sse1 <- esfit$SSE
sse <- hlfit$SSE

rmse <- sqrt(sse1/149)
rmse1 <- sqrt(sse/149)
rmse1
rmse
