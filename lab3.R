## this is for lab3

train <- window(sunspot.year, start = 1700, end = 1970)
holdout <- window(sunspot.year, start = 1971, end = 1988)

alpha <- 1
beta <- 0.9659916
v_n <- 104.5000000
b_n <- -0.9637942


sse <- 0

fc <- v_n + b_n

zt <- holdout[1]

newfc <- fc

fcerror <- zt - fc

sse <- sse + fcerror^2

vprev <- v_n
bprev <- b_n

for (i in 2:18){
  vnew <- alpha*holdout[i-1] + (1-alpha)*fc
  bnew <- beta*(vnew - vprev) + (1-beta)*bprev
  fc <- vnew + bnew
  zt <- holdout[i]
  fcerror <- zt-fc
  sse <- sse + fcerror^2
  vprev <- vnew
  bprev <- bnew
}

rmse <- sqrt(sse/18)
rmse
