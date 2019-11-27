## this is for STAT443 HW3

train <- bradfordtemp$maxtemp[1:780]
holdout <- bradfordtemp$maxtemp[781:816]

z <- ts(train, start = c(1950, 1), frequency = 12)

wafit <- HoltWinters(z, seasonal = "additive")
wafit$fitted

#a
jan_sum <-0

for (i in 1:780){
  if (i %% 12 == 1){
    jan <- z[i]
    jan_sum = jan_sum + jan
  }
}
mean_jan <- jan_sum/65
mean_jan

july_sum <- 0
for (i in 1:780){
  if (i %% 12 == 7){
    july <- z[i]
    july_sum = july_sum + july
  }
}
mean_july <- july_sum/65

#b
dec_sum <- 0
for (i in 1:780){
  if (i %% 12 == 0){
    print(z[i])
    dec <- z[i]
    dec_sum = dec_sum + dec
  }
}

mean_dec <- dec_sum/65

#c
feb_sum <-0

for (i in 1:780){
  if (i %% 12 == 2){
    feb <- z[i]
    feb_sum = feb_sum + feb
  }
}
mean_feb <- feb_sum/65
mean_feb

mar_sum <-0

for (i in 1:780){
  if (i %% 12 == 3){
    mar <- z[i]
    mar_sum = mar_sum + mar
  }
}
mean_mar <- mar_sum/65
mean_mar

apr_sum <-0

for (i in 1:780){
  if (i %% 12 == 4){
    apr <- z[i]
    apr_sum = apr_sum + apr
  }
}
mean_apr <- apr_sum/65
mean_apr

may_sum <-0

for (i in 1:780){
  if (i %% 12 == 5){
    may <- z[i]
    may_sum = may_sum + may
  }
}
mean_may <- may_sum/65

jun_sum <-0

for (i in 1:780){
  if (i %% 12 == 6){
    jun <- z[i]
    jun_sum = jun_sum + jun
  }
}
mean_jun <- jun_sum/65

aug_sum <-0

for (i in 1:780){
  if (i %% 12 == 8){
    aug <- z[i]
    aug_sum = aug_sum + aug
  }
}
mean_aug <- aug_sum/65

sep_sum <-0

for (i in 1:780){
  if (i %% 12 == 9){
    sep <- z[i]
    sep_sum = sep_sum + sep
  }
}
mean_sep <- sep_sum/65

oct_sum <-0

for (i in 1:780){
  if (i %% 12 == 10){
    oct <- z[i]
    oct_sum = oct_sum + oct
  }
}
mean_oct <- oct_sum/65

nov_sum <-0

for (i in 1:780){
  if (i %% 12 == 11){
    nov <- z[i]
    nov_sum = nov_sum + nov
  }
}
mean_nov <- nov_sum/65

for (i in 1:780){
  if (i %% 12 == 0){
    new_z[i] <- new_z[i] - mean_dec
  }
}

x <- c(1:780)

z_hat_fit<-lm(zz~x)

all_mean <- c(mean_jan, mean_feb, mean_mar, mean_apr, mean_may, mean_jun, mean_july, mean_aug, mean_sep, mean_oct, mean_nov, mean_dec)

mean(all_mean)

mean_jan - mean(all_mean)


## part d, e, f
z_stl <- stl(z, s.window = "periodic")

trend <- c(z_stl$time.series[781:1560])
ts.plot(trend)

level<-c(wafit$fitted[769:1536])

ts.plot(level)

## part g


alpha <- 0.1062086
beta <- 0.01058402
gamma <- 0.07989711

v_n <- 13.271040677
b_n <- 0.006106753

sse <- 0

fc <- v_n + b_n - 5.952881909

zt<-holdout[1]

newfc <- fc

fcerror <- zt-fc

sse <- sse + fcerror^2

vprev <- v_n

bprev <- b_n

s <- c(-5.952881909,-5.550245656, -3.286305980, -0.300327271, 2.847313097, 5.514826340, 7.642621375, 6.933346687, 4.548808993, 0.942178959, -3.144280706, -5.782080935)

## for holtwinters exponential smoothing

for (i in 2:36){
  if( ((i-1)%%12) == 0 | (i%%12) == 0){
    vnew <- alpha*(holdout[i-1] - s[12]) + (1-alpha)*(vprev + bprev)
    bnew <- beta*(vnew-vprev) + (1-beta)*bprev
    snew <- gamma*(holdout[i-1]-vnew) + (1-gamma)*s[12]
    fc <- vnew + bnew + s[12]
    zt<-holdout[i]
    fcerror <- zt-fc
    sse <- sse + fcerror^2
    vprev <- vnew
    bprev <- bnew
    s[12] <- snew
  } 
  else {
    vnew <- alpha*(holdout[i-1] - s[(i-1)%%12]) + (1-alpha)*(vprev + bprev)
    bnew <- beta*(vnew-vprev) + (1-beta)*bprev
    snew <- gamma*(holdout[i-1]-vnew) + (1-gamma)*s[(i-1)%%12]
    fc <- vnew + bnew + s[i%%12]
    zt<-holdout[i]
    fcerror <- zt-fc
    sse <- sse + fcerror^2
    vprev <- vnew
    bprev <- bnew
    s[(i-1)%%12] <- snew
    }
  }

rmse <- sqrt(sse/36)
rmse

## this is for using the preview month

new_holdout <- c(11.5, 9.2, 7.0)
mse <- 0

fc <- train[780]
zt <- holdout[1]
fcerror <- zt-fc
mse <- mse + fcerror^2

for (i in 2:3){
  fc <- holdout[i-1]
  zt <- holdout[i]
  fcerror <- zt - fc
  mse <- mse + fcerror^2
}

rmse <- sqrt(mse/3)
rmse
## averaging the sum over all previous year
sum_all <- 0
new_holdout1 <- c(6.3, 7.5, 6.6)


for (i in 1:780) {
  if(i %% 12 == 1){
    sum_all <- sum_all + train[i]
  }
}

mse <- 0

fc <- sum_all/65
zt <- new_holdout1[1]
fcerror <- zt - fc
mse <- mse + fcerror^2

for (i in 2:3){
  sum_all <- sum_all + new_holdout1[i-1]
  fc <- sum_all/(65+i-1)
  zt <- new_holdout1[i]
  fcerror <- zt-fc
  mse <- mse + fcerror^2
}
rmse <- sqrt(mse/3)
rmse
