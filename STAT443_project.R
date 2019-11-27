## Project: Predict Future Sales 
# Team member: DENG,LIN,WANG,JIA

# Required Package
library(forecast)

# Load data
sales <- read.csv("sales_train.csv")
sales_train <- sales[sales$shop_id==2,] 

# Re-shuffle the data ( weekly Income of Shop 2 )
sales_train$date<- as.Date(sales_train$date,c("%d.%m.%Y"))
sales_train$days<- as.numeric(sales_train[,1]-as.Date("2013-01-01"))
sales_train$week<- sales_train[,7]%/%7+1
X<- aggregate(sales_train$item_cnt_day*sales_train$item_price,by = list(Category = sales_train$week),FUN=sum)

# Handle missing data, we decided to add a row with weekly income = 0
newrow <- data.frame(Category = '5', x = '0')
b <- data.frame(rbind(X[1:4,], newrow,X[5:147,]))
rownames(b) <- 1:nrow(b)
b$x <- as.numeric(b$x)

# Data Exploration
# par(mfrow=c(3,1))
plot.ts(b$x, main = 'Original time series plot')
acf(b$x, main = 'Original acf plot')
pacf(b$x, main = 'Original pacf plot')
b_ts<- ts(b$x)
b_ts<- b_ts/10000
acf(b_ts)
pacf(b_ts)

# differenced the series

# This is for lag = 8 and take 1 difference
diff_b8 <- diff(b_ts, lag = 8, differences = 1)
plot.ts(diff_b8, main = 'Time series plot of differenced series with lag = 8')
acf(diff_b8, main = 'Acf plot of differenced series with lag = 8')
pacf(diff_b8, main = 'Pacf plot of differenced series with lag = 8')

# This is for lag = 4 and take 1 difference
diff_b4 <- diff(b_ts, lag = 4, differences = 1)
plot.ts(diff_b4, main = 'Time series plot of differenced series with lag = 4')
acf(diff_b4, main = 'Acf plot of differenced series with lag = 4')
pacf(diff_b4, main = 'Pacf plot of differenced series with lag = 4')

# define training set and holdout set
tr <- b[1:136,]$x/10000 # scale
hd <- b[137:148,]$x/10000 # scale
# size of each set
n_train <- length(tr)
n_holdout <- length(hd)

### PREDICT
## Peristence:
mse <- 0
fc <- tr[n_train]
zt <- hd[1]
fcerror <- zt - fc
mse <- mse + fcerror^2

for (i in 2:n_holdout) {
  fc <- hd[i-1]
  zt <- hd[i]
  fcerror <- zt - fc
  mse <- mse + fcerror^2
}

# compute root mean square error
rmse1 <- sqrt(mse/n_holdout)
print(rmse1) # computed rmse is 10.38949
## Average:
c_sum <- sum(tr)
mse <- 0
fc <- c_sum/n_train
zt <- hd[1]
fcerror <- zt - fc
mse <- mse + fcerror^2
for (i in 2:n_holdout) {
  c_sum <- c_sum + hd[i-1]
  fc <- c_sum/(n_holdout+i-1)
  zt <- hd[i]
  fcerror <- zt-fc
  mse <- mse + fcerror^2
}
# To compute root mean square error
rmse2 <- sqrt(mse/n_holdout)
print(rmse2) # 207.8936

## Exponential smoothing
forcast1 <- HoltWinters(tr,gamma=F)
plot(forcast1$fitted)

# in-sample RMSE
rmse.es <- sqrt(forcast1$SSE/(n_train-1)) # 21.5555
print(rmse.es)

#out-of-sample RMSE
alpha<-0.4520125
beta<-0.1274582
v<-forcast1$coefficients[1]
b<-forcast1$coefficients[2]
sse<-0
fc<-v+b
zt<-hd[1]
newfc<-fc
fcerror<-zt-fc
sse<-sse+fcerror^2
vprev<-v
bprev<-b
for (i in 2:12){
  vnew<-alpha*hd[i-1]+(1-alpha)*fc
  bnew<-beta*(vnew-vprev)+(1-beta)*bprev
  fc<-vnew+bnew
  zt<-hd[i]
  fcerror<-zt-fc
  sse<-sse+fcerror^2
  vprev<-vnew
  bprev<-bnew
}
rmse4 <- sqrt(sse/12) # RMSE: 8.943779
print(rmse4)

## ARIMA (period 4)
model1<- Arima(window(b_ts,end=136),order=c(0,1,1),seasonal=list(order=c(1,1,1),period=4),method="CSS")
pred_model1<- Arima(window(b_ts,start=137),model=model1)
accur1 <- accuracy(pred_model1) #5.209149
print(accur1)

## ARIMA (period 8)
model2<- Arima(window(b_ts,end=136),order=c(0,1,1),seasonal=list(order=c(1,1,1),period=8),method="CSS")
pred_model2<- Arima(window(b_ts,start=137),model=model2)
accur2 <- accuracy(pred_model2) #3.852723
print(accur2)
## ARIMAX (Add sin & cos)
n<- length(b_ts)

# by testing through different values, period chosen as 10 would achieve lowest RMSE for ARIMA model

# n = 12 would have:
# x1=as.numeric(sin(2*pi*(1:n)/12))
# x2=as.numeric(cos(2*pi*(1:n)/12))
# RMSE = 3.791196

x1=as.numeric(sin(2*pi*(1:n)/10))
x2=as.numeric(cos(2*pi*(1:n)/10))
dfr=data.frame(b_ts,x1,x2)
fitar2x=Arima(window(dfr$b_ts,end=136),order=c(0,1,1),seasonal= list(order = c(1,1,2),period=8),xreg=as.matrix(dfr[1:136,2:3]),method="CSS")
pred_fitar2x = Arima(window(dfr$b_ts,start=137),xreg = as.matrix(dfr[137:148,2:3]),model=fitar2x)
accur2x <- accuracy(pred_fitar2x) # 3.263868
print(accur2x)
# prediction
# forecast plot:
# for model 1:
model11<- Arima(b_ts,order=c(0,1,1),seasonal=list(order=c(1,1,1),period=4),method="CSS")
plot(forecast(model11, h = 8))
# for model 2:
model2<- Arima(b_ts,order=c(0,1,1),seasonal=list(order=c(1,1,1),period=8),method="CSS")
plot(forecast(model2, h = 8))

# for fitar2x:
fitar2x=Arima(dfr$b_ts,order=c(0,1,1),seasonal= list(order = c(1,1,1),period=8),xreg=as.matrix(dfr[,2:3]),method="CSS")
x1<- as.numeric(sin(2*pi*(149:156)/10))
x2<- as.numeric(cos(2*pi*(149:156)/10))
xx<- data.frame(x1,x2)
plot(forecast(fitar2x, h = 8, xreg = as.matrix(xx)))

     