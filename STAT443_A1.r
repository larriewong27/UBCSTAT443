train <- c(-1.024, -0.825, -0.96, -0.427, -1.127, -0.537, -0.607, 0.296, 0.07, 0.035, 1.454, 1.066, -0.04, -0.855, 0.59, -0.148, 1.727, -0.049, -0.207, -0.286)
> my_acf <- acf(train, plot = FALSE)
> my_acf

Autocorrelations of series ‘train’, by lag

     0      1      2      3      4      5      6      7      8      9     10     11     12     13 
 1.000  0.327  0.257 -0.017  0.159  0.078  0.056 -0.126 -0.178 -0.112 -0.307 -0.126 -0.163 -0.020 
> x_i <- c(train[1:19])
> y_i <- c(train[2:20])
> x_i
 [1] -1.024 -0.825 -0.960 -0.427 -1.127 -0.537 -0.607  0.296  0.070  0.035  1.454  1.066 -0.040
[14] -0.855  0.590 -0.148  1.727 -0.049 -0.207
> y_i
 [1] -0.825 -0.960 -0.427 -1.127 -0.537 -0.607  0.296  0.070  0.035  1.454  1.066 -0.040 -0.855
[14]  0.590 -0.148  1.727 -0.049 -0.207 -0.286
> train
 [1] -1.024 -0.825 -0.960 -0.427 -1.127 -0.537 -0.607  0.296  0.070  0.035  1.454  1.066 -0.040
[14] -0.855  0.590 -0.148  1.727 -0.049 -0.207 -0.286
> lm(y_i~x_i)

Call:
lm(formula = y_i ~ x_i)

Coefficients:
(Intercept)          x_i  
   -0.01666      0.32745  

> holdout <- c(-0.926, -0.253, -0.309, 0.524, -1.278)
> mse <- 0;
> fc <- train[20];
> zt <- holdout[1];
> fcerror <- zt-fc;
> mse <- mse + fcerror^2
> for (i in 2:5) {
+   
+   fc<-holdout[i-1];
+   zt<-holdout[i];
+   fcerror <- zt-fc;
+   mse <- mse + fcerror^2
+   
+ }
> rmse <- sqrt(mse/5);
> print(rmse)
[1] 0.9804854
> 2:5
[1] 2 3 4 5
> c_sum <- sum(train)
> mse <- 0;
> fc <- c_sum/20;
> zt <- holdout[1];
> fcerror <- zt-fc;
> mse <- mse + fcerror^2
> for (i in 2:5) {
+   
+   c_sum <- c_sum + holdout[i-1]
+   fc<-c_sum/(20+i-1);
+   zt<-holdout[i];
+   fcerror <- zt-fc;
+   mse <- mse + fcerror^2
+   
+ }
> rmse <- sqrt(mse/5);
> print(rmse)
[1] 0.7117791
> 
> mse <- 0;
> fc <- x_i + y_i*train[20];
> zt <- holdout[1];
> fcerror <- zt-fc;
> mse <- mse + fcerror^2
> mse <- 0;
> fc <- x_i + y_i*train[20];
> mse <- 0;
> fc <- -0.01666 + 0.32745*train[20];
> zt <- holdout[1];
> fcerror <- zt-fc;
> mse <- mse + fcerror^2
> for (i in 2:5) {
+   
+   fc<--0.01666 + 0.32745*holdout[i-1];
+   zt<-holdout[i];
+   fcerror <- zt-fc;
+   mse <- mse + fcerror^2
+   
+ }
> rmse <- sqrt(mse/5);
> print(rmse)
[1] 0.7973634