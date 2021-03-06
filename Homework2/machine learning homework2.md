Homework2
================
Weixi Chen
1/30/2022

### Note: the homework code is behind the original code

## original code

``` r
## load prostate data
prostate <- 
  read.table(url(
    'https://web.stanford.edu/~hastie/ElemStatLearn/datasets/prostate.data'))
```

``` r
## subset to training examples
prostate_train <- subset(prostate, train==TRUE)
```

``` r
## plot lcavol vs lpsa
plot_psa_data <- function(dat=prostate_train) {
  plot(dat$lpsa, dat$lcavol,
       xlab="log Prostate Screening Antigen (psa)",
       ylab="log Cancer Volume (lcavol)",
       pch = 20)
}
plot_psa_data()
```

![](Homework2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

############################ 

## regular linear regression

############################ 

``` r
## L2 loss function
L2_loss <- function(y, yhat)
  (y-yhat)^2
```

``` r
## fit simple linear model using numerical optimization
fit_lin <- function(y, x, loss=L2_loss, beta_init = c(-0.51, 0.75)) {
  err <- function(beta)
    mean(loss(y,  beta[1] + beta[2]*x))
  beta <- optim(par = beta_init, fn = err)
  return(beta)
}
```

``` r
## make predictions from linear model
predict_lin <- function(x, beta)
  beta[1] + beta[2]*x
```

``` r
## fit linear model
lin_beta <- fit_lin(y=prostate_train$lcavol,
                    x=prostate_train$lpsa,
                    loss=L2_loss)
```

``` r
## compute predictions for a grid of inputs
x_grid <- seq(min(prostate_train$lpsa),
              max(prostate_train$lpsa),
              length.out=100)
lin_pred <- predict_lin(x=x_grid, beta=lin_beta$par)
```

``` r
## plot data
plot_psa_data()

## plot predictions
lines(x=x_grid, y=lin_pred, col='darkgreen', lwd=2)

## do the same thing with 'lm'
lin_fit_lm <- lm(lcavol ~ lpsa, data=prostate_train)

## make predictins using 'lm' object
lin_pred_lm <- predict(lin_fit_lm, data.frame(lpsa=x_grid))

## plot predictions from 'lm'
lines(x=x_grid, y=lin_pred_lm, col='pink', lty=2, lwd=2)
```

![](Homework2_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

################################## 

## try modifying the loss function

################################## 

``` r
## custom loss function
custom_loss <- function(y, yhat)
  (y-yhat)^2 + abs(y-yhat)
```

``` r
## plot custom loss function
err_grd <- seq(-1,1,length.out=200)
plot(err_grd, custom_loss(err_grd,0), type='l',
     xlab='y-yhat', ylab='custom loss')
```

![](Homework2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
## fit linear model with custom loss
lin_beta_custom <- fit_lin(y=prostate_train$lcavol,
                    x=prostate_train$lpsa,
                    loss=custom_loss)

lin_pred_custom <- predict_lin(x=x_grid, beta=lin_beta_custom$par)
```

``` r
## plot data
plot_psa_data()

## plot predictions from L2 loss
lines(x=x_grid, y=lin_pred, col='darkgreen', lwd=2)

## plot predictions from custom loss
lines(x=x_grid, y=lin_pred_custom, col='pink', lwd=2, lty=2)
```

![](Homework2_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## Question1: implement the L1 loss function and tilted absolute loss function

``` r
## L1 loss function
L1_loss <- function(y, yhat){
  abs(y-yhat)
}
```

``` r
## tilted absolute loss function
tilted_loss <- function(y, yhat, tau){
  loss <- c()
  for(i in 1:length(y)){
    if ((y[i] - yhat[i]) > 0) {
      loss[i] = tau*(y[i] - yhat[i])
    }
    else {
      loss[i] = (tau - 1)*(y[i] - yhat[i])
    }
  }
  return(loss)
}
```

## Question2: the linear model predictors with different loss functions

``` r
## fit simple linear model using numerical optimization
fit_lin1 <- function(y, x, loss = tilted_loss, beta_init = c(-0.51, 0.75), tau) {
  err <- function(beta)
    mean(loss(y,  beta[1] + beta[2]*x, tau))
  beta <- optim(par = beta_init, fn = err)
  return(beta)
}
```

``` r
## fit linear model with l1 loss functions
lin_beta_l1 <- fit_lin(y = prostate_train$lcavol,
                    x = prostate_train$lpsa,
                    loss = L1_loss)
## fit linear model with l2 loss function
lin_beta_l2 <- fit_lin(y = prostate_train$lcavol,
                    x = prostate_train$lpsa,
                    loss = L2_loss)
## fit linear model with tilted absolute loss function (tau = 0.25)
lin_beta_tilted1 <- fit_lin1(y = prostate_train$lcavol,
                    x = prostate_train$lpsa,
                    loss = tilted_loss,
                    tau = 0.25)
## fit linear model with tilted absolute loss function (tau = 0.75)
lin_beta_tilted2 <- fit_lin1(y = prostate_train$lcavol,
                    x = prostate_train$lpsa,
                    loss = tilted_loss,
                    tau = 0.75)

## get the predicted values when using l1 loss function
lin_pred_l1 <- predict_lin(x = x_grid, beta = lin_beta_l1$par)
## get the predicted values when using l2 loss function
lin_pred_l2 <- predict_lin(x = x_grid, beta = lin_beta_l2$par)
## get the predicted values when using tilted absolute loss function(tau = 0.25)
lin_pred_tilted1 <- predict_lin(x = x_grid, beta = lin_beta_tilted1$par)
## get the predicted values when using tilted absolute loss function(tau = 0.75)
lin_pred_tilted2 <- predict_lin(x = x_grid, beta = lin_beta_tilted2$par)
```

``` r
## plot data
plot_psa_data()

## plot predictions from L1 loss
lines(x = x_grid, y = lin_pred_l1, col='darkgreen', lwd=2)

## plot predictions from L2 loss
lines(x=x_grid, y=lin_pred_l2, col='pink', lwd=2, lty=2)

## plot predictions from tilted absolute loss(tau = 0.25)
lines(x = x_grid, y = lin_pred_tilted1, col='blue', lwd=2, lty = 3)

## plot predictions from tilted absolute loss(tau = 0.75)
lines(x=x_grid, y=lin_pred_tilted2, col='orange', lwd=2, lty=4)

## add legend to the plot
legend("topleft", c("L1 loss", "L2 loss", "tau = 0.25", "tau = 0.75"), lty = c(1, 2, 3, 4), col = c("darkgreen", "pink", "blue", "orange"))
```

![](Homework2_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

## Question3: fit and predict from a nonlinear model

``` r
## fit simple nonlinear model using numerical optimization
fit_nonlin <- function(y, x, loss=L2_loss, beta_init = c(-1, 0, -0.3)) {
  err <- function(beta)
    mean(loss(y,  beta[1] + beta[2]*exp(-beta[3]*x)))
  beta <- optim(par = beta_init, fn = err)
  return(beta)
}
```

``` r
## make predictions from nonlinear model
predict_nonlin <- function(x, beta)
  beta[1] + beta[2]*exp(-beta[3]*x)
```

## Question4: the nonlinear model wuth different loss functions

``` r
## fit simple nonlinear model using numerical optimization
fit_nonlin1 <- function(y, x, loss = tilted_loss, beta_init = c(-1, 0, -0.3), tau) {
  err <- function(beta)
    mean(loss(y,  beta[1] + beta[2]*exp(-beta[3]*x), tau))
  beta <- optim(par = beta_init, fn = err)
  return(beta)
}
```

``` r
## fit nonlinear model with l1 loss functions
nonlin_beta_l1 <- fit_nonlin(y = prostate_train$lcavol,
                    x = prostate_train$lpsa,
                    loss = L1_loss)
## fit nonlinear model with l2 loss function
nonlin_beta_l2 <- fit_nonlin(y = prostate_train$lcavol,
                    x = prostate_train$lpsa,
                    loss = L2_loss)
## fit nonlinear model with tilted absolute loss function (tau = 0.25)
nonlin_beta_tilted1 <- fit_nonlin1(y = prostate_train$lcavol,
                    x = prostate_train$lpsa,
                    loss = tilted_loss,
                    tau = 0.25)
## fit nonlinear model with tilted absolute loss function (tau = 0.75)
nonlin_beta_tilted2 <- fit_nonlin1(y = prostate_train$lcavol,
                    x = prostate_train$lpsa,
                    loss = tilted_loss,
                    tau = 0.75)

## get the predicted values when using l1 loss function
nonlin_pred_l1 <- predict_nonlin(x = x_grid, beta = nonlin_beta_l1$par)
## get the predicted values when using l2 loss function
nonlin_pred_l2 <- predict_nonlin(x = x_grid, beta = nonlin_beta_l2$par)
## get the predicted values when using tilted absolute loss function(tau = 0.25)
nonlin_pred_tilted1 <- predict_nonlin(x = x_grid, beta = nonlin_beta_tilted1$par)
## get the predicted values when using tilted absolute loss function(tau = 0.75)
nonlin_pred_tilted2 <- predict_nonlin(x = x_grid, beta = nonlin_beta_tilted2$par)
```

``` r
## plot data
plot_psa_data()

## plot predictions from L1 loss
lines(x = x_grid, y = nonlin_pred_l1, col='darkgreen', lwd=2)

## plot predictions from L2 loss
lines(x=x_grid, y=nonlin_pred_l2, col='pink', lwd=2, lty=2)

## plot predictions from tilted absolute loss(tau = 0.25)
lines(x = x_grid, y = nonlin_pred_tilted1, col='blue', lwd=2, lty = 3)

## plot predictions from tilted absolute loss(tau = 0.75)
lines(x=x_grid, y=nonlin_pred_tilted2, col='orange', lwd=2, lty=4)

## add legend to the plot
legend("topleft", c("L1 loss", "L2 loss", "tau = 0.25", "tau = 0.75"), lty = c(1, 2, 3, 4), col = c("darkgreen", "pink", "blue", "orange"))
```

![](Homework2_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->
