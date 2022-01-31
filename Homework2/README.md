## Homework 2

Using the RMarkdown/knitr/github mechanism, implement the following tasks by extending the example R script ( prostate-data-lin.R):

1. Write functions that implement the L1 loss and tilted absolute loss functions.
2. Create a figure that shows lpsa (x-axis) versus lcavol (y-axis). Add and label (using the 'legend' function) the linear model predictors associated with L2 loss, L1 loss, and tilted absolute value loss for tau = 0.25 and 0.75.
3. Write functions to fit and predict from a simple nonlinear model with three parameters defined by 'beta[1] + beta[2]*exp(-beta[3]*x)'. Hint: make copies of 'fit_lin' and 'predict_lin' and modify them to fit the nonlinear model. Use c(-1.0, 0.0, -0.3) as 'beta_init'.
4. Create a figure that shows lpsa (x-axis) versus lcavol (y-axis). Add and label (using the 'legend' function) the nonlinear model predictors associated with L2 loss, L1 loss, and tilted absolute value loss for tau = 0.25 and 0.75.
