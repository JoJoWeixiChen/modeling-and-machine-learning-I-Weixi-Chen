## Homework5

Goal: Understand and implement various ways to approximate test error.

In the ISLR book, read section 6.1.3 “Choosing the Optimal Model” and section 5.1 “Cross-Validation”. Extend and convert the attached effective-df-aic-bic-mcycle.R R script into an R markdown file that accomplishes the following tasks.

1.Randomly split the mcycle data into training (75%) and validation (25%) subsets.

2.Using the mcycle data, consider predicting the mean acceleration as a function of time. Use the Nadaraya-Watson method with the k-NN kernel function to create a series of prediction models by varying the tuning parameter over a sequence of values. (hint: the script already implements this)

3.With the squared-error loss function, compute and plot the training error, AIC, BIC, and validation error (using the validation data) as functions of the tuning parameter.

4.For each value of the tuning parameter, Perform 5-fold cross-validation using the combined training and validation data. This results in 5 estimates of test error per tuning parameter value.

5.Plot the CV-estimated test error (average of the five estimates from each fold) as a function of the tuning parameter. Add vertical line segments to the figure (using the segments function in R) that represent one “standard error” of the CV-estimated test error (standard deviation of the five estimates from each fold).

6.Interpret the resulting figures and select a suitable value for the tuning parameter.
