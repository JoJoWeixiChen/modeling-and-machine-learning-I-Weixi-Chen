## Homework6
> Goal: Understand and implement a random forest classifier.
Using the “vowel.train” data, and the “randomForest” function in the R package “randomForest”. Develop a random forest classifier for the vowel data by doing the following:
1.Convert the response variable in the “vowel.train” data frame to a factor variable prior to training, so that “randomForest” does classification rather than regression.
2.Review the documentation for the “randomForest” function.
3.Fit the random forest model to the vowel data using all of the 11 features using the default values of the tuning parameters.
4.Use 5-fold CV and tune the model by performing a grid search for the following tuning parameters: 1) the number of variables randomly sampled as candidates at each split; consider values 3, 4, and 5, and 2) the minimum size of terminal nodes; consider a sequence (1, 5, 10, 20, 40, and 80).
5.With the tuned model, make predictions using the majority vote method, and compute the misclassification rate using the ‘vowel.test’ data.
