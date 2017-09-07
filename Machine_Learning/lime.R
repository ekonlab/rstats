#######################################################
# https://github.com/thomasp85/lime
#######################################################

# LIME - Local Interpretable Model-Agnostic 

# The purpose of lime is to explain the predictions of black box classifiers. 
# What this means is that for any given prediction and any given classifier it is able to determine a small set of features
# in the original data that has driven the outcome of the prediction


library(caret)
library(lime)

# Split up the data set
iris_test <- iris[1:5, 1:4]
iris_train <- iris[-(1:5), 1:4]
iris_lab <- iris[[5]][-(1:5)]

# Create Random Forest model on iris data
model <- train(iris_train, iris_lab, method = 'rf')
model

# Create explanation function
explain <- lime(iris_train, model)
explain

# Explain new observation
explanation <- explain(iris_test, n_labels = 1, n_features = 2)

# The output is provided in a nice tidy format
tibble::glimpse(explanation)


