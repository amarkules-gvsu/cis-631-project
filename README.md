# STA 631 Project

## Overview

The goal of this project is to create interactive examples of building two 
different types of Generalized Linear Models: one that focuses on linear 
regression and another that focuses on logistic regression. 

## The Data

The dataset I've chosen come from [the USDA's FoodData Central dataset](https://fdc.nal.usda.gov/download-datasets.html).
The data include nutritional information on a variety of foods. In particular, 
I'm focusing on the macro-nutrients in foods for the linear regression application,
and food categories and micro-nutrients for the logistic regression model.

### Linear Regression Model

Linear regression generally uses quantitative independent variables to predict 
or estimate the value of a quantitative dependent variable. This application 
allows users to build models that predict the Energy in a food or the portion size,
using variables such as total lipids, protein, and carbohydrates. It also allows
users to apply transformations such as log, square root, and squared values, to
build a better model. You can check out the application here: 
https://marqless.shinyapps.io/linear_regression_app/
