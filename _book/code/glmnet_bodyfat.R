library(tidyverse)
library(glmnet)

# this code uses L1 and L2 regularization (ridge & lasso, respectively) to compare 
# root mean squared error (rmse) for each
# elastic net regularization is a weighted average of the two, with a parameter
# alpha controlling the weight on each

# First, read the data
bodyfat <- readr::read_tsv('http://vulstats.ucsd.edu/data/bodyfat.data2.txt') %>%
  select(-density) # but don't keep density cause it's the same as bf.percent

# Next,  set up our training and test set 
N = nrow(bodyfat)
k = 4 # we'll use 1/k rows for testing and the rest for training
#bodyfat.new <- data.frame(x = runif(N), y = runif(N)) # this just generates random data -- not useful here
train.n <- round(N*(k-1)/k)  # this is taking 3/4 of the data for training, and 1/4 for test
train.idx <- sample(N, train.n, replace = F) # what are the row numbers of the samples we want for training?

# Make the training set
bodyfat.train <- bodyfat[train.idx, ] # put a random 3/4 of rows into the training set
bodyfat.y.train <- bodyfat.train$bf.percent # keep the y values to use in glmnet() as a VECTOR
bodyfat.train <- as.matrix(bodyfat.train[,!(names(bodyfat.train) %in% c('bf.percent'))]) # but take the y values out of the training set AS A MATRIX

# Make the test set
bodyfat.test <- bodyfat[-train.idx, ] # put the remaining 1/4 of rows into the training set
bodyfat.y.test <- bodyfat.test$bf.percent # keep the y values to use in glmnet()
bodyfat.test <- as.matrix(bodyfat.test[,!(names(bodyfat.test) %in% c('bf.percent'))]) # but take the y values out of the test set too

# Now, fit the models
# We've saved the predictor variables (`bodyfat.train`) & response variable (`bodyfat.y.train`)

fit.ridge <- glmnet(bodyfat.train, # MATRIX
                    y = bodyfat.y.train, # VECTOR
                    alpha = 0)

fit.elastic <- glmnet(bodyfat.train, # MATRIX
                      y = bodyfat.y.train, # VECTOR
                      alpha = .5) # or 0 < alpha < 1

fit.lasso <- glmnet(bodyfat.train, # MATRIX
                    y = bodyfat.y.train, # VECTOR
                    alpha = 1)

# Now we can evaluate their predictions
# writing a function to do that 
rmse_glmnet <- function(model, correct_y){ # take fitted model and correct bodyfat percentages as input
  y_hat = predict(model, bodyfat.test) # make model predictions for test set
  sqrt(mean((y_hat-correct_y)^2)) # compare those to actual correct and take rmse
}

# get out rmse for each model
rmse_glmnet(fit.ridge, bodyfat.y.test)
rmse_glmnet(fit.lasso, bodyfat.y.test)
rmse_glmnet(fit.elastic, bodyfat.y.test)
