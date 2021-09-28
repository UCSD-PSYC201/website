library(glmnet)
library(tidyverse)

dat <- read_tsv('data/bodyfat.data2.txt')
dat <- dat %>% select(-density)
dat <- dat %>% filter (height > 36)

# pull out predictors as a matrix
predictors <- dat %>% select(-bf.percent) %>% as.matrix()

# fit a glmnet model
M <- glmnet(predictors, y = dat$bf.percent, alpha=0.5)

# generate predictions (on training data), given penalty term s
predictions = predict.glmnet(M, predictors, s=0)

# plotting
plot(predictions, dat$bf.percent)


# k fold cross validation
k = 25

# assign each row a number 1:k, 
foldidx = rep(1:k, ceiling(nrow(dat)/k))[1:nrow(dat)]
# shuffle the numbers across rows.
foldidx = sample(foldidx, nrow(dat), replace=F)

rmse.train = c()
rmse.test = c()
# for each fold
for(fold in 1:k){
  # figure out which rows are included in the training data
  train.idx = !(foldidx == fold)
  # figure out which rows are in the test data
  test.idx = foldidx == fold
  
  # fit a model, using just the training data
  M <- glmnet(predictors[train.idx,], 
              y = dat$bf.percent[train.idx], 
              alpha=0.5)
  
  # prediction and rmse on train
  # generate predictions on the training data.
  predictions = predict.glmnet(M, 
                               predictors[train.idx,], 
                               s=0)
  # calculate rmse on the training data.
  rmse.train[fold] = sqrt(mean((dat$bf.percent[train.idx] - predictions)^2))
  # prediction and rmse on test
  # gnerate predictions on the test data, using model from training data.
  predictions = predict.glmnet(M, 
                               predictors[test.idx,], 
                               s=0)
  # calculate rmse on test data.
  rmse.test[fold] = sqrt(mean((dat$bf.percent[test.idx] - predictions)^2))
}

# average rmse for training and test data.
c(mean(rmse.train), mean(rmse.test))



nlambda = 100
rmse.train = matrix(nrow = k, ncol=nlambda)
rmse.test = matrix(nrow = k, ncol=nlambda)
# for each fold generate rmse for all values of s
for(fold in 1:k){
  # figure out which rows are included in the training data
  train.idx = !(foldidx == fold)
  # figure out which rows are in the test data
  test.idx = foldidx == fold
  
  # fit a model, using just the training data
  M <- glmnet(predictors[train.idx,], 
              y = dat$bf.percent[train.idx], 
              alpha=0.5, 
              lambda = c(0, 10^seq(-1, log10(15), length.out=99)))
  
  # prediction and rmse on train
  # generate predictions on the training data.
  predictions = predict.glmnet(M, 
                               predictors[train.idx,])
  # calculate rmse on the training data.
  rmse.train[fold, ] = sqrt(colSums((matrix(dat$bf.percent[train.idx], 
         nrow = sum(train.idx), 
         ncol = dim(predictions)[2]) - predictions)^2)/sum(train.idx))
  # prediction and rmse on test
  # gnerate predictions on the test data, using model from training data.
  predictions = predict.glmnet(M, 
                               predictors[test.idx,])
  # calculate rmse on the training data.
  rmse.test[fold, ] = sqrt(colSums((matrix(dat$bf.percent[test.idx], 
                                            nrow = sum(test.idx), 
                                            ncol = dim(predictions)[2]) - 
                                       predictions)^2)/sum(test.idx))
}

# average rmse for training and test data.
c(mean(rmse.train), mean(rmse.test))

data.frame(s = 1:100, rmse.test = colMeans(rmse.test), rmse.train = colMeans(rmse.train)) %>% 
  ggplot(aes(x = rmse.train, y=rmse.test, size=s))+
  geom_point()
