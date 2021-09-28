library(tidyverse)

kc <- read_csv('~/Downloads/kc_house_data.csv')

library(glmnet)
glimpse(kc)

nrow(kc)

kc <- sample(nrow(kc), size = 1000, replace=F)

train.df <- kc[train.idx, ]
test.df <- kc[-train.idx, ]

# model selection: cross validation

kitchen.sink <- lm(data = train.df, log10(price) ~ .-id)

test.df$prediction <- predict(object = kitchen.sink, test.df)

