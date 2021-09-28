library(tidyverse)

dat <- read_tsv('http://vulstats.ucsd.edu/data/bodyfat.data2.txt')
dat <- dat %>% select(-density) %>% filter(height > 30)

require(e1071)

RMSE = function(true_y, predicted_y){
  sqrt(mean((predicted_y - true_y)^2))
}

# R.squared = function(true_y, predicted_y){
#   var_y = var(true_y)
#   var_res = mean((predicted_y - true_y)^2)
#   return(1 - var_res / var_y)
# }

svr.model = svm(data = dat, bf.percent ~ ., cross=0)

lm.model = lm(data = dat, bf.percent ~ .)

lm2.model = lm(data = dat, 
              bf.percent ~ polym(age, 
                                 weight, 
                                 height, neck, chest, abdomen, hip, thigh, knee, ankle, bicep, forearm, wrist, degree = 2))

R.squared(dat$bf.percent, predict(svr.model, dat))
R.squared(dat$bf.percent, predict(lm2.model, dat))
R.squared(dat$bf.percent, predict(lm.model, dat))

n.hold = 50

n_shuffles = 100

results = tibble()

for(i in 1:n_shuffles){
  idx = sample(1:nrow(dat), nrow(dat), replace=F)
  test.dat = dat[idx[1:n.hold],]
  train.dat = dat[idx[(n.hold+1):nrow(dat)],]
  
  svr.model = svm(data = train.dat, bf.percent ~ ., cross=0)
  
  results = bind_rows(results, 
                      tibble(iteration = i, 
                             model = 'svr',
                             r2.train = R.squared(train.dat$bf.percent,
                                                  predict(svr.model, train.dat)),
                             r2.test = R.squared(test.dat$bf.percent,
                                                 predict(svr.model, train.dat))))
  
  lm.model = lm(data = train.dat, bf.percent ~ .)
  
  results = bind_rows(results, 
                      tibble(iteration = i, 
                             model = 'lm.1',
                             r2.train = R.squared(train.dat$bf.percent,
                                                  predict(lm.model, train.dat)),
                             r2.test = R.squared(test.dat$bf.percent,
                                                 predict(lm.model, train.dat))))
  
  lm2.model = lm(data = train.dat, 
                 bf.percent ~ polym(age, 
                                    weight, 
                                    height, 
                                    neck, 
                                    chest, 
                                    abdomen, 
                                    hip, 
                                    thigh, 
                                    knee, 
                                    ankle, 
                                    bicep, 
                                    forearm, 
                                    wrist, 
                                    degree = 2))
  
  results = bind_rows(results, 
                      tibble(iteration = i, 
                             model = 'lm.2',
                             r2.train = R.squared(train.dat$bf.percent,
                                                  predict(lm2.model, train.dat)),
                             r2.test = R.squared(test.dat$bf.percent,
                                                 predict(lm2.model, train.dat))))

}
