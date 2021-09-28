library(tidyverse)

dat <- read_tsv('http://vulstats.ucsd.edu/data/bodyfat.data2.txt') %>%
  select(-density)  # drop density since we never use it (as it it is just a step in calculating bf.percent)

# Let's make a bunch of variables that might be helpful...
dat <- dat %>% 
  mutate(waist.hip = abdomen/hip,
         thigh.knee = thigh/knee,
         forearm.wrist = forearm/wrist,
         weight.height = weight/height,
         bmi = weight/(height^2)*703,
         weight.h3 = weight/(height^3))
# note that simply linearly transforming variables
# e.g., by z.scoring / adding / subtracting / etc
# will not make new variables that will be helpful for prediction
# it will just make our coefficients easier to interpret
# although we can't include bothheight and z.score(height)
# or height, weight, and height-weight, 
# because the transformed variable will be perfectly colinear with existing variables.
# try doing that, and see what happens. 
# (we have no way to assign credit among linearly identical groups of variables; consequently, some of them must be dropped)

# let's look at the correlation structure of all the variables
# the corrplot package makes a tidy looking plot of the correlation matrix.
dat %>% 
  cor() %>%
  corrplot::corrplot.mixed(tl.pos = 'lt')

# let's look at all the scatterplots of bf.percent ~ x for all x
# to do this easily in ggplot, we need to take our data, and convert it into a long format
# with a column bf.percent, a column for measurement, and a column for what variable is being measured.
# this way we can facet based on variable.
dat %>%
  gather(key = variable,   # what we will call the variable label column
         value=measurement,  # what we will call the measurement column
         -bf.percent) %>%  # do this gathering to all variables except bf.percent
  ggplot(aes(x=measurement, y=bf.percent))+
  facet_wrap(~variable, scales = 'free_x')+ # free_x makes these plots easier to read, rather than asking them all to have the same x scale
  geom_point()+
  geom_smooth(method='lm')

# we seem to have an error in height for one person.  
# 30 inches is not just a very short person 
# it's too short for that.  it would set records.
# it must be a measurement/recording error.
# let's exclude this data point from future analysis.
dat = dat %>%
  filter(height > 30)

# let's do a regression with every single predictor we have.
# i.e., a kitchen-sink regression  
# note: tthe . in the formula says "use all other variables to predict"
dat %>% 
  lm(data = ., formula = bf.percent ~ .) %>% 
  summary()
# the relevant numbers here are the multiple r-squared statistic, and perhaps the adjusted r-squared

# which variables are least helpful?
# we will use drop1() to ask this question.
dat %>%
  lm(data = ., formula = bf.percent ~ .) %>%
  drop1(test='F') %>%   # this does model comparisons for all models starting with the baseline, but missing 1 variable
  broom::tidy() %>%     # nifty package/function for getting a tidy data frame from most model results
  arrange(AIC)          # sort the terms from least to most helpful.
# the logic here: AIC is a measure of how *badly* a model fits, correcting for the number of parameters
# so lower AIC is better.
# insofar as dropping a given term yields a lower AIC, that means that this term was pretty useless.


# basically, all the variables we made ourselves are pretty useless
dat %>% 
  lm(data = ., formula = bf.percent ~ . - weight.height - bmi - weight.h3) %>% # the ". -x" expression in the formula means: use everything except x
  drop1(test='F') %>% 
  broom::tidy() %>% 
  arrange(AIC)

dat %>% 
  lm(data = ., formula = bf.percent ~ . - weight.height - bmi - weight.h3 -weight -hip) %>%
  drop1(test='F') %>% 
  broom::tidy() %>% 
  arrange(AIC)

dat %>% 
  lm(data = ., formula = bf.percent ~ . - weight.height - bmi - weight.h3 -weight -hip -ankle -bicep) %>%
  drop1(test='F') %>% 
  broom::tidy() %>% 
  arrange(AIC)


dat %>% 
  lm(data = ., formula = bf.percent ~ . - weight.height - bmi - weight.h3 -weight -hip -ankle -bicep -chest) %>%
  drop1(test='F') %>% 
  broom::tidy() %>% 
  arrange(AIC)
# at this point, we can't drop any of the terms and lower AIC.  So that's one threshold for keeping the model
# another threshold is "which drop1 model is not significantly worse than the original, which would mean we should keep dropping
# let's just go with the AIC criterion here.

# so how well does this simplified model do?
dat %>% 
  lm(data = ., formula = bf.percent ~ . - weight.height - bmi - weight.h3 -weight -hip -ankle -bicep -chest) %>%
  summary()
# just about as well as the original model with everything.

# one problem with multiple regression, and particularly this stepwise approach, is that it tends to overfit the data.
# instead of just evaluating R^2 on the data we fit to, let's try to see how much we overfit.
# to do this, we will fit the model to part of the data, and evaluate our error on the other half.
# this is called cross-validation.  (more in 201b)

# first, we label part of the data (I choose 75%, somewhat arbitrarily) as the training data, and 25% as the holdout test data
dat = dat %>% 
  mutate(use.data = sample(x = c('test', 'train'),   # sample values from this set
                           size = n(),  # sample size is the size of the data frame (n() is a function from dplyr, that works inside mutate to yield the number of rows)
                           prob = c(0.25, 0.75),  # sample with these probabilities
                           replace=T)) # sample with replacement
# next, we fit a model to just the training data.
model = dat %>% 
  filter(use.data == 'train') %>% 
  select(-use.data) %>% 
  lm(data = ., formula = bf.percent ~ . - weight.height - bmi - weight.h3 -weight -hip -ankle -bicep -chest)

# we will now use the predictions on the test data, to calculate the squared error of the predictions,
# and thus calculate an R squared for the hold out data.
dat %>% filter(use.data == 'test') %>%
  mutate(prediction = predict.lm(object = model, newdata = .),  # generate predictions for the test data.
         squared.error = (bf.percent - prediction)^2) %>%       # calculate squared error for hte predictions
  summarize(var.y = var(bf.percent), # overall variance of y
            var.error = mean(squared.error)) %>%   # variance of the errors
  mutate(R.squared.test = 1 - var.error/var.y,   # 1-fraction of variance that is errors, is fraction of variance that is predicted.  an R.squared
         R.squared.train = summary(model)$r.squared)  # this gives us the R squared from the trained model on the training data.
# so the R squared for the hold-out test data isn't much lower than the r squared on the training data.
# this means that we mostly avoided overfitting (because we had so much data.)

# let's compare this to the kitchen-sink model, which has many more parameters.
model.big = dat %>% 
  filter(use.data == 'train') %>% 
  select(-use.data) %>% 
  lm(data = ., formula = bf.percent ~ .)

dat %>% filter(use.data == 'test') %>%
  mutate(prediction = predict.lm(object = model.big, newdata = .), 
         squared.error = (bf.percent - prediction)^2) %>%     
  summarize(var.y = var(bf.percent), 
            var.error = mean(squared.error)) %>%   
  mutate(R.squared.test = 1 - var.error/var.y,   
         R.squared.train = summary(model.big)$r.squared) 
# this seems to overfit considerably more.
# note (a) the r squared on the training data is higher than in the smaller model
# (b) the r squared on the test data is both lower than the test data and lower than the same for the smaller model
# so in this case shrinking our model by eliminating unnecessary predictors gave us a model that does a better job predicting
# by reducing overfitting.
# we will talk much more about this in 201b

# Ooops!  one problem here, is that we ran this for only one split of the data.  
# things looked good in that single split for the simple model, but in a different random split
# (running lines 118 - 147 again, we would get a different pattern of results.)
# we should ask how this will work on average across many splits....
# note that (your results for the lines above might not look so good)

# lets do this random split and evaluation many times
# this will take a while to run.
results = data.frame()
splits = 1000
for(i in 1:splits){
  dat = dat %>% 
    mutate(use.data = sample(x = c('test', 'train'),  
                             size = n(), 
                             prob = c(0.25, 0.75),  
                             replace=T)) 
  model.small = dat %>% 
    filter(use.data == 'train') %>% 
    select(-use.data) %>% 
    lm(data = ., formula = bf.percent ~ . - weight.height - bmi - weight.h3 -weight -hip -ankle -bicep -chest)
  
  results = dat %>% filter(use.data == 'test') %>%
    mutate(prediction = predict.lm(object = model.small, newdata = .), 
           squared.error = (bf.percent - prediction)^2) %>%      
    summarize(var.y = var(bf.percent), 
              var.error = mean(squared.error)) %>%   
    mutate(R.squared.test = 1 - var.error/var.y,  
           R.squared.train = summary(model.small)$r.squared,
           model = 'smaller', # label this result as coming from the small model
           split = i) %>%
    bind_rows(results)  # bind it to the result data frame we are building.
  
  # let's compare this to the kitchen-sink model, which has many more parameters.
  model.big = dat %>% 
    filter(use.data == 'train') %>% 
    select(-use.data) %>% 
    lm(data = ., formula = bf.percent ~ .)
  
  results = dat %>% filter(use.data == 'test') %>%
    mutate(prediction = predict.lm(object = model.big, newdata = .), 
           squared.error = (bf.percent - prediction)^2) %>%     
    summarize(var.y = var(bf.percent), 
              var.error = mean(squared.error)) %>%   
    mutate(R.squared.test = 1 - var.error/var.y,   
           R.squared.train = summary(model.big)$r.squared,
           model = 'bigger',  # label this as coming from the bigger model
           split = i) %>%
    bind_rows(results) # add it to the results
}

# let's look at how training and test R squared compare for the two models:
results %>% 
  ggplot(aes(x=R.squared.train, y=R.squared.test, color=model))+
  geom_point()+geom_smooth(method='lm')
# in both models, higher training R squared is related to lower testing R squared.
# this is basically the signature of overfitting.
# on some random splits the data used for training can be fit really well, by chance
# this will mean that we fit that random noise really well, but then fail to generalize to the test data.

# we can also look to see how the test R squared compares across the two models for different splits
results %>% 
  select(split, model, R.squared.test) %>%
  spread(key = model, value=R.squared.test) %>%
  ggplot(aes(x=bigger, y=smaller))+
  geom_point()
# note that the hold out R squared is highly correlated across the two models,
# because some splits just give us test data that is more different from the training data. 
# and this will influence both models.

# finally, let's just look at the average test and train R squared for the two models
results %>% 
  select(model, R.squared.test, R.squared.train) %>%
  group_by(model) %>%
  summarize(R2.test.mean = mean(R.squared.test),
                R2.train.mean = mean(R.squared.train),
                R2.test.sd = sd(R.squared.test),
                R2.train.sd = sd(R.squared.train))
# ah, ok.  so this gives us a more stable result. (if we run lines 163 to here many times, we will get roughly the same pattern)
# the bigger model has a lower test R^2, but a higher train R^2, because it overfits more, due to having more parameters.


# ok.... 
# let's use our (smaller) model to predict bf percent for a new person (me... at some point.)
model = dat %>% 
  select(-use.data) %>% 
  lm(data = ., formula = bf.percent ~ . - weight.height - bmi - weight.h3 -weight -hip -ankle -bicep -chest)

newdata = data.frame(
  age = 33,    #  oops, I forgot to include this in the slide.  I think this was my age when I took these measurements
  height = 69, 
  weight = 175,
  neck = 36,
  chest = 100, 
  abdomen = 90,
  hip = 99,
  thigh=58,
  knee=38,
  ankle=22,
  bicep=31,
  forearm=28,
  wrist=17) %>%
  mutate(waist.hip = abdomen/hip,   # remember, we need to generate the new variables we made.  
         thigh.knee = thigh/knee,   # here i just generate all of them, the extra ones not in our model will not be used.
         forearm.wrist = forearm/wrist,
         weight.height = weight/height,
         bmi = weight/(height^2)*703,
         weight.h3 = weight/(height^3))

predict.lm(model, newdata, interval = 'prediction')  
# note that we are asking for a prediction interval, since we care about the confidence interval on the 
# predicted bodyfat for one specific person (not for the average of all people with these measurements)
# so... this predicts my bodyfat to be around 20%, but with a pretty wide interval (between 12% and 28%)

