# 201b 2020-03-11

# 1. Final plan
# We will post final after class today
# It will be due by end of day (11:59 PST) Tuesday (3/16)






# 2. Group projects:
# Report due before our scheduled final period Thursday (3/18, 3-6pm)
# Presentations will happen during scheduled final period.
# Report includes a bundle of:
#  - Paper (should look/read like real paper / manuscript! word limit: 3000 words)
#  - Data (zipped or external link if need be)
#  - All code needed to reproduce the results in the paper, and an obvious way to use the code
# Presentations follow the same format as in 201a.



# 3. Final review / questions
# topics that will definitely be on the final
# cleaning / manipulating data
# dealing with binary data
# bootstrapping
# logistic regression
# polynomial regression
# lasso regression via glmnet, picking a solution, plotting it
# Stan: slightly modifying models we explicitly covered in class/lab
# Stan: calculating bayes factors for models
# Stan: extracting parameters and interpreting them


library(tidyverse)
load('../data/cal1020.Rdata')

cal1020 %>% glimpse()

cal1020 %>% 
  mutate(age_bin = floor(Age/5)*5) %>% 
  group_by(age_bin) %>% 
  summarize(m = mean(speed.mph), se = sd(speed.mph)/sqrt(n())) %>% 
  ggplot(aes(x=age_bin, y=m))+
  geom_pointrange(aes(ymin=m-se, ymax=m+se))

cal1020 %>% 
  mutate(Age2 = Age^2) %>% 
  ggplot(aes(Age, Age2))+
  geom_point()

cal1020 <- cal1020 %>% 
  filter(!is.na(Age))


poly(cal1020$Age, degree=2) %>% 
  as_tibble() %>% 
  ggplot(aes(x=`1`, y=`2`))+
  geom_point()

poly(cal1020$Age, degree=2) %>% 
  as_tibble() %>% 
  rename(age1 = `1`, age2=`2`) %>% 
  summarize_all(.funs = list(mean='mean', sd='sd'))


cal1020 <- poly(cal1020$Age, degree=2) %>% 
  as_tibble() %>% 
  rename(age1 = `1`, age2=`2`) %>% 
  bind_cols(cal1020) 

M = cal1020 %>% 
  lm(data = ., speed.mph ~ age1 + age2)

cal1020$prediction = predict(M, newdata = cal1020)


cal1020 %>% 
  mutate(age_bin = floor(Age/5)*5) %>% 
  group_by(age_bin) %>% 
  summarize(m = mean(speed.mph), se = sd(speed.mph)/sqrt(n())) %>% 
  ggplot(aes(x=age_bin, y=m))+
  geom_pointrange(aes(ymin=m-se, ymax=m+se))+
  geom_line(data = cal1020, aes(x=Age, y=prediction), color='red')
# higher order polynomial

cal1020_lean <- poly(cal1020$Age, degree=5) %>% 
  as_tibble() %>% 
  bind_cols(cal1020 %>% select(Age, speed.mph))

M2 <- lm(data = cal1020_lean, speed.mph ~ `1`+`2`+`3`+`4`+`5`)

cal1020_lean$prediction = predict(M2, cal1020_lean)

cal1020_lean %>% 
  mutate(age_bin = floor(Age/5)*5) %>% 
  group_by(age_bin) %>% 
  summarize(m = mean(speed.mph), se = sd(speed.mph)/sqrt(n())) %>% 
  ggplot(aes(x=age_bin, y=m))+
  geom_pointrange(aes(ymin=m-se, ymax=m+se))+
  geom_line(data = cal1020_lean, aes(x=Age, y=prediction), color='red')

# 20th order polynomial


cal1020_poly20 <- poly(cal1020$Age, degree=20) %>% 
  as_tibble() %>% 
  bind_cols(cal1020 %>% select(Age, speed.mph))

M20 <- lm(data = cal1020_poly20, speed.mph ~ . -Age)

cal1020_poly20$prediction = predict(M20, cal1020_poly20)

cal1020_poly20 %>% 
  mutate(age_bin = floor(Age/5)*5+2.5) %>% 
  group_by(age_bin) %>% 
  summarize(m = mean(speed.mph), se = sd(speed.mph)/sqrt(n())) %>% 
  ggplot(aes(x=age_bin, y=m))+
  geom_pointrange(aes(ymin=m-se, ymax=m+se))+
  geom_line(data = cal1020_poly20, aes(x=Age, y=prediction), color='red')

#  
library(glmnet)
M <- glmnet(x = cal1020_poly20 %>% 
              select(-Age, -speed.mph, -prediction) %>% 
              as.matrix(),
       y = cal1020_poly20$speed.mph,
       alpha = 0.5)

M$beta

cvob <- cv.glmnet(x = cal1020_poly20 %>% 
              select(-Age, -speed.mph, -prediction) %>% 
              as.matrix(),
            y = cal1020_poly20$speed.mph,
            alpha = 0.5)
lambda.1se = cvob$lambda.1se
which(M$lambda == cvob$lambda.1se)

M$beta[,45]

cal1020_poly20$prediction = predict.glmnet(M, 
               newx = cal1020_poly20 %>% 
                 select(-Age, -speed.mph, -prediction, -predict) %>% 
                 as.matrix(), 
               s = cvob$lambda.1se)

cal1020_poly20 %>% 
  mutate(age_bin = floor(Age/5)*5+2.5) %>% 
  group_by(age_bin) %>% 
  summarize(m = mean(speed.mph), se = sd(speed.mph)/sqrt(n())) %>% 
  ggplot(aes(x=age_bin, y=m))+
  geom_pointrange(aes(ymin=m-se, ymax=m+se))+
  geom_line(data = cal1020_poly20, aes(x=Age, y=prediction), color='red')



cal1020_poly20$prediction = predict.glmnet(M, 
                                           newx = cal1020_poly20 %>% 
                                             select(-Age, -speed.mph, -prediction, -predict) %>% 
                                             as.matrix(), 
                                           s = cvob$lambda.1se)

cal1020_poly20 %>% 
  ggplot(aes(x=Age, y=speed.mph))+
  geom_point(size=0.1, alpha=0.2)+
  geom_line(data = cal1020_poly20, aes(x=Age, y=prediction), color='red')



predictors <- poly(cal1020$Age, 5)

newAge = 40

poly(newAge, 5, coefs = attr(predictors, 'coefs'))

cal1020_poly20 %>% 
  ggplot(aes(x=Age, y=`1`))+
  geom_point()





df <- tibble(x = seq(-6, 6, by=0.01)) %>% 
  mutate(y = 0.5*(x-2)^2)

df %>% ggplot(aes(x=x, y=y))+geom_point()

df <- bind_cols(df, 
          poly(df$x, 2) %>% as_tibble())

M = lm(data = df,  y ~ `1` + `2`)

df$prediction = predict(M, df)

df %>% ggplot(aes(x=x, y=y))+geom_point()+
  geom_line(aes(y=prediction), color='red')

M = lm(data = df,  y ~ `2`)

df$prediction = predict(M, df)

df %>% ggplot(aes(x=x, y=y))+geom_point()+
  geom_line(aes(y=prediction), color='red')

M = lm(data = df,  y ~ `2`)

df$x.22 = (df$x-2)^2

M = lm(data = df,  y ~ x.22)

df$prediction = predict(M, df)

df %>% ggplot(aes(x=x, y=y))+geom_point()+
  geom_line(aes(y=prediction), color='red')


# bayes factor between two models

# P(data | m1) / P(data | m0)

bs1 = bridgesampling::bridge_sampler(fit1)

# BF = P(data | m1) / P(data | m0)
bs1$logml  # log( P(data | m1) )

bs2 = bridgesampling::bridge_sampler(fit2)
bs2$logml  # log( P(data | m2) )

# P(data | m1) / P(data | m2)
# log ( BF ) = 
# log ( P(data | m1) / P(data | m2) ) = 
# log( P(data | m1) ) - log( P(data | m2) )
bs1$logml - bs2$logml # log (BF)
exp( bs1$logml - bs2$logml )  # BF
exp( bs2$logml - bs1$logml )  # BF



