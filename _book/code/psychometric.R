n = 10000
ratio = 10^runif(n,-3,3)
p = 0.05+0.9*pnorm(log10(ratio), 0, 0.5)
response = rbinom(n, 1, p)
df <- data.frame(ratio = ratio, correct = response)

library(tidyverse)

df %>% group_by(ratio = 10^(round(log10(ratio)*20)/20)) %>%
  summarize(p.correct = mean(correct)) %>%
  ggplot(aes(x=log10(ratio), y=p.correct))+geom_point()

m <- glm(data = df, family=binomial, response ~ log10(ratio))
logistic = function(x){1/(1+exp(-x))}

model.fit = data.frame(ratio = 10^seq(-3,3,by=0.001)) %>%
  mutate(prob = logistic(log10(ratio)*coef(m)[2]+coef(m)[1]))
  
df %>% group_by(ratio = 10^(round(log10(ratio)*20)/20)) %>%
  summarize(p.correct = mean(correct)) %>%
  ggplot(aes(x=log10(ratio), y=p.correct))+geom_point()+
  geom_line(data=model.fit, aes(x=log10(ratio), y=prob), color='red')


library(stats4)

loglik = function(m,s){
  log.odds = (log10(df$ratio)-m)/s
  return(-sum(log(dbinom(df$correct, 1, logistic(log.odds)))))
}

m2 <- mle(loglik, start = list('m'=0, 's'=1))

model.fit = data.frame(ratio = 10^seq(-3,3,by=0.001)) %>%
  mutate(prob = logistic((log10(ratio)-coef(m)[1])*coef(m)[2]))

loglik = function(m,s, logit.lapse){
  lapse = logistic(logit.lapse)
  log.odds = (log10(df$ratio)-m)/s
  probability = lapse/2+(1-lapse)*logistic(log.odds)
  log.likelihood = sum(log(dbinom(df$correct, 1, probability)))
  return(-log.likelihood)
}


m2 <- mle(loglik, start = list('m'=0, 's'=1, 'logit.lapse'=0))
summary(m2)

location = coef(m2)[1]
scale = coef(m2)[2]
lapse = logistic(coef(m2)[3])

model.fit = data.frame(ratio = 10^seq(-3,3,by=0.001)) %>%
  mutate(prob = lapse/2+(1-lapse)*logistic(
    (log10(ratio)-location)/scale))



dat <- data.frame(error = c(rnorm(1000,0,20), runif(500, -180,180)))

dat %>% ggplot(aes(x=error))+
  geom_histogram(binwidth=5, color='black', fill='blue')

loglik = function(m,s){
  return(-sum(log(dnorm(dat$error, m, pmax(0.0001, exp(s))))))
}

m <- mle(loglik, start=list('m'=0, 's'=4))

model.fit = data.frame(error = seq(-180,180,by=1)) %>%
  mutate(prob = dnorm(error, coef(m)[1], exp(coef(m)[2]))) 

dat %>% ggplot(aes(x=error))+
  geom_histogram(binwidth=5, color='black', fill='blue')+
  geom_line(data=model.fit, 
            aes(x=error, y=prob*nrow(dat)*5), 
            color='red', size=1.5)

loglik = function(m,s,logit.p){
  p = logistic(logit.p)
  m = m
  s = pmax(0.0001, exp(s))
  return(-sum(log(p/360+(1-p)*dnorm(dat$error, m, s))))
}

m2 <- mle(loglik, start=list('m'=0, 's'=4, 'logit.p'=0))

m = coef(m2)[1]
s = exp(coef(m2)[2])
p = logistic(coef(m2)[3])
model.fit = data.frame(error = seq(-180,180,by=1)) %>%
  mutate(prob = p/360 + (1-p)*dnorm(error, m, s)) 
