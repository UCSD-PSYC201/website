library(tidyverse)
library(stats4)
n = 50
b0 = -100
b1 = 1

df <- data.frame(x=runif(n, 50,150)) %>%
  mutate(y = x*b1+b0+rnorm(n,0,15)+
           rbinom(n,1,0.2)*
           sample(c(-1,1), n, replace=T)*
           runif(n,50,150))
df %>% ggplot(aes(x,y))+
  geom_point()+
  geom_smooth(method='lm')

nlogLikelihood = function(b0,b1,log.sigma){
  mu = b0+b1*df$x
  residual = df$y-mu
  sigma = exp(log.sigma)
  -sum(dnorm(residual, 0, sigma, log=T))
}

m1 <- mle(nlogLikelihood, start=list(b0 = 0, b1=1, log.sigma=1))
ch
logistic = function(x){1/(1+exp(-x))}

nlogLikelihood = function(b0,b1,log.sigma, logit.p.outlier){
  mu = b0+b1*df$x
  residual = df$y-mu
  sigma = exp(log.sigma)
  sigma.outlier = sd(df$y)
  p.outlier = logistic(logit.p.outlier)
  prob = p.outlier*dnorm(residual, 0, sigma.outlier)+
        (1-p.outlier)*dnorm(residual, 0, sigma)
  -sum(log(prob))
}

m2 <- mle(nlogLikelihood, start=list(b0 = 0, b1=0, log.sigma=1, logit.p.outlier=0))

nlogLikelihood = function(b0,b1,log.sigma,log.nu){
  mu = b0+b1*df$x
  residual = df$y-mu
  sigma = exp(log.sigma)
  z.residual = residual/sigma
  nu = exp(log.nu)+1
  -sum(dt(z.residual, df=nu, log=T))
}

m3 <- mle(nlogLikelihood, 
          start=list(b0 = 0, b1=1, log.nu=0))

