library(tidyverse)
library(stats4)
# anova-like likelihood

# Generating fake data.
n = 25
grps = 3
m = rnorm(grps)
s = 2
data = data.frame(grp = sample(1:grps, n, replace=T)) %>% 
  mutate(y = rnorm(n(), m[grp], s),
         grp = letters[grp])

glimpse(data)

nll = function(ma,mb,mc,s){
  means = c(a=ma, b=mb, c=mc)
  M = means[data$grp]
  likelihood = dnorm(data$y, M, s)
  ll = sum(log(likelihood))
  return(-1*ll)
}

fit = mle(nll, start = list(ma=0, mb=0, mc=0, s=1),
          lower = c(-Inf, -Inf, -Inf, 0))

mb.est = coef(summary(fit))[2,1]
mb.se =  coef(summary(fit))[2,2]

qnorm(c(0.025, 0.975))
mb.est + qnorm(c(0.025, 0.975))*mb.se


## null model
nll.0 = function(mall,s){
  likelihood = dnorm(data$y, mall, s)
  ll = sum(log(likelihood))
  return(-1*ll)
}

fit.0 = mle(nll.0, start = list(mall=0, s=1),
          lower = c(-Inf, 0))

# likelihood ratio test.
D = summary(fit.0)@m2logL - summary(fit)@m2logL
1 - pchisq(D, 2)



# moon

data <- suncalc::getMoonIllumination(date = Sys.Date()-(1:100)) %>%
  mutate(fraction = rbeta(n(), fraction*50, (1-fraction)*50)) %>% 
  mutate(day = 1:n()) %>% 
  select(day, fraction)

data %>% ggplot(aes(x=day, y=fraction))+geom_point()

## Try this with a normal likelihood:
# s = sd of normal
nll = function(period, phase, s){
  m = 0.5+0.5*sin(data$day*1/period*2*pi + phase)
  # add the line below to spit out evaluation details for debugging.
  # cat(c(period, phase, s), '\n')
  -sum(dnorm(data$fraction, m, s, log=T))
}

# this throws an error of suggesting that we end up evaluating an illegal parameter
init = list(period = 5, phase = 0, s = 0.1)
fit <- mle(nll, start = init,
           lower = c(1e-9, -pi, 0), upper = c(Inf, pi,Inf))

# let's make s=0 out of bounds.
# this yields an answer
fit <- mle(nll, start = init,
           lower = c(1e-9, -pi, 1e-9), upper = c(Inf, pi,Inf))
# but it is not a particularly good one
summary(fit)
Cs = summary(fit) %>% coef()
data %>% 
  ggplot(aes(x=day, y=fraction))+
  geom_point()+
  geom_line(aes(y = 0.5+0.5*sin(day*1/Cs['period',1]*2*pi + Cs['phase',1])), color='red')

# why does it not yield a good answer?  Let's look at the likelihood surface
nll_grid <- expand.grid(period = 1:100, phase = seq(0, 2*pi, length.out = 40)) %>% 
  mutate(nll= purrr::map2_dbl(period, 
                              phase, 
                              function(period, phase){
                                nll(period, phase, 0.1)}))
nll_grid %>% 
  ggplot(aes(x=period, y=phase, fill=nll))+
  geom_raster()+
  annotate('text', x = Cs['period', 1], y=Cs['phase',1]%%(2*pi), color='red', label='end')+
  annotate('text', x = init$period, y=init$phase, color='red', label='start')
# A few things of note here:
# 1. we got somewhere, relative to our start... but not to a very good point.
# 2. we can find a decent fit fairly easily with grid search

grid_max <- nll_grid %>% filter(nll == min(nll)) 

data %>% 
  ggplot(aes(x=day, y=fraction))+
  geom_point()+
  geom_line(aes(y = 0.5+0.5*sin(day*1/grid_max$period*2*pi + grid_max$phase)), color='red')


# If we instead start somewhere else, we may do better
init = list(period = 60, phase = pi, s = 0.1)
fit <- mle(nll, start = init,
           lower = c(1e-9, -pi, 1e-9), upper = c(Inf, pi,Inf))
Cs = summary(fit) %>% coef()
data %>% 
  ggplot(aes(x=day, y=fraction))+
  geom_point()+
  geom_line(aes(y = 0.5+0.5*sin(day*1/Cs['period',1]*2*pi + Cs['phase',1])), color='red')
nll_grid %>% 
  ggplot(aes(x=period, y=phase, fill=nll))+
  geom_raster()+
  annotate('text', x = Cs['period', 1], y=Cs['phase',1]%%(2*pi), color='red', label='end')+
  annotate('text', x = init$period, y=init$phase, color='red', label='start')
# this gets somewhere else.... but not quite to the best fit.  

# if we start with a pretty good guess 
# (such that the local maximum is the global max), 
# we get optimize locally

init = list(period = 40, phase = pi, s = 0.1)
fit <- mle(nll, start = init,
           lower = c(1e-9, -pi, 1e-9), upper = c(Inf, pi,Inf))
Cs = summary(fit) %>% coef()
data %>% 
  ggplot(aes(x=day, y=fraction))+
  geom_point()+
  geom_line(aes(y = 0.5+0.5*sin(day*1/Cs['period',1]*2*pi + Cs['phase',1])), color='red')
nll_grid %>% 
  ggplot(aes(x=period, y=phase, fill=nll))+
  geom_raster()+
  annotate('text', x = Cs['period', 1], y=Cs['phase',1]%%(2*pi), color='red', label='end')+
  annotate('text', x = init$period, y=init$phase, color='red', label='start')


# in general -- this likelihood surface is very multimodal.  
# most optimization algorithms will fail
# there are *stochastic* search algorithms that try to deal with this
# or you can just do a grid search.