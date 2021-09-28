library(stats4)

p = 0
a = -1
b = 1
s = 1
x = seq(-5, 5, by=0.01)
m = a + b * x

y = ifelse(runif(length(x))<p, 
           rnorm(length(x), 0, 100),
           rnorm(length(x), m, s))
plot(x,y)


logl = function(a, b, s){
  m = a + b*x
  ll = sum(dnorm(y, m, s, log=T))
  return(ll)
}

nlogl = function(a, b, s){
  m = a + b*x
  ll = sum(dnorm(y, m, s, log=T))
  return(-1*ll)
}

fit <- mle(nlogl, 
           start = c(a=0, b=0, s=1), 
           lower=list(s=0))

# create an outlier proces and thus a mixture

p = 0.1
a = -1
b = 1
s = 1
x = seq(-5, 5, by=0.01)
m = a + b * x

y = ifelse(runif(length(x))<p, 
           rnorm(length(x), 0, 100),
           rnorm(length(x), m, s))
plot(x,y)

nlogl = function(p, a, b, s){
  m = a + b*x
  likelihood = p*dnorm(y, 0, 100) + (1-p)*dnorm(y, m, s)
  ll = sum(log(likelihood))
  return(-1*ll)
}

fit <- mle(nlogl, 
           start = c(p = 0.5, a=0, b=0, s=1), 
           lower=list(p = 1e-6, s=0), upper = list(p=1-1e-6))

#

estimateds = c()

for(i in 1:1000){
  x = rnorm(5)
  nlogl = function(m, s){
    -sum(dnorm(x, m, s, log = T))
  }
  fit = mle(nlogl, start = c(m=0, s=1), lower = list(s=0.0001))
  estimateds[i] = summary(fit) %>% coef() %>% .[[2]]
  
  
}

sum((x - mean(x))^2) / length(x)

sum((x - mean(x))^2) / (length(x) - 1)

#  



hist(estimateds)

