# example: bernoulli
## sampling
### flip a fair coin once. H=1, T=0
rbernoulli.fair = function(n){
  sample(c(1, 0), 
        size = n,
        replace = T,
        prob = c(0.5, 0.5))
}

### flip a Prob(H)=p coin. H=1, T=0
rbernoulli = function(n, p){
  sample(c(1, 0), 
         size = n,
         replace = T,
         prob = c(p, 1-p))
}

## probability distribution function
dbernoulli = function(x, p, log=FALSE){
  if(!all(x %in% c(1, 0))){
    stop('all x must be 0 or 1')
  }
  p = ifelse(x == 1, p, (1-p))
  if(log){
    return(log(p))
  } else {
    return(p)
  }
}

## cumulative distribution function
pbernoulli = function(x, p, log=FALSE){
  if(!all(x %in% c(1, 0))){
    stop('all x must be 0 or 1')
  }
  p = ifelse(x == 0, (1-p), 1)
  if(log){
    return(log(p))
  } else {
    return(p)
  }
}

## quantile function


## example: binomial

sample.binomial.3 = function(){
  outcomes = sample(c('H', 'T'), 
                    3, 
                    replace=T, 
                    prob=c(0.5, 0.5))
  sum(outcomes == 'H')
}


sample.binomial.3p = function(p){
  outcomes = sample(c('H', 'T'), 
                    3, 
                    replace=T, 
                    prob=c(p, (1-p)))
  sum(outcomes == 'H')
}

sample.binomial = function(size, p){
  outcomes = sample(c('H', 'T'), 
                    size, 
                    replace=T, 
                    prob=c(p, (1-p)))
  sum(outcomes == 'H')
}

replicate(n = 10000, sample.binomial(10, 0.6)) %>% 
  data.frame(x = .) %>% 
  ggplot(aes(x))+
  geom_histogram(binwidth = 0.5)



## example: bernoulli

flip.coin = function(p){
  sample(c('H','T'), 
         size=1, 
         replace=T, 
         prob=c(p, 1-p))
}

# sampling process
rflip = function(n, p){
}

# probability distribution function
dflip = function(x, p, log=FALSE){
  if(!all(x %in% c('H','T'))){
    stop('all x must be 0 or 1')
  }
  p = ifelse(x == 1, p, (1-p))
  if(log){
    return(log(p))
  } else {
    return(p)
  }
}






