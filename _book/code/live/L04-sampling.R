flip = function(){
  ifelse(runif(1)<0.5, 'H', 'T')
}

flip()
flip()
flip()

flip.n = function(n){
  replicate(n, flip())
}

flip.n(3)

evaluate.X = function(outcome){
  sum(outcome == 'H')
}

evaluate.X(flip.n(3))
evaluate.X(flip.n(3))
evaluate.X(flip.n(3))

samples.X = replicate(10000, evaluate.X(flip.n(3)))
hist(samples.X)

hist(rbinom(10000, 3, 0.5))
