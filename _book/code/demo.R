library(ggplot2)
mu_Y = 100
sigma_Y = 15
n = 100

y = mu_Y + 
  rnorm(n, 0, sigma_Y)

ggplot(data.frame(y), aes(y))+geom_histogram()
y = round(y)

median(y)
quantile(y, c(0.25, 0.75))
IQR(y)
mean(y)



sd(y)
summary(lm(y~1))

( sem = sd(y) / sqrt(length(y)) )

( t = (mean(y) - 100) / (sd(y) / sqrt(length(y))) )


t = function(x){(mean(x)-100)/(sd(x)/sqrt(length(x)))}
ggplot(data.frame(error.mean.y=replicate(100000, mean(rnorm(n, mu_Y, sigma_Y))-100)),
       aes(error.mean.y))+geom_density(fill='blue')

save(y, file='y.rdata')
