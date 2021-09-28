x = rnorm(5)

dat = expand.grid(m = seq(-4, 4, length.out=1000), v=exp(seq(log(0.1), log(10), length.out = 1000)))

dat$ll = purrr::map2_dbl(dat$m, dat$v, function(m,v){sum(dnorm(x, m, sqrt(v), log=T))})

dat %>% mutate(p = exp(ll)) %>% 
  mutate(p = p/sum(p)) %>% 
  group_by(v) %>% 
  summarize(p = sum(p)) %>% 
  ggplot(aes(x=v, y=p))+
  geom_line()+
  geom_vline(xintercept = 1, color='red')+
  theme_minimal()


dat %>% 
  ggplot(aes(x=m, y=log(v), fill=log(-ll)))+
  geom_raster()+
  theme_minimal()


dat %>% mutate(p = exp(ll)) %>% 
  mutate(p = p/sum(p)) %>% 
  group_by(v) %>% 
  summarize(p = sum(p)) %>% 
  filter(p == max(p))


library(stats4)

x = rnorm(5)

negll = function(m,v){
  -sum(dnorm(x,m,sqrt(v), log=T))
}

fit = mle(negll, start=list(m=0, v=1), lower=list(v=0))
summary(fit)
sum((x - mean(x))^2)/length(x)
