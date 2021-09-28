height = round(rnorm(1000, 70, 4),0)
logodds = height * 0.5 - 38
p = logistic(logodds)
recruited = rbinom(length(p), 1, p)
recruited


dat <- data.frame(height, recruited) %>% 
  group_by(height) %>% 
  summarize(n = n(),
            recruited = sum(recruited))


dat %>% 
  print(n=40)

dat %>% 
  ggplot(aes(x=height, y=n))+
  geom_col()+
  geom_col(aes(y=recruited), fill='red')+
  theme_minimal()

glm(data = dat, 
    recruited ~ height, 
    family = binomial())


glm(data = dat, 
    cbind(recruited, n - recruited) ~ height, 
    family = binomial())
