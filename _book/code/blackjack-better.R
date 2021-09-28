library(tidyverse)

# this is a more efficient, and more general approach to the blackjack problems.

cards = rep(c(2:10, 10, 10, 10, 1), 4) # coding values from the getgo

# this function gets the best possible total out of the values
# variation due to interpretation of aces (coded as value=1)
best.total = function(values){
  possible = sum(values) + 10*(0:sum(values == 1))
  max(possible[possible <= 21])
}

# lots of simulations...
n = 1000000
my.best.2 = rep(NA, n)
my.best.3 = rep(NA, n)
dealer.best.2 = rep(NA, n)
dealer.best.3 = rep(NA, n)
for(i in 1:n){
  values = sample(cards, 15, replace=T)
  my.best.2[i] = best.total(values[1:2])
  dealer.best.2[i] = best.total(values[4:5])
  my.best.3[i] = best.total(values[1:3])
  dealer.best.3[i] = best.total(values[4:6])
}

# get probability of winning at 16-16, with and without hitting
data.frame(my.best.2, 
           dealer.best.2,
           my.best.3,
           dealer.best.final) %>% 
  filter(my.best.2 == 16, dealer.best.2 == 16) %>% 
  mutate(i.won.3 = my.best.3 > dealer.best.3,
         i.won.2 = my.best.2 > dealer.best.3) %>% 
  summarize(mean(i.won.3), mean(i.won.2))

# instead, evaluate for every possible combination of my count and dealer count
outcome.summary <- data.frame(my.best.2, 
                              dealer.best.2,
                              my.best.3,
                              dealer.best.3) %>% 
  group_by(my.best.2, dealer.best.2) %>% 
  # this next part takes dealers best of 3 if best of 2 is <=16, otherwise best of 2
  mutate(i.won.3 = my.best.3 > ifelse(dealer.best.2 > 16, dealer.best.2, dealer.best.3),
         i.won.2 = my.best.2 > ifelse(dealer.best.2 > 16, dealer.best.2, dealer.best.3)) %>% 
  summarize(p.won.3 = mean(i.won.3), 
            p.won.2 = mean(i.won.2), 
            n = n())

# let's make a plot of... sample size
outcome.summary %>% 
  ggplot(aes(x=my.best.2, y=dealer.best.2, fill=log10(n)))+
  geom_tile()

# let's make a plot of... win probability without hitting.
outcome.summary %>% 
  ggplot(aes(x=my.best.2, y=dealer.best.2, fill=p.won.3))+
  geom_tile()

outcome.summary %>% 
  ggplot(aes(x=my.best.2, y=dealer.best.2, fill=p.won.3-p.won.2))+
  geom_tile()+
  scale_fill_gradient2(low = 'darkred', high='darkgreen')


# this highlights a problem... you and the dealer might hit multiple times...

n = 1000000
my.best.2 = rep(NA, n)
my.best.3 = rep(NA, n)
dealer.best.2 = rep(NA, n)
dealer.best.final = rep(NA, n)
for(i in 1:n){
  values = sample(cards, 15, replace=T)
  my.best.2[i] = best.total(values[1:2])
  dealer.best.2[i] = best.total(values[4:5])
  my.best.3[i] = best.total(values[1:3])
  dealer.cards = 2
  d.best = best.total(values[(1:dealer.cards)+3])
  while(d.best < 16 & d.best > 0){
    dealer.cards = dealer.cards + 1
    d.best = best.total(values[(1:dealer.cards)+3])
  }
  dealer.best.final[i] = d.best
}

outcome.summary <- data.frame(my.best.2, 
                              dealer.best.2,
                              my.best.3,
                              dealer.best.final) %>% 
  group_by(my.best.2, dealer.best.2) %>% 
  mutate(i.won.3 = my.best.3 > ifelse(dealer.best.2 > 16, dealer.best.2, dealer.best.final),
         i.won.2 = my.best.2 > ifelse(dealer.best.2 > 16, dealer.best.2, dealer.best.final)) %>% 
  summarize(p.won.3 = mean(i.won.3), 
            p.won.2 = mean(i.won.2), 
            n = n())

outcome.summary %>% 
  ggplot(aes(x=my.best.2, y=dealer.best.2, fill=p.won.3-p.won.2))+
  geom_tile()+
  scale_fill_gradient2(low = 'darkred', high='darkgreen')

# should I hit?
outcome.summary %>% 
  ggplot(aes(x=my.best.2, y=dealer.best.2, fill=sign(p.won.3-p.won.2)))+
  geom_tile()

# as a function of what the dealer has, what should my decision threshold be?
outcome.summary %>% 
  mutate(should.hit = (p.won.3-p.won.2)>=0) %>% 
  group_by(dealer.best.2) %>% 
  summarize(max.hit.threshold = max(my.best.2[should.hit]))

# finally, let's simulate the policy of
# "hit while my total is < max(16, dealer.2)"

n = 1000000
my.best.2 = rep(NA, n)
my.best.final = rep(NA, n)
dealer.best.2 = rep(NA, n)
dealer.best.final = rep(NA, n)
for(i in 1:n){
  values = sample(cards, 52, replace=T)
  my.best.2[i] = best.total(values[1:2])
  dealer.best.2[i] = best.total(values[(1:2)+25])
  my.cards = 2
  my.best = best.total(values[(1:my.cards)+0])
  while(my.best <= max(16, dealer.best.2[i]) & my.best > 0){
    my.cards = my.cards + 1
    my.best = best.total(values[(1:my.cards)+0])
  }
  my.best.final[i] = my.best
  
  dealer.cards = 2
  d.best = best.total(values[(1:dealer.cards)+25])
  while(d.best <= 16 & d.best > 0){
    dealer.cards = dealer.cards + 1
    d.best = best.total(values[(1:dealer.cards)+25])
  }
  dealer.best.final[i] = d.best
}

# overall win probability:
data.frame(my.best.2, 
           dealer.best.2,
           my.best.final,
           dealer.best.final) %>% 
  mutate(i.won = my.best.final > ifelse(dealer.best.2 > 16, dealer.best.2, dealer.best.final)) %>% 
  summarize(p.won = mean(i.won),  
            n = n())


data.frame(my.best.2, 
           dealer.best.2,
           my.best.final,
           dealer.best.final) %>% 
  mutate(i.won = my.best.final > ifelse(dealer.best.2 > 16, dealer.best.2, dealer.best.final)) %>% 
  group_by(my.best.2, dealer.best.2) %>% 
  summarize(p.won = mean(i.won),  
            n = n()) %>% 
  ggplot(aes(x=my.best.2, y=dealer.best.2, fill=p.won))+
  scale_fill_gradient2(midpoint = 0.5) +
  geom_tile()


outcome.summary <- data.frame(my.best.2, 
                              dealer.best.2,
                              my.best.final,
                              dealer.best.final) %>% 
  group_by(my.best.2, dealer.best.2) %>% 
  mutate(i.won = my.best.final > ifelse(dealer.best.2 > 16, dealer.best.2, dealer.best.final)) %>% 
  summarize(p.won = mean(i.won),  
            n = n())
