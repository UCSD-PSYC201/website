library(tidyverse)

cards = rep(c(2:10, 'J', 'Q', 'K', 'A'), 4)

x = sample(cards, size = 3, replace = F)

get.card.values = function(x){
  case_when(
    x == 'K' ~ 10,
    x == 'Q' ~ 10,
    x == 'J' ~ 10,
    x == 'A' ~ 11,
    TRUE ~ as.numeric(x)
}
    
is.16 = function(x){
  values = case_when(
    x == 'K' ~ 10,
    x == 'Q' ~ 10,
    x == 'J' ~ 10,
    x == 'A' ~ 11,
    TRUE ~ as.numeric(x)
  )
  sum(values[1:2])==16
}

bust = function(x){
  values = case_when(
    x == 'K' ~ 10,
    x == 'Q' ~ 10,
    x == 'J' ~ 10,
    x == 'A' ~ 1,
    TRUE ~ as.numeric(x)
  )
  sum(values[1:3])>21
}

i.won = function(x){
  # dealer busts
  # i have more without busting.
  values = case_when(
    x == 'K' ~ 10,
    x == 'Q' ~ 10,
    x == 'J' ~ 10,
    x == 'A' ~ 1,
    TRUE ~ as.numeric(x)
  )
  mytotals = sum(values[1:3])
  n.aces= sum(values[1:3] == 1) # how many aces
  mytotals = (0:n.aces)*10 + mytotals
  my.best = max(mytotals[mytotals <= 21])
  
  dealertotals = sum(values[4:6])
  n.aces= sum(values[4:6] == 1) # how many aces
  dealertotals = (0:n.aces)*10 + dealertotals
  dealer.best = max(dealertotals[dealertotals <= 21])
  
  my.best > dealer.best

}

n = 100000
sixteen.me = rep(NA, n)
busted.me = rep(NA, n)
sixteen.dealer = rep(NA, n)
busted.dealer = rep(NA, n)
i.won.this.one = rep(NA, n)
for(i in 1:n){
  x = sample(cards, 6, replace=T)
  sixteen.me[i] = is.16(x[1:3])
  busted.me[i] = bust(x[1:3])
  sixteen.dealer[i] = is.16(x[4:6])
  busted.dealer[i] = bust(x[4:6])
  i.won.this.one[i] = i.won(x)
}

