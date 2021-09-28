# generate some random effect structure

# donut tastiness across supermarkets
library(tidyverse)

n.supermarkets = 5
n.donuttypes = 4
n.tasters = 10

tasters = paste0('T', 1:n.tasters)
donuttypes = c('jelly', 'old', 'sprinkles', 'crauller')
supermarkets = c('vons', 'safeway', 'ralphs', 'food4less', 'wholefoods')


# random effects
ef.taster = rnorm(n.tasters, 0, 1)
names(ef.taster) = tasters
ef.donuttype = c('jelly'=+0, 'old'=-0, 'sprinkles'=0, 'crauller'=0)
ef.supermarket = rnorm(n.supermarkets, 0, 1)
names(ef.supermarket) <- supermarkets

ef.taster.donuttype = matrix(rnorm(n.tasters * n.donuttypes, 0, 0.5), 
                             ncol=n.donuttypes,
                             dimnames = list(tasters, donuttypes))

ef.supermarket.donuttype = matrix(rnorm(n.supermarkets * n.donuttypes, 0, 0.1), 
                             ncol=n.donuttypes,
                             dimnames = list(supermarkets, donuttypes))


ef.taster.supermarket = matrix(rnorm(n.tasters * n.supermarkets, 0, 0.5), 
                             ncol=n.supermarkets,
                             dimnames = list(tasters, supermarkets))

dat <- expand.grid(taster = tasters, 
                   donuttype = donuttypes, 
                   supermarket = supermarkets)

dat <- dat %>% 
  mutate(y = ef.taster[taster] + 
           ef.donuttype[donuttype] + 
           ef.supermarket[supermarket] +
           ef.taster.donuttype[cbind(taster, donuttype)] + 
           ef.taster.supermarket[cbind(taster, supermarket)] + 
           ef.supermarket.donuttype[cbind(supermarket, donuttype)] + 
           rnorm(n(), 0, 0.25))

library(lmerTest)

lm(data = dat, y ~ donuttype) %>% anova()

M.intercept <- lmer(data = dat, y ~ donuttype*supermarket + 
            (1|taster) + 
            (1|taster:donuttype) +
            (1 | taster:supermarket))

M.slope <- lmer(data = dat, y ~ donuttype*supermarket + 
            (donuttype + supermarket|taster))
anova(M.slope)
anova(M.intercept)


M.3 <- lmer(data = dat, y ~ donuttype + 
                      (1|taster) + 
                      (1|taster:donuttype) +
                      (1 | taster:supermarket) +
                      (1|supermarket) +
                      (1|supermarket:donuttype))

ranef(M.3)
fixef(M.intercept)
