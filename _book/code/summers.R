rm(list=ls())
library(ggplot2)

# from US Reading scores from Machin & Pekkarinen (2008)
mean.m.minus.f = -0.32 # in units of standard deviation
var.m.div.f = 1.17 # variance ratio m/f

mu.m = 0 
sd.m = 1
mu.f = mu.m - mean.m.minus.f*sd.m
sd.f = sqrt(sd.m^2 / var.m.div.f)
# an alternate formulation:
# mu.f = 0
# sd.f = 1
# mu.m = mu.f + mean.m.minus.f*sd.f
# sd.m = sqrt(var.m.div.f * sd.f^2)
# note that these effectively have a different discrepancy between
# m and f means because they use different sds (m or f) to define it.
# here it doesn't really matter.  If we wanted to be really careful,
# we would use a "pooled" sd to define the mean difference:
# sd.f = 1
# sd.m = sqrt(var.m.div.f * sd.f^2)
# sd.pooled = sqrt(0.5*sd.f^2 + 0.5*sd.m^2)
# mu.m = mu.f + mean.m.minus.f*sd.pooled
# but it really doesn't matter for the present purposes.

# plots to illustrate what is happening
crit = 2  # criterion used for graph.
illustration.df = data.frame(x = seq(-4,4,by=0.01)) # possible score values (standardized units)
illustration.df$p.x.m = dnorm(illustration.df$x, mu.m, sd.m) #P(x|m)
illustration.df$p.x.f = dnorm(illustration.df$x, mu.f, sd.f) #P(x|f)
illustration.df$above.crit = ifelse(illustration.df$x>=crit, 'above', 'below') # x>crit?

ggplot(illustration.df, aes(x=x,y=p.x.m, fill=above.crit))+
  geom_area(alpha=0.2)+
  geom_line(color='blue')+
  geom_area(aes(y=p.x.f), alpha=0.2)+
  geom_line(aes(y=p.x.f), color='red')+
  geom_vline(xintercept = crit, color="black")+
  theme_bw()+
  theme(legend.position = 'None')+
  ylab('P(x|sex)')+
  xlab('x (in standardized units)')


# how to find criterion that gets us to P(m|x>crit)=0.8?
# in class, I suggested writing functions, like so:
p.gt_crit.m = function(crit){1-pnorm(crit, mu.m, sd.m)}
p.gt_crit.f = function(crit){1-pnorm(crit, mu.f, sd.f)}
p.m = 0.5
p.gt_crit = function(crit){p.m*p.gt_crit.m(crit) + (1-p.m)*p.gt_crit.f(crit)}
p.m.gt_crit = function(crit){p.m*p.gt_crit.m(crit) / p.gt_crit(crit)}

# at this point we could try various values like 
p.m.gt_crit(4)
# until we find one that's roughly 0.8.

# Another strategy would be to define a whole data frame with many possible
# criteria, and for each one, calculate all the probabilities
# this is better for making a picture of what's going on:

prob.df = data.frame(crits = seq(-3,7,0.001))
prob.df$p.m = 0.5
prob.df$p.gt_crit.m = 1-pnorm(prob.df$crits, mu.m, sd.m)
prob.df$p.gt_crit.f = 1-pnorm(prob.df$crits, mu.f, sd.f)
prob.df$p.gt_crit = prob.df$p.m*prob.df$p.gt_crit.m + (1-prob.df$p.m)*prob.df$p.gt_crit.f
prob.df$p.m.gt_crit = prob.df$p.m*prob.df$p.gt_crit.m / prob.df$p.gt_crit

# we can find the row in the data frame that has p(m|x>crit)=~0.8, by finding
# the value where (p(m|x>crit)-0.8)^2 is smallest:
idx = which.min((prob.df$p.m.gt_crit-0.8)^2)
# here is the corresponding row, showing the criterion, and probabilities associated with it.
prob.df[idx,]

# let's look at what's going on here.

ggplot(prob.df, aes(x=crits, y=p.gt_crit.m))+
  geom_line(color="blue", size=1.5)+
  geom_line(aes(y=p.gt_crit.f), color="red", size=1.5)+
  theme_bw()+
  xlab('crit')+
  ylab('P( x>crit | sex )')+
  geom_vline(xintercept = prob.df$crits[idx])+
  scale_x_continuous(breaks=seq(-3,8,by=1))+
  coord_cartesian(xlim=c(-2.5,6.5))

ggplot(prob.df, aes(x=crits, y=p.gt_crit.m))+
  geom_line(color="blue", size=1.5)+
  geom_line(aes(y=p.gt_crit.f), color="red", size=1.5)+
  theme_bw()+
  xlab('crit')+
  ylab('log[P( x>crit | sex )]')+
  geom_vline(xintercept = prob.df$crits[idx])+
  scale_x_continuous(breaks=seq(-3,8,by=1))+
  coord_cartesian(xlim=c(-2.5,6.5))+
  scale_y_log10()

  
ggplot(prob.df, aes(x=crits, y=p.m.gt_crit))+
  geom_line(color="blue", size=1.5)+
  geom_line(aes(y=(1-p.m.gt_crit)), color="red", size=1.5)+
  theme_bw()+
  xlab('crit')+
  ylab('P( sex | x>crit )')+
  geom_vline(xintercept = prob.df$crits[idx])+
  scale_x_continuous(breaks=seq(-3,8,by=1))+
  coord_cartesian(xlim=c(-2.5,6.5))


prob.df[idx,]
# so criterion is about 6.69 (which is "standard deviations above the male mean".)
# is this "plausible"?
# well, how many people are past this criterion?
prob.df$p.gt_crit[idx]*7400000000 #7.4 billion people in the world
# 5/100th of a person.  
# your answer may differ numerically depending on whether you set male or female scores as the standard
# and how precisely you estimate the criterion to get P(male | x>crit) = 0.8
# the rough result (of having an impossibly low baserate of P(x>crit)) will not change

