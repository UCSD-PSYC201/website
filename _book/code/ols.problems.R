library(tidyverse)

load(url('http://vulstats.ucsd.edu/data/cal1020.cleaned.Rdata'))

# speed ~ age 
# correlation
cor(cal1020$speed.mph, cal1020$age)
# covariance
cov(cal1020$speed.mph, cal1020$age)
# slope
m <- lm(data=cal1020, speed.mph ~ age)
summary(m)
#significant? (at 0.05)
coef(summary(m))['age', 'Pr(>|t|)'] < 0.05

# for corral, just substitute age for corral in the above.

new.data = data.frame(age=60)

# predict mean of 60yos
predict.lm(m, newdata = new.data, interval = 'confidence', level = 0.95)
# confidence interval is [lwr, upr]

# predict one 60yo
predict.lm(m, newdata = new.data, interval = 'prediction', level = 0.95)

# is anything worrisome about speed~age?
library(gvlma)
gvlma(m)
# 1. technically, we can reject all assumptions

# 2. but that's just because we have so much data.
# the scatterplot looks fine. (maybe worth using log(speed), but thats for 201b)
cal1020 %>%
  ggplot(aes(x=age, y=speed.mph))+
  geom_point(size=0.1, alpha=0.5)+
  geom_smooth(method='lm')+
  theme_minimal()
plot(m) # and the various residual plots look fine

# what happens if you do speed ~ sex

summary(lm(data = cal1020, speed.mph~sex))
t.test(cal1020$speed.mph ~ cal1020$sex, 
       var.equal = T)
t.test(cal1020$speed.mph[cal1020$sex == 'male'], 
       cal1020$speed.mph[cal1020$sex=='female'], 
       var.equal = T)
# these are equivalent (note, equivalent only with assumption of equal variance)

# make a plot for different corrals
cal1020 %>%
  ggplot(aes(x=age, y=speed.mph))+
  facet_wrap(~corral)+
  geom_point(size=0.1, alpha=0.5)+
  geom_smooth(method='lm')+
  theme_minimal()
