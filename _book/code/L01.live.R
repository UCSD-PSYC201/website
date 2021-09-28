rm(list=ls())
load(url('http://vulstats.ucsd.edu/data/cal1020.cleaned.Rdata'))

# install.packages('tidyverse')
library(tidyverse)

# Random Q&A
# how to set factor order manually, rather than default to alphabetical
# (which determines the order of factor placement on x, colors, etc.)
cal1020$sex.2 = factor(cal1020$sex, levels=c('male', 'female'))

ggplot(data = cal1020, mapping = aes(x = age, y=time.sec/60, color=sex)) +
  facet_wrap(~sex, ncol = 2)+
  geom_bin2d()+
  scale_y_continuous("Minutes", breaks = seq(0, 5*60, by=30)) +
  scale_x_continuous("Age", breaks = seq(0, 100, by=10)) +
  scale_fill_continuous(low = "lightgray", high = "black")+
  theme_minimal()+
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size=20))

ggplot(cal1020, aes(x=sex, y=speed.mph, fill=sex))+
  stat_summary(fun.y = mean, geom = 'bar')+
  stat_summary(fun.data = mean_se, geom = 'errorbar')+
  coord_cartesian(ylim=c(5,7))


