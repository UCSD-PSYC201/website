rm(list=ls())
library(tidyverse)
load(url("http://vulstats.ucsd.edu/data/cal1020.cleaned.Rdata"))

cal1020 %>%
  ggplot(aes(x=sex))+
  geom_histogram(stat='count')

cal1020 %>%
  ggplot(aes(x=State, fill=sex))+
  geom_bar(position='stack')

cal1020 %>%
  ggplot(aes(x=speed.mph))+
  geom_histogram()

cal1020 %>%
  ggplot(aes(x=speed.mph))+
  geom_density(fill='red')

cal1020 %>%
  ggplot(aes(x=round(speed.mph,0), fill=sex))+
  scale_x_continuous(breaks = 1:13)+
  geom_bar(position='fill')

cal1020 %>%
  ggplot(aes(x=sex, y=speed.mph))+
  geom_jitter()

