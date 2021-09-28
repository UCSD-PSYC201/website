# PCA illustration

library(tidyverse)

df <- tibble(x = (-5:5)+sample(c(-1,0,1), 11, replace=T), 
             y=(-5:5)+sample(c(-1,0,1), 11, replace=T))

df %>% ggplot(aes(x=x,y=y))+geom_point(size=5)+theme_minimal()+
  scale_x_continuous(breaks=-7:7)+
  scale_y_continuous(breaks=-7:7)+
  theme(panel.grid.minor = element_blank())+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)

df %>% ggplot(aes(x=x,y=y))+theme_minimal()+
  scale_x_continuous(breaks=-7:7)+
  scale_y_continuous(breaks=-7:7)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  geom_abline(intercept = -10:10, slope=1, color='gray')+
  geom_abline(intercept = -10:10, slope=-1, color='gray')+
  geom_abline(slope=1, intercept=0)+
  geom_abline(slope=-1, intercept=0)+geom_point(size=5)
