# Set default theme to be a bit nicer.

library(ggplot2)

# here we set the various theme properties...
th <- theme_minimal() +
theme(text = element_text(size=13, color="black"),
      title = element_text(size=14, face = 'bold'),
      plot.title = element_text(size=15, face='bold'),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, color="lightgray"),
      strip.text = element_text(size=14, face='bold'))

# let's try them here.
data.frame(x=rnorm(100), 
           y=rnorm(100), 
           a=sample(c('a','b','c'), size = 100, replace = T)) %>% 
  ggplot(aes(x=x,y=y,color=x, size=y))+
  facet_grid(~a)+
  geom_point() +
  ggtitle('Whatever plot')+
  th

# set them as the default
theme_set(th)

# make sure it is set as default
data.frame(x=rnorm(100), 
           y=rnorm(100), 
           a=sample(c('a','b','c'), size = 100, replace = T),
           b=sample(c('zip','zap','zoom'), size = 100, replace = T)) %>% 
  ggplot(aes(x=x,y=y,color=x, size=y))+
  facet_grid(b~a)+
  geom_point() +
  ggtitle('Whatever plot')
