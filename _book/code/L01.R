ls()

load(url('http://vulstats.ucsd.edu/data/cal1020.cleaned.Rdata'))

ls()

str(cal1020)

library(tidyverse)
# install.packages('tidyverse')

glimpse(cal1020)

head(cal1020)

View(cal1020)

summary(cal1020)

cal1020 %>% 
  glimpse()

# distribution of times.
# histogram, density aes(y = ..count..)

cal1020 %>% 
  ggplot(aes(x=time.sec)) +
  geom_histogram()

colors <- c('female'='green', 'male'='purple')

# time ~ sex
cal1020 %>% 
  ggplot(aes(x=time.sec,
             fill=sex)) +
  facet_grid(sex~.)+
  geom_histogram(position = 'identity',
                 alpha = 0.5) +
  scale_fill_manual(values = colors)+
  theme_bw()

# speed ~ corral
cal1020 %>%
  ggplot(aes(x = corral, 
             y = speed.mph,
             color = corral))+
  geom_jitter(size = 0.01, alpha=0.2)

cal1020 %>%
  ggplot(aes(x = as.factor(corral), 
             y = speed.mph,
             fill = corral))+
  geom_violin()



# distribution of times by sex.  
# via Facets?  wrap vs grid
# via fill color?   setting alpha.  setting colors.
# position = {stack, identity, fill}?
# via categorical x axis?
# bar plot


# categorical y axis? (coord_flip)

# making the plot look better.



# how to make histogram and density align on the y axis.
# cal1020 %>% ggplot(aes(x=time.sec))+
#   geom_histogram(binwidth = 60*5)+
#   geom_density(aes(y=60*5 * ..count..), bw=60*5)