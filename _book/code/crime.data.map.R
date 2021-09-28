rm(list=ls())
library(tidyverse)
library(lme4)
library(arm)
library(maps)

load(url('http://vulstats.ucsd.edu/data/crime.data.Rdata'))

# 
abbr.lookup = c()
for(state in unique(as.character(crime.data$State))){
  if(state == "DISTRICT OF COLUMBIA"){
    abbr.lookup[state] = "DC"
  } else {
    abbr.lookup[state] = state.abb[grep(paste0("^",state), state.name, ignore.case = TRUE)]
  }
}
crime.data <- crime.data %>%
  mutate(State = abbr.lookup[as.character(State)]) %>%
  mutate(name = toupper(paste0(as.character(City), " ", State)))

# full.df <- merge(crime.data, mutate(maps::us.cities, name=toupper(name)), by='name', all=TRUE)
# 
# missing.from.us.cities <- full.df %>% 
#   filter(is.na(lat)) %>% 
#   select(name) %>% 
#   .$name %>% 
#   unique()
# missing.from.crime.data <- full.df %>% 
#   filter(is.na(Population)) %>% 
#   select(name) %>% 
#   .$name %>% 
#   unique()
# 
# missing <- rbind(data.frame(name = missing.from.crime.data, missing="from crime.data", stringsAsFactors = FALSE),
#                  data.frame(name = missing.from.us.cities, missing="from us.cities", stringsAsFactors = FALSE)) %>%
#   mutate(state = unlist(lapply(strsplit(name," "), function(n)tail(n,1)))) %>%
#   arrange(name) %>%
#   arrange(state)

use.crime = "Assault"
per.n = 1000

full.df <- merge(crime.data, mutate(maps::us.cities, name=toupper(name)), by='name') %>%
  mutate(Crime = as.character(Crime)) %>%
  filter(Crime == use.crime) %>%
  mutate(rate = Count/Population*per.n)


label.cities = unique(c('SAN DIEGO CA', 'NEW YORK NY', 'BALTIMORE MD', 'DETROIT MI', 
                        full.df %>% top_n(10, rate) %>% .$name %>% as.character(),
                        full.df %>% top_n(5, Population) %>% .$name %>% as.character()))


label.cities = unique(c(full.df %>% top_n(15, rate) %>% .$name %>% as.character()))
  
us = map_data("state")
dat_region = data.frame(region = unique(us$region))
full.df %>% 
  filter(!(State %in% c('AK', 'HI'))) %>%
  ggplot(aes(x=long, y=lat)) +
  geom_map(data=us, map=us, aes(map_id=region), fill="#EEEEEE", color="#555555") + 
  geom_text(data = filter(full.df, name %in% label.cities), aes(label=City), hjust=-0.1)+
  geom_point(shape = 21, color="black", aes(size=Population, fill=rate), alpha=0.5) +
  scale_size(limits = c(0, 10000000), range=c(0.25, 25)) + 
  scale_fill_gradient(low="#CCFFCC", high = "#880000")+
  coord_map("albers", lat0 = 39, lat1 = 45) +
  theme_minimal() +
  guides(size = 'none', fill='none')+
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

full.df %>% 
  filter(!(State %in% c('AK', 'HI'))) %>%
  ggplot(aes(x = Population, y=rate)) +
  geom_label(data = filter(full.df, name %in% label.cities), aes(label=City))+
  geom_point(shape = 21, color="black", aes(size=Population, fill=rate), alpha=0.5) +
  scale_size(limits = c(0, 10000000), range=c(0.25, 25), label=scales::comma) + 
  scale_fill_gradient(low="#CCFFCC", high = "#880000")+
  scale_x_log10('Population', breaks=10^seq(3,7,1), labels=scales::comma) + 
  ylab(sprintf('%s per %d', use.crime, per.n)) +
  guides(size = 'none', fill='none')+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(size=16))

m3 = glmer(data=crime.data, family=poisson(), 
           Count~log(Population)+(1+log(Population)|Crime)+
             (1|Crime) + (1|Crime:State))

names(abbr.lookup) <- tolower(names(abbr.lookup))
rev.lookup = names(abbr.lookup)  
names(rev.lookup) = unname(abbr.lookup)

effects <- ranef(m3)$`Crime:State`
effects <- data.frame(ef.name = rownames(effects), effect = unname(effects))
row.names(effects) <- NULL
effects <- effects %>% 
  separate(ef.name, into = c('Crime', 'State'), sep = ":") %>%
  mutate(region = unname(rev.lookup[State]))

effects <- effects %>% 
  filter(!(State %in% c('AK', 'HI'))) %>% 
  filter(Crime != 'Rape') 
ggplot() +
  facet_wrap(~Crime) +
  geom_map(data=us, map=us, aes(x=long, y=lat, map_id=region), fill="#FFFFFF", color="#FFFFFF") + 
  geom_map(data = effects, map=us, aes(map_id=region, fill=effect), color="#FFFFFF") +
  scale_fill_gradient2(low='darkgreen', mid = 'gray', high='darkred') +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  theme_minimal() +
  guides(fill='none')+
  theme(panel.grid = element_blank(),
        strip.text = element_text(size=16),
        axis.text = element_blank(),
        axis.title = element_blank())


m2 = glmer(data=crime.data, family=poisson(), 
           Count~log(Population)
           +(1+log(Population)|Crime)
           +(1|Crime:State))

m4 = glmer(data=crime.data, family=poisson(), 
           Count~log(Population)*Crime
           +(1+Crime|State))

m3 = glmer(data=crime.data, family=poisson(), 
           Count~log(Population)
           +(1+log(Population)|Crime)
           +(1|State)
           +(1|Crime:State))

m1 = glmer(data=crime.data, family=poisson(), 
           Count~log(Population)*Crime
           +(1|State))

m0 = glmer(data=crime.data, family=poisson(), 
           Count~log(Population)+Crime
           +(1|State))


mn = glmer(data=crime.data, family=poisson(), 
           Count~log(Population)
           +(1|State))

p0 = predict(m0, crime.data)
p1 = predict(m1, crime.data)
p2 = predict(m2, crime.data)
p3 = predict(m3, crime.data)
p4 = predict(m4, crime.data)


pn = predict(mn, crime.data)

plot(p3, pn)


crime.data.10 = crime.data %>% sample_frac(0.10)


m4.10 = glmer(data=crime.data.10, family=poisson(), 
           Count~log(Population)*Crime
           +(1+Crime|State))

m3.10 = glmer(data=crime.data.10, family=poisson(), 
           Count~log(Population)
           +(1+log(Population)|Crime)
           +(1|State)
           +(1|Crime:State))

crime.data.01 = crime.data %>% sample_frac(0.01)

m4.01 = glmer(data=crime.data.01, family=poisson(), 
              Count~log(Population)*Crime
              +(1+Crime|State))

m3.01 = glmer(data=crime.data.01, family=poisson(), 
              Count~log(Population)
              +(1+log(Population)|Crime)
              +(1|State)
              +(1|Crime:State))


AIC(m3, m4)


AIC(m3.10, m4.10)

AIC(m3.01, m4.01)
