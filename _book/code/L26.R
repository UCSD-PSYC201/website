load(url("http://vulstats.ucsd.edu/data/e2.data.Rdata"))
library(tidyverse)

just.personality = e2.data %>% 
  select(subject:neuroticism) %>%
  group_by(subject) %>%
  summarise_all(mean) 

just.personality %>%
  GGally::ggpairs(columns = 2:6)

just.personality %>%
  mutate_at(.vars = 2:6, rank) %>%
  GGally::ggpairs(columns = 2:6, 
                  aes(alpha=0.1))

just.personality %>%
  cor(method = 'spearman') %>%
  round(digits = 4)
