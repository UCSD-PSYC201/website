library(tidyverse)

dat <- read_tsv('http://vulstats.ucsd.edu/data/OSRI_data_Aug2018/data.csv')

z.score = function(x){(x-mean(x))/sd(x)}
smoothp = function(x){(x+1/length(x))/(sum(x)+1)}
entropy = function(p){-sum(p*log2(p))}

# calculate response entropy, and what response folks provided when giving only 1 answer
dat <- dat %>% 
  rowwise() %>%
  mutate(resp.ent = entropy(smoothp(table(Q1:Q44))),
         one.val = ifelse(n_distinct(Q1:Q44)==1, Q1, 0)) %>%
  ungroup()

# recode categorical variables into labels
dat <- dat %>% 
  mutate(gender = case_when(gender==1 ~ 'male',
                            gender==2 ~ 'female',
                            gender==3 ~ 'other',
                            TRUE ~ 'unspecified'),
         education = case_when(education == 1 ~ '1 <HS',
                               education == 2 ~ '2 HS',
                               education == 3 ~ '3 BS',
                               education == 4 ~ '4 grad',
                               TRUE ~ 'unspecified'),
         race = case_when(race == 1 ~ 'Mixed',
                          race == 2 ~ 'Asian',
                          race == 3 ~ 'Black',
                          race == 4 ~ 'Nat. Am.',
                          race == 5 ~ 'Nat. Aus.',
                          race == 6 ~ 'White',
                          race == 7 ~ 'Other',
                          TRUE ~ 'unspecified'),
         orientation = case_when(orientation == 1 ~ 'heterosexual',
                                 orientation == 2 ~ 'bisexual',
                                 orientation == 3 ~ 'homosexual',
                                 orientation == 4 ~ 'asexual',
                                 orientation == 5 ~ 'other',
                                 TRUE ~ 'unspecified'),
         religion = case_when(religion == 1 ~ 'atheist',
                              religion == 2 ~ 'christian',
                              religion == 3 ~ 'muslim',
                              religion == 4 ~ 'jewish',
                              religion == 5 ~ 'hindu',
                              religion == 6 ~ 'buddhist',
                              religion == 7 ~ 'other',
                              TRUE ~ 'unspecified'),
         hand = case_when(hand == 1 ~ 'right',
                          hand == 2 ~ 'left',
                          hand == 3 ~ 'both'))


# who is providing all the same answers?
# turns out its mostly heterosexual males, or those who did not respond to gender question.
# probably ok to drop these folks as just providing noise.
dat %>% count(same.resp = ifelse(resp.ent == 0, 'novar', 'var'), 
              gender, orientation) %>%
  spread(same.resp, n) %>%
  mutate(p = (novar)/(novar+var)) %>%
  mutate(p.novar = novar/sum(novar),
         p.var = var/sum(var))

# filter BS
dat <- dat %>% 
  filter(resp.ent > 0) %>%
  filter(age < 99)

# counts
dat %>% count(gender, orientation) %>%
  spread(gender, n)

# sexuality and gender.
dat %>% count(gender, orientation) %>%
  spread(gender, n) %>%
  mutate_at(2:5, function(x){round(x/sum(x), 2)})

osri.pca <- dat %>% 
  select(1:44) %>%
  prcomp(center=T, scale=T)

plot(osri.pca)


round(osri.pca$rotation[,1:3],2)

osri.rot = osri.pca$x %>% 
  as.tibble() %>%
  mutate(gender = dat$gender,
         age = dat$age,
         orientation = dat$orientation)

# plots by gender
colors = c('female' = 'darkred',
           'male' = 'darkblue',
           'other' = 'darkgreen',
           'unspecified' = 'darkgray')

osri.rot %>%
  ggplot(aes(x=PC1, y=PC2, color=gender))+
  geom_point(alpha=0.1, size=0.1)+
  scale_color_manual(values = colors)+
  theme_minimal()

osri.rot %>%
  ggplot(aes(x=PC1, fill=gender))+
  geom_histogram(position='identity', alpha=0.3)+
  scale_fill_manual(values = colors)+
  theme_minimal()


osri.rot %>%
  ggplot(aes(x=PC4, fill=gender))+
  geom_histogram(position='identity', alpha=0.3)+
  scale_fill_manual(values = colors)+
  theme_minimal()

osri.rot %>%
  ggplot(aes(x=PC1, fill=gender))+
  geom_density(alpha=0.3)+
  scale_fill_manual(values = colors)+
  theme_minimal()


# plots by sexual orientation

colors.g.o = c('female-heterosexual' = 'darkred',
               'female-homosexual' = 'red',
               'female-bisexual' = 'pink',
               'male-bisexual' = 'skyblue',
               'male-homosexual' = 'blue',
               'male-heterosexual' = 'darkblue')

osri.rot %>%
  filter(gender %in% c('male', 'female')) %>%
  filter(orientation %in% c('heterosexual', 'homosexual', 'bisexual')) %>%
  mutate(gender.orientation = factor(paste0(gender, '-', orientation), levels=names(colors.g.o))) %>%
  ggplot(aes(x=PC1, fill=gender.orientation))+
  scale_fill_manual(values = colors.g.o)+
  geom_density(alpha=0.3)+
  theme_minimal()

rescale <- osri.rot %>% filter(orientation=='heterosexual', gender %in% c('male', 'female')) %>%
  group_by(gender) %>%
  summarize(m = mean(PC1), s = sd(PC1)) %>%
  .$m

osri.rot %>%
  mutate(PC1.scale = ((PC1-rescale[1])/(rescale[2]-rescale[1])-0.5)*2) %>%
  group_by(gender, orientation) %>%
  summarize(m = mean(PC1.scale), s = sd(PC1.scale)) %>%
  select(-s) %>%
  spread(gender, m)

osri.rot %>%
  mutate(PC1.scale = ((PC1-rescale[1])/(rescale[2]-rescale[1])-0.5)*2) %>%
  group_by(gender, orientation) %>%
  summarize(m = mean(PC1.scale), s = sd(PC1.scale), n=n()) %>%
  arrange(m) %>%
  print(n=40)

osri.rot %>%
  mutate(PC1.scale = ((PC1-rescale[1])/(rescale[2]-rescale[1])-0.5)*2) %>%
  group_by(gender, orientation) %>%
  summarize(m = mean(PC1.scale), s = sd(PC1.scale), n=n()) %>%
  arrange(m) %>%
  filter(gender != 'unspecified', orientation != 'unspecified') %>%
  print(n=40)

