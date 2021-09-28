library('tidyverse')

dat <- read_tsv('../data/duckworth-grit-scale-data/data.csv')

var.names = names(dat)
var.char = gsub("[[:digit:]]","",var.names)
var.num = as.numeric(gsub("[a-zA-Z]","",var.names))

scales = c('extroversion' = 'E',
          'neuroticism' = 'N',
          'agreeableness' = 'A',
          'conscientiousness' = 'C',
          'openness' = 'O',
          'grit' = 'GS',
          'vocabulary' = 'VCL')

scoring = list(
  'extroversion' = rep(c(1, -1), 5),
  'neuroticism' = c(1, -1, 1, -1, 1, 1, 1, 1, 1, 1),
  'agreeableness' = c(-1, 1, -1, 1, -1, 1, -1, 1, 1, 1),
  'conscientiousness' = c(1, -1, 1, -1, 1, -1, 1, -1, 1, 1),
  'openness' = c(1, -1, 1, -1, 1, -1, 1, 1, 1, 1),
  'grit' = c(1, -1, -1, 1, -1, 1, -1, -1, 1, 1, -1, 1),
  'vocabulary' = c(1, 1, 1, 1, 1, -4, 1, 1, -4, 1, 1, -4, 1, 1, 1, 1)
)

for(scale in names(scales)){
  idx = which(var.char==scales[scale])
  print(c(length(idx), length(scoring[[scale]])))
  dat[[scale]] = rowSums(as.matrix(dat[,idx])*rep(scoring[[scale]], each=nrow(dat)))
}

dat <- dat %>% 
  select(country, surveyelapse, education, urban, gender, engnat, age, hand, religion, orientation, race, voted, married, familysize,
         operatingsystem, browser, screenw, screenh, introelapse, testelapse, extroversion, neuroticism, agreeableness, conscientiousness, openness, grit, vocabulary)

dat <- dat %>% mutate(gender = case_when(
  gender == 1 ~ 'male',
  gender == 2 ~ 'female',
  gender == 3 ~ 'other')) %>% 
  mutate(hand = case_when(
    hand == 1 ~ 'right',
    hand == 2 ~ 'left',
    hand == 3 ~ 'both')) %>% 
  mutate(race = case_when(
    race == 1 ~ 'asian',
    race == 2 ~ 'arab',
    race == 3 ~ 'black',
    race == 4 ~ 'white or indigenous',
    race == 5 ~ 'other')) %>% 
  mutate(voted = case_when(
    voted == 1 ~ 'yes',
    voted == 2 ~ 'no')) %>% 
  mutate(married = case_when(
    married == 1 ~ 'never',
    married == 2 ~ 'currently',
    married == 3 ~ 'previously'))

dat %>% write_csv('../data/duckworth-grit-scale-data/data-coded.csv')

dat %>% count(gender)

dat %>% 
  filter(gender %in% c('male', 'female')) %>% 
  ggplot(aes(x=neuroticism, fill=gender))+
  geom_density(alpha=0.5)

dat %>% 
  filter(gender %in% c('male', 'female')) %>% 
  ggplot(aes(x=neuroticism, y = agreeableness, color=gender))+
  facet_grid(~gender)+
  geom_point()+
  geom_smooth(method='lm')
