## 201ab L02

## files and paths
# http://vulstats.ucsd.edu/data/cal1020.cleaned.Rdata
# enter in browser -- where did it go?

# absolute paths
# relative paths

getwd()
setwd()
file.path()
.Platform$file.sep

## Data summaries
load(url('http://vulstats.ucsd.edu/data/cal1020.cleaned.Rdata'))

str(cal1020)


# Descriptive stats
## central tendency
### mean (weighted)
sum(cal1020$time.sec)/length(cal1020$time.sec)
mean(cal1020$time.sec)
### median
median(cal1020$time.sec)
### (mode?)

# dispersion
## sd
sd(cal1020$time.sec)
## var
var(cal1020$time.sec)
sqrt(var(cal1020$time.sec))

## quantile
quantile(cal1020$time.sec, probs = c(0.5))
## range
quantile(cal1020$time.sec, probs = c(0, 1))
## IQR
quantile(cal1020$time.sec, probs = c(0.25, 0.75))
IQR(cal1020$time.sec)
## (entropy?)


## sensitivity to tails

## shape
# standardize (z-score) 
m = mean(cal1020$time.sec)
s = sd(cal1020$time.sec)
cal1020$zscores = (cal1020$time.sec-m)/s

# make a z_score(x) function
z_score = function(x){
  m = mean(x)
  s = sd(x)
  return((x-m)/s)
}

z_score(1:10)
# intuitions behind moments

mean(z_score(cal1020$time.sec))
mean(z_score(cal1020$time.sec)^3)
moments::skewness(cal1020$time.sec)
## skewness: asymmetry
## kurtosis: tail heaviness
mean(z_score(cal1020$time.sec)^4)-3
moments::kurtosis(cal1020$time.sec)-3

# moments::skewness, 
# moments::kurtosis


library(dplyr)
### tidyverse:  
# dplyr, ggplot, pipe, readr, and some other libraries.
## why tidyverse?  
# not much unique functionality, but lots is easier, cleaner

# e.g., print dataframe vs tibble
# e.g., str() vs glimpse()
# e.g., read.csv() vs read_csv()

## to start: dplyr for data transformations.
# https://rstudio.com/resources/cheatsheets/
# data wrangling (not directly linked on site?)
# data transformations

##  "subset observations": extracting subsets of rows. 
cal1020[(cal1020$sex == 'female') & (cal1020$time.sec < 3500), ]

# filter     {handy: top_n, sample_n, sample_frac, slice, distinct}
cal1020 %>% 
  filter(sex == 'female', time.sec < 3500)

# e.g., get just the females over 50...
# compare to subset(), base indexing.

cal1020 %>% 
  filter(age > 50, sex=="female") %>% 
  glimpse()

cal1020[cal1020$age > 50 & cal1020$sex == 'female',]

##  "subset variables": extract subset of columns
# select    {handy predicates: contains(), matches(), one_of(); handy variants: select_if, select_at}

glimpse(cal1020)
cal1020[, c("name.first", "State")]
cal1020 %>% select(name.first, State)
cal1020 %>% select_if(is.numeric) %>% glimpse()

cal1020 %>% select(name.first:State)
cal1020[, 1:4]

cal1020 %>% select(-sex) %>% glimpse()

# e.g., get just first name, age, and tim

cal1020 %>% 
  filter(age > 50, sex=="female") %>% 
  select(name.first, age, time.sec)
# compare to base indexing

## pipes!
# A() %>% B()
# provide output of A() as first argument to B()
# usually we have a data processing pipeline:
# we take the data, do f(), then do g(), then do h(), etc.
# e.g., 
# - convert strings to numbers,
# - drop missing data
# - z-score a variable
# - rename some factor levels
# - calculate averages of subgroups
# - etc.
# conventional approaches are painful:
# - nesting functions
# - create a bunch of intermediate variables.

# e.g., find women over 50, get their first name, age, time.

## make / change columns
# mutate: add new variablse (keep old onse and everything else)
# rename: rename variables (drop old ones, keep everything else)
# transmute: add new variables (drop old ones, and everything else) -- I never use this.  mutate %>% select
# handy variants: mutate_if  e.g., mutate_if(is.factor, as.character)

cal1020 %>% glimpse()
cal1020 %>% mutate(time.min = time.sec/60) %>% glimpse()

cal1020 %>% mutate(time.seconds = time.sec) %>% 
  select(-time.sec) %>% glimpse()

cal1020 %>% rename(time.seconds = time.sec) %>% glimpse()

cal1020 %>% transmute(time.min = time.sec/60) %>% glimpse()

cal1020 %>% mutate_if(is.numeric, z_score) %>% glimpse()

## making groups
# group_by, ungroup
# we usually want to do stuff to specific subsets of the data.
# e.g., for each subject-condition, calculate median RT.
# conventional approach would be:
# - define unique groups
# - loop through them
# - on each loop extract relevant subset of data
# - do stuff to it
# - save group-level output somewhere.
## group_by approach:
# - define groups
# - do stuff to the grouped data frame.
# e.g., find fastest male, female, in each decadal age bracket {0-9, 10-19, etc.}

cal1020 %>% glimpse()
cal1020 %>% group_by(sex)

cal1020 %>% group_by(sex) %>% 
  summarize(m = mean(time.sec), s = sd(time.sec))

cal1020 %>% 
  group_by(sex, age.group = floor(age/10)*10) %>% 
  mutate(zscore = z_score(time.sec)) %>% 
  ungroup() %>% 
  glimpse()


cal1020 %>% 
  group_by(sex, age.group = floor(age/10)*10) %>% 
  arrange(time.sec) %>% 
  slice(1) %>% 
  select(name.first, sex, age.group, time.sec)

## summarizing many observations
# summarize()  {handy variants: summarize_if(), summarize_each(), 
# summary functions: take a vector, yield one number.  min, max, mean, median, var, sd, etc. 

cal1020 %>% 
  group_by(sex, age.group = floor(age/10)*10) %>% 
  summarize(mean.time.sec = mean(time.sec),
            sd.time.sec = sd(time.sec))

cal1020 %>% 
  group_by(State) %>% 
  summarize(n = n())

cal1020 %>% 
  count(State) %>% 
  filter(n == max(n))

cal1020 %>% 
  group_by(State) %>% 
  summarize(mean.time = mean(time.sec), n= n()) %>% 
  top_n(2, desc(mean.time))


# for each corral, get n, and mean, sd, min, max of time.min
cal1020  %>% 
  group_by(corral) %>%   
  mutate(time.min = time.sec/60) %>%   
  summarize(n = n(), 
            mean.time.min = mean(time.min),
            sd.time.min = sd(time.min),
            min.time.min = min(time.min),
            max.time.min = max(time.min))

# corral, measurement (n, mena..), value
wide.data = cal1020  %>% 
  group_by(corral) %>%   
  mutate(time.min = time.sec/60) %>%   
  summarize(n = n(), 
            mean.time.min = mean(time.min),
            sd.time.min = sd(time.min),
            min.time.min = min(time.min),
            max.time.min = max(time.min))

long.data <- wide.data %>% 
  pivot_longer(cols = n:max.time.min, 
               names_to = 'measurement', 
               values_to = 'value')

long.data %>% 
  pivot_wider(id_cols = corral, 
              names_from = measurement, 
              values_from = value)



# subject, condition, performance
# 1, 'a', 0.8
# 1, 'b', 0.9

# subject, condition_a_performance, condition_b_performance
# 1, 0.8, 0.9

## Reshaping data.
# "long" vs "wide" formats.
# tidyr::
# tidyr::gather / spread
# tidyr::separate / unite
# new: pivot_longer  pivot_wider

## Advanced: 
# two data frames: {left,right,inner,full}_join, intersect, union, setdiff 
# *_{if, at, all, each}

# handy: count, add_count

cal1020 %>% 
  ggplot(aes(x=age, y=time.sec, size=corral, color=corral)) +
  geom_point(position = position_jitter(), alpha=1)+
  scale_size_continuous(range = c(0.01, 0.5))





