## 201ab L02
# the content of this will change during class, and I will upload the final version after.
library(tidyverse)
load(url('http://vulstats.ucsd.edu/data/cal1020.cleaned.Rdata'))

### Descriptive stats
## central tendency
# mean (weighted), median, (mode?)
cal1020$time.min <- cal1020$time.sec/60
mean(cal1020$time.min)
median(cal1020$time.min)
# mode (write own function, perhaps if there is time at end of class)
cal1020 %>% 
  ggplot(aes(x=time.min))+
  geom_histogram()
# sensitivity to tails

## dispersion
# sd, var, range, IQR, (entropy?)
sd(cal1020$time.min)
var(cal1020$time.min)

quantile(x=cal1020$time.min, probs = c(0.25, 0.5, 0.75))
IQR(cal1020$time.min)


## shape
# install.packages('moments')
# moments::{skewness, kurtosis}

moments::skewness(cal1020$time.min)
moments::kurtosis(cal1020$time.min) - 3 # -3 for "excess kurtosis"

# standardizing / z scoring
cal1020$time.min.z = (cal1020$time.min - mean(cal1020$time.min))/sd(cal1020$time.min)

# skew and kurtosis
mean(cal1020$time.min.z^3)
mean(cal1020$time.min.z^4)
mean(cal1020$time.min.z^4)-3

# (if time: the wonders of logarithms)

## dplyr wrangling, grouping
# why dplyr?  
# not much unique functionality, but lots is 
# easier, cleaner, (sometimes faster), more homogenous

# print df vs tbl
cal1020
tbl_df(cal1020)

# glimpse vs str
str(cal1020)
glimpse(cal1020)

## pick rows
# filter     {handy: top_n, sample_n, sample_frac, slice, distinct}
cal1020[1,]
slice(cal1020, 1)

x = 2
x==3

females <- cal1020 %>%
  filter(sex=='female')
females[1,]

cal1020 %>%
  filter(sex=='female') %>%
  slice(1)

cal1020 %>%
  filter(sex=='female', time.min < 120, age > 35) %>%
  glimpse()

subset(cal1020, sex=='female' | time.min < 120 | age > 35) %>%
  glimpse()

cal1020 %>% arrange(sex) %>% head()

## pick columns
# select   
# awkwardness of getting vector out.

cal1020$age
cal1020[['age']]
cal1020[[6]]
cal1020[,'age']
cal1020[,6] %>% str()
cal1020 %>%
  select(age) %>% 
  str()

## make/change columns
# mutate     {handy: rename, transmute (like mutate %>% select)}

x = cal1020 %>%
  mutate(time.hours = time.min/60,
         time.days = time.hours/24)

## pipes: A() %>% B()
# provide output of A() as first argument to B()
# enabled by homogenous syntax
# select(
#   filter(
#     cal1020, sex=='female', age>50), 
#   name.first, age, time.sec)

cal1020 %>%
  filter(sex == 'female', age > 50 ) %>%
  select(name.first, age, time.sec)



## grouping
# group_by 


## summarizing
# summarize  {_at, _if}

tbl_df(cal1020) %>%
  group_by(sex, corral) %>%
  mutate(fastest = min(time.sec)) %>%
  summarise(m = mean(time.min), 
            fastest.mean = mean(fastest))

## others
# count, arrange

cal1020 %>%
  group_by(sex, corral) %>%
  summarize(n = n())

cal1020 %>% count(sex, corral)


## Tidy data.
# tidyr::gather / spread
# tidyr::separate / unite

## Advanced: 
# two data frames: {left,right,inner,full}_join, intersect, union, setdiff 
# _{if, at, all}





