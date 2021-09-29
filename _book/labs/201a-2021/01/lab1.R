# PSYC 201A Lab 01: R basics
# Wenhao (James) Qi
# 2021-09-29

# Ed's stuff ----
# vector implicit expansion
a = c(1, 2, 3, 1, 2, 3, 1, 2)
b = c(1, 2)
a == b
# wtf?
rep(c(1, 2), 4)
#.... dont do this.
# also... what was the intent?
# perhaps... %in%?
a %in% b
# this happens with all operators...
c(1, 2, 3, 1, 2, 3, 1, 2) * c(1, 2)
# my suggestion: do not use it intentionally (rep stuff yourself, for clarity)!


# aggregation functions
a = c(8, 6, 7, 5, 3, 0, 9)
a >= 6
any(a >= 6)
all(a >= 6)
sum(a)
mean(a)

# influence of weird types:
a = c(1, 2, 3, 4, 3/0)
sum(a)
a = c(1, 2, 3, 4, 0/0)
mean(a)
a = c(1, 2, 3, 4, NA)
sum(a)
sum(a, na.rm = TRUE)

# functions
fx = function(x) {
  return(x^2)
}
fx
fx(4)

# return explicitly to avoid confusion
fx = function(x) x^2 # allowed, concise
# variable scope
fx = function(x){
  a = 2
  x = x+a
  return(x^2)
}
a = 3
x = 4
fx(x)
a
x

# global variables show up in functions... 
a = 3
x = 4
fx = function(x){
  print(sprintf('a=%d, x=%d', a, x))
  a = 2
  print(sprintf('a=%d, x=%d', a, x))
  x = x+a
  print(sprintf('a=%d, x=%d', a, x))
  return(x^2)
}
print(sprintf('a=%d, x=%d', a, x))
fx(5)
# to make life easy:
# - don't rely on global variables inside functions (sometimes very handy, so minimize this at least)
# - don't try to set global variables in functions (<<- will do it though)


# loops
for(i in 1:10) {
  print(i^2)
}
# while(){} also available

# conditionals
x = 3
if(x != 5){
  print(x)
}

if(x == 5){
  print(x)
} else {
  print('meow')
}

if(x != 5){
  x
} else {
  'meow'
}

y = if(x != 5) x else 'meow'
x != 5 ? x : 'meow' # C, js
y

for(i in 1:10){
  if(i != 5){
    print(i)
  } else {
    print('meow')
  }
}

# fizz buzz?
for(i in 1:15){
  if((i %% 3 == 0) & (i %% 5 == 0)){
    print('fizzbuzz')
  } else if(i %% 5 == 0){
    print('buzz')
  } else if(i %% 3 == 0){
    print('fizz')
  } else {
    print(i)
  }
}


# vector operations are fast, loops are slow
n = 1:1000000
system.time(
  for(i in 1:length(n)){
    n[i] = n[i]^2
  }
)
n[2]

n = 1:1000000
system.time(n <- n^2)
n[2]

# ... but don't worry about optimization until you need to.


# lists
a = list(1:3, letters[1:4], list(1:5))
str(a)
a[1]
a[[1]]
a[1:2]
names(a)
names(a) <- c('x', 'y', 'z')
a
a = list(x = 1:3,
         y = letters[1:4],
         z = list(1:5))
a$x
a[[1]]
a[['x']]


# data frames
a = data.frame(x = 1:3, y = letters[1:3])
a = data.frame(x = 1:3, y = letters[1:4])
a
a$x
a[[1]]
a[['x']]
a[1]
a[1,]
a[,1]
a[,'x']
a[1,2]
a[1,'y']
a$y[1]
# oy!
# tomorrow: an opinionated approach from tidyverse that yields more readable code.

# data frame building:
expand.grid(suit = c('diamonds', 'clubs', 'hearts', 'spades'),
            value = c(2:10, 'jack', 'queen', 'king', 'ace'))

# built in dataset...
mtcars
str(mtcars)
summary(mtcars)
hist(mtcars$mpg)
plot(mtcars$mpg, mtcars$hp)
# we will use ggplot, but these are useful for quick plotting.

# tibbles from tidyverse -- slightly handier data.frames

# reading data, working with the file system.
getwd()
setwd('/Users/jamesqi/Documents/graduate/TAship/UCSD-PSYC201/website/_book/labs/201a-2021/01')

# download... vulstats.ucsd.edu/data/fifa-2019.csv
# but where is it?
# '/Users/jamesqi/Documents/graduate/TAship/UCSD-PSYC201/website/_book/data/fifa-2019.csv'
fifa = read.csv('../../../data/fifa-2019.csv')
fifa = read.csv('~/Documents/graduate/TAship/UCSD-PSYC201/website/_book/data/fifa-2019.csv')
fifa = read.csv('http://vulstats.ucsd.edu/data/fifa-2019.csv')
# apologies to those on windows... \\
# for homeworks, files to be loaded will be in the same directory, to avoid path separator confusion
# also:
.Platform$file.sep
file.path('..', '..', '..', 'data', 'fifa-2019.csv')

# read.csv has annoying defaults.  tidyverse read_csv is better.
fifa.tibble = readr::read_csv('http://vulstats.ucsd.edu/data/fifa-2019.csv')
# :: indicates function inside library.


# Isabella's stuff ----

# packages ----
# check where packages are installed
.libPaths()
dir.exists(.libPaths()[1])
dir.create(.libPaths()[1], recursive = T)

# install tidyverse
install.packages('tidyverse')

# load (and attach) tidyverse
library(tidyverse)
filter()
stats::filter()

# list packages in tidyverse
tidyverse_packages()

# inspect data ----
# load csv
df <- read_csv('lab1.csv')

# view data with data viewer
View(df)

# show the first few rows
head(df, 10)

# summarize each column
summary(df)

# show column types and a few values
glimpse(df)

# base R variant
str(df)

# count occurrences of each value in a column
table(df$sex)

# size of data frame
length(df$SubjectID)
dim(df)
nrow(df)
ncol(df)

# clean data ----
# SubjectID
table(df$SubjectID)
df$SubjectID <- 1:nrow(df)
df$SubjectID.new <- 1:nrow(df)

# BirthYear
table(df$BirthYear)
df$BirthYear[df$BirthYear == 0] <- NA

# sex
table(df$sex)
df$sex.new <- factor(df$sex, labels = c('Male', 'Female'))
fct <- as.factor(df$sex)
levels(fct)
df$sex.new <- fct_recode(fct, Male = '0', Female = '1')
df$sex.new <- recode(df$sex, `0` = 'Male', `1` = 'Female')
df$sex.new <- recode(df$sex + 1, 'Male', 'Female')
df$sex.new <- recode_factor(df$sex, `0` = 'Male', `1` = 'Female')

# IQ
table(df$IQ)
df$IQ[df$IQ == '.'] <- NA
df$IQ <- as.numeric(df$IQ)

# drop NAs
drop_na(df)

# transform data ----
df <- read_csv('lab1.csv')

# pipeline
f(x, y)
x %>% f(y)
x
x1 <- f1(x)
x2 <- f2(x1)
x2 <- x %>% f1() %>% f2()

# select columns
df %>% 
  select(SubjectID, IQ)
select(df, SubjectID, IQ)

# select rows
df %>% 
  slice(c(1, 3, 5))
df %>% 
  slice_head(n = 5)
df %>% 
  slice_tail(prop = .1)
df %>% 
  filter(sex == 0)

# sort rows
df %>% 
  arrange(BirthYear)

# rename columns
df %>% 
  rename(id = SubjectID)

# modify/add columns
df %>% 
  mutate(SubjectID = 1:n())

# clean data (again)
df2 <- df %>% 
  mutate(SubjectID = 1:n(), sex = recode(sex + 1, 'Male', 'Female')) %>% 
  filter(BirthYear != 0, IQ != '.') %>% 
  mutate(IQ = as.numeric(IQ))

# convert BirthYear to age and decade
df3 <- df2 %>% 
  mutate(age = 2021 - BirthYear, decade = round(age, -1))

# summarize data ----
# number of rows, mean and sd of age and IQ
df3 %>% 
  summarize(nrows = n(), age.mean = mean(age), age.sd = sd(age), IQ.mean = mean(IQ), IQ.sd = sd(IQ))

# after grouping by decade
df3 %>% 
  group_by(decade) %>% 
  summarize(nrows = n(), age.mean = mean(age), age.sd = sd(age), IQ.mean = mean(IQ), IQ.sd = sd(IQ))

# visualize data (preview) ----
# scatter plot, IQ ~ age
df3 %>% 
  ggplot(aes(x = age, y = IQ)) +
  geom_point()

# color by sex
plot2 <- df3 %>% 
  ggplot(aes(x = age, y = IQ, color = sex)) +
  geom_point()

# theme
plot3 <- plot2 + theme_bw()

# title
plot3 + ggtitle('IQ ~ age')
plot3 + labs(title = 'IQ ~ age') + theme(plot.title = element_text(hjust = .5))
