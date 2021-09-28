# Midterm review

# 0. help!  ? ?? tab completion
# 1. variables and operators
# 2. vectors and vector operations
# 3. data frames / tibbles, and operations
# 4. functions and summary stats
# 5. probability functions
# 6. ggplot
??stepwise

load(url('http://vulstats.ucsd.edu/data/cal1020.cleaned.Rdata'))
df <- data.frame(variable=names(cal1020))
for(n in df$variable){
  df[df$variable==n,'r.squared'] <- summary(
    lm(cal1020$speed.mph ~ cal1020[[n]]))$r.squared
}

lm(data = cal1020, speed.mph ~ .)

library(dplyr)


# subsetting by column
cal1020 <- as_tibble(cal1020)

cal1020 %>% select(age) # return a dataframe
cal1020$age  # returns a vector 
cal1020['age'] # return a data.frame
cal1020[['age']] # return a vector
cal1020[4] # return a data fra,e
cal1020[,4]
cal1020[[4]] # return a vector
cal1020[names(cal1020)=='age'] # returns a data frame
cal1020[[names(cal1020)=='age']] # does nothing.
cal1020 %>% select(sex, age) # return a dataframe
cal1020[c('sex', 'age')] # return a data.frame
cal1020[c(6,7)] # return a data fra,e
cal1020[names(cal1020) %in% c('sex', 'age')] # returns a data frame

# filter on rows (logical)
cal1020 %>% filter(sex=='female', age > 35)
filter(cal1020, sex=='female', age > 35)
cal1020[cal1020$sex == 'female' & cal1020$age>35,]
subset(cal1020, cal1020$sex=='female' & cal1020$age>35)

# filter rows (by number)
cal1020 %>% slice(1:10) # rows 1:10
cal1020[1:10,] # rows 1:10
cal1020 %>% slice(-1) # omits row 1
cal1020[-1,] # omits row 1

# if number of nas in a row == 0, then take this row.
cal1020[rowSums(is.na(cal1020))==0,]
cal1020[complete.cases(cal1020),]
cal1020 %>% filter(complete.cases(.))

# grouping
library(ggplot2)
plot.ofsomething <- cal1020 %>% 
  group_by(sex, age.5 = round(age/5,0)*5) %>% 
  summarise(top.speed = max(speed.mph)) %>%
  ggplot(aes(x=age.5, y=top.speed, color=sex))+geom_line(size=3)

weird.grouped.lm <- cal1020 %>% 
  group_by(sex, age.5 = round(age/5,0)*5) %>% 
  summarise(b0 = lm(speed.mph~corral)$coefficients[1],
            b1 = lm(speed.mph~corral)$coefficients[2])

cal1020 %>% 
  mutate(age.5 = round(age/5,0)*5) %>% 
  ggplot(aes(x=corral, y=speed.mph, color=sex))+
  facet_wrap(~age.5)+
  geom_point()+
  geom_smooth(method='lm')


cal1020 %>% mutate(age.5 = round(age/5,0)*5)
cal1020$age.5 = round(cal1020$age/5,0)*5


M0 <- lm(data = cal1020, speed.mph ~ 1)
M1 <- lm(data = cal1020, speed.mph ~ age)
M2 <- lm(data = cal1020, speed.mph ~ age+corral)
M3 <- lm(data = cal1020, speed.mph ~ age + corral + sex)
summary(M3)
anova(M3)


anova(M0, M3)
anova(M0, M1, M2, M3)
