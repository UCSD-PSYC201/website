library(tidyverse)

n.students = 20
n.parts = 10
part = 1:n.parts
student = 1:n.students
test = c('pre', 'post')
dat <- expand.grid(student=student, 
                   part=part, 
                   test=test) %>% 
  mutate(sex = case_when(floor((student-1)/10) == 0 ~ 'male',
                         TRUE ~ 'female'),
         section = case_when(floor((part-1)/5) == 0 ~ 'qualitative',
                             TRUE ~ 'quantitative')) %>% 
  mutate(measurement = 1:n()) %>% 
  mutate(z = 0)

# effect standard deviations
effects = c(
  'student' = 4,
  'part' = 3,
  'student:part' = 1.5,
  'student:section' = 1,
  'student:test' = 1,
  'student:section:test' = 0.5,
  'part:test' = 1,
  'part:test:sex' = 0.25,
  'part:sex' = 0.5,
  'test' = 2,
  'sex' = 1,
  'section' = 2,
  'sex:test' = 0.5,
  'sex:section' = 0.5,
  'section:test' = 1,
  'sex:test:section' = 0.25
)
effects['measurement'] = min(effects)
effects = sqrt(effects^2 / sum(effects^2))

err = list()

for(ef in names(effects)){
  vars = strsplit(ef, ':')[[1]]
  eid = as.character(interaction(dat[,vars]))
  uid = unique(eid)
  err[[ef]] = rnorm(length(uid), 0, effects[ef])
  names(err[[ef]]) <- uid
  dat$z = dat$z + err[[ef]][eid]
}

dat$score = round(dat$z*10+70,0)

dat <- dat %>% select(-z, -measurement) %>% 
  mutate(student = paste0('S.', student),
         part = paste0('P.', part))

m <- aov(data = dat, 
         score ~ sex*test*section + Error(student/(test*section)))

summary(m)

summary(aov(data = dat, 
            score ~ sex*test*section + Error(part/(test*sex))))

library(lmerTest)

M = lmer(data = dat, score ~ sex*test*section +
       (1|student) + (1|part) + (1|student:part) + 
       (1|student:section) + (1|student:test) + (1|student:section:test)+
       (1|part:test) + (1|part:sex) + (1|part:test:sex))
summary(M)

M0 = lmer(data = dat, score ~ 1 +
       (1|student) + (1|part) + (1|student:part) + 
       (1|student:section) + (1|student:test) + (1|student:section:test)+
       (1|part:test) + (1|part:sex) + (1|part:test:sex))
summary(M0)

anova(M0, M)


M = lmer(data = dat, 
         score ~ sex*test*section +
           (1|student) + 
           (1|part) + 
           (1|student:part) + 
           (1|student:section) + 
           (1|student:test) + 
           (1|student:section:test)+
           (1|part:test) + 
           (1|part:sex) + 
           (1|part:test:sex))
anova(M)
M2 = lmer(data = dat, 
          score ~ sex*test*section +
            (test*section|student) + 
            (sex*test|part) + 
            (1|student:part))
anova(M2)
