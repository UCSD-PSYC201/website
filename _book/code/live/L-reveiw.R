library(tidyverse)

load('../data/cal1020.cleaned.Rdata')

cal1020 %>% 
  ggplot(aes(x=age, y=time.sec, color=sex))+
  geom_point()+
  geom_smooth(method='lm')


lm(data = cal1020, time.sec ~ age*sex)


library(lme4)

m <- lmer(data = cal1020, time.sec ~ age + (age|sex))
summary(m)
ranef(m)


lm(data = cal1020, time.sec ~ age+sex)

m <- lmer(data = cal1020, time.sec ~ age + (1|sex))

# sampling: person, time, score, 

# personality: person, neur, open, big five
left_join(sampling, personality)

# : person, time, score, neur, open, big five

lmer(score ~ time + neur + open + (1|person))

lmer(score ~ day*drug + (day|person))

# when to vary slopes intercepts

# factorial design
# subjects x items
# A is within subject
# B is within item 

lmer(y ~ A*B + (B|item) + (A|subject) + (A+B|item:subject))

# each person drinks 1 cup of something, then recognizes 100 words
# drinks another cup of something then recognizes 100 words
# drink (within subject, within item)
# 5o words are nounds 50 verbs  (between item, within subject)
# subjects are male/female (sex between subject, within item)

y ~ drink*pos*sex + (drink*pos|subject) + (sex*drink|item) + (1|item:subject)

(item|subject)

(drink*pos|subject)

ranef(m)$subject
S1 (intercept)
S2 drink_coffee
S3 pos_verb
S4 drink_coffee:pos_verb

(1|subject)
(1|subject:drink)
(1|subject:pos)
(1|subject:drink:pos)

