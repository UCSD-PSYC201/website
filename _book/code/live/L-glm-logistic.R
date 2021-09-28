logistic = function(z){1/(1+exp(-z))}

b0 = -53.839
b1 = 0.7174

height = 60
logodds = b0 + b1*height

logistic(logodds)

odds = exp(logodds)
odds / (1+odds)

logistic(b0 + b1*72)

logistic(b0 + b1*(5*12 + 9)) - logistic(b0 + b1*(5*12 + 8))


logistic(b0 + b1*(6*12 + 5)) - logistic(b0 + b1*(6*12 + 4))


logistic(b0 + b1*(7*12 + 1)) - logistic(b0 + b1*(7*12 + 0))


library(tidyverse)
load(url("http://vulstats.ucsd.edu/data/fieldgoals.Rdata"))
glimpse(fieldgoals)

M1 = glm(success ~ yardage + week, 
         data = fieldgoals, 
         family = binomial())

summary(M1)
anova(M1, test = 'LRT')


M2 = glm(success ~ yardage * week, 
         data = fieldgoals, 
         family = binomial())

anova(M1, M2, test = 'LRT')

AIC(M1)
AIC(M2)

sum_fieldgoals = fieldgoals %>% 
  mutate(bin = ntile(yardage, 20)) %>% 
  group_by(bin) %>% 
  summarize(n = n(),
            yardage = mean(yardage),
            p = mean(success),
            se_p = sqrt(p*(1-p)/n))

yardage = 40


# predictions
betas <- coef(M1)
logistic = function(z){1/(1+exp(-z))}

coef(M1)['(Intercept)'] + 
  coef(M1)['yardage']*40
predict(M1, 
        data.frame(yardage = 40, 
                   week = 0))


coef(M1)['(Intercept)'] + 
  coef(M1)['yardage']*40+
  coef(M1)['week']*mean(fieldgoals$week)
predict(M1, 
        data.frame(yardage = 40, 
                   week = mean(fieldgoals$week)))

M0 = glm(success ~ yardage, 
         data = fieldgoals, 
         family = binomial())
coef(M0)['(Intercept)'] + 
  coef(M0)['yardage']*40


logistic = function(z){1/(1+exp(-z))}

logistic(
  coef(M1)['(Intercept)'] + 
  coef(M1)['yardage']*40+
  coef(M1)['week']*mean(fieldgoals$week))
predict(M1, 
        data.frame(yardage = 40, 
                   week = mean(fieldgoals$week)),
        type = 'response')



newdata = data.frame(yardage = 30, week = 0)

logistic(betas['(Intercept)'] + 
           betas['yardage']*40 + 
           betas['week']*mean(fieldgoals$week))

prediction = data.frame(yardage = 10:70) %>% 
  mutate(predicted_p = logistic(betas['(Intercept)'] + 
                                  betas['yardage']*yardage + 
                                  betas['week']*mean(fieldgoals$week)))

fieldgoals %>% 
  ggplot(aes(x = yardage, y = success))+
  geom_point(position = position_jitter(height = 0.05), alpha=0.5)+
  geom_pointrange(data = sum_fieldgoals, 
                  aes(x = yardage, y=p, ymax = p+se_p, ymin=p-se_p),
                  color='blue')+
  geom_line(data = prediction, aes(x=yardage, y=predicted_p), color='red')

# consider the two predictions below.
# what happens if you remove the type='response' argument?  What do you get?
predict(M1, 
        data.frame(yardage = 10:70, 
                   week = 0), 
        type = 'response')
predict(M1, 
        data.frame(yardage = 10:70, 
                   week = mean(fieldgoals$week)), 
        type = 'response')

logistic(6.3)


