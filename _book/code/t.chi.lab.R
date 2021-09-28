# using Pearson's father,son data set, calculate

father.son = read.csv(url('http://vulstats.ucsd.edu/data/Pearson.csv'))

# 1. change in height from fathers to sons, and ask if it is significant 
# (using just the first 120)

father.son$difference = father.son$Son - father.son$Father
t.test(father.son$difference[1:120])

# 2. using all the data, see if the son-father difference differs for 
# tall (>80th percentile) and short (<20th percentile) fathers.
percentile.80 = quantile(father.son$Father, 0.8)
percentile.20 = quantile(father.son$Father, 0.2)
difference.80 = father.son$difference[father.son$Father > percentile.80]
difference.20 = father.son$difference[father.son$Father < percentile.20]
t.test(difference.20, difference.80)
m.x = mean(difference.20)
m.y = mean(difference.80)
s.x = sd(difference.20)
s.y = sd(difference.80)
# equal variance
df.x = length(difference.20)-1
df.y = length(difference.80)-1
s.p = sqrt((s.x^2*df.x + s.y^2*df.y)/(df.x+df.y))
d = (m.x-m.y)/s.p
t = d / sqrt(1/(df.x+1) + 1/(df.y+1))
t = (m.x - m.y)/(s.p*sqrt(1/(df.x+1) + 1/(df.y+1)))
t.test(difference.20, difference.80, var.equal = TRUE)
# unequal variance
d = (m.x - m.y) / sqrt((s.x^2 + s.y^2)/2)
t.test(difference.20, difference.80)
alpha = (1-0.75)/2
t.crit = qt(alpha, 423.67) # degrees of freedom from uneq. var t-test
# S +/- t.crit * se{S}
diff.20.80 = mean(difference.20) - mean(difference.80)
se.diff.20.80 = sqrt(s.x^2 / length(difference.20) + s.y^2/length(difference.80))
diff.20.80 + c(-1,1)*abs(t.crit)*se.diff.20.80
t.test(difference.20, difference.80, conf.level = 0.75)



# using the spsp demographics data set, calculate...
spsp = read.csv(url('http://vulstats.ucsd.edu/data/spsp.demographics.cleaned.csv'))

# 1, test null of 50/50 male/female distribution among grads.
table.grad = table(spsp$gender[spsp$stage == 'Grad'])
table.grad = table.grad[(names(table.grad) != 'Transgender')]
binom.test(table.grad, 0.5)
# 2. .... among regular members
table.regular = table(spsp$gender[spsp$stage == 'Regular Member'])
table.regular = table.regular[(names(table.regular) != 'Transgender')]
binom.test(table.regular, 0.5)
# 3. test for independence between male/female and grad/regular/undergrad.
big.table = table(spsp$gender, spsp$stage)
lil.table = big.table[c('Female', 'Male'),c('Grad', 'Regular Member', 'Undergrad')]
chisq.test(lil.table)
# 4. what is the 90% confidence interval on the proportion of white folks in
# SPSP?
binom.test(sum(spsp$ethnicity=='White'), nrow(spsp))
is.white = spsp$ethnicity=='White'
k.white = sum(is.white)
n.total = length(is.white)
p.white = k.white/n.total
se.p.white = sqrt(p.white*(1-p.white)/n.total)
p.white + c(-1,1)*qnorm((1-0.95)/2)*se.p.white
# 5. plot the distribution of ethnicities as a function of "stage"
library(ggplot2)
colors = c('Asian' = hsv(0.5, 0, 0.5),
           'Arab' = rgb(1,0,0.8),
           'Black' = '#FF8888',
           'Latino' = 'blue',
           'Native American' = 'yellow',
           'Other'= 'purple',
           'White' = 'pink'
           )
levels(spsp$ethnicity)
level.order = c('Other', 
                'Asian',
                'Arab', 
                'Native American', 
                'White', 
                'Black', 
                'Latino')
spsp$ethnicity <- factor(spsp$ethnicity, 
                         levels = level.order)
ggplot(spsp, aes(x=stage, fill=ethnicity))+
  geom_bar(position = 'fill')+
  theme_minimal()+
  ggtitle('Ethinicity ~ Stage')+
  ylab('Proportion')+
  scale_fill_manual(values = colors)+
  theme(axis.text.x = 
          element_text(angle = 90, 
                       hjust = 1, 
                       vjust=0.5),
        panel.grid = element_blank())
