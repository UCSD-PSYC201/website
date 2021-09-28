# manova
library(tidyverse)
means = rbind(c(0,0), c(1,1))
rownames(means) <- c('A', 'B')
n = 10
s = 0.01
y1 = rnorm(n*2)
y2 = -1*y1
y1 = y1+rnorm(n*2)*s
y2 = y2+rnorm(n*2)*s

df <- data.frame(category = c(rep('A', n), rep('B', n)),
           y1,y2)
df <- df %>% mutate(y1 = y1+means[category,1],
                    y2 = y2+means[category,2])
df %>% ggplot(aes(x=y1,y=y2,color=category))+geom_point()

m.res = manova(data=df, cbind(y1,y2)~category)
summary.aov(m.res)
summary.manova(m.res)
