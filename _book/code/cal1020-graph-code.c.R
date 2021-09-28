load(url('http://vulstats.ucsd.edu/data/cal1020.cleaned.Rdata'))

str(cal1020)

vul.idx=which(cal1020$name.last=="Vul")

library(PSYC201)
library(ggplot2)

cal1020$poi = 'Folks';
cal1020[vul.idx,]$poi = 'Vul'

p <- ggplot(cal1020, aes(sex, pace.min, colour = factor(poi)))
p + geom_point()

p <- ggplot(cal1020, aes(age, pace.min, colour = factor(poi)))
p + geom_point()

p <- ggplot(cal1020, aes(age, pace.min, colour = factor(poi)))
p + geom_point()+ ggtitle('Gender difference') + facet_grid(sex ~ .)

p <- ggplot(cal1020, aes(age, pace.min, colour = factor(poi)))
p + geom_point()+ ggtitle('By corral') + facet_grid(.~ corral)

p <- ggplot(cal1020, aes(sex, pace.min, colour = factor(poi)))
p + geom_point()+ ggtitle('By age') + facet_grid(.~age)



