load(url('http://vulstats.ucsd.edu/data/cal1020.cleaned.Rdata'))

### Distribution of Pace in Min for Males and Females from California Ages 30 to 35"
cal1020%>%
  filter(State=='CA' & age==30:35)%>%
  ggplot(aes(x=pace.min, fill=sex))+
  geom_density(alpha=.2)+
  xlab('Pace(minutes)') + ylab('density count')+
  ggtitle("Distribution of Pace in Min for Males and Females from California Ages 30 to 35")



#Distribution of males 30 to 35 in CA [note: I wasn’t sure how to mark on the graph where you fell…did that in a separate program]
subrun = cal1020 %>%
  filter(sex=='male' & age==30:35 & State=='CA') 

ggplot(subrun,aes(x=pace.min))+
  geom_density(binwidth=1,fill='blue')+
  xlab('Pace(minutes)') + ylab('density count')+
  ggtitle("Distribution of Pace in Min for Males from California Ages 30 to 35")
 



###Distribution of pace for males vs females 
ggplot(cal1020,aes(x=pace.min,group=sex,fill=sex))+
  xlab('Pace(minutes)') + ylab('density count') + 
  geom_density(binwidth=1,alpha=.2)+
  ggtitle("Pace (in minutes) by Gender”)



###Graph of speed as a function of age 
ggplot(cal1020,aes(x=age,y=pace.min))+
  geom_histogram(width=1, stat='identity', fill='darkgreen') +
  xlab('Age(years)') + ylab('Pace (minutes)') + 
  scale_fill_brewer(palette='Set2')+  theme(text = element_text(size=15))+
  ggtitle("Pace (in minutes) by Age”)




#Graph as speed as a function of corral [this one is ugly…wasn’t sure a better way to visualize it…feel free to use this as an ‘ugly’ example :)]
ggplot(cal1020,aes(x=corral,y=pace.min))+
  geom_bar(stat='identity', fill='darkgreen') +
  xlab('Corral') + ylab('Pace (minutes)') + 
  scale_x_discrete(breaks=c('1','2','3','4','5','6','7','8','9','10'))
  scale_fill_brewer(palette='Set2')+  theme(text = element_text(size=15))+
  ggtitle("Pace (in minutes) by Corral”)
