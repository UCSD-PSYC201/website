

library(ggplot2)
library('gridExtra')

load(url('http://vulstats.ucsd.edu/data/cal1020.cleaned.Rdata'))
vul.idx = which(cal1020$name.last=="Vul")
vul.pace = cal1020[vul.idx, "pace.min"]

# ... males
Vul.male<-with(subset(cal1020, cal1020$sex=="male"), 
               sum(pace.min<=vul.pace)/length(pace.min))

male<-subset(cal1020, cal1020$sex=="male")
male.mean<-mean(male$pace.min)
male.mean
male.sd<-sd(male$pace.min)
male.sd


pace=male$pace.min
n=sum(!is.na(pace))
pace.ecdf=ecdf(pace)
pace.ecdf
Vul.male.plot<-plot(pace.ecdf,xlab='pace in minute',ylab='',main='male division')+abline(v=vul.pace, h=Vul.male,col=3,lty=3)



# ... 30-34 yo males
Vul.age<-with(subset(cal1020, cal1020$sex=="male" & cal1020$age>=30 & cal1020$age<35), 
     sum(pace.min<=vul.pace)/length(pace.min))
age<-subset(cal1020, cal1020$sex=="male" & cal1020$age>=30 & cal1020$age<35)
age.mean<-mean(age$pace.min)
age.sd<-sd(age$pace.min)

pace.age=age$pace.min
n=sum(!is.na(pace.age))
pace.age.ecdf=ecdf(pace.age)
pace.age.ecdf

Vul.age.plot<-plot(pace.age.ecdf,xlab='pace in minute',ylab='',main='male 30-34 division')+abline(v=vul.pace, h=Vul.age,col=3,lty=3)


# ... 30-34 yo males from CA
Vul.CA<-with(subset(cal1020, cal1020$sex=="male" & cal1020$age>=30 & cal1020$age<35 & cal1020$State=="CA"), 
     sum(pace.min<=vul.pace)/length(pace.min))
CA<-subset(cal1020, cal1020$sex=="male" & cal1020$age>=30 & cal1020$age<35 & cal1020$State=="CA")
CA.mean<-mean(CA$pace.min)
CA.sd<-sd(CA$pace.min)

pace.CA=CA$pace.min
n=sum(!is.na(pace.CA))
pace.CA.ecdf=ecdf(pace.CA)
pace.CA.ecdf

Vul.CA.plot<-plot(pace.CA.ecdf,xlab='pace in minute',ylab='',main='male 30-34 CA')+abline(v=vul.pace, h=Vul.CA,col=3,lty=3)




#male vs. female speed

male.speed.plot<-ggplot(male,aes(x=speed.mph))+geom_histogram(fill='navy')+ggtitle('speed distribution of the male group')
female<-subset(cal1020,sex=='female')
female.speed.plot<-ggplot(female,aes(x=speed.mph))+geom_histogram(fill='pink')+ggtitle('speed distribution of the female group')

gender.difference<-grid.arrange(male.speed.plot,female.speed.plot)

#speed and corral

speed.corral<-ggplot(cal1020,aes(x=corral,y=speed.mph)) +   
  geom_point()+ggtitle('speed as a function of corral')


#speed and age 
speed.age<-ggplot(cal1020,aes(x=age,y=speed.mph)) +   
  geom_point()+ggtitle('speed as a function of age')

all.four<-grid.arrange(male.speed.plot,female.speed.plot,speed.corral,speed.age, ncol=2)