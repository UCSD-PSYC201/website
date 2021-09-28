load(url('http://vulstats.ucsd.edu/data/cal1020.cleaned.Rdata'))

vul.idx = which(cal1020$name.last=="Vul")
vul.pace = cal1020[vul.idx, "pace.min"]

# percentile compared to all
with(cal1020, sum(pace.min<=vul.pace)/length(pace.min)*100)

# percentile compared to corral
with(subset(cal1020, cal1020$corral==cal1020$corral[vul.idx]), 
      sum(pace.min<=vul.pace)/length(pace.min)*100)

# ... males
with(subset(cal1020, cal1020$sex=="male"), 
     sum(pace.min<=vul.pace)/length(pace.min)*100)

# ... 30-34 yo males
with(subset(cal1020, cal1020$sex=="male" & cal1020$age>=30 & cal1020$age<35), 
     sum(pace.min<=vul.pace)/length(pace.min)*100)

# ... 30-34 yo males from CA
with(subset(cal1020, cal1020$sex=="male" & cal1020$age>=30 & cal1020$age<35 & cal1020$State=="CA"), 
     sum(pace.min<=vul.pace)/length(pace.min)*100)

