rm(list=ls())
library(tidyverse)
library(lme4)
library(arm)
load(url('http://vulstats.ucsd.edu/data/crime.data.Rdata'))

m0 = glm(data=crime.data, family=poisson(), 
         Count~log(Population))
m1 = glmer(data=crime.data, family=poisson(), 
           Count~log(Population)+(1|Crime), 
           REML=FALSE)
m2 = glmer(data=crime.data, family=poisson(), 
           Count~log(Population)+(1|Crime)+(1|State), 
           REML=FALSE)
m3 = glmer(data=crime.data, family=poisson(), 
           Count~log(Population)+(1+log(Population)|Crime)+(1|State), 
           REML=FALSE)
m4 = glmer(data=crime.data, family=poisson(), 
           Count~log(Population)+(1+log(Population)|Crime)+(1|State)+(1|State:Crime), 
           REML=FALSE)

anova(m1,m2,m3,m4)

save('m0', 'm1', 'm2', 'm3', 'm4', file="mdata.Rdata") # helpful to not have to refit all the models, as they take some time.

#m1 crime population slopes
slopes = coef(m1)$Crime  # get intercept + population slope by crime type
# make a data frame of predictions for ggplot
population = 10^seq(1, 7, by=0.1)
d = do.call(cbind, lapply(1:nrow(slopes), function(i){data.frame(exp(slopes[i,1]+slopes[i,2]*log(population)))}))
names(d) <- rownames(slopes)
d = melt(d)
d[,"population"] = rep(population, nrow(slopes))
names(d) <- c("Crime", "Count", "Population")
# plot on log-log coordinates
ggplot(data=d, aes(x=Population, y=Count, color=Crime, group=Crime))+geom_line()+scale_x_log10()+scale_y_log10()   # expected count
ggplot(data=d, aes(x=Population, y=Count/Population, color=Crime, group=Crime))+geom_line()+scale_x_log10()+scale_y_log10()   # expected rate

###########
#m2 state effects.
rs = cbind(ranef(m2)$State, se.ranef(m2)$State)  # get state random effect coefficients and their standard errors.
rs = rs[order(rs[,1]),]  # order them high to low.
# plot
dotchart(rs[,1], labels = rownames(rs))
segments(rs[,1]-rs[,2], 1:nrow(rs), rs[,1]+rs[,2], 1:nrow(rs))

###########
#m3 crime population slopes
slopes = coef(m3)$Crime  # get intercept + population slope by crime type
# data frame for ggplot
population = 10^seq(1, 7, by=0.1)
d = do.call(cbind, lapply(1:nrow(slopes), function(i){data.frame(exp(slopes[i,1]+slopes[i,2]*log(population)))}))
names(d) <- rownames(slopes)
d = melt(d)
d[,"population"] = rep(population, nrow(slopes))
names(d) <- c("Crime", "Count", "Population")
# plot on log-log coordinates
ggplot(data=d, aes(x=Population, y=Count, color=Crime, group=Crime))+geom_line()+scale_x_log10()+scale_y_log10()
ggplot(data=d, aes(x=Population, y=Count/Population, color=Crime, group=Crime))+geom_line()+scale_x_log10()+scale_y_log10()




###########
#m4 exploration
r = cbind(ranef(m4)$State, se.ranef(m4)$State) # state random effects
r[,"State"] = rownames(r)

rs = cbind(ranef(m4)$`State:Crime`, se.ranef(m4)$`State:Crime`) # state:crime random effects.
# extract state and crime labels from State:Crime rownames.
rs[,"State"] = unlist(lapply(strsplit(rownames(rs), ':'), function(u){u[[1]]}))
rs[,"Crime"] = unlist(lapply(strsplit(rownames(rs), ':'), function(u){u[[2]]}))

# look at dotcharts for individual state:crime offsets.
srs = subset(rs, rs$Crime=="Burglary")
srs = srs[order(srs[,1]),]
dotchart(srs[,1], labels = srs$State)
segments(srs[,1]-srs[,2], 1:nrow(srs), srs[,1]+srs[,2], 1:nrow(srs))

# look at dotcharts for state + state:crime for a particular crime.
R = cbind(srs, r)
R[,"total"] = R[,1]+R[,5]
R[,"total.se"] = sqrt(R[,2]^2+R[,6]^2)
R = R[order(R[,"total"]),]
dotchart(R$total, labels = R$State)
segments(R$total-R$total.se, 1:nrow(R), R$total+R$total.se, 1:nrow(R))

# correlation matrix of state:crime coefficients and state coefficient.
t = rs[,c(1,3,4)]
names(t)[1] = "I"
rownames(t) = NULL
t = reshape(t, idvar = "State", timevar="Crime", direction = "wide") # cast to wide format
t[,"I.Overall"] = r[,1]  # add overall state effect.
# t[,2:8] = t[,2:8]+t[,9] # if we want correlation matrix of (state+state:crime), for illustration
round(cor(t[,2:9]),2) # print correlation matrix

# get correlation matrix into data frame for ggplot
cormat = cor(t[,2:9])
diag(cormat)=NA
cormat = data.frame(cormat)
cormat[,"row"] = rownames(cormat)
cormat = reshape(cormat, idvar = "row", timevar="col", varying = names(cormat)[1:8], direction="long")
cormat$row = as.factor(cormat$row)
cormat$col = as.factor(cormat$col)
levels(cormat$row) <- substr(levels(cormat$row), 3, 20)

# order to make plot easier to read.
o = c("Overall", "Murder", "Assault", "Robbery", "Burglary", "GTA", "Rape", "Theft")
cormat$row = factor(cormat$row, o)
cormat$col = factor(cormat$col, o)
names(cormat)[3] = "correlation"

# plot heatmap.
base_size = 20
ggplot(cormat, aes(row, col)) + geom_tile(aes(fill = correlation), colour = "white") + scale_fill_gradient2(low = "#2222FF", mid = "white", high = "#FF2222", midpoint=0) +
  theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + 
  theme(legend.position = "right", axis.ticks = element_blank(), axis.text.x = element_text(size = base_size*0.8, angle = 330, hjust = 0, colour = "grey50"))
