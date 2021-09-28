rm(list=ls())
load(url('http://vulstats.ucsd.edu/data/cal1020.cleaned.Rdata'))
# drop wheelchairs!
nowheels = subset(cal1020, substr(cal1020$Division, 1, 5)!="Wheel")
nrow(cal1020)-nrow(nowheels)   # how many wheelchairs were dropped?
attach(nowheels)

# is there a linear relationship?
cor(Age, speed.mph) # huh?  Why NA?  

c(sum(is.na(Age)), sum(is.na(speed.mph))) # aha!  5 people with no recorded age!

# option 1: correlation specifying to exclude NAs
cor(Age, speed.mph, use = "complete.obs")

# option 2: subset to drop missing rows
nowheel.nona = subset(nowheels, !is.na(nowheels$Age))
nrow(nowheels)- nrow(nowheel.nona)
cor(nowheel.nona$Age, nowheel.nona$speed.mph)

# let's just stick to using the complete data here...
attach(nowheel.nona)  # things are masked?  Huh?
search()  # oh!  we have all these things attached...  let's detach the other databases
detach(name="nowheels")
search()  # cleaner

# ok, we have a correlation, is it significant?
cor.test(Age, speed.mph)

# let's consider the line that this fits...
fit <- lm(speed.mph~Age)
summary(fit) 
# note -- p value for slope is the same as p value for cor.test -- they test the same thing!
# note -- t stat is same as correlation -- they are the same.  F stat is t^2

# let's plot the linear fit, and look at some diagnostics

# option 1 (using plot, abline)
plot(Age, speed.mph) # eww.  that's ugly, let's clean it up
plot(Age, speed.mph, pch=20) # woah... still hard too see density
plot(Age, speed.mph, pch=20, cex=0.1) # doesn't really help...
plot(Age, speed.mph, pch=20, col=rgb(0, 0, 0)) # this is just black
plot(Age, speed.mph, pch=20, col=rgb(0, 0, 0, 0.15)) # add transparency)
plot(Age, speed.mph, pch=20, cex=1.75, col=rgb(0, 0, 0, 0.1)) # more polish
abline(fit) # add a line
abline(fit, col="red")
abline(fit, col="red", lwd=4)
# ok -- so what can we tell fro this plot?
# - variance seems higher at younger ages

# option 2 (using qplot)
library(ggplot2)
qplot(Age, speed.mph)
qplot(Age, speed.mph) + theme_bw()  # using bw ggplot theme
qplot(Age, speed.mph, size=I(1.5), position=position_jitter(w=0.5, h=0)) + theme_bw()
qplot(Age, speed.mph, size=I(5), ) + theme_bw()
qplot(Age, speed.mph, size=I(3), alpha=I(0.3), position=position_jitter(w=0.5, h=0)) + theme_bw()
# let's add a line to this -- 
qplot(Age, speed.mph, size=I(3), alpha=I(0.3), position=position_jitter(w=0.5, h=0)) + 
  theme_bw() +
  geom_smooth(method=lm)
# ugly -- blue, size is inherited from our big points... but neat confidence bounds.
# note -- this method does it's own linear model fit, so it won't work later...
qplot(Age, speed.mph, size=I(3), alpha=I(0.3), position=position_jitter(w=0.5, h=0)) + 
  theme_bw() +
  geom_smooth(method=lm, size = 2, colour="red", fill="red") # annoying british spelling

# let's look at some diagnostic plots
plot(fit, pch=20, cex=1.75, col=rgb(0, 0, 0, 0.1), lwd=3, which=1)
# non-linearity seems minimal, but a bit quadratic.
plot(fit, pch=20, cex=1.75, col=rgb(0, 0, 0, 0.1), lwd=3, which=2)
# this indicates a positive skew in residuals... evident in scatter.
plot(fit, pch=20, cex=1.75, col=rgb(0, 0, 0, 0.1), lwd=3, which=5)
# nothing weird here...
plot(fit, pch=20, cex=1.75, col=rgb(0, 0, 0, 0.1), lwd=3, which=4)
# and no points with undue influence!  yay.

# -- ok, so we have some skewed residuals, and maybe some non-linearity...

# what about a rank-order relationship?
cor.test(Age, speed.mph, method="spearman")

# is there a quadratic relationship?
summary(lm(speed.mph~Age + Age^2))  # this doesn't work
# We can do:
summary(lm(speed.mph~Age + I(Age^2)))  # this doesn't work
# Or:
Age.sq = Age^2
summary(lm(speed.mph~Age + Age.sq))  # this doesn't work

# ok...
Age.sq = Age^2
fit = lm(speed.mph~Age + Age.sq)

# how to plot quadratic fit?  We have to generate line ourselves.
A = 12:83
S = coef(fit)[1] + coef(fit)[2]*A + coef(fit)[3]*A^2

# option 1: add line to plot with lines()
plot(Age, speed.mph, pch=20, cex=1.75, col=rgb(0, 0, 0, 0.1))
lines(A, S, lwd=4, col="red")

# option 2: add line to qplot with geom_line
qplot(Age, speed.mph, size=I(3), alpha=I(0.3), position=position_jitter(w=0.5, h=0)) + 
  theme_bw() +
  geom_line(aes(x=A, y=S), size = 2, colour="red")
# let's look at some diagnostic plots

# let's look at some diagnostic plots
plot(fit, pch=20, cex=1.75, col=rgb(0, 0, 0, 0.1), lwd=3, which=1)
# non-linearity seems minimal, but a bit quadratic.
plot(fit, pch=20, cex=1.75, col=rgb(0, 0, 0, 0.1), lwd=3, which=2)
# this indicates a positive skew in residuals... evident in scatter.
plot(fit, pch=20, cex=1.75, col=rgb(0, 0, 0, 0.1), lwd=3, which=5)
# nothing weird here...
plot(fit, pch=20, cex=1.75, col=rgb(0, 0, 0, 0.1), lwd=3, which=4)

# ok... a preview of 201b
# a few different lines:
fit = lm(speed.mph~Age)
fit.sq = lm(speed.mph~Age+Age.sq)
fit.exp = lm(log10(speed.mph)~Age)
fit.exp.sq = lm(log10(speed.mph)~Age+Age.sq)
lA = log10(Age)
lA.sq = lA^2
fit.pow = lm(log10(speed.mph)~lA)
fit.pow.sq = lm(log10(speed.mph)~lA+lA.sq)

A = 0:100
plot(Age, speed.mph, pch=20, cex=1.75, col=rgb(0, 0, 0, 0.1))
lines(A, coef(fit)[1]+coef(fit)[2]*A, lwd=4, col="red")
lines(A, coef(fit.sq)[1]+coef(fit.sq)[2]*A+coef(fit.sq)[3]*A^2, lwd=4, col="purple")
lines(A, 10^(coef(fit.exp)[1]+coef(fit.exp)[2]*A), lwd=4, col="green")
lines(A, 10^(coef(fit.exp.sq)[1]+coef(fit.exp.sq)[2]*A+coef(fit.exp.sq)[3]*A^2), lwd=4, col="blue")
lines(A, 10^(coef(fit.pow)[1]+coef(fit.pow)[2]*log10(A)), lwd=4, col="orange")
lines(A, 10^(coef(fit.pow.sq)[1]+coef(fit.pow.sq)[2]*log10(A)+coef(fit.pow.sq)[3]*log10(A)^2), lwd=4, col="brown")

# smoothed fit
qplot(Age, speed.mph, size=I(3), alpha=I(0.3), position=position_jitter(w=0.5, h=0)) + 
  theme_bw() +
  geom_smooth(method=loess, size = 3, colour="red", fill="red") +
  geom_line(aes(x=A, y=S), size = 1, colour="blue")


