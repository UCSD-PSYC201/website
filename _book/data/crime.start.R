rm(list=ls())
library(rjags)
library(BEST)
library(PSYC201)
library(reshape)

# read in data.
dat = read.csv(url('http://vulstats.ucsd.edu/data/crimestats.csv'))
names(dat) <- c('State', 'City', 'Population', 'Murder', 'Rape', 'Robbery', 'Assault', 'Burglary', 'Theft', 'GTA')
# take a look at data shape...
head(dat)

# reshape data into long form (using reshape package)
library(reshape)
dat2 = melt(dat, id=c('State', 'City', 'Population'), variable_name="Crime")
names(dat2)[5] <-"Count"
head(dat2)

# some data cleaning
missing = is.na(dat2$State) | is.na(dat2$City) | dat2$Population<1 | is.na(dat2$Crime) | is.na(dat2$Count)
# how many?
sprintf("%d of %d:   %0.2f%%", sum(missing), length(missing), 100*sum(missing)/length(missing))
# remove missing data.
dat2 = dat2[!missing,]
dat2$Crime <- as.factor(as.character(dat2$Crime)) # refactoring to make factor levels alphabetical
dat2$State.City = as.factor(paste(dat2$State, dat2$City, sep=':')) # create a unique state:city identifier 
head(dat2)
dat2$State <- as.factor(dat2$State) # make state a factor, explicitly

# let's look at these data.
hist(log10(dat2$Population))
# stupid hack to see the zeros as -2 on the log plot, and later.  
# we'll need to adopt a better strategy for an actual analysis.
hist(log10(dat2$Count+0.01))   
plot(log10(dat2$Population), log10(dat2$Count+0.01), pch='.')
plot(log10((dat2$Count+0.01)/dat2$Population)~dat2$Crime)
plot(log10((dat2$Count+0.01)/dat2$Population)~dat2$State)

# making our data JAGS friendly.
# convert factors to level numbers, and save a vector of factor levels, to be able to look them up later.
states <- levels(dat2$State)
dat2$state.id <- as.numeric(dat2$State)
crimes <- levels(dat2$Crime)
dat2$crime.id <- as.numeric(dat2$Crime)
cities <- levels(dat2$State.City)
dat2$city.id <- as.numeric(dat2$State.City)
head(dat2)

# to make for more rapid model development, let's try a subsample of data
dat2.small = dat2[sample(nrow(dat2), 2000, replace=F),]

# let's start with the basic poisson regression
model = "model{
  B1 ~ dnorm(0,0.01)
  B0 ~ dnorm(0, 0.0001)
  for(i in 1:length(count)){
    count[i] ~ dpois( exp( log(population[i])*B1+B0 ) )
  }
}"
writeLines(model , con="model.txt")
dat.list = list('population'=dat2.small$Population, 
               'count'=dat2.small$Count)
sampler <- jags.model(
  'model.txt', 
  data = dat.list,
  n.chains = 1, n.adapt=1000)
update(sampler, 100)
samples.raw = coda.samples(sampler, c('B1', 'B0'), 1000)
samples = flattenMCMC(samples.raw)

# now we should gradually add
# 1. difference in intercept as a function of crime?
# 2. difference in intercept as a function of state?
# 3. difference in slope as a function of crime.?
# 4. crime-state interaction on the intercept ???
# 5. random effect of state.city???
# 6. random effect of state???
# The tricky part will be interpreting the emergent coefficients
# for some of the simpler models glm(, family="poisson") will work just fine.