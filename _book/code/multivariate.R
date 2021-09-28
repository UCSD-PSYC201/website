# multivariate data.
library(tidyverse)
poke <- read_csv('http://vulstats.ucsd.edu/data/Pokemon.csv')
menu <- read_csv('http://vulstats.ucsd.edu/data/menu.csv')

z.score = function(X){
  apply(X, 2, function(x){(x-mean(x))/sd(x)})
}

# part 1: low-d linear projections / latent variables.
# SVD, PCA, PPCA, FA, ICA
n = 100 # number of observations
k = 2   # number of latent dimensions
d = 6   # number of observed dimensions

# true forward loadings:
W <- rbind(c(1,1,0,0,0,0), 
           c(0,0,0,0,1,1)) 
# W <- matrix(rnorm(d*k), ncol=d)

# observable measure stats: noise, scale, mean
noise.sd = rep(3, d)# rep(0, d) # c(5,4,3,2,1)
Y.scale = rep(1, d) # (1:d)^2 #
Y.mean = c(100,0,-100,-100,0,100) #rep(0, d) # c(100,0,-100,-100,0,100) 

L <- matrix(rnorm(k*n), ncol=k) # generate latent scores.
Y <- L%*%W  # project latent onto observed
Y <- Y + matrix(rnorm(n*d,0,noise.sd), ncol=d, byrow=T) # add noise
Y <- Y*matrix(rep(Y.scale, each=n), ncol=d) # scale observed
Y <- Y + matrix(rep(Y.mean, each=n), ncol=d) # add means

# what have we wrought?
cor(Y)
library(GGally)
ggpairs(as.data.frame(Y))

# svd: singular value decomposition of data matrix.
# will be really influenced by the means! (generally: center and scale before using)
a.svd <- svd(Y)
head(a.svd$u, 10) # values on latent dimensions
apply(a.svd$u, 2, sd) # constrained to be unit vectors: apply(a.svd$u, 2, function(x){sum(x^2)}) == 1
apply(a.svd$u, 2, function(x){sum(x^2)})

plot(a.svd$d)  # scale of latent dimensions: sd of latent variables.
plot(a.svd$d^2/sum(a.svd$d^2)) # percent overall variance accounted for by each.
plot(cumsum(a.svd$d^2/sum(a.svd$d^2))) # sumulative percent overall variance accounted for
a.svd$v # loadings: how each latent dimension projects onto observed dimensions.
# e.g., a.svd$v[,1] is how you go from u[,1]*d[1] to y
# how to get the original data?
plot(a.svd$u[,1:2])
y.hat <- a.svd$u %*% diag(a.svd$d) %*% t(a.svd$v)
cor(y.hat[,1], Y[,1]) # should be 1...
# we can do this with just a subset of the latent components, say the top 2:
y.hat <- a.svd$u[,1:2] %*% diag(a.svd$d[1:2]) %*% t(a.svd$v[,1:2])
cor(y.hat[,1], Y[,1]) # should be 1...


# PCA: solving the same problem, often via SVD (or via covariance matrix)
# but build in centering
a.pca <- prcomp(Y, center=T, scale=F) # just svd with centering (scaling not usually default)
plot(a.pca$sdev)
a.pca$rotation
a.pca <- prcomp(poke[,6:11], center=T, scale=T) # just svd with centering (scaling not usually default)
plot(a.pca$sdev^2/sum(a.pca$sdev^2))
round(a.pca$rotation, 2)
summary(a.pca)

str(a.pca$x)
a.pca$x %>% as.tibble() %>% mutate(pname=poke$Name) %>%
  slice(1:25) %>% 
  ggplot(aes(x=PC1, y=PC2))+geom_text(aes(label=pname))
glimpse(poke)

# a slightly different algorithm doing the same thing (eigen decomposition of covariance)
a.pca2 <- princomp(Y) 
plot(a.pca2$sdev)
a.pca2$loadings

# PPCA and factor analysis are generative models, in that they allow for noise
# they also estimate a fixed number of latent components.
library(pcaMethods) # obnoxiously requires bioconductor details to install.

# ppca: k components plus iid noise with the same sd in all dimensions.
a.ppca <- pca(Y, method = 'ppca', nPcs = 2, center=T)
a.ppca@sDev
a.ppca@loadings

# factor analysis: k components plus independent noise with *different* sds in all dimensions
a.fa <- factanal(Y, 2, rotation='none')
# fa with varimax rotation: find rotation of standardized latent variables, 
# such that best-fitting loadings are as sparse as you can get.
a.fa <- factanal(Y, 2, rotation='varimax')

# ICA: linear projection with the assumption of heavy-tailed latent variables.
# useful for *source separation* if we think our measurements are the sum of
# several independent and non-gaussian sources
library(fastICA)
a.ICA <- fastICA(Y, 2)
a.ICA$A

# 2. multivariate analysis.
# MAN(C)OVA: anova, but *responses* are multivariate.
# strategy: find linear combination of response variables that yield largest differences in means


poke <- read_csv('http://vulstats.ucsd.edu/data/Pokemon.csv')
ggpairs(poke[,c(13,6:11)])

Y = as.matrix(poke[,6:11])
T1 <- poke[[3]]
T2 <- poke[[4]]
L1 <- poke[[13]]
m.res <- manova(Y~T1+T2+L1)
summary.manova(m.res)
summary.aov(m.res)
tmp <- summary.manova(m.res)
tmp$Eigenvalues
coef(m.res)
# canonical correlation:
# multivariate X and Y, 
# find linear combinations of X and Y that are correlated
X <- poke[,6:8]
Y <- poke[,9:11]
cancor(X, Y)

# LDA (linear discriminant analysis)
# DFA (discriminant function analysis)
# FDA (fisher's discriminant analysis)
# these ask the backwards question from a mANOVA: 
# how can i use my multivariate data to predict category?

# part 1: low-d linear projections / latent variables.
# SVD, PCA, PPCA, FA, ICA
n = 1000 # number of observations
k = 2   # number of latent dimensions
d = 2   # number of observed dimensions

# true forward loadings:
W <- matrix(rnorm(d*k), ncol=d)

# observable measure stats: noise, scale, mean
noise.sd = rep(0, d)# rep(0, d) # c(5,4,3,2,1)
Y.scale = rep(1, d) # (1:d)^2 #
Y.mean = rep(0, d) # c(100,0,-100,-100,0,100) 

L <- matrix(rnorm(k*n)^3, ncol=k) # generate latent scores. 
Y <- L%*%W  # project latent onto observed
Y <- Y + matrix(rnorm(n*d,0,noise.sd), ncol=d, byrow=T) # add noise
Y <- Y*matrix(rep(Y.scale, each=n), ncol=d) # scale observed
Y <- Y + matrix(rep(Y.mean, each=n), ncol=d) # add means

plot(Y[,1], Y[,2])

a.pca <- prcomp(Y)
plot(a.pca$x)
library(fastICA)
a.ica <- fastICA(Y, 2)
