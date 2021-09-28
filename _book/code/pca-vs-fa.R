dat <- read_csv('http://vulstats.ucsd.edu/data/OSRI-cleaned.csv')

names(dat)

pca = prcomp(dat[,1:44], scale=T, center=T)

fa = list()
for(k in 1:10){
  fa[[k]] = factanal(dat[,1:44], k, rotation = 'none', scores='regression')
}

fa.var = list()
for(k in 1:10){
  fa.var[[k]] = factanal(dat[,1:44], k, rotation = 'varimax', scores='regression')
}



masculine = rep(c(0, 1), 22)
feminine = rep(c(1, 0), 22)

Cs = list()
for(component in 1:10){
  Cs[[component]] = data.frame(pc = pca$rotation[,component])
  Cs[[component]][['masculine']] = masculine
  Cs[[component]][['feminine']] = feminine
  for(k in component:10){
    Cs[[component]][[paste0('fa-', k)]] = fa[[k]]$loadings[,component]
    Cs[[component]][[paste0('fa.var-', k)]] = fa.var[[k]]$loadings[,component]
    
  }
}

Cs[[1]] %>% GGally::ggcorr(geom = "circle") + 
  scale_color_gradient2(low = 'red', 
                       high='red', 
                       mid = 'white', midpoint=0)

Cs[[2]] %>% GGally::ggcorr() + 
  scale_fill_gradient2(low = 'red', 
                       high='red', 
                       mid = 'white', midpoint=0)


cor(pca$x[,c(1,2)])

dat[,c(seq(1, 43, by=2), seq(2, 44, by=2))] %>% GGally::ggcorr(geom = "circle")

cor(fa[[2]]$scores[,c(1,2)])

dat[,c(seq(1, 43, by=2), seq(2, 44, by=2))] %>% GGally::ggcorr(geom = "circle")

pca

tibble(pc2 = pca$rotation[,2], 
       f2 = fa$loadings[,2],
       fv2 = fa.var$loadings[,2]) %>% 
  GGally::ggpairs()


fa = factanal(dat[,1:44], 2, rotation = 'none', scores='regression')





mf <- function(x){c(sum(x * masculine), sum(x * feminine))}

mf(pca$rotation[,1])
mf(pca$rotation[,2])
mf(pca$rotation[,3])

mf(fa$loadings[,1])
mf(fa$loadings[,2])
mf(fa$loadings[,3])

mf(fa.var$loadings[,1])
mf(fa.var$loadings[,2])
mf(fa.var$loadings[,3])



pca$x[,c(1:2)] %>% as_tibble() %>% 
  ggplot(aes(x=PC1, y=PC2))+
  geom_point(size=0.1, alpha=0.1)+
  theme_minimal()

fa <- factanal(dat[,1:44], 2, rotation = 'none', scores='regression')

fa$scores %>% as_tibble() %>% 
  ggplot(aes(x=Factor1, y=Factor2))+
  geom_point(size=0.1, alpha=0.1)+
  theme_minimal()

fa.z <- dat %>% select(1:44) %>% mutate_all(function(x){(x-mean(x))/sd(x)}) %>% 
  factanal(2, rotation = 'none', scores='regression')

fa.z$scores %>% as_tibble() %>% 
  ggplot(aes(x=Factor1, y=Factor2))+
  geom_point(size=0.1, alpha=0.1)+
  theme_minimal()


fa.zr <- dat %>% select(1:44) %>% mutate_all(function(x){(x-mean(x))/sd(x)}) %>% 
  factanal(2, rotation = 'varimax', scores='regression')

fa.zr$scores %>% as_tibble() %>% 
  ggplot(aes(x=Factor1, y=Factor2))+
  geom_point(size=0.1, alpha=0.1)+
  theme_minimal()

cor(pca$rotation[,1:2])

t(fa$loadings[,1]) %*% fa$loadings[,2]
