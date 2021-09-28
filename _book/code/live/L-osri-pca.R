library(tidyverse)
dat <- read_csv('http://vulstats.ucsd.edu/data/OSRI-cleaned.csv')

masculine = seq(1, 43, by=2)
feminine = seq(2, 44, by=2)

dat[,c(masculine, feminine)] %>% 
  GGally::ggcorr(geom='circle')

pca <- prcomp(dat[,1:44], center=T, scale=T)

summary(pca)

str(pca)

pca$rotation[,1]

dat <- bind_cols(dat, pca$x %>% as_tibble())

dat %>% 
  ggplot(aes(x=PC1, fill=gender))+
  geom_density(alpha=0.5)


dat %>% 
  ggplot(aes(x=PC2, fill=gender))+
  geom_density(alpha=0.5)

tibble(question = 1:44, loading = pca$rotation[,2]) %>% 
  ggplot(aes(question, loading))+
  geom_col()

lm(data = dat, PC2 ~ age) %>% summary()

dat %>% sample_n(1000) %>% 
  ggplot(aes(x=age, y=PC2))+
  geom_point()+
  geom_smooth(method='lm')

dat %>% sample_n(1000) %>% 
  ggplot(aes(x=PC2, fill=religion))+
geom_histogram(alpha=0.5)


pca$rotation[,2]

fa <- factanal(dat[,1:44], factors = 3, rotation = 'none')

tibble(questions = 1:44, loadings = pca$rotation[,2]) %>% 
  ggplot(aes(x=questions, y=loadings))+
  geom_col()

tibble(questions = 1:44, loadings = fa$loadings[,2]) %>% 
  ggplot(aes(x=questions, y=loadings))+
  geom_col()

fa$loadings[,1:2] %>% as_tibble(rownames = 'question') %>% 
  ggplot(aes(x=Factor1, y=Factor2))+
  geom_text(aes(label=question))

pca$rotation[,1:2] %>% 
  as_tibble(rownames = 'question') %>% 
  ggplot(aes(x=PC1, y=PC2))+
  geom_text(aes(label=question))

pca$rotation[,1:2] %>% 
  as_tibble(rownames = 'question') %>% 
  ggplot(aes(x=PC1, y=PC2))+
  geom_text(aes(label=question))

fa <- factanal(dat[,1:44], factors = 10, rotation = 'none')


fa$loadings[,1:2] %>% as_tibble(rownames = 'question') %>% 
  ggplot(aes(x=Factor1, y=Factor2))+
  geom_text(aes(label=question))


fa$uniquenesses
