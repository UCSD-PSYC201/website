
fa <- factanal(dat[,c(masculine, feminine)]+rnorm(nrow(dat)*44, 0, 0.5), factors = 2, rotation = 'none')


fa$loadings[,1:2] %>% as_tibble(rownames = 'question') %>% 
  ggplot(aes(x=Factor1, y=Factor2))+
  geom_text(aes(label=question))


fa$loadings[,1:2] %>% as_tibble(rownames = 'question') %>% 
  pivot_longer(-question, names_to='factor', values_to='loading') %>% 
  ggplot(aes(x=question, y=loading))+
  facet_grid(factor~.)+
  geom_col()
  
noise = 0.5
fa <- factanal(dat[,1:44]+rnorm(nrow(dat)*44, 0, noise), 
               factors = 2, rotation = 'varimax', scores = 'regression')

bind_cols(dat, fa$scores %>% as_tibble()) %>% 
  ggplot(aes(x=Factor1, y=Factor2, color=gender))+
  geom_point(size=0.1, alpha=0.1)


