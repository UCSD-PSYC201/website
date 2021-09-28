library(tidyverse)
library(gridExtra)
dat <- read_tsv('../data/HPI_EXP_metro.txt')
dat <- dat %>% mutate(yr.pt = yr+(qtr-1)/4)
dat <- dat %>% 
  group_by(metro_name) %>% 
  mutate(log.yoy = log(index_nsa/lag(index_nsa, 4)),
         apr = (index_nsa/lag(index_nsa, 4)-1)*100,
         net.apr = ((index_nsa/index_nsa[yr.pt==1991])^(1/(yr.pt-1991))-1)*100) %>% 
  ungroup()   %>% 
  group_by(yr.pt) %>% 
  mutate(m.log.yoy = mean(log.yoy),
         m.apr = (exp(m.log.yoy)-1)*100) %>% 
  ungroup() %>% 
  mutate(d.log.yoy = log.yoy-m.log.yoy)

yr.start = 1992
yr.end = 2004
cbind(dat %>% filter(yr.pt >=yr.start, yr.pt <= yr.end) %>%
  filter(yr.pt %% 1 == 0) %>%
  select(metro_name, yr.pt, d.log.yoy) %>%
  spread(key=yr.pt, value=d.log.yoy) %>%
  summarize_if(is_numeric, var) %>%
  gather(key=yr.pt, val=var.d.log.yoy) %>%
  summarize(n=n(), sum.var.d.log.yoy = sum(var.d.log.yoy)), 
dat %>% filter(yr.pt >=yr.start, yr.pt <= yr.end) %>%
  filter(yr.pt %% 1 == 0) %>%
  select(metro_name, yr.pt, d.log.yoy) %>%
  group_by(metro_name) %>%
  summarize(sum.d.log.yoy = sum(d.log.yoy)) %>%
  ungroup() %>%
  summarize(var.sum.d.log.yoy = var(sum.d.log.yoy)))

cor.mat <- dat %>% filter(yr.pt >=yr.start, yr.pt <= yr.end) %>%
  filter(yr.pt %% 1 == 0) %>%
  select(metro_name, yr.pt, d.log.yoy) %>%
  spread(key=yr.pt, value=d.log.yoy) %>% 
  select(-metro_name) %>% 
  as.matrix() %>% 
  cor()

m.r = c()
s.r = c()
idx = abs(row(cor.mat)-col(cor.mat))
for(i in 1:(dim(cor.mat)[1]-1)){
  m.r[i] = mean(cor.mat[idx == i])
  s.r[i] = sd(mean(cor.mat[idx == i]))
}
plot(m.r)

metros <- dat$metro_name %>% unique()

plotter(42)

plotter = function(showmetro){
  g1 <- dat %>% ggplot(aes(x=yr.pt, y=index_nsa, color=metro_name))+
    geom_line()+
    geom_line(data = dat %>% group_by(yr.pt) %>% summarise(m.index_nsa = mean(index_nsa)), 
              aes(y=m.index_nsa), color='black', size=2)+
    geom_line(data = dat%>%filter(metro_name==metros[showmetro]), size=2, color='blue')+
    geom_label(data = dat%>%filter(metro_name==metros[showmetro], yr.pt==2007),
               aes(label=metro_name), color='blue')+
    theme_minimal()+
    theme(legend.position = 'none')+
    ylab('Price normed to 1991=100')+
    xlab('year')
  
  
  g2 <- dat %>% ggplot(aes(x=yr.pt, y=net.apr, color=metro_name))+
    geom_line()+
    geom_line(data = dat %>% group_by(yr.pt) %>% summarise(m.net.apr = mean(net.apr)), 
              aes(y=m.net.apr), color='black', size=2)+
    geom_line(data = dat%>%filter(metro_name==metros[showmetro]), size=2, color='blue')+
    geom_label(data = dat%>%filter(metro_name==metros[showmetro], yr.pt==2007),
               aes(label=metro_name), color='blue')+
    theme_minimal()+
    theme(legend.position = 'none')+
    ylab('Net annualized appreciation since 1991')+
    xlab('year')
  
  
  g3 <- dat %>% ggplot(aes(x=yr.pt, y=apr, color=metro_name))+
    geom_abline(slope=0, intercept=0, color='black')+
    geom_line()+
    geom_line(data = dat %>% group_by(yr.pt) %>% summarise(m.apr = mean(m.apr, na.rm=T)),
              aes(y=m.apr), color='black', size=2)+
    geom_line(data = dat%>%filter(metro_name==metros[showmetro]), size=2, color='blue')+
    geom_label(data = dat%>%filter(metro_name==metros[showmetro], yr.pt==2007),
               aes(label=metro_name), color='blue')+
    theme_minimal()+
    theme(legend.position = 'none')+
    ylab('YoY Appreciation')+
    xlab('year')
  
  
  g4 <- dat %>% ggplot(aes(x=yr.pt, y=d.log.yoy, color=metro_name))+
    geom_line()+
    geom_line(data = dat %>% group_by(yr.pt) %>% summarise(m.d.log.yoy = mean(d.log.yoy, na.rm=T)),
              aes(y=m.d.log.yoy), color='black', size=2)+
    geom_line(data = dat%>%filter(metro_name==metros[showmetro]), size=2, color='blue')+
    geom_label(data = dat%>%filter(metro_name==metros[showmetro], yr.pt==2007),
               aes(label=metro_name), color='blue')+
    theme_minimal()+
    theme(legend.position = 'none')+
    ylab('diff from mean (log yoy ratio)')+
    xlab('year')
  
  grid.arrange(g1,g2,g3,g4, ncol=2)
}
