phone.plans <- data.frame(
carrier = c('Sprint','AT&T','AT&T','Sprint','Verizon','AT&T','Verizon','Sprint','Verizon','AT&T','Verizon', 'Sprint','AT&T','Verizon','Sprint', 'AT&T', 'Verizon', 'Sprint', 'AT&T'),
GB = c(40,30,25,24,24,16,16,12,12,10,8,6,6,4,3,3,2,1,1),
Cost = c(100,135,110,80,110,90,90,60,80,80,70,45,60,50,30,40,35,20,30))

library(tidyverse)

phone.plans %>%
  ggplot(aes(x=GB, y=Cost, fill=carrier))+
  scale_fill_manual(values=c('Sprint' = '#CCCC00', 'AT&T'='#222299', 'Verizon'='#990000'))+
  geom_point(size=5, shape=21, alpha=0.7)+
  scale_y_continuous(breaks=c(0, 25, 50, 75, 100, 125))+
  scale_x_continuous(breaks=c(0, 10, 20, 30, 40, 50))+
  theme_minimal() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=14),
        legend.title = element_text(size=16))
