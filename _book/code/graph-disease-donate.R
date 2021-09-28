library(tidyverse)

dat <- read_csv('../data/diseases-donations.csv')

dat <- dat %>% 
  rename(money = `Money Raised for Cause`) %>% 
  rename(deaths = `Deaths from Disease`)

dat %>% 
  ggplot(aes(x=deaths, y=money)) +
  geom_point(color='red') + 
  ggrepel::geom_text_repel(aes(label=Disease), size=3)+
  xlab('Total US Deaths (log scale)')+
  ylab('Money Raised ($, log sclae)')+
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  coord_cartesian(xlim=c(10^3.7, 10^6))+
  theme_bw()
