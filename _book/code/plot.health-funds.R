library(tidyverse)
lines = "ailment, deaths, funds
breast cancer, 41374, 257.9
prostate cancer, 21176, 147
heart disease, 596577, 54.1
motor neuron (ALS), 6849, 22.9
AIDS, 7683, 14
pulmonary disease, 142942,7
diabetes, 73831, 4.2
suicide, 39518, 3.2"
con <- textConnection(lines)
df <- read.csv(con, stringsAsFactors = FALSE)
         
df %>% ggplot(aes(x=deaths, y=funds*1e6, label=ailment))+
  geom_point()+
  geom_text()+
  scale_y_log10()+
  scale_x_log10()+
  theme_minimal()
