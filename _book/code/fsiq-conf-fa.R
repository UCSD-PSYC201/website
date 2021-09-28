library(tidyverse)


fsiq = read_csv('../data/FSIQ/data.csv')


fsiq = fsiq %>% select(-(dateload:endelapse)) %>% 
  mutate(subject = 1:n()) %>% 
  pivot_longer(-subject, names_to = 'question', values_to = 'score') %>% 
  separate(question, into=c('null', 'type', 'Q', 'num', 'var'), sep='') %>% 
  filter(var != "a") %>% 
  select(-null, -Q)


fsiq <- fsiq %>% filter(var == 's') %>% 
  unite(question, type, num, var, sep='') %>% 
  pivot_wider(subject, names_from = question, values_from = score)

pca <- fsiq %>% select(-subject) %>% prcomp(scale=T, center=T)

summary(pca)
pca$rotation

factanal(fsiq %>% select(-subject), factors = 3, rotation = 'varimax')


library(lavaan)

# V1s + V2s + V3s + V4s + V5s + V6s + V7s + R1s + R2s + R3s + R4s + R5s + R6s + M1s + M2s + M3s + M4s + M5s + M6s
formula1_3 = '
overall =~ verbal + memory + rotation
verbal =~ V1s + V2s + V3s + V4s + V5s + V6s + V7s
rotation =~ R1s + R2s + R3s + R4s + R5s + R6s
memory =~ M1s + M2s + M3s + M4s + M5s + M6s
'

formula1 = '
overall =~ V1s + V2s + V3s + V4s + V5s + V6s + V7s + R1s + R2s + R3s + R4s + R5s + R6s + M1s + M2s + M3s + M4s + M5s + M6s
'

formula3 = '
verbal =~ V1s + V2s + V3s + V4s + V5s + V6s + V7s
rotation =~ R1s + R2s + R3s + R4s + R5s + R6s
memory =~ M1s + M2s + M3s + M4s + M5s + M6s
'

fsiq_1_3 <- sem(formula1_3, fsiq)
fsiq_1 <- sem(formula1, fsiq)
fsiq_3 <- sem(formula3, fsiq, orthogonal=T)
summary(fsiq_1_3, standardize = T)  
summary(fsiq_1, standardize = T)  
summary(fsiq_3, standardize = T)  

semPlot::semPaths(fsiq_3, residuals=F,sizeMan=7,"std",
                  # posCol=c("skyblue4", "red"),
                  #edge.color="skyblue4",
                  edge.label.cex=1.2,layout="circle2")
semPlot::semPaths(fsiq_1, residuals=F,sizeMan=7,"std",
                  #posCol=c("skyblue4", "red"),
                  #edge.color="skyblue4",
                  edge.label.cex=1.2,layout="circle2")
semPlot::semPaths(fsiq_1_3, residuals=F,sizeMan=7,"std",
                  #posCol=c("skyblue4", "red"),
                  #edge.color="skyblue4",
                  edge.label.cex=1.2)




