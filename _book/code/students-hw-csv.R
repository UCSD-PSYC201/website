# install.packages('babynames')
# install.packages('stringr')

library(tidyverse)
class <- read_tsv('../../students/201a-list.txt', skip = 3)

class %>% select(Major, 
                 email=Email,
                 studentid=PID,
                 name=Student) %>% 
  separate(name, into=c('last', 'first'), sep=', ') %>% 
  write_csv('../../students/201a-list-hwdb.csv')
