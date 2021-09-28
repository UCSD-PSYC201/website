library(tidyverse)
library(lubridate)

sleep <- read_csv('../data/molly/Molly Vul_sleep.csv')
sleep <- sleep %>%
  mutate(duration.min = `Duration(minutes)`) %>%
  select(Time, duration.min) 
sleep <- sleep %>%
  mutate(date.time = lubridate::mdy_hm(Time),
         date = lubridate::date(date.time),
         time = lubridate::hour(date.time)*60 + lubridate::minute(date.time))

sleep %>% 
  ggplot(aes(y=date, yend=date, x=time))+
  geom_segment(aes(xend = time+duration.min), color='blue')

# sleep %>% arrange(desc(duration.min))
# sleep %>% filter(date == '2018-09-01')

# fix obvious error, impute last day
# sleep <- sleep %>%
#   mutate(duration.min = ifelse(date == '2018-09-02' & time == 1140, 650, duration.min)) %>%
#   filter(date > '2017-12-13')

sleep %>% group_by(date) %>% 
  summarize(total.min = sum(duration.min)) %>% 
  ggplot(aes(x=date, y=total.min))+
  geom_point()

sleep %>% 
  ggplot(aes(y=date, yend=date, x=time/60))+
  geom_segment(aes(xend = (time+duration.min)/60), color='blue', size=1.25)+
  geom_segment(aes(y = date+1, 
                   yend=date+1, 
                   x=(time)/60-24, 
                   xend = (time+duration.min)/60-24), color='blue', size=1.25)+
  geom_segment(aes(y = date-1, 
                   yend=date-1, 
                   x=(time)/60+24, 
                   xend = (time+duration.min)/60+24), color='blue', size=1.25)+
  geom_vline(xintercept = 24)+
  geom_vline(xintercept = 0)+
  coord_cartesian(xlim = c(0, 24))+
  scale_x_continuous(breaks = seq(0, 24, 4))+
  scale_y_date(date_breaks = 'month', date_labels = '%b %Y')+
  labs(x='hour')+
  theme_minimal()




bymin <- expand_grid(minute = seq(0, 24*60, by=5),
                     date = unique(sleep$date)) %>% 
  mutate(asleep = 0)

for(i in 1:nrow(sleep)){
  t.start = sleep$time[i]
  t.end = (sleep$time[i] + sleep$duration.min[i]) %% (24*60)
  date = sleep$date[i]
  if(t.end < t.start){
    bymin[bymin$date==date & t.start < bymin$minute, 'asleep'] = 1
    bymin[bymin$date==(date+1) & t.end > bymin$minute, 'asleep'] = 1
  } else {
    bymin[bymin$date==date & t.start < bymin$minute & bymin$minute < t.end, 'asleep'] = 1
  }
}

bymin %>% 
  filter(date > '2020-08-01') %>% 
  filter(date < '2020-10-14') %>% 
  mutate(month = month(date, label=TRUE)) %>% 
  group_by(minute, month) %>% 
  summarize(asleep = mean(asleep)) %>% 
  ggplot(aes(x=minute/60, y=asleep, color=month))+
  geom_line()+
  scale_color_manual(values = c('Sep' = 'black', 'Oct'='red',
                                'Aug' = 'blue'))+
  scale_x_continuous(limits = c(0, 24),
                     breaks = seq(0, 24, by=2))+
  xlab('hour')+
  ylab('probability asleep')+
  theme_minimal()

mga.oct.days = c('2020-10-05',
             '2020-10-06',
             '2020-10-07',
             '2020-10-08',
             '2020-10-09',
             '2020-10-13')
nonmga.oct.days = c('2020-10-01',
                    '2020-10-02',
                    '2020-10-03',
                    '2020-10-04',
                    '2020-10-10',
                    '2020-10-11',
                    '2020-10-12')

bymin %>% 
  filter(date > '2020-09-01') %>% 
  filter(date < '2020-10-14') %>% 
  mutate(month = month(date, label=TRUE)) %>% 
  mutate(period = case_when(
    month == 'Sep' ~ 'Sep',
    as.character(date) %in% mga.oct.days ~ 'Oct MGA',
    as.character(date) %in% nonmga.oct.days ~ 'Oct Home',
    TRUE ~ 'error'
  )) %>% 
  group_by(minute, period) %>% 
  summarize(asleep = mean(asleep)) %>% 
  ggplot(aes(x=minute/60, y=asleep, color=period))+
  geom_line(size=1.5)+
  scale_color_brewer(type = 'qual')+
  scale_x_continuous(limits = c(0, 24),
                     breaks = seq(0, 24, by=2))+
  xlab('hour')+
  ylab('probability asleep')+
  theme_minimal()

bymin %>% 
  filter(date > '2020-09-01') %>% 
  filter(date < '2020-10-14') %>% 
  mutate(month = month(date, label=TRUE)) %>% 
  mutate(period = case_when(
    month == 'Sep' ~ '09 - Sep',
    as.character(date) %in% mga.oct.days ~ '10 - Oct MGA',
    as.character(date) %in% nonmga.oct.days ~ '10 - Oct Home',
    TRUE ~ 'error'
  )) %>% 
  group_by(date, period) %>% 
  summarize(hours.asleep = sum(asleep)*5/60) %>% 
  group_by(period) %>% 
  summarize(m.hours.asleep = mean(hours.asleep),
            s.hours.asleep = sd(hours.asleep)/sqrt(n())) %>% 
  ggplot(aes(x=period, y=m.hours.asleep,
             ymin = m.hours.asleep - s.hours.asleep,
             ymax = m.hours.asleep + s.hours.asleep))+
           geom_pointrange()+
  ylab('Total hours asleep per day (+/- 1 se)')+
  theme_minimal()

sleep.min <- sleep %>% 
  mutate(days.old = interval("2017-12-13", date) %/% days(1),
         time = time+1,
         start = days.old*24+(time+0.01)/60, end=days.old*24+time/60 + duration.min/60,
         start.y = ceiling(start/24), end.y = ceiling(end/24), start.x = start %% 24, end.x = end %% 24) %>%
  select(date, time, duration.min, start, end, start.y, end.y, start.x, end.x)

adds = data.frame()
for(i in 1:nrow(sleep.min)){
  if(sleep.min$start.y[i] != sleep.min$end.y[i]){
    x = slice(sleep.min, i)
    sleep.min$end.y[i] = sleep.min$start.y[i]
    sleep.min$end.x[i] = 23.999
    x$start.y = x$end.y
    x$start.x = 0.0001
    adds = rbind(adds, x)
  }
}

rbind(sleep.min, adds) %>%
  ggplot(aes(y=start.y, yend=end.y, x=start.x, xend=end.x))+
  geom_segment(color='blue', size=1.25)+
  geom_vline(xintercept = 24)+
  geom_vline(xintercept = 0)+
  coord_polar()+
  scale_x_continuous(breaks = seq(0, 24, 4))+
  scale_y_continuous(limits = c(0, 366))+
  labs(x='hour')+
  theme_minimal()

## TODO: add nursing / diapering imputed sleep before 12-24

