library(tidyverse)

hotels <- read_csv('exams/201b-exam-a-2020/hotel_bookings_slice.csv')
hotels %>% glimpse()

# I would like to identify "holiday" bookings (as contrasted with work travel bookings). 
# I think holiday bookings can be identified based on the number of weekend and weekday 
# nights included in the booking.  Define a criterion based on these two variables to 
# label a given booking a "holiday".  
# 
# If holidays are correctly labeled, then they should be (a) more likely to include kids, 
# and (b) overrepresented in the resort (rather than city) hotel.  
# Report the proportion of holiday and non-holiday bookings (according to 
# your criterion) that include kids, and that were at the resort hotel.

hotels <- hotels %>% 
  mutate(has_kids = (children + babies) > 0,
         is_resort = hotel == 'Resort Hotel') %>% 
  mutate(nights = stays_in_weekend_nights + stays_in_week_nights) %>% 
  filter(nights > 0)

mK = mean(hotels$has_kids, na.rm=T)
mR = mean(hotels$is_resort, na.rm=T)

descriptive_stats = function(is_holiday){
  output = list()
  output$nT = sum(is_holiday)
  output$nF = sum(!is_holiday)
  output$K_T = (sum(hotels$has_kids[is_holiday], na.rm=T)+mK)/(output$nT + 1)
  output$K_F = (sum(hotels$has_kids[!is_holiday], na.rm=T)+mK)/(output$nF + 1)
  output$R_T = (sum(hotels$is_resort[is_holiday], na.rm=T)+mR)/(output$nT + 1)
  output$R_F = (sum(hotels$is_resort[!is_holiday], na.rm=T)+mR)/(output$nF + 1)
  return(output)
}

score = function(is_holiday){
  output = descriptive_stats(is_holiday)
  if(output$nT > 0  & output$nF > 0){
    # (output$K_T - output$K_F) +
    #   (output$R_T - output$R_F) -
    #   abs(output$nT - output$nF)/(output$nT + output$nF)/10000
    log(output$K_T / output$K_F) +
      log(output$R_T/output$R_F)  -
      abs(output$nT - output$nF)/(output$nT + output$nF)/10000
  } else {
    -abs(output$nT - output$nF)/(output$nT + output$nF)/10000
  }
  # log(K1 / K0) + log(R1/R0)
}

z.score = function(x){(x-mean(x))/sd(x)}
predict2 = function(params){
  (params[1] + 
     z.score(hotels$stays_in_weekend_nights)*params[2]) > 0
}

predict3 = function(params){
  (params[1] + 
     z.score(hotels$stays_in_weekend_nights)*params[2]  + 
     z.score(hotels$stays_in_week_nights)*params[3]) > 0
}

predict4 = function(params){
  (params[1] + 
     z.score(hotels$stays_in_weekend_nights)*params[2]  + 
     z.score(hotels$stays_in_week_nights)*params[3] +
     z.score(hotels$stays_in_weekend_nights / hotels$nights) * params[4]) > 0
}
  # (hotels$stays_in_weekend_nights / hotels$nights) * params[4]


fit2 <- optim(rep(0,2), function(params){-1*score(predict2(params))})
descriptive_stats(predict2(fit2$par))
score(predict2(fit2$par))

fit3 <- optim(rep(0,3), function(params){-1*score(predict3(params))})
descriptive_stats(predict3(fit3$par))
score(predict3(fit3$par))

fit4 <- optim(rep(0,4), function(params){-1*score(predict4(params))})
descriptive_stats(predict4(fit4$par))
score(predict4(fit4$par))

