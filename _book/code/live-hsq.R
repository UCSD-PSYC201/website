
# Questions?   
# Difference of something with age? gender?

# Q1. I usually don’t laugh or joke around much with other people.
# Q2. If I am feeling depressed, I can usually cheer myself up with humor.
# Q3. If someone makes a mistake, I will often tease them about it.
# Q4. I let people laugh at me or make fun at my expense more than I should.
# Q5. I don’t have to work very hard at making other people laugh—I seem to be a naturally humorous person.
# Q6. Even when I’m by myself, I’m often amused by the absurdities of life.
# Q7. People are never offended or hurt by my sense of humor.
# Q8. I will often get carried away in putting myself down if it makes my family or friends laugh.
# Q9. I rarely make other people laugh by telling funny stories about myself.
# Q10. If I am feeling upset or unhappy I usually try to think of something funny about the situation to make myself feel better.
# Q11. When telling jokes or saying funny things, I am usually not very concerned about how other people are taking it.
# Q12. I often try to make people like or accept me more by saying something funny about my own weaknesses, blunders, or faults.
# Q13. I laugh and joke a lot with my closest friends.
# Q14. My humorous outlook on life keeps me from getting overly upset or depressed about things.
# Q15. I do not like it when people use humor as a way of criticizing or putting someone down.
# Q16. I don’t often say funny things to put myself down.
# Q17. I usually don’t like to tell jokes or amuse people.
# Q18. If I’m by myself and I’m feeling unhappy, I make an effort to think of something funny to cheer myself up.
# Q19. Sometimes I think of something that is so funny that I can’t stop myself from saying it, even if it is not appropriate for the situation.
# Q20. I often go overboard in putting myself down when I am making jokes or trying to be funny.
# Q21. I enjoy making people laugh.
# Q22. If I am feeling sad or upset, I usually lose my sense of humor.
# Q23. I never participate in laughing at others even if all my friends are doing it.
# Q24. When I am with friends or family, I often seem to be the one that other people make fun of or joke about.
# Q25. I don’t often joke around with my friends.
# Q26. It is my experience that thinking about some amusing aspect of a situation is often a very effective way of coping with problems.
# Q27. If I don’t like someone, I often use humor or teasing to put them down.
# Q28. If I am having problems or feeling unhappy, I often cover it up by joking around, so that even my closest friends don’t know how I really feel.
# Q29. I usually can’t think of witty things to say when I’m with other people.
# Q30. I don’t need to be with other people to feel amused – I can usually find things to laugh about even when I’m by myself.
# Q31. Even if something is really funny to me, I will not laugh or joke about it if someone will be offended.
# Q32. Letting others laugh at me is my way of keeping my friends and family in good spirits.


hsq <- read_csv('../data/HSQ/data.csv')

hsq <- hsq %>% select(Q1:Q32, age)

hsq <- hsq %>% mutate_at(vars(Q1:Q32), function(x){ifelse(x==-1, NA, x)}) %>% drop_na()
hsq <- hsq %>% filter(age < 120)

M <- lm(data = hsq, age ~ .)

summary(M)

# loo
n = nrow(hsq)
train_error = rep(NA, n)
test_error = rep(NA, n)
for(i  in 1:n){
  train_data = hsq[(1:n)[-i], ]
  test_data = hsq[i,]
  M <- lm(data = train_data, age ~ .)
  predicted_age = predict(M, test_data)
  test_error[i] = abs(test_data$age - predicted_age)
  train_error[i] = mean(abs(train_data$age - predict(M, train_data)))
}


# z score Q likert items for more sensble performance with raw polynomials
hsq <- hsq %>% mutate_at(vars(Q1:Q32), function(x){(x-mean(x))/sd(x)})
# random subsampling
k = 100
n = nrow(hsq)
iterations = 100
train_error = rep(NA, iterations)
test_error = rep(NA, iterations)
for(i in 1:iterations){
  test_idx = sample(n, k, replace = F)
  test_data = hsq[test_idx,]
  train_data = hsq[(1:n)[-test_idx], ]
  M <- lm(data = train_data, age ~ polym(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, degree=4, raw=T))
  # M <- e1071::svm(data = train_data, age ~ .)
  test_error[i] = mean(abs(test_data$age - predict(M, test_data)))
  train_error[i] = mean(abs(train_data$age - predict(M, train_data)))
}

hist(train_error)
hist(test_error)



