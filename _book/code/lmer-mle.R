z.score = function(x){(x-mean(x))/sd(x)}

ns = 10
nt = 2

dat <- data.frame(subject = rep(paste0('S', 1:ns), nt),
           test = rep(LETTERS[1:nt], each=ns),
           y = round((rnorm(ns*nt, 0, 0)+
                        rep(rnorm(ns, 0, 0), nt) + 
                        rep(rnorm(nt, 0, 1), each=ns))*5+50, 1))

dat

# repeated measures t-test version
dat.w = dat %>% 
  pivot_wider(subject, 
              names_from = test, 
              values_from = y) %>% 
  mutate(d = A-B)

dat.w

t.test(dat.w$d)


dat %>% 
  pivot_wider(subject, 
              names_from = test, 
              values_from = y) %>% 
  mutate(d = a-b) %>% 
  lm(data = ., d ~ 1) %>% 
  summary()


# AOV version
dat %>% 
  aov(data = ., 
      y ~ test + Error(subject/test)) %>% 
  summary()

# what's the actual model?

M.lm = lm(data = dat, y ~ 0 + subject + test)
summary(M.lm)


# lmer
library(lme4)
library(lmerTest)
M = lmerTest::lmer(data = dat, y ~ test + (1|subject))
M0 = lmer(data = dat, y ~ 1 + (1|subject), REML=F)
anova(M0, M, test='LRT')

summary(M)
lmerTest::lmer(y ~ A*B + (A*B|unit))


