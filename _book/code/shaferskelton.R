library(tidyverse)
load(url('http://vulstats.ucsd.edu/data/shaferskelton.rdata'))

dat <- dat %>% 
  mutate(mouse = `Mouse/Touchscreen`) %>%
  select(-`Mouse/Touchscreen`)
aov(data=dat, Error_Dist ~ Task*mouse*saccade_condition + Error(Subject_Initials / (Task*mouse*saccade_condition))) %>%
  summary()

dat.summ <- dat %>% group_by(Subject_Initials, Task, mouse, saccade_condition) %>%
  summarize(n = n(), m.error = mean(Error_Dist)) 

keep.subs = dat.summ %>%
  count(Subject_Initials, Task, mouse, saccade_condition) %>%
  ungroup() %>%
  count(Subject_Initials) %>%
  filter(nn == 16) %>%
  .$Subject_Initials

dat.summ = dat.summ %>% filter(Subject_Initials %in% keep.subs)

aov(data=dat.summ, m.error ~ Task*mouse*saccade_condition + Error(Subject_Initials / (Task*mouse*saccade_condition))) %>%
  summary()


library(lme4)

m.0 <- lmer(data = dat,
          Error_Dist ~ Task*mouse*saccade_condition + (Task*mouse*saccade_condition | Subject_Initials))

summary(m.0)


m.1 <- lmer(data = dat,
            Error_Dist ~ Task*mouse*saccade_condition + 
              (1 | Subject_Initials) +
              (1 | Subject_Initials:Task) +
              (1 | Subject_Initials:mouse) +
              (1 | Subject_Initials:saccade_condition) +
              (1 | Subject_Initials:Task:mouse) +
              (1 | Subject_Initials:Task:saccade_condition) +
              (1 | Subject_Initials:mouse:saccade_condition) +
              (1 | Subject_Initials:Task:mouse:saccade_condition),
            REML = F)


m.0 <- lmer(data = dat,
            Error_Dist ~ Task*mouse+saccade_condition + 
              (1 | Subject_Initials) +
              (1 | Subject_Initials:Task) +
              (1 | Subject_Initials:mouse) +
              (1 | Subject_Initials:saccade_condition) +
              (1 | Subject_Initials:Task:mouse) +
              (1 | Subject_Initials:Task:saccade_condition) +
              (1 | Subject_Initials:mouse:saccade_condition) +
              (1 | Subject_Initials:Task:mouse:saccade_condition),
            REML = F)

summary(m.1)

(deviance(m.0) - deviance(m.1))/deviance(m.0)

library(brms)


stan.code = '
data {
  int<lower=0> N; // number of trials
  int<lower=0> nS; // number of subjects
  int<lower=0> nT; // number of tasks
  int<lower=0> nM; // number of mouse/touchpad
  int<lower=0> nC; // number of saccade conditions
  real y[N]; // observed error
  int task[N]; // task ID
  int mouse[N]; // mouse/touchscreen ID
  int condition[N]; // saccade condition ID
}
parameters {
  real m;
  real m_S[nS];
  real m_T[nT];
  real m_M[nM];
  real m_C[nC];
  real m_T_C[nT, nC];
  real m_T_M[nT, nM];
  real m_M_C[nM, nC];
  real m_T_M[nT, nM];
  real m_T_M_C[nT, nM, nC];

  real m_S_T[nS, nT];
  real m_S_M[nS, nM];
  real m_S_C[nS, nC]; 
  real m_S_T_C[nS, nT, nC];
  real m_S_T_M[nS, nT, nM];
  real m_S_M_C[nS, nM, nC];
  real m_S_T_M[nS, nT, nM];
  real m_S_T_M_C[nS, nT, nM, nC];
  
  real<lower=0> s_y;
  real<lower=0> s_m_S;
  real<lower=0> tau;
  real gmu;
}
model {
sigma ~ exponential(2);
tau ~ exponential(2);
mu ~ normal(gmu, tau);
x ~ normal(mu[g], sigma[g]);
}
'
# stan.file = 'model.stan'
# fp <- file(stan.file)
# writeLines(stan.code, fp)
# close(fp)

data.for.stan <- list(
  N = nrow(df),
  x = df$x,
  g = df$g,
  K = length(unique(df$g))
)
xxxxxx
fit <- stan(
  # file = stan.file,   
  model_code = stan.code,  # Stan program
  data = data.for.stan,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 100,          # number of warmup iterations per chain
  iter = 10000,            # total number of iterations per chain
  cores = 1,              # number of cores (using 2 just for the vignette)
  refresh = 100          # show progress every 'refresh' iterations
)
fit
samples <- extract(fit) %>% as.data.frame()

ggplot(samples, aes(x=mu.1, y=mu.2, color=lp__))+
  geom_point(size=0.1)


ggplot(samples, aes(x=sigma))+
  geom_histogram()

m.1 <- lmer(data = dat,
            Error_Dist ~ 
              (1|Task) +
              (1|mouse) +
              (1|saccade_condition) + 
              (1|Task:mouse)+
              (1|Task:saccade_condition)+
              (1|saccade_condition:mouse)+
              (1|Task:mouse:saccade_condition)+
              (1 | Subject_Initials) +
              (1 | Subject_Initials:Task) +
              (1 | Subject_Initials:mouse) +
              (1 | Subject_Initials:saccade_condition) +
              (1 | Subject_Initials:Task:mouse) +
              (1 | Subject_Initials:Task:saccade_condition) +
              (1 | Subject_Initials:mouse:saccade_condition),
            REML = F)
library(brms)

m.0 <- brm(data = dat, 
           Error_Dist ~ 
             (1|Task) +
             (1|mouse) +
             (1|saccade_condition) + 
             (1|Task:mouse)+
             (1|Task:saccade_condition)+
             (1|saccade_condition:mouse)+
             (1|Task:mouse:saccade_condition)+
             (1 | Subject_Initials) +
             (1 | Subject_Initials:Task) +
             (1 | Subject_Initials:mouse) +
             (1 | Subject_Initials:saccade_condition) +
             (1 | Subject_Initials:Task:mouse) +
             (1 | Subject_Initials:Task:saccade_condition) +
             (1 | Subject_Initials:mouse:saccade_condition))

