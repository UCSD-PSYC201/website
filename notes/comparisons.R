library(ggplot2)
library(dplyr)

# point 1.  distribution of p value under the null is uniform(0,1)

get.null.p = function(){
  y = rnorm(100)
  t.test(y)$p.value
}
data.frame(p.value = replicate(n=10000, get.null.p())) %>%
  ggplot(aes(x=p.value))+
  geom_histogram(breaks=seq(0,1,by=0.1), color='black')

# so we can sample null p values just from runif(0,1)

# as a consequence, as we add more data, our p-value does not 
# converge to 1 under the null, but wanders around.

df.sim = data.frame()
for(chain in 1:5){
  y = rnorm(1)
  p.value = NA
  for(i in 2:10000){
    y = c(y, rnorm(1))
    p.value = c(p.value, t.test(y)$p.value)
  }
  df.sim = rbind(df.sim, data.frame(chain =chain, iteration = 1:10000, p.value=p.value))
}
df.sim %>%
  ggplot(aes(x=iteration, y=p.value))+
  facet_grid(chain~.)+
  geom_line()

# This is why conditional stopping is so dangerous.

# point 2:  family-wise error rate increases with family size.

get.fwe = function(k,alpha){
  any(runif(k, 0, 1)<alpha)  # is any one of these p.values significant?
}

df.sim = data.frame()
for(k in 1:100){
  df.sim = rbind(df.sim, data.frame(n.comparisons = k,
                                    familywise.error = mean(replicate(1000, get.fwe(k,0.05)))))
}

df.sim %>% 
  ggplot(aes(x=n.comparisons, y=familywise.error))+
  geom_line() +
  geom_hline(yintercept = 0.05, color='red')+
  geom_label(label='0.05', color='red', x=10, y= 0.05)

# this is bad.  with lots of comparisons we are guaranteed to get something significant
# even if everything is truly null.

# point 3: we can correct the per-test alpha to get a desired FWE rate

sig.procedure = list()
sig.procedure[['uncorrected']] = function(alpha, ps){
  (1:length(ps))[ps<alpha]
}

sig.procedure[['bonferroni']] = function(alpha, ps){
  (1:length(ps))[ps < (alpha/length(ps))]
}

sig.procedure[['sidak']] = function(alpha, ps){
  (1:length(ps))[ps<(1-(1-alpha)^(1/length(ps)))]
}

sig.procedure[['holm']] = function(alpha, ps){
  m = length(ps)
  sorted.ps = sort(ps)
  tmp = sorted.ps > alpha/(m+1-(1:m))
  if(any(tmp)){
    (1:m)[ps < min(sorted.ps[tmp])]
  } else { 
    1:m
  }
}

sig.procedure[['hochberg']] = function(alpha,ps){
  m = length(ps)
  sorted.ps = sort(ps)
  sig = sorted.ps <= alpha/(m+1-(1:m))
  if(any(sig)){ 
    (1:m)[ps<=max(sorted.ps[sig])] 
  } else { numeric(0) }
  # o[1:k]
}

sig.procedure[['fdr.BH']] = function(q, ps){ # corrects for FDR.  q is desired FDR
  m = length(ps)
  sorted.ps = sort(ps)
  sig = sorted.ps <= (1:m)/m*q
  if(any(sig)){ 
    (1:m)[ps<=max(sorted.ps[sig])] 
  } else { numeric(0) }
}

sig.procedure[['fdr.BHY']] = function(q, ps){ # corrects for FDR.  q is desired FDR.  positively dependent.
  m = length(ps)
  sorted.ps = sort(ps)
  cm = ifelse(m<50, sum(1/(1:m)), log(m)-digamma(1)+1/(2*m))
  # approximation works great for large m.  cutoff of 10 probably sufficient
  # replace m<50 with m<20 or m<10 for performance gains.
  sig = sorted.ps <= (1:m)/(m*cm)*q
  if(any(sig)){ 
    (1:m)[ps<=max(sorted.ps[sig])] 
  } else { numeric(0) }
}

# specialized procedures:
# tukey: all pairwise comparisons
# scheffe: all linear contrasts
# dunnett: 1 mean to all others

simulate.batch = function(params){
  nulls = runif(params$k)<params$p.null
  ps = ifelse(nulls, runif(params$k), rbeta(params$k,10,1))
  sig.ps.idx = sig.procedure[[params$correction]](params$alpha, ps)
  cbind(params, 
        data.frame(n.sig.nulls = ifelse(length(sig.ps.idx)>0, sum(nulls[sig.ps.idx]),0),  # n. sig. nulls
          n.sig.trues = ifelse(length(sig.ps.idx)>0, sum(!nulls[sig.ps.idx]),0), # n sig trues
          n.ns.nulls = ifelse(length(sig.ps.idx)>0, sum(nulls[-sig.ps.idx]),0),
          n.ns.trues = ifelse(length(sig.ps.idx)>0, sum(!nulls[-sig.ps.idx]),0)))
}

simulator = function(params){
  bind_rows(replicate(n.sims, simulate.batch(params), simplify=F)) %>%
    summarize(fwe = mean(n.sig.nulls > 0),
              fdr = sum(n.sig.nulls)/(sum(n.sig.nulls+n.sig.trues)),
              pow = sum(n.sig.trues)/sum(n.sig.trues + n.ns.trues)) %>%
    bind_cols(params)
    
}

n.sims = 1000
df.sim = expand.grid(k = 1:100, 
                     correction=names(sig.procedure),
                     p.null = 1,
                     effect.size = 0,
                     alpha = 0.05) %>%
  rowwise() %>%
  do(simulator(.))


df.sim %>% ggplot(aes(x=k, y=fwe, color=correction))+
  geom_line()+
  geom_hline(data=NULL, yintercept = 0.05, color='red')+
  geom_label(data=NULL, label='0.05', x=10, y=0.05, color='red')

# point 4: what's the distribution under some power

# under normal... sample size and effect size all cancel out, so only power matters.
alpha = 0.05
pows = seq(alpha/2, 1-alpha/2, by=alpha/2)
df.sim = data.frame()
for(pow in pows){
  sep = qnorm(1-alpha/2)+qnorm(pow)
  z = rnorm(100000, sep, 1)
  p.value = 1-pnorm(z, 0, 1)
  df.sim = bind_rows(df.sim, 
                     data.frame(p.value = p.value, power=pow, alpha=alpha, sep=sep))
}

df.sim %>% group_by(power) %>% summarise(emp.power = mean(p.value < alpha/2), sep=mean(sep))

df.sim %>% ggplot(aes(x=log10(p.value), color=as.factor(power), group=as.factor(power)))+
  geom_density(fill=NA)

# for an F test... p-value distributions with same power, through f/n vary.

alpha = 0.05
pow = seq(0.1, 0.9, by=0.1)
k = 2:10
n = 2^(1:9)
df <- expand.grid(alpha = alpha, 
            power = pow, 
            k = k, 
            n = n)
df.sim = data.frame()
for(i in 1:nrow(df)){
  P = pwr::pwr.anova.test(k=df$k[i], n=df$n[i], sig.level = df$alpha[i], power=df$power[i])
  # qs = seq(0, 1, by=0.001)
  # p.vals <- 1-pf(qf(qs, P$k-1, P$k*(P$n-1), P$f^2*P$k*P$n), P$k-1, P$k*(P$n-1))
  # plot(rev(log10(p.vals)), c(0, diff(rev(1-qs)))/c(1, diff(rev(log10(p.vals)))))
  p.vals <- 1-pf(rf(1000, P$k-1, P$k*(P$n-1), P$f^2*P$k*P$n), P$k-1, P$k*(P$n-1))
  df.sim <- bind_rows(df.sim, 
            data.frame(p.value = p.vals,
                       alpha = P$sig.level,
                       k = P$k,
                       n = P$n,
                       power = P$power,
                       f = P$f))
}

df.sim %>% ggplot(aes(x=log10(p.value), color=as.factor(paste0(k,'-',n))))+
  geom_density(fill=NA)+
  facet_wrap(~power)+
  theme(legend.position = 'none')

df.sim %>% group_by(k,n,power) %>% summarize(e.power = mean(p.value<alpha)) %>%
  ggplot(aes(x=k, y=log10(e.power/power), color=log2(n)))+
  geom_point()+
  facet_wrap(~power)+
  theme(legend.position = 'none')


df.sim %>% group_by(k,n,power) %>% summarize(qp.05 = quantile(log10(p.value),0.05),
                                             qp.25 = quantile(log10(p.value),0.25),
                                             qp.50 = quantile(log10(p.value),0.5),
                                             qp.75 = quantile(log10(p.value),0.75),
                                             qp.95 = quantile(log10(p.value),0.95),
                                             qp.pow = quantile(log10(p.value),mean(power))) %>%
  ggplot(aes(x=log10(k*(n-1)), y=qp.50, ymin=qp.25, ymax=qp.75))+
  geom_hline(data=NULL, yintercept = log10(0.05), color='red')+
  geom_ribbon(aes(ymin=qp.05, ymax=qp.95), alpha=0.2)+
  geom_pointrange(aes(color=log2(n/k)))+
  geom_line(aes(y=qp.pow), color='black')+
  facet_wrap(~power)+
  theme(legend.position = 'none')

