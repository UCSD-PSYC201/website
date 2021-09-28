library(tidyverse)

between = list('A'=paste0('A', 1:3),
               'B'=paste0('B',1:4))
between.cell.n = 4
within = list('C'=paste0('C',1:2),
              'D'=paste0('D',1:5),
              'E'=paste0('E', 1:3))
within.cell.n = 3
within.cell.sd = 1

rescale = function(x,s){
  return((x-mean(x))/sd(x)*s)
}

# generate fixed effects
f.effects = list()
all.effects = c(between, within)
for(i in 1:length(all.effects)){
  effs = combn(names(all.effects), i)
  s = rexp(1,5)/i
  for(j in 1:dim(effs)[2]){
    ef.name = paste0(effs[,j], collapse = ':')
    f.effects[[ef.name]] = do.call(expand.grid, all.effects[effs[,j]])
    f.effects[[ef.name]]$offset = rescale(rnorm(nrow(f.effects[[ef.name]])), s)
  }
}

# generate random effects
N = prod(unlist(lapply(between, length)))*between.cell.n
r.effects = list()
all.effects = c(list('subject'=paste0('subj-',1:N)), within)
for(i in 1:length(all.effects)){
  effs = combn(names(all.effects), i)
  s = rexp(1,5)/i
  for(j in 1:dim(effs)[2]){
    if('subject' %in% effs[,j]){
      ef.name = paste0(effs[,j], collapse = ':')
      r.effects[[ef.name]] = do.call(expand.grid, all.effects[effs[,j]])
      r.effects[[ef.name]]$offset = rescale(rnorm(nrow(r.effects[[ef.name]])), s)
    }
  }
}

# generate subject randomness.
S.labels = do.call(expand.grid, all.effects)
S.data = do.call(expand.grid, all.effects)
S.data$x = 0
for(n in names(r.effects)){
  S.data = mutate(S.data, x = x + left_join(S.labels, r.effects[[n]])$offset)
}
S.data = S.data %>% arrange(subject)
S.data = S.data[rep(1:nrow(S.data), each=within.cell.n),] %>% mutate(x = x + rnorm(n(),0,within.cell.sd))

# add fixed effects.
fixed = do.call(expand.grid, between)
fixed = fixed[rep(1:nrow(fixed), each=between.cell.n),]
fixed$subject = paste0('subj-', 1:nrow(fixed))
S.data = left_join(S.data, fixed)
for(n in names(f.effects)){
  S.data = mutate(S.data, x = x + left_join(select(S.data, -x), f.effects[[n]])$offset)
}

summary(aov(data=S.data, x~A*B*C*D*E+Error(subject/(C*D*E))))
