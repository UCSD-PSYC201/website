n = 20
grp = sample(c(1, 2), n, replace=T)
m = sort(rnorm(2))
x = rnorm(n, m[grp], 1)
dat = data.frame(x=x, grp=grp)

statistic = function(dat){
  with(dat, mean(x[grp==2]) - mean(x[grp==1]))
}

shuffle = function(dat){
  data.frame(x = dat$x, grp = sample(dat$grp, nrow(dat), replace=F))
}

resample = function(dat){
  n1 = sum(dat$grp==1)
  n2 = sum(dat$grp==2)
  rbind(data.frame(x = sample(dat$x[grp==1], n1, replace=T), grp=1),
        data.frame(x = sample(dat$x[grp==2], n2, replace=T), grp=2))
}

my_stat = statistic(dat)

K = 10000
null_samps = replicate(K, statistic(shuffle(dat)))
rand_p = (sum(null_samps >= my_stat)+1)/(K+2)

boot_samps = replicate(K, statistic(resample(dat)))
boot_p = (sum(boot_samps < 0) + 1)/(K+2)
