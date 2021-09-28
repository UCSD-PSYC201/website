

x = 0:25

px = dbinom(x, 25, 0.7)

e.x = sum(x*px) # mean
e.x2 = sum((x-e.x)^2*px) # variance
zx = (x-e.x)/sqrt(e.x2)
e.x3 = sum(zx^3*px) # skew
e.x4 = sum(zx^4*px) # kurtosis (not excess kurtosis)


pnorm(-5, mean = 0, sd = 1) + (1-pnorm(3))

# 99% confidence interval critical value

alpha = (1-0.99)/2
qnorm(alpha)

power = P(sig | H1)
 ( p(sig | h0) (1-p(h1)) * P(h1 | sig)))/( P(h1) -  p(h1) * P(h1 | sig))  = P(sig | h1)
 
 
 X = P(h1 | sig)
 A = P(sig | h1)
 B = P(sig | h0)
 C = P(h1)
 
 X = A*C/(A*C + B(1-C))
 
 
 
 
 b.w.m +/- t * se.b.w.m
 
 
 se.b.w.m = (sr / sx) / sqrt(n-1)
sy
r
(1-r^2) = sr^2 / sy^2


sqrt(sy^2*(1-r^2)*(n-1)/(n-2)) = sr

0.9 = P

P(h1)
P(sig | h1)

SS[y]
SSR[x1, x2, x3]
SSR[x1]

SSE[x1,x2,x3] = SS[y] - SSR[x1, x2, x3]
SSR[x2,x3|x1] = SSR[x1, x2, x3] - SSR[x1]
n
F = (SSR[x2,x3|x1] / 2) / (SSE[x1,x2,x3] / (n-4))





