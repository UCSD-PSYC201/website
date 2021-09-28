
# sinusoidal data.
# we are playing marco polo with alice
# alice is on a ferris wheel.
# every few seconds we shout "marco", and record the volume with which we hear "polo"

height = function(t, period, rstart, radius, pedestal){
  (sin(t*1/period*2*pi + rstart)+1)*radius+pedestal
}
translation = function(t, period, rstart, radius){
  cos(t*1/period*2*pi + rstart)*radius
}
distance = function(t, period, rstart, radius, pedestal){
  sqrt(height(t, period, rstart, radius, pedestal)^2 + 
         translation(t, period, rstart, radius)^2)
}

t = seq(0, 180, 1)
rstart = -pi/4
period = 180
radius = 10
pedestal = 1
h = height(t, period, rstart, radius, pedestal)
d = translation(t, period, rstart, radius)

plot(d, h)
plot(t, h)
plot(t, d)
plot(t, distance(t, period, rstart, radius, pedestal))


m = 80
s = 1
y = rnorm(length(t), 0, s) + m - 6*log2(distance(t, period, rstart, radius, pedestal))

plot(t, y)


params = list(rstart = 0, #[-pi to pi]
              period = 100,  # [1, Inf]
              radius = 5,   # [0 Inf]
              pedestal = 0, #[0, Inf] 
              m = 75,
              s = 2)


nll = function(rstart, period, radius, pedestal, m, s){
  h = (sin(t*1/period*2*pi + rstart)+1)*radius+pedestal
  x = cos(t*1/period*2*pi + rstart)*radius
  distance = sqrt(h^2+d^2)
  attenuation = -log2(distance)*6
  cat(c(rstart, period, radius, pedestal, m, s), '\n')
  -sum(dnorm(y, m+attenuation, s, log=T))
}

fit <- mle(nll, params,
           lower = list(rstart = -pi,
                        period = 0.01,
                        radius = 1e-6,
                        s = 1e-6),
           upper = list(rstart = pi))

plot(t, y)
lines(t, 79 - 6*log2(distance(t, 178, -0.82, 9.9, -0.93)), col='red')

