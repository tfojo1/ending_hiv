

persistence = 0.6
lambda = -log(persistence)

exp(-lambda*c(0,3,6,9,12)/12)

discontinuation.times = rexp(10000, lambda)
mean(discontinuation.times>1)
mean(discontinuation.times>2)


times = seq(0,1,length=1000)
mean(sapply(times, function(time){mean(discontinuation.times>time)}))

exp(-lambda*.5)
mean(discontinuation.times>.5)

1-mean(1-exp(-lambda*times))



persistence = 0.8
lambda = -log(persistence)
r=exp(-lambda*.5)

.33 * r
