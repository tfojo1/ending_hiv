int.p = c(.25, .5, .6, .7)
slope.o = 1.1
times = 0:20

#derived

n = length(int.p)
nt = length(times)

int.lo = log(int.p) - log(1-int.p)
slope.lo = log(slope.o)



lo = rep(int.lo, each=nt) + rep(times, n) * slope.lo
catg = rep(as.character(1:n), each=nt)
p = 1 / (1+exp(-lo))


print(qplot(rep(times, n), p, color=catg, geom='line'))
