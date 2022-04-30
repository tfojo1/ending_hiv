

num = 200000 # number who ever start prep
pop = num * 10

persistence = 0.56
lambda = -log(persistence)

start.year = -10
end.year = 11

start.prep.time = runif(num, start.year, end.year)
end.prep.time = start.prep.time + rexp(num, rate=lambda)

frac.on.prep.at.time = function(tt){
    sum(start.prep.time <= tt & end.prep.time >= tt)
}

uptake.at.year = sum((start.prep.time >= 0 & start.prep.time <= 1) | 
    (end.prep.time >= 0 & end.prep.time <= 1) |
    (start.prep.time <= 0 & end.prep.time >= 1))

times = seq(0,1,length=1000)
coverage.fractions = sapply(times, frac.on.prep.at.time) / uptake.at.year
print(mean(coverage.fractions))
print(qplot(times, coverage.fractions) + ylim(0,1))
print(qplot(coverage.fractions)) + xlim(0,1)


if (1==2)
{
    
    num = 200000
    start.prep.time = 0
    end.prep.time = start.prep.time + rexp(num, rate=lambda)
    times = seq(0,1,length=1000)
    coverage.fractions = sapply(times, frac.on.prep.at.time)/num
    mean(coverage.fractions)
    print(qplot(times, coverage.fractions) + ylim(0,1))
}