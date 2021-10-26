
persistence = 0.8
lambda = -log(persistence) / 1




time = seq(0,1,length=1000)
coverage.at.one.year = exp(-lambda); coverage.at.one.year

coverage.at.time = exp(-lambda*time)
qplot(time, coverage.at.time, geom='line') + ylim(0,1)

mean(coverage.at.time)


uptake = c(.1,.25,.50)
coverage = uptake * (1/lambda - exp(-lambda)/lambda)
round(100*cbind(uptake, coverage))


c(.6,.8)/(.6+.8)


uptake.to.coverage <- function(uptake, persistence)
{
    lambda = -log(persistence)
    uptake * (1/lambda - exp(-lambda)/lambda)
}


coverages = uptake.to.coverage(c(oral=12.5, lai=12.5), c(.6,.8))
#rr = 1-efficacy
rrs = c(oral=.14, lai=.14*.34)
efficacies = 1-rrs

weights = coverages/sum(coverages)
combined.coverage = sum(coverages)
combined.rr = sum(rrs*weights)
