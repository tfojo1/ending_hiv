

load('results/prep/msm.results.by.race_2022-03-23.Rdata')

inc.per.pop = msm.results.by.race[,,,'incidence',,] / msm.results.by.race[,,,'population',,]

b.vs.o = inc.per.pop[,'black',,,] / inc.per.pop[,'other',,,]
h.vs.o = inc.per.pop[,'hispanic',,,] / inc.per.pop[,'other',,,]

black.disparity.ratios=cbind(colMeans(b.vs.o['2019',,,1]),colMeans(b.vs.o['2030',,,]));dimnames(black.disparity.ratios)[[2]]=NULL;round(black.disparity.ratios,1)
hispanic.disparity.ratios=cbind(colMeans(h.vs.o['2019',,,1]),colMeans(h.vs.o['2030',,,]));dimnames(hispanic.disparity.ratios)[[2]]=NULL;round(hispanic.disparity.ratios,1)

