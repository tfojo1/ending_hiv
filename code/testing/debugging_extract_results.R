

source('code/source_code.R')

loc = "33100"
int = "msm.baseline.oral_23_27"
load(paste0("Q:Ending_HIV/mcmc_runs/prep_simsets/",loc,"/1.0_", loc, "_", int, ".Rdata"))
full.simset = simset
simset = subset.simset(simset, 1:10)
YEARS = 2020:2025


x=load("results/prep/total.msm.results_2022-03-09.Rdata")
x=load("results/prep/msm.results.by.race_2022-03-09.Rdata")

##-- CHECK THAT get.arr.for.data.type IS EQUIVALENT TO project.absolute.<x> --##

# total
total.population = get.total.population.for.simset(simset, years=YEARS, census.totals = CENSUS.TOTALS)

dimension.subsets=list(age=simset@simulations[[1]]$ages,
                       race=simset@simulations[[1]]$races,
                       sex='msm',
                       risk=simset@simulations[[1]]$risks)

arr.from.get = get.arr.for.data.type(simset,
                                     data.type='incidence',
                                     years=YEARS,
                                     keep.dimensions='year',
                                     dimension.subsets=dimension.subsets,
                                     total.population=total.population,
                                     year.anchor='mid',
                                     use.cdc.categorizations=F)

arr.from.project = sapply(simset@simulations, project.absolute.incidence, years=YEARS, sexes='msm', use.cdc.categorizations=F)


all(arr.from.get == arr.from.project)


# does this match the big results array?
arr.from.ex = total.msm.results[as.character(YEARS),1:simset@n.sim,'incidence',loc,int]
all(arr.from.ex == arr.from.get)

#-- by race --#
arr.from.get = get.arr.for.data.type(simset,
                                     data.type='incidence',
                                     years=YEARS,
                                     keep.dimensions=c('year','race'),
                                     dimension.subsets=dimension.subsets,
                                     total.population=total.population,
                                     year.anchor='mid',
                                     use.cdc.categorizations=F)

arr.from.project = sapply(simset@simulations, project.absolute.incidence, years=YEARS, keep.dimensions=c('year','race'), sexes='msm', use.cdc.categorizations=F)


all(arr.from.get == as.numeric(arr.from.project))

arr.from.ex = msm.results.by.race[as.character(YEARS),,1:simset@n.sim,'incidence',loc,int]
all(arr.from.ex == arr.from.get)



#-- Calculate the ratios --#

inc.race = sapply(simset@simulations, project.absolute.incidence, years=2030, keep.dimensions=c('year','race'), sexes='msm', use.cdc.categorizations=F)
pop.race = sapply(simset@simulations, project.population.subset, years=2030, keep.dimensions=c('year','race'), sexes='msm', use.cdc.categorizations=F)

all(inc.race == msm.results.by.race['2030',,1:simset@n.sim,'incidence',loc,int])
all(pop.race == msm.results.by.race['2030',,1:simset@n.sim,'population',loc,int])

b.vs.o = inc.race[1,] / pop.race[1,] / (inc.race[3,] / pop.race[3,]); mean(b.vs.o)

gen.b.vs.o = msm.results.by.race['2030','black',,'incidence',loc,] / msm.results.by.race['2030','black',,'population',loc,] /
    (msm.results.by.race['2030','other',,'incidence',loc,] / msm.results.by.race['2030','other',,'population',loc,])
now.gen.b.vs.o = msm.results.by.race['2020','black',,'incidence',loc,] / msm.results.by.race['2020','black',,'population',loc,] /
    (msm.results.by.race['2020','other',,'incidence',loc,] / msm.results.by.race['2020','other',,'population',loc,])
abs.gen.b.vs.o = msm.results.by.race['2030','black',,'incidence',loc,] / msm.results.by.race['2030','other',,'incidence',loc,]

colMeans(gen.b.vs.o)
colMeans(now.gen.b.vs.o)
colMeans(abs.gen.b.vs.o)


#inc reduction by race
inc.red = (msm.results.by.race['2020',,,'incidence',loc,] - msm.results.by.race['2030',,,'incidence',loc,]) / msm.results.by.race['2020',,,'incidence',loc,]

apply(inc.red, c('race','intervention'), mean)



#-- Checks out --#

gen.b.vs.o = msm.results.by.race['2030','black',,'incidence',,] / msm.results.by.race['2030','black',,'population',,] /
    (msm.results.by.race['2030','other',,'incidence',,] / msm.results.by.race['2030','other',,'population',,])
now.b.vs.o = msm.results.by.race['2020','black',,'incidence',,1] / msm.results.by.race['2020','black',,'population',,1] /
    (msm.results.by.race['2020','other',,'incidence',,1] / msm.results.by.race['2020','other',,'population',,1])

z = cbind(colMeans(now.b.vs.o), colMeans(gen.b.vs.o))
dimnames(z)[[2]] = NULL
round(z,1)
