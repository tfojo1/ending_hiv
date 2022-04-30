
setwd('../../../Ending HIV/Ending_HIV/')

source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

# Get our starting points
#mcmc = assemble.mcmc.from.cache('Q:Ending_HIV/mcmc_runs/systematic_caches/38900_1x20K_manual_2022-02-20',T)
#sim1 = mcmc@simulations[[1]]
#sim2 = mcmc@simulations[[length(mcmc@simulations)]]
#msa = attr(sim1, 'location')
#pp1 = mcmc@samples[1,1,]
#pp2 = mcmc@samples[1,mcmc@n.iter,]
load('mcmc_runs/temp_debugging.Rdata')
run.simulation = create.run.simulation.function(msa, pp2)
sim1 = run.simulation(pp1)
sim2 = run.simulation(pp2)

simplot(sim1, sim2)
simplot(sim1, sim2, facet.by='risk')
simplot(sim1, sim2, facet.by='age')
simplot(sim1, sim2, facet.by='race')
simplot(sim1, sim2, facet.by='race', split.by='risk')

simplot(sim1, sim2, facet.by=c('race','risk'), data.types='prevalence')

round(sapply(liks, function(lik){lik(sim2)-lik(sim1)}),2)
round(dlik.prev(sim2) - dlik.prev(sim1),2)

pp3 = pp2

pp3['age2.msm.susceptibility.rr.mult.1'] = 0.7
pp3['age1.msm.susceptibility.rr.mult.1']  = 1.5
pp3['age2.msm.susceptibility.rr.mult.2'] = 1.5
pp3['age1.msm.susceptibility.rr.mult.2'] = 2.5
pp3['msm.fraction.trate.change.after.t2'] = 0.49


sim3pre=sim3;sim3 = run.simulation(pp3)

round(likelihood(sim3)-likelihood(sim2), 2)
round(sapply(liks, function(lik){lik(sim3)-lik(sim2)}),2)
#round(dlik.prev(sim3) - dlik.prev(sim2),2)


simplot(sim3, sim2, facet.by=c('race','risk'), data.types='new')
simplot(sim3, sim2, facet.by='age')
simplot(sim3, sim2)


# Get likelihood for comparison
likelihood = OLD.create.msa.likelihood.v1.for.annals(msa)
liks = attr(likelihood, 'components')

EVERYTHING.WEIGHT=1/2
NEW.WEIGHT = 1/4
PREV.WEIGHT = 1
use.prev.to.new.cv.ratio=T
FOCUS.WEIGHT=1
new.correlated.year.chunks=list(2008:2014, 2015:2018)
prevalence.correlated.year.chunks=list(2007:2013, 2014:2017)

year.to.year.chunk.correlation=0.65
year.to.year.off.correlation=0.25
measurement.error.cv.vs.sqrt.weight = 1
measurement.error.sd.mult=1
new.cv = 0.065
prevalence.cv = 0.09
new.exp = 0.33
prevalence.exp = 0.69

#-- Elements for New Diagnoses --#
SD.INFLATION.NEW.NUM = 1/sqrt(NEW.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
SD.INFLATION.NEW = function(description){SD.INFLATION.NEW.NUM /
        (sqrt(FOCUS.WEIGHT)^as.numeric(to.focus(description)))}

#  NEW.SD = function(years, num){sqrt(measurement.error.cv.vs.sqrt.weight * (0.047*num)^2 +#from 2016 mult.exp
NEW.SD = function(years, num){sqrt(measurement.error.cv.vs.sqrt.weight * (new.cv*num)^2 +
                                       (1-measurement.error.cv.vs.sqrt.weight) * (num^new.exp)^2) *
        measurement.error.sd.mult}

to.focus = function(description){
    (grepl(' male',description) & (grepl('13-24', description) | grepl('25-34',description))) |
        (grepl('black, msm$',description) | grepl('hispanic, msm$', description))
}

#-- Elements for Prevalence --#
if (use.prev.to.new.cv.ratio)
    PREV.INFLATION = get.cv.weights(location=msa, weight.to='new')['prevalence']
if (!use.prev.to.new.cv.ratio)
    PREV.INFLATION = 1

SD.INFLATION.PREV.NUM = 1/sqrt(PREV.WEIGHT)/sqrt(EVERYTHING.WEIGHT)*PREV.INFLATION
SD.INFLATION.PREV = function(description){SD.INFLATION.PREV.NUM /
        (sqrt(FOCUS.WEIGHT)^as.numeric(to.focus(description)))}

PREV.SD = function(years, num){sqrt(measurement.error.cv.vs.sqrt.weight * (prevalence.cv*num)^2 + 
                                        (1-measurement.error.cv.vs.sqrt.weight) * (num^prevalence.exp)^2) *
        measurement.error.sd.mult}

#-- Pull Other Values to make likelihood --#
POPULATION.TOTALS=get.census.totals(ALL.DATA.MANAGERS$census.totals, msa)

dlik.prev = create.decomposed.likelihood.function(data.type='prevalence',
                                                  years = sort(unlist(prevalence.correlated.year.chunks)),
                                                  surv=msa.surveillance,
                                                  location=msa,
                                                  by.total = T,
                                                  by.sex.age=T,
                                                  by.sex.race=T,
                                                  by.sex.risk=T,
                                                  by.race.risk=T,
                                                  by.sex=F,
                                                  #by.risk=T,
                                                  #by.race=T,
                                                  #by.age=T,
                                                  population=POPULATION.TOTALS,
                                                  denominator.dimensions='year',
                                                  msm.cv=0,
                                                  idu.cv=0,
                                                  sd.inflation=SD.INFLATION.PREV,
                                                  year.to.year.correlation = 0,
                                                  numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                                  numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                                  numerator.chunk.years=prevalence.correlated.year.chunks,
                                                  numerator.sd = PREV.SD)

dlik.prev(sim1)


starting.parameters = pp3
save(starting.parameters, file='mcmc_runs/start_values/38900.Rdata')
