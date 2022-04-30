

source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

mcmc = assemble.mcmc.from.cache('Q:Ending_HIV/mcmc_runs/systematic_caches/38900_4x100K_2022-02-06',T)

sim1 = mcmc@simulations[[1]]
sim2 = mcmc@simulations[[length(mcmc@simulations)]]


likelihood = create.msa.likelihood(msa=attr(sim1, 'location'))
liks = attr(likelihood, 'components')

simplot(sim1, sim2)

c(sim1=likelihood(sim1), sim2=likelihood(sim2))

z=t(sapply(liks, function(lik){
    c(sim1=lik(sim1), sim2=lik(sim2))
}));z



# Running
run.simulation = create.run.simulation.function(msa=attr(sim1, 'location'),
                                                start.values = mcmc@samples[1,1,])
pp0 = pp = mcmc@samples[1,mcmc@n.iter,]
sim0 = run.simulation(pp)
names(pp)[grepl('trate', names(pp))]

simplot(sim0)
simplot(sim0, facet.by='age', split.by='sex')

pp2 = pp
pp2['age2.msm.susceptibility.rr.mult.2'] = 2
pp2['age5.suppressed.or'] = 0.5


sim2 = run.simulation(pp2)
simplot(sim0, sim2, facet.by='age', split.by='sex')
simplot(sim0, sim2, data.types='suppression', facet.by='age')
simplot(sim0, sim2, data.types='diagnosed')


c(sim0=likelihood(sim0), sim2=likelihood(sim2))

z=t(sapply(liks, function(lik){
    c(sim0=lik(sim0), sim2=lik(sim2))
}));z



##-- LET'S DEBUG THE DIAGNOSED LIKELIHOOD --##


simplot(sim0, sim2, data.types='diagnosed')

msa = attr(mcmc@simulations[[1]], 'location')
EVERYTHING.WEIGHT=1/2
TOTAL.DX.WEIGHT = 1/16
diagnosed.years=2008:2019
nested.error.cache = make.nested.errors.cache(msa.surveillance, state.surveillance, county.surveillance)

TOTAL.SD.INFLATION.DX = 1/sqrt(TOTAL.DX.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
#STRATIFIED.SD.INFLATION.DX = 1/sqrt(STRATIFIED.DX.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
DIAGNOSED.OBS.ERROR.SD=function(...){0.005}
PROBABILITY.DIAGNOSIS.DECREASING = NA
dx.lik = create.nested.likelihood(data.type='diagnosed',
                                  years=diagnosed.years,
                                  msa=msa,
                                  
                                  msa.surveillance = msa.surveillance,
                                  state.surveillance = state.surveillance,
                                  county.surveillance = county.surveillance,
                                  
                                  observation.error.fn = DIAGNOSED.OBS.ERROR.SD,
                                  sd.inflation = TOTAL.SD.INFLATION.DX,
                                  
                                  probability.decreasing.slope=PROBABILITY.DIAGNOSIS.DECREASING,
                                  
                                  cached.errors = nested.error.cache
) 

sapply(list(sim0, sim2), dx.lik, debug=T)
dx.lik(sim0, debug=T)
get.surveillance.data(state.surveillance, 'OR', data.type='diagnosed')



##-- SUPP LIK --##
msa = attr(mcmc@simulations[[1]], 'location')
EVERYTHING.WEIGHT=1/2
SUPPRESSION.SD = 0.01
SUPPRESSION.WEIGHT = 1/16
suppression.years = 2008:2019

SUPPRESSION.OBS.ERROR.SD = function(...){SUPPRESSION.SD}
SUPPRESSION.SD.INFLATION = 1/sqrt(SUPPRESSION.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
PROBABILITY.SUPPRESSION.DECREASING = 0.05

suppressed.lik = create.nested.likelihood(data.type='suppression',
                                          years=suppression.years,
                                          msa=msa,
                                          
                                          msa.surveillance = msa.surveillance,
                                          state.surveillance = state.surveillance,
                                          county.surveillance = county.surveillance,
                                          
                                          observation.error.fn = SUPPRESSION.OBS.ERROR.SD,
                                          sd.inflation = SUPPRESSION.SD.INFLATION,
                                          
                                          probability.decreasing.slope=PROBABILITY.SUPPRESSION.DECREASING,
                                          
                                          cached.errors = nested.error.cache
)