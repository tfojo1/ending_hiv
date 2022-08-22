
#load up the simset from baseline_quick_expanded

msa = get.simset.location(simset)

source('code/processing/visualization/sim_plots.R')
simplot(simset)
simplot(simset, facet.by='risk')
simplot(simset, facet.by='race')

simplot(simset, data.types=c('linkage','engagement','retention','suppression'))

simplot(simset, data.types='engagement', facet.by='race')
simplot(simset, data.types='engagement', facet.by='risk')

simplot(simset, data.types='suppression', facet.by='race')
simplot(simset, data.types='suppression', facet.by='risk')


#-- BELOW - RUN INDIVIDUAL SIMULATIONS AND CHECK THE LIKELIHOOD --#

sim = sim1 = simset@simulations[[simset@n.sim]]
params = simset@parameters[simset@n.sim,]

simplot(sim, data.types='suppression', facet.by='risk')
simplot(sim, data.types='engagement', facet.by='risk')

run.simulation = create.run.simulation.function(msa, version='expanded_1.0', params)

load(file.path('start_values/expanded_1.0/', paste0(msa ,'.Rdata')))
params0 = starting.parameters
sim0 = run.simulation(params0)

params2 = params

params2['hispanic.proportion.lost.or'] = 1

params2['age1.start.art.or'] = 2
params2['age5.start.art.or'] = 20
params2['age2.proportion.adherent.or'] = 5
params2['age5.proportion.adherent.or'] = 5
params2['age1.proportion.adherent.slope.or'] = 2

#params2['hiv.mortality.2'] = 0.005
sim2 = run.simulation(params2)
simplot(sim, sim2, data.types='engagement', facet.by='race')

simplot(sim, sim2, data.types='suppression', facet.by='risk')
simplot(sim, sim2, data.types='suppression', facet.by='race')
simplot(sim, sim2, data.types='suppression', facet.by='age')
simplot(sim, sim2, data.types='suppression', facet.by='sex')
simplot(sim, sim2, data.types='suppression', facet.by=NULL)
simplot(sim, sim2, data.types='engagement', facet.by='risk')
simplot(sim, sim2, data.types='engagement', facet.by='race')
simplot(sim, sim2, data.types='engagement', facet.by='age')
simplot(sim, sim2, data.types='engagement', facet.by='sex')
simplot(sim, sim2, data.types='engagement')


simplot(sim, sim2, data.types='suppression', facet.by='race')
simplot(sim, sim2, data.types='engagement', facet.by='race')
simplot(sim, sim2, facet.by='race')
#simplot(sim, sim2, data.types='engagement', facet.by='risk')

simplot(sim, sim2, data.types=c('suppression','suppression.of.engaged'), facet.by='race')
simplot(sim, sim2, data.types='suppression', facet.by='race')
simplot(sim, sim2, data.types='suppression', facet.by='risk')
simplot(sim, sim2, data.types='suppression', facet.by='age')

simplot(sim, sim2, data.types=c('engagement','suppression'), facet.by='race')
simplot(sim, sim2, data.types=c('engagement','suppression'), facet.by='risk')


simplot(sim, sim2, data.types='mortality')
simplot(sim, sim2, data.types='diagnosed')
simplot(sim, sim2, data.types='diagnosed', facet.by = 'race')

lik = create.msa.likelihood(msa, version='expanded_1.0')
lik(sim2) - lik(sim)

lik(sim) - lik(sim0)

liks = attr(lik, 'components')
liks[['engagement']](sim2) - liks[['engagement']](sim)

sapply(liks, function(l){
    l(sim2) - l(sim)
})

sapply(liks, function(l){
    l(sim) - l(sim0)
})

prior = get.parameters.prior.for.version('expanded_1.0')
calculate.density(prior, params2) - calculate.density(prior, params)

liks[['supp']](sim2, debug=T)


orig.new = liks[['new']]
orig.prev = liks[['prev']]
orig.mort = liks[['mort']]

lik(sim2) - lik(sim)

new.new = create.msa.likelihood(msa, version='expanded_1.0', debug.component = 'new', NEW.WEIGHT = 1/8)
new.prev = create.msa.likelihood(msa, version='expanded_1.0', debug.component = 'prevalence', NEW.WEIGHT = 1/8)
new.mort = create.msa.likelihood(msa, version='expanded_1.0', debug.component = 'mortality', NEW.WEIGHT = 1/8)

orig.new(sim2) - orig.new(sim)
new.new(sim2) - new.new(sim)

orig.prev(sim2) - orig.prev(sim)
new.prev(sim2) - new.prev(sim)

orig.mort(sim2) - orig.mort(sim)
new.mort(sim2) - new.mort(sim)

