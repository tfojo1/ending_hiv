

source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

list.files('Q:Ending_HIV/mcmc_runs/systematic_caches')
mcmc = assemble.mcmc.from.cache('Q:Ending_HIV/mcmc_runs/systematic_caches/38900_1x20K_2022-02-20',T)

mcmc@n.iter
simset = extract.simset(mcmc, additional.burn=500, additional.thin=10)

simplot(simset)

simplot(simset, data.types='new', facet.by='age')
simplot(simset, data.types='new', facet.by='age', split.by='sex')


sim1 = simset@simulations[[simset@n.sim]]

simplot(sim1, data.types='new', facet.by='age', split.by='sex')


pp1 = simset@parameters[simset@n.sim,]
run.simulation = create.run.simulation.function(attr(sim1, 'location'), start.values = pp1)

pp2 = pp1
pp2['age1.msm.susceptibility.rr.mult.2'] = 1.5

sim2 = run.simulation(pp2)
simplot(sim1, sim2, data.types='new', facet.by='age', split.by='sex')

simplot(sim1, sim2, data.types='suppression', facet.by='age', split.by='sex')

sim.s = run.simulation(starting.parameters)
pp0 = starting.parameters
#pp0['age2.msm.susceptibility.rr.mult.1'] = 1.5
pp0['age1.msm.susceptibility.rr.mult.2'] = 1.5
pp0['age2.msm.susceptibility.rr.mult.2'] = 1.5
sim0 = run.simulation(pp0)

simplot(sim.s, sim0, data.types='new', facet.by='age', split.by='sex')
