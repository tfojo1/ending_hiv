

source('code/source_code.R')

mcmc = assemble.mcmc.from.cache('Q:Ending_HIV/mcmc_runs/systematic_caches_expanded/12580_4x130K_expanded_2022-02-02', T)

mcmc@n.iter
trace.plot(mcmc, '*proportion.lost')

source('code/processing/visualization/sim_plots.R')
simplot(mcmc@simulations[[length(mcmc@simulations)]])

simset = extract.simset(mcmc, additional.burn = mcmc@n.iter - 200/4)


#broken, so we're going to do this
simset = new('simset',
             n.parameters=mcmc@n.var,
             n.sim=200,
             simulations=mcmc@simulations[length(mcmc@simulations)-200 + 1:200])
simplot(simset)
simplot(simset, data.types=c('suppression','engagement','linkage'))


save(simset, file='Q:Ending_HIV/mcmc_runs/expanded_simsets/baltimore_croi_v1.Rdata')
