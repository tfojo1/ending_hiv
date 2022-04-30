source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

load('Q:Ending_HIV/mcmc_runs/systematic_initial_expanded/12580_2022-03-26.Rdata')
simset = extract.simset(mcmc, additional.burn=500, additional.thin=10)

simplot(simset)
simplot(simset, facet.by='risk')
simplot(simset, data.types=c('engagement','suppression','linkage','retention'))
simplot(simset, data.types=c('engagement','suppression','suppression.of.engaged','retention'))

trace.plot(mcmc, '*already')


sim = mcmc@simulations[[length(mcmc@simulations)]]

comps = attr(sim, 'components')

extract.retention(sim)
reengagement = calculate.rates(comps, 'reengagement')
sapply(reengagement$rates, mean)
