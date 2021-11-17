
#load('mcmc_runs/quick_simsets/1.0_12580_full.Rdata')
#load('mcmc_runs/baltimore_initial_simset_2021-11-11.Rdata')
load('mcmc_runs/baltimore_noint.Rdata')

source('code/visualization/sim_plots.R')


sim1 = simset@simulations[[1]]
sim2 = simset@simulations[[simset@n.sim]]


simplot(simset)
simplot(simset, plot.individual.simset.sims = T)

simplot(sim1)
simplot(sim1, sim2, data.types=c('new','prevalence','suppression'))

simplot(sim1, simset, plot.individual.simset.sims = T)
simplot(sim1, simset, plot.individual.simset.sims = F)

simplot(sim1, data.types=c('new','suppression'))
simplot(simset, data.types=c('suppression', 'engagement'))
simplot(simset, data.types=c('suppression', 'engagement','suppression.of.engaged'))
simplot(sim1, data.types=c('suppression', 'engagement','suppression.of.engaged'))
simplot(simset, data.types=c('suppression.of.engaged'))

simplot(simset, data.types='suppression', facet.by = 'risk')

simplot(sim1, data.types=c('suppression'), facet.by='risk')

simplot(sim1, data.types=c('new','suppression'), split.by='sex')

source('code/visualization/plot_wrappers.R')
plot.simulations.flex(simset)
