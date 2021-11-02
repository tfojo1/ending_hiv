
source('code/source_code.R')
source('code/calibration/calibrated_parameters_expanded_1.R')
source('code/plots.R')
load('mcmc_runs/baltimore_initial_simset_v2.Rdata')

pp = simset@parameters[simset@n.sim,]
pp[grepl('.or$', names(pp))] = 1

run.simulation = create.run.simulation.function(msa=BALTIMORE.MSA, start.values=pp, 
                                                version = 'expanded_1.0', fix.components=F, catch.errors = F)

sim = run.simulation(pp, end.year=2030, keep.years=2010:2030)

plot.calibration.total(sim, data.types=c('engagement','suppression','suppression.of.engaged'), years=2011:2030)






plot.calibration(sim, data.types=c('engagement','suppression'), years=2011:2030)

plot.calibration(sim, data.types=c('engagement','suppression'), years=2011:2030, facet.by = 'race')

pp[grepl('proportion.lost',names(pp))]

pp2 = pp
pp2[c(135)] = 0.9
sim2 = run.simulation(pp2, end.year=2030, keep.years=2010:2030)
plot.calibration.total(list(sim, sim2), data.types=c('engagement','suppression'), years=2011:2030)

plot.calibration(list(sim, sim2), data.types=c('engagement'), years=2011:2030, 
                 facet.by = c('risk','age'))

pp2['suppressed.vs.nonsuppressed.proportion.lost.or'] = 1
pp2['already.lost.vs.nonsuppressed.proportion.lost.or'] = 1
