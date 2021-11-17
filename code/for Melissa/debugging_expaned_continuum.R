
source('code/source_code.R')
source('code/calibration/calibrated_parameters_expanded_1.R')
source('code/plots.R')
load('mcmc_runs/baltimore_initial_simset_v2.Rdata')

pp = simset@parameters[simset@n.sim,]
pp[grepl('.or$', names(pp))] = 1


pp = get.medians(parameters.prior)
load('mcmc_runs/start_values/12580.Rdata')
to.copy = intersect(names(pp), names(starting.parameters))
pp[to.copy] = starting.parameters[to.copy]

run.simulation = create.run.simulation.function(msa=BALTIMORE.MSA, start.values=pp, 
                                                version = 'expanded_1.0', fix.components=F, catch.errors = F)

sim = run.simulation(pp, end.year=2030, keep.years=2010:2030)

plot.calibration.total(sim, data.types=c('engagement','suppression','suppression.of.engaged'), years=2011:2030)




pp2 = pp
pp2['heterosexual.proportion.lost.or'] = 
    pp2['msm.proportion.lost.or'] = 
    pp2['msm.idu.proportion.lost.or'] = 
    pp2['idu.proportion.lost.or'] = 2
pp2['heterosexual.proportion.lost.slope.or'] = 
    pp2['msm.proportion.lost.slope.or'] = 
    pp2['msm.idu.proportion.lost.slope.or'] = 
    pp2['idu.proportion.lost.slope.or'] = 1.05

pp2['heterosexual.proportion.adherent.slope.or'] = 
    pp2['msm.proportion.adherent.slope.or'] = 
    pp2['msm.idu.proportion.adherent.slope.or'] = 
    pp2['idu.proportion.adherent.slope.or'] = 1.1

sim2 = run.simulation(pp2, end.year=2030, keep.years=2010:2030)

plot.calibration.total(list(sim,sim2), data.types=c('engagement','suppression','suppression.of.engaged'), years=2011:2030)




source('code/interventions/melissa_croi_interventions_2022.R')

sim2.int = run.sim.intervention(sim2, WHOLEPOP.R95, run.from.year = 2018, run.to.year = 2030)
plot.calibration.total(list(sim2,sim2.int), data.types=c('engagement','suppression','suppression.of.engaged','incidence'), years=2011:2030)

sim2.inc = project.absolute.incidence(sim2, years=c(2020,2030))
(sim2.inc[1]-sim2.inc[2])/sim2.inc[1]
sim2.int.inc = project.absolute.incidence(sim2.int, years=c(2020,2030))
(sim2.int.inc[1]-sim2.int.inc[2])/sim2.int.inc[1]



sim2.int = run.sim.intervention(sim2, WHOLEPOP.AS95, run.from.year = 2018, run.to.year = 2030)
plot.calibration.total(list(sim2,sim2.int), data.types=c('engagement','suppression','suppression.of.engaged','incidence'), years=2011:2030)




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
