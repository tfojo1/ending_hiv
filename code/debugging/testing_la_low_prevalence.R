

source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')
source('code/calibration/calibrated_parameters_73.R')
mcmc = assemble.mcmc.from.cache('mcmc_runs/la.73_2020-07-21/',T)

msa = attr(mcmc@simulations[[1]], 'location')
base.components = setup.initial.components(msa=msa)
init.components = get.components.for.calibrated.parameters(mcmc@samples[1,1,], base.components)
init.components = fix.components.for.calibration(components = init.components)
run.simulation <- function(parameters)
{
    components = get.components.for.calibrated.parameters(parameters, init.components)
    run.jheem.from.components(components, max.run.time.seconds = Inf)
}

full.likelihood = create.msa.likelihood(msa)

pp1 = mcmc@samples[1,40,]
pp1[grepl('aging', names(pp1))]
sim1 = run.simulation(pp1)

plot.calibration.sex.age(sim1)

pp2 = pp1
pp2['msm.age3.aging.2'] = .2
sim2 = run.simulation(pp2)

plot.calibration.sex.age(list(sim1,sim2))
plot.calibration.risk.race(list(sim1,sim2))

pp3 = pp2
pp2['msm.age3.aging.2'] = .2
pp3['other.msm.trate.0'] = 1.2
pp3['msm.age2.aging.1'] = .15
pp3['msm.age4.aging.2'] = .15
pp3['msm.age1.aging.base'] = .4
pp3['age1.msm.susceptibility.rr1'] = 1.2
pp3['age2.msm.susceptibility.rr1'] = 1.2
pp3['age2.msm.susceptibility.rr2'] = 1.2
pp3['age5.susceptibility.rr'] = .15
sim3 = run.simulation(pp3)

plot.calibration.sex.age(list(sim1, sim3))
plot.calibration(list(sim1, sim3), facet.by=c('risk','race'), risk='msm')


exp(-full.likelihood(sim1, verbose = T) + full.likelihood(sim2, verbose=T))
