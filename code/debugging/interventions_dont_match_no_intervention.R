source('code/source_code.R')

load('mcmc_runs/limited_simset_interventions/35620/No_Intervention.Rdata')
base.simset=simset
load('mcmc_runs/limited_simset_interventions/35620/Young_Black_and_Hispanic_MSM_testing_1pyr_0.8_suppressed_0.25_PrEP.Rdata')
intervention.simset=simset
load('mcmc_runs/limited_simsets/35620.Rdata')


i=1;plot.calibration.total.incidence(list(base.simset@simulations[[i]], intervention.simset@simulations[[i]]))
i=1;plot.calibration.total.incidence(list(simset@simulations[[i]], base.simset@simulations[[i]]))



int.comps=attr(intervention.simset@simulations[[1]], 'components')
base.comps=attr(base.simset@simulations[[1]], 'components')

x = setup.jheem.from.components(base.comps)
x = setup.jheem.from.components(int.comps)

source('code/check_inner_workings/plot_smoothed_proportions.R')
plot.smoothed.proportions(attr(base.comps, 'smoothed.suppressed.proportions'), facet.by='race') + xlim(2019,NA)
plot.smoothed.proportions(attr(int.comps, 'smoothed.suppressed.proportions'), facet.by='race') + xlim(2019,NA)

plot.smoothed.proportions(attr(base.comps, 'smoothed.testing.proportions'), facet.by='race') + xlim(2019,NA)
plot.smoothed.proportions(attr(int.comps, 'smoothed.testing.proportions'), facet.by='race') + xlim(2019,NA)



plot.calibration.total.incidence(list(base.simset, intervention.simset))

i=1;plot.calibration.total.incidence(list(base.simset@simulations[[i]], intervention.simset@simulations[[i]]))
i=1;plot.calibration.total.incidence(list(simset@simulations[[i]], base.simset@simulations[[i]]))


sim = simset@simulations[[1]]

components.after.prepare = attr(sim, 'components')
sim2 = run.jheem.from.components(components.after.prepare, start.year=2010, end.year=2030, prior.results = sim)

plot.calibration.total.incidence(list(sim, sim2))

load('mcmc_runs/systematic_parallel/35620_4x60K_2020-05-23.Rdata')
simset.unprepared = extract.simset(mcmc, additional.burn=220, additional.thin=10)
components.before.prepare = attr(simset.unprepared@simulations[[1]], 'components')

plot.calibration.total.incidence(list(sim, simset.unprepared@simulations[[1]]))

sim3 = run.jheem.from.components(components.before.prepare, start.year=2010, end.year=2030, prior.results = sim)
plot.calibration.total.incidence(list(sim, sim3))

parameters.before.prepare = simset.unprepared@parameters[1,]
comps = unfix.jheem.components(components.before.prepare)
comps = get.components.for.calibrated.parameters(parameters.before.prepare, comps)
comps = fix.jheem.components(comps)

sim4 = run.jheem.from.components(comps, start.year=2010, end.year=2030, prior.results = sim)
plot.calibration.total.incidence(list(sim, sim4))
plot.calibration.total.incidence(list(sim2, sim4))

comps2 = unfix.jheem.components(comps)
comps2 = get.components.for.calibrated.parameters(parameters.before.prepare, comps2)
comps2 = fix.jheem.components(comps2)
sim5 = run.jheem.from.components(comps2, start.year=2010, end.year=2030, prior.results = sim)
plot.calibration.total.incidence(list(sim4, sim5))
plot.calibration.total.incidence(list(sim, sim4, sim5))
