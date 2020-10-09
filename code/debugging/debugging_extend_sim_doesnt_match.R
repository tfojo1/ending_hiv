

load('mcmc_runs/systematic_initial/35620_1x20K_2020-10-01.Rdata')
simset = extract.simset(mcmc, additional.burn=520, additional.thin=6)
full.base.simset = prepare.simset.for.interventions(simset)
i = 40
base.sim = full.base.simset@simulations[[i]]
noint.sim = run.sim.intervention(base.sim,
                                 intervention = NO.INTERVENTION,
                                 run.from.year=attr(full.base.simset, 'run.from.year'), run.to.year=2030)

plot.calibration.total(list(base.sim, noint.sim), data.types=c('incidence','new'), years=2015:2025)

load('mcmc_runs/visualization_simsets/35620/1.0_35620_baseline.Rdata')
compressed.base.simset = simset

plot.calibration.total(list(base.sim, compressed.base.simset@simulations[[i]]), data.types=c('incidence','new'), 
                       years=2015:2025)

load('mcmc_runs/visualization_simsets/35620/1.0_35620_noint.Rdata')
compressed.noint.simset = simset
plot.calibration.total(list(base.sim, noint.sim, compressed.noint.simset@simulations[[i]]), data.types=c('incidence','new'), years=2015:2025)


load('mcmc_runs/visualization_simsets/35620/1.0_35620_seed.Rdata')
seed.simset = simset
plot.calibration.total(list(base.sim, seed.simset@simulations[[i]]), data.types=c('incidence','new'), years=2015:2025)

noint.sim.from.seed = run.sim.intervention(seed.simset@simulations[[i]],
                                           intervention = NO.INTERVENTION,
                                           run.from.year=attr(full.base.simset, 'run.from.year'), run.to.year=2030)
plot.calibration.total(list(base.sim, noint.sim.from.seed, compressed.noint.simset@simulations[[i]]), data.types=c('incidence','new'), years=2015:2025) +
    geom_vline(xintercept = 2018)
