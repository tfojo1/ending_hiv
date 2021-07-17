
load('mcmc_runs/quick_simsets/1.0_35620_quick.Rdata')
baseline = simset
baseline.sim1 = simset@simulations[[1]]

load('mcmc_runs/quick_simsets/35620/1.0_35620_non.idu.p25_idu.n25.moud25_23.27.Rdata')
int1 = simset
c1 = attr(int1@simulations[[1]], 'components')
c1$needle.exchange.remission.rate.ratio
c1$needle.exchange.rr



x1 = attr(int1, 'intervention')
x1$raw$idu.relapse$intervention.units[[1]]

load('mcmc_runs/quick_simsets/35620/1.0_35620_noint.Rdata')
noint = simset

load('mcmc_runs/quick_simsets/35620/1.0_35620_all.p25_23.27.Rdata')
int2 = simset

source('code/plots.R')
plot.calibration(list(noint, int1), years=2020:2030, plot.individual.simset.sims = F, data.types=c('incidence','new'))
plot.calibration(list(int2, int1), years=2020:2030, plot.individual.simset.sims = F, data.types=c('incidence','new'))

plot.calibration(list(noint, int1), years=2020:2030, plot.individual.simset.sims = F, data.types=c('incidence','prep'))
plot.calibration(list(int2, int1), years=2020:2030, plot.individual.simset.sims = F, data.types=c('incidence','prep'))


check.sim = run.jheem.from.components(c1, start.year=2018, end.year=2030, prior.results = baseline.sim1)
plot.calibration(list(noint@simulations[[1]], check.sim), years=2020:2030, plot.individual.simset.sims = F, data.types=c('incidence','prep'))
plot.calibration(list(int1@simulations[[1]], check.sim), years=2020:2030, plot.individual.simset.sims = F, data.types=c('incidence','prep'))
plot.calibration(list(noint@simulations[[1]], int1@simulations[[1]]), years=2020:2030, plot.individual.simset.sims = F, data.types=c('incidence','prep'))
