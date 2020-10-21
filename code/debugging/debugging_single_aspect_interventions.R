
SEATTLE.MSA
load('mcmc_runs/visualization_simsets/42660/1.0_42660_baseline.Rdata')
baseline=simset
load('mcmc_runs/visualization_simsets/42660/1.0_42660_noint.Rdata')
noint = simset
load('mcmc_runs/visualization_simsets/42660/1.0_42660_ybhm.s90.Rdata')
int1 = simset
load('mcmc_runs/visualization_simsets/42660/1.0_42660_mi.tq2.ybh.tq1.x.Rdata')
int2 = simset
load('mcmc_runs/visualization_simsets/42660/1.0_42660_ybhm.1.25.80.Rdata')
int3 = simset

x=plot.simulations.total(list(baseline,noint,int1), years=2010:2030, data.types=c('incidence','new','suppression'), return.change.data.frame=T);x$plot
plot.simulations.total(list(baseline,noint,int1), years=2010:2030, data.types=c('suppression'))
plot.simulations.total(list(baseline,noint,int1), years=2010:2030, data.types=c('incidence'), plot.format='mean.and.interval')

plot.calibration.total(list(noint, int1), years=2020:2030, data.types='testing', plot.individual.simset.sims = F)
plot.calibration.total(list(noint, int1), years=2020:2030, data.types='suppression', plot.individual.simset.sims = F)

sim.index = 40
sim.noint = noint@simulations[[sim.index]]
sim.int1 = int1@simulations[[sim.index]]
sim.int2 = int2@simulations[[sim.index]]
sim.int3 = int3@simulations[[sim.index]]

plot.calibration.total(list(sim.noint, sim.int1), years=2020:2030, data.types='incidence')
plot.calibration.total(list(sim.noint, sim.int1), years=2020:2030, data.types='suppression')
plot.calibration(list(sim.noint, sim.int1), years=2020:2030, data.types='suppression', split.by='age', facet.by='race')

supp.noint = extract.suppression(sim.noint, years=2022, keep.dimensions=c('age','race','risk'), use.cdc.categorizations = T)
supp.noint[,,'msm']
supp.int1 = extract.suppression(sim.int1, years=2022, keep.dimensions=c('age','race','risk'), use.cdc.categorizations = T)
supp.int1[,,'msm']
supp.int3 = extract.suppression(sim.int3, years=2022, keep.dimensions=c('age','race','risk'), use.cdc.categorizations = T)
supp.int3[,,'msm']

#For testing
plot.calibration.total(list(sim.noint, sim.int2), years=2020:2030, data.types='incidence')
plot.calibration.total(list(sim.noint, sim.int2), years=2020:2030, data.types='testing')
plot.calibration.age(list(sim.noint, sim.int2), years=2020:2030, data.types='testing')


load('mcmc_runs/visualization_simsets/42660/1.0_42660_seed.Rdata')
seed = simset
SUPPRESSION.100.21.20 = create.intervention.unit('suppression', 2021, 1, 2022)
YBHM.S100 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, SUPPRESSION.100.21.20)
int4 = run.simset.intervention(seed, YBHM.S100)
get.simset.incidence.reduction(int4)
