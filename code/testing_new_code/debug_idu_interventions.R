source('code/source_code.R')

load('mcmc_runs/quick_simsets/1.0_35620_quick.Rdata')
baseline = simset
baseline.sim1 = simset@simulations[[1]]
baseline.comps = attr(baseline.sim1, 'components')
baseline.comps = unfix.jheem.components(baseline.comps)
baseline.comps = setup.needle.exchange.remission.effect(baseline.comps, 
                                                    needle.exchange.remission.rate.ratio = BASE_PARAMETER_VALUES['needle.exchange.remission.rate.ratio'])
baseline.comps = setup.needle.exchange.susceptibility(baseline.comps,
                                                  needle.exchange.rr = BASE_PARAMETER_VALUES['needle.exchange.rr'])
baseline.comps = crunch.all.jheem.components(baseline.comps)

start.year = 2023
end.year = 2027
PREP.25 = create.intervention.unit('prep', start.year, 0.25, end.year)
NEEDLE.EXCHANGE.25 = create.intervention.unit('needle.exchange', start.year, 0.25, end.year)
MOUD.25 = create.proportion.multiplier.intervention.unit('idu.relapse', start.year,
                                                         proportions=0.25 * BASE_PARAMETER_VALUES['fraction.opioid.of.idu'],
                                                         multipliers=BASE_PARAMETER_VALUES['moud.relapse.rr'],
                                                         apply.function = 'multiplier',
                                                         allow.less.than.otherwise = T,
                                                         end.year)


INT.M = create.intervention(ALL.PRIOR.IDU, MOUD.25)

sim.noint = run.jheem.from.components(baseline.comps, start.year=2018, end.year=2030, prior.results = baseline.sim1)

comps.m = setup.components.for.intervention(components = baseline.comps, intervention = INT.M)
sim.m = run.jheem.from.components(comps.m, start.year=2018, end.year=2030, prior.results = baseline.sim1)


source('code/plots.R')
plot.calibration(list(sim.noint, sim.m), years=2020:2030, data.types=c('incidence','prep'))

extract.population.subset(sim.noint, keep.dimensions=c('year','risk'), years=2020:2030)
extract.population.subset(sim.m, keep.dimensions=c('year','risk'), years=2020:2030)





INT.P = create.intervention(NON.ACTIVE.IDU, PREP.25)

INT.N = create.intervention(ALL.ACTIVE.IDU, NEEDLE.EXCHANGE.25)


INT.PN = join.interventions(
    create.intervention(NON.ACTIVE.IDU, PREP.25),
    create.intervention(ALL.ACTIVE.IDU, NEEDLE.EXCHANGE.25)
)

INT.PNM = join.interventions(
    create.intervention(NON.ACTIVE.IDU, PREP.25),
    create.intervention(ALL.ACTIVE.IDU, NEEDLE.EXCHANGE.25),
    create.intervention(ALL.PRIOR.IDU, MOUD.25)
)

sim.noint = run.jheem.from.components(baseline.comps, start.year=2018, end.year=2030, prior.results = baseline.sim1)

comps.p = setup.components.for.intervention(components = baseline.comps, intervention = INT.P)
sim.p = run.jheem.from.components(comps.p, start.year=2018, end.year=2030, prior.results = baseline.sim1)

source('code/plots.R')
plot.calibration(list(sim.noint, sim.p), years=2020:2030, data.types=c('incidence','prep'))

comps.n = setup.components.for.intervention(components = baseline.comps, intervention = INT.N)
sim.n = run.jheem.from.components(comps.n, start.year=2018, end.year=2030, prior.results = baseline.sim1)
check.c.p = attr(sim.n, 'components')

plot.calibration(list(sim.noint, sim.n), years=2020:2030, data.types=c('incidence','prep'))
plot.calibration(list(sim.noint, sim.n), years=2020:2030, data.types=c('incidence','population'), use.cdc = F)


comps.pn = setup.components.for.intervention(components = baseline.comps, intervention = INT.PN)
sim.pn = run.jheem.from.components(comps.pn, start.year=2018, end.year=2030, prior.results = baseline.sim1)

plot.calibration(list(sim.noint, sim.pn), years=2020:2030, data.types=c('incidence','prep'))



comps.pnm = setup.components.for.intervention(components = baseline.comps, intervention = INT.PNM)
sim.pnm = run.jheem.from.components(comps.pnm, start.year=2018, end.year=2030, prior.results = baseline.sim1)

plot.calibration(list(sim.noint, sim.pnm), years=2020:2030, data.types=c('incidence','prep'))

plot.calibration(list(sim.noint, sim.pnm), years=2020:2030, data.types='population')


#dig into the rates of the components

comps.pn = setup.components.for.intervention(components = baseline.comps, intervention = INT.PN); comps.pn = crunch.all.jheem.components(comps.pn)

comps.n = setup.components.for.intervention(components = baseline.comps, intervention = INT.N); comps.n = crunch.all.jheem.components(comps.n)

comps.p = setup.components.for.intervention(components = baseline.comps, intervention = INT.P); comps.p = crunch.all.jheem.components(comps.p)

mean.susc.pn = sapply(comps.pn$idu.susceptibility, function(arr){
    mean(arr[,,,,'active_IDU',])
})
mean.susc.n = sapply(comps.n$idu.susceptibility, function(arr){
    mean(arr[,,,,'active_IDU',])
})
mean.susc.p = sapply(comps.n$idu.susceptibility, function(arr){
    mean(arr[,,,,'active_IDU',])
})
mean.susc.noint = sapply(baseline.comps$idu.susceptibility, function(arr){
    mean(arr[,,,,'active_IDU',])
})

data.frame(year=baseline.comps$idu.susceptibility.years,
               noint=mean.susc.noint,
               pn=mean.susc.pn,
           n=mean.susc.n,
           p=mean.susc.p)

i = 16
comps.n$idu.susceptibility.years[i]
all(comps.n$idu.susceptibility[[i]] == comps.pn$idu.susceptibility[[i]])
range(comps.n$idu.susceptibility[[i]] - comps.pn$idu.susceptibility[[i]])
