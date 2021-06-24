
source('code/source_code.R')
source('code/plots.R')

load('mcmc_runs/full_simsets/1.0_12580_full.Rdata')
simset = thin.simset(simset, 200)
simset = prepare.simset.for.interventions(simset)

#building blocks

tpop.all = create.target.population()

t.2021.2030 = create.intervention.unit('testing', start.year=2021, rates=2, years=2022)
p.2021.2030 = create.intervention.unit('prep', start.year=2021, rates=.5, years=2022)
s.2021.2030 = create.intervention.unit('suppression', start.year=2021, rates=.9, years=2022)

s.2021.2025 = create.intervention.unit('suppression', start.year=2021, end.year=2025, rates=c(.9,.9), years=c(2022,2024))

s.half = create.intervention.unit('suppression', start.year=2021, end.year=2025, rates=c(.5,.5), years=c(2022,2024),
                                  apply.function = 'multiplier', allow.less.than.otherwise = T)

p.half = create.intervention.unit('prep', start.year=2021, end.year=2025, rates=c(.5,.5), years=c(2022,2024),
                                  apply.function = 'multiplier', allow.less.than.otherwise = T)

t.half = create.intervention.unit('testing', start.year=2021, end.year=2025, rates=c(.5,.5), years=c(2022,2024),
                                  apply.function = 'multiplier', allow.less.than.otherwise = T)


ht.half = create.intervention.unit('heterosexual.transmission', start.year=2020.25, end.year=2022,
                                   rates=c(0.5,0.5), years=c(2020.251, 2021.5), apply.function='multiplier',
                                   allow.less.than.otherwise = T)
msm.half = create.intervention.unit('msm.transmission', start.year=2020.25, end.year=2022,
                                   rates=c(0.5,0.5), years=c(2020.251, 2021.5), apply.function='multiplier',
                                   allow.less.than.otherwise = T)

idu.half = create.intervention.unit('idu.transmission', start.year=2020.25, end.year=2022,
                                    rates=c(0.5,0.5), years=c(2020.251, 2021.5), apply.function='multiplier',
                                    allow.less.than.otherwise = T)


#interventions

int.all.half = create.intervention(tpop.all, 
                                   ht.half, msm.half, idu.half,
                                   t.half, p.half, s.half)
ss.all.half = run.simset.intervention(simset, int.all.half)
plot.calibration(list(ss.noint,ss.all.half),
                 data.types='incidence', years=2020:2030, sim1.line.size = 1, sim1.alpha = 1)


int.idu.half = create.intervention(tpop.all, idu.half)
ss.idu.half = run.simset.intervention(simset, int.idu.half)
plot.calibration(list(ss.noint,ss.idu.half),
                 data.types='incidence', years=2020:2030, sim1.line.size = 1, sim1.alpha = 1)


int.st.half = create.intervention(tpop.all, ht.half,msm.half)

ss.st.half = run.simset.intervention(simset, int.st.half)
plot.calibration(list(ss.noint,ss.st.half),
                 data.types='incidence', years=2020:2030, sim1.line.size = 1, sim1.alpha = 1)





int.ht.half = create.intervention(tpop.all, ht.half)
ss.ht.half = run.simset.intervention(simset, int.ht.half)

plot.calibration.risk(list(simset,ss.ht.half), risk='heterosexual',
                 data.types='incidence', years=2010:2030, sim1.line.size = 2)

c1 = attr(ss.ht.half@simulations[[1]], 'components')
r = sapply(c1$sexual.contact.arrays)

plot.calibration(list(simset,ss.ht.half),
                 data.types='incidence', years=2010:2030, sim1.line.size = 2)



plot.calibration(list(ss.noint,ss.ht.half),
                 data.types='incidence', years=2010:2030, sim1.line.size = 1, sim1.alpha = 1)


int.tps.2021.2030 = create.intervention(tpop.all,
                                        t.2021.2030, p.2021.2030, s.2021.2030)

ss.noint = run.simset.intervention(simset, NO.INTERVENTION)

ss.tps.2021.2030 = run.simset.intervention(simset, int.tps.2021.2030)
plot.calibration(ss.tps.2021.2030, data.types='suppression', years=2010:2030, sim1.line.size = 2)

int.s.2021.2025 = create.intervention(tpop.all, s.2021.2025)
ss.s.2021.2025 = run.simset.intervention(simset, int.s.2021.2025)
plot.calibration(ss.s.2021.2025, data.types='suppression', years=2010:2030, sim1.line.size = 2)

int.s.half = create.intervention(tpop.all, s.half)
ss.s.half = run.simset.intervention(simset, int.s.half)
plot.calibration(ss.s.half, data.types='suppression', years=2010:2030, sim1.line.size = 2)
plot.calibration.age(ss.s.half, data.types='suppression', years=2010:2030, sim1.line.size = 2)


c1 = attr(ss.s.2021.2025@simulations[[1]], 'components')
