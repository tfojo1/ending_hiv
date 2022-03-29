
#load up a simset before you do the rest

#let's cut it down to 3 sims for the sake of time
sub.simset = subset.simset(simset, 1:3)
noint = run.simset.intervention(sub.simset, NO.INTERVENTION,
                                run.from.year = 2018, run.to.year = 2030)


# RETENTION
RET.95.NAIVE = create.intervention.unit(type='retention.naive',
                                        start.year=2023, rates=.99, years=2027)
RET.95.FAILING = create.intervention.unit(type='retention.failing',
                                        start.year=2023, rates=.99, years=2027)
RET.95.SUPPRESSED = create.intervention.unit(type='retention.suppressed',
                                        start.year=2023, rates=.99, years=2027)

INT.RET.95 = create.intervention(WHOLE.POPULATION,
                                 RET.95.NAIVE, RET.95.FAILING, RET.95.SUPPRESSED)

ss.ret.95 = run.simset.intervention(sub.simset, INT.RET.95,
                                    run.from.year = 2018, run.to.year = 2030)
simplot(noint, ss.ret.95, data.types=c('incidence', 'suppression', 'engagement'))

c1 = attr(ss.ret.95@simulations[[1]], 'components')
c0 = attr(noint@simulations[[1]], 'components')

# SUPPRESSION
SUPP.90 = create.intervention.unit(type='gain.of.suppression.failing',
                                   start.year=2023, rates=0.90, years=2027)
INT.SUPP.90 = create.intervention(WHOLE.POPULATION,
                                  SUPP.90)

ss.supp.90 = run.simset.intervention(sub.simset, INT.SUPP.90,
                                    run.from.year = 2018, run.to.year = 2030)
simplot(noint, ss.supp.90, data.types=c('incidence', 'suppression', 'suppression.of.engaged'), years=2020:2030)


# LINKAGE
LINK.95 = create.intervention.unit(type='linkage',
                                   start.year=2023, rates=0.99, 2027)
INT.LINK.95 = create.intervention(WHOLE.POPULATION, LINK.95)

ss.link.95 = run.simset.intervention(sub.simset, INT.LINK.95,
                                     run.from.year = 2018, run.to.year = 2030)
simplot(noint, ss.link.95, data.types=c('incidence', 'linkage', 'engagement'))

