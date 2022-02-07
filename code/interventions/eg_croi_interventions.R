

source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

mcmc.noret = assemble.mcmc.from.cache('Q:Ending_HIV/mcmc_runs/systematic_caches_expanded/12580_1x25K_expanded_prep2_noret_2022-02-05',T)
mcmc.wret = assemble.mcmc.from.cache('Q:Ending_HIV/mcmc_runs/systematic_caches_expanded/12580_1x25K_expanded_prep2_wret_2022-02-05',T)

mcmc.noret@n.iter
simset = extract.simset(mcmc.noret, additional.burn=500, additional.thin=5)
simset.wret = extract.simset(mcmc.wret, additional.burn=500, additional.thin=5)


simplot(simset)
simplot(simset, simset.wret, data.types=c('retention','suppression','engagement'), years=2010:2019) +
    geom_hline(yintercept = c(.9,.95))

simplot(simset, simset.wret, data.types=c('retention'), facet.by='continuum', years=2010:2019) +
    geom_hline(yintercept = c(.9,.95))


simset = subset.simset(simset.wret, 70:72)

source('code/interventions/melissa_croi_interventions_2022_v2.R')

ss.r90 = run.simset.intervention(simset, WHOLEPOP.R90, run.from.year = 2020, run.to.year = 2031,
                                 keep.years=2018:2031)
ss.r95 = run.simset.intervention(simset, WHOLEPOP.R95, run.from.year = 2020, run.to.year = 2031,
                                 keep.years=2018:2031)
noint = run.simset.intervention(simset, NO.INTERVENTION, run.from.year = 2020, run.to.year = 2031,
                                keep.years=2018:2031)

ss.s90 = run.simset.intervention(simset, WHOLEPOP.AS90, run.from.year = 2020, run.to.year = 2031,
                                 keep.years=2018:2031)


simplot(noint, ss.r95, data.types=c('incidence','engagement','suppression'), years=2020:2030 )
simplot(noint, ss.r95, data.types=c('retention'), facet.by='continuum', years=2020:2030 )

simplot(noint, ss.s90, data.types=c('incidence','engagement','suppression'), years=2020:2030 )




RETENTION.SUPP.99 = create.intervention.unit(type = "retention.suppressed", rates = .99, start.year = 2023, years = 2027)
RETENTION.FAILING.99 = create.intervention.unit(type = "retention.failing", rates = .99, start.year = 2023, years = 2027)
RETENTION.NAIVE.99 = create.intervention.unit(type = "retention.naive", rates = .99, start.year = 2023, years = 2027)

WHOLEPOP.RS99 = create.intervention(WHOLE.POPULATION, RETENTION.SUPP.99)
WHOLEPOP.RF99 = create.intervention(WHOLE.POPULATION, RETENTION.FAILING.99)
WHOLEPOP.RN99 = create.intervention(WHOLE.POPULATION, RETENTION.NAIVE.99)
WHOLEPOP.R99 = join.interventions(WHOLEPOP.RS99, WHOLEPOP.RF99, WHOLEPOP.RN99)

ss.r99 = run.simset.intervention(simset, WHOLEPOP.R99, run.from.year = 2020, run.to.year = 2031,
                                 keep.years=2018:2031)

simplot(noint, ss.r99, data.types=c('incidence','engagement','suppression','retention'), years=2020:2030 )


INTERVENTIONS.TO.RUN = list(
    WHOLEPOP.AS90,
    WHOLEPOP.L90
)


run.systematic.interventions(simset, 
                             interventions = INTERVENTIONS.TO.RUN,
                             dst.dir=file.path('mcmc_runs/visualization_simsets', attr(simset@simulations[[1]], 'location')),
                             overwrite = T)

simset = simset.wret
save(simset, file='mcmc_runs/baltimore_croi_v2.Rdata')
