


load('mcmc_runs/baltimore_croi_v2.Rdata')
simset =subset.simset(simset, 1:3) #don't do this
baseline = simset

ss.noint = run.simset.intervention(baseline, NO.INTERVENTION, run.from.year=2021, run.to.year=2031,
                                   keep.years=2018:2031) #don't do this
#load('mcmc_runs/file_name_for_no_interveniton.Rdata')
#ss.noint = simset

ss.int = run.simset.intervention(baseline, WHOLEPOP.AS95, run.from.year=2021, run.to.year=2031,
                                 keep.years=2018:2031) #don't do this

load('mcmc_runs/visualization_simsets/12580/1.0_12580_noint.Rdata')
ss.noint = simset

load('mcmc_runs/visualization_simsets/12580/1.0_12580_wholepop.c.p.high.Rdata')
ss.int = simset


source('code/processing/visualization/sim_plots.R')
simplot(ss.int, ss.noint, data.types='incidence', years=2020:2030) + theme(panel.background = element_blank())
