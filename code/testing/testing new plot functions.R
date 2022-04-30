
source('code/source_code.R')
load('mcmc_runs/baltimore_initial_simset_v2.Rdata')


plot.calibration.total(simset@simulations[[1]], data.types=c('suppression', 'engagement', 'suppression.of.engaged'),
                       years=2010:2030)

