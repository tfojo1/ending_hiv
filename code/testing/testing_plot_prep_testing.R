


load('mcmc_runs/full_simsets/1.0_35620_full.Rdata')
base = simset
load('mcmc_runs/full_simsets/35620/1.0_35620_noint.Rdata')
noint = simset

plot = plot.simulations.flex(base, split.by=NULL, data.type='prep'); plot
