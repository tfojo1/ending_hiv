

source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

# Likelihood
suffix = ''
print("Creating Likelihood")
likelihood = create.msa.likelihood(BALTIMORE.MSA,
                                   version='expanded_1.0')

print('Done - setting up for MCMC')
set.seed(12444)

N.ITER = 25000
THIN = 25
UPDATE.FREQ = 200
# Set up and run
mcmc = setup.initial.mcmc.for.msa(msa=BALTIMORE.MSA,
                           version='expanded_1.0',
                           likelihood=likelihood,
                           
                           chains=1,
                           n.iter=N.ITER,
                           thin=THIN,
                           burn=0,
                           max.sim.time=20,
                           save.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_initial_expanded'),
                           cache.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_caches_expanded'),
                           update.frequency=UPDATE.FREQ,#200,
                           save.suffix=paste0('expanded', suffix),
                           
                           target.acceptance.rate=0.1,
                           SCALING.BASE.UPDATE = 1,
                           SCALING.UPDATE.PRIOR=10,
                           SCALING.UPDATE.DECAY=.5,#0.25,
                           COV.BASE.UPDATE=0.2,
                           COV.UPDATE.PRIOR=500,
                           COV.UPDATE.DECAY=0.5,#0.25
                           
                           run=T,
                           plot.first.sim = F,
                           verbose=T)


save(mcmc, file=paste0('Q:/Ending_HIV/mcmc_runs/systematic_initial_expanded', '/12580',suffix, '_', Sys.Date(),'.Rdata'))

# to cut simset
if (1==2)
{
    simset = extract.simset(mcmc, additional.burn=596, additional.thin=2);simset@n.sim
    save(simset, file=paste0('mcmc_runs/baltimore_initial_simset_', Sys.Date(),'.Rdata'))
}

# to resume
if (1==2)
{
    mcmc = run.mcmc.from.cache(dir = file.path(SYSTEMATIC.ROOT.DIR, 'systematic_caches_expanded', '12580_1x25K_expanded_infl.nobs.wt2_2022-03-18'),
                        update.frequency = UPDATE.FREQ, update.detail = 'high')
    
    save(mcmc, file=paste0('Q:/Ending_HIV/mcmc_runs/systematic_initial_expanded', '/12580',suffix, '_', Sys.Date(),'.Rdata'))
    
    
    simset = extract.simset(mcmc, additional.burn=500, additional.thin=5)
    
    plot.calibration(simset)
    plot.calibration.total(simset, data.types=c('linkage','engagement','suppression'))
}
