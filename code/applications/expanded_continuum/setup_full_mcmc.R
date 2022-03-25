

source('code/source_code.R')
source('code/calibration/parameter_mappings/calibrated_parameters_expanded_2.R')
source('code/processing/visualization/sim_plots.R')

# Likelihood
likelihood = create.msa.likelihood(BALTIMORE.MSA,
                                   include.engagement=T, include.linkage=T)

N.ITER = 130000
THIN = 130
UPDATE.FREQ = 200
N.CHAINS=4
# Set up and run
setup.parallel.mcmc.for.msa(msa=BALTIMORE.MSA,
                            version='expanded_1.0',
                            likelihood=likelihood,
                            
                            chains=N.CHAINS,
                            n.iter=N.ITER,
                            thin=THIN,
                            burn=0,
                            max.sim.time=20,
                            save.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_parallel_expanded'),
                            cache.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_caches_expanded'),
                            initial.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_initial_expanded'),
                            
                            update.frequency=UPDATE.FREQ,#200,
                            save.suffix='expanded',
                            
                            run=F,
                            verbose=T)
