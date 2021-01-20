
if (1==2)
{
    setwd('../Ending_HIV/')
}

source('code/plots.R')
source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')
source('code/targets/target_msas.R')

set.seed(5557)
 
msa=SEATTLE.MSA
save.suffix = 'final.2011'
DO.vIS.INTERVENTIONS = T

RESUME=F
if (RESUME)
{
    print('----------------------------------------------------')
    print(paste0("RESUMING initial MCMC on ", msa.names(msa)))
    print(save.suffix)
    print('----------------------------------------------------')
    print(qplot(1,1) + ggtitle(msa.names(msa)) + theme(plot.title=element_text(hjust=1)))
    mcmc = run.mcmc.for.msa.cache(paste0('mcmc_runs/systematic_caches/',msa,'_1x20K_final.2011_2020-12-15/'))
}
if (!RESUME)
{
    print('----------------------------------------------------')
    print(paste0("Setting up to run initial MCMC on ", msa.names(msa)))
    print(save.suffix)
    print('----------------------------------------------------')
mcmc = setup.initial.mcmc.for.msa(msa, run=T, save.suffix = save.suffix, 
                                  target.acceptance.rate = 0.100, derive.step.size.from.prior.mcmc = F,
                                  likelihood = create.msa.likelihood(msa=msa, EVERYTHING.WEIGHT=1/8))
}
#if (save.suffix=='')
    save.name = paste0(msa, '.Rdata')
#if (save.suffix != '')
#    paste0(msa, '_', save.suffix, '.Rdata')

save(mcmc, file=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_initial', save.name))




#to make the visualization simset
if (DO.vIS.INTERVENTIONS)
{
    source('code/source_code.R')
    source('code/interventions/systematic_interventions.R')
 #   load('mcmc_runs/systematic_initial/40900.Rdata')
    
    simset = extract.simset(mcmc, additional.burn=520, additional.thin=6)
    simset@n.sim
    msa.names(attr(simset@simulations[[1]], 'location'))
    run.systematic.interventions(simset, 
                                 interventions = ALL.INTERVENTIONS,
                                 dst.dir=file.path('mcmc_runs/visualization_simsets', attr(simset@simulations[[1]], 'location')),
                                 overwrite = T)
    
    run.systematic.interventions(simset, 
                                 interventions = ALL.INTERVENTIONS.3Y,
                                 dst.dir=file.path('mcmc_runs/visualization_simsets', attr(simset@simulations[[1]], 'location')),
                                 overwrite = T)
    
}
