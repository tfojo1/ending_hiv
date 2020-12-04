
if (1==2)
{
    setwd('../Ending_HIV/')
}

source('code/plots.R')
source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')
source('code/targets/target_msas.R')

set.seed(5557)
 
msa=CHICAGO.MSA
save.suffix = ''

RESUME=T
if (RESUME)
{
    print('----------------------------------------------------')
    print(paste0("RESUMING initial MCMC on ", msa.names(msa)))
    print(save.suffix)
    print('----------------------------------------------------')
    mcmc = run.mcmc.for.msa.cache('mcmc_runs/systematic_caches/16980_1x20K_2020-12-03/')
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
if (1==2)
{
    source('code/source_code.R')
    source('code/interventions/systematic_interventions.R')
    load('mcmc_runs/systematic_initial/17460_1x20K_2020-10-08.Rdata')
    
    simset = extract.simset(mcmc, additional.burn=520, additional.thin=6)
    simset@n.sim
    msa.names(attr(simset@simulations[[1]], 'location'))
    run.systematic.interventions(simset, 
                                 interventions = ALL.INTERVENTIONS,
                                 dst.dir=file.path('mcmc_runs/visualization_simsets', attr(simset@simulations[[1]], 'location')),
                                 overwrite = T)
    
    #to rerun the test code
    source('code/source_code.R')
    source('code/systematic_calibration/systematic_calibration.R')
    source('code/targets/target_msas.R')
    load('mcmc_runs/systematic_initial/35620_1x20K_2020-10-01.Rdata')
}