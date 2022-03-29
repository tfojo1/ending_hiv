
source('code/source_code.R')
source("code/processing/visualization/sim_plots.R")

set.seed(5557)
 
msa=PORTLAND.MSA 
save.suffix = 'manual2'
DO.vIS.INTERVENTIONS = F

RESUME=F
if (RESUME)
{
    print('----------------------------------------------------')
    print(paste0("RESUMING initial MCMC on ", msa.names(msa)))
    print(save.suffix)
    print('----------------------------------------------------')
    print(qplot(1,1) + ggtitle(msa.names(msa)) + theme(plot.title=element_text(hjust=1)))
    mcmc = run.mcmc.for.msa.cache('Q:Ending_HIV/mcmc_runs/systematic_caches/19740_1x20K_manual_2022-02-20')
}
if (!RESUME)
{
    print('----------------------------------------------------')
    print(paste0("Setting up to run initial MCMC on ", msa.names(msa)))
    print(save.suffix)
    print('----------------------------------------------------')
    
print("Using OLD (v1 for Annals) likelihood.")
likelihood = OLD.create.msa.likelihood.v1.for.annals(msa)#, EVERYTHING.WEIGHT=1/8)
#likelihood = create.msa.likelihood(msa=msa, EVERYTHING.WEIGHT=1/8)
mcmc = setup.initial.mcmc.for.msa(msa, run=T, save.suffix = save.suffix, 
                                  likelihood=likelihood,
                                  target.acceptance.rate = 0.100, derive.step.size.from.prior.mcmc = F)
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
