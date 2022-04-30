
source('code/source_code.R')
source("code/processing/visualization/sim_plots.R")

set.seed(5557)

msa=MIAMI.MSA
save.suffix = ''
version='expanded_1.0'

RESUME=F
if (RESUME)
{
    print('----------------------------------------------------')
    print(paste0("RESUMING initial MCMC on ", msa.names(msa)))
    print(save.suffix)
    print('----------------------------------------------------')
    print(qplot(1,1) + ggtitle(paste0(msa.names(msa), ' - INITIAL')) + theme(plot.title=element_text(hjust=1)))
    stop('set the dir')
    mcmc = run.mcmc.for.msa.cache('Q:Ending_HIV/mcmc_runs/systematic_caches/19740_1x20K_manual_2022-02-20')
}
if (!RESUME)
{
    print('----------------------------------------------------')
    print(paste0("Setting up to run initial MCMC on ", msa.names(msa)))
    print(save.suffix)
    print('----------------------------------------------------')
    
    likelihood = create.msa.likelihood(msa=msa, version = version)
    print(qplot(1,1) + ggtitle(paste0(msa.names(msa), ' - INITIAL')) + theme(plot.title=element_text(hjust=1)))
    mcmc = setup.initial.mcmc.for.msa(msa, version = version,
                                      run=T, 
                                      save.suffix = save.suffix, 
                                      likelihood=likelihood,
                                      target.acceptance.rate = 0.100, 
                                      derive.step.size.from.prior.mcmc = F)
}

save.name = paste0(msa, save.suffix, '.Rdata')

save.to.file = file.path(SYSTEMATIC.ROOT.DIR, 
                         paste0('systematic_initial', get.directory.suffix.for.version(version)), 
                         save.name)
print(paste0("ALL DONE - SAVING TO:"))
print(paste0("   '", save.to.file, "'"))
save(mcmc, file=save.to.file)
