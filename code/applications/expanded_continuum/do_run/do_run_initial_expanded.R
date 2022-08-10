
source('code/source_code.R')
source("code/processing/visualization/sim_plots.R")

set.seed(5557)

msa=ORLANDO.MSA
RESUME=T
SAVE.INITIAL.SIMSET = T

save.suffix = ''
version='expanded_1.0'

print(paste0("Starting script at ", Sys.time()))



if (RESUME)
{
    print('----------------------------------------------------')
    print(paste0("RESUMING initial MCMC on ", msa.names(msa)))
    print(save.suffix)
    print('----------------------------------------------------')
    print(qplot(1,1) + ggtitle(paste0(msa.names(msa), ' - INITIAL')) + theme(plot.title=element_text(hjust=1)))
    
    dir = file.path(MCMC.DIR,
                    paste0('systematic_caches', get.directory.suffix.for.version(version)))
    
    
    mcmc = resume.most.recent.mcmc.for.location(dir, location=msa)
}
if (!RESUME)
{
    print('----------------------------------------------------')
    print(paste0("Setting up to run initial MCMC on ", msa.names(msa)))
    print(save.suffix)
    print('----------------------------------------------------')
    
    print('creating likelihood...')
    likelihood = create.msa.likelihood(msa=msa, version = version)
    print(' - Done creating likelihood')
    
    print(qplot(1,1) + ggtitle(paste0(msa.names(msa), ' - INITIAL')) + theme(plot.title=element_text(hjust=1)))
    mcmc = setup.initial.mcmc.for.msa(msa, version = version,
                                      run=T, 
                                      save.suffix = save.suffix, 
                                      likelihood=likelihood,
                                      n.iter = 25000,
                                      target.acceptance.rate = 0.100, 
                                      
                                      derive.step.size.from.prior.mcmc = F)
}

save.name = paste0(msa, save.suffix, '.Rdata')

save.to.file = file.path(MCMC.DIR, 
                         paste0('systematic_initial', get.directory.suffix.for.version(version)), 
                         save.name)
print(paste0("ALL DONE - SAVING TO:"))
print(paste0("   '", save.to.file, "'"))
save(mcmc, file=save.to.file)

if (SAVE.INITIAL.SIMSET)
{
    simset = extract.simset(mcmc, additional.burn=750, additional.thin=5)
    save.simset(simset, dir='Q:JHEEM/simulations/baseline_quick_expanded', full = T, save.full.in.master.directory = T)
}