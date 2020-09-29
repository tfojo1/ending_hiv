
if (1==2)
{
    setwd('../Ending_HIV/')
}


source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')
source('code/targets/target_msas.R')

set.seed(5556)
 
msa=ATLANTA.MSA
save.suffix = 't.25.25.dec.thresh_dx.1'

print('----------------------------------------------------')
print(paste0("Setting up to run initial MCMC on ", msa.names(msa)))
print(save.suffix)
print('----------------------------------------------------')

#mcmc = run.mcmc.for.msa.cache('mcmc_runs/systematic_caches/12060_1x20K_log2.sd.inv16.inv16_2020-09-24/')
mcmc = setup.initial.mcmc.for.msa(msa, run=T, save.suffix = save.suffix, 
                                  target.acceptance.rate = 0.100, derive.step.size.from.prior.mcmc = T)

