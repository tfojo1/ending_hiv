
if (1==2)
{
    setwd('../../../Ending HIV/Ending_HIV/')
}


source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')
source('code/targets/target_msas.R')

set.seed(1234)
mcmc = run.mcmc.for.msa.cache('mcmc_runs/systematic_caches/29820_4x100K_total.2_2020-10-29/',
                              chains=4)

