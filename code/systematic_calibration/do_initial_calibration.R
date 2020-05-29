
if (1==2)
{
    setwd('../../../Ending HIV/jheem/')
}


source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')
source('code/targets/target_msas.R')

set.seed(1234)
mcmc = run.initial.mcmc.for.msa(LA.MSA)
