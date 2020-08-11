
if (1==2)
{
    setwd('../../../Ending HIV/Ending_HIV/')
}


source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')
source('code/targets/target_msas.R')

set.seed(1234)
msa = NYC.MSA
mcmc = setup.parallel.mcmc.for.msa(msa)

