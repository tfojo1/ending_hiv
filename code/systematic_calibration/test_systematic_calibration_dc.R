
if (1==2)
{
    setwd('../../../Ending HIV/jheem/')
}


source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')

#source('../code/baltimore.R')

set.seed(1324)
DC.MSA = cbsa.for.msa.name('Washington,DC')

mcmc = run.mcmc.for.msa(msa=DC.MSA, save.suffix = 'v68')
#mcmc = run.mcmc.for.msa(msa=DC.MSA, chains=1, save.suffix='c1')

if (1==2)
{
    mcmc = run.mcmc.for.msa(msa=DC.MSA,
                            save.suffix='v66',
                            likelihood=create.msa.likelihood(DC.MSA),
                            resume.cache='../results/mcmc_caches/47900_v66_2020-04-27/')
}
