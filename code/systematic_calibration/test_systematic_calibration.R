
if (1==2)
{
    setwd('../../../Ending HIV/jheem/')
}


source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')

BALTIMORE.CITY = 24510
BALTIMORE.MSA = msa.for.county(BALTIMORE.CITY)


set.seed(1234)
mcmc = run.mcmc.for.msa(msa=BALTIMORE.MSA,
                        save.suffix='v65',
                        likelihood=create.msa.likelihood(BALTIMORE.MSA))

if (1==2)
{

    mcmc = run.mcmc.for.msa(msa=BALTIMORE.MSA,
                            save.suffix='v63.2',
                            likelihood=create.msa.likelihood(BALTIMORE.MSA),
                            resume.cache='../results/mcmc_caches/12580_v63.2_2020-04-22/')
}
