
if (1==2)
{
    setwd('../../../Ending HIV/Ending_HIV/')
}


source('code/source_code.R')
source('code/targets/target_msas.R')

set.seed(1234)
msa = VEGAS.MSA
setup.parallel.mcmc.for.msa(msa, 
                            likelihood = create.msa.likelihood(msa, EVERYTHING.WEIGHT = 1/2),
                            save.suffix = 'total.2')

