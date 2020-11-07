
if (1==2)
{
    setwd('../../../Ending HIV/Ending_HIV/')
}


source('code/source_code.R')
source('code/targets/target_msas.R')

set.seed(1234)
msa = LA.MSA
setup.parallel.mcmc.for.msa(msa, 
                            likelihood = create.msa.likelihood(msa),
                            save.suffix = '')

