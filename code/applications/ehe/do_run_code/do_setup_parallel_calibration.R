source('code/source_code.R')

msa = JACKSON.MSA

set.seed(1234)
#likelihood = OLD.create.msa.likelihood.v1.for.annals(msa)
likelihood = create.msa.likelihood(msa, version='collapsed_1.0', include.new.msm.idu = F)
setup.parallel.mcmc.for.msa(msa=msa, likelihood=likelihood)

print(paste0("Done setting up for parallel MCMC on ", 
             msa, " ('", msa.names(msa), "')"))
