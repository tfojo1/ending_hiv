source('code/source_code.R')

msa = PORTLAND.MSA

set.seed(1234)
likelihood = OLD.create.msa.likelihood.v1.for.annals(msa)
setup.parallel.mcmc.for.msa(msa=msa, likelihood=likelihood)
