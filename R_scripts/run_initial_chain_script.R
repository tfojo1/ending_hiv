
#-- CHECK ARGUMENTS --#
args = commandArgs(trailingOnly=TRUE)
if (length(args)<1) {
    stop("One argument must be supplied", call.=FALSE)
}

#-- SET THE WD --#
setwd('Ending_HIV')

#-- SOURCE THE RELEVANT FILES --#
print("Loading Source Files...")
source('code/source_code.R')
source('code/targets/target_msas.R')

#-- GET THE MSA --#
index = as.integer(args[1])
if (is.na(index) || index<1 || index>length(TARGET.MSAS))
    stop(paste0("The first argument to the script must be an integer between 1 and ", length(TARGET.MSAS)))
msa = TARGET.MSAS[index]

#-- DO THE SET UP--#
print(paste0("Running initial MCMC for ", msa.names(msa)))

set.seed(1234)

mcmc = setup.initial.mcmc.for.msa(msa, run=T, save.suffix = '', 
                                  run = T, plot.first.sim = F,
                                  target.acceptance.rate = 0.100, derive.step.size.from.prior.mcmc = F,
                                  likelihood = create.msa.likelihood(msa=msa, EVERYTHING.WEIGHT=1/8))

save.name = paste0(msa, '.Rdata')
save(mcmc, file=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_initial', save.name))

