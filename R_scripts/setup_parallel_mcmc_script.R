
#-- CHECK ARGUMENTS --#
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
    stop("One argument must be supplied", call.=FALSE)
}
index = as.integer(args[1])
print(paste0("Index is ", index))

#-- SET THE WD --#
setwd('Ending_HIV')

#-- SOURCE THE RELEVANT FILES --#
print("Loading Source Files...")
source('code/source_code.R')
source('code/targets/target_msas.R')

#-- GET THE MSA --#
if (is.na(index) || index<1 || index>length(TARGET.MSAS))
    stop(paste0("The argument to the script must be an integer between 1 and ", length(TARGET.MSAS)))
msa = TARGET.MSAS[index]

#-- DO THE SET UP--#
print(paste0("Setting up MCMC for ", msa.names(msa)))

set.seed(12345)
setup.parallel.mcmc.for.msa(msa)

