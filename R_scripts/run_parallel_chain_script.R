
#-- CHECK ARGUMENTS --#
args = commandArgs(trailingOnly=TRUE)
if (length(args)<2) {
    stop("Two arguments must be supplied", call.=FALSE)
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

#-- GET THE CHAIN --#
N.CHAINS = 4
chain = as.integer(args[2])
if (is.na(chain) || chain<1 || chain>N.CHAINS)
    stop(paste0("The second argument to the script must be an integer between 1 and ", N.CHAINS))

#-- DO THE SET UP--#
print(paste0("Running chain ", chain, " of the MCMC for ", msa.names(msa)))

set.seed(1234)
cache.dirs = list.files('mcmc_runs/systematic_caches/')
mask = grepl(msa, cache.dirs)

if (!any(mask))
    stop(paste0("No cache dir has been set up for '", msa, "' (", msa.names(msa), ")"))
if (sum(mask)>0)
    stop(paste0("There is more than one cache set up for '", msa, "' (", msa.names(msa), ")"))

cache.dir = cache.dirs[mask]
mcmc = run.mcmc.for.msa.cache(cache.dir, chains=chain)

