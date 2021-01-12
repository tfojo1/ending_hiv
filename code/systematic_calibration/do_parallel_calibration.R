
if (1==2)
{
    setwd('../../../Ending HIV/Ending_HIV/')
}


source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')
source('code/targets/target_msas.R')

msa = BALTIMORE.MSA
chain = 1

# Get the cache and run it
cache.dirs = list.dirs(file.path(SYSTEMATIC.ROOT.DIR, 'systematic_caches'), recursive = F)
mask = grepl(msa, cache.dirs)

if (!any(mask))
    stop(paste0("No cache dir has been set up for '", msa, "' (", msa.names(msa), ")"))
if (sum(mask)>1)
    stop(paste0("There is more than one cache set up for '", msa, "' (", msa.names(msa), ")"))

cache.dir = cache.dirs[mask]
print(qplot(1,1) + ggtitle(paste0(msa.names(msa), " - ", chain)) + theme(plot.title=element_text(hjust=1)))

set.seed(1234)
mcmc = run.mcmc.for.msa.cache(cache.dir, chains=chain)

                              
                              
