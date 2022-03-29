
source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

msa = DENVER.MSA
chains = 1:4

# Get the cache and run it
cache.dirs = list.dirs(file.path(SYSTEMATIC.ROOT.DIR, 'systematic_caches'), recursive = F)
mask = grepl(msa, cache.dirs) & grepl('4x', cache.dirs)

if (!any(mask))
    stop(paste0("No cache dir has been set up for '", msa, "' (", msa.names(msa), ")"))
if (sum(mask)>1)
    stop(paste0("There is more than one cache set up for '", msa, "' (", msa.names(msa), ")"))

cache.dir = cache.dirs[mask]

mcmc = assemble.mcmc.from.cache(cache.dir, T, chains = chains)
print(paste0("Done assembling mcmc: ", format(mcmc@n.iter * mcmc@n.chains, big.mark=','), " total samples"))

if (1==2)
{
    mcmc@n.iter
    simset = extract.simset(mcmc, additional.burn=230, additional.thin=2)
    simset@n.sim
    
    simplot(simset)
    
    save.simset(simset, dir=file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets'), 
                full=T, save.full.in.master.directory = T)
}