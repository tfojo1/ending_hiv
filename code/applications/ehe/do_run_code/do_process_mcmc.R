
source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

msa = JACKSON.MSA
chains = 1:4
version = 'collapsed_1.0'
INCOMPLETE = T


# Get the cache and run it
dir.suffix = get.directory.suffix.for.version(version)
cache.dirs = list.dirs(file.path(SYSTEMATIC.ROOT.DIR, 
                                 paste0('systematic_caches', dir.suffix)),
                       recursive = F)
mask = grepl(msa, cache.dirs) & grepl('4x', cache.dirs)

if (!any(mask))
    stop(paste0("No cache dir has been set up for '", msa, "' (", msa.names(msa), ")"))
if (sum(mask)>1)
    stop(paste0("There is more than one cache set up for '", msa, "' (", msa.names(msa), ")"))

cache.dir = cache.dirs[mask]
cache.suffix = gsub(".*/([^/]+)$", '\\1', cache.dir)
cache.suffix = gsub("[^_]+(.*)$", '\\1', cache.suffix)

mcmc = assemble.mcmc.from.cache(cache.dir, allow.incomplete = INCOMPLETE, chains = chains)
print(paste0("Done assembling mcmc: ", format(mcmc@n.iter * mcmc@n.chains, big.mark=','), " total samples - saving..."))
save(mcmc, file=file.path(SYSTEMATIC.ROOT.DIR,
                          paste0('systematic_parallel', dir.suffix),
                          paste0(msa, cache.suffix, '.Rdata')))

if (!INCOMPLETE)
{
    simset = extract.simset(mcmc, additional.burn=500, additional.thin=2)
    
    save.simset(simset, dir=file.path(SYSTEMATIC.ROOT.DIR, 
                                      paste0('full_simsets', dir.suffix)), 
                full=T, save.full.in.master.directory = T)
}

if (1==2)
{
    mcmc@n.iter
    simset = extract.simset(mcmc, additional.burn=350, additional.thin=1)
    simset@n.sim
    
    simplot(simset)
    
    save.simset(simset, dir=file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets'), 
                full=T, save.full.in.master.directory = T)
}