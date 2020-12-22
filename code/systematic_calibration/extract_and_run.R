source('code/source_code.R')
source('code/targets/target_msas.R')

done.mcmc.locations <- function(dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_parallel'))
{
    files = list.files(dir)
    gsub("^([^_]+)_.*$", "\\1", files)
}

do.run.interventions <- function(location,
                                 simset.dir=file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets'),
                                 dst.dir=file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets'))
{
    full.filename = get.full.filename(location=location)
    load(file=file.path(simset.dir, full.filename))
    
    print("Running Systematic Interventions...")
    run.systematic.interventions(simset,
                                 dst.dir=dst.dir,
                                 interventions=ALL.INTERVENTIONS,
                                 overwrite=F,
                                 compress=T,
                                 run.to.year=2030,
                                 verbose=T,
                                 save.baseline.and.seed=F)
}

assemble.and.thin.mcmcs <- function(cache.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_caches'),
                                    dst.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_parallel'),
                                    simset.dir=file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets'),
                                    targets = TARGET.MSAS,
                                    create.simset=T,
                                    redo.done = F,
                                    additional.burn=500,
                                    additional.thin=2)
{
    if (!redo.done)
        targets = setdiff(targets, done.mcmc.locations(dst.dir))
    
    caches = list.dirs(cache.dir, recursive = F, full.names = F)
    locations.for.cache = gsub("^([^_]+)_.*$", "\\1", caches)
    
    target.mask = sapply(locations.for.cache, function(loc){
        any(loc==targets)
    })
    
    caches = caches[target.mask]
    locations.for.cache = locations.for.cache[target.mask]
 
    cat(length(caches), " cache directories representing target locations found\n", sep='')
    
    location.counts = table(locations.for.cache)
    repeated.locations = names(location.counts)[location.counts>1]
    if (length(repeated.locations)>1)
        cat("NOTE: The following locations have more than one cache and will NOT be extracted: ",
                     paste0("'", repeated.locations, "'", collapse=', '),
            "\n", sep='')
    
    not.repeated.mask = location.counts[locations.for.cache] == 1
    caches = caches[not.repeated.mask]
    locations.for.cache = locations.for.cache[not.repeated.mask]
    
    if (length(repeated.locations)>1)
        cat(length(Caches), " cache directories remain to be extracted\n", sep='')        
    
    cat("Checking which of the ", length(caches), " caches are complete...\n", sep='')
    
    cache.complete = sapply(1:length(caches), function(i){
        file = file.path(cache.dir, caches[i])
        cat(" - Checking ", locations.for.cache[i], sep='')
        complete = is.mcmc.cache.complete(file)
        if (complete)
            cat(" - complete\n")
        else
            cat(" - NOT complete\n")
        complete
    })
    
    caches = caches[cache.complete]
    locations.for.cache = locations.for.cache[cache.complete]
    
    cat("Processing ", length(caches), " completed caches...\n", sep='')
    for (i in 1:length(caches))
    {
        cache = caches[i]
        cat(" - Extracting MCMC for ", locations.for.cache[i], sep='')
        
        mcmc = assemble.mcmc.from.cache(file.path(cache.dir, cache))
        mcmc = subset.mcmc(mcmc, additional.burn=additional.burn, additional.thin=additional.thin)
        
        save(mcmc, file=file.path(dst.dir, paste0(cache, '.Rdata')))

        if (create.simset)
        {
            simset = extract.simset(mcmc)
            full.filename = get.full.filename(location=locations.for.cache[i])
            save(simset, file=file.path(simset.dir, full.filename))
        }
                
        cat(" - DONE\n")
    }
    
    cat("All Done!\n")
}
