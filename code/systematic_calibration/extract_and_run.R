source('code/source_code.R')
source('code/targets/target_msas.R')

done.mcmc.locations <- function(dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_parallel'))
{
    files = list.files(dir)
    gsub("^([^_]+)_.*$", "\\1", files)
}

do.run.interventions <- function(location,
                                 simset.dir=file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets'),
                                 dst.dir=file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets'),
                                 interventions=ALL.INTERVENTIONS,
                                 overwrite=F)
{
    full.filename = get.full.filename(location=location)
    load(file=file.path(simset.dir, full.filename))
    
    print("Running Systematic Interventions...")
    run.systematic.interventions(simset,
                                 dst.dir=dst.dir,
                                 interventions=interventions,
                                 overwrite=overwrite,
                                 compress=T,
                                 run.to.year=2030,
                                 verbose=T,
                                 save.baseline.and.seed=F)
}

do.rerun.simset <- function(locations,
                            bk.dir=file.path(SYSTEMATIC.ROOT.DIR, 'backup_simsets'),
                            simset.dir=file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets'),
                            verbose=T,
                            update.frequency.frac=0.05)
{
    if (verbose)
        print(paste0("REDOING ", length(locations),
                     ifelse(length(locations)==1, "location", "locations")))
    
    for (location in locations)
    {
        filename = get.full.filename(location=msa, version=VERSION)
        
        if (file.exists(file.path(bk.dir, filename)))
        {
            if (verbose)
                print(paste0("- Skipping ", msa, " (", msa.names(msa), ") - already done."))
        }
        else if (file.exists(file.path(simset.dir, filename)))
        {
            if (verbose)
                print(paste0("- Redoing ", msa, " (", msa.names(msa), ")..."))
            
            load(file.path(SYSTEMATIC.ROOT.DIR, 'start_values', paste0(msa, '.Rdata')))
            load(file.path(simset.dir, filename))
            orig.simset = simset

            n.sim = simset@n.sim
            counter = 0
            update.frequency = floor(n.sim * update.frequency.frac)
            
            run.simulation = create.run.simulation.function(msa=msa, start.values = starting.parameters)
            simset = extend.simulations(simset, fn=function(sim, pp){
                run.simulation(pp)  
                
                counter <<- counter + 1
                if (verbose && counter %% update.frequency == 0)
                    print(paste0("Finished ", counter, " of ", n.sim, " simulations"))
            })
            save(simset, file=file.path(simset.dir, filename))
            
            simset = orig.simset
            save(simset, file=file.path(bk.dir, filename))
        }
        else
        {
            if (verbose)
                print(paste0("- Skipping ", msa, " (", msa.names(msa), ") - simset not present."))
        }
    }
    
    if (verbose)
        print("DONE")
}

assemble.and.thin.mcmcs <- function(targets = TARGET.MSAS,
                                    cache.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_caches'),
                                    dst.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_parallel'),
                                    simset.dir=file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets'),
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
