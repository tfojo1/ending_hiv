

done.mcmc.locations <- function(dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_parallel'))
{
    files = list.files(dir)
    gsub("^([^_]+)_.*$", "\\1", files)
}

check.intervention.status <- function(dir=file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets'),
                                      interventions=ALL.INTERVENTIONS,
                                      locations=TARGET.MSAS)
{
    done = do.check.interventions.done(dir=dir,
                                       locations=locations,
                                       interventions=interventions)
    
    cat(round(100*mean(done)), "% (",
          sum(done), " of ", length(done), " interventions x locations) of all interventions done\n",
        sep = '')
    
    cat("\n---\n")
    
    done.by.intervention = colSums(done)
    int.codes = sapply(interventions, get.intervention.code)
    cat(paste0("- ", int.codes, ": ", done.by.intervention, " of ", length(locations), " locations done", collapse='\n'))
    
    
    cat("\n---\n")
    done.by.location = rowSums(done)
    loc.names = unlist(msa.names(locations))
    cat(paste0("- ", loc.names, ": ", done.by.location, " of ", length(interventions), " interventions done", collapse='\n'))
    
}

do.check.interventions.done <- function(dir,
                                        locations,
                                        interventions)
{
    sapply(interventions, function(int){
        sapply(locations, function(loc){
            filename = get.simset.filename(location=loc, intervention=int)
            file.exists(file.path(dir, loc, filename))
        })
    })
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
                     ifelse(length(locations)==1, "vlocation", " locations")))
    
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
                sim = run.simulation(pp)  
                
                counter <<- counter + 1
                if (verbose && counter %% update.frequency == 0)
                    print(paste0("Finished ", counter, " of ", n.sim, " simulations"))
                
                sim
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

copy.and.thin.simsets <- function(locations,
                                  src.dir = file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets'),
                                  dst.dir = file.path(SYSTEMATIC.ROOT.DIR, 'visualization_simsets'),
                                  interventions=ALL.INTERVENTIONS,
                                  thin.to=80,
                                  compress=T,
                                  verbose=T,
                                  redo.seed=T,
                                  redo.baseline=T,
                                  compress.to.years=2009:2030)
{
    for (location in locations)
    {
        print(paste0("Copying and thinning simsets for ", location))
        
        if (!file.exists(dst.dir))
            dir.create(dst.dir)
        if (!file.exists(file.path(dst.dir, location)))
            dir.create(file.path(dst.dir, location))
        
        need.to.do.seed = redo.seed || !file.exists(file.path(src.dir, location, get.seed.filename(location)))
        need.to.do.baseline = redo.baseline || !file.exists(file.path(src.dir, location, get.baseline.filename(location)))
        if (need.to.do.seed || need.to.do.baseline)
        {
            if (verbose)
            {
                if (need.to.do.seed && need.to.do.baseline)
                    print(" - Preparing seed and baseline simsets")
                else if (need.to.do.seed)
                    print(" - Preparing seed simset")
                else
                    print(" - Preparing baseline simset")
            }
            
            full.file = file.path(src.dir, get.full.filename(location))
            if (!file.exists(full.file))
                stop(paste0("Full file ('", full.file, "') does not exist"))
            load(full.file)
            simset = do.thin.simset.to(simset, thin.to)
            
            if (need.to.do.seed)
                save.seed.simset(simset, dir=file.path(dst.dir, location))
            
            if (need.to.do.baseline)
            {
                if (compress)
                    simset = compress.simset(simset, 
                                             keep.years=intersect(simset@simulations[[1]]$years, compress.to.years))
                save.simset(simset, dir=file.path(dst.dir, location), compress=F)
            }
        }
        
        for (i in (1:length(interventions)))
        {
            file = get.simset.filename(location=location, intervention=interventions[[i]])
            if (verbose)
                print(paste0(" - Loading and thinning simset ", i, " of ", length(interventions)))
            
            load(file.path(src.dir, location, file))
            simset = do.thin.simset.to(simset, thin.to)
            if (compress)
                simset = compress.simset(simset, keep.years=intersect(simset@simulations[[1]]$years, compress.to.years))
            
            #this is a fix for in case we have changed the intervention representation (lumping target populations, eg)
            attr(simset, 'intervention') = interventions[[i]]
            
            # save it
            save.simset(simset, dir=file.path(dst.dir, location), compress=F)
        }
        
        print(paste0("Done with ", location))
    }
}

do.thin.simset.to <- function(simset, thin.to)
{
    if (simset@n.sim < thin.to)
        stop(paste0("The simset only has ", simset@n.sim, " simulations. Cannot thin to ", thin.to, " simulations"))
    
    indices = (1:thin.to)*floor(simset@n.sim/thin.to)
    indices = indices + (simset@n.sim - max(indices))
    
    simset = subset.simset(simset, indices=indices)

    #return
    simset
}
