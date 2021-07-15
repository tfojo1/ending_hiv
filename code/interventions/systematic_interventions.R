
MAX.FIRST.KEEP.YEAR = 2018 #so that census totals can map




run.systematic.interventions <- function(simset,
                                         dst.dir,
                                         interventions=ALL.INTERVENTIONS,
                                         overwrite=F,
                                         compress=T,
                                         run.to.year=2030,
                                         verbose=T,
                                         save.baseline.and.seed=T,
                                         seed=123415)
{
    if (!dir.exists(dst.dir))
        dir.create(dst.dir)
    
    if (verbose)
        print("Preparing baseline simset...")
    
    if (!is.na(seed))
        set.seed(seed)
    base.simset = prepare.simset.for.interventions(simset, fix.components = F)
    
    location = attr(base.simset@simulations[[1]], 'location')
    run.from.year=attr(base.simset, 'run.from.year')
    keep.years=min(run.from.year, MAX.FIRST.KEEP.YEAR):run.to.year
    
    if (save.baseline.and.seed)
    {
        if (verbose)
            print("Compressing baseline simset...")
        
        save.simset(simset, dir=dst.dir, compress=compress)

        if (verbose)
            print("Preparing seed simset...")
        save.seed.simset(simset, dir=dst.dir)
    }
    
    start.time = Sys.time()
    n.total.sim=0
    
    for (int in interventions)
    {
        filename = get.simset.filename(location=location,
                                       intervention=int)
        int.name = get.intervention.name(int)
        if (!overwrite && file.exists(file.path(dst.dir, location, filename)))
        {
            if (verbose)
                print(paste0("Skipping intervention: '", int.name, "' - already done"))
        }
        else
        {
            if (verbose)
                print(paste0("Running intervention: '", int.name, "' on ", base.simset@n.sim, " simulations..."))
            
            simset = run.simset.intervention(base.simset,
                                             intervention=int,
                                             run.from.year = run.from.year,
                                             run.to.year = run.to.year,
                                             keep.years=keep.years)
            
            n.total.sim = n.total.sim + base.simset@n.sim
            run.time = as.numeric(difftime(Sys.time(), start.time, units = 'secs'))
            
            if (verbose)
                print(paste0("Total runtime = ", get.timespan.text(run.time),
                             " (", get.timespan.text(run.time/n.total.sim), " per simulation on average)"))
            
            save.simset(simset, dir=dst.dir, compress=compress)
        }
    }
    
    if (verbose)
        print('All Done')
}

run.systematic.interventions.from.seed <- function(locations,
                                                   dir,
                                                   interventions=ALL.INTERVENTIONS,
                                                   overwrite=T,
                                                   compress=T,
                                                   run.to.year=2030,
                                                   version='1.0',
                                                   recrunch.baseline = F, #a temp param to crunch intervention rates for baselines
                                                   verbose=T)
{
    if (!dir.exists(dir))
        stop(paste0("Directory '", dir, "' does not exist"))
    base.dir = dir
    
    start.time = Sys.time()
    n.total.sim=0
    
    for (location in locations)
    {
        dir = base.dir
        if (verbose)
        {
            print("------------------------------")
            print(paste0("DOING ", msa.names(location)))
            print("------------------------------")
        }
            
            
        seed.filename = get.seed.filename(location=location, version=version)
        if (!file.exists(file.path(dir, seed.filename)))
        {
            dir = file.path(dir, location)
            if (!file.exists(file.path(dir, seed.filename)))
                stop(paste0("There is no seed file ('", seed.filename, "') in the given subdirectory"))
        }
        
        if (recrunch.baseline)
        {
            baseline.filename = get.simset.filename(simset=NULL,
                                                    location=location,
                                                    intervention.code=NULL,
                                                    version=version)
            if (file.exists(file.path(dir, baseline.filename)))
            {
                if (verbose)
                    print("Recrunching baseline")
                
                load(file.path(dir, baseline.filename))
                simset@simulations = lapply(simset@simulations, function(sim){
                    attr(sim, 'components') = crunch.intervention.rates(attr(sim, 'components'))
                    sim
                })
                save(simset, file=file.path(dir, baseline.filename))
            }
        }
        
        load(file.path(dir, seed.filename))
        seed = simset
        run.from.year=attr(seed, 'run.from.year')
        keep.years=min(run.from.year, MAX.FIRST.KEEP.YEAR):run.to.year
        
        for (int in interventions)
        {
            filename = get.simset.filename(location=location,
                                           intervention=int)
            int.name = get.intervention.name(int)
            
            if (!overwrite && file.exists(file.path(dir, filename)))
            {
                if (verbose)
                    print(paste0("Skipping intervention: '", int.name, "' - already done"))
            }
            else
            {
                if (verbose)
                    print(paste0("Running intervention: '", int.name, "' on ", seed@n.sim, " simulations..."))
                simset = run.simset.intervention(seed,
                                                 intervention=int,
                                                 run.from.year = run.from.year,
                                                 run.to.year = run.to.year,
                                                 keep.years=keep.years)
                
                n.total.sim = n.total.sim + seed@n.sim
                run.time = as.numeric(difftime(Sys.time(), start.time, units = 'secs'))
                
                if (verbose)
                    print(paste0("Total runtime = ", get.timespan.text(run.time),
                                 " (", get.timespan.text(run.time/n.total.sim), " per simulation on average)"))
                
                save.simset(simset, dir=dir, compress=compress)
            }
        }
    }
    
    
    if (verbose)
        print('All Done')
}

remove.systematic.interventions <- function(dir,
                                            locations = TARGET.MSAS,
                                            interventions)
{
    for (location in locations)
    {
        for (int in interventions)
        {
            print(paste0("Removing intervention '",
                   get.intervention.name(int),
                   "' from ",
                   location))
            filename = get.simset.filename(location=location,
                                           intervention=int)
            file.remove(file.path(dir, location, filename))   
        }
    }
}