


A1.INTERVENTION.SET = list(NO.INTERVENTION,
                             YBHM.1.25.80,
                             YBHM.1.25.90,
                             YBHM.1.50.80,
                             YBHM.1.50.90,
                             MSM.IDU.1.25.80.YBH.HIGH.X,
                             MSM.IDU.1.25.90.YBH.HIGH.X,
                             MSM.IDU.1.50.80.YBH.HIGH.X,
                             MSM.IDU.1.50.90.YBH.HIGH.X,
                             HET.1.10.80.MI.HIGH.X,
                             HET.1.10.90.MI.HIGH.X,
                             HET.1.25.80.MI.HIGH.X,
                             HET.1.25.90.MI.HIGH.X)

A2.INTERVENTION.SET = list(YBHM.TQ2,
                           YBHM.TQ1,
                           YBHM.S80,
                           YBHM.S90,
                           YBHM.P25,
                           YBHM.P50,
                           MSM.IDU.TQ2.YBH.TQ1.X,
                           MSM.IDU.TQ1.YBH.TQ1.X,
                           MSM.IDU.S80.YBH.S90.X,
                           MSM.IDU.S90.YBH.S90.X,
                           MSM.IDU.P25.YBH.P50.X,
                           MSM.IDU.P50.YBH.P50.X,
                           HET.TQ2.MI.TQ1.X,
                           HET.TQ1.MI.TQ1.X,
                           HET.S80.MI.S90.X,
                           HET.S90.MI.S90.X,
                           HET.P10.MI.P50.X,
                           HET.P25.MI.P50.X)

ALL.INTERVENTIONS = c(A1.INTERVENTION.SET,
                      A2.INTERVENTION.SET)

ORIG.INTERVENTION.SET = list(NO.INTERVENTION,
                     YBHM.1.25.80,
                     YBHM.1.25.90,
                     YBHM.1.50.80,
                     YBHM.1.50.90,
                     MSM.IDU.1.25.80.YBH.HIGH,
                     MSM.IDU.1.25.90.YBH.HIGH,
                     MSM.IDU.1.50.80.YBH.HIGH,
                     MSM.IDU.1.50.90.YBH.HIGH,
                     HET.1.10.80.MI.HIGH,
                     HET.1.10.90.MI.HIGH,
                     HET.1.25.80.MI.HIGH,
                     HET.1.25.90.MI.HIGH)

run.systematic.interventions <- function(simset,
                                         dst.dir,
                                         interventions=ALL.INTERVENTIONS,
                                         overwrite=F,
                                         compress=T,
                                         run.to.year=2030,
                                         verbose=T,
                                         save.baseline.and.seed=T)
{
    if (!dir.exists(dst.dir))
        dir.create(dst.dir)
    
    if (verbose)
        print("Preparing baseline simset...")
    base.simset = prepare.simset.for.interventions(simset)
    location = attr(base.simset@simulations[[1]], 'location')
    run.from.year=attr(base.simset, 'run.from.year')
    keep.years=run.from.year:run.to.year
    
    if (save.baseline.and.seed && compress)
    {
        if (verbose)
            print("Compressing baseline simset...")
        make.and.save.compressed.baseline.and.seed(simset, dir=dst.dir)
    }
    
    start.time = Sys.time()
    n.total.sim=0
    for (int in interventions)
    {
        filename = get.simset.filename(location=location,
                                       intervention=int)
        int.name = get.intervention.name(int)
        
        if (!overwrite && file.exists(file.path(dst.dir, filename)))
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
        keep.years=run.from.year:run.to.year
        
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