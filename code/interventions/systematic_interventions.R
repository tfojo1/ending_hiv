
MAX.FIRST.KEEP.YEAR = 2018 #so that census totals can map

##------------------------------##
##-- DEFINE INTERVENTION SETS --##
##------------------------------##

A1.INTERVENTION.CODES = c('het.tq1.mi.tq6m',
                          'het.s90.mi.s90.x',
                          'het.p25.mi.p50.x',
                          'ybhm.6m.25.80',
                          'ybhm.6m.25.90',
                          'ybhm.6m.50.80',
                          'ybhm.6m.50.90',
                          'mi.6m.25.80.ybh.high6',
                          'mi.6m.25.90.ybh.high6',
                          'mi.6m.50.80.ybh.high6',
                          'mi.6m.50.90.ybh.high6',
                          'het.1.10.80.mi.high6',
                          'het.1.10.90.mi.high6',
                          'het.1.25.80.mi.high6',
                          'het.1.25.90.mi.high6')

A2.INTERVENTION.CODES = c('ybhm.tq1',
                          'ybhm.tq6m',
                          'ybhm.s80',
                          'ybhm.s90',
                          'ybhm.p25',
                          'ybhm.p50',
                          'mi.tq1.ybh.tq6m',
                          'mi.tq6m.ybh.tq6m',
                          'mi.s80.ybhm.s90.x',
                          'mi.s90.ybhm.s90.x',
                          'mi.p25.ybh.p50.x',
                          'mi.p50.ybh.p50.x',
                          'het.tq2.mi.tq6m',
                          'het.tq1.mi.tq6m',
                          'het.s80.mi.s90.x',
                          'het.s90.mi.s90.x',
                          'het.p10.mi.p50.x',
                          'het.p25.mi.p50.x')

A1.INTERVENTION.SET.1Y = c(list(NO.INTERVENTION),
                        lapply(A1.INTERVENTION.CODES, intervention.from.code))

A2.INTERVENTION.SET.1Y = c(list(NO.INTERVENTION),
                        lapply(A2.INTERVENTION.CODES, intervention.from.code))

ALL.INTERVENTIONS.1Y = union.intervention.lists(A1.INTERVENTION.SET.1Y,
                                                A2.INTERVENTION.SET.1Y)



A1.INTERVENTION.SET.3Y = c(list(NO.INTERVENTION),
                           lapply(paste0(A1.INTERVENTION.CODES, '.3y'), intervention.from.code))

A2.INTERVENTION.SET.3Y = c(list(NO.INTERVENTION),
                           lapply(paste0(A2.INTERVENTION.CODES, '.3y'), intervention.from.code))

ALL.INTERVENTIONS.3Y = union.intervention.lists(A1.INTERVENTION.SET.3Y,
                                                A2.INTERVENTION.SET.3Y)


A1.INTERVENTION.SET = A1.INTERVENTION.SET.3Y
A2.INTERVENTION.SET = A2.INTERVENTION.SET.3Y
ALL.INTERVENTIONS = ALL.INTERVENTIONS.3Y

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
    base.simset = prepare.simset.for.interventions(simset)
    
    location = attr(base.simset@simulations[[1]], 'location')
    run.from.year=attr(base.simset, 'run.from.year')
    keep.years=min(run.from.year, MAX.FIRST.KEEP.YEAR):run.to.year
    
    if (save.baseline.and.seed)
    {
        if (verbose)
            print("Compressing baseline simset...")
        
        save.simset(simset, dir=dst.dir, compress=compress)

        if (verbose)
            print("Preparing baseline simset...")
        save.seed.simset(simset, dir=dst.dir)
        
        return()
    }
    
    browser()
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