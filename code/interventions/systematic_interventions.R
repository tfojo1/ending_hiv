
source('code/systematic_calibration/file_manager.R')
source('code/interventions/interventions_for_simset.R')
source('code/visualization/compression.R')

source('../../commoncode/time_text.R')

INTERVENTION.SET = list(NO.INTERVENTION,
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
                                         interventions=INTERVENTION.SET,
                                         overwrite=F,
                                         compress=T,
                                         run.to.year=2030,
                                         verbose=T)
{
    if (!dir.exists(dst.dir))
        dir.create(dst.dir)
    
    if (verbose)
        print("Preparing baseline simset...")
    base.simset = prepare.simset.for.interventions(simset)
    location = attr(base.simset@simulations[[1]], 'location')
    run.from.year=attr(base.simset, 'run.from.year')
    keep.years=run.from.year:run.to.year
    
    if (compress)
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