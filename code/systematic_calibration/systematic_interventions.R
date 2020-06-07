
source('code/systematic_calibration/systematic_settings.R')
source('code/interventions/interventions_setup.R')
source('code/interventions/interventions_for_simset.R')

source('../../commoncode/time_text.R')

INTERVENTION.RAMPED.UP.YEAR = 2022
SYSTEMATIC.TARGET.POPULATIONS = c('Young Black and Hispanic MSM',
                                  'All MSM or IDU',
                                  'Everyone')
SYSTEMATIC.TESTING.FREQUENCIES = 1
SYSTEMATIC.SUPPRESSED.PROPORTIONS = c(.8,.9)
SYSTEMATIC.PREP.COVERAGES = c(.25, .5)

INTERVENTION.SET = c(list(NULL), 
                     create.all.intervention.combinations(target.populations=SYSTEMATIC.TARGET.POPULATIONS,
                                                        testing.frequencies=SYSTEMATIC.TESTING.FREQUENCIES,
                                                        suppressed.proportions=SYSTEMATIC.SUPPRESSED.PROPORTIONS,
                                                        prep.coverages=SYSTEMATIC.PREP.COVERAGES,
                                                        intervention.ramped.up.year=INTERVENTION.RAMPED.UP.YEAR))
names(INTERVENTION.SET) = sapply(INTERVENTION.SET, get.intervention.filename)

run.systematic.interventions <- function(location,
                                         interventions=INTERVENTION.SET,
                                         full.simset=T,
                                         keep.years.if.null.intervention=2000:2030,
                                         keep.years.for.interventions=2020:2030,
                                         verbose=T)
{
    if (full.simset)
    {
        dst.dir = file.path(SYSTEMATIC.ROOT.DIR, 'full_simset_interventions', location)
        load(file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets', paste0(location, '.Rdata')))
    }
    else
    {
        dst.dir = file.path(SYSTEMATIC.ROOT.DIR, 'limited_simset_interventions', location)
        load(file.path(SYSTEMATIC.ROOT.DIR, 'limited_simsets', paste0(location, '.Rdata')))
    }
    
    if (!dir.exists(dst.dir))
        dir.create(dst.dir)
    base.simset = simset
    
    start.time = Sys.time()
    n.total.sim=0
    for (int.name in names(interventions))
    {
        int = interventions[[int.name]]
        if (is.null(int))
            keep.years = keep.years.if.null.intervention
        else
            keep.years = keep.years.for.interventions
        
        
        print(paste0("Running intervention: '", int.name, "' on ", base.simset@n.sim, " simulations..."))
        simset = run.simset.intervention(base.simset,
                                         intervention=int,
                                         run.to.year=max(keep.years),
                                         keep.years=keep.years)
        
        n.total.sim = n.total.sim + base.simset@n.sim
        run.time = as.numeric(difftime(Sys.time(), start.time, units = 'secs'))
        
        if (verbose)
            print(paste0("Total runtime = ", get.timespan.text(run.time),
                         " (", get.timespan.text(run.time/n.total.sim), " per simulation on average)"))
        
        save(simset, file=file.path(dst.dir, paste0(int.name, '.Rdata')))
    }
}