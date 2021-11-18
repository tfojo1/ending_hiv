source('code/systematic_calibration/systematic_settings.R')
source('code/interventions/interventions_for_simset.R')
source('code/targets/target_msas.R')

make.and.save.simset.for.location <- function(location,
                                              full=T,
                                              from.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_parallel'))
{
    # Set up file
    if (full)
        dst.dir = file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets')
    else
        dst.dir = file.path(SYSTEMATIC.ROOT.DIR, 'limited_simsets')
    
    print("Loading mcmc")
    mcmc = load.mcmc.from.dir(location, from.dir)
    
    # Pull the simset
    if (mcmc@n.iter==720)
        additional.burn=220
    else
        additional.burn=250
    
    print("Cutting simset")
    if (full)
        simset = extract.simset(mcmc, additional.burn=additional.burn, additional.thin=2)
    else
        simset = extract.simset(mcmc, additional.burn=additional.burn, additional.thin=10)
    #        simset = extract.simset(mcmc, additional.burn=additional.burn+20, additional.thin=16)
    
    # Set up for interventions
    
    print("Preparing simset to run interventions")
    simset = prepare.simset.for.interventions(simset)
    
    # Save and Return
    print("Saving")
    save(simset, file=file.path(dst.dir, paste0(location, '.Rdata')))
    invisible(simset)
}