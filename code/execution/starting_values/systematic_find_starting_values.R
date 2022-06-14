
source('code/processing/visualization/sim_plots.R')
source('code/execution/systematic_calibration.R')

#'@description Generate a function to test starting values for a location and JHEEM version
#'
#'@param location The location code for which to generate the function
#'@param version The JHEEM version
#'
#'@return Returns a function. The return function is a wrapped call to do.test.starting.values, with the arguments run.simulation.function and base.parameters already wrapped in
#' It returns a list with three elements
#' $sim - the generated simulation
#' $parameters - the parameter vector that generated the simulation
#' $version - the version number passed
create.test.starting.values.function <- function(location,
                                                 version,
                                                 verbose=F,
                                                 from.beta.values=T)
{
    starting.parameters = get.initial.starting.parameters(msa=location,
                                                          version=version,
                                                          allow.beta.values=T,
                                                          require.beta.values=from.beta.values,
                                                          verbose=verbose)
    
    run.simulation.function = create.run.simulation.function(msa=location,
                                                             version = version,
                                                             start.values = starting.parameters)
    
    function(msm.trate.mult=NA,
             msm.trate.1.mult=NA,
             msm.trate.2.mult=NA,
             
             idu.trate.mult=NA,
             idu.trate.1.mult=NA,
             idu.trate.2.mult=NA,
             
             het.trate.mult=NA,
             het.trate.1.mult=NA,
             het.trate.2.mult=NA,
             
             lost.or.mult=NA,
             lost.or.slope.mult=NA,
             
             supp.or.mult=NA,
             supp.or.slope.mult=NA,
             
             reengagement.or.mult=NA,
             
             ...)
    {
        do.test.starting.values(run.simulation.function=run.simulation.function,
                             base.parameters=starting.parameters,
                             version=version,
                             
                             msm.trate.mult=msm.trate.mult,
                             msm.trate.1.mult=msm.trate.1.mult,
                             msm.trate.2.mult=msm.trate.2.mult,
                             
                             idu.trate.mult=idu.trate.mult,
                             idu.trate.1.mult=idu.trate.1.mult,
                             idu.trate.2.mult=idu.trate.2.mult,
                             
                             het.trate.mult=het.trate.mult,
                             het.trate.1.mult=het.trate.1.mult,
                             het.trate.2.mult=het.trate.2.mult,
                             
                             lost.or.mult=lost.or.mult,
                             lost.or.slope.mult=lost.or.slope.mult,
                             
                             supp.or.mult=supp.or.mult,
                             supp.or.slope.mult=supp.or.slope.mult,
                             
                             reengagement.or.mult=reengagement.or.mult,
                             
                             ...)
    }
}

#'@param ... the names of other parameters
#'
#'@return a list with three elements
#' $sim - the generated simulation
#' $parameters - the parameter vector that generated the simulation
#' $version - the version number passed
do.test.starting.values <- function(run.simulation.function,
                                    base.parameters,
                                    version,
                                    
                                    msm.trate.mult=NA,
                                    msm.trate.1.mult=NA,
                                    msm.trate.2.mult=NA,
                                    
                                    idu.trate.mult=NA,
                                    idu.trate.1.mult=NA,
                                    idu.trate.2.mult=NA,
                                    
                                    het.trate.mult=NA,
                                    het.trate.1.mult=NA,
                                    het.trate.2.mult=NA,
                                    
                                    lost.or.mult=NA,
                                    lost.or.slope.mult=NA,
                                    
                                    supp.or.mult=NA,
                                    supp.or.slope.mult=NA,
                                    
                                    reengagement.or.mult=NA,
                                    
                                    ...
                                    )
{
    orig.params = base.parameters
    other.param.values = list(...)
    
    # Set up masks for parameters
    msm.trates = grepl('msm.trate',names(base.parameters))
    msm.trates.1 = grepl('msm.trate.1',names(base.parameters))
    msm.trates.2 = grepl('msm.trate.2',names(base.parameters))
    idu.trates = grepl('idu.trate',names(base.parameters))
    idu.trates.1 = grepl('idu.trate.1',names(base.parameters))
    idu.trates.2 = grepl('idu.trate.2',names(base.parameters))
    het.trates = grepl('heterosexual.trate',names(base.parameters))
    het.trates.1 = grepl('heterosexual.trate.1',names(base.parameters))
    het.trates.2 = grepl('heterosexual.trate.2',names(base.parameters))
    msm.idu.multipliers = grepl('msm.vs',names(base.parameters))
    testing = grepl('proportion.tested', names(base.parameters))
    suppression = grepl('suppressed', names(base.parameters))
    lost = intersect(names(base.parameters),
                           c("heterosexual.proportion.lost.or",
                             "msm.proportion.lost.or",
                             "idu.proportion.lost.or",
                             "msm.idu.proportion.lost.or"))
    lost.slope = intersect(names(base.parameters),
                                 c("heterosexual.proportion.lost.slope.or",
                                   "msm.proportion.lost.slope.or",
                                   "idu.proportion.lost.slope.or",
                                   "msm.idu.proportion.lost.slope.or"))
    supp = intersect(names(base.parameters),
                     c(#expanded_1.0
                         "heterosexual.proportion.adherent.or",
                         "msm.proportion.adherent.or",
                         "idu.proportion.adherent.or",
                         "msm.idu.proportion.adherent.or",
                         #collapsed_1.0
                         "heterosexual.proportion.suppressed.or",
                         "msm.proportion.suppressed.or",
                         "idu.proportion.suppressed.or",
                         "msm.idu.proportion.suppressed.or"))
    supp.slope = intersect(names(base.parameters),
                           c(#expanded_1.0
                               "heterosexual.proportion.adherent.slope.or",
                               "msm.proportion.adherent.slope.or",
                               "idu.proportion.adherent.slope.or",
                               "msm.idu.proportion.adherent.slope.or",
                               #collapsed_1.0
                               "heterosexual.proportion.suppressed.slope.or",
                               "msm.proportion.suppressed.slope.or",
                               "idu.proportion.suppressed.slope.or",
                               "msm.idu.proportion.suppressed.slope.or"))
    reengagement = intersect(names(base.parameters), 'already.lost.vs.failing.proportion.lost.or')
 
    # MSM Transmission Rates
    if (!is.na(msm.trate.mult))
        base.parameters[msm.trates] = orig.params[msm.trates] * msm.trate.mult
    if (!is.na(msm.trate.1.mult))
        base.parameters[msm.trates.1] = orig.params[msm.trates.1] * msm.trate.1.mult
    if (!is.na(msm.trate.2.mult))
        base.parameters[msm.trates.2] = orig.params[msm.trates.2] * msm.trate.2.mult
    
    # IDU Transmission Rates
    if (!is.na(idu.trate.mult))
        base.parameters[idu.trates] = orig.params[idu.trates] * idu.trate.mult
    if (!is.na(idu.trate.1.mult))
        base.parameters[idu.trates.1] = orig.params[idu.trates.1] * idu.trate.1.mult
    if (!is.na(idu.trate.2.mult))
        base.parameters[idu.trates.2] = orig.params[idu.trates.2] * idu.trate.2.mult
    
    # Heterosexual Transmission Rates
    if (!is.na(het.trate.mult))
        base.parameters[het.trates] = orig.params[het.trates] * het.trate.mult
    if (!is.na(het.trate.1.mult))
        base.parameters[het.trates.1] = orig.params[het.trates.1] * het.trate.1.mult
    if (!is.na(het.trate.2.mult))
        base.parameters[het.trates.2] = orig.params[het.trates.2] * het.trate.2.mult
    
    # Lost
    if (!is.na(lost.or.mult))
        base.parameters[lost] = orig.params[lost] * lost.or.mult
    if (!is.na(lost.or.slope.mult))
        base.parameters[lost.slope] = orig.params[lost] * lost.or.slope.mult
    
    # Suppressed/Adherent
    if (!is.na(supp.or.mult))
        base.parameters[supp] = orig.params[supp] * supp.or.mult
    if (!is.na(supp.or.slope.mult))
        base.parameters[supp.slope] = orig.params[supp] * supp.or.slope.mult
    
    # Reengagement
    if (!is.na(reengagement.or.mult))
        base.parameters[reengagement] = orig.params[reengagement] / reengagement.or.mult
    
    # Other parameter values
    for (param.name in names(other.param.values))
    {
        if (is.null(param.name))
            stop("values passed to ... must be named")
        
        if (all(names(base.parameters)!=param.name))
            stop(paste0("Invalid parameter name passed to ... : There is no parameter named '",
                        param.name, "'"))
        
        if (!is.numeric(base.parameters[[param.name]]) || length(base.parameters[[param.name]])!=1)
            stop(paste0("Invalid value for '",
                        param.name, 
                        "' passed to ... - must be a numeric scalar value"))
        
        base.parameters[param.name] = orig.params[param.name] * other.param.values[[param.name]]
    }
    
    # run the sim
    sim = run.simulation.function(base.parameters)
    
    # plot
   # print(simplot(sim, facet.by='risk'))
    
    list(
        sim=sim,
        parameters=base.parameters,
        version=version
    )
    
    
}

#'@description Saves a given parameter vector as starting values. Can either take a single argument (the sim.and.params object resulting from a call to the function generated by create.test.starting.values.function)
#'
#'@param sim.and.params The value returned by a call to create.test.starting.values.function
#'@param parameters The parameters to save
#'@param location The location
#'@param version The JHEEM version
#'@param beta Whether this should be saved as a beta starting parameters set
save.starting.values <- function(sim.and.params,
                                 parameters=sim.and.params$parameters,
                                 location=attr(sim.and.params$sim, 'location'),
                                 version=sim.and.params$version,
                                 beta=F)
{
    starting.parameters = parameters
    if (beta)
        sub.dir = paste0(version, "_beta")
    else
        sub.dir = version
    
    if (!dir.exists(file.path(START.VALUES.DIR, sub.dir)))
        dir.create(file.path(START.VALUES.DIR, sub.dir), recursive = T)
    
    save(starting.parameters, file=file.path(START.VALUES.DIR, sub.dir, paste0(location, '.Rdata')))
    
    if (ON.DESKTOP)
    {   
        if (!dir.exists(file.path('start_values', sub.dir)))
            dir.create(file.path('start_values', sub.dir), recursive = T)
        
        save(starting.parameters, file=file.path('start_values', sub.dir, paste0(location, '.Rdata')))
    }
}

#'@description A function for Todd to use internally to sync up local starting values with the big data storage on the desktop
sync.starting.values <- function(versions=VERSION.MANAGER$versions,
                                 verbose=T)
{
    stop("now start values are in the home dir - no need to sync")
    if (!ON.DESKTOP)
        stop("We only sync starting values from the big desktop")
    
    for (version in versions)
    {
        if (verbose)
            print(paste0("Sync-ing starting values for version '", version, "'"))
        
        dir1 = file.path(SYSTEMATIC.ROOT.DIR, 'start_values', version)
        dir2 = file.path('start_values', version)
        files1 = list.files(dir1)
        files2 = list.files(dir2)
        files.in.both = intersect(files1, files2)
        
        new.files.from.1 = setdiff(list.files(dir1), list.files(dir2))
        if (length(new.files.from.1)>0)
        {
            file.copy(from=file.path(dir1, new.files.from.1),
                      to=file.path(dir2, new.files.from.1))
            if (verbose)
                print(paste0(" - Copied start values for ", length(new.files.from.1), " location(s) to LOCAL from DESKTOP"))
        }
        
        new.files.from.2 = setdiff(list.files(dir2), list.files(dir1))
        if (length(new.files.from.2)>0)
        {
            file.copy(from=file.path(dir2, new.files.from.2),
                      to=file.path(dir1, new.files.from.2))
            if (verbose)
                print(paste0(" - Copied start values for ", length(new.files.from.2), " location(s) to DESKTOP from LOCAL"))
        }
        
        
        time1 = file.info(file.path(dir1, files.in.both))$mtime
        time2 = file.info(file.path(dir2, files.in.both))$mtime
        overwrite.from.1.mask = time1 > time2
        
        overwrite.from.1.files = files.in.both[overwrite.from.1.mask]
        if (length(overwrite.from.1.files)>0)
        {
            file.copy(from=file.path(dir2, overwrite.from.1.files),
                      to=file.path(dir1, overwrite.from.1.files), overwrite = T)
            if (verbose)
                print(paste0(" - Overwrote start values for ", length(overwrite.from.1.files), " location(s) to DESKTOP from LOCAL"))
        }
        
        overwrite.from.2.files = files.in.both[!overwrite.from.1.mask]
        if (length(overwrite.from.2.files)>0)
        {
            file.copy(from=file.path(dir2, overwrite.from.2.files),
                      to=file.path(dir1, overwrite.from.2.files), overwrite = T)
            if (verbose)
                print(paste0(" - Overwrote start values for ", length(overwrite.from.2.files), " location(s) to LOCAL from DESKTOP"))
        }
    }
}

#'@description A function to pull out starting values from prior versions  and save them as beta starting values to be modified for the current version
prepare.beta.start.values <- function(version,
                                   locations,
                                   verbose=T)
{
    for (loc in locations)
    {
        if (verbose)
            cat("Preparing starting parameters for '", loc, "'...", sep='')
        starting.parameters = get.initial.starting.parameters(msa=loc,
                                                              version=version,
                                                              allow.beta.values=F,
                                                              verbose=F)
        
        save.starting.values(parameters = starting.parameters,
                             version = version,
                             location = loc,
                             beta=T)
        
        cat('Done\n')
    }
}