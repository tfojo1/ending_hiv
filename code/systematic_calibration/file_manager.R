

##-------------------------##
##-- MANAGING FILE NAMES --##
##-------------------------##

make.filenames.from.elements <- function(location,
                                        intervention.code,
                                        version=VERSION)
{
    paste0(version, "_", location, "_", intervention.code, ".Rdata")
}

get.locations.from.filenames <- function(filenames)
{
    pattern = "^[^_]+_([^_]+)_.*$"
    gsub(pattern, '\\1', filenames)
}

get.interventions.from.filenames <- function(filenames,
                                             remove.null=T,
                                             remove.unregistered=F)
{
    pattern = "^[^_]+_[^_]+_(.*).Rdata$"
    intervention.codes = gsub(pattern, '\\1', filenames)
    rv = lapply(intervention.codes, intervention.from.code)
    
    if (remove.null)
    {
        null.mask = sapply(rv, is.null)
        rv = rv[!null.mask]
    }
    else
    {
        unregistered.mask = sapply(rv, is.null) & intervention.codes != 'baseline'
        
        if (any(unregistered.mask))
        {
            if (remove.unregistered)
                rv = rv[!unregistered.mask]
            else
                stop(paste0("The following are not registered intervention codes: ",
                            paste0("'", intervention.codes[unregistered.mask], "'", collapse=', ')))
        }
    }
    
    rv
}

VERSION = '1.0'
get.simset.filename <- function(simset,
                                location=attr(simset@simulations[[1]], 'location'),
                                intervention=attr(simset, 'intervention'),
                                intervention.code=NULL,
                                version=VERSION)
{
    if (is.null(intervention.code))
    {
        if (is.null(intervention))
            intervention.code = 'baseline'
        else
            intervention.code = get.intervention.code(intervention)
    }
    
    if (is.null(intervention.code))
        stop("The given intervention has not been registered with the intervention manager")
    
    make.filenames.from.elements(version=version,
                                 location=location,
                                 intervention.code=intervention.code)
}

##--------------------##
##-- SAVING SIMSETS --##
##--------------------##

save.simset <- function(simset, 
                        dir='mcmc_runs/visualization_simsets',
                        compress=T)
{
    if (compress)
        simset = compress.simset(simset)
    
    filename = get.simset.filename(simset)
    save(simset, file=file.path(dir, filename))
}

make.and.save.compressed.baseline.and.seed <- function(simset,
                                                       dir='mcmc_runs/visualization_simsets',
                                                       seed.keep.to.year=2020,
                                                       version=VERSION)
{
    save.simset(simset, dir=dir, compress=T)
    
    simset = prepare.simset.for.interventions(simset)
    run.from.year = attr(simset, 'run.from.year')
    simset = subset.simset.by.year(simset, (run.from.year-1):seed.keep.to.year)
    
    filename = get.seed.filename(location=attr(simset@simulations[[1]], 'location'),
                                 version=version)
    save(simset, file=file.path(dir, filename))
}

get.seed.filename <- function(location,
                              version=VERSION)
{
    make.filenames.from.elements(version=version,
                                 location=location,
                                 intervention.code = 'seed')
}

get.baseline.filename <- function(location,
                                  version=VERSION)
{
    get.simset.filename(simset=NULL,
                        location=location,
                        intervention.code=NULL,
                        version=version)
}