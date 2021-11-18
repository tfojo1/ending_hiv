

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

get.intervention.codes.from.filenames <- function(filenames)
{   
    pattern = "^[^_]+_[^_]+_(.*).Rdata$"
    intervention.codes = gsub(pattern, '\\1', filenames)
    
    intervention.codes
}

get.interventions.from.filenames <- function(filenames,
                                             remove.null=T,
                                             remove.unregistered=F)
{
    intervention.codes = get.intervention.codes.from.filenames
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
                        compress=!full,
                        pare.components=!full,
#                        compress.years=max(attr(simset, 'run.from.year'), min(simset@simulations[[1]]$years)):max(simset@simulations[[1]]$years),
                        full=F)
{
    if (compress)
        simset = compress.simset(simset)#, keep.years=compress.years)
    if (pare.components)
        simset = pare.simset.components(simset)
    
    location = attr(simset@simulations[[1]], 'location')
    
    if (full)
        filename = get.full.filename(location)
    else
        filename = get.simset.filename(simset)
    
    if (basename(dir) != location)
        dir = file.path(dir, location)
    if (!dir.exists(dir))
        dir.create(dir)
    
    save(simset, file=file.path(dir, filename))
}

save.seed.simset <- function(simset,
                             dir,
                             min.seed.keep.to.year=2020,
                             max.seed.keep.from.year=2018, #for census totals
                             version=VERSION)
{
    if (is.null(attr(simset, 'run.from.year')))
        simset = prepare.simset.for.interventions(simset)
    
    run.from.year = attr(simset, 'run.from.year')
    
    seed.keep.from.year = min(run.from.year-1, max.seed.keep.from.year-1)
    seed.keep.to.year = max(run.from.year, min.seed.keep.to.year)
    
    simset = subset.simset.by.year(simset, seed.keep.from.year:seed.keep.to.year)
#    attr(simset, 'components') = unfix.jheem.components(attr(simset, 'components'))
    
    location = attr(simset@simulations[[1]], 'location')
    filename = get.seed.filename(location=location,
                                 version=version)
    
    save(simset, file=file.path(dir, location, filename))
}

get.seed.filename <- function(location,
                              version=VERSION)
{
    make.filenames.from.elements(version=version,
                                 location=location,
                                 intervention.code = 'seed')
}

get.full.filename <- function(location,
                              version=VERSION)
{
    make.filenames.from.elements(version=version,
                                 location=location,
                                 intervention.code = 'full')
}

get.baseline.filename <- function(location,
                                  version=VERSION)
{
    get.simset.filename(simset=NULL,
                        location=location,
                        intervention.code=NULL,
                        version=version)
}

parse.simset.filenames <- function(filenames)
{
    splits = strsplit(filenames, "_")
    rv = sapply(splits, function(one.split){
        c(version=one.split[1],
          location=one.split[2],
          code=paste0(one.split[-(1:2)], collapse='_'))
    })
    if (length(filenames)==1)
        rv[,1]
    else
        t(rv)
}