

##-------------------------##
##-- MANAGING FILE NAMES --##
##-------------------------##

make.filenames.from.elements <- function(location,
                                        intervention.code,
                                        version)
{
    version = check.version.for.file(version)
    
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

get.simset.filename <- function(simset,
                                location=attr(simset@simulations[[1]], 'location'),
                                intervention=attr(simset, 'intervention'),
                                intervention.code=NULL,
                                version=get.simset.file.version(simset))
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
                        compress.cd4=T,
                        #compress.years=max(attr(simset, 'run.from.year'), min(simset@simulations[[1]]$years)):max(simset@simulations[[1]]$years),
                        full=F,
                        version=get.simset.file.version(simset),
                        save.full.in.master.directory=F)
{
    if (compress)
        simset = compress.simset(simset, keep.cd4=!compress.cd4)#, keep.years=compress.years)
    if (pare.components)
        simset = pare.simset.components(simset)
    
    location = attr(simset@simulations[[1]], 'location')
    
    if (full)
        filename = get.full.filename(location, version=version)
    else
        filename = get.simset.filename(simset, version=version)
    
    if (basename(dir) != location && 
        (!full || !save.full.in.master.directory))
        dir = file.path(dir, location)
    if (!dir.exists(dir))
        dir.create(dir)
    
    save(simset, file=file.path(dir, filename))
}

save.seed.simset <- function(simset,
                             dir,
                             min.seed.keep.to.year=2020,
                             max.seed.keep.from.year=2018, #for census totals
                             version=get.simset.file.version(simset))
{
    version = check.version.for.file(version)
    
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
    
    if (!dir.exists(file.path(dir, location)))
        dir.create(file.path(dir, location), recursive=T)
    save(simset, file=file.path(dir, location, filename))
}

get.seed.filename <- function(location,
                              version)
{
    make.filenames.from.elements(version=version,
                                 location=location,
                                 intervention.code = 'seed')
}

get.full.filename <- function(location,
                              version)
{
    make.filenames.from.elements(version=version,
                                 location=location,
                                 intervention.code = 'full')
}

get.baseline.filename <- function(location,
                                  version)
{
    get.simset.filename(simset=NULL,
                        location=location,
                        intervention.code=NULL,
                        version=version)
}

parse.simset.filenames <- function(filenames, remove.rdata.extension=T)
{
    if (remove.rdata.extension)
        filenames = gsub('\\.Rdata$', '', filenames, ignore.case = T)
    
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

check.version.for.file <- function(version, version.manager=VERSION.MANAGER)
{
    if (!is.character(version) || length(version) != 1 || is.na(version) || version=='')
        stop('version must be a non-empty, non-NA, single character value')
    
    if (any(version.manager$file.version==version))
    {
        if (grepl('_', version))
            stop("version cannot contain the character '_'")
        version
    }
    else if (any(version.manager$versions==version))
    {
        rv = get.file.version(version, version.manager = version.manager)
        
        if (grepl('_', rv))
            stop(paste0("file.version ('", rv, "') for version cannot contain the character '_'"))
        
        rv
    }
    else
        stop(paste0("'", version, "' has not been registered as a valid JHEEM version or a file.version with the version manager"))
    
    
}