
SYSTEMATIC.ROOT.DIR = 'mcmc_runs'

load.mcmc.from.dir <- function(location, 
                              dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_initial'))
{
    files = list.files(dir)
    full.files = list.files(dir, full.names = T)
    
    mask = grepl(location, files)
    if (!any(mask))
        stop(paste0("No mcmc runs found in ", dir, " for '", msa.names(location), "' MSA (", location, ')'))
    load(full.files[mask][sum(mask)])
    
    mcmc
}

do.extract.and.save.test.simset <- function(mcmc,
                                            additional.thin=5,
                                            additional.burn=500)
{
    do.extract.and.save.simset(mcmc,
                               simset.prefix='test',
                               additional.thin=additional.thin,
                               additional.burn=additional.burn)
}

get.mcmc.for.msa <- function(msa, dir)
{
    files = list.files(dir)
    files = files[grepl(msa, files)]
    if (length(files)==0)
        stop("No files for location '", msa, "' (", msa.names(msa), ')')
    
    file = sort(files, decreasing = T)[1]
    load(file.path(dir, file))
    mcmc
}

do.extract.and.save.simset <- function(mcmc,
                                       simset.prefix,
                                       additional.thin=5,
                                       additional.burn=500)
{
    simset = extract.simset(mcmc, additional.burn=additional.burn, additional.thin=additional.thin)
    simset = prepare.simset.for.interventions(simset)
    location = attr(simset@simulations[[1]], 'location')
    
    file = get.base.simset.filename(location,
                                    simset.prefix=simset.prefix)
    save(simset, file=file)
    invisible(simset)
}

##-- FILENAME MANAGERS --##

get.base.simset.filename <- function(location,
                                     simset.prefix = c('full','limited','test')[1])
{
    file.path(SYSTEMATIC.ROOT.DIR, paste0(simset.prefix, '_simsets'), paste0(location, '.Rdata'))
}

get.intervention.simsets.dir <- function(location,
                                         simset.prefix)
{
    file.path(SYSTEMATIC.ROOT.DIR, paste0(simset.prefix, '_simset_interventions'), location)
}