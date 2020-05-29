
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