
SYSTEMATIC.ROOT.DIR = '../work-tfojo1/Ending_HIV/mcmc_runs' #for the cluster
if (!dir.exists(SYSTEMATIC.ROOT.DIR))
    SYSTEMATIC.ROOT.DIR = 'Q:Ending_HIV/mcmc_runs'
if (!dir.exists(SYSTEMATIC.ROOT.DIR))
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
