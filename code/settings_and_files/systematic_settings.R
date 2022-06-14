
##--------------------------------##
##-- PATHS to MAJOR DIRECTORIES --##
##--------------------------------##

COMPUTER.LOCATION = 'cluster'
SYSTEMATIC.ROOT.DIR = '../work-tfojo1/Ending_HIV/' #for the cluster
if (!dir.exists(SYSTEMATIC.ROOT.DIR))
{
    SYSTEMATIC.ROOT.DIR = 'Q:JHEEM'
    COMPUTER.LOCATION = 'desktop'
}
if (!dir.exists(SYSTEMATIC.ROOT.DIR))
{
    SYSTEMATIC.ROOT.DIR = '.'
    COMPUTER.LOCATION = 'laptop'
}

SIMULATIONS.DIR = file.path(SYSTEMATIC.ROOT.DIR, 'simulations')
MCMC.DIR = file.path(SYSTEMATIC.ROOT.DIR, 'mcmc_runs')
RESULTS.DIR = file.path(SYSTEMATIC.ROOT.DIR, 'results')
START.VALUES.DIR = 'start_values'

ON.LAPTOP = COMPUTER.LOCATION == 'laptop'
ON.DESKTOP = COMPUTER.LOCATION == 'desktop'
ON.CLUSTER = COMPUTER.LOCATION == 'cluster'


##------------------------------------------##
##--      SOME CONVENIENCE FUNCTIONS      --##
##-- (for pulling mcmc runs by location)  --##
##------------------------------------------##

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
