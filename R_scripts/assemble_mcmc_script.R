
setwd("Ending_HIV")
source('code/systematic_calibration/extract_and_run.R')
source('code/systematic_calibration/systematic_cache_status.R')
to.do = setdiff(TARGET.MSAS, get.mcmc.done.msas())
assemble.and.thin.mcmcs(to.do)