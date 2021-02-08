
setwd("Ending_HIV")
source('code/systematic_calibration/extract_and_run.R')
source('code/systematic_calibration/systematic_cache_status.R')
to.do = setdiff(TARGET.MSAS, get.mcmc.done.msas())

print("CHECKING CACHE STATUS")
print(Sys.Date())
print(Sys.time())
print(paste0("Checking ", length(to.do), " MSAs: ",
             paste0(to.do, collapse=', ')))
print("------------------------")

check.cache.status(to.do)