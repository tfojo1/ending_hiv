
setwd("Ending_HIV")
source('code/systematic_calibration/extract_and_run.R')
source('code/systematic_calibration/systematic_cache_status.R')
to.do = setdiff(TARGET.MSAS, get.mcmc.done.msas())

print("ASSEMBLING MCMCs FROM CACHE")
print(Sys.Date())
print(Sys.time())
print(paste0("Attempting to assemble from ", length(to.do), " MSAs: ",
             paste0(to.do, collapse=', ')))
print("------------------------")

assemble.and.thin.mcmcs(to.do)