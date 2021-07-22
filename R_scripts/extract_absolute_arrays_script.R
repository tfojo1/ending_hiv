
#-- CHECK ARGUMENTS --#
args = commandArgs(trailingOnly=TRUE)
if (length(args)<1) {
    stop("One argument must be supplied", call.=FALSE)
}

#-- SET THE WD --#
setwd('Ending_HIV')

#-- SOURCE THE RELEVANT FILES --#
print("Loading Source Files...")
source('code/systematic_calibration/extract_and_run.R')
source('code/targets/target_msas.R')
source('code/process_results/extract_absolute_arrays.R')

#-- GET THE MSA --#
index = as.integer(args[1])
if (is.na(index) || index<1 || index>length(TARGET.MSAS))
    stop(paste0("The first argument to the script must be an integer between 1 and ", length(TARGET.MSAS)))
msa = TARGET.MSAS[index]

#-- DO THE SET UP--#
print(paste0("Extracting absolute arrays for for ", msa.names(msa)))


#-- EXTRACT --#

print("Extracting for baseline")
distributed.get.absolute.outcome.array(dir.name='full',
                                       location=msa,
                                       intervention=NULL,
                                       years=2010:2020)
print("Done\n\n")

print("Extracting for no intervention")
distributed.get.absolute.outcome.array(dir.name='full',
                                       location=msa,
                                       intervention=NO.INTERVENTION,
                                       years=2020:2030)
print("Done\n\n")



print("Extracting for 95-95-95 (v2)")
distributed.get.absolute.outcome.array(dir.name='full',
                                       location=msa,
                                       intervention=APPROX959595.INTERVENTIONS.23.25[[2]],
                                       years=2020:2030)
print("Done\n\n")
