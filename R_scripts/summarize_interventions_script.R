
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
source('code/process_results/make_systematic_table.R')
source('code/process_results/distributed_process_results.R')

#-- GET THE MSA --#
index = as.integer(args[1])
if (is.na(index) || index<1 || index>length(TARGET.MSAS))
    stop(paste0("The first argument to the script must be an integer between 1 and ", length(TARGET.MSAS)))
msa = TARGET.MSAS[index]

#-- DO THE SET UP--#
print(paste0("Processing interventions for ", msa.names(msa)))


do.get.raw.estimates(dir.name='full',
                     interventions=MAIN.INTERVENTIONS.23.27,
                     suffix='main',
                     year2=2030,
                     msa=msa,
                     overwrite=F,
                     save.if.incomplete = F)

do.get.raw.estimates(dir.name='full',
                     interventions=MAIN.INTERVENTIONS.23.27,
                     suffix='main.2025',
                     year2=2025,
                     msa=msa,
                     overwrite=F,
                     save.if.incomplete = F)

do.get.raw.estimates(dir.name='full',
                     interventions=MAIN.INTERVENTIONS.23.25,
                     suffix='rollout.3y',
                     year2=2030,
                     msa=msa,
                     overwrite=F,
                     save.if.incomplete = F)

do.get.raw.estimates(dir.name='full',
                     interventions=MAIN.INTERVENTIONS.23.25,
                     suffix='rollout.3y.2025',
                     year2=2025,
                     msa=msa,
                     overwrite=F,
                     save.if.incomplete = F)

do.get.raw.estimates(dir.name='full',
                     interventions=IDU.INTERVENTIONS.PLUS.23.27,
                     suffix='idu',
                     year2=2030,
                     msa=msa,
                     overwrite=F,
                     save.if.incomplete = F)


print("All Done")
