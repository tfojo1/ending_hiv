
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

#-- GET THE MSA --#
index = as.integer(args[1])
if (is.na(index) || index<1 || index>length(TARGET.MSAS))
    stop(paste0("The first argument to the script must be an integer between 1 and ", length(TARGET.MSAS)))
msa = TARGET.MSAS[index]

#-- DO THE SET UP--#
print(paste0("Running interventions for ", msa.names(msa)))

do.run.interventions(location=msa,
                     simset.dir=file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets'),
                     dst.dir=file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets'),
                     interventions = ALL.INTERVENTIONS)
