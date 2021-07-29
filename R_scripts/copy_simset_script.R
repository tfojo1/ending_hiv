
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

#-- GET THE MSA --#
index = as.integer(args[1])
if (is.na(index) || index<1 || index>length(ALL.MSAS))
    stop(paste0("The first argument to the script must be an integer between 1 and ", length(ALL.MSAS)))
msa = ALL.MSAS[index]

#-- DO THE SET UP--#
print(paste0("Copying simsets for ", msa.names(msa)))
print("----------------------------------")


copy.and.thin.simsets(locations=msa,
                      src.dir = file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets'),
                      dst.dir = file.path(SYSTEMATIR.ROOT.DIR, 'visualization_simsets'),
                      thin.to=80,
                      compress=T,
                      verbose=T,
                      redo.seed=T,
                      redo.baseline=T,
                      compress.to.years=2010:2030)


print("----------------------------------")
