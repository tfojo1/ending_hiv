
source('code/source_code.R')
source('code/systematic_calibration/systematic_interventions.R')
source('code/systematic_calibration/simsets_from_systematic.R')
source('code/targets/target_msas.R')

make.and.save.simset.for.location(NYC.MSA, full = F)
run.systematic.interventions(NYC.MSA, full.simset = F)
