
setwd('Ending_HIV')

print("Sourcing Code Files")
source('code/source_code.R')
source('code/targets/target_msas.R')
source('code/process_results/extract_absolute_arrays.R')

ROOT.DIR = file.path(SYSTEMATIC.ROOT.DIR, "..", "results", "full", "estimates")

#-- MAIN --#
print("Assembling Absolute Arrays for Baseline")
abs.arr = assemble.absolute.outcome.arrays(dir.name='full',
                                           locations = TARGET.MSAS,
                                           interventions=NULL)
save(abs.arr, file=file.path(SYSTEMATIC.ROOT.DIR, '..', 'results', 'full', 'estimates', 'baseline.abs.arr.Rdata'))



print("Assembling Absolute Arrays for No Intervention")
abs.arr = assemble.absolute.outcome.arrays(dir.name='full',
                                           locations = TARGET.MSAS,
                                           interventions=NO.INTERVENTION)
save(abs.arr, file=file.path(SYSTEMATIC.ROOT.DIR, '..', 'results', 'full', 'estimates', 'noint.abs.arr.Rdata'))

print("Assembling Absolute Arrays for 95-95-95 (v2)")
abs.arr = assemble.absolute.outcome.arrays(dir.name='full',
                                           locations = TARGET.MSAS,
                                           interventions=APPROX959595.INTERVENTIONS.23.25[[2]])
save(abs.arr, file=file.path(SYSTEMATIC.ROOT.DIR, '..', 'results', 'full', 'estimates', 'approx959595.v2.Rdata'))