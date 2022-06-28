

source('code/source_code.R')

SIMSET.DIR = file.path(SIMULATIONS.DIR, 'laart_test')

load(file.path(SIMSET.DIR, '12580.Rdata'))

test.run = run.simset.intervention(simset, NO.INTERVENTION,
                                   run.to.year=2035)
