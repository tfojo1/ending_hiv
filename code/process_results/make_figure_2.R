source('code/source_code.R')
source('code/targets/target_msas.R')

msa = MIAMI.MSA
dir = 'mcmc_runs/test_simset_interventions/'

load(file.path(dir, msa, 'No_Intervention.Rdata'))
base.simset = simset
load(file.path(dir, msa, 'Young_Black_and_Hispanic_MSM_testing_1pyr_0.9_suppressed_0.5_PrEP.Rdata'))
int.simset = simset
simset=NULL


plot.calibration.total(base.simset)

plot.calibration.risk(base.simset)
plot.calibration.total.incidence(list(base.simset, int.simset))
plot.calibration.total.incidence(list(base.simset, int.simset), facet.by='risk')
