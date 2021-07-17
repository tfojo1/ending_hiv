
setwd("Ending_HIV")
source('code/source_code.R')
source('code/targets/target_msas.R')
source('code/process_results/make_systematic_table.R')
source('code/process_results/summarize_baseline_levels.R')

to.do = TARGET.MSAS

print("SUMMARIZING BASELINES FROM SIMSETS")
print(Sys.Date())
print(Sys.time())
print("------------------------")


print("Doing Baseline PrEP")
baseline.prep = get.baseline.levels('prep', dir='mcmc_runs/quick_simsets/')
save(baseline.prep, file=file.path(SYSTEMATIC.ROOT.DIR, '..', 'results', 'baseline.prep.Rdata'))
print("Done")

print("Doing Baseline Testing")
baseline.testing = get.baseline.levels('testing', dir='mcmc_runs/quick_simsets/')
save(baseline.testing, file=file.path(SYSTEMATIC.ROOT.DIR, '..', 'results', 'baseline.testing.Rdata'))
print("Done")

print("Doing Baseline Suppression")
baseline.suppression = get.baseline.levels('suppression', dir='mcmc_runs/quick_simsets/')
save(baseline.suppression, file=file.path(SYSTEMATIC.ROOT.DIR, '..', 'results', 'baseline.suppression.Rdata'))
print("Done")