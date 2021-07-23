
setwd('Ending_HIV')

print("Sourcing Code Files")
source('code/source_code.R')
source('code/targets/target_msas.R')
source('code/sensitivity_analyses/sensitivity_analyses.R')


N.SIM = 1000
INTERVENTION.FOR.SENSITIVITY = intervention.from.code('mi.t2x.p25.s90_23.27')   

print("ASSEMBLING SENSITIVITY DATA FRAMES...")
print("")
sensitivity.dfs = make.sensitivity.dfs('mcmc_runs/full_simsets',
                                       interventions=list(INTERVENTION.FOR.SENSITIVITY),
                                       n.sim=N.SIM)
save(sensitivity.dfs, file='results/sensitivity.dfs.Rdata')
print("")
print("ALL DONE")