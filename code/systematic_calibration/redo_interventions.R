
source('code/source_code.R')
source('code/interventions/systematic_interventions.R')
source('code/targets/target_msas.R')

run.systematic.interventions.from.seed(locations=TARGET.MSAS[28 + 1 + 1:3],
                                       dir='mcmc_runs/visualization_simsets/',
                                       interventions=ALL.INTERVENTIONS,
                                       overwrite=T,
                                       compress=T,
                                       run.to.year=2030,
                                       version='1.0',
                                       recrunch.baseline = T, #a temp param to crunch intervention rates for baselines
                                       verbose=T)
    