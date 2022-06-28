

source('code/source_code.R')

source('code/applications/expanded_continuum/expanded_continuum_01_jheem_settings.R')
source('code/calibration/parameter_mappings/calibrated_parameters_expanded_01.R')

source('code/processing/visualization/sim_plots.R')

load('mcmc_runs/laart_test/12580.Rdata')
params = simset@parameters[1,]
run.simulation = create.run.simulation.function(BALTIMORE.MSA, version = 'expanded_0.1', start.values = params)

sim = run.simulation(params)

simplot(sim)
