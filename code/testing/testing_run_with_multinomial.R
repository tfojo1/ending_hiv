

source('code/source_code.R')
source('code/calibration/calibrated_parameters_expanded_1.R')

load('code/for Melissa/balt_starting_params.Rdata')

run.simulation = create.run.simulation.function(BALTIMORE.MSA,
                                                start.values=starting.parameters,
                                                settings=SETTINGS.EXPANDED.CONTINUUM)

sim = run.simulation(starting.parameters)

source('code/plots.R')
plot.calibration(sim)
