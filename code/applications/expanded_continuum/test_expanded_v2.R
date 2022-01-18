

source('code/source_code.R')
source('code/calibration/parameter_mappings/calibrated_parameters_expanded_2.R')



load('mcmc_runs/start_values/12580.Rdata')

pp1 = suppressWarnings(get.medians(parameters.prior))
matching.names = intersect(names(pp1), names(starting.parameters))
pp1[matching.names] = starting.parameters[matching.names]
run.simulation = create.run.simulation.function(BALTIMORE.MSA,
                                                start.values=pp1,
                                                version='expanded_1.0', 
                                                catch.errors = F)


sim1 = run.simulation(pp1)

source('code/processing/visualization/sim_plots.R')
simplot(sim1)
simplot(sim1, data.types = c('suppression','engagement','suppression.of.engaged'))


# @Melissa, play with parameters here

# for example
pp2 = pp1
pp2['msm.start.art.or'] = 0.5
pp2['idu.start.art.or'] = 0.5
pp2['msm.idu.start.art.or'] = 0.5
pp2['heterosexual.start.art.or'] = 0.5

sim2 = run.simulation(pp2)

simplot(sim1, sim2)
simplot(sim1, sim2, data.types = c('suppression','engagement','suppression.of.engaged'))

# the parameters that are specific to your model are:
print(setdiff(get.parameters.prior.for.version(VERSION.MANAGER, 'expanded_1.0')@var.names,
              get.parameters.prior.for.version(VERSION.MANAGER, 'collapsed_1.0')@var.names))
