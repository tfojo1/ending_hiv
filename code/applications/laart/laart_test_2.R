

source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')
load('simulations/laart_test/12580.Rdata')

source('code/settings_and_files/setup_versions.R')
source('code/applications/laart/laart_jheem_settings.R')
source('code/applications/laart/laart_parameters.R')
source('code/applications/laart/laart_parameter_mapping.R')


simset = subset.simset(simset, 1:5)
#simplot(simset)

prepared = prepare.simset.for.intervention(simset, update.version = 'laart', verbose=T)

noint = run.simset.intervention(prepared, NO.INTERVENTION)

simplot(noint)

sim = noint@simulations[[1]]

rate.10 = -log(1-0.1)/(implemented.year-start.year)
u.DURABLE.LAART.10 = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                              start.year = start.year,
                                              years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
                                              rates = expr{c(rate.10, rate.10, 0.1 / (1-.1) * engaged.durably.suppressed.switch.to.laart)},
                                              scale = 'rate',
                                              apply.function = 'absolute')