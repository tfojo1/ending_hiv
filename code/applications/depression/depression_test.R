

load('simulations/laart_test/12580.Rdata')
params = simset@parameters[1,]


source('code/source_code.R')
ALL.DATA.MANAGERS$comorbidities = comorbidities.manager

source('code/settings_and_files/setup_versions.R')
source('code/applications/depression/depression_jheem_settings.R')


run.simulation = create.run.simulation.function('12580', params,
                                                version='depression_1.0')


sim = run.simulation(params)

source('code/processing/visualization/sim_plots.R')
simplot(sim)

round(extract.new.diagnoses(sim, keep.dimensions = c('year','subpopulation'), per.population = NA))


if (1==2)
{
    source('code/applications/depression/depression_best_rate_estimates.R')
    source('code/applications/depression/read_depression_into_comorbidities.R')
    comorbidities.manager = create.comorbidities.manager()
    comorbidities.manager = read.depression.data.into.comorbidities.manager(comorbidities.manager)
    ALL.DATA.MANAGERS$comorbidities = comorbidities.manager
}

