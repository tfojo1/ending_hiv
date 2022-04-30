

source('code/source_code.R')
load('mcmc_runs/quick_simsets/1.0_12580_full.Rdata')
simset = subset.simset(simset, 1:5)
base = prepare.simset.for.interventions(simset)

source('code/applications/ehe/create_ehe_intervention_presets.R')
source('code/applications/ehe/ehe_systematic_intervention_sets.R')

# covid interventions
if (1==2)
{
    source('code/core_code/data_managers/mobility_data_manager.R')
    source('code/applications/covid/covid_interventions.R')
    load('cached/MOBILITY.DATA.MANAGER.Rdata')
    
    intervention = create.covid.scenario(sexual.transmission.reduction.distribution = Uniform.Distribution(0,0.5),
                                         suppression.reduction.distribution = Uniform.Distribution(0,0.4),
                                         testing.reduction.distribution = Uniform.Distribution(0,0.5),
                                         prep.reduction.distribution = Uniform.Distribution(0,0.3),
                                         mobility.weight.distribution = Uniform.Distribution(0,1),
                                         
                                         index.to.mobility = T,
                                         
                                         start.pandemic.effects.time = as.Date('2020-03-01'),
                                         start.sexual.transmission.normalize.time = as.Date('2021-03-08'),
                                         sexual.transmission.normalize.duration.months = 4,
                                         
                                         start.suppression.normalize.time = as.Date('2021-09-08'),
                                         suppression.normalize.duration.months = 4
    )
    
    
    
    noint = run.simset.intervention(base, NO.INTERVENTION)
    covid = run.simset.intervention(base, intervention)
    
    simplot(noint, covid)
}


# regular interventions
if (1==2)
{
    int = intervention.from.code('all.s90_23.27')
    
    post = run.simset.intervention(base, int)
    noint = run.simset.intervention(base, NO.INTERVENTION)
    
    source('code/processing/visualization/sim_plots.R')
    simplot(noint, post)
}