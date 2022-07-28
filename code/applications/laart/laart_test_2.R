

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

sim

# for debugging
if (1==2)
{
    init.components = create.initial.components(location = get.simset.location(simset),
                                                version = 'laart',
                                                start.values = simset@parameters[1,],
                                                fix.components = T)
    
    
    
    base.components = setup.initial.components(msa=get.simset.location(simset), version='laart', verbose=F)
    get.components.fn = get.components.function.for.version('laart')
    
    init.components = get.components.fn(simset@parameters[1,], base.components)
}