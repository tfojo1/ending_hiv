
if (1==2)
    setwd('../../../Ending HIV/Ending_HIV/')

load('simulations/laart_test/12580.Rdata')

source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

source('code/settings_and_files/setup_versions.R')
source('code/applications/laart/laart_jheem_settings.R')
source('code/applications/laart/laart_parameters.R')
source('code/applications/laart/laart_parameter_mapping.R')


simset = subset.simset(simset, 1:5)
#simplot(simset)

prepared = prepare.simset.for.intervention(simset, update.version = 'laart')


noint = run.simset.intervention(prepared, NO.INTERVENTION, run.to.year = 2035)



# some interventions
implemented.year = 2028
start.year = 2025

rate.10 = -log(1-0.1)/(implemented.year-start.year)
u.DURABLE.LAART.10 = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                              start.year = start.year,
                                              years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
                                              rates = expression(c(rate.10, rate.10, 0.1 / (1-.1) * laart.discontinuation)),
                                              scale = 'rate',
                                              apply.function = 'absolute',
                                              expression.parameters = list(rate.10=rate.10))

intervention = create.intervention(WHOLE.POPULATION, u.DURABLE.LAART.10)
INTERVENTION.MANAGER.1.0 = register.intervention(intervention,
                                                 code = 'durable.laart.10_25.28',
                                                 name = 'start 10% of durably suppressed PWH on LAART',
                                                 manager = INTERVENTION.MANAGER.1.0)

# now with 50%

rate.50 = -log(1-0.5)/(implemented.year-start.year)
u.DURABLE.LAART.50 = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                              start.year = start.year,
                                              years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
                                              rates = expression(c(rate.50, rate.50, 0.5 / (1-.5) * laart.discontinuation)),
                                              scale = 'rate',
                                              apply.function = 'absolute',
                                              expression.parameters = list(rate.50=rate.50))

intervention = create.intervention(WHOLE.POPULATION, u.DURABLE.LAART.50)
INTERVENTION.MANAGER.1.0 = register.intervention(intervention,
                                                 code = 'durable.laart.50_25.28',
                                                 name = 'start 50% of durably suppressed PWH on LAART',
                                                 manager = INTERVENTION.MANAGER.1.0)


int = run.simset.intervention(prepared, intervention, run.to.year = 2035)

simplot(noint, int)
simplot(noint, int, data.types='suppression')


# the below is indexed [year, simulation]
suppression.delta = sapply(1:int@n.sim, function(i){
    sim.noint = noint@simulations[[i]]
    sim.int = int@simulations[[i]]
    
    (extract.suppression(sim.int) - extract.suppression(sim.noint) ) / extract.suppression(sim.noint)
})

print(paste0("On average, the intervention changed suppression by ",
             round(mean(suppression.delta['2035',]), 3), '% from what it would have been in 2035'))


incidence.delta = sapply(1:int@n.sim, function(i){
    sim.noint = noint@simulations[[i]]
    sim.int = int@simulations[[i]]
    
    (project.absolute.incidence(sim.int) - project.absolute.incidence(sim.noint) ) / project.absolute.incidence(sim.noint)
})

print(paste0("On average, the intervention changed incidence by ",
             round(mean(incidence.delta['2035',]), 3), '% from what it would have been in 2035'))


engagement.delta = sapply(1:int@n.sim, function(i){
    sim.noint = noint@simulations[[i]]
    sim.int = int@simulations[[i]]
    
    (do.extract.engagement(sim.int) - do.extract.engagement(sim.noint) ) / do.extract.engagement(sim.noint)
})

print(paste0("On average, the intervention changed engagement by ",
             round(mean(engagement.delta['2035',]), 3), '% from what it would have been in 2035'))
