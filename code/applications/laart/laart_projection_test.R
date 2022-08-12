
#-- LOAD UP STUFF --#
source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

source('code/settings_and_files/setup_versions.R')
source('code/applications/laart/laart_jheem_settings.R')
source('code/applications/laart/laart_parameters.R')
source('code/applications/laart/laart_parameter_mapping.R')
source('code/applications/laart/laart_interventions.R')

load('simulations/laart_test/12580.Rdata')

prepared = prepare.simset.for.intervention(subset.simset(simset, simset@n.sim-2:0), update.version = 'laart')


#-- MAKE FUNCTIONS TO SUMMARIZE RESULTS --#

get.proportions.durable <- function(sim, years=2035)
{
    pop = do.extract.population.subset(sim, years=years, keep.dimensions=c('continuum'), include.hiv.negative = F)
    
    of.interest = pop[c('engaged_durably_suppressed','laart_durably_suppressed','resistant_durably_suppressed')]
    of.interest / sum(of.interest)
}

get.proportions.unsuppressed <- function(sim, years=2035)
{
    pop = do.extract.population.subset(sim, years=years, keep.dimensions=c('continuum'), include.hiv.negative = F)
    
    of.interest = pop[c('engaged_unsuppressed_failing','laart_unsuppressed','resistant_unsuppressed')]
    of.interest / sum(of.interest)
}

get.proportions.engaged <- function(sim, years=2035)
{
    pop = do.extract.population.subset(sim, years=years, keep.dimensions=c('continuum'), include.hiv.negative = F)
    
    of.interest = pop[get.settings.for.version('laart')$ENGAGED_STATES]
    of.interest / sum(of.interest)
}

get.proportions.engaged.by.laart <- function(sim, years=2035)
{
    pop = do.extract.population.subset(sim, years=years, keep.dimensions=c('continuum'), include.hiv.negative = F)
    
    engaged.states = get.settings.for.version('laart')$ENGAGED_STATES
    laart.states = c('laart_unsuppressed','laart_recently_suppressed', 'laart_durably_suppressed')
    resistant.states = c('resistant_unsuppressed','resistant_recently_suppressed', 'resistant_durably_suppressed')
    oral.states = setdiff(engaged.states, c(laart.states, resistant.states))
    
    c(oral=sum(pop[oral.states]), laart=sum(pop[laart.states]), resistant=sum(pop[resistant.states])) / sum(pop[engaged.states])
}

#-- DEFINE A TEST INTERVENTION --#

start.year = 2025
implemented.year = 2028

rate.50 = -log(1-0.50)/(implemented.year-start.year)
u.DURABLE.LAART.50 = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                              start.year = start.year,
                                              years = c(start.year+0.0001, implemented.year, implemented.year+0.001),#engaged.durably.suppressed.switch.to.laart
                                              rates = expression(c(rate.50, rate.50, 0)),
                                              scale = 'rate',
                                              apply.function = 'absolute',
                                              expression.parameters = list(rate.50=rate.50))
u.stop.laart.attrition = create.intervention.unit(type = 'laart.durably.suppressed.to.engaged.durably.suppressed',
                                                  start.year = start.year,
                                                  years = start.year+0.0001,
                                                  rates = 0,
                                                  scale = 'rate',
                                                  apply.function = 'absolute')

intervention.to.test = create.intervention(WHOLE.POPULATION, 
                                        u.DURABLE.LAART.50)

source('code/core_code/interventions/intervention_defaults.R')
INTERVENTION.MANAGER.1.0 = register.intervention(intervention.to.test, code=paste0('test.intervention'),
                                             name='test',
                                             manager = INTERVENTION.MANAGER.1.0, allow.intervention.multiple.names = T)

#-- RUN AND EXPLORE THE INTERVENTION --#
projected = run.simset.intervention(prepared, 
                                    intervention=intervention.to.test,
                                    run.to.year=2035,
                                    keep.years=2015:2035)


round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(projected@simulations, get.proportions.durable, years=as.numeric(year)))
})),1)
round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(projected@simulations, get.proportions.engaged.by.laart, years=as.numeric(year)))
})),1)
