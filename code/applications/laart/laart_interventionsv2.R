
create.laart.interventions <- function(target.population=WHOLE.POPULATION,
                                       simset = simset,
                                       start.year=2025,
                                       implemented.year=2028,
                                       durable.laart.25.parameters,
                                       durable.laart.50.parameters,
                                       suffix = '',
                                       INTERVENTION.MANAGER = INTERVENTION.MANAGER.1.0)
{
    if (suffix != '' && substr(suffix, 1,1)!='_')
        suffix = paste0("_", suffix)
    source('code/core_code/interventions/intervention_defaults.R')
    
    #-- Define Intervention Units --#
    switch.coefficient.25 = durable.laart.25.parameters$par[1]
    discontinuation.coefficient.25 = durable.laart.25.parameters$par[2]
    rate.25 = -log(1-0.25)/(implemented.year-start.year)
    rate.25 = rate.25 * switch.coefficient.25
    u.DURABLE.LAART.25 = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                                  start.year = start.year,
                                                  years = c(start.year+0.0001, implemented.year, implemented.year+0.001),#engaged.durably.suppressed.switch.to.laart
                                                  rates = expression(c(rate.25, rate.25, (0.25 / (1-.25)) * laart.discontinuation * discontinuation.coefficient.25)),
                                                  scale = 'rate',
                                                  apply.function = 'absolute',
                                                  expression.parameters = list(rate.25=rate.25, discontinuation.coefficient.25 = discontinuation.coefficient.25))
    switch.coefficient.50 = durable.laart.50.parameters$par[1]
    discontinuation.coefficient.50 = durable.laart.50.parameters$par[2]
    rate.50 = -log(1-0.50)/(implemented.year-start.year)
    rate.50 = rate.50 * switch.coefficient.50
    u.DURABLE.LAART.50 = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                                  start.year = start.year,
                                                  years = c(start.year+0.0001, implemented.year, implemented.year+0.001),#engaged.durably.suppressed.switch.to.laart
                                                  rates = expression(c(rate.50, rate.50, (0.50 / (1-.50)) * laart.discontinuation * discontinuation.coefficient.50)),
                                                  scale = 'rate',
                                                  apply.function = 'absolute',
                                                  expression.parameters = list(rate.50=rate.50, discontinuation.coefficient.50 = discontinuation.coefficient.50))
    
    rate.25 = -log(1-0.25)
    u.UNSUPPRESSED.LAART.25 = create.intervention.unit(type = 'engaged.unsuppressed.switch.to.laart',
                                                       start.year = start.year,
                                                       years = c(implemented.year),
                                                       rates = expression(c(rate.25)),
                                                       scale = 'rate',
                                                       apply.function = 'absolute',
                                                       expression.parameters = list(rate.25=rate.25))
    rate.50 = -log(1-0.50)
    u.UNSUPPRESSED.LAART.50 = create.intervention.unit(type = 'engaged.unsuppressed.switch.to.laart',
                                                       start.year = start.year,
                                                       years = c(implemented.year),
                                                       rates = expression(c(rate.50)),
                                                       scale = 'rate',
                                                       apply.function = 'absolute',
                                                       expression.parameters = list(rate.50=rate.50))
    
    
    rate.25 = -log(1-0.25)
    u.NAIVE.LAART.25 = create.intervention.unit(type = 'engaged.naive.switch.to.laart',
                                                start.year = start.year,
                                                years = c(implemented.year),
                                                rates = expression(c(rate.25)),
                                                scale = 'rate',
                                                apply.function = 'absolute',
                                                expression.parameters = list(rate.25=rate.25))
    
    rate.50 = -log(1-0.50)
    u.NAIVE.LAART.50 = create.intervention.unit(type = 'engaged.naive.switch.to.laart',
                                                start.year = start.year,
                                                years = c(implemented.year),
                                                rates = expression(c(rate.50)),
                                                scale = 'rate',
                                                apply.function = 'absolute',
                                                expression.parameters = list(rate.50=rate.50))


    #-- Create Interventions --#
    
    durable.laart.25 = create.intervention(WHOLE.POPULATION, 
                                           u.DURABLE.LAART.25)
    
    durable.laart.50 = create.intervention(WHOLE.POPULATION, 
                                           u.DURABLE.LAART.50)
    
    unsuppressed.laart.25 = create.intervention(WHOLE.POPULATION, 
                                                u.UNSUPPRESSED.LAART.25)
    
    unsuppressed.laart.50 = create.intervention(WHOLE.POPULATION, 
                                                u.UNSUPPRESSED.LAART.50)
    
    naive.laart.25 = create.intervention(WHOLE.POPULATION, 
                                         u.NAIVE.LAART.25)
    
    naive.laart.50 = create.intervention(WHOLE.POPULATION, 
                                         u.NAIVE.LAART.50)
    
    everything.laart.25 = create.intervention(WHOLE.POPULATION, 
                                              u.NAIVE.LAART.25, u.DURABLE.LAART.25, u.UNSUPPRESSED.LAART.25)
    
    everything.laart.50 = create.intervention(WHOLE.POPULATION, 
                                              u.NAIVE.LAART.50, u.DURABLE.LAART.50, u.UNSUPPRESSED.LAART.50)
    
    
    INTERVENTION.MANAGER.1.0 = register.intervention(durable.laart.25, code=paste0('durable.laart.25'),
                                                     name='25% of durably suppressed start LAART',
                                                     manager = INTERVENTION.MANAGER.1.0)
    
    INTERVENTION.MANAGER.1.0 = register.intervention(durable.laart.50, code=paste0('durable.laart.50'),
                                                     name='50% of durably suppressed start LAART',
                                                     manager = INTERVENTION.MANAGER.1.0)
    
    INTERVENTION.MANAGER.1.0 = register.intervention(unsuppressed.laart.25, code=paste0('unsuppressed.laart.25'),
                                                     name='25% of unsuppressed start LAART',
                                                     manager = INTERVENTION.MANAGER.1.0)
    INTERVENTION.MANAGER.1.0 = register.intervention(unsuppressed.laart.50, code=paste0('unsuppressed.laart.50'),
                                                     name='50% of unsuppressed start LAART',
                                                     manager = INTERVENTION.MANAGER.1.0)
    
    INTERVENTION.MANAGER.1.0 = register.intervention(naive.laart.25, code=paste0('naive.laart.25'),
                                                     name='25% of unsuppressed naive start LAART',
                                                     manager = INTERVENTION.MANAGER.1.0)
    
    INTERVENTION.MANAGER.1.0 = register.intervention(naive.laart.50, code=paste0('naive.laart.50'),
                                                     name='50% of unsuppressed naive start LAART',
                                                     manager = INTERVENTION.MANAGER.1.0)
    
    INTERVENTION.MANAGER.1.0 = register.intervention(everything.laart.25, code=paste0('everything.laart.25'),
                                                     name='25% of unsuppressed naive/engaged and durable  start LAART',
                                                     manager = INTERVENTION.MANAGER.1.0)
    
    INTERVENTION.MANAGER.1.0 = register.intervention(everything.laart.50, code=paste0('everything.laart.50'),
                                                     name='50% of unsuppressed naive/engaged and durable  start LAART',
                                                     manager = INTERVENTION.MANAGER.1.0)
    
    
    #-- Return the intervention manager --#
    INTERVENTION.MANAGER.1.0
}
#durable.laart.25.parameters = get.durable.25.intervention.parameters(optim.simset = optim.simset)
#durable.laart.50.parameters = get.durable.50.intervention.parameters(optim.simset = optim.simset)
#print(durable.laart.25.parameters)
#INTERVENTION.MANAGER.1.0 = create.laart.interventions(durable.laart.25.parameters = durable.laart.25.parameters, durable.laart.50.parameters = durable.laart.50.parameters)
#LAART.INTERVENTION.CODES = c("noint", "durable.laart.25", "durable.laart.50", "unsuppressed.laart.25", "unsuppressed.laart.50", "naive.laart.25", "naive.laart.50", "everything.laart.25", "everything.laart.50")
#LAART.INTERVENTIONS = lapply(LAART.INTERVENTION.CODES, intervention.from.code)

