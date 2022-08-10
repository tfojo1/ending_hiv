
create.laart.interventions <- function(target.population=WHOLE.POPULATION,
                                       
                                       start.year=2025,
                                       implemented.year=2028,
                                       
                                       suffix = '',
                                       INTERVENTION.MANAGER = INTERVENTION.MANAGER.1.0)
{
    if (suffix != '' && substr(suffix, 1,1)!='_')
        suffix = paste0("_", suffix)
    
    
    #-- Define Intervention Units --#
    
    
    rate.10 = -log(1-0.1)/(implemented.year-start.year)
    u.DURABLE.LAART.10 = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                                  start.year = start.year,
                                                  years = c(start.year+0.0001, implemented.year, implemented.year+0.001),#engaged.durably.suppressed.switch.to.laart
                                                  rates = expression(c(rate.10, rate.10, 0.1 / (1-.1) * laart.discontinuation)),
                                                  scale = 'rate',
                                                  apply.function = 'absolute',
                                                  expression.parameters = list(rate.10=rate.10))
    
    rate.25 = -log(1-0.25)/(implemented.year-start.year)
    u.DURABLE.LAART.25 = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                                  start.year = start.year,
                                                  years = c(start.year+0.0001, implemented.year, implemented.year+0.001),#engaged.durably.suppressed.switch.to.laart
                                                  rates = expression(c(rate.25, rate.25, 0.25 / (1-.25) * laart.discontinuation)),
                                                  scale = 'rate',
                                                  apply.function = 'absolute',
                                                  expression.parameters = list(rate.25=rate.25))
    
    rate.50 = -log(1-0.50)/(implemented.year-start.year)
    u.DURABLE.LAART.50 = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                                  start.year = start.year,
                                                  years = c(start.year+0.0001, implemented.year, implemented.year+0.001),#engaged.durably.suppressed.switch.to.laart
                                                  rates = expression(c(rate.50, rate.50, 0.50 / (1-.50) * laart.discontinuation)),
                                                  scale = 'rate',
                                                  apply.function = 'absolute',
                                                  expression.parameters = list(rate.50=rate.50))
    
    
    u.UNSUPPRESSED.LAART.10 = create.intervention.unit(type = 'engaged.unsuppressed.switch.to.laart',
                                                       start.year = start.year,
                                                       years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
                                                       rates = expression(c(rate.10, rate.10, 0.10 / (1-.10) * laart.discontinuation)),#engaged.durably.suppressed.switch.to.laart
                                                       scale = 'rate',
                                                       apply.function = 'absolute',
                                                       expression.parameters = list(rate.10=rate.10)) 
    
    u.UNSUPPRESSED.LAART.25 = create.intervention.unit(type = 'engaged.unsuppressed.switch.to.laart',
                                                  start.year = start.year,
                                                  years = c(start.year+0.0001, implemented.year, implemented.year+0.001),#engaged.durably.suppressed.switch.to.laart
                                                  rates = expression(c(rate.25, rate.25, 0.25 / (1-.25) * laart.discontinuation)),
                                                  scale = 'rate',
                                                  apply.function = 'absolute',
                                                  expression.parameters = list(rate.25=rate.25))
    
    u.UNSUPPRESSED.LAART.50 = create.intervention.unit(type = 'engaged.unsuppressed.switch.to.laart',
                                                  start.year = start.year,
                                                  years = c(start.year+0.0001, implemented.year, implemented.year+0.001),#engaged.durably.suppressed.switch.to.laart
                                                  rates = expression(c(rate.50, rate.50, 0.50 / (1-.50) * laart.discontinuation)),
                                                  scale = 'rate',
                                                  apply.function = 'absolute',
                                                  expression.parameters = list(rate.50=rate.50))
    
    
    
    #@preetham redo all as above
    # 
    # 
    # 
    # u.DURABLE.LAART.05py = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
    #                                                 start.year = start.year,
    #                                                 years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
    #                                                 rates = expr{c(0.05, 0.05, 0.1 / (1-.1) * engaged.durably.suppressed.switch.to.laart)},
    #                                                 scale = 'proportion',
    #                                                 apply.function = 'absolute')
    # 
    # u.DURABLE.LAART.10py = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
    #                                                 start.year = start.year,
    #                                                 years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
    #                                                 rates = expr{c(0.1, 0.1, 0.25 / (1-.25) * engaged.durably.suppressed.switch.to.laart)},
    #                                                 scale = 'proportion',
    #                                                 apply.function = 'absolute')
    # 
    # u.DURABLE.LAART.20py = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
    #                                                 start.year = start.year,
    #                                                 years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
    #                                                 rates = expr{c(0.2, 0.2, 0.5 / (1-.5) * engaged.durably.suppressed.switch.to.laart)},
    #                                                 scale = 'proportion',
    #                                                 apply.function = 'absolute')
    # 
    # u.UNSUPPRESSED.LAART.05py = create.intervention.unit(type = 'engaged.unsuppressed.switch.to.laart',
    #                                                 start.year = start.year,
    #                                                 years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
    #                                                 rates = expr{c(0.05, 0.05, 0.1 / (1-.1) * engaged.unsuppressed.switch.to.laart)},
    #                                                 scale = 'proportion',
    #                                                 apply.function = 'absolute')
    # 
    # u.UNSUPPRESSED.LAART.10py = create.intervention.unit(type = 'engaged.unsuppressed.switch.to.laart',
    #                                                 start.year = start.year,
    #                                                 years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
    #                                                 rates = expr{c(0.1, 0.1, 0.25 / (1-.25) * engaged.unsuppressed.switch.to.laart)},
    #                                                 scale = 'proportion',
    #                                                 apply.function = 'absolute')
    # 
    # u.UNSUPPRESSED.LAART.20py = create.intervention.unit(type = 'engaged.unsuppressed.switch.to.laart',
    #                                                 start.year = start.year,
    #                                                 years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
    #                                                 rates = expr{c(0.2, 0.2, 0.5 / (1-.5) * engaged.unsuppressed.switch.to.laart)},
    #                                                 scale = 'proportion',
    #                                                 apply.function = 'absolute')
    


    #-- Create Interventions --#
    
    durable.laart.10 = create.intervention(target.population, 
                                             u.DURABLE.LAART.10)
    
    durable.laart.25 = create.intervention(target.population, 
                                           u.DURABLE.LAART.25)
    
    durable.laart.50 = create.intervention(target.population, 
                                           u.DURABLE.LAART.50)
    
    unsuppressed.laart.10 = create.intervention(target.population, 
                                             u.UNSUPPRESSED.LAART.10)
    
    unsuppressed.laart.25 = create.intervention(target.population, 
                                                u.UNSUPPRESSED.LAART.25)
    
    unsuppressed.laart.50 = create.intervention(target.population, 
                                                u.UNSUPPRESSED.LAART.50)
    
    
    INTERVENTION.MANAGER = register.intervention(durable.laart.10, code=paste0('durable.laart.10', suffix),
                                                 name='10% of durably suppressed start LAART per year',
                                                 manager = INTERVENTION.MANAGER)
    
    INTERVENTION.MANAGER = register.intervention(durable.laart.25, code=paste0('durable.laart.25', suffix),
                                                 name='25% of durably suppressed start LAART',
                                                 manager = INTERVENTION.MANAGER)
    
    INTERVENTION.MANAGER = register.intervention(durable.laart.50, code=paste0('durable.laart.50', suffix),
                                                 name='50% of durably suppressed start LAART',
                                                 manager = INTERVENTION.MANAGER)
    
    INTERVENTION.MANAGER = register.intervention(unsuppressed.laart.10, code=paste0('unsuppressed.laart.10', suffix),
                                                 name='10% of unsuppressed start LAART per year',
                                                 manager = INTERVENTION.MANAGER)
    
    INTERVENTION.MANAGER = register.intervention(unsuppressed.laart.25, code=paste0('unsuppressed.laart.25', suffix),
                                                 name='25% of unsuppressed start LAART',
                                                 manager = INTERVENTION.MANAGER)
    INTERVENTION.MANAGER = register.intervention(unsuppressed.laart.50, code=paste0('unsuppressed.laart.50', suffix),
                                                 name='50% of unsuppressed start LAART',
                                                 manager = INTERVENTION.MANAGER)
    
    
    #-- Return the intervention manager --#
    INTERVENTION.MANAGER
}

INTERVENTION.MANAGER.1.0 = create.laart.interventions()
LAART.INTERVENTION.CODES = c("noint", "durable.laart.10", "durable.laart.25", "durable.laart.50", "unsuppressed.laart.10", "unsuppressed.laart.25", "unsuppressed.laart.50")
LAART.INTERVENTIONS = lapply(LAART.INTERVENTION.CODES, intervention.from.code)
