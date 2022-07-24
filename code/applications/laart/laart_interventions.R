
create.laart.interventions <- function(target.population=WHOLE.POPULATION,
                                       
                                       start.year=2025,
                                       implemented.year=2028,
                                       
                                       suffix = '',
                                       INTERVENTION.MANAGER = INTERVENTION.MANAGER.1.0)
{
    if (suffix != '' && substr(suffix, 1,1)!='_')
        suffix = paste0("_", suffix)
    
    
    #-- Define Intervention Units --#
    u.DURABLE.LAART.05py = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                                    start.year = start.year,
                                                    years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
                                                    rates = expr{c(0.05, 0.05, 0.1 / (1-.1) * engaged.durably.suppressed.switch.to.laart)},
                                                    scale = 'proportion',
                                                    apply.function = 'absolute')
    
    u.DURABLE.LAART.10py = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                                    start.year = start.year,
                                                    years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
                                                    rates = expr{c(0.1, 0.1, 0.25 / (1-.25) * engaged.durably.suppressed.switch.to.laart)},
                                                    scale = 'proportion',
                                                    apply.function = 'absolute')
    
    u.DURABLE.LAART.20py = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                                    start.year = start.year,
                                                    years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
                                                    rates = expr{c(0.2, 0.2, 0.5 / (1-.5) * engaged.durably.suppressed.switch.to.laart)},
                                                    scale = 'proportion',
                                                    apply.function = 'absolute')

    u.UNSUPPRESSED.LAART.05py = create.intervention.unit(type = 'engaged.unsuppressed.switch.to.laart',
                                                    start.year = start.year,
                                                    years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
                                                    rates = expr{c(0.05, 0.05, 0.1 / (1-.1) * engaged.unsuppressed.switch.to.laart)},
                                                    scale = 'proportion',
                                                    apply.function = 'absolute')
    
    u.UNSUPPRESSED.LAART.10py = create.intervention.unit(type = 'engaged.unsuppressed.switch.to.laart',
                                                    start.year = start.year,
                                                    years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
                                                    rates = expr{c(0.1, 0.1, 0.25 / (1-.25) * engaged.unsuppressed.switch.to.laart)},
                                                    scale = 'proportion',
                                                    apply.function = 'absolute')
    
    u.UNSUPPRESSED.LAART.20py = create.intervention.unit(type = 'engaged.unsuppressed.switch.to.laart',
                                                    start.year = start.year,
                                                    years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
                                                    rates = expr{c(0.2, 0.2, 0.5 / (1-.5) * engaged.unsuppressed.switch.to.laart)},
                                                    scale = 'proportion',
                                                    apply.function = 'absolute')
    


    #-- Create Interventions --#
    durable.laart.05py = create.intervention(target.population, 
                                             u.DURABLE.LAART.05py)
    
    durable.laart.10py = create.intervention(target.population, 
                                             u.DURABLE.LAART.10py)
    
    durable.laart.20py = create.intervention(target.population, 
                                             u.DURABLE.LAART.20py)
    
    unsuppressed.laart.05py = create.intervention(target.population, 
                                             u.UNSUPPRESSED.LAART.05py)
    
    unsuppressed.laart.10py = create.intervention(target.population, 
                                             u.UNSUPPRESSED.LAART.10py)
    
    unsuppressed.laart.20py = create.intervention(target.population, 
                                             u.UNSUPPRESSED.LAART.20py)
    
    INTERVENTION.MANAGER = register.intervention(durable.laart.05py, code=paste0('durable.laart.05py', suffix),
                                                 name='5% of durably suppressed start LAART per year',
                                                 manager = INTERVENTION.MANAGER)
    
    INTERVENTION.MANAGER = register.intervention(durable.laart.10py, code=paste0('durable.laart.10py', suffix),
                                                 name='10% of durably suppressed start LAART per year',
                                                 manager = INTERVENTION.MANAGER)
    
    INTERVENTION.MANAGER = register.intervention(durable.laart.20py, code=paste0('durable.laart.20py', suffix),
                                                 name='20% of durably suppressed start LAART per year',
                                                 manager = INTERVENTION.MANAGER)
    
    INTERVENTION.MANAGER = register.intervention(unsuppressed.laart.05py, code=paste0('unsuppressed.laart.05py', suffix),
                                                 name='5% of unsuppressed start LAART per year',
                                                 manager = INTERVENTION.MANAGER)
    
    INTERVENTION.MANAGER = register.intervention(unsuppressed.laart.10py, code=paste0('unsuppressed.laart.10py', suffix),
                                                 name='10% of unsuppressed start LAART per year',
                                                 manager = INTERVENTION.MANAGER)
    
    INTERVENTION.MANAGER = register.intervention(unsuppressed.laart.20py, code=paste0('unsuppressed.laart.20py', suffix),
                                                 name='20% of unsuppressed start LAART per year',
                                                 manager = INTERVENTION.MANAGER)
    
    
    #-- Return the intervention manager --#
    INTERVENTION.MANAGER
}