
create.laart.interventions <- function(target.population=WHOLE.POPULATION,
                                       
                                       start.year=2025,
                                       implemented.year=2028,
                                       
                                       suffix = '',
                                       INTERVENTION.MANAGER = INTERVENTION.MANAGER.1.0)
{
    if (suffix != '' && substr(suffix, 1,1)!='_')
        suffix = paste0("_", suffix)
    
    
    #-- Define Intervention Units --#
    
    u.DURABLE.LAART.10py = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                                    start.year = start.year,
                                                    years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
                                                    rates = expr{c(0.1, 0.1, 0.25 / (1-.25) * laart.durably.suppressed.to.engaged.durably.suppressed)},
                                                    scale = 'proportion',
                                                    apply.function = 'absolute')


    #-- Create Interventions --#
    
    durable.laart.10py = create.intervention(target.population, 
                                             u.DURABLE.LAART.10py)
    
    INTERVENTION.MANAGER = register.intervention(durable.laart.10py, code=paste0('durable.laart.10py', suffix),
                                                 name='10% of durably suppressed start LAART per year',
                                                 manager = INTERVENTION.MANAGER)
    
    
    #-- Return the intervention manager --#
    INTERVENTION.MANAGER
}