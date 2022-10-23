
create.laart.interventions <- function(target.population=WHOLE.POPULATION,
                                       
                                       start.year=2025,
                                       implemented.year=2028,
                                       
                                       suffix = '',
                                       INTERVENTION.MANAGER = INTERVENTION.MANAGER.1.0)
{
    if (suffix != '' && substr(suffix, 1,1)!='_')
        suffix = paste0("_", suffix)
    
    #--Optimization Function--#
    min.loss <-function(param, percentage, start.year, implemented.year){
      start.year = start.year
      implemented.year = implemented.year
      switch.coefficient = param[1]
      discontinuation.coefficient = param[2]
      rate = -log(1-percentage)/(implemented.year-start.year)
      rate = rate * switch.coefficient
      u.DURABLE.LAART = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                                    start.year = start.year,
                                                    years = c(start.year+0.0001, implemented.year, implemented.year+0.001),#engaged.durably.suppressed.switch.to.laart
                                                    rates = expression(c(rate, rate, discontinuation.coefficient* (percentage / (1-percentage)) *laart.discontinuation)),
                                                    scale = 'rate',
                                                    apply.function = 'absolute',
                                                    expression.parameters = list(rate=rate, discontinuation.coefficient = discontinuation.coefficient ))
      intervention.to.test = create.intervention(WHOLE.POPULATION, 
                                                 u.DURABLE.LAART)
      source('code/core_code/interventions/intervention_defaults.R')
      
      INTERVENTION.MANAGER.1.0 = register.intervention(intervention.to.test, code=paste0('test.intervention'),
                                                       name='test',
                                                       manager = INTERVENTION.MANAGER.1.0, allow.intervention.multiple.names = T)
      
      #-- RUN AND EXPLORE THE INTERVENTION --#
      projected = run.simset.intervention(prepared, 
                                          intervention=intervention.to.test,
                                          run.to.year=2035,
                                          keep.years=2015:2035)
      
      columns = c("implemented_year_laart_percentage","2035_laart_percentage") 
      df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
      colnames(df) = columns
      for (i in 1:projected@n.sim){
        df[nrow(df) + 1,] = c(round(100*t(sapply(as.character(2024:2035), function(year){
          rowMeans(sapply(projected@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
        })),1)[toString(implemented.year-1), 'laart'], round(100*t(sapply(as.character(2024:2035), function(year){
          rowMeans(sapply(projected@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
        })),1)['2035', 'laart'])
      }
      
      loss = sum((df-50)^2)/projected@n.sim
      return(loss)
    }
    #-- Define Intervention Units --#
    
    optim.output<- optim(par = c(2.5110, 8.7675), fn = min.loss, percentage = 0.1, start.year = 2025, implemented.year = 2028, control = list(maxit = 10))
    switch.coefficient = optim.output$par[1]
    discontinuation.coefficient = optim.output$par[2]
    rate.10 = -log(1-0.1)/(implemented.year-start.year)
    rate.10 = rate.10 * switch.coefficient
    u.DURABLE.LAART.10 = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                                  start.year = start.year,
                                                  years = c(start.year+0.0001, implemented.year, implemented.year+0.001),#engaged.durably.suppressed.switch.to.laart
                                                  rates = expression(c(rate.10, rate.10, (0.1 / (1-.1)) * laart.discontinuation * discontinuation.coefficient)),
                                                  scale = 'rate',
                                                  apply.function = 'absolute',
                                                  expression.parameters = list(rate.10=rate.10))
    optim.output<- optim(par = c(2.5110, 8.7675), fn = min.loss, percentage = 0.25, start.year = 2025, implemented.year = 2028, control = list(maxit = 10))
    switch.coefficient = optim.output$par[1]
    discontinuation.coefficient = optim.output$par[2]
    rate.25 = -log(1-0.25)/(implemented.year-start.year)
    rate.25 = rate.25 * switch.coefficient
    u.DURABLE.LAART.25 = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                                  start.year = start.year,
                                                  years = c(start.year+0.0001, implemented.year, implemented.year+0.001),#engaged.durably.suppressed.switch.to.laart
                                                  rates = expression(c(rate.25, rate.25, (0.25 / (1-.25)) * laart.discontinuation * discontinuation.coefficient)),
                                                  scale = 'rate',
                                                  apply.function = 'absolute',
                                                  expression.parameters = list(rate.25=rate.25))
    optim.output<- optim(par = c(2.5110, 8.7675), fn = min.loss, percentage = 0.50, start.year = 2025, implemented.year = 2028, control = list(maxit = 10))
    switch.coefficient = optim.output$par[1]
    discontinuation.coefficient = optim.output$par[2]
    rate.50 = -log(1-0.50)/(implemented.year-start.year)
    rate.50 = rate.50 * switch.coefficient
    u.DURABLE.LAART.50 = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                                  start.year = start.year,
                                                  years = c(start.year+0.0001, implemented.year, implemented.year+0.001),#engaged.durably.suppressed.switch.to.laart
                                                  rates = expression(c(rate.50, rate.50, (0.50 / (1-.50)) * laart.discontinuation * discontinuation.coefficient)),
                                                  scale = 'rate',
                                                  apply.function = 'absolute',
                                                  expression.parameters = list(rate.50=rate.50))
    
    rate.10 = -log(1-0.1)/(implemented.year-start.year)
    u.UNSUPPRESSED.LAART.10 = create.intervention.unit(type = 'engaged.unsuppressed.switch.to.laart',
                                                       start.year = start.year,
                                                       years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
                                                       rates = expression(c(rate.10, rate.10, 0.10 / (1-.10) * laart.discontinuation)),
                                                       scale = 'rate',
                                                       apply.function = 'absolute',
                                                       expression.parameters = list(rate.10=rate.10)) 
    rate.25 = -log(1-0.25)/(implemented.year-start.year)
    u.UNSUPPRESSED.LAART.25 = create.intervention.unit(type = 'engaged.unsuppressed.switch.to.laart',
                                                  start.year = start.year,
                                                  years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
                                                  rates = expression(c(rate.25, rate.25, 0.25 / (1-.25) * laart.discontinuation)),
                                                  scale = 'rate',
                                                  apply.function = 'absolute',
                                                  expression.parameters = list(rate.25=rate.25))
    rate.50 = -log(1-0.50)/(implemented.year-start.year)
    u.UNSUPPRESSED.LAART.50 = create.intervention.unit(type = 'engaged.unsuppressed.switch.to.laart',
                                                  start.year = start.year,
                                                  years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
                                                  rates = expression(c(rate.50, rate.50, 0.50 / (1-.50) * laart.discontinuation)),
                                                  scale = 'rate',
                                                  apply.function = 'absolute',
                                                  expression.parameters = list(rate.50=rate.50))
    


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
