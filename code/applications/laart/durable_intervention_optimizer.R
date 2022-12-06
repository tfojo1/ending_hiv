
get.durable.25.intervention.parameters <- function(target.population=WHOLE.POPULATION,
                                       optim.simset = optim.simset,
                                       start.year=2025,
                                       implemented.year=2028,
                                       initial.parameter = c(1.829965, 4.273775),
                                       suffix = '',
                                       INTERVENTION.MANAGER = INTERVENTION.MANAGER.1.0)
{
    if (suffix != '' && substr(suffix, 1,1)!='_')
        suffix = paste0("_", suffix)
    
    #--Optimization Function--#
    min.loss <-function(param, start.year, implemented.year){
      start.year = start.year
      implemented.year = implemented.year
      switch.coefficient = param[1]
      discontinuation.coefficient = param[2]
      rate = -log(1-0.25)/(implemented.year-start.year)
      rate = rate * switch.coefficient
      u.DURABLE.LAART = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                                    start.year = start.year,
                                                    years = c(start.year+0.0001, implemented.year, implemented.year+0.001),#engaged.durably.suppressed.switch.to.laart
                                                    rates = expression(c(rate, rate, discontinuation.coefficient* (.25 / (1-.25)) *laart.discontinuation)),
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
      projected = run.simset.intervention(simset = optim.simset, 
                                          intervention=intervention.to.test,
                                          run.to.year=2035,
                                          keep.years=2015:2035)
      columns = c("implemented_year_laart_percentage","2035_laart_percentage") 
      df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
      colnames(df) = columns
      for (i in 1:projected@n.sim){
        df[nrow(df) + 1,] = c(round(100*t(sapply(as.character((start.year-1):2035), function(year){
          rowMeans(sapply(projected@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
        })),1)[toString(implemented.year-1), 'laart'], round(100*t(sapply(as.character((start.year-1):2035), function(year){
          rowMeans(sapply(projected@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
        })),1)['2035', 'laart'])
      }
      
      loss = sum((df-25)^2)/projected@n.sim
      return(loss)
    }
    #-- Define Intervention Units --#
    
    optim.output<- optim(par = initial.parameter, fn = min.loss, start.year = start.year, implemented.year = implemented.year, control = list(maxit = 10))
    return(optim.output)
}

get.durable.50.intervention.parameters <- function(target.population=WHOLE.POPULATION,
                                                   optim.simset = optim.simset,
                                                   start.year=2025,
                                                   implemented.year=2028,
                                                   initial.parameter = c(2.27722, 5.069739),
                                                   suffix = '',
                                                   INTERVENTION.MANAGER = INTERVENTION.MANAGER.1.0)
{
  if (suffix != '' && substr(suffix, 1,1)!='_')
    suffix = paste0("_", suffix)
  
  #--Optimization Function--#
  min.loss <-function(param, start.year, implemented.year){
    start.year = start.year
    implemented.year = implemented.year
    switch.coefficient = param[1]
    discontinuation.coefficient = param[2]
    rate = -log(1-0.50)/(implemented.year-start.year)
    rate = rate * switch.coefficient
    u.DURABLE.LAART = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                               start.year = start.year,
                                               years = c(start.year+0.0001, implemented.year, implemented.year+0.001),#engaged.durably.suppressed.switch.to.laart
                                               rates = expression(c(rate, rate, discontinuation.coefficient* (.5 / (1-.5)) *laart.discontinuation)),
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

    projected = run.simset.intervention(simset = optim.simset, 
                                        intervention=intervention.to.test,
                                        run.to.year=2035,
                                        keep.years=2015:2035)
    
    columns = c("implemented_year_laart_percentage","2035_laart_percentage") 
    df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
    colnames(df) = columns
    for (i in 1:projected@n.sim){
      df[nrow(df) + 1,] = c(round(100*t(sapply(as.character((start.year-1):2035), function(year){
        rowMeans(sapply(projected@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
      })),1)[toString(implemented.year-1), 'laart'], round(100*t(sapply(as.character((start.year-1):2035), function(year){
        rowMeans(sapply(projected@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
      })),1)['2035', 'laart'])
    }
    
    loss = sum((df-50)^2)/projected@n.sim
    return(loss)
  }
  #-- Define Intervention Units --#
  
  optim.output<- optim(par = initial.parameter, fn = min.loss, start.year = start.year, implemented.year = implemented.year, control = list(maxit = 10))
  return(optim.output)
}