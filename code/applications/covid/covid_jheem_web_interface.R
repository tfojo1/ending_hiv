
##-----------------##
##-- RUN SIMSETS --##
##-----------------##

run.covid.simset.intervention <- function(simset, 
                                          intervention, 
                                          run.from.year,
                                          run.to.year,
                                          keep.years,
                                          update.progress,
                                          parameter.names=c('sexual.transmission.reduction',
                                                            'suppression.reduction',
                                                            'testing.reduction',
                                                            'prep.reduction'))
{
    # prepare parameters
    simset = prepare.covid.simset.parameters.for.intervention(simset,
                                                              covid.intervention=intervention,
                                                              parameter.names=parameter.names)
    # Run Simulations
    simset@simulations = lapply(1:simset@n.sim, function(i){
        if (!is.null(update.progress))
            update.progress(i)
        
        int = map.covid.intervention.to.jheem.intervention(covid.intervention = intervention,
                                                           parameters = simset@parameters[i,],
                                                           parameter.names = parameter.names)
        
        run.sim.intervention(simset@simulations[[i]], int,
                             run.from.year=run.from.year, run.to.year = run.to.year,
                             keep.years = keep.years)
    })
    
    # Return
    simset
}

##-------------------------------##
##-- PREPARE SIMSET PARAMETERS --##
##-------------------------------##

prepare.covid.simset.parameters.for.intervention <- function(simset,
                                                             covid.intervention,
                                                             parameter.names)
{
    for (i in covid.intervention$n.subpopulations)
    {
        sub.unit = covid.intervention$sub.units[[i]]
        if (i == 1)
            suffix = ''
        else
            suffix = paste0('.', i)
        
        for (parameter.name in parameter.names)
        {
            sub.param = sub.unit$parameters[[parameter.name]]
            if (!is.null(sub.param) && sub.param$use)
            {
                effect.range = sub.param$effect
                values = runif(simset@n.sim, effect.range[1], effect.range[2])
                
                simset = add.parameters(simset, 
                                        parameters = values,
                                        parameter.names = paste0(parameter.name, suffix),
                                        parameter.lower.bounds = effect.range[1],
                                        parameter.upper.bounds = effect.range[2])
            }
        }
    }
    
    simset
}

##------------------##
##-- INTERVENTION --##
##------------------##

COVID.INTEVENTION.EFFECT.ONSET.TIME = 1/365
map.covid.intervention.to.jheem.intervention <- function(covid.intervention,
                                                         parameters,
                                                         parameter.names=c('sexual.transmission.reduction',
                                                                           'suppression.reduction',
                                                                           'testing.reduction',
                                                                           'prep.reduction'))
{
    sub.interventions = lapply(covid.intervention$n.subpopulations, function(i){
        sub.pop = covid.intervention$sub.populations[[i]]
        sub.unit = covid.intervention$sub.units[[i]]
        if (i == 1)
            suffix = ''
        else
            suffix = paste0('.', i)
        
        intervention.units = lapply(parameter.names, map.covid.intervention.to.jheem.intervention.unit,
                                    covid.intervention = covid.intervention,
                                    parameter.values = parameters,
                                    i=i)
        
        intervention.units = intervention.units[!sapply(intervention.units, is.null)]
        
        create.intervention(sub.pop, intervention.units)
    })
    
    join.interventions(sub.interventions)
}

map.covid.intervention.to.jheem.intervention.unit <- function(covid.intervention,
                                                              parameter.name,
                                                              parameter.values,
                                                              i)
{
    sub.unit = covid.intervention$sub.units[[i]]
    if (i == 1)
        suffix = ''
    else
        suffix = paste0('.', i)
    
    sub.param = sub.unit$parameters[[parameter.name]]
    if (is.null(sub.param) || !sub.param$use)
        NULL
    else
    {
        start.year = date.to.numeric.year(sub.unit$start.time)
        rr = 1 - parameter.values[paste0(parameter.name, suffix)]
        start.normalize.year = date.to.numeric.year(sub.param$start.normalize.time)
        normal.year = start.normalize.year + sub.param$normalize.span/12
        if (normal.year==start.normalize.year)
            normal.year = start.normalize.year + 0.0001
        
        create.intervention.unit('suppression', 
                                 start.year=start.year, 
                                 end.year=normal.year, 
                                 rates=c(rr, rr), 
                                 years=c(start.year + COVID.INTEVENTION.EFFECT.ONSET.TIME,
                                         start.normalize.year),
                                 apply.function = 'multiplier',
                                 allow.less.than.otherwise = T)
    }
}


##-------------##
##-- HELPERS --##
##-------------##

# converts a date object to a number such that, eg,
# 2021 corresponds to jan 1, 2021,
# 2021.04986301 corresponds to the 182nd day of 2021
date.to.numeric.year <- function(date)
{
    if (is(date, 'character'))
        date = as.Date(date)
    
    if (is(date, 'Date'))
    {
        this.year.num = as.numeric(format(date, format="%Y"))
        jan.1.this.year = as.Date(paste0(this.year.num, '-01-01'))
        jan.1.next.year = as.Date(paste0(this.year.num+1, '-01-01'))
        
        frac.of.this.year = (as.numeric(date) - as.numeric(jan.1.this.year))/
            (as.numeric(jan.1.next.year) - as.numeric(jan.1.this.year))
        
        this.year.num + frac.of.this.year
    }
    else if (is(date, 'numeric') || is(date, 'integer'))
        date
    else
        stop("'date' must be a Date object, a character that can be converted to a Date object (or a numeric or integer)")
}
