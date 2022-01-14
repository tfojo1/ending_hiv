library(lubridate)

setClassUnion("intervention_or_null", c("intervention", "NULL"))

setOldClass('target.population')
setClass('covid_scenario',
         contains='intervention',
         representation = list(
             pandemic.effects.distribution = 'Distribution',
             target.population = 'target.population',
             mobility.weight.distribution = 'Distribution',
             
             index.to.mobility = 'logical',
             mobility.types = 'character',
             mobility.relative.to.year='numeric',
             mobility.relative.to.month='numeric',
             mobility.anchor.day='numeric',
             
             start.time = 'Date',
             onset.duration = 'numeric',
             
             effect.details = 'list',
             effect.types = 'character',
             
             next.scenario = 'intervention_or_null'
         ))


#'@param pandemic.effects.distribution A Distribution object with at least four variables: sexual.transmission.reduction, suppression.reduction, testing.reduction, prep.reduction.
#'@param sexual.transmission.reduction.distribution,suppression.reduction.distribution,testing.reduction.distribution,prep.reduction.distribution If pandemic.effects.distribution is NULL, these four independent distributions are joined to make it. Each must be a univariate distribution
create.covid.scenario <- function(pandemic.effects.distribution=NULL,
                                  target.population=WHOLE.POPULATION,
                                  
                                  mobility.weight.distribution=Beta.Distribution(2,2),
                                  
                                  sexual.transmission.reduction.distribution,
                                  suppression.reduction.distribution,
                                  testing.reduction.distribution,
                                  prep.reduction.distribution,
                                  
                                  pandemic.affects.sexual.transmission=T,
                                  pandemic.affects.suppression=T,
                                  pandemic.affects.testing=T,
                                  pandemic.affects.prep=T,
                                  
                                  index.to.mobility,
                                  mobility.types = NON.PARK.MOBILITY.TYPES,
                                  mobility.relative.to.year=2020,
                                  mobility.relative.to.month=4,
                                  mobility.anchor.day=15,
                                  
                                  start.pandemic.effects.time,
                                  pandemic.effects.onset.duration=1/365,
                                  
                                  start.sexual.transmission.normalize.time,
                                  sexual.transmission.normal.time,
                                  
                                  start.suppression.normalize.time,
                                  suppression.normal.time,
                                  start.testing.normalize.time=start.suppression.normalize.time,
                                  testing.normal.time=suppression.normal.time,
                                  start.prep.normalize.time=start.suppression.normalize.time,
                                  prep.normal.time=suppression.normal.time,
                                  
                                  check.min.time=as.Date('2020-03-01'),
                                  check.max.time=as.Date('2023-01-01'),
                                  throw.error.if.fail.check=T)
{
    #-- Check Distribution Objects --#
    
    all.var.names = c('sexual.transmission.reduction','suppression.reduction','testing.reduction','prep.reduction')
    required.var.names = all.var.names[c(pandemic.affects.sexual.transmission,
                                         pandemic.affects.suppression,
                                         pandemic.affects.testing,
                                         pandemic.affects.prep)]
    if (length(required.var.names)==0)
        stop("The pandemic must be specified to affect at least one of the following: ",
             paste0(all.var.names, collapse=', '))
    
    if (is.null(pandemic.effects.distribution))
    {
        to.join = list()
        if (!is.null(sexual.transmission.reduction.distribution))
        {
            if (!is(sexual.transmission.reduction.distribution, 'Distribution') ||
                sexual.transmission.reduction.distribution@n.var > 1)
                stop("sexual.transmission.reduction.distribution must be a univariate distribution")
            sexual.transmission.reduction.distribution@var.names = 'sexual.transmission.reduction'
            
            to.join = c(to.join, list(sexual.transmission.reduction.distribution))
        }
        else if (pandemic.affects.sexual.transmission)
            stop("sexual.transmission.reduction.distribution cannot be null. Set pandemic.affects.sexual.transmission=F to omit")
        
        if (!is.null(suppression.reduction.distribution))
        {
            if (!is(suppression.reduction.distribution, 'Distribution') ||
                suppression.reduction.distribution@n.var > 1)
                stop("suppression.reduction.distribution must be a univariate distribution")
            suppression.reduction.distribution@var.names = 'suppression.reduction'
            
            to.join = c(to.join, list(suppression.reduction.distribution))
        }
        else if (pandemic.affects.suppression)
            stop("suppression.reduction.distribution cannot be null. Set pandemic.affects.suppression=F to omit")
        
        if (!is.null(testing.reduction.distribution))
        {
            if (!is(testing.reduction.distribution, 'Distribution') ||
                testing.reduction.distribution@n.var > 1)
                stop("testing.reduction.distribution must be a univariate distribution")
            testing.reduction.distribution@var.names = 'testing.reduction'
            
            to.join = c(to.join, list(testing.reduction.distribution))
        }
        else if (pandemic.affects.testing)
            stop("testing.reduction.distribution cannot be null. Set pandemic.affects.testing=F to omit")
            
        if (!is.null(prep.reduction.distribution))
        {
            if (!is(prep.reduction.distribution, 'Distribution') ||
                prep.reduction.distribution@n.var > 1)
                stop("prep.reduction.distribution must be a univariate distribution")
            prep.reduction.distribution@var.names = 'prep.reduction'
            
            to.join = c(to.join, list(testing.reduction.distribution))
        }
        else if (pandemic.affects.prep)
            stop("testing.reduction.distribution cannot be null. Set pandemic.affects.prep=F to omit")

        pandemic.effects.distribution = join.distributions(to.join)
    }
    
    if (!is(pandemic.effects.distribution, 'Distribution') &&
        length(setdiff(required.var.names, pandemic.effects.distribution@var.names))>0)
        stop(paste0("pandemic.effects.distribution must be a multivariate distribution that contains all of the following ",
                    length(required.var.names), " variables: ",
                    paste0("'", required.var.names, "'", collapse=', ')))
    
    if (mobility.weight.distribution@n.var!=1)
        stop("mobility.weight.distribution must be a univariate distribution")
    
    mobility.weight.distribution@var.names = 'mobility.weight'
    
    #-- Check Dates --#
    
    check.covid.dates(start.pandemic.effects.time,
                      values.name.for.error='start.pandemic.effects.time',
                      is.vector.for.error = F,
                      check.min=check.min.time,
                      check.max=check.max.time,
                      throw.error.if.fail.check = throw.error.if.fail.check)
    
    check.covid.dates(start.sexual.transmission.normalize.time,
                      values.name.for.error='start.sexual.transmission.normalize.time',
                      is.vector.for.error = F,
                      check.min=check.min.time,
                      check.max=check.max.time,
                      throw.error.if.fail.check = throw.error.if.fail.check)
    check.covid.dates(sexual.transmission.normal.time,
                      values.name.for.error='sexual.transmission.normal.time',
                      is.vector.for.error = F,
                      check.min=check.min.time,
                      check.max=check.max.time,
                      throw.error.if.fail.check = throw.error.if.fail.check)
    if (sexual.transmission.normal.time<=start.sexual.transmission.normalize.time)
        stop("sexual.transmission.normal.times must be AFTER start.sexual.transmission.normalize.time")
    

    check.covid.dates(start.suppression.normalize.time,
                      values.name.for.error='start.suppression.normalize.time',
                      is.vector.for.error = F,
                      check.min=check.min.time,
                      check.max=check.max.time,
                      throw.error.if.fail.check = throw.error.if.fail.check)
    check.covid.dates(suppression.normal.time,
                      values.name.for.error='suppression.normal.time',
                      is.vector.for.error = F,
                      check.min=check.min.time,
                      check.max=check.max.time,
                      throw.error.if.fail.check = throw.error.if.fail.check)
    if (suppression.normal.time<=start.suppression.normalize.time)
        stop("suppression.normal.times must be AFTER start.suppression.normalize.time")
    
    check.covid.dates(start.testing.normalize.time,
                      values.name.for.error='start.testing.normalize.time',
                      is.vector.for.error = F,
                      check.min=check.min.time,
                      check.max=check.max.time,
                      throw.error.if.fail.check = throw.error.if.fail.check)
    check.covid.dates(testing.normal.time,
                      values.name.for.error='testing.normal.time',
                      is.vector.for.error = F,
                      check.min=check.min.time,
                      check.max=check.max.time,
                      throw.error.if.fail.check = throw.error.if.fail.check)
    if (testing.normal.time<=start.testing.normalize.time)
        stop("testing.normal.times must be AFTER start.testing.normalize.time")
    
    check.covid.dates(start.prep.normalize.time,
                      values.name.for.error='start.prep.normalize.time',
                      is.vector.for.error = F,
                      check.min=check.min.time,
                      check.max=check.max.time,
                      throw.error.if.fail.check = throw.error.if.fail.check)
    check.covid.dates(prep.normal.time,
                      values.name.for.error='prep.normal.time',
                      is.vector.for.error = F,
                      check.min=check.min.time,
                      check.max=check.max.time,
                      throw.error.if.fail.check = throw.error.if.fail.check)
    if (prep.normal.time<=start.prep.normalize.time)
        stop("prep.normal.times must be AFTER start.prep.normalize.time")
    
    
    #-- Package it up and return --#
    
    make.details.object = function(type, start.normalize.time, normal.time){
        list(
            type = type,
            start.normalize.time=start.normalize.time,
           # normalize.span=normalize.span,
            normal.time=normal.time#start.normalize.time %m+% months(normalize.span)
        )
    }
    
    effect.details = list(
        make.details.object(
            type = 'sexual.transmission',
            start.normalize.time = start.sexual.transmission.normalize.time,
            normal.time = sexual.transmission.normal.time
        ),
        make.details.object(
            type = 'suppression',
            start.normalize.time = start.suppression.normalize.time,
            normal.time = suppression.normal.time
        ),
        make.details.object(
            type = 'testing',
            start.normalize.time = start.testing.normalize.time,
            normal.time = testing.normal.time
        ),
        make.details.object(
            type = 'prep',
            start.normalize.time = start.prep.normalize.time,
            normal.time = prep.normal.time
        ))[c(pandemic.affects.sexual.transmission,
             pandemic.affects.suppression,
             pandemic.affects.testing,
             pandemic.affects.prep)]
    
    
    effect.types = sapply(effect.details, function(d){d$type})
    names(effect.details) = effect.types
    
    rv = new('covid_scenario', 
             type='covid',
             pandemic.effects.distribution = pandemic.effects.distribution,
             target.population = target.population,
             mobility.weight.distribution = mobility.weight.distribution,
                          
             index.to.mobility = T,
             mobility.types = mobility.types,
             mobility.relative.to.year=mobility.relative.to.year,
             mobility.relative.to.month=mobility.relative.to.month,
             mobility.anchor.day=mobility.anchor.day,
             
             start.time = start.pandemic.effects.time,
             onset.duration = pandemic.effects.onset.duration,
             
             effect.details = effect.details,
             effect.types = effect.types,
             
             next.scenario=NULL
    )
    
    rv
}

##-----------------------##
##-- METHODS FOR CLASS --##
##-----------------------##

setMethod('interventions.equal',
          signature = 'covid_scenario',
def = function(int1, int2)
{
    if (!is(int2, 'covid_scenario'))
        return (F)
    
    equal = 
        int1@index.to.mobility == int2@index.to.mobility &&
        setequal(int1@mobility.types, int2@mobility.types) &&
        int1@mobility.relative.to.year == int2@mobility.relative.to.year &&
        int1@mobility.relative.to.month == int2@mobility.relative.to.month &&
        int1@mobility.anchor.day == int2@mobility.anchor.day &&
        int1@start.time == int2@start.time &&
        int1@onset.duration == int2@onset.duration &&
        setequal(int1@effect.types, int2@effect.types) &&
        target.populations.equal(int1@target.population, int2@target.population)
    
    if (!equal)
        return (F)
    
    details.equal = sapply(names(int1@effect.details), function(type){
        all(sapply(1:length(int1@effect.details[[type]]), function(i){
            all(int1@effect.details[[type]][[i]] == int2@effect.details[[type]][[i]])
        }))
    })
    if (!all(details.equal))
        return (F)
    
    distributions.equal(int1@pandemic.effects.distribution, int2@pandemic.effects.distribution) &&
        distributions.equal(int1@mobility.weight.distribution, int2@mobility.weight.distribution)
})

setMethod('do.join.interventions',
          signature = 'standard_intervention',
def = function(int1, int2)
{
    if (!is(int2, 'covid_scenario'))
      stop("Both interventions must be of class 'covid_scenario'")
    
    if (is.null(int1@next.scenario))
        int1@next.scenario = int2
    else
        int1@next.scenario = do.join.interventions(int1@next.scenario, int2)
    
    int1
})

##--------------------------------------##
##-- CASTING TO STANDARD INTERVENTION --##
##--------------------------------------##

setMethod('prepare.intervention.for.simset',
          signature='covid_scenario',
def = function(intervention, simset)
{
    cast.covid.intervention.for.location(intervention,
                                         location = attr(simset@simulations[[1]], 'location'))    
})

cast.covid.intervention.for.location <- function(intervention,
                                                 location,
                                                 mdm=MOBILITY.DATA.MANAGER)
{
    if (!is(intervention, 'covid_scenario'))
        stop("intervention must be an object of class 'covid_scenario'")
    
    if (intervention@index.to.mobility)
    {
        #-- Calculate from and to times --#
        from.year = as.numeric(format(intervention@start.time, '%Y'))
        from.month = as.numeric(format(intervention@start.time, '%m'))
        
        to.years = sapply(intervention@effect.details, function(timing){
            as.numeric(format(timing$normal.time, '%Y'))
        })
        to.months = sapply(intervention@effect.details, function(timing){
            as.numeric(format(timing$normal.time, '%m'))
        })
        
        desired.to.year = max(to.years)
        to.year = min(mdm$max.year, desired.to.year)
        if (to.year<desired.to.year)
            to.month = min(max(mdm$months[mdm$years==to.year]),
                           12)
        else
            to.month = min(max(mdm$months[mdm$years==to.year]),
                           max(to.months[to.years==to.year]))
    
        #-- Interpolate times we're going to get back --#
        
        if (from.year==to.year)
        {
            months = from.month:to.month
            years = rep(from.year, length(months))
        }
        else
        {
            unique.years = from.year:to.year
            
            months = c(
                from.month:12,
                rep(1:12, length(unique.years)-2),
                1:to.month
            )
            
            years = c(
                rep(from.year,12-from.month+1),
                rep(unique.years[-c(1,length(unique.years))], each=12),
                rep(to.year, to.month)
            )
        }
        
        relative.to.mask = months == intervention@mobility.relative.to.month & years == intervention@mobility.relative.to.year
        if (!any(relative.to.mask))
            stop(paste0("The mobility data do not contain the relative.to month (", 
                        intervention@mobility.relative.to.month, "-",
                        intervention@mobility.relative.to.year, ")"))
        
        #-- Pull the mobility data --#
        mobility.deltas = get.mobility.data.by.month(mdm, locations=location,
                                                     from.year=from.year,
                                                     from.month=from.month,
                                                     to.year=to.year,
                                                     to.month=to.month,
                                                     types=intervention@mobility.types)
        
        mobility.deltas = colMeans(mobility.deltas / mobility.deltas[,relative.to.mask])
        mobility.times = as.Date(paste0(years, '-', months, '-', intervention@mobility.anchor.day))
    }
    else
        mobility.deltas = mobility.times = NULL
    
    
    
    #-- Make the Intervention Units --#
    units = list()
    for (details in intervention@effect.details)
        units = c(units, 
                  cast.covid.intervention.unit(type = details$type,
                                               start.time = intervention@start.time,
                                               onset.duration = intervention@onset.duration,
                                               
                                               start.normalize.time = details$start.normalize.time,
                                               normal.time = details$normal.time,
                                               
                                               mobility.times = mobility.times,
                                               mobility.deltas = mobility.deltas
                  ))
    
    #-- Put it together --#
    rv = create.intervention(
        intervention@target.population,
        units,
        intervention@pandemic.effects.distribution,
        intervention@mobility.weight.distribution
    )
    
    if (is.null(intervention@next.scenario))
        rv
    else
        join.interventions(rv, 
                           cast.covid.intervention.for.location(intervention@next.scenario,
                                                                location=location,
                                                                mdm=mdm))
}

cast.covid.intervention.unit <- function(type,
                                         effect.name=paste0(type, '.reduction'),
                                         start.time,
                                         onset.duration,
                                         start.normalize.time,
                                         normal.time,
                                         mobility.times,
                                         mobility.deltas,
                                         effect.is.reduction=T,
                                         mobility.weight.name='mobility.weight')
{
    #-- Cast Times --#
    start.time = date.to.numeric.year(start.time)
    start.normalize.time = date.to.numeric.year(start.normalize.time)
    normal.time = date.to.numeric.year(normal.time)
    
    
    if (is.null(mobility.deltas))
    {
        create.intervention.unit(type=type,
                                 start.year=start.time,
                                 rates=parse(text=paste0("1-c(",effect.name,", ",effect.name,", 0)")),
                                 years=c(start.time + onset.duration,
                                         start.normalize.time,
                                         normal.time),
                                 end.year = normal.time+.0001,
                                 apply.function = 'multiplier',
                                 allow.less.than.otherwise = T)
    }
    else
    {
        mobility.times = date.to.numeric.year(mobility.times)
        
        merged = merge.rates(rates1 = mobility.deltas,
                             times1 = mobility.times,
                             rates2 = c(1,1,0),
                             times2 = c(start.time+onset.duration,
                                        start.normalize.time,
                                        normal.time))

        mask = merged$times <= normal.time
        times = merged$times[mask]
                
        mobility.multipliers = unlist(merged$rates1[mask])
        taper.multipliers = unlist(merged$rates2[mask])
        
     #   rates = function(parameters) {
     #       mobility.weight = parameters[mobility.weight.name]
     #       1 - parameters[effect.name] * (mobility.weight * mobility.multipliers + (1-mobility.weight)) * taper.multipliers
     #   }
        
        #evaluates to this expression:
        #   1 + <effect> * (<mobility.weight> * mobility.multipliers + (1-<mobility.weight>)) * taper.multiplier
        # = 1 + <effect> * (<mobility.weight> * (mobility.multipliers-1) + 1) * taper.multiplier
        expr.text = paste0("1 - ", effect.name, 
                           " * (", mobility.weight.name,
                           " * (c(", paste0(mobility.multipliers, collapse=','), 
                           ")-1) + 1) * c(", paste0(taper.multipliers, collapse=','), ")")
        rates = parse(text=expr.text)
        
        if (type == 'sexual.transmission')
            types.for.unit = c('heterosexual.transmission', 'msm.transmission')
        else
            types.for.unit = type
        
        lapply(types.for.unit,
               create.intervention.unit,
               start.year=start.time,
               rates=rates,
               years=times,
               end.year = normal.time+.0001,
               apply.function = 'multiplier',
               allow.less.than.otherwise = T)
    }
}

##------------------##
##-- DATE HELPERS --##
##------------------##

check.covid.dates <- function(values,
                                 check.min,
                                 check.max,
                                 throw.error.if.fail.check,
                                 values.name.for.error='values',
                                 is.vector.for.error=T)
{
    if (!is(values, 'Date'))
    {
        stop(paste0("'", values.name.for.error, "' must be a Date",
                ifelse(is.vector.for.error, ' vector', '')))
    }
    
    if (any(values < check.min) || any(values > check.max))
    {
        if (is.vector.for.error)
            error.msg = paste0("Some elements of '", values.name.for.error, 
                               "' are prior to ", check.min,
                               " or after ", check.max)
        else
            error.msg = paste0("'", values.name.for.error, 
                               "' is prior to ", check.min,
                               " or after ", check.max)
        
        if (throw.error.if.fail.check)
            stop(error.msg)
        else
            print(paste0("WARNING: ", error.msg))
        
        F
    }
    else
        T
    
}

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
