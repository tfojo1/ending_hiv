library(lubridate)

if (1==2)
{
    intervention = create.covid.scenario(sexual.transmission.reduction.distribution = Uniform.Distribution(0,0.5),
                                suppression.reduction.distribution = Uniform.Distribution(0,0.4),
                                testing.reduction.distribution = Uniform.Distribution(0,0.5),
                                prep.reduction.distribution = Uniform.Distribution(0,0.3),
                                
                                index.to.mobility = T,
                                
                                start.pandemic.effects.time = as.Date('2020-03-01'),
                                start.sexual.transmission.normalize.time = as.Date('2021-03-08'),
                                sexual.transmission.normalize.duration.months = 4,
                                
                                start.suppression.normalize.time = as.Date('2021-09-08'),
                                suppression.normalize.duration.months = 4
                                )
}


#'@param pandemic.effects.distribution A Distribution object with at least four variables: sexual.transmission.reduction, suppression.reduction, testing.reduction, prep.reduction.
#'@param sexual.transmission.reduction.distribution,suppression.reduction.distribution,testing.reduction.distribution,prep.reduction.distribution If pandemic.effects.distribution is NULL, these four independent distributions are joined to make it. Each must be a univariate distribution
create.covid.scenario <- function(pandemic.effects.distribution=NULL,
                                      sexual.transmission.reduction.distribution,
                                      suppression.reduction.distribution,
                                      testing.reduction.distribution,
                                      prep.reduction.distribution,
                                      
                                      index.to.mobility,
                                  mobility.types = NON.PARK.MOBILITY.TYPES,
                                  mobility.relative.to.year=2020,
                                  mobility.relative.to.month=4,
                                  mobility.anchor.day=15,
                                      
                                      start.pandemic.effects.time,
                                      
                                      start.sexual.transmission.normalize.time,
                                      sexual.transmission.normalize.duration.months,
                                      
                                      start.suppression.normalize.time,
                                      suppression.normalize.duration.months,
                                      start.testing.normalize.time=start.suppression.normalize.time,
                                      testing.normalize.duration.months=suppression.normalize.duration.months,
                                      start.prep.normalize.time=start.suppression.normalize.time,
                                      prep.normalize.duration.months=suppression.normalize.duration.months,
                                      
                                      check.min.time=as.Date('2020-03-01'),
                                      check.max.time=as.Date('2023-01-01'),
                                      throw.error.if.fail.check=T)
{
    #-- Check Distribution Objects --#
    if (is.null(pandemic.effects.distribution))
    {
        if (!is(sexual.transmission.reduction.distribution, 'Distribution') ||
            sexual.transmission.reduction.distribution@n.var > 1)
            stop("sexual.transmission.reduction.distribution must be a univariate distribution")
        if (!is(suppression.reduction.distribution, 'Distribution') ||
            suppression.reduction.distribution@n.var > 1)
            stop("suppression.reduction.distribution must be a univariate distribution")
        if (!is(testing.reduction.distribution, 'Distribution') ||
            testing.reduction.distribution@n.var > 1)
            stop("testing.reduction.distribution must be a univariate distribution")
        if (!is(prep.reduction.distribution, 'Distribution') ||
            prep.reduction.distribution@n.var > 1)
            stop("prep.reduction.distribution must be a univariate distribution")
        
        sexual.transmission.reduction.distribution@var.names = 'sexual.transmission.reduction'
        suppression.reduction.distribution@var.names = 'suppression.reduction'
        testing.reduction.distribution@var.names = 'testing.reduction'
        prep.reduction.distribution@var.names = 'prep.reduction'
        
        pandemic.effects.distribution = join.distributions(sexual.transmission.reduction.distribution,
                                                           suppression.reduction.distribution,
                                                           testing.reduction.distribution,
                                                           prep.reduction.distribution)
    }
    
    required.var.names = c('sexual.transmission.reduction','suppression.reduction','testing.reduction','prep.reduction')
    if (!is(pandemic.effects.distribution, 'Distribution') &&
        length(setdiff(required.var.names, pandemic.effects.distribution@var.names))>0)
        stop(paste0("pandemic.effects.distribution must be a multivariate distribution that contains the following ",
                    length(required.var.names), " variables: ",
                    paste0("'", required.var.names, "'", collapse=', ')))
    
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
    if ((!is.numeric(sexual.transmission.normalize.duration.months) &&
         !is.integer(sexual.transmission.normalize.duration.months)) ||
         sexual.transmission.normalize.duration.months<=0)
        stop("'sexual.transmission.normalize.duration.months' must be a positive number")
    

    check.covid.dates(start.suppression.normalize.time,
                      values.name.for.error='start.suppression.normalize.time',
                      is.vector.for.error = F,
                      check.min=check.min.time,
                      check.max=check.max.time,
                      throw.error.if.fail.check = throw.error.if.fail.check)
    if ((!is.numeric(suppression.normalize.duration.months) &&
         !is.integer(suppression.normalize.duration.months)) ||
        suppression.normalize.duration.months<=0)
        stop("'suppression.normalize.duration.months' must be a positive number")
    
    check.covid.dates(start.testing.normalize.time,
                      values.name.for.error='start.testing.normalize.time',
                      is.vector.for.error = F,
                      check.min=check.min.time,
                      check.max=check.max.time,
                      throw.error.if.fail.check = throw.error.if.fail.check)
    if ((!is.numeric(testing.normalize.duration.months) &&
         !is.integer(testing.normalize.duration.months)) ||
        testing.normalize.duration.months<=0)
        stop("'testing.normalize.duration.months' must be a positive number")
    
    check.covid.dates(start.prep.normalize.time,
                      values.name.for.error='start.prep.normalize.time',
                      is.vector.for.error = F,
                      check.min=check.min.time,
                      check.max=check.max.time,
                      throw.error.if.fail.check = throw.error.if.fail.check)
    if ((!is.numeric(prep.normalize.duration.months) &&
         !is.integer(prep.normalize.duration.months)) ||
        prep.normalize.duration.months<=0)
        stop("'prep.normalize.duration.months' must be a positive number")
    
    
    #-- Package it up and return --#
    
    make.timing.object = function(start.normalize.time, normalize.span){
        list(
            start.normalize.time=start.normalize.time,
            normalize.span=normalize.span,
            normal.time=start.normalize.time %m+% months(normalize.span)
        )
    }
    
    rv = list(
        pandemic.effects.distribution = pandemic.effects.distribution,
        
        index.to.mobility = T,
        mobility.types = mobility.types,
        mobility.relative.to.year=mobility.relative.to.year,
        mobility.relative.to.month=mobility.relative.to.month,
        mobility.anchor.day=mobility.anchor.day,
        
        start.time = start.pandemic.effects.time,
        
        effect.timing = list(
            sexual.transmission = make.timing.object(
                start.normalize.time = start.sexual.transmission.normalize.time,
                normalize.span = sexual.transmission.normalize.duration.months
            ),
            suppression = make.timing.object(
                start.normalize.time = start.suppression.normalize.time,
                normalize.span = suppression.normalize.duration.months
            ),
            testing = make.timing.object(
                start.normalize.time = start.testing.normalize.time,
                normalize.span = testing.normalize.duration.months
            ),
            prep = make.timing.object(
                start.normalize.time = start.prep.normalize.time,
                normalize.span = prep.normalize.duration.months
            ))
    )
    
    rv$effect.types = names(rv$effect.timing)
    
    class(rv) = 'intervention'
    
    rv
}

cast.covid.intervention.for.location <- function(intervention,
                                                 location,
                                                 mdm=MOBILITY.DATA.MANAGER)
{
    #-- Calculate from and to times --#
    from.year = as.numeric(format(intervention$start.time, '%Y'))
    from.month = as.numeric(format(intervention$start.time, '%m'))
    
    to.years = sapply(intervention$effect.timing, function(timing){
        as.numeric(format(timing$normal.time, '%Y'))
    })
    to.months = sapply(intervention$effect.timing, function(timing){
        as.numeric(format(timing$normal.time, '%m'))
    })
    
    desired.to.year = max(to.years)
    to.year = min(mdm$max.year, desired.to.year)
    if (to.year<desired.to.year)
        to.month = min(max(mdm$months[mdm$years==to.year]),
                       12)
    else
        to.month = min(max(mdm$months[mdm$years==to.year]),
                       max(to.months[to.years==to.month]))

    #-- Interpolate times we're going to get back --#
    
    if (from.year==to.year)
    {
        months = from.month:to.month
        years = rep(from.year, length(months))
    }
    else
    {
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
    
    relative.to.mask = months == intervention$mobility.relative.to.month & years == intervention$mobility.relative.to.year
    if (!any(relative.to.mask))
        stop(paste0("The mobility data do not contain the relative.to month (", 
                    intervention$mobility.relative.to.month, "-",
                    intervention$mobility.relative.to.year, ")"))
    
    #-- Pull the mobility data --#
    mobility.deltas = get.mobility.data.by.month(mdm, locations=location,
                                                 from.year=from.year,
                                                 from.month=from.month,
                                                 to.year=to.year,
                                                 to.month=to.month,
                                                 types=intervention$mobility.types)
    
    mobility.deltas = colMeans(mobility.deltas / mobility.deltas[,relative.to.mask])
    
    #-- Make the Intervention Units --#
    units = lapply()
}

cast.covid.intervention.unit <- function(type,
                                         effect.name=paste0(type, '.reduction'),
                                         start.time,
                                         onset.duration,
                                         start.normalize.time,
                                         normal.time,
                                         mobility.times,
                                         mobility.deltas,
                                         effect.is.reduction,
                                         mobility.weight.name='mobility.weight')
{
    #-- Cast Times --#
    start.time = date.to.numeric.year(start.time)
    start.normalize.time = date.to.numeric.year(start.normalize.time)
    normal.time = date.to.numeric.year(normal.time)
    mobility.times = date.to.numeric.year(mobility.times)
    
    
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
        merged = merge.rates(rates1 = mobility.deltas,
                             times1 = mobility.times,
                             rates2 = c(1,1,0),
                             times2 = c(start.time+onset.duration,
                                        start.normalize.time,
                                        normal.time))

        mask = merged$times <= normal.time
        times = merged$imes[mask]
                
        mobility.multipliers = unlist(merged$rates1[mask])
        taper.multipliers = unlist(merged$rates2[mask])
        
     #   rates = function(parameters) {
     #       mobility.weight = parameters[mobility.weight.name]
     #       1 + parameters[effect.name] * (mobility.weight * mobility.multipliers + (1-mobility.weight)) * taper.multipliers
     #   }
        
        #evaluates to this expression:
        #   1 + <effect> * (<mobility.weight> * mobility.multipliers + (1-<mobility.weight>)) * taper.multiplier
        # = 1 + <effect> * (<mobility.weight> * (mobility.multipliers-1) + 1) * taper.multiplier
        expr.text = paste0("1 + ", effect.name, 
                           " * (", mobility.weight.name,
                           " * (c(", paste0(mobility.multipliers, collapse=','), 
                           ")-1) + 1) * c(", paste0(taper.multipliers, collapse=','), ")")
        rates = parse(text=expr.text)
        
        create.intervention.unit(type=type,
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
