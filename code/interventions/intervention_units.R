
CHECK.YEAR = 2030
#Will print warnings if an intervention uses any time greater than this year
THROW.ERROR.IF.PAST.CHECK.YEAR = T



##------------------------##
##-- INTERVENTION UNITS --##
##------------------------##


#'@param type The specific rates/proportions to which this intervention applies
#'@param start.year The precise time at which the intervention starts to scale up (ie, the last time prior to which the intervention rates apply)
#'@param end.year The precise time at which the intervention stops being applied (ie, the first time when the intervention rates no longer apply)
#'@param rates The rates that will apply going forward. Either (1) a numeric or character vector or 2) a list, each element of which is a numeric or character vector
#'@param years The times at which those rates apply. Should be the same length as rates
#'@param apply.function The character name of the function that applies the intervention rate. Options are 'absolute' (the intervention rate is used as the new rate), 'multiplier' (the intervention rate is multiplied by the old rate), 'odds.ratio' (the intervention rate is treated as an odds ratio, and applied to the old rate, which is treated as a probability), 'additive' (the intervention rate is added to the old rate)
#'@param allow.less.than.otherwse Whether the relevant rates are allowed to be less than they would have been without the intervention
#'
create.intervention.unit <- function(type=c('testing','prep','suppression','needle.exchange',''),
                                     start.year=2021,
                                     rates,
                                     years,
                                     end.year=Inf,
                                     apply.function=c('absolute','multiplier','odds.ratio','additive')[1],
                                     allow.less.than.otherwise = F,
                                     min.rate=-Inf,
                                     max.rate=Inf,
                                     raw.rates=rates,
                                     #Name Arguments
                                     name.category=UNIT.NAME.CATEGORY[type],
                                     name.as.pct=UNIT.NAME.AS.PCT[type],
                                     name.pre.descriptor=UNIT.NAME.PRE.DESCRIPTOR[type],
                                     name.post.descriptor=UNIT.NAME.POST.DESCRIPTOR[type],
                                     name.rate.suffix=UNIT.NAME.RATE.SUFFIX[type],
                                     name.apply.function=apply.function,
                                     name.transformation.function=NULL)
{
    #-- Check Types --#
    if (!is(type, 'character') || !length(type)==1)
        stop("'type' must be a single character value")
    if (all(type != ALLOWED.INTERVENTION.UNIT.TYPES))
        stop(paste0("'", type, "' is not a valid value for the 'type' argument. It must be one of: ",
                    paste0("'", ALLOWED.INTERVENTION.UNIT.TYPES, "'", collapse=', ')))
    
    #-- Check years --#
    check.unit.years(start.year, end.year, years)
    
    if ((!is(years, 'numeric') && !is(years, 'integer') && !is(years, 'character')))
        stop("'years' must be a numeric, integer, or character vector")
    if (any(is.na(years)))
        stop("'years' cannot contain NA values")
    

    #-- Check rates --#
    if ((!is(rates, 'numeric') && !is(rates, 'integer') && !is(rates, 'character')) || length(rates)==0)
        stop("'rates' must be a numeric or character vector of length 1 or greater")
    if (length(years) != length(rates))
        stop("'years' must have the same length as 'rates'")
    
    #-- Check raw rates --#
    if ((!is(raw.rates, 'numeric') && !is(raw.rates, 'integer') && !is(raw.rates, 'character')) || length(raw.rates)==0)
        stop("'raw.rates' must be a numeric or character vector of length 1 or greater")
    if (length(raw.rates) != length(rates))
        stop("'raw.rates' and 'rates' must have the same length")
    
    #-- Check apply function --#
    ALLOWED.APPLY.FUNCTIONS = c('absolute','multiplier','odds.ratio','additive')
    if (!is(apply.function, 'character') || length(apply.function)!=1 || is.na(apply.function))
        stop("apply.function must be a single character value")
    
    if (all(apply.function != ALLOWED.APPLY.FUNCTIONS))
        stop(paste0("apply.function must be one of: ",
                    paste0("'", ALLOWED.APPLY.FUNCTIONS, "'", collapse=', ')))
    
    #-- Set up names to resolve --#
    
    to.resolve = unique(c(
        as.character(start.year[need.to.resolve(start.year)]),
        as.character(end.year[need.to.resolve(end.year)]),
        as.character(rates[need.to.resolve(rates)]),
        as.character(raw.rates[need.to.resolve(raw.rates)]),
        as.character(years[need.to.resolve(years)])
    ))
    
    #-- Set up name metadata --#
    
    name.metadata = list(
        category=name.category,
        as.pct=name.as.pct,
        pre.descriptor=name.pre.descriptor,
        post.descriptor=name.post.descriptor,
        rate.suffix=name.rate.suffix,
        apply.function=name.apply.function,
        transformation.function=name.transformation.function
    )
    
    #-- Sort by year --#
    o = order(years)
    years = years[o]
    rates = rates[o]
    raw.rates = raw.rates[o]
    
    rv = list(type=type,
              start.year=start.year,
              end.year=end.year,
              rates=rates,
              years=years,
              apply.function=apply.function,
              allow.less.than.otherwise=allow.less.than.otherwise,
              min.rate = min.rate,
              max.rate = max.rate,
              raw.rates = raw.rates,
              name.metadata = name.metadata,
              to.resolve=to.resolve)
    
    class(rv) = 'intervention.unit'
    rv
}



# Creates an intervention unit where the effect is a multiplier applied to a proportion of the population
create.proportion.multiplier.intervention.unit <- function(type=c('testing','prep','suppression'),
                                                           start.year=2021,
                                                           proportions,
                                                           multipliers,
                                                           years,
                                                           end.year=Inf,
                                                           apply.function=c('absolute','multiplier','odds.ratio','additive')[1],
                                                           allow.less.than.otherwise = F,
                                                           min.rate=-Inf,
                                                           max.rate=Inf,
                                                           raw.proportions=proportions,
                                                           #Name Arguments
                                                           name.category=UNIT.NAME.CATEGORY[type],
                                                           name.as.pct=UNIT.NAME.AS.PCT[type],
                                                           name.pre.descriptor=UNIT.NAME.PRE.DESCRIPTOR[type],
                                                           name.post.descriptor=UNIT.NAME.POST.DESCRIPTOR[type],
                                                           name.rate.suffix=UNIT.NAME.RATE.SUFFIX[type],
                                                           name.apply.function=apply.function,
                                                           name.transformation.function=NULL)
{
    rates = (1-proportions) + proportions * multipliers
    create.intervention.unit(type=type,
                             start.year=start.year,
                             rates=rates,
                             years=years,
                             end.year=end.year,
                             apply.function = apply.function,
                             allow.less.than.otherwise=allow.less.than.otherwise,
                             min.rate=min.rate,
                             max.rate=max.rate,
                             raw.rates = raw.proportions,
                             # name arguments
                             name.category=name.category,
                             name.as.pct=name.as.pct,
                             name.pre.descriptor=name.pre.descriptor,
                             name.post.descriptor=name.post.descriptor,
                             name.rate.suffix=name.rate.suffix,
                             name.apply.function=name.apply.function,
                             name.transformation.function=name.transformation.function
                             )
}

check.unit.years <- function(start.year, end.year, years)
{
    
    if (!need.to.resolve(start.year))
    {
        if (any(!need.to.resolve(years) & years <= start.year))
            stop("'start.year' must be prior to ALL elements of 'years'")
        if (!need.to.resolve(end.year) && start.year >= end.year)
           stop("'start.year' must precede 'end.year'") 
    }
    
    if (!need.to.resolve(end.year) & any(!need.to.resolve(years) & years >= end.year))
        stop("'end.year' must be after ALL elements of 'years'")
    
    #-- Check Years against CHECK.YEAR --#
    #   This makes sure we didn't accidentally set the year wrong
    check.year.msg = ''
    if (!need.to.resolve(start.year) && start.year > CHECK.YEAR)
        check.year.msg = paste0("start year (", start.year, ") is > ", CHECK.YEAR, "; ",
                                check.year.msg)
    if (!need.to.resolve(end.year) && end.year > CHECK.YEAR && end.year != Inf)
        check.year.msg = paste0("end year (", end.year, ") is > ", CHECK.YEAR, "; ",
                                check.year.msg)
    if (any(!need.to.resolve(years) & years > CHECK.YEAR))
        check.year.msg = paste0("some years (", 
                                paste0(years[years>CHECK.YEAR], collapse=", "),
                                ") are > ", CHECK.YEAR, "; ",
                                check.year.msg)
    if (check.year.msg != '')
    {
        if (THROW.ERROR.IF.PAST.CHECK.YEAR)
            stop(check.year.msg)
        else
            print(paste0("**WARNING:** ", check.year.msg))
    }
}

##---------------------------------##
##-- RESOLVING ELEMENTS of UNITS --##
##---------------------------------##


# Values (for years or rates) in intervention units can be given as character placeholders
# These functions aid in replacing those placeholders with numeric values

need.to.resolve <- function(vals)
{
    if (is.character(vals))
    {
        as.num = suppressWarnings(as.numeric(vals))
        !is.na(vals) & is.na(as.num)
    }
    else
        rep(F, length(vals))
}

intervention.unit.is.resolved <- function(unit)
{
    length(unit$to.resolve) == 0
}

#parameters is a named, numeric vector
resolve.intervention.unit <- function(unit, parameters)
{
    if (!is.numeric(parameters) && !is.integer(parameters))
        stop("'parameters' must be a numeric vector")
    
    for (param.name in unit$to.resolve)
    {
        if (any(names(parameters)==param.name))
        {
            param.value = parameters[param.name]
            if (is.na(param.value))
                stop("'parameters' cannot contain NA values to resolve")
        
            unit$start.year = resolve.element(unit$start.year, param.name=param.name, param.value=param.value)
            unit$end.year = resolve.element(unit$end.year, param.name=param.name, param.value=param.value)
            unit$years = resolve.element(unit$years, param.name=param.name, param.value=param.value)
            
            unit$raw.rates = resolve.element(unit$raw.rates, param.name=param.name, param.value=param.value)
            unit$rates = resolve.element(unit$rates, param.name=param.name, param.value=param.value)
            
            unit$to.resolve = setdiff(unit$to.resolve, param.name)
        }
    }
    
    check.unit.years(unit$start.year, unit$end.year, unit$years)
    
    o = order(unit$years)
    unit$years = unit$years[o]
    unit$rates = unit$rates[o]
    unit$raw.rates = unit$raw.rates[o]
    
    unit
}

resolve.element <- function(elem, param.name, param.value)
{
    if (is.character(elem))
    {
        if (any(elem==param.name))
            elem[elem==param.name] = param.value
        if (!any(need.to.resolve(elem)))
            suppressWarnings(as.numeric(elem))
        else
            elem
    }
    else
        elem
}

##----------------##
##-- UNIT NAMES --##
##----------------##

UNIT.NAME.AS.PCT = c(
    testing=F,
    prep=T,
    rr.prep=F,
    suppression=T,
    needle.exchange=T,
    idu.incidence=F,
    idu.remission=F,
    idu.relapse=F,
    heterosexual.transmission=F,
    msm.transmission=F,
    idu.transmission=F
)

ALLOWED.INTERVENTION.UNIT.TYPES = names(UNIT.NAME.AS.PCT)


UNIT.NAME.RATE.SUFFIX = c(
    testing='x',
    prep='',
    rr.prep='-fold',
    suppression='',
    needle.exchange='',
    idu.incidence='',
    idu.remission='',
    idu.relapse='',
    heterosexual.transmission='',
    msm.transmission='',
    idu.transmission=''
)
if (!setequal(ALLOWED.INTERVENTION.UNIT.TYPES, names(UNIT.NAME.RATE.SUFFIX)))
    stop(paste0("Failed sanity check - UNIT.NAME.RATE.SUFFIX names do not match ALLOWED.INTERVENTION.UNIT.TYPES"))

UNIT.NAME.PRE.DESCRIPTOR = c(
    testing='tested ',
    prep='',
    rr.prep='',
    suppression='',
    needle.exchange='',
    idu.incidence='',
    idu.remission='',
    idu.relapse='',
    heterosexual.transmission='',
    msm.transmission='',
    idu.transmission=''
)
if (!setequal(ALLOWED.INTERVENTION.UNIT.TYPES, names(UNIT.NAME.PRE.DESCRIPTOR)))
    stop(paste0("Failed sanity check - UNIT.NAME.PRE.DESCRIPTOR names do not match ALLOWED.INTERVENTION.UNIT.TYPES"))

UNIT.NAME.POST.DESCRIPTOR = c(
    testing=' per year',
    prep=' on PrEP',
    rr.prep=' PrEP effectiveness',
    suppression=' suppressed',
    needle.exchange=' in needle-exchange programs',
    idu.incidence=' IDU incidence',
    idu.remission=' IDU remission',
    idu.relapse=' IDU relapse',
    heterosexual.transmission=' heterosexual transmission',
    msm.transmission=' msm transmission',
    idu.transmission=' idu transmission'
)
if (!setequal(ALLOWED.INTERVENTION.UNIT.TYPES, names(UNIT.NAME.POST.DESCRIPTOR)))
    stop(paste0("Failed sanity check - UNIT.NAME.POST.DESCRIPTOR names do not match ALLOWED.INTERVENTION.UNIT.TYPES"))

UNIT.NAME.CATEGORY = c(
    testing='HIV Testing',
    prep='PrEP',
    rr.prep='PrEP Effectiveness',
    suppression='Viral Suppression',
    needle.exchange='Needle Exchange',
    idu.incidence='IDU Incidence',
    idu.remission='IDU Remission',
    idu.relapse='IDU Relapse',
    heterosexual.transmission='Heterosexual Transmission',
    msm.transmission='Male-to-Male Sexual Transmission',
    idu.transmission='IV Transmission'
)
if (!setequal(ALLOWED.INTERVENTION.UNIT.TYPES, names(UNIT.NAME.CATEGORY)))
    stop(paste0("Failed sanity check - UNIT.NAME.CATEGORY names do not match ALLOWED.INTERVENTION.UNIT.TYPES"))


# rate suffix
# pre descriptor
# post descriptor

get.intervention.unit.name <- function(unit,
                                       include.start.time=F,
                                       include.end.time=F,
                                       round.digits=2)
{
    #-- Set up the text for rates --#
    if (is.null(unit$name.metadata$transform.rates))
        rates = unit$raw.rates
    else
        rates = unit$name.metadata$transform.rates(unit$raw.rates)

        as.pct = unit$name.metadata$as.pct & unit$name.metadata$apply.function!='multiplier' && unit$name.metadata$apply.function!='odds.ratio'
    if (as.pct)
        rates = 100 * rates
    
    if (unit$name.metadata$apply.function=='additive')
    {
        increase = rates>0
        rates = abs(rates)
    }
    else if (unit$name.metadata$apply.function=='multiplier' || unit$name.metadata$apply.function=='odds.ratio')
        increase = rates>1
    
    rates = round(rates, round.digits)
    if (as.pct)
        rates = paste0(rates, '%')
    
    text.values = rates
    
    if (unit$name.metadata$apply.function=='multiplier')
    {
        text.values = paste0(text.values, '-fold')
        text.values[increase] = paste0(text.values[increase], ' more')
        text.values[!increase] = paste0(text.values[!increase], ' less')
    }
    else if (unit$name.metadata$apply.function=='odds.ratio')
    {
        text.values = paste0(text.values, '-fold odds')
        text.values[increase] = paste0(text.values[increase], ' more')
        text.values[!increase] = paste0(text.values[!increase], ' less')
    }
    else if (unit$name.metadata$apply.function=='additive')
    {
        text.values = paste0(text.values, unit$name.metadata$rate.suffix)
        text.values[increase] = paste0(text.values[increase], ' more')
        text.values[!increase] = paste0(text.values[!increase], ' less')
    }
    else #absolute
        text.values = paste0(text.values, unit$name.metadata$rate.suffix)
    
    
    #-- Put it together --#
    
    text.values[1] = paste0(unit$name.metadata$pre.descriptor, text.values[1], unit$name.metadata$post.descriptor)
    
    if (include.end.time || length(text.values)>1)
        rv = paste0(text.values, ' by ', unit$years-1, collapse=', ')
    else
        rv = text.values[1]
    
    #-- Start Text --#
    if (include.start.time)
        rv = paste0("Starting in ", unit$start.year, ", ")
    
    #-- Return --#
    rv
}


get.intervention.unit.code <- function(unit)
{
    TYPE.CODES = c(testing='t',
                   suppression='s',
                   prep='p')
    
    type.code = TYPE.CODES[unit$type]
    start.year.code = unit$start.year%%100
    year.codes = unit$years%%100
    
    paste0(type.code,
           start.year.code,
           paste0('_', unit$rates, '_', year.codes, collapse='')
    )
}

intervention.units.equal <- function(unit1, unit2)
{
    get.intervention.unit.code(unit1) == get.intervention.unit.code(unit2)
}

