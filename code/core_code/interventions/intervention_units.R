
CHECK.YEAR = 2030
#Will print warnings if an intervention uses any time greater than this year
THROW.ERROR.IF.PAST.CHECK.YEAR = T



##------------------------##
##-- INTERVENTION UNITS --##
##------------------------##

#'@param type The specific rates/proportions to which this intervention applies
#'@param start.year The precise time at which the intervention starts to scale up (ie, the last time prior to which the intervention rates apply)
#'@param end.year The precise time at which the intervention stops being applied (ie, the first time when the intervention rates no longer apply)
#'@param rates The rates that will apply going forward. Either a numeric, integer, character, or expression vector
#'@param years The times at which those rates apply. Should be the same length as rates
#'@param apply.function The character name of the function that applies the intervention rate. Options are 'absolute' (the intervention rate is used as the new rate), 'multiplier' (the intervention rate is multiplied by the old rate), 'odds.ratio' (the intervention rate is treated as an odds ratio, and applied to the old rate, which is treated as a probability), 'additive' (the intervention rate is added to the old rate)
#'@param allow.less.than.otherwse Whether the relevant rates are allowed to be less than they would have been without the intervention
#'@param allow.greater.than.otherwse Whether the relevant rates are allowed to be greater than they would have been without the intervention
#'@param scale Scale on which the effect takes place. Options are 'rate', 'proportion', 'proportion.leaving', 'proportion.staying', 'time'


create.intervention.unit <- function(type=c('testing','prep','suppression','needle.exchange',''),
                                     start.year=2021,
                                     rates,
                                     years,
                                     scale,
                                     end.year=Inf,
                                     apply.function=c('absolute','multiplier','odds.ratio','additive')[1],
                                     allow.less.than.otherwise = apply.function!='absolute',
                                     allow.greater.than.otherwise = T,
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
                                     name.transformation.function=NULL,
                                     expression.parameters=NULL)
{
    #-- Check Types --#
    if (!is(type, 'character') || !length(type)==1)
        stop("'type' must be a single character value")
 
# Disallowing until we do a smarter check based on transition manager
#    if (all(type != ALLOWED.INTERVENTION.UNIT.TYPES))
#        stop(paste0("'", type, "' is not a valid value for the 'type' argument. It must be one of: ",
#                    paste0("'", ALLOWED.INTERVENTION.UNIT.TYPES, "'", collapse=', ')))
    
    #-- Check years --#
    check.unit.years(start.year, end.year, years, resolved.bindings=numeric())

    #-- Check rates and raw.rates --#
    check.unit.rates(rates=rates, raw.rates=raw.rates, years=years, resolved.bindings=numeric())
    
    #-- Check apply function --#
    ALLOWED.APPLY.FUNCTIONS = c('absolute','multiplier','odds.ratio','additive')
    if (!is(apply.function, 'character') || length(apply.function)!=1 || is.na(apply.function))
        stop("apply.function must be a single character value")
    
    if (all(apply.function != ALLOWED.APPLY.FUNCTIONS))
        stop(paste0("apply.function must be one of: ",
                    paste0("'", ALLOWED.APPLY.FUNCTIONS, "'", collapse=', ')))
    
    #-- Check scale --#
    check.transition.element.type(scale, varname.for.error = 'scale')
    
    #-- Check expression.parameters --#
    if (is.null(expression.parameters))
        expression.parameters = list()
    else if (!is.list(expression.parameters) || is.null(names(expression.parameters)) || 
             any(names(expression.parameters)==''))
        stop("expression.parameters must be a named list")
    
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
    
    rv = list(type=type,
              start.year=start.year,
              end.year=end.year,
              rates=rates,
              years=years,
              scale=scale,
              apply.function=apply.function,
              allow.less.than.otherwise=allow.less.than.otherwise,
              allow.greater.than.otherwise=allow.greater.than.otherwise,
              min.rate = min.rate,
              max.rate = max.rate,
              raw.rates = raw.rates,
              name.metadata = name.metadata,
              resolved.bindings=expression.parameters)

    #-- Set up names to resolve --#
    
    rv = index.unit.to.resolve(rv)
    
        
    class(rv) = 'intervention.unit'
    rv
}



# Creates an intervention unit where the effect is a multiplier applied to a proportion of the population
create.proportion.multiplier.intervention.unit <- function(type=c('testing','prep','suppression'),
                                                           start.year=2021,
                                                           proportions,
                                                           multipliers,
                                                           years,
                                                           scale='proportion',
                                                           end.year=Inf,
                                                           apply.function=c('absolute','multiplier','odds.ratio','additive')[1],
                                                           allow.less.than.otherwise = apply.function!='absolute',
                                                           allow.greater.than.otherwise = T,
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
                             scale=scale,
                             end.year=end.year,
                             apply.function = apply.function,
                             allow.less.than.otherwise=allow.less.than.otherwise,
                             allow.greater.than.otherwise=allow.greater.than.otherwise,
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

##------------------------##
##-- MANAGING DIMENSIONS --##
##------------------------##

get.dim.names.for.intervention.unit <- function(unit.type, settings)
{
    if (any(names(settings$transition.mapping$transition.elements) == unit.type))
    {
        tr.el = settings$transition.mapping$transition.elements[[unit.type]]
        tr.el$dim.names
    }
    else if (any(names(REGISTERED.INTERVENTION.UNIT.METADATA) == unit.type))
    {
        metadata = REGISTERED.INTERVENTION.UNIT.METADATA[[unit.type]]
        settings$DIMENSION.NAMES[metadata$dimensions]
    }
    else
        stop(paste0("Invalid unit type: '", unit.type, "' for the given settings. No transition element named '",
                    unit.type, "' has been registered for model version '", settings$VERSION,
                    "', nor has it been registered as a global intervention unit type."))
}

##------------------------##
##-- CHECKING FUNCTIONS --##
##------------------------##

check.intervention.unit.argument <- function(values,
                                             arg.name=deparse(substitute(value)),
                                             allowed.classes=c('numeric',
                                                               'integer',
                                                               'character',
                                                               'expression',
                                                               'function'),
                                             length=NA,
                                             min.length=if (is.na(length)) 1 else length,
                                             max.length=length,
                                             error.prefix='')
{
    if (!any(sapply(allowed.classes, is, object=values)))
        stop(paste0(error.prefix, "'", arg.name, "' must be one of the following classes: ", 
                    paste0(allowed.classes, collapse=', ')))
    
    if (!is(values, 'function') && !is(values, 'expression') && any(is.na(values)))
        stop(paste0(error.prefix, "'", arg.name, "' cannot contain NA values"))
    
    if (is(values, 'expression'))
    {
        if (min.length>0 && length(values)==0)
            stop(paste0(error.prefix, "'", arg.name, "' must have at least one element"))
    }
    else if (!is(values, 'function'))
    {
        if ((!is.na(min.length) && length(values) < min.length) || 
            (!is.na(max.length) && length(values) > max.length))
        {
            if (min.length==max.length)
                stop(paste0(error.prefix, "'", arg.name, "' must have length ", min.length))
            else if (is.infinite(max.length))
                stop(paste0(error.prefix, "'", arg.name, "' must have length ", min.length, " or greater"))
            else
                stop(paste0(error.prefix, "'", arg.name, "' must have a length between ", min.length, " and ", max.length))
        }
    }
}


check.unit.rates <- function(rates, raw.rates, years,
                             resolved.bindings,
                             error.prefix='')
{
    check.intervention.unit.argument(rates,
                                     arg.name = 'rates',
                                     error.prefix=error.prefix)
    check.intervention.unit.argument(raw.rates,
                                     arg.name = 'raw.rates',
                                     error.prefix=error.prefix)
    check.intervention.unit.argument(years,
                                     arg.name = 'years',
                                     error.prefix=error.prefix)
    
    if (!is(rates, 'function') && !is(rates, 'expression') &&
        !is(years, 'function') && !is(years, 'expression') &&
        length(years) != length(rates))
        stop("'years' must have the same length as 'rates'")
    
    if (!is(rates, 'function') && !is(raw.rates, 'function') && length(raw.rates) != length(rates))
        stop("'raw.rates' and 'rates' must have the same length")
    
}

check.unit.years <- function(start.year, end.year, years,
                             resolved.bindings,
                             error.prefix='')
{
    check.intervention.unit.argument(start.year,
                                     arg.name = 'start.year',
                                     length=1,
                                     error.prefix=error.prefix)
    check.intervention.unit.argument(end.year,
                                     arg.name = 'end.year',
                                     length=1,
                                     error.prefix=error.prefix)
    check.intervention.unit.argument(years,
                                     arg.name = 'years',
                                     error.prefix=error.prefix)
    
    
    if (!need.to.resolve(start.year, resolved.bindings))
    {
        if (any(!need.to.resolve(years, resolved.bindings) & years <= start.year))
            stop("'start.year' must be prior to ALL elements of 'years'")
        if (!need.to.resolve(end.year, resolved.bindings) && start.year >= end.year)
           stop("'start.year' must precede 'end.year'") 
    }
    
    if (!need.to.resolve(end.year, resolved.bindings) & any(!need.to.resolve(years, resolved.bindings) & years >= end.year))
        stop("'end.year' must be after ALL elements of 'years'")
    
    #-- Check Years against CHECK.YEAR --#
    #   This makes sure we didn't accidentally set the year wrong
    check.year.msg = ''
    if (!need.to.resolve(start.year, resolved.bindings) && start.year > CHECK.YEAR)
        check.year.msg = paste0("start year (", start.year, ") is > ", CHECK.YEAR, "; ",
                                check.year.msg)
    if (!need.to.resolve(end.year, resolved.bindings) && end.year > CHECK.YEAR && end.year != Inf)
        check.year.msg = paste0("end year (", end.year, ") is > ", CHECK.YEAR, "; ",
                                check.year.msg)
    if (any(!need.to.resolve(years, resolved.bindings) & years > CHECK.YEAR))
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

# Makes a list of what still needs to be resolved
index.unit.to.resolve <- function(unit)
{
    unit$to.resolve = unique(c(
        get.names.to.resolve(unit$start.year),
        get.names.to.resolve(unit$end.year),
        get.names.to.resolve(unit$rates),
        get.names.to.resolve(unit$raw.rates),
        get.names.to.resolve(unit$years)
    ))
    
    unit$unresolved = c(
        start.year = any(need.to.resolve(unit$start.year, resolved.bindings=list())),
        end.year = any(need.to.resolve(unit$end.year, resolved.bindings=list())),
        rates = any(need.to.resolve(unit$rates, resolved.bindings=list())),
        raw.rates = any(need.to.resolve(unit$raw.rates, resolved.bindings=list())),
        years = any(need.to.resolve(unit$years, resolved.bindings=list()))
    )
    
    unit
}

# Values (for years or rates) in intervention units can be given as character placeholders
# These functions aid in replacing those placeholders with numeric values

need.to.resolve <- function(vals,
                            resolved.bindings)
{
    if (is(vals, 'function'))
        T
    else if (is.character(vals) || is.expression(vals))
    {
        length(get.names.to.resolve(vals, resolved.bindings=resolved.bindings)) > 0
    }
    else
        rep(F, length(vals))
}

get.names.to.resolve <- function(vals,
                                 resolved.bindings=numeric())
{
    if (is.character(vals))
    {
        as.num = suppressWarnings(as.numeric(vals))
        need.to.resolve.mask = !is.na(vals) & is.na(as.num)
        need.to.resolve.names = unique(vals[need.to.resolve.mask])
        setdiff(need.to.resolve.names, names(resolved.bindings))
    }
    else if (is.expression(vals))
    {
        need.to.resolve.names = as.character(unique(unlist(sapply(vals, all.vars))))
        setdiff(need.to.resolve.names, names(resolved.bindings))
    }
    else
        character()
}

intervention.unit.is.resolved <- function(unit)
{
    all(!unit$unresolved)
    #length(unit$to.resolve) == 0
}

#parameters is a named, numeric vector
resolve.intervention.unit <- function(unit, parameters)
{
    if (!is.numeric(parameters) && !is.integer(parameters))
        stop("'parameters' must be a numeric vector")
    
    to.bind = intersect(unit$to.resolve, names(parameters))
    unit$resolved.bindings[to.bind] = parameters[to.bind]
    unit$to.resolve = setdiff(unit$to.resolve, to.bind)
    
    if (any(is.na(unit$resolved.bindings)))
        stop("'parameters' cannot contain NA values to resolve")
        
    unit$start.year = resolve.element(unit$start.year, resolved.bindings = unit$resolved.bindings, parameters = parameters)
    unit$end.year = resolve.element(unit$end.year, resolved.bindings = unit$resolved.bindings, parameters = parameters)
    unit$years = resolve.element(unit$years, resolved.bindings = unit$resolved.bindings, parameters = parameters)
    unit$raw.rates = resolve.element(unit$raw.rates, resolved.bindings = unit$resolved.bindings, parameters = parameters)
    unit$rates = resolve.element(unit$rates, resolved.bindings = unit$resolved.bindings, parameters = parameters)
    
    unit = index.unit.to.resolve(unit)

    check.unit.years(start.year = unit$start.year, end.year = unit$end.year, years = unit$years,
                     resolved.bindings = unit$resolved.bindings)
    check.unit.rates(rates = unit$rates, raw.rates = unit$raw.rates, years = unit$years,
                     resolved.bindings = unit$resolved.bindings)
    
    if (intervention.unit.is.resolved(unit))
    {
        to.check = c('start.year','end.year','years','raw.rates','rates')
        allowed.length.zero = 'end.year'
        allowed.length.zero = sapply(to.check, function(x){any(x==allowed.length.zero)})
        
        checked.numeric.or.integer = sapply(unit[to.check], is.numeric) | sapply(unit[to.check], is.character)
        length.zero = sapply(unit[to.check], length) == 0
        
        if (any( !(length.zero & allowed.length.zero) & !checked.numeric.or.integer))
            stop("Resolved values must be numeric values")
        
        if (any(sapply(unit[to.check], function(x){any(is.na(x))})))
            stop("Resolved values cannot be NA")
        
        o = order(unit$years)
        unit$years = unit$years[o]
        unit$rates = unit$rates[o]
        unit$raw.rates = unit$raw.rates[o]
    }
    
    check.unit.years(unit$start.year, unit$end.year, unit$years,
                     resolved.bindings = unit$resolved.bindings)
    
    unit
}

get.intervention.unit.unresolved.elements <- function(unit)
{
    names(unit$unresolved[unit$unresolved])
}

get.intervention.unit.unresolved.var.names <- function(unit)
{
    setdiff(unit$to.resolve, names(unit$resolved.bindings))
}

resolve.element <- function(elem, resolved.bindings, parameters)
{
    if (is(elem, 'function')){
        pp.for.fn = as.numeric(resolved.bindings)
        pp.for.fn[names(parameters)] = parameters
        rv = elem(pp.for.fn)
        
        # A recursive call, in case the function returned things that need further resolving
        resolve.element(rv, resolved.bindings, parameters)
    }
    else if (is.character(elem))
    {
        for (param.name in names(resolved.bindings))
        {
            if (any(elem==param.name))
                elem[elem==param.name] = resolved.bindings[[param.name]]
        }
        
        if (!any(need.to.resolve(elem, resolved.bindings=resolved.bindings)))
            suppressWarnings(as.numeric(elem))
        else
            elem
    }
    else if (is.expression(elem))
    {
        resolvable = sapply(elem, function(one.val){
            length(setdiff(all.vars(one.val), names(resolved.bindings)))==0
        })
        
        if (all(resolvable))
        {
            elem.resolved = lapply(elem, function(one.val){
                eval(one.val, envir=list2env(as.list(resolved.bindings)))
            })
            
            elem.resolved = unlist(elem.resolved)
            
            if (is(elem.resolved, 'list'))
                stop("Unable to resolve multiple expressions in intervention unit")
            
            # A recursive call, in case the function returned things that need further resolving
            resolve.element(elem.resolved, resolved.bindings, parameters)
        }
        else
            elem
    }
    else if (is(elem, 'numeric') || is(elem, 'integer'))
        as.numeric(elem)
    else
        stop("Error resolving intervention unit: expressions and functions must resolve to numeric values")
}

##--------------------##
##-- UNIT META-DATA --##
##--------------------##


INTERVENTION.UNIT.SUBGROUPS = c('hiv.negative', 'hiv.positive')
ALLOWED.UNIT.METADATA.DIMENSIONS = c('age','race','subpopulation','sex','risk','non.hiv.subset')
create.unit.metadata <- function(type,
                                 applies.to.subgroups,
                                 affects.dimensions=character())
{
    list(type = type,
         applies.to.subgroups = applies.to.subgroups,
         dimensions = affects.dimensions)
}

REGISTERED.INTERVENTION.UNIT.METADATA = list(
    prep = create.unit.metadata(type = 'prep',
                                applies.to.subgroups = 'hiv.negative',
                                affects.dimensions = c('age','race','sex','risk'))
)


ADDITIONAL.PREP.TYPES = c(
    'lai.prep'
)

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
    idu.transmission=F,
    linkage=T,
    retention.suppressed=T,
    retention.naive=T,
    retention.failing=T,
    gain.of.suppression.naive=T,
    gain.of.suppression.failing=T,
    sapply(ADDITIONAL.PREP.TYPES, function(type){T}),
    sapply(paste0('rr.',ADDITIONAL.PREP.TYPES), function(type){F})
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
    idu.transmission='',
    linkage='',
    retention.suppressed='',
    retention.naive='',
    retention.failing='',
    gain.of.suppression.naive='',
    gain.of.suppression.failing='',
    sapply(ADDITIONAL.PREP.TYPES, function(type){''}),
    sapply(paste0('rr.',ADDITIONAL.PREP.TYPES), function(type){'-fold'})
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
    idu.transmission='',
    linkage='',
    retention.suppressed='',
    retention.naive='',
    retention.failing='',
    gain.of.suppression.naive='',
    gain.of.suppression.failing='',
    sapply(ADDITIONAL.PREP.TYPES, function(type){''}),
    sapply(paste0('rr.',ADDITIONAL.PREP.TYPES), function(type){''})
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
    idu.transmission=' idu transmission',
    linkage=' linked',
    retention.suppressed=' retained',
    retention.naive=' retained',
    retention.failing=' retained',
    gain.of.suppression.naive = ' per year',
    gain.of.suppression.failing = ' per year',
    lai.prep = 'on long-acting PrEP',
    rr.lai.prep = 'long-acting PrEP effectiveness'
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
    idu.transmission='IV Transmission',
    linkage='Linkage',
    retention.suppressed='Retention',
    retention.naive='Retention',
    retention.failing='Retention',
    gain.of.suppression.naive='Gain Suppression',
    gain.of.suppression.failing='Gain Suppression',
    lai.prep = 'on long-acting PrEP',
    rr.lai.prep = 'long-acting PrEP effectiveness'
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

