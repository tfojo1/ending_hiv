
##------------------------##
##-- INTERVENTION UNITS --##
##------------------------##

#'@param type The specific rates/proportions to which this intervention applies
#'@param start.year The precise time at which the intervention starts to scale up (ie, the last time prior to which the intervention rates apply)
#'@param rates A numeric vector of rates that will apply going forward
#'@param years The times at which those rates apply
#'
create.intervention.unit <- function(type=c('testing','prep','suppression'),
                                     start.year=2021,
                                     rates,
                                     years)
{
    #-- Check Types --#
    ALLOWED.TYPES = c('testing','prep','suppression')
    if (!is(type, 'character') || !length(type)==1)
        stop("'type' must be a single character value")
    if (all(type != ALLOWED.TYPES))
        stop(paste0("'", type, "' is not a valid value for the 'type' argument. It must be one of: ",
                    paste0("'", ALLOWED.TYPES, "'", collapse=', ')))
    
    #-- Check rates --#
    if ((!is(rates, 'numeric') && !is(rates, 'integer')) || length(rates)==0)
        stop("'rates' must be a numeric vector of length 1 or greater")
    if (any(is.na(rates)))
        stop("'rates' cannot contain NA values")
    
    #-- Check years --#
    if ((!is(years, 'numeric') && !is(years, 'integer')))
        stop("'years' must be a numeric or integer vector")
    if (any(is.na(years)))
        stop("'years' cannot contain NA values")
    if (length(years) != length(rates))
        stop("'years' must have the same length as 'rates'")
    
    if (any(years <= start.year))
        stop("'start.year' must be prior to ALL elements of 'years'")
    
    #-- Sort by year --#
    o = order(years)
    years = years[o]
    rates = rates[o]
    
    rv = list(type=type,
              start.year=start.year,
              rates=rates,
              years=years)
    
    class(rv) = 'intervention.unit'
    rv
}

get.intervention.unit.name <- function(unit, 
                                       include.start=F,
                                       round.digits=1)
{
    if (unit$type=='testing')
    {
        period = round(1/unit$rates, round.digits)
        per.year = round(unit$rates, round.digits)
        
        text.values = paste0('every ', period, ' years')
        text.values[unit$rates==1] = 'yearly'
        text.values[unit$rates>1] = paste0(per.year[unit$rates>1], ' times per year')
        text.values[unit$rates==2] = 'twice a year'
        
        rv = paste0("tested ",
                    paste0(text.values, ' by ', unit$years, collapse=', '))
    }
    else if (unit$type=='prep')
    {
        text.values = paste0(round(100*unit$rates, round.digits), '%')
        
        rv = paste0(text.values[1], ' on PrEP by ', unit$years[1])
        if (length(unit$rates)>1)
            rv = paste0(rv, ', ', paste0(text.values[-1], ' by ', unit$years[-1]), collapse=', ')
    }
    else if (unit$type=='suppression')
    {
        text.values = paste0(round(100*unit$rates, round.digits), '%')
        
        rv = paste0(text.values[1], ' suppressed by ', unit$years[1])
        if (length(unit$rates)>1)
            rv = paste0(rv, ', ', paste0(text.values[-1], ' by ', unit$years[-1]), collapse=', ')
    }
    else
        stop("Only 'testing', 'prep', and 'suppression' are currently supported unit types")
    
    if (include.start)
        rv = paste0("starting in ", unit$start.year, ', ', rv)
    
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

