
CHECK.YEAR = 2030
#Will print warnings if an intervention uses any time greater than this year
THROW.ERROR.IF.PAST.CHECK.YEAR = T

INTERVENTION.UNIT.TYPE.PRETTY.NAMES = c(testing='Testing',
                                        prep='PrEP',
                                        suppression="Viral Suppression",
                                        needle.exchange='Needle Exchange',
                                        
                                        #Transmission
                                        heterosexual.transmission="Heterosexual Transmission",
                                        msm.transmission = 'MSM Transmission',
                                        idu.transmission = "IV Transmission",
                                        
                                        #IDU Transitions
                                        idu.incidence = "Incident IDU",
                                        idu.remission = "IDU Remission",
                                        idu.relapse = "IDU Relapse"
                                        
                                        )

INTERVENTION.UNIT.TYPES = names(INTERVENTION.UNIT.TYPE.PRETTY.NAMES)

get.pretty.unit.type.names <- function(type)
{
    INTERVENTION.UNIT.TYPE.PRETTY.NAMES[type]
}

##------------------------##
##-- INTERVENTION UNITS --##
##------------------------##


#'@param type The specific rates/proportions to which this intervention applies
#'@param start.year The precise time at which the intervention starts to scale up (ie, the last time prior to which the intervention rates apply)
#'@param end.year The precise time at which the intervention stops being applied (ie, the first time when the intervention rates no longer apply)
#'@param rates A numeric vector of rates that will apply going forward
#'@param years The times at which those rates apply
#'@param apply.function The character name of the function that applies the intervention rate. Options are 'absolute' (the intervention rate is used as the new rate), 'multiplier' (the intervention rate is multiplied by the old rate), 'odds.ratio' (the intervention rate is treated as an odds ratio, and applied to the old rate, which is treated as a probability), 'additive' (the intervention rate is added to the old rate)
#'@param allow.less.than.otherwse Whether the relevant rates are allowed to be less than they would have been without the intervention
#'
create.intervention.unit <- function(type=c('testing','prep','suppression'),
                                     start.year=2021,
                                     rates,
                                     years,
                                     end.year=Inf,
                                     apply.function=c('absolute','multiplier','odds.ratio','additive')[1],
                                     allow.less.than.otherwise = F,
                                     min.rate=Inf,
                                     max.rate=Inf)
{
    #-- Check Years against CHECK.YEAR --#
    check.year.msg = ''
    if (start.year > CHECK.YEAR)
        check.year.msg = paste0("start year (", start.year, ") is > ", CHECK.YEAR, "; ",
                                check.year.msg)
    if (end.year > CHECK.YEAR && end.year != Inf)
        check.year.msg = paste0("end year (", end.year, ") is > ", CHECK.YEAR, "; ",
                                check.year.msg)
    if (any(years > CHECK.YEAR))
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
    
    #-- Check Types --#
    if (!is(type, 'character') || !length(type)==1)
        stop("'type' must be a single character value")
    if (all(type != INTERVENTION.UNIT.TYPES))
        stop(paste0("'", type, "' is not a valid value for the 'type' argument. It must be one of: ",
                    paste0("'", INTERVENTION.UNIT.TYPES, "'", collapse=', ')))
    
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
    
    #-- Check apply function --#
    ALLOWED.APPLY.FUNCTIONS = c('absolute','multiplier','odds.ratio','additive')
    if (!is(apply.function, 'character') || length(apply.function)!=1 || is.na(apply.function))
        stop("apply.function must be a single character value")
    
    if (all(apply.function != ALLOWED.APPLY.FUNCTIONS))
        stop(paste0("apply.function must be one of: ",
                    paste0("'", ALLOWED.APPLY.FUNCTIONS, "'", collapse=', ')))
    
    #-- Sort by year --#
    o = order(years)
    years = years[o]
    rates = rates[o]
    
    rv = list(type=type,
              start.year=start.year,
              end.year=end.year,
              rates=rates,
              years=years,
              apply.function=apply.function,
              allow.less.than.otherwise=allow.less.than.otherwise,
              min.rate = min.rate,
              max.rate = max.rate)
    
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
                                                           allow.less.than.otherwise = F,
                                                           min.rate=Inf,
                                                           max.rate=Inf)
{
    rates = (1-proportions) + proportions * multipliers
    create.intervention.unit(type=type,
                             start.year=start.year,
                             rates=rates,
                             years=years,
                             end.year=end.year,
                             allow.less.than.otherwise=allow.less.than.otherwise,
                             min.rate=min.rate,
                             max.rate=max.rate)
}

get.intervention.unit.name <- function(unit, 
                                       include.start.text=NA,
                                       include.by=F,
                                       round.digits=1,
                                       testing.descriptor = 'tested',
                                       prep.descriptor = 'on PrEP',
                                       suppression.descriptor = 'suppressed'
                                       )
{
    if (!is.null(testing.descriptor) && !is.na(testing.descriptor) && testing.descriptor!='')
        testing.descriptor = paste0(trimws(testing.descriptor), ' ')
    if (!is.null(prep.descriptor) && !is.na(prep.descriptor) && prep.descriptor!='')
        prep.descriptor = paste0(' ', trimws(prep.descriptor))
    if (!is.null(suppression.descriptor) && !is.na(suppression.descriptor) && suppression.descriptor!='')
        suppression.descriptor = paste0(' ', trimws(suppression.descriptor))
    
    if (unit$type=='testing')
    {
        period = round(1/unit$rates, round.digits)
        per.year = round(unit$rates, round.digits)
        
        text.values = paste0('every ', period, ' years')
        text.values[unit$rates==1] = 'yearly'
        text.values[unit$rates>1] = paste0(per.year[unit$rates>1], ' times per year')
        text.values[unit$rates==2] = 'twice a year'
        
        if (include.by || length(text.values)>1)
            rv = paste0(testing.descriptor,
                        paste0(text.values, ' by ', unit$years, collapse=', '))
        else
            rv = paste0(testing.descriptor, text.values)
    }
    else if (unit$type=='prep')
    {
        text.values = paste0(round(100*unit$rates, round.digits), '%')
        
        rv = paste0(text.values[1], prep.descriptor)
        if (include.by || length(unit$rates)>1)
            rv = paste0(rv, ' by ', unit$years[1])
        if (length(unit$rates)>1)
            rv = paste0(rv, ', ', paste0(text.values[-1], ' by ', unit$years[-1]), collapse=', ')
    }
    else if (unit$type=='suppression')
    {
        text.values = paste0(round(100*unit$rates, round.digits), '%')
        
        rv = paste0(text.values[1], suppression.descriptor)
        if (include.by || length(text.values)>1)
            rv = paste0(rv, ' by ', unit$years[1])
        if (length(unit$rates)>1)
            rv = paste0(rv, ', ', paste0(text.values[-1], ' by ', unit$years[-1]), collapse=', ')
    }
    else
        stop("Only 'testing', 'prep', and 'suppression' are currently supported unit types")
    
    if (!is.null(include.start.text) && !is.na(include.start.text))
        rv = paste0(include.start.text," ", unit$start.year, ', ', rv)
    
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

