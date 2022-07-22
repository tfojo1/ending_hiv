

##-----------------------------------------------------##
##--                      SCHEMA                     --##
##                                                     ##
##-- Define the overall structure of what dimensions --##
##-- are allowed to have transition mappings         --##
##-----------------------------------------------------##

TRANSITION.MAPPING.SUBGROUPS = c('hiv.negative', 'hiv.positive')

create.transition.mapping.schema <- function(dimension,
                                             subgroups,#hiv_positive and/or hiv_negative
                                             always.required,
                                             settings.name)
{
    if (!is.character(dimension) || length(dimension)!=1)
        stop("dimension must be a single character value")
    
    if (!is.character(subgroups) || (length(subgroups)!=1 && length(subgroups)!=2) ||
        length(setdiff(subgroups, TRANSITION.MAPPING.SUBGROUPS))>0)
        stop(paste0("subgroup must be a character with one or two elements that are a subset of: ",
                    paste0("'", TRANSITION.MAPPING.SUBGROUPS, "'", collapse=', ')))
    
    if (!is.logical(always.required) || length(always.required)!=1)
        stop("always.required must be a single logical value")
    
    if (!is.character(settings.name) || length(settings.name)!=1)
        stop("settings.name must be a single character value")
    
    list(dimension=dimension,
         subgroups=subgroups,
         always.required=always.required,
         settings.name=settings.name)
}

TRANSITION.MAPPING.SCHEMA = list(
    create.transition.mapping.schema(dimension='continuum',
                                     subgroups=c('hiv.positive'),
                                     always.required=T,
                                     settings.name='CONTINUUM_OF_CARE'),
    create.transition.mapping.schema(dimension='subpopulation',
                                     subgroups=c('hiv.negative','hiv.positive'),
                                     always.required=F,
                                     settings.name='SUBPOPULATIONS')
)
names(TRANSITION.MAPPING.SCHEMA) = sapply(TRANSITION.MAPPING.SCHEMA, function(sch){sch$dimension})

##----------------------------------------------------##
##--            TRANSITION ELEMENTS TYPES           --##
##                                                    ##
##--   The elements that get combined/manipulated   --##
##--   to produce transition rates                  --##
##----------------------------------------------------##

TRANSITION.ELEMENT.TYPES = c('rate',
                             'proportion', #an alias for proportion.leaving
                             'proportion.leaving',
                             'proportion.staying',
                             'time')

TRANSITION.ELEMENT.MODEL.SOURCES = c('continuum.manager',
                                     'prep.manager',
                                     'comorbidities.manager')

TRANSITION.ELEMENT.RAMP.SCALES = c('identity','log','exp')

# values should be a list
convert.transition.element.type <- function(values,
                                            convert.from.type,
                                            convert.to.type)
{
    check.transition.element.type(convert.from.type)
    check.transition.element.type(convert.to.type)
    
    need.to.unlist = !is.list(values)
    if (need.to.unlist)
        values = list(values)
    
    if (convert.from.type==convert.to.type)
    {}
    else if (convert.from.type=='rate')
    {
        if (convert.to.type=='proportion' || convert.to.type=='proportion.leaving')
            values = lapply(values, function(r){1-exp(-r)})
        else if (convert.to.type=='proportion.staying')
            values = lapply(values, function(r){exp(-r)})
        else if (convert.to.type=='time')
            values = lapply(values, function(r){1/r})
        else
            stop(paste0("Internal Error: Have not defined conversions from '", convert.from.type, "' to '", convert.to.type, "'"))
    }
    else if (convert.from.type=='proportion' || convert.from.type=='proportion.leaving')
    {
        if (convert.to.type=='rate')
            values = lapply(values, function(p){-log(1-p)})
        else if (convert.to.type=='proportion.staying')
            values = lapply(values, function(p){1-p})
        else if (convert.to.type=='time')
            values = lapply(values, function(p){-1/log(1-p)})
        else
            stop(paste0("Internal Error: Have not defined conversions from '", convert.from.type, "' to '", convert.to.type, "'"))
    }
    else if (convert.from.type=='proportion.staying')
    {
        if (convert.to.type=='rate')
            values = lapply(values, function(p){-log(p)})
        else if (convert.to.type=='proportion' || convert.to.type=='proportion.leaving')
            values = lapply(values, function(p){1-p})
        else if (convert.to.type=='time')
            values = lapply(values, function(p){-1/log(p)})
        else
            stop(paste0("Internal Error: Have not defined conversions from '", convert.from.type, "' to '", convert.to.type, "'"))
    }
    else if (convert.from.type=='time')
    {
        if (convert.to.type=='rate')
            values = lapply(values, function(t){1/t})
        else if (convert.to.type=='proportion' || convert.to.type=='proportion.leaving')
            values = lapply(values, function(t){1-exp(-1/t)})
        else if (convert.to.type=='proportion.staying')
            values = lapply(values, function(t){exp(-1/t)})
        else
            stop(paste0("Internal Error: Have not defined conversions from '", convert.from.type, "' to '", convert.to.type, "'"))
    }
    else
        stop(paste0("Internal Error: Have not defined conversions from '", convert.from.type, "'"))
    
    # Return
    if (need.to.unlist)
        values[[1]]
    else
        values
}

check.transition.element.type <- function(type,
                                          varname.for.error='type')
{
    if (!is.character(type) || length(type)!=1 || is.na(type))
        stop(paste0("'", varname.for.error, "' must be a non-NA character scalar"))
    
    if (all(type != TRANSITION.ELEMENT.TYPES))
        stop(paste0("'", varname.for.error, "' must be one of the following: ",
                    paste0("'", TRANSITION.ELEMENT.TYPES, "'", collapse=', ')))
}

create.transition.element <- function(name,
                                      type=NULL,
                                      background.model.type=type,
                                      ramp.type=type,
                                      return.type=type,
                                      required=T,
                                      default.value=NULL,
                                      ramp.times=numeric(),
                                      ramp.multipliers=numeric(),
                                      ramp.interpolate.scales=character(),
                                      model.source=NULL)
{
    if (!is.character(name) || length(name)!=1 || is.na(name))
        stop("'name' must be a non-na character scalar")
    
#    check.transition.element.type(type, 'type')
    check.transition.element.type(background.model.type, 'background.model.type')
    check.transition.element.type(ramp.type, 'ramp.type')
    check.transition.element.type(return.type, 'return.type')
    
    # Check required/default value
    if (!is.logical(required) || length(required) != 1 || is.na(required))
        stop("required must be a non-na logical scalar")
    if (!required && (length(default.value)==0 || any(is.na(default.value))))
        stop("if required is FALSE, default value must be set")
    if (required && !is.null(default.value))
        stop("default.value only applies if required is FALSE")
    if (required)
        default.value = NULL
    
    # Check ramp
    if (length(ramp.times)>0)
    {
        if (length(ramp.multipliers)==0)
            stop("ramp.times has been set, but ramp.multipliers is empty")
        if (length(ramp.interpolate.scales)==0)
            stop("ramp.times has been set, but ramp.interpolate.scales is empty")
        
        if (!is.numeric(ramp.times))
            stop("ramp.times must be a numeric vector")
        if (!all(ramp.times==sort(ramp.times)))
            stop("ramp.times must be in increasing order")
        
        if (!is.numeric(ramp.multipliers))
            stop("ramp.times must be a numeric vector")
        if (!is.character(ramp.interpolate.scales))
            stop("ramp.times must be a character vector")
        invalid.scales = setdiff(ramp.interpolate.scales, TRANSITION.ELEMENT.RAMP.SCALES)
        if (length(invalid.scales)>0)
            stop(paste0("Invalid value(s) for ramp.interpolate.scales (",
                        paste0("'", invalid.scales, "'", collapse=', '),
                        ") - values must be one of: ",
                        paste0("'", TRANSITION.ELEMENT.RAMP.SCALES, "'", collapse=', ')))
        
        if (length(ramp.interpolate.scales)==1)
            ramp.interpolate.scales = rep(ramp.interpolate.scales, length(ramp.times))
        if (length(ramp.times) != length(ramp.interpolate.scales))
            stop("ramp.times and ramp.interpolate.scales must have the same length OR ramp.interpolate.scales must be a single value")
        
        if (length(ramp.times) != length(ramp.multipliers))
            stop("ramp.times and ramp.multipliers must have the same length")
        
        if (is.null(names(ramp.times)))
        {
            if (!is.null(names(ramp.multipliers)))
                names(ramp.times) = names(ramp.multipliers)
        }
        else
        {
            if (is.null(names(ramp.multipliers)))
                names(ramp.multipliers) = names(ramp.times)
            else if (!all(names(ramp.multipliers)==names(ramp.times)))
                stop("ramp.multipliers and ramp.times must have the same names")
        }
        
        if (!is.null(names(ramp.interpolate.scales)) && 
            !all(names(ramp.interpolate.scales)==names(ramp.multipliers)))
            stop("ramp.interpolate.scales must have the same names as ramp.times and/or ramp.multipliers")
        
        names(ramp.interpolate.scales) = names(ramp.multipliers)
    }
    else if (length(ramp.multipliers)>0)
        stop("ramp.multipliers has been set, but ramp.times is empty")
    else
        ramp.multipliers = ramp.times = NULL
    
    if (!is.null(model.source))
    {
        if (!is.character(model.source) || length(model.source)!=1 || is.na(model.source))
            stop("model.source must be a non-NA character scalar")
        if (all(model.source != TRANSITION.ELEMENT.MODEL.SOURCES))
            stop(paste0("'", model.source, "' is not a valid model.source, which must be one of: ",
                        paste0("'", TRANSITION.ELEMENT.MODEL.SOURCES, "'", collapse=', ')))
    }
    
    rv = list(
        name=name,
        background.model.type=background.model.type,
        ramp.type=ramp.type,
        return.type=return.type,
        required=required,
        default.value=default.value,
        ramp.times = ramp.times,
        ramp.multipliers = ramp.multipliers,
        ramp.interpolate.scales=ramp.interpolate.scales,
        model.source = model.source
    )
    
    class(rv) = 'transition.element'
    rv
}

calculate.transition.ramp.multipliers <- function(transition.element,
                                                  ramp.times,
                                                  ramp.multipliers,
                                                  non.ramp.times)
{
    non.ramp.time = min(non.ramp.times)
    if (non.ramp.time < max(ramp.times))
        stop("ramp.times must all be PRIOR to non-ramp time")
    
    if (any(is.na(ramp.multipliers)))
        stop("NA values not allowed when calculating ramp multipliers")
    
    o = order(ramp.times)
    
    ramp.times = ramp.times[o]
    ramp.multipliers = ramp.multipliers[o]
    ramp.scales = transition.element$ramp.interpolate.scales[o]
    times = c(ramp.times, non.ramp.time)
    multipliers = c(ramp.multipliers, 1)
    
    n.segments = length(ramp.times)
    
    rv = list()
    multipliers = unlist(sapply(1:n.segments, function(i){
        interpolate.times = times[i]:times[i+1]
        interpolate.times = interpolate.times[-length(interpolate.times)]
        
        if (ramp.scales[i]=='log')
            exp(interpolate.parameters(values=log(multipliers[i:(i+1)]),
                                       value.times=times[i:(i+1)],
                                       desired.times = interpolate.times,
                                       return.list = F))
        else if (ramp.scales[i]=='exp')
            log(interpolate.parameters(values=exp(multipliers[i:(i+1)]),
                                       value.times=times[i:(i+1)],
                                       desired.times = interpolate.times,
                                       return.list = F))
        else #identity
            multipliers[i]
    }))
    
    times = unlist(sapply(1:n.segments, function(i){
        if (ramp.scales[i]=='identity')
            times[i]
        else #not identity
        {
            rv = times[i]:times[i+1]
            rv[-length(rv)]
        }
    }))
    
    list(
        multipliers=multipliers,
        times=times
    )
}

transition.element.needs.background.model <- function(transition.element)
{
    if (!is(transition.element, 'transition.element'))
        stop("transition.element must be an object of class 'transition.element'")
    
    length(transition.element$model.source)>0
}

get.transition.element.background.model <- function(transition.element,
                                                    continuum.manager=NULL,
                                                    prep.manager=NULL,
                                                    settings=NULL,
                                                    ...)
{
    if (!is(transition.element, 'transition.element'))
        stop("transition.element must be an object of class 'transition.element'")
    
    if (length(transition.element$model.source)==0)
    {
        stop(paste0("No model source has been set for this transition element ('", 
                    transition.element$name, "'). Cannot pull background model"))
    }
    else if (transition.element$model.source=='continuum.manager')
    {
        if (is.null(continuum.manager))
            stop(paste0("continuum.manager must be specified for this transition element ('",
                        transition.element$name, "'). Cannot pull background model"))
        
        get.continuum.model(continuum.manager = continuum.manager,
                            type=transition.element$name,
                            ...)
    }
    else if (transition.element$model.source=='prep.manager')
    {
        if (is.null(prep.manager))
            stop(paste0("prep.manager must be specified for this transition element ('",
                        transition.element$name, "'). Cannot pull background model"))
        
        get.prep.model(prep.manager = prep.manager, settings = settings)
    }
    else
        stop(paste0("Invaild model source ('", transition.element$model.source, 
                    "') in transition.element ('", transition.element$name, "')"))
}

##---------------------------------------------------------##
##--                  TRANSITION MAPPING                 --##
##                                                         ##
##-- A general data structure that descirbes transitions --##
##-- for each dimension and their component elements     --##
##---------------------------------------------------------##

create.transition.mapping <- function(version)
{
    by.subgroup = lapply(TRANSITION.MAPPING.SUBGROUPS, function(subgroup){
        list()
    })
    names(by.subgroup) = TRANSITION.MAPPING.SUBGROUPS
    
    rv = list(version = version,
              by.subgroup = by.subgroup,
              transition.elements = list(),
              elements.to.subgroups.and.dimensions = list())
    class(rv) = 'transition.mapping'
    
    rv
}

copy.transition.mapping <- function(template,
                                    version,
                                    exclude.dimensions=character(),
                                    rename.states=character())
{
    if (!is(template, 'transition.mapping'))
        stop("template must be an object of class 'transition.mapping'")
    
    if (!is.character(exclude.dimensions))
        stop("exclude.dimensions must be a character vector")
    invalid.dimensions = setdiff(exclude.dimensions, names(TRANSITION.MAPPING.SCHEMA))
    if (length(invalid.dimensions)>0)
        stop(paste0("Invalid exclude.dimension(s): ",
                    paste0("'", invalid.dimensions, "'", collapse=', '),
                    ". exclude.dimensions must be a subset of ",
                    paste0("'", names(TRANSITION.MAPPING.SCHEMA), "'", collapse=', ')))
    
    if (!is.character(rename.states) || is.null(rename.states) || any(names(rename.states)==''))
        stop("rename.states must be a named character vector")
    
    rv = template
    
    unused.rename.states = names(rename.states)
    for (subgroup in names(template$by.subgroup))
    {
        # exclude dimensions
        rv$by.subgroup[[subgroup]][exclude.dimensions] = NULL
        
        # rename states
        for (dimension in names(rv$by.subgroup[[subgroup]]))
        {
            n.trans = length(rv$by.subgroup[[subgroup]][[dimension]]$transitions)
            if (n.trans > 0)
            {
                for (i in 1:n.trans)
                {
                    tr = rv$by.subgroup[[subgroup]][[dimension]]$transitions[[i]]
                    if (any(names(rename.states)==tr$from))
                    {
                        rv$by.subgroup[[subgroup]][[dimension]]$transitions[[i]]$from = as.character(rename.states[tr$from])
                        unused.rename.states = setdiff(unused.rename.states, tr$from)
                    }
                    if (any(names(rename.states)==tr$to))
                    {
                        rv$by.subgroup[[subgroup]][[dimension]]$transitions[[i]]$to = as.character(rename.states[tr$to])
                        unused.rename.states = setdiff(unused.rename.states, tr$to)
                    }
                }
            }
        }
    }
    
    if (length(unused.rename.states)>0)
        stop(paste0("The following state(s) were not present in template, and cannot be renames: ",
                    paste0("'", unused.rename.states, "'", collapse=', ')))
    

    # version
    rv$version = version
    
    rv
}

#'@param rate A value specifying how to calculate the rate to be applied. It can be either
#'             (1) a character scalar with the name of a rate
#'             (2) an expression 
register.transition <- function(transition.mapping,
                                            dimension,
                                            from.state,
                                            to.state,
                                            rate,
                                            label,
                                            subgroups=TRANSITION.MAPPING.SCHEMA[[dimension]]$subgroups)
{
    if (!is(transition.mapping, 'transition.mapping'))
        stop("transition.mapping must be an object of class 'transition.mapping'")
    

    transition = create.transition(dimension=dimension,
                                   from=from.state,
                                   to=to.state,
                                   rate=rate,
                                   subgroups=subgroups,
                                   label=label)
    
    for (subgroup in subgroups)
    {
        if (is.null(transition.mapping$by.subgroup[[subgroup]][[dimension]]))
            transition.mapping$by.subgroup[[subgroup]][[dimension]] = list(transitions=list())
    
        matching.from.to = sapply(transition.mapping$by.subgroup[[subgroup]][[dimension]]$transitions, function(tr){
            tr$from==from.state && tr$to==to.state
        })        
        if (any(matching.from.to))
            stop(paste0("A mapping has already been created for transitions from '", from.state,
                        "' to '", to.state, "' in dimension '",
                        dimension, "' for subgroup '", subgroup, "'"))
        
        transition.mapping$by.subgroup[[subgroup]][[dimension]]$transitions = c(
            transition.mapping$by.subgroup[[subgroup]][[dimension]]$transitions,
            list(transition)
        )
    }
    
    for (element.name in transition$element.names)
    {
        if (is.null(transition.mapping$elements.to.subgroups.and.dimensions[[element.name]]))
            transition.mapping$elements.to.subgroups.and.dimensions[[element.name]] = list(
                subgroup=character(),
                dimension=character()
            )
        
        transition.mapping$elements.to.subgroups.and.dimensions[[element.name]]$subgroup = c(
            transition.mapping$elements.to.subgroups.and.dimensions[[element.name]]$subgroup,
            subgroups
        )
        
        transition.mapping$elements.to.subgroups.and.dimensions[[element.name]]$dimension = c(
            transition.mapping$elements.to.subgroups.and.dimensions[[element.name]]$dimension,
            rep(dimension, length(subgroups))
        )
    }
    
    transition.mapping
}

get.transitions <- function(transition.mapping,
                            subgroup=subgroup,
                            dimension=dimension)
{
    if (!is(transition.mapping, 'transition.mapping'))
        stop("transition.mapping must be an object of class 'transition.mapping'")
    
    
    if (!is.character(subgroup) || length(subgroup)!=1 || is.na(subgroup))
        stop("subgroup must be a non.na, single character value")
    
    if (!is.character(dimension) || length(dimension)!=1 || is.na(dimension))
        stop("dimension must be a non.na character scalar")
    
    if (all(dimension != names(TRANSITION.MAPPING.SCHEMA)))
        stop(paste0("Invalid dimension ('", dimension, "') - must be one of: ",
                    paste0("'", names(TRANSITION.MAPPING.SCHEMA), "'", collapse=', ')))
    
    if (all(subgroup != TRANSITION.MAPPING.SCHEMA[[dimension]]$subgroups))
        stop(paste0("'", subgroup, "' is not a valid subgroup for transitions in the '", dimension, "' dimension"))

    rv = transition.mapping$by.subgroup[[subgroup]][[dimension]]$transitions
    if (length(rv)==0)
        stop(paste0("No transitions have been set up in the '", dimension, "' for '", subgroup, "'"))
    
    rv
}

get.transitions.by.labels <- function(transition.mapping,
                                     labels)
{
    if (!is(transition.mapping, 'transition.mapping'))
        stop("transition.mapping must be an object of class 'transition.mapping'")
    
    rv = list()
    {
        for (subgroup in names(transition.mapping$by.subgroup))
        {
            for (dim in names(transition.mapping$by.subgroup[[subgroup]]))
            {
                mask = sapply(transition.mapping$by.subgroup[[subgroup]][[dim]]$transitions, function(tr){
                    any(tr$label == labels)
                })
                
                rv = c(rv, transition.mapping$by.subgroup[[subgroup]][[dim]]$transitions[mask])
            }
        }
    }
    
    rv
}

get.transitions.by.to.from <- function(transition.mapping,
                                       dimension,
                                       subgroup,
                                       to,
                                       from)
{
    if (!is(transition.mapping, 'transition.mapping'))
        stop("transition.mapping must be an object of class 'transition.mapping'")
    
    
    if (!is.character(subgroup) || length(subgroup)!=1 || is.na(subgroup))
        stop("subgroup must be a non.na, single character value")
    
    if (!is.character(dimension) || length(dimension)!=1 || is.na(dimension))
        stop("dimension must be a non.na character scalar")
    
    if (all(dimension != names(TRANSITION.MAPPING.SCHEMA)))
        stop(paste0("Invalid dimension ('", dimension, "') - must be one of: ",
                    paste0("'", names(TRANSITION.MAPPING.SCHEMA), "'", collapse=', ')))
    
    if (all(subgroup != TRANSITION.MAPPING.SCHEMA[[dimension]]$subgroups))
        stop(paste0("'", subgroup, "' is not a valid subgroup for transitions in the '", dimension, "' dimension"))
    
    mask = sapply(transition.mapping$by.subgroup[[subgroup]][[dimension]]$transitions, function(tr){
        any(tr$from == from) && any(tr$to==to)
    })
    
    transition.mapping$by.subgroup[[subgroup]][[dimension]]$transitions[mask]
}

register.transition.element <- function(transition.mapping,
                                        name,
                                        type,
                                        background.model.type=type,
                                        ramp.type=type,
                                        return.type=type,
                                        required=is.null(default.value),
                                        default.value=NULL,
                                        ramp.times=numeric(),
                                        ramp.multipliers=numeric(),
                                        ramp.interpolate.scales='identity',
                                        model.source=NULL)
{
    if (!is(transition.mapping, 'transition.mapping'))
        stop("transition.mapping must be an object of class 'transition.mapping'")
    
    tr.el = create.transition.element(name=name,
                                      type=type,
                                      background.model.type=background.model.type,
                                      ramp.type=ramp.type,
                                      return.type=return.type,
                                      required=required,
                                      default.value=default.value,
                                      ramp.times=ramp.times,
                                      ramp.multipliers=ramp.multipliers,
                                      ramp.interpolate.scales=ramp.interpolate.scales,
                                      model.source=model.source)
    
    if (any(names(transition.mapping$transition.elements)==name))
        stop(paste0("A transition element with name '", name, '" has already been registered.'))
    
    transition.mapping$transition.elements[[name]] = tr.el
    
    transition.mapping
}

get.transition.elements <- function(transition.mapping,
                                    transitions=NULL,
                                    element.names=NULL)
{
    if (!is(transition.mapping, 'transition.mapping'))
        stop("transition.mapping must be an object of class 'transition.mapping'")
    
    if (length(transitions)==0)
    {
        if (length(element.names)==0)
            stop("You must specify either transitions or element.names")
        if (!is.character(element.names) || any(is.na(element.names)))
            stop("element.names must be a non-na character vector")
        
        lapply(element.names, get.transition.element.by.name, transition.mapping=transition.mapping)
    }
    else
    {
        get.transition.elements(transition.mapping, element.names=get.transition.element.names(transitions))
    }
}

get.all.transition.elements <- function(transition.mapping)
{
    if (!is(transition.mapping, 'transition.mapping'))
        stop("transition.mapping must be an object of class 'transition.mapping'")
    
    transition.mapping$transition.elements
}

get.transition.element.by.name <- function(transition.mapping,
                                           name,
                                           allow.missing=F)
{
    if (!is(transition.mapping, 'transition.mapping'))
        stop("transition.mapping must be an object of class 'transition.mapping'")
    
    if (all(names(transition.mapping$transition.elements)!=name))
    {
        if (allow.missing)
            NULL
        else
            stop(paste0("No transition element with name '", name, "' has been registered."))
    }
    else
        transition.mapping$transition.elements[[name]]
}

##------------------------------------------------------##
##--                TRANSITION OBJECTS                --##
##                                                      ##
##-- The information on how to put elements together  --##
##-- to make transition rates between compartments    --##
##------------------------------------------------------##

create.transition <- function(dimension,
                              from,
                              to,
                              rate,
                              subgroups,
                              label)
{
    if (!is.character(dimension) || length(dimension)!=1 || is.na(dimension))
        stop("dimension must be a non.na character scalar")
    
    if (!any(dimension==names(TRANSITION.MAPPING.SCHEMA)))
        stop(paste0("'", dimension, "' is not a valid dimension for the transition.mapping"))
    
    
    if (!is.character(subgroups) || length(subgroups)==0 || any(is.na(subgroups)))
        stop("subgroups must be a non.na character vector")
    
    invalid.subgroups = setdiff(subgroups, TRANSITION.MAPPING.SCHEMA[[dimension]]$subgroups)
    if (length(invalid.subgroups)>0)
        stop(paste0("'", invalid.subgroups, "' is not a valid subgroup for transitions in the '", dimension, "' dimension"))
    
    
    if (!is.character(from) || length(from)!=1 || is.na(from))
        stop("from must be a non.na character scalar")
    
    if (!is.character(to) || length(to)!=1 || is.na(to))
        stop("to must be a non.na character scalar")
    
    
    if (is.expression(rate))
    {
        rate.type = 'expression'
        element.names = all.vars(rate)
    }
    else if (is.character(rate))
    {
        rate.type = 'character'
        element.names = rate
    }
    else
        stop("rate for a transition must be either a single character or a single expression")
    
    if (length(rate)!=1)
        stop("rate for a transition must be a single value")
    
    
    if (!is.character(label) || length(label)!=1 || is.na(label))
        stop("label must be a non-na character scalar")
    
    rv = list(dimension = dimension,
              from = from,
              to = to,
              rate = rate,
              rate.type = rate.type,
              element.names = element.names,
              subgroups = subgroups,
              label = label
    )
    
    class(rv) = 'transition'
    
    rv
}

resolve.transition <- function(transition, bindings)
{
    if (!is(transition, 'transition'))
        stop("'transition' must be an object of class transition")
    
    if (!is.list(bindings) || is.null(names(bindings)))
        stop("'bindings' must be a named list")
    
    missing.elements = setdiff(transition$element.names, names(bindings))
    if (length(missing.elements)>0)
        stop(paste0("The following element(s) are not contained in bindings: ",
                    paste0("'", missing.elements, "'", collapse=', '),
                    ". Cannot resolve transition."))
    
    if (transition$rate.type=='character')
        bindings[[transition$rate]]
    else if (transition$rate.type=='expression')
        eval(transition$rate, envir=bindings)
    else
        stop("Invalid transition state: rate.type must be either 'character' or 'expression'")
}

get.transition.element.names <- function(transitions)
{
    if (!is(transitions, 'transition') && 
        (!is.list(transitions) || !all(sapply(transitions, function(tr){is(tr, 'transition')}))))
        stop("transitions must be either an object of class 'transition' or a list containing only transition objects")
    
    if (is.list(transitions))
        unique(unlist(lapply(transitions, function(tr){tr$element.names})))
    else
        transitions$element.names
}



##----------------------------##
##-- CHECK AGAINST SETTINGS --##
##----------------------------##

check.transition.mapping.against.settings <- function(transition.mapping,
                                                      settings)
{
    if (!is(transition.mapping, 'transition.mapping'))
        stop("transition.mapping must be an object of class 'transition.mapping'")
    
    if (transition.mapping$version != settings$VERSION)
        stop(paste0("version for settings ('", settings$VERSION, 
                    "') does not match version for transition.mapping ('",
                    transition.mapping$version, "')"))
    
    for (subgroup in TRANSITION.MAPPING.SUBGROUPS)
    {
        required.dimension.mask = sapply(TRANSITION.MAPPING.SCHEMA, function(sch){
            any(sch$subgroups==subgroup) && sch$always.required
        })
        
        # Make sure none of the always-required dimensions are missing
        required.dimensions = names(TRANSITION.MAPPING.SCHEMA)[required.dimension.mask]
        missing.dimensions = setdiff(required.dimensions, names(transition.mapping$by.subgroup[[subgroup]]))
        if (length(missing.dimensions)>0)
            stop(paste0("No transitions have been set for dimension(s) ",
                        paste0("'", missing.dimensions, "'", collapse=', '),
                        " for subgroup '", subgroup, "'"))
        
        for (dimension in names(transition.mapping$by.subgroup[[subgroup]]))
        {
            states.for.dim = settings[[TRANSITION.MAPPING.SCHEMA[[dimension]]$settings.name]]
            for (transition in transition.mapping$by.subgroup[[subgroup]][[dimension]]$transitions)
            {
                if (all(transition$from != states.for.dim))
                    stop(paste0("'", transition$from, "' is not a valid from state for the '", dimension, "' dimension"))
                if (all(transition$to != states.for.dim))
                    stop(paste0("'", transition$to, "' is not a valid to state for the '", dimension, "' dimension"))
            }
        }
    }
    
    transition.elements.from.transitions = unique(unlist(sapply(transition.mapping$by.subgroup, function(by.subgroup){
        unlist(sapply(by.subgroup, function(by.dim){
            sapply(by.dim$transitions, function(transition){
                transition$element.names
            })
        }))
    })))
    
    missing.from.transitions = setdiff(transition.elements.from.transitions, 
                                       names(transition.mapping$transition.elements))
    if (length(missing.from.transitions)>0)
        stop(paste0("The following transition element(s) have not been registered with the transition.mapping: ",
                    paste0("'", missing.from.transitions, "'", collapse=', ')))
    
    unused.elements = setdiff(names(transition.mapping$transition.elements),
                              transition.elements.from.transitions)
    if (length(unused.elements)>0)
        print(paste0("WARNING: The following transition element(s) have been registered, but are not used in any transitions: ",
                     paste0("'", unused.elements, "'", collapse=', ')))
    
    for (elem.name in names(transition.mapping$transition.elements))
    {
        dimensions = calculate.transition.element.dimensions(transition.mapping,
                                                             elem.name=elem.name,
                                                             settings = settings)
        
        transition.mapping$transition.elements[[elem.name]]$dimensions = dimensions
        
        transition.mapping$transition.elements[[elem.name]]$dim.names = settings$DIMENSION.NAMES[dimensions]
        
        tr.el = transition.mapping$transition.elements[[elem.name]]
        if (!is.null(tr.el$default.value))
        {
            if (is.null(dim(tr.el$default.value)))
            {
                if (length(tr.el$default.value) != 1 || !is.numeric(tr.el$default.value) || is.na(tr.el$default.value))
                    stop(paste0("The default value - given for '",
                                elem.name, "' must either be an array or a scalar, numeric, non-NA value"))
            }
            else if (!named.lists.equal(dimnames(tr.el$default.value), tr.el$dim.names))
            {
                if (!is.named.list.subset(dimnames(tr.el$default.value),
                                          tr.el$dim.names))
                    stop(paste0("The dimensions for the default value for '", elem.name,
                                "' are not a subset of the expected dimensions"))
                
                tr.el$default.value = expand.population(tr.el$default.value, target.dim.names = tr.el$dim.names)
            }
        }
    }
    
    transition.mapping
}

# the dim.names for an element is an intersection of the dim.names for that element in each subgroup
# the dim.names for an element in a subgroup is the union of dim.
calculate.transition.element.dimensions <- function(transition.mapping,
                                                   elem.name,
                                                   settings)
{
    subgroups = transition.mapping$elements.to.subgroups.and.dimensions[[elem.name]]$subgroup
    
    rv = names(settings$DIMENSION.NAMES)
    
    for (sub in unique(subgroups))
    {
        dimensions = transition.mapping$elements.to.subgroups.and.dimensions[[elem.name]]$dimension[subgroups==sub]
        if (length(dimensions)>1)
            rv = intersect(rv, names(settings$dimension.names.by.subgroup[[sub]]))
        else
            rv = intersect(rv, setdiff(names(settings$dimension.names.by.subgroup[[sub]]),
                                       dimensions))
    }
    
    rv
    
}
