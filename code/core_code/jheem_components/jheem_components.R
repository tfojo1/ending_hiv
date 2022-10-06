
##-- JHEEM COMPONENTS --##

## A JHEEM components object contains all the elements needed to set up a JHEEM Object
## Since parameters cannot be REMOVED from a JHEEM object once it is set up, we use
##  a components object to hold all the parameters, or variables which are used to 
##  create parameters, which we may change some or all of.
## Then at the last minute, we crunch all the value sin components and
##  create a JHEEM object from it

## There are, in general, two kinds of things we set in a jheem.components object:
## 1) Parameters - these are specific values that are set (eg a transmission rate),
##    and generally don't require data pulls from data managers
## 2) Data - these are more complex data structures, like underlying populations or 
##    population-specific rates, that typically require a data pull from a data manager,
##    and are generally not set by directly passing a value. We generally expect that these
##    will be set up once for components; although they can be re-set, we gear our
##    coding efficiency on the assumption that they will only be set up once
## 
## 
## The major elements of the components object are:
## $settings
## $jheem
## $element.backgrounds
## $element.foregrounds
## $calculated.elements
## $calculated.quantities

##---------------------##
##-- THE CONSTRUCTOR --##
##---------------------##

#'@export
create.jheem.components <- function(version,
                                    location)
{
    #-- Check arguments --#
    if (!is.character(location) || length(location)!=1 || is.na(location))
        stop("'location' must be a single, non-NA character value")
    
    #-- Pull transition mapping --#
    specification = get.specification.for.version(version)
    if (is.null(specification))
        stop(paste0("The version '", version, "' has not been registered with the version.manager"))
    transition.mapping = specification$transition.mapping
    
    #-- Initialize the rv --#
    rv = list(settings=list(version = version,
                            code.iteration = CURRENT.CODE.ITERATION,
                            location = location,
                            fixed = F),
              jheem=NULL,
              element.backgrounds=list(),
              element.foregrounds=list(),
              calculated.elements = list(),
              calculated.quantities = list()
              )   
    
    class(rv) = 'jheem.components'
    
    #-- Set up background for model elements --#
    
    element.names = get.model.element.names(transition.mapping)
    rv$element.backgrounds = lapply(element.names, setup.model.element.background, 
                                    location=location, specification=specification)
    names(rv$element.backgrounds) = element.names
    
    
    #-- Set Class and Return --#
    rv
}


##----------------------------------##
##-- FUNCTIONS TO MODIFY SETTINGS --##
##----------------------------------##


##---------------------------------------------------##
##-- FORWARD-FACING FUNCTIONS to MODIFY PARAMETERS --##
##---------------------------------------------------##

#'@export
set.model.element.value <- function(components,
                                    element.name,
                                    value,
                                    check.arguments=T)
{
    # Check arguments
    if (!is(components, 'jheem.components'))
        stop("'components' must be an object of class 'jheem.components'")
    
    if (check.arguments)
    {
        if (!is.character(element.name) || length(element.name)!=1 || is.na(element.name))
            stop("'element.name' must be a single, non-NA, character value")
        
        if (all(names(components$element.backgrounds)!=element.name))
            stop("No element named '", element.name, "' exists in this components object")
        
        if (!is.null(components$element.backgrounds[[element.name]]$model))
            stop(paste0("Cannot set value for element '", element.name,
                 "': a model has been specified; set.model.element.value only applies when there is no model"))
        
        # Check value dimensions
        if (!is.numeric(value))
            stop(paste0("Cannot set value for element '", element.name,
                 "': value must be a numeric object"))
        
        if (!value.fits.into.dim.names(value, element$dim.names))
            stop(paste0("Cannot set value for element '", element.name,
                 "': the given value's dimensions are not a subset of the expected model.element dimensions"))
    }
    
    # Set and Return
    components$element.backgrounds[[element.name]]$model$value = value
    component = clear.components.dependencies(components, element.name)
    components
}

#'@export
set.model.element.model.alphas <- function(components,
                                           element.name,
                                           alpha.name,
                                           values,
                                           applies.to.dimension.values=names(values),
                                           dimensions,
                                           as.interaction=F,
                                           check.arguments=T)
{
    # Check arguments
    if (!is(components, 'jheem.components'))
        stop("'components' must be an object of class 'jheem.components'")
    
    
    error.prefix = paste0("Cannot set model alphas for element '", element.name, "': ")
    model = components$element.backgrounds[[element.name]]$model
    if (check.arguments)
    {
        #-- Check valid element with a model --#
        if (!is.character(element.name) || length(element.name)!=1 || is.na(element.name))
            stop("'element.name' must be a single, non-NA, character value")
        
        if (all(names(components$element.backgrounds)!=element.name))
            stop("No element named '", element.name, "' exists in this components object")
        
        if (is.null(model))
            stop(paste0(error.prefix, "
                        a model has not been specified; set.model.element.model.alphas only applies for elements that have a model set"))
        
        if (all(get.alpha.names.for.model(model) != alpha.name))
            stop(paste0(error.prefix,
                        "'", alpha.name, 
                        "' is not the name of a valid alpha for this model of type '",
                        model$type, "'"))
    }
    
    transition.mapping = get.components.transition.mapping(components)
    model.element = get.model.element(transition.mapping)
    
    if (is.null(components$element.backgrounds[[element.name]]$alphas))
        components$element.backgrounds[[element.name]]$alphas = list()
    
    if (is.null(components$element.backgrounds[[element.name]]$alphas[[alpha.name]]))
        alphas = create.alphas.object(dim.names=model.element$dim.names)
    else
        alphas = components$element.backgrounds[[element.name]]$alphas[[alpha.name]]
    
    # Set the value
    if (as.interaction)
        components$element.backgrounds[[element.name]]$alphas[[alpha.name]] = set.alpha.interaction.values(alphas,
                                                                                                           dimensions = dimensions,
                                                                                                           dimension.values = dimension.values,
                                                                                                           value = values,
                                                                                                           check.arguments = check.arguments,
                                                                                                           error.prefix = error.prefix)
    else
        
        components$element.backgrounds[[element.name]]$alphas[[alpha.name]] = set.alpha.main.effect.values(alphas,
                                                                                                           dimensions = dimensions,
                                                                                                           dimension.values = dimension.values,
                                                                                                           value = values,
                                                                                                           check.arguments = check.arguments,
                                                                                                           error.prefix = error.prefix)
    
    
    # Return
    component = clear.components.dependencies(components, element.name)
    components
}

#'@export
set.model.element.foreground <- function(components,
                                         intervention,
                                         check.arguments=T)
{
    # Check arguments
    if (!is(components, 'jheem.components'))
        stop("'components' must be an object of class 'jheem.components'")
    
    if (!is(intervention, 'intervention'))
        stop("'intervention' must be an object of class 'intervention'")
    

    # Profit
}

#'@export
set.model.element.model.from.time <- function(components,
                                              element.name,
                                              from.time,
                                              check.arguments=T)
{
    # Check arguments
    if (!is(components, 'jheem.components'))
        stop("'components' must be an object of class 'jheem.components'")
    
    if (check.arguments)
    {
        if (!is.character(element.name) || length(element.name)!=1 || is.na(element.name))
            stop("'element.name' must be a single, non-NA, character value")
        
        if (all(names(components$element.backgrounds)!=element.name))
            stop("No element named '", element.name, "' exists in this components object")
        
        if (is.null(components$element.backgrounds[[element.name]]$model))
            stop("Cannot set model from.time for element '", element.name,
                 "': a model has not been specified; set.model.element.model.from.time only applies for elements that have a model set")
        
        # Check from time
        if (!is.numeric(from.time) || length(from.time) != 1 || is.na(from.time))
            stop(paste0("Cannot set model from.time for element '", element.name,
                        "'. from.time must be a single, non-NA, numeric value"))
        
        current.year = as.numeric(format(Sys.Date(), "%Y"))
        if (from.time < 1970 || from.time > current.year)
            stop(paste0("Cannot set model from.time for element '", element.name,
                        "'. from.time (",
                        from.time, 
                        ") must be between ", MIN.MODEL.FROM.YEAR,
                        " and ", current.year))
        
        if (from.time > components$element.backgrounds[[element.name]]$model.to.time)
            stop(paste0("Cannot set model from.time for element '", element.name,
                        "'. from.time (",
                        from.time, 
                        ") must be a less than or equal to the previously specified 'to.time' (",
                        components$element.backgrounds[[element.name]]$model.to.time, ")"))
        
        if (!is.null(components$element.backgrounds[[element.name]]$ramp.times) &&
            from.time <= components$element.backgrounds[[element.name]]$ramp.times)
            stop(paste0("Cannot set model from.time for element '", element.name,
                        "'.  from.time (",
                        from.time, 
                        ") must be a less than or equal to the previously set ramp.times (",
                        components$element.backgrounds[[element.name]]$ramp.times[length(components$element.backgrounds[[element.name]]$ramp.times)],
                        ")"))
    }
    
    # Set and Return
    components$element.backgrounds[[element.name]]$model.from.time = from.time
    component = clear.components.dependencies(components, element.name)
    components
}

#'@export
set.model.element.model.to.time <- function(components,
                                            element.name,
                                            to.time,
                                            check.arguments=T)
{
    # Check arguments
    if (!is(components, 'jheem.components'))
        stop("'components' must be an object of class 'jheem.components'")
    
    if (check.arguments)
    {
        if (!is.character(element.name) || length(element.name)!=1 || is.na(element.name))
            stop("'element.name' must be a single, non-NA, character value")
        
        if (all(names(components$element.backgrounds)!=element.name))
            stop(paste0("No element named '", element.name, "' exists in this components object"))
        
        if (is.null(components$element.backgrounds[[element.name]]$model))
            stop(paste0("Cannot set model to.time for element '", element.name,
                 "': a model has not been specified; set.model.element.model.to.time only applies for elements that have a model set"))
        
        # Check from time
        if (!is.numeric(to.time) || length(to.time) != 1 || is.na(to.time))
            stop(paste0("Cannot set model to.time for element '", element.name,
                        "'. to.time' must be a single, non-NA, numeric value"))
        
        
        if (to.time < components$element.backgrounds[[element.name]]$model.from.time)
            stop(paste0("Cannot set model to.time for element '", element.name,
                        "'. to.time (",
                        to.time, 
                        ") must be a less than or equal to the previously specified 'from.time' (",
                        components$element.backgrounds[[element.name]]$model.from.time, ")"))
        
        
        if (!is.null(components$element.backgrounds[[element.name]]$taper.times) &&
            to.time >= components$element.backgrounds[[element.name]]$taper.times)
            stop(paste0("Cannot set model to.time for element '", element.name,
                        "'.  to.time (",
                        to.time, 
                        ") must be a less than or equal to the previously set taper.times (",
                        components$element.backgrounds[[element.name]]$taper.times[1], ")"))
        
    }
    
    # Set and Return
    components$element.backgrounds[[element.name]]$model.to.time = to.time
    component = clear.components.dependencies(components, element.name)
    components
}

#'@export
set.model.element.ramp.times <- function(components,
                                         element.name,
                                         times,
                                         indices,
                                         check.arguments=T)
{
    # Check arguments
    if (!is(components, 'jheem.components'))
        stop("'components' must be an object of class 'jheem.components'")
    
    if (check.arguments)
    {
        if (!is.character(element.name) || length(element.name)!=1 || is.na(element.name))
            stop("'element.name' must be a single, non-NA, character value")
        
        if (all(names(components$element.backgrounds)!=element.name))
            stop("No element named '", element.name, "' exists in this components object")
        
        if (is.null(components$element.backgrounds[[element.name]]$model))
            stop(paste0("Cannot set ramp.times for element '", element.name,
                 "': a model has not been specified; set.model.element.ramp.times only applies for elements that have a model set"))
        
        if (is.null(components$element.backgrounds[[element.name]]$ramp.multipliers))
            stop(paste0("Cannot set ramp.times for element '", element.name,
                        "': a ramp has not been specified for this element"))
        
        
        indices = check.ramp.or.taper.values.and.indices(values=times,
                                                         indices=indices,
                                                         current.values=components$element.backgrounds[[element.name]]$ramp.times,
                                                         is.ramp=T,
                                                         is.times=T,
                                                         error.prefix=paste0("Cannot set ramp.times for element '",
                                                                             element.name, "': "))
    }
    
    # Set and check ordering
    components$element.backgrounds[[element.name]]$ramp.times[indices] = times

    if (check.arguments)
    {
        if (!all(components$element.backgrounds[[element.name]]$ramp.times==
                 sort(components$element.backgrounds[[element.name]]$ramp.times)))
            stop(paste0("Cannot set ramp.times for element '", element.name,
                 "': The ramp.times are not in ascending order"))
        
        if (components$element.backgrounds[[element.name]]$ramp.times[length(components$element.backgrounds[[element.name]]$ramp.times)] >=
            components$element.backgrounds[[element.name]]$model.from.time)
            stop(paste0("Cannot set ramp.times for element '", element.name,
                 "': All ramp.times must be BEFORE the previously set model.from.time (",
                 components$element.backgrounds[[element.name]]$model.from.time, ")"))
    }
    
    # Clear dependencies and Return
    component = clear.components.dependencies(components, element.name)
    components
}

#'@export
set.model.element.ramp.multipliers <- function(components,
                                               element.name,
                                               multipliers,
                                               indices,
                                               check.arguments=T)
{
    # Check arguments
    if (!is(components, 'jheem.components'))
        stop("'components' must be an object of class 'jheem.components'")
    
    if (check.arguments)
    {
        if (!is.character(element.name) || length(element.name)!=1 || is.na(element.name))
            stop("'element.name' must be a single, non-NA, character value")
        
        if (all(names(components$element.backgrounds)!=element.name))
            stop("No element named '", element.name, "' exists in this components object")
        
        if (is.null(components$element.backgrounds[[element.name]]$model))
            stop(paste0("Cannot set ramp.multipliers for element '", element.name,
                        "': a model has not been specified; set.model.element.ramp.multipliers only applies for elements that have a model set"))
        
        if (is.null(components$element.backgrounds[[element.name]]$ramp.multipliers))
            stop(paste0("Cannot set ramp.multipliers for element '", element.name,
                        "': a ramp has not been specified for this element"))
        
        
        indices = check.ramp.or.taper.values.and.indices(values=times,
                                                         indices=indices,
                                                         current.values=components$element.backgrounds[[element.name]]$ramp.times,
                                                         is.ramp=T,
                                                         is.times=F,
                                                         error.prefix=paste0("Cannot set ramp.multipliers for element '",
                                                                             element.name, "': "))
    }
    
    # Set and Return
    components$element.backgrounds[[element.name]]$ramp.multipliers[indices] = multipliers
    component = clear.components.dependencies(components, element.name)
    components
}

#'@export
set.model.element.taper.times <- function(components,
                                         element.name,
                                         times,
                                         indices,
                                         check.arguments=T)
{
    # Check arguments
    if (!is(components, 'jheem.components'))
        stop("'components' must be an object of class 'jheem.components'")
    
    if (check.arguments)
    {
        if (!is.character(element.name) || length(element.name)!=1 || is.na(element.name))
            stop("'element.name' must be a single, non-NA, character value")
        
        if (all(names(components$element.backgrounds)!=element.name))
            stop("No element named '", element.name, "' exists in this components object")
        
        if (is.null(components$element.backgrounds[[element.name]]$model))
            stop(paste0("Cannot set taper.times for element '", element.name,
                        "': a model has not been specified; set.model.element.taper.times only applies for elements that have a model set"))
        
        if (is.null(components$element.backgrounds[[element.name]]$taper.multipliers))
            stop(paste0("Cannot set taper.times for element '", element.name,
                        "': a taper has not been specified for this element"))
        
        
        indices = check.ramp.or.taper.values.and.indices(values=times,
                                                         indices=indices,
                                                         current.values=components$element.backgrounds[[element.name]]$taper.times,
                                                         is.ramp=F,
                                                         is.times=T,
                                                         error.prefix=paste0("Cannot set taper.times for element '",
                                                                             element.name, "': "))
    }
    
    # Set and check ordering
    components$element.backgrounds[[element.name]]$taper.times[indices] = times
    
    if (check.arguments)
    {
        if (!all(components$element.backgrounds[[element.name]]$taper.times==
                 sort(components$element.backgrounds[[element.name]]$taper.times)))
            stop(paste0("Cannot set taper.times for element '", element.name,
                        "': The taper.times are not in ascending order"))
        
        if (components$element.backgrounds[[element.name]]$taper.times[1] <=
            components$element.backgrounds[[element.name]]$model.to.time)
            stop(paste0("Cannot set taper.times for element '", element.name,
                        "': All taper.times must be AFTER the previously set model.to.time (",
                        components$element.backgrounds[[element.name]]$model.to.time, ")"))
    }
    
    # Clear dependencies and Return
    component = clear.components.dependencies(components, element.name)
    components
}

#'@export
set.model.element.taper.multipliers <- function(components,
                                               element.name,
                                               multipliers,
                                               indices,
                                               check.arguments=T)
{
    # Check arguments
    if (!is(components, 'jheem.components'))
        stop("'components' must be an object of class 'jheem.components'")
    
    if (check.arguments)
    {
        if (!is.character(element.name) || length(element.name)!=1 || is.na(element.name))
            stop("'element.name' must be a single, non-NA, character value")
        
        if (all(names(components$element.backgrounds)!=element.name))
            stop("No element named '", element.name, "' exists in this components object")
        
        if (is.null(components$element.backgrounds[[element.name]]$model))
            stop(paste0("Cannot set taper.multipliers for element '", element.name,
                        "': a model has not been specified; set.model.element.taper.multipliers only applies for elements that have a model set"))
        
        if (is.null(components$element.backgrounds[[element.name]]$taper.multipliers))
            stop(paste0("Cannot set taper.multipliers for element '", element.name,
                        "': a taper has not been specified for this element"))
        
        
        indices = check.ramp.or.taper.values.and.indices(values=times,
                                                         indices=indices,
                                                         current.values=components$element.backgrounds[[element.name]]$taper.times,
                                                         is.ramp=T,
                                                         is.times=F,
                                                         error.prefix=paste0("Cannot set taper.multipliers for element '",
                                                                             element.name, "':"))
    }
    
    # Set and Return
    components$element.backgrounds[[element.name]]$taper.multipliers[indices] = multipliers
    component = clear.components.dependencies(components, element.name)
    components
}


##------------------------------------##
##-- UNDER-THE-HOOD SETUP FUNCTIONS --##
##------------------------------------##

# returns an element.background object
setup.model.element.background <- function(element.name,
                                           location,
                                           specification)
{
    transition.mapping = specification$transition.mapping
    rv = get.model.element(transition.mapping, element.name = element.name)
    
    # set up the value 
    rv$alue = get.model.element.value(rv,
                                      location=location,
                                      specification=specification)
    
    # set up the model
    rv$model = get.model.element.model(rv,
                                       location=location,
                                       specification=specification)
    
    rv
}

##-------------##
##-- HELPERS --##
##-------------##

#returns a vector of validated indices
check.ramp.or.taper.values.and.indices <- function(values,
                                                   indices,
                                                   current.values,
                                                   is.ramp,
                                                   is.times,
                                                   error.prefix='')
{
    # Set up for error messages
    if (is.ramp)
        ramp.or.taper = 'ramp'
    else
        ramp.or.taper = 'taper'
    
    if (is.times)
        values.or.times = "'times'"
    else
        values.or.times = "'multipliers'"
    
    # Check values
    if (!is.numeric(values) || length(values)==0 || any(is.na(values)))
        stop(paste0(error.prefix, values.or.times, " must be a non-empty, non-NA, numeric vector"))
    if (length(values) > length(current.values))
        stop(paste0(error.prefix, values.or.times, " has ",
                    length(values), " elements, but the ",
                    ramp.or.taper, " only takes ",
                    length(current.values), " ", values.or.times,
                    ifelse(length(current.values==1),
                           "",
                           "s"),
                    "."))
    if (any(values<0))
        stop(paste0(error.prefix, values.or.times, " must all be greater than or equal to zero"))
    
    
    # Infer indices if none specified
    if (is.null(indices))
    {
        if (is.null(names(values)) || is.null(names(current.values)))
            indices = 1:length(values)
        else
            indices = names(values)
    }
    
    # Check indices
    if (is.character(indices))
    {
        if (is.null(names(current.values)))
            stop(paste0(error.prefix, 
                        "'indices' cannot be a character value for this element, because the ",
                        ramp.or.taper,
                        " elements have not been given names."))
        
        invalid.names = setdiff(indices, names(current.values))
        if (length(invalid.names)>0)
            stop(paste0(error.prefix,
                        "Invalid name(s) in indices. None of the ", ramp.or.taper,
                        " elements is named ",
                        paste0.with.conjunction(invalid.names)))
        
        if (length(indices) != length(values))
            stop(paste0(error.prefix,
                        "'indices' has length ", length(indices),
                        " but ", values.or.times, " has length ", length(values)))
    }
    else if (is.integer(indices))
    {
        if (any(indices<1) || any(indices>length(current.values)))
            stop(paste0(error.prefix,
                        "indices out of bounds: ",
                        paste0.with.conjuction(indices[indices<0 | indices>length(current.values)],
                                               quote=F, conjunction='and')))
        
        if (length(indices) != length(values))
            stop(paste0(error.prefix,
                        "'indices' has length ", length(indices),
                        " but ", values.or.times, " has length ", length(values)))
    }
    else if (is.logical(indices))
    {
        if (length(indices) != length(current.values))
            stop(paste0(error.prefix,
                        "'indices' (as a logical vector) has length ", length(indices),
                        " but the ", ramp.or.taper,
                        " takes ", length(current.values), " ", values.or.times,
                        ifelse(length(current.values)==1,
                               "",
                               "s"),
                        "."))
        
        if (sum(indices) != length(values))
            stop(paste0(error.prefix,
                        "'indices' (as a logical vector) references ", sum(indices),
                        " elements, but the ", ramp.or.taper,
                        " takes ", length(current.values), " ", values.or.times,
                        ifelse(length(current.values)==1,
                               "",
                               "s"),
                        "."))
    }
    
    # Return
    indices
}

paste0.with.conjunction <- function(values,
                           quote=T,
                           conjunction='or')
{
    if (quote)
        values = paste0("'", values, "'")
    
    if (length(values)==1)
        values
    else if (length(values)==2)
        paste0(values[1], " ", conjunction, " ", values[2])
    else
        paste0(paste0(values[-length(values)], collapse=', '),
               ", ", conjunction, " ",
               values[length(values)])
}