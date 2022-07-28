
##---------------------------##
##-- THE CLASS DEFINITIONS --##
##---------------------------##

setClass('target.population',
         slots=c(
             dim.names='list',
             code=character()
         ))

setClass('simple.target.population',
         contains = 'target.population',
         slots=c(
             invert = 'logical'
         ))

setClass('combination.target.population',
         contains = 'target.population',
         slots = c(
             subpop1 = 'target.population',
             subpop2 = 'target.population',
             combine.function = 'character'
         ))


##----------------------##
##-- THE CONSTRUCTORS --##
##----------------------##

setMethod('initialize',
          signature = 'target.population',
def = function(.Object, dim.names){
    
    stop("'target.population' objects cannot be directly instantiated. Instead create a simple.target.population or other descendant class")

})

setMethod('initialize',
          signature = 'simple.target.population',
def = function(.Object, dim.names, invert=F)
{
    if (length(dim.names) > 0 && is.null(names(dim.names)) || any(names(dim.names)==''))
        stop('dim.names must be a named list')
    
    if (any(sapply(dim.names, function(elem){
        !is.character(elem) && !is.integer(elem) && !is.logical(elem)
    })))
        stop("dim.names must contain only character, integer, or logical vectors")
    
    if (!is.logical(invert) || length(invert) != 1 || is.na(invert))
        stop("invert must be a single TRUE or FALSE value")
    
    .Object@dim.names = dim.names[sapply(dim.names, function(elem){length(elem)>0})]
    
    .Object
})

setMethod('initialize',
          signature = 'combination.target.population',
def = function(.object, subpop1, subpop2, combine.function)
{
    if (!is.character(combine.function) || length(combine.function) != 1 || is.na(combine.function))
        stop("combine.function must be a single character value")
    
    ALLOWED.COMBINE.FUNCTIONS = c('union','intersect','diff')
    if (all(ALLOWED.COMBINE.FUNCTIONS != combine.function))
        stop(paste0("Invalid combine.function ('", combine.function, "'). Must be one of: ",
                    paste0("'", ALLOWED.COMBINE.FUNCTIONS, "'", collapse=', ')))
    
    .Object@subpop1 = subpop1
    .Object@subpop2 = subpop2
    .Object@combine.function = combine.function
    .Object@dim.names = union.named.lists(subpop1@dim.names, subpop2@dim.names)
    
    .Object
})

# A wrapper for the formal "new" constructor
create.target.population <- function(ages=NULL,
                                     races=NULL,
                                     locations=NULL,
                                     subpopulations=NULL,
                                     sexes=NULL,
                                     risks=NULL,
                                     non.hiv.subsets=NULL,
                                     continuum=NULL,
                                     cd4=NULL,
                                     hiv.subsets=NULL,
                                     invert=F)
{
    dim.names = list(age=ages,
                     race=races,
                     location=locations,
                     subpopulation=subpopulations,
                     sex=sexes,
                     risk=risks,
                     non.hiv.subset=non.hiv.subsets,
                     continuum=continuum,
                     cd4=cd4,
                     hiv.subset=hiv.subsets)
    
    new('simple.target.population',
        dim.names=dim.names,
        invert=invert)
}

# Wrappers for combination target populations
union.target.populations <- function(pop1, pop2)
{
    new('combination.target.population',
        subpop1 = pop1,
        subpop2 = pop2,
        combine.function = 'union')
}

intersect.target.populations <- function(pop1, pop2)
{
    new('combination.target.population',
        subpop1 = pop1,
        subpop2 = pop2,
        combine.function = 'intersect')
}

diff.target.populations <- function(pop1, pop2)
{
    new('combination.target.population',
        subpop1 = pop1,
        subpop2 = pop2,
        combine.function = 'diff')
}


##--------------------------##
##-- TESTING FOR EQUALITY --##
##--------------------------##

setGeneric('target.populations.equal',
           def=function(target.population, dim.names){standardGeneric('target.populations.equal')})

setMethod('target.populations.equal',
          signature = 'simple.target.population',
def = function(tpop1, tpop2)
{
    if (!is(tpop2, 'simple.target.population'))
        return(F)
    
    if (!setequal(tpop1@dim.names, tpop2@dim.names))
        return (F)
    
    if (!all(sapply(names(tpop1@dim.names), function(dim){
        length(tpop1@dim.names[[dim]]) == length(tpop2@dim.names[[dim]]) &&
            all(tpop1@dim.names[[dim]] == tpop2@dim.names[[dim]])
    })))
        return (F)
    
    if (tpop1@invert != tpop2@invert)
        return (F)
    
    # They are equal
    T
})


setMethod('target.populations.equal',
          signature = 'combination.target.population',
def = function(tpop1, tpop2)
{
    length(tpop1@combine.function) == length(tpop2@combine.function) &&
        all(tpop1@combine.function == tpop2@combine.function) &&
        target.populations.equal(tpop1@subpop1, tpop2@subpop1) &&
        target.populations.equal(tpop1@subpop2, tpop2@subpop2)
})
          
##---------------------------##
##--  THE RESOLVE FUNCTION --##
##-- Returns an array mask --##
##---------------------------##

setGeneric('resolve.target.population',
           def=function(target.population, dim.names){standardGeneric('resolve.target.population')})

setMethod('resolve.target.population',
          signature = 'simple.target.population',
def = function(target.population, dim.names)
{
    # Check dim.names argument
    if (!is.list(dim.names) || length(dim.names)==0 ||
        is.null(names(dim.names)) || any(names(dim.names)=='') ||
        any(sapply(dim.names, length)==0) ||
        any(!sapply(dim.names,is.character)))
        stop("dim.names must be a named list, whose elements are non-empty character vectors")
    
    # Check that dim.names is a superset of target.population's dim.names
    missing.dimensions = setdiff(names(target.population@dim.names), names(dim.names))
    if (length(missing.dimensions)>0)
        stop("The dimension(s) ",
             paste0("'", missing.dimensions, "'", collapse=', '),
             " are specified in the given target.population, but are not present in dim.names")
    
    # Check the validity of dimension values in the target population against those in dim.names
    mapped.dimension.values = lapply(names(dim.names), function(elem.name){
        dim.names.elem = dim.names[[elem.name]]
        tpop.elem = target.population@dim.names[[elem.name]]
        
        if (is.null(tpop.elem))
        {
            rep(T, length(dim.names.elem))
        }
        else
        {
            if (is.character(elem))
            {
                missing.dimension.values = setdiff(tpop.elem, dim.names.elem)
                if (length(missing.dimension.values)>0)
                    stop(paste0("The following value(s) are specified for dimension '", elem.name,
                                "' in the target.population, but are not present in dim.names: ",
                                paste0("'", missing.dimension.values, "'", collapse=', ')))
                
                rv = rep(F, length(dim.names.elem))
                names(rv) = dim.names.elem
                rv[tpop.elem] = T
                rv
            }
            else if (is.logical(tpop.elem))
            {
                if (length(tpop.elem) != length(dim.names.elem))
                    stop(paste0("The values for dimension '", elem.name,
                                "' in the target.population are specified as a logical vector, but the length of that vector (",
                                length(tpop.elem), " does not match the length of '", elem.name,
                                "' in dim.names (", length(dim.names.elem), ")"))
                
                tpop.elem
            }
            else if (is.integer(tpop.elem))
            {
                if (min(tpop.elem)<1 | max(tpop.elem)>length(dim.names.elem))
                    stop(paste0("The values for dimension '", elem.name,
                                "' in the target.population are specified as an integer vector, but some indices are out of bounds"))
                
                rv = rep(F, length(dim.names.elem))
                rv[tpop.elem] = T
                rv
            }
            else
                stop("invalid type of target.population dim.name")
        }
    })
    
    # Set up the array values
    n.dim = length(dim.names)
    dims = sapply(dim.names, length)
    n = prod(dims)
    n.before = sapply(1:n.dim, function(d){
        prod(dims[1:d]) / dims[d]
    })
    
    rv = sapply(1:n, function(i){
        indices.for.dimensions = floor((i-1)/n.before) %% dims + 1

        all(sapply(1:n.dim, function(d){
            mapped.dimension.values[[d]][indices.for.dimensions[d]]
        }))
    })
    
    # Invert if requested
    if (target.population@invert)
        rv = !rv
    
    # Set dimensions and retur
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    rv
})

setMethod('resolve.target.population',
          signature = 'combination.target.population',
def = function(target.population, dim.names)
{
    pop1 = resolve.target.population(target.population@subpop1, dim.names=dim.names)
    pop2 = resolve.target.population(target.population@subpop2, dim.names=dim.names)
    
    if (combine.function == 'union')
        pop1 | pop2
    else if (combine.function == 'intersect')
        pop1 & pop2
    else if (combine.function == 'diff')
        pop1 & !pop2
    else
        stop("invalid combine.function in target.population")
})

##-----------------------------------##
##-- THE TARGET POPULATION MANAGER --##
##-----------------------------------##

create.target.population.manager <- function()
{
    rv = list(population=list(),
         code=character(),
         name=character())
    
    class(rv) = 'target.population.manager'
    rv
}

DISALLOWED.TPOP.CODE.CHARACTERS = c("_")

add.target.population <- function(tpop, 
                                  code,
                                  name,
                                  allow.duplicates=F,
                                  manager=TARGET.POPULATION.MANAGER.1.0)
{
    if (!is(tpop, 'target.population'))
        stop("'tpop' must be of class 'target.population'")
    
    if (!is.character(code) || length(code) != 1 || is.na(code) || nchar(trimws(code))==0)
        stop("code must be a single, non-NA, non-empty string")
    
    # Check if already added under a different name or code
    if (!allow.duplicates)
    {
        mask = sapply(manager$population, target.populations.equal, tpop1=tpop)
   
        if (any(mask))
        {
            if (manager$name[mask] == name)
            {
                if (manager$code[mask] == code)
                    return (manager)
                stop(paste0("Attempted to add a target population (with code '",
                            code, "') which is already present with code(s) '",
                            paste0(manager$code[mask], collapse=', '),
                            "'. There can only be one code per target population, unless allow.duplicates=T"))
            }
            else
                stop(paste0("Attempted to add a target population (under the name '",
                            name, "') which is already present under the name(s) '",
                            paste0("'", manager$name[mask], "'", collapse=', '), 
                            "'. There can only be one name per target population, unless allow.duplicates=T."))
        }
    }
    
    # Check that the code does not have any disallowed characters
    for (ch in DISALLOWED.TPOP.CODE.CHARACTERS)
        if (grepl(ch, code))
            stop(paste0("Target population codes cannot contain '", ch, "'"))
    
    # Check to confirm that name and code are unique
    if (any(manager$name==name))
        stop(paste0("A different target population is already present under the name, '",
                    name, "'. Target population names must be unique"))
    if (any(manager$code==code))
        stop(paste0("A different target population is already present under the code, '",
                    code, "'. Target population codes must be unique"))
    
    # Add it in and return
    
    tpop@code = code
    
    manager$population = c(manager$population, list(tpop))
    manager$name = c(manager$name, name)
    manager$code = c(manager$code, code)
    
    names(manager$population) = names(manager$name) = manager$code
    
    manager
}

##-------------##
##-- GETTERS --##
##-------------##

target.population.to.code <- function(tpop, manager=TARGET.POPULATION.MANAGER.1.0,
                                      throw.error.if.no.intervention=T)
{
    if (length(tpop@code)==0)
    {
        mask = sapply(manager$population, target.populations.equal, tpop1 = tpop)
        if (any(mask))
            manager$code[mask][1]
        else if (throw.error.if.no.intervention)
            stop("The target population has not been added to the TARGET.POPULATION.MANAGER. Cannot find a code")
        else
            NULL
    }
    else
        tpop@code
}

target.population.from.code <- function(code, manager=TARGET.POPULATION.MANAGER.1.0,
                                        throw.error.if.no.intervention=T)
{
    mask = manager$code==code
    if (any(mask))
        manager$population[[ code ]]
    else if (throw.error.if.no.intervention)
        stop(paste0("No intervention has been added to the TARGET.POPULATION.MANAGER with code '", code, "'"))
    else
        NULL
}

target.population.code.to.name <- function(code, manager=TARGET.POPULATION.MANAGER.1.0,
                                           throw.error.if.no.intervention=T)
{
    rv = manager$name[code]
    if (is.na(rv))
    {
        if (throw.error.if.no.intervention)
            stop(paste0("No intervention has been added to the TARGET.POPULATION.MANAGER with code '", code, "'"))
        else
            NULL
    }
    else
        rv
}

target.population.name.to.code <- function(name, manager=TARGET.POPULATION.MANAGER.1.0,
                                           throw.error.if.no.intervention=T)
{
    mask = manager$name==name
    if (any(mask))
        manager$code[[ (1:length(manager$code))[mask] ]]
    else if (throw.error.if.no.intervention)
        stop(paste0("No intervention has been added to the TARGET.POPULATION.MANAGER with name '", name, "'"))
    else
        NULL
}

target.population.name <- function(tpop, manager=TARGET.POPULATION.MANAGER.1.0,
                                   throw.error.if.no.intervention=T)
{
    code = target.population.to.code(tpop, throw.error.if.no.intervention = throw.error.if.no.intervention)
    if (is.null(code))
    {
        if (throw.error.if.no.intervention)
            stop(paste0("No intervention has been added to the TARGET.POPULATION.MANAGER with code '", code, "'"))
        else
            NULL
    }
    else
        manager$name[code]
}
