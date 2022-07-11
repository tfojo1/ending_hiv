
##---------------------------##
##-- THE CLASS DEFINITIONS --##
##---------------------------##

setClass('target.population',
         slots=c(
             dim.names='list'
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

