
# TO DO to make models functional
# 
# redo in the continuum manager
# 
# make flexible dimensions for intervention.units (and get dims)
# 
# finish the tail model for prep


#-- CONSTRUCTORS --#

create.logistic.model <- function(intercept,
                                  slope,
                                  anchor.year,
                                  min.proportion,
                                  max.proportion,
                                  log.ors)
{
    if (!is.array(intercept))
        stop("'intercept' must be an array")
    if (!is.array(slope) || length(dim(intercept))!=length(dim(slope)) || !all(dim(intercept)==dim(slope)))
        stop("'slope' must be an array with the same dimensions as 'intercept'")
    if (length(anchor.year)!=1 || !is.numeric(anchor.year) || is.na(anchor.year))
        stop("anchor.year must be a single, non-NA numeric value")
    if (length(min.proportion)!=1 || !is.numeric(min.proportion) || is.na(min.proportion) || 
        min.proportion < 0 || min.proportion >=1)
        stop("min.proportion must be a single numeric value between 0 and 1")
    if (length(max.proportion)!=1 || !is.numeric(max.proportion) || is.na(max.proportion) || 
        max.proportion <= min.proportion || max.proportion >1)
        stop(paste0("max.proportion (", max.proportion, 
                    ") must be a single numeric value between min.proportion (",
                    min.proportion, ") and 1"))
    
    if (is.null(dimnames(intercept)) || is.null(dimnames(slope)) ||
        is.null(names(dimnames(intercept))) || is.null(names(dimnames(slope))) ||
        !named.lists.equal(dimnames(intercept), dimnames(slope)))
        stop("'intercept' and 'slope' must have named dimnames which are the same for both")
    
    rv = list(type='logistic.model',
              intercept=intercept,
              slope=slope,
              anchor.year=anchor.year,
              min.proportion=min.proportion,
              max.proportion=max.proportion,
              log.ors=log.ors,
              dim.names = dimnames(intercept),
              scale='log')
    
    class(rv) = c('logistic.model','model')
    rv
}

#-- PROJECT FROM MODEL --#

#'@param future.slope May be NULL (missing)
project.from.model <- function(model,
                               years,
                               alphas,
                               future.slope,
                               future.slope.after.year,
                               dim.names,
                               interact.race.sex)
{
    if (is.null(model) || !is(model, 'model'))
        stop("model must be an object of class 'model'")
    if (length(years)==0 || !is.numeric(years) || any(is.na(years)))
        stop("years must be a non-empty, non-NA numeric vector")
    if (!is.list(alphas))
        stop("alphas must be a list")
    if (!is.list(dim.names) || length(dim.names)==0 || is.null(names(dim.names)) || any(names(dim.names)==''))
        stop("dim.names must be a non-empty, named list")
    
    if (model$type=='logistic.model')
        project.from.logistic.model(model=model,
                                    years=years,
                                    alphas=alphas,
                                    future.slope=future.slope,
                                    future.slope.after.year=future.slope.after.year,
                                    dim.names=dim.names)
    else
        stop(paste0("Do not know how to project from model of type '", model$type, "'"))
}

project.from.logistic.model <- function(model, years, 
                                        alphas,
                                        future.slope, future.slope.after.year,
                                        dim.names)
{
    intercept.alphas = map.alphas(raw.alphas=alphas$intercept$main.effects, dim.names=dim.names,
                                  collapse.sex.risk = alphas$intercept$interact.sex.risk)
    intercept = add.alphas.to.array(arr=model$intercept,
                                    alphas=intercept.alphas,
                                    target.dim.names = dim.names)
    if (!is.null(alphas$intercept$interaction.effects))
        intercept = add.interaction.alphas.to.array(arr=intercept,
                                                    interaction.alphas=alphas$intercept$interaction.effects)
    
    if (is.null(alphas$slope))
        slope = add.alphas.to.array(arr=model$slope,
                                    alphas=NULL,
                                    target.dim.names = dim.names)
    else
    {
        slope.alphas = map.alphas(raw.alphas=alphas$slope$main.effects, dim.names=dim.names,
                                  collapse.sex.risk = alphas$slope$interact.sex.risk)
        slope = add.alphas.to.array(arr=model$slope,
                                    alphas=slope.alphas,
                                    target.dim.names = dim.names)
        if (!is.null(alphas$intercept$interaction.effects))
            slope = add.interaction.alphas.to.array(arr=slope,
                                                    interaction.alphas=alphas$slope$interaction.effects)
    }
    
    lapply(years, function(year){
        
        #lo = intercept + 
        #    slope * (min(year, future.slope.after.year) - model$anchor.year) +
        #    future.slope * max(0,year-future.slope.after.year)
        
        lo = intercept + 
            slope * (year - model$anchor.year)
        
        if (!is.null(future.slope) && year > future.slope.after.year)
            lo = lo + future.slope * (year - future.slope.after.year)
        
        model$min.proportion + (model$max.proportion-model$min.proportion) / (1+exp(-lo))
    })
}


# NEED TO IMPLEMENT THIS
project.from.logistic.tail.model <- function(model, years, 
             alphas,
             future.slope, future.slope.after.year,
             dim.names)
{
    intercept.alphas = map.alphas(raw.alphas=intercept.alphas, dim.names=dim.names)
    slope.alphas = map.alphas(raw.alphas=slope.alphas, dim.names=dim.names)
    
    logit.intercept = add.alphas.to.array(arr=model$intercept,
                                    alphas=intercept.alphas)
    logit.slope = add.alphas.to.array(arr=model$slope,
                                alphas=slope.alphas)
    
    
    stop("We need to finish this here")
    lapply(years, function(year){
        
        # The value based off linear model
        rv = expit.intercept + 
            expit.slope * (year-model$anchor.year)
        
        if (year > future.slope.after.year)
            rv = rv + expit.future.slope * (year - future.slope.after.year)
        
        rv = pmin(model$max.proportion, pmax(model$min.proportion, rv))
        mask = rv > model$logistic.after.p
        
        # The value based off logistic model
        lo = logit.intercept[mask] + logit.slope * (year - model$anchor.year)
        
        if (year > future.slope.after.year)
            lo - model$logistic.slope.with.additional[mask] * pmax(0, year-model$additional.after.year)
        
        # Overwrite where appropriate
        rv[mask] = model$min.proportion + (model$max.proportion-model$min.proportion) / (1+exp(-lo))
        
        rv[rv<0] = 0
        rv
    })
}


#-- GET MODEL DIMENSIONS --#

get.model.dim.names <- function(model)
{
    if (is.null(model) || !is(model, 'model'))
        stop("model must be an object of type 'model'")
    
    model$dim.names
}

#-- FOR COLLAPSING SEX-RISK --#


collapse.dim.values <- function(dim.value1, dim.value2)
{
    paste0(dim.value1, '__', dim.value2)
}

collapse.dim.names.sex.risk <- function(dim.names)
{
    if (all(names(dim.names)!='sex') || all(names(dim.names)!='race'))
        stop('sex and race are not both present in dim names')
    
    sex.risk = collapse.dim.values(rep(dim.names$sex, length(dim.names$risk)),
                                   rep(dim.names$risk, each = length(dim.names$sex)))
    
    dim.names$sex = sex.risk
    names(dim.names)[names(dim.names)=='sex'] = collapse.dim.values('sex','risk')
    dim.names = dim.names[setdiff(names(dim.names), 'risk')]
    
    dim.names
}

#-- SCALE TRANSFORMATION --#

get.model.scale.transformation.function <- function(model)
{
    if (is.null(model) || !is(model, 'model'))
        stop("model must be an object of type 'model'")
    
    if (is.null(model$scale))
        stop("the scale slot has not been set on this model")
    else if (model$scale=='log')
        log
    else if (model$scale=='identity')
        function(x){x}
    else
        stop(paste0("Don't know how to transform for scale '", model$scale, "'"))
}

#-- HELPERS --#

map.alphas <- function(raw.alphas,
                       dim.names,
                       collapse.sex.risk,
                       missing.value=0)
{
    if (collapse.sex.risk)
        dim.names = collapse.dim.names.sex.risk(dim.names)
    
    alphas = lapply(names(dim.names), function(dim.name.name){
        rv = rep(missing.value, length(dim.names[[dim.name.name]]))
        names(rv) = dim.names[[dim.name.name]]
        
        if (length(raw.alphas[[dim.name.name]])>0)
        {
            invalid.dimensions = setdiff(names(raw.alphas[[dim.name.name]]), dim.names[[dim.name.name]])
            if (length(invalid.dimensions)>0)
                stop(paste0("Invalid dimension values in alpha(s) for dimension '", dim.name.name,
                            "': ", pasteo("'", invalid.dimensions, "'", collapse=', ')))
        }
        
        rv[names(raw.alphas[[dim.name.name]])] = raw.alphas[[dim.name.name]]
        rv
    })
    
    names(alphas) = names(dim.names)
    
    alphas
}

# alphas - a named list of named numeric vectors
# returns an array with dimnames the names of each element of alphas
add.alphas.to.array <- function(arr,
                                alphas,
                                target.dim.names)
{
    if (!is.null(alphas))
    {
        n.dim.alphas = length(alphas)
        dims.alphas = sapply(alphas, length)
        n.before.alphas = sapply(1:n.dim.alphas, function(d){
            prod(dims.alphas[1:d]) / dims.alphas[d]
        })
        
        non.empty.alpha.mask = sapply(alphas, function(a){any(a!=0)})
        dims.alphas = dims.alphas[non.empty.alpha.mask]
        n.before.alphas = n.before.alphas[non.empty.alpha.mask]
        n.dim.alphas = sum(non.empty.alpha.mask)
        alphas = alphas[non.empty.alpha.mask]
    }
    
    n.dim.target = length(target.dim.names)
    dims.target = sapply(target.dim.names, length)
    n = prod(dims.target)
    n.before.target = sapply(1:n.dim.target, function(d){
        prod(dims.target[1:d]) / dims.target[d]
    })
    
    n.dim.arr = length(dim(arr))
    d.target.for.d.arr = sapply(1:n.dim.arr, function(d.arr){
        (1:n.dim.target)[names(dimnames(arr))[d.arr]==names(target.dim.names)]
    })
    n.before.target = n.before.target[d.target.for.d.arr]
    dims.target = dims.target[d.target.for.d.arr]
    
    dims.arr = dim(arr)
    n.before.arr = sapply(1:n.dim.arr, function(d.arr){
        prod(dims.arr[1:d.arr]) / dims.arr[d.arr]
    })
    
    if (is.null(alphas) || length(alphas)==0)
    {
        rv = sapply(1:n, function(i){
            indices.for.arr = floor((i-1)/n.before.target) %% dims.target + 1
            arr.index = 1+sum((indices.for.arr-1) * n.before.arr)
            
            arr[arr.index] 
        })
    }
    else
    {
        rv = sapply(1:n, function(i){
            indices.for.alpha.dimensions = floor((i-1)/n.before.alphas) %% dims.alphas + 1
            
            indices.for.arr = floor((i-1)/n.before.target) %% dims.target + 1
            arr.index = 1+sum((indices.for.arr-1) * n.before.arr)
            
            sum(sapply(1:n.dim.alphas, function(d){
                alphas[[d]][indices.for.alpha.dimensions[d]]
            })) +
                arr[arr.index] 
        })
    }
    
    dim(rv) = sapply(target.dim.names, length)
    dimnames(rv) = target.dim.names
    
    rv
}

# interaction.alphas is a list, where each element is one interaction term
# each element has two sub-elements
# $dim.values - a named list. The name of each element is a dimension, and the value is the specific index/name into that list
# $value - the value of the alpha to add

add.interaction.alphas.to.array <- function(arr,
                                           interaction.alphas)
{
    for (one.alpha in interaction.alphas)
    {
        if (length(one.alpha$dim.values)!=2)
            stop(paste0("We are only set up to do interaction alphas for two-way interactions at this point. ",
                        "Cannot add alphas for ", 
                        paste0(names(one.alpha$dim.values), "='", one.alpha$dim.values, "'",
                               collapse = ' x ')))
        
        mask = get.two.dim.access.indices(dim.names=dimnames(arr),
                                          dim1=names(one.alpha$dim.values)[1],
                                          dim.value1=one.alpha$dim.values[1],
                                          dim2=names(one.alpha$dim.values)[2],
                                          dim.value2=one.alpha$dim.values[2])
        arr[mask] = arr[mask] + one.alpha$value
    }
    
    arr
}
