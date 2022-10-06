
# TO DO to make models functional
# 
# redo in the continuum manager
# 
# make flexible dimensions for intervention.units (and get dims)
# 
# finish the tail model for prep

##------------------------##
##-- MODEL CONSTRUCTORS --##
##------------------------##

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
              scale='logit')
    
    class(rv) = c('logistic.model','model')
    rv
}

create.logistic.tail.model <- function(intercept,
                                       slope,
                                       anchor.year,
                                       min.proportion,
                                       max.proportion,
                                       log.ors,
                                       logistic.after.frac.of.p.span=0.5)
{
    if (!is.numeric(intercept) || any(is.na(intercept)))
        stop("'intercept' must be a numeric value with no NA values")
    if (!is.numeric(slope) || any(is.na(slope)))
        stop("'slope' must be a numeric value with no NA values")
    
    if (is.null(dim(intercept)) && length(intercept) != 1)
        stop("'intercept' must either be an array/matrix or a scalar value")
    if (is.null(dim(slope)) && length(slope) != 1)
        stop("'slope' must either be an array/matrix or a scalar value")
    
    if (is.null(dim(intercept)))
    {
        if (is.null(dim(slope)))
            stop("Either 'intercept' or 'slope' must be an array/matrix with named dimensions")
        else
        {
            intercept = rep(intercept, length(slope))
            dim(intercept) = dim(slope)
            
            if (is.null(dimnames(slope)))
                stop("Either 'intercept' or 'slope' must be an array/matrix with named dimensions")
            dimnames(intercept) = dimnames(slope)
        }
    }
    else
    {
        if (is.null(dim(slope)))
        {
            slope = rep(slope, length(intercept))
            dim(slope) = dim(intercept)
            
            if (is.null(dimnames(intercept)))
                stop("Either 'intercept' or 'slope' must be an array/matrix with named dimensions")
            dimnames(slope) = dimnames(intercept)
        }
        else if (length(dim(intercept))!=length(dim(slope)) || !all(dim(intercept)==dim(slope)))
            stop("If 'intercept' and 'slope' are both arrays/matrices, they must have the same dimensions")
        else if (is.null(dimnames(intercept)))
        {
            if (is.null(dimnames(slope)))
                stop("Either 'intercept' or 'slope' must be an array/matrix with named dimensions")
            else
                dimnames(intercept) = dimnames(slope)
        }
        else if (is.null(dimnames(slope)))
            dimnames(slope) = dimnames(intercept)
        else if (!all(sapply(1:length(dimnames(intercept)), function(i){
            all(dimnames(intercept)[[i]]==dimnames(slope)[[i]])
        })))
            stop("If 'intercept' and 'slope' both have named dimensions, the dimnames must be the same for both")
    }
    
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
    if (length(logistic.after.frac.of.p.span)!=1 || !is.numeric(logistic.after.frac.of.p.span) || is.na(logistic.after.frac.of.p.span) || 
        logistic.after.frac.of.p.span < 0 || logistic.after.frac.of.p.span >=1)
        stop("logistic.after.frac.of.p.span must be a single numeric value between 0 and 1")
    
    rv = list(type='logistic.model',
              intercept=intercept,
              slope=slope,
              anchor.year=anchor.year,
              min.proportion=min.proportion,
              max.proportion=max.proportion,
              p.span=max.proportion-min.proportion,
              logistic.after.frac.of.p.span=logistic.after.frac.of.p.span,
              logistic.after.p = min.proportion + logistic.after.frac.of.p.span * (max.proportion-min.proportion),
              log.ors=log.ors,
              dim.names = dimnames(intercept))
    
    class(rv) = c('logistic.tail.model','model')
    rv
}

create.single.dimension.log.rate.model <- function(dimension,
                                                   intercept.values,
                                                   slope.values = NULL,
                                                   anchor.year=2010,
                                                   suppress.warnings=F)
{
    if (!is.character(dimension) || length(dimension)!=1 || is.na(dimension) || dimension=='')
        stop("'dimension' must be a single, non-NA, non-empty character value")
    
    if (!is.numeric(intercept.values) || !is.vector(intercept.values) || length(intercept.values)==0 ||
        any(is.na(intercept.values)))
        stop("'intercept.values' must be a non-empty numeric vector without NA values")
    
    dim.names = list(names(intercept.values))
    names(dim.names) = dimension
    intercept = array(intercept.values, dim = length(intercept.values), dimnames = dim.names)
    
    if (is.null(slope.values))
        slope = NULL
    else
    {
        if (!is.numeric(slope.values) || !is.vector(slope.values) || length(slope.values)!=length(intercept.values) ||
            any(is.na(slope.values)))
            stop("If it is not NULL, 'slope.values' must be a numeric vector without NA values, of the same length as 'intercept.values")
        
        if (is.null(names(slope.values)))
            names(slope.values) = intercept.values
        else if (!setequal(names(intercept.values), names(slope.values)))
            stop("'intercept.values' and 'slope.values' must have the same names")
        else
            slope.values = slope.values[names(intercept.values)]
        
        slope = array(slope.values, dim = length(slope.values), dimnames = dim.names)
    }
    
    create.log.rate.model(intercept = intercept,
                          slope = slope,
                          anchor.year = anchor.year,
                          suppress.warnings = suppress.warnings)
}

#slope, intercept should be on the log scale
create.log.rate.model <- function(intercept,
                                  slope,
                                  anchor.year,
                                  suppress.warnings=F)
{
    if (!is.array(intercept) && (!is.numeric(intercept) || length(intercept)!=1) )
        stop("'intercept' must be an array or a numeric scalar")
 
    if (!suppress.warnings && all(intercept>0) && (is.null(slope) || all(slope>0)))
        print("NOTE: 'intercept' and 'slope' should already be on the log scale for create.log.rate.model. Did you need to take the log before passing?")
       
    if (is.null(slope))
    {
        if (is.array(intercept))
            slope = array(0, dim=dim(intercept), dimnames=dimnames(intercept))
        else
            slope = 0
    }
    
    if (is.array(slope))
    {
        if (length(dim(intercept))!=length(dim(slope)) || !all(dim(intercept)==dim(slope)))
            stop("'slope' must be an array with the same dimensions as 'intercept' or a numeric scalar")
    }
    else if (!is.numeric(slope) || length(slope) != 1)
        stop("'slope' must be an array with the same dimensions as 'intercept' or a numeric scalar")
        
    if (length(anchor.year)!=1 || !is.numeric(anchor.year) || is.na(anchor.year))
        stop("anchor.year must be a single, non-NA numeric value")
    
    if (is.array(intercept))
    {
        if (is.null(dimnames(intercept)) || is.null(dimnames(slope)) ||
            is.null(names(dimnames(intercept))) || is.null(names(dimnames(slope))) ||
            !named.lists.equal(dimnames(intercept), dimnames(slope)))
            stop("'intercept' and 'slope' must have named dimnames which are the same for both if they are not scalar values")
    }
    
    rv = list(type='log.rate.model',
              intercept=intercept,
              slope=slope,
              anchor.year=anchor.year,
              dim.names = dimnames(intercept),
              scale='log')
    
    class(rv) = c('log.rate.model','model')
    rv
}

create.natural.spline.model <- function(knot.times,
                                        knot.values,
                                        scale = c('identity','log','logit')[1])
{
    # Check scale
    check.model.scale(scale)
    
    
    # Check knots
    if (!is.list(knot.values))
        knot.values = as.list(knot.values)
    
    if (!is.numeric(knot.times) || length(knot.times)==0 || any(is.na(knot.times)))
        stop("'knot.times' must be a non-empty, numeric vector with non-NA values")
    
    if (length(knot.times)<2)
        stop("A natural spline model must have at least two knots")
    
    if (length(knot.values) != length(knot.times))
        stop("knot.values must be a list or numeric vector with the same length as knot.times")
    
    if (any(sapply(knot.values, function(v){
        !is.numeric(knot.values) || any(is.na(knot.values))
    })))
        stop("The elements of knot.values must be only non-NA, numeric values")
    
    for (i in 2:length(knot.values))
    {
        if (is.null(dim(knot.values[[1]])))
        {
            if (!is.null(dim(knot.values)[[i]]) || length(knot.values[[1]]) != length(knot.values[[i]]))
                stop("All elements of knot.values must have the same dimensions or length (if dimensionless)")
            if (is.null(names(knot.values[[1]])) != is.null(names(knot.values[[i]])) ||
                (!is.null(names(knot.values[[1]])) && !all(names(knot.values[[1]])==names(knot.values[[i]]))))
                stop("If the elements of knot.values are dimensionless, they must all have the same names")
        }
        else
        {
            if (length(dim(knot.values[[1]])) != length(dim(knot.values[[i]])) ||
                !all(dim(knot.values[[1]])==dim(knot.values[[i]])) ||
                is.null(dimnames(knot.values[[1]])) || is.null(dimnames(knot.values[[2]])) ||
                !all(sapply(1:length(knot.values), function(k){
                    all(dimnames(knot.values[[1]])[[k]]==dimnames(knot.values[[i]])[[k]])
                })))
                stop("All elements of knot.values must have the same dimensions and dimnames")
        }
    }
    
    # Transform knots
    knot.values = lapply(knot.values, get.transform.to.model.scale.function(scale))
    
    # Package and return
    rv = list(type='natural.spline.model',
              knot.times=knot.times,
              knot.values=knot.values,
              dim.names = dimnames(knot.values[[1]]),
              scale=scale)
    
    class(rv) = c('natural.spline.model','model')
    rv
}

create.natural.spline.tail.multiplier.model <- function(knot.times,
                                                        knot.values,
                                                        before.time,
                                                        after.time,
                                                        before.multiplier,
                                                        after.multiplier,
                                                        scale)
{
    rv = create.natural.spline.model(knot.times=knot.times,
                                     knot.values=knot.values,
                                     scale=scale)
    
    # check times
    if (!is.numeric(before.time) || length(before.time)!=1 || is.na(before.time))
        stop("'before.time' must be a single, non-NA, numeric value")
    if (!is.numeric(after.time) || length(after.time)!=1 || is.na(after.time))
        stop("'after.time' must be a single, non-NA, numeric value")
    
    # Check multipliers
    if (!is.numeric(before.multiplier) || length(before.multiplier)!=0 || any(is.na(before.multiplier)))
        stop("'before.multiplier' must be a non-empty, non-NA numeric")
    if (!is.numeric(after.multiplier) || length(after.multiplier)!=0 || any(is.na(after.multiplier)))
        stop("'after.multiplier' must be a non-empty, non-NA numeric")
    
    # add to the rv
    rv$before.time = before.time
    rv$after.time = after.time
    rv$before.multiplier = expand.population(log(before.multiplier), target.dimnames = rv$dim.names)
    rv$after.multiplier = expand.population(log(after.multiplier), target.dimnames = rv$dim.names)
    rv$scales.for.alphas = c(rep(rv$scale, length(rv$knot.values)), 'log','log')
    names(rv$scales.for.alphas) = c(paste0('knot', 1:length(rv$knot.values)), 'before.multiplier','after.multiplier')
    
    class(rv) = c('natural.spline.modified.tail.model','model')
    rv
}

create.logistic.spline.model <- function(knot.times,
                                         knot.values,
                                         fraction.of.asymptote.after.end=0.05,
                                         fraction.of.asymptote.before.start=0.025,
                                         fraction.of.asymptote.for.change.dir=0.02,
                                         alphas.are.multipliers = T)
{
    # Check knots
    if (!is.list(knot.values))
        knot.values = as.list(knot.values)
    
    if (!is.numeric(knot.times) || length(knot.times)==0 || any(is.na(knot.times)))
        stop("'knot.times' must be a non-empty, numeric vector with non-NA values")
    
    if (length(knot.times) != 2 && length(knot.times) != 3)
        stop("A logistic spline model must have at either two or three knots")
    
    if (length(knot.values) != length(knot.times))
        stop("knot.values must be a list or numeric vector with the same length as knot.times")
    
    if (any(sapply(knot.values, function(v){
        !is.numeric(v) || any(is.na(v))
    })))
        stop("The elements of knot.values must be only non-NA, numeric values")
    
    for (i in 2:length(knot.values))
    {
        if (is.null(dim(knot.values[[1]])))
        {
            if (!is.null(dim(knot.values)[[i]]) || length(knot.values[[1]]) != length(knot.values[[i]]))
                stop("All elements of knot.values must have the same dimensions or length (if dimensionless)")
            if (is.null(names(knot.values[[1]])) != is.null(names(knot.values[[i]])) ||
                (!is.null(names(knot.values[[1]])) && !all(names(knot.values[[1]])==names(knot.values[[i]]))))
                stop("If the elements of knot.values are dimensionless, they must all have the same names")
        }
        else
        {
            if (length(dim(knot.values[[1]])) != length(dim(knot.values[[i]])) ||
                !all(dim(knot.values[[1]])==dim(knot.values[[i]])) ||
                is.null(dimnames(knot.values[[1]])) || is.null(dimnames(knot.values[[2]])) ||
                !all(sapply(1:length(knot.values), function(k){
                    all(dimnames(knot.values[[1]])[[k]]==dimnames(knot.values[[i]])[[k]])
                })))
                stop("All elements of knot.values must have the same dimensions and dimnames")
        }
    }
    
    # Check fraction of asymptotes
    if (!is.numeric(fraction.of.asymptote.after.end) || length(fraction.of.asymptote.after.end) != 1 ||
        is.na(fraction.of.asymptote.after.end) ||
        fraction.of.asymptote.after.end <= 0 || fraction.of.asymptote.after.end >= 1)
        stop("'fraction.of.asymptote.after.end' must be a single, non-NA numeric value between 0 and 1")
    if (!is.numeric(fraction.of.asymptote.before.start) || length(fraction.of.asymptote.before.start) != 1 ||
        is.na(fraction.of.asymptote.before.start) ||
        fraction.of.asymptote.before.start <= 0 || fraction.of.asymptote.before.start >= 1)
        stop("'fraction.of.asymptote.before.start' must be a single, non-NA numeric value between 0 and 1")
    if (!is.numeric(fraction.of.asymptote.for.change.dir) || length(fraction.of.asymptote.for.change.dir) != 1 ||
        is.na(fraction.of.asymptote.for.change.dir) ||
        fraction.of.asymptote.for.change.dir <= 0 || fraction.of.asymptote.for.change.dir >= 1)
        stop("'fraction.of.asymptote.for.change.dir' must be a single, non-NA numeric value between 0 and 1")
    
    if (!is.logical(alphas.are.multipliers) || length(alphas.are.multipliers)!=1 || is.na(alphas.are.multipliers))
        stop("'alphas.are.multipliers' must be a single, non-NA, logical value")
    
    if (alphas.are.multipliers)
        knot.values = lapply(knot.values, log)
    
    # Package and return
    rv = list(type='logistic.spine.model',
              knot.times=knot.times,
              knot.values=knot.values,
              dim.names = dimnames(knot.values[[1]]),
              fraction.of.asymptote.after.end=log(fraction.of.asymptote.after.end) - log(1-fraction.of.asymptote.after.end),
              fraction.of.asymptote.before.start=log(fraction.of.asymptote.before.start) - log(1-fraction.of.asymptote.before.start),
              fraction.of.asymptote.for.change.dir=log(fraction.of.asymptote.for.change.dir) - log(1-fraction.of.asymptote.for.change.dir),
              alphas.are.multipliers = alphas.are.multipliers)
    
    class(rv) = c('logistic.spine.model','model')
    rv
}

##---------------------------------------------------------------------------##
##-- ACCESSORY FUNCTIONS - NEED TO BE SPECIFIED FOR EACH DEFINE MODEL TYPE --##
##---------------------------------------------------------------------------##


get.alpha.names.for.model <- function(model)
{
    if (model$type == 'logistic.model' || model$type=='logistic.tail.model' || model$type=='log.rate.model')
        c('intercept','slope')
    else if (model$type == 'natural.spline.model')
        paste0("knot", 1:length(model$knot.times))
    else if (model$type == 'logistic.spline.model')
        c(paste0("knot", 1:length(model$knot.times)),
          'fraction.of.asymptote.after.end',
          'fraction.of.asymptote.before.start',
          'fraction.of.asymptote.for.change.dir')
    else if (model$type == 'natural.spline.modified.tail.model')
        c(paste0("knot", 1:length(model$knot.times)),
          'before.multiplier','after.multiplier')
    else
        stop(paste0("Alpha names have not been defined for model type '", model$type, "'"))
}

##-------------------##
##-- ALPHA OBJECTS --##
##-------------------##

create.alphas.object <- function(dim.names)
{
    rv = list(
        dim.names=dim.names,
        main.effects=list(),
        interaction.effects=list()
    )
    
    class(rv) = 'model.alphas'
    rv
}

set.alpha.main.effect.values <- function(alphas,
                                         dimensions,
                                         dimension.values,
                                         values,
                                         check.arguments=T,
                                         error.prefix='')
{
    # Prep the values and arguments
    dimensions.and.values = prep.dimensions.and.values.for.alphas(alphas=alphas,
                                                                  dimensions=dimensions,
                                                                  dimension.values=dimension.values,
                                                                  values=values,
                                                                  check.arguments=check.arguments,
                                                                  error.prefix=error.prefix,
                                                                  is.interaction=F)
    dimensions = dimensions.and.values$dimensions
    dimension.values = dimensions.and.values$dimension.values
    
    # Set the values
    for (i in 1:length(values))
    {
        d = dimensions[[i]]
        
        if (is.null(alphas$main.effects[[d]]))
            alphas$main.effects[[d]] = numeric()
        
        alphas$main.effects[[d]][ dimension.values[i] ] = values[i]
    }
    
    # Return
    alphas
}

set.alpha.interaction.values <- function(alphas,
                                         dimensions,
                                         dimension.values,
                                         value,
                                         check.arguments=T,
                                         error.prefix='')
{
    # Prep the values and arguments
    dimensions.and.values = prep.dimensions.and.values.for.alphas(alphas=alphas,
                                                                  dimensions=dimensions,
                                                                  dimension.values=dimension.values,
                                                                  values=value,
                                                                  check.arguments=check.arguments,
                                                                  error.prefix=error.prefix,
                                                                  is.interaction=T)
    dimensions = dimensions.and.values$dimensions
    dimension.values = dimensions.and.values$dimension.values
    
    # Get all pairs of dimension values
    unique.dimensions = intersect(names(alphas$dim.names), unique(dimensions))
    if (check.arguments)
    {
        if (length(unique.dimensions)!=2)
            stop(paste0(error.prefix,
                        "For interaction alphas, there must be exactly two dimensions specified"))
    }
    
    d1.values = dimension.values[dimensions==unique.dimensions[1]]
    d2.values = dimension.values[dimensions==unique.dimensions[2]]
    
    n1 = length(d1.values)
    d1.values = rep(d1.values, length(d2.values))
    d2.values = rep(d2.values, each=n1)
    
    if (is.null(alphas$interaction.effects[[2]]))
        alphas$interaction.effects[[2]] = list()
    
    d.name = paste0(unique.dimensions, collapse="_")
    if (is.null(alphas$interaction.effects[[2]][[d.name]]))
        alphas$interaction.effects[[2]][[d.name]] = list(
            dim1 = unique.dimensions[1],
            dim2 = unique.dimensions[2],
            d1.values=character(),
            d2.values=character(),
            values=numeric()
        )
    
    # Set the values
    dv.names = paste0(d1.values, "_", d2.values)
    alphas$interaction.effects[[2]][[d.name]]$d1.values[dv.names] = d1.values
    alphas$interaction.effects[[2]][[d.name]]$d2.values[dv.names] = d2.values
    alphas$interaction.effects[[2]][[d.name]]$values[dv.names] = values
    
    # Return
    alphas
}

prep.dimensions.and.values.for.alphas <- function(alphas,
                                                  dimensions,
                                                  dimension.values,
                                                  values,
                                                  check.arguments=T,
                                                  error.prefix='',
                                                  is.interaction)
{
    if (!is(alphas, 'model.alphas'))
        stop("'alphas' must be a object of class 'model.alphas'")
    
    # Fill in missing dimensions
    if (length(dimensions)==0)
    {
        dimensions = get.dimension.for.values(alphas$dim.names, values=dimension.values)
        dimensions[is.na(dimensions) && sapply(dimensions.values, function(dv){dv=='all'})] = 'all'
        if (any(is.na(dimensions)))
            stop(paste0(error.prefix,
                        "Invalid dimension values(s): ",
                        paste0("'", dimension.values[is.na(dimensions)], "'", collapse=', ')))
    }
    
    if (length(dimensions)==1)
        rep(dimensions, length(values))
    
    if (!is.list(dimension.values))
        dimension.values = list(dimension.values)
    
    if (check.arguments)
    {
        if (length(dimensions) != length(dimension.values))
            stop(paste0(error.prefix,
                        "'dimensions' must either be a single value, or the same length as 'dimension.values"))
     
        invalid.dimensions = setdiff(dimensions, c('all', names(alphas$dim.names)))
        if (length(invalid.dimensions))
            stop(paste0(error.prefix,
                        "Invalid dimension(s): ",
                        paste0("'", invalid.dimensions, "'")))
        
        if (all(!sapply(dimension.values, is.numeric) && !sapply(dimension.values, is.integer)) || 
            any(sapply(dimension.values, is.na)))
            stop(paste0(error.prefix,
                        "'dimension.values' must contain non-NA numeric or integer elements"))
        
        errors = sapply(1:length(dimension.values), function(i){
            if (dimensions[[i]]=='all')
            {}
            else if (is.integer(dimension.values[[i]]))
            {
                if (dimension.values[[i]]<1 || dimension.values[[i]]>length(alphas$dim.names[[ dimensions[i] ]]))
                    paste0("integer dimension.values for dimension '", dimensions[[i]],
                           "' must be between 1 and ", length(alphas$dim.names[[ dimensions[i] ]]))
                else
                    NA
            }
            else
            {
                if (all(dimension.values[[i]] != alphas$dim.names[[ dimensions[i] ]]))
                    paste("'", dimension.values[[i]], "' is not a valid value for dimension '",
                          length(alphas$dim.names[[ dimensions[i] ]], "'"))
                else
                    NA
            }
        })
        errors = errors[!is.na(errors)]
        if (length(errors)>0)
            stop(paste0(error.prefix,
                        "Invalid dimension values: \n",
                        paste0(errors, collapse='\n')))
        
        if (!is.numeric(values) || any(is.na(values)))
            stop(paste0(error.prefix, "'values' must be a non-NA numeric vector"))
        
        if (is.interaction)
        {
            if (length(values) != 1)
                stop(paste0(error.prefix,
                            "For an interaction alpha, 'value' must be of length one"))
        }
        else
        {
            if (length(values) != length(dimension.values))
                stop(paste0(error.prefix,
                            "'values' must have the same length as 'dimension.values'"))
        }
    }
    
    dimension.values = sapply(1:length(dimension.values), function(i){
        if (is.integer(dimension.values[[i]]))
            alphas$dim.names[[ dimensions[i] ]][ dimension.values[[i]] ]
        else
            dimension.values[[i]]
    })


    list(dimensions=dimensions,
         dimension.values=dimension.values)    
}

get.dimension.for.values <- function(dim.names,
                                    values)
{
    sapply(values, function(val){
        names(dim.names)[sapply(dim.names, function(dim.values){
            any(dim.values==val)
        })][1]
    })
}

validate.alpha.dimensions.and.values <- function(alphas,
                                              dimensions,
                                              dimension.values)
{
    invalid.dimensions = setdiff(dimensions, names(alphas$dim.names))
    if (length(invalid.dimensions)>0)
        stop()
}

##-------------##
##-- GETTERS --##
##-------------##

#-- GET MODEL DIMENSIONS --#

get.model.dim.names <- function(model)
{
    if (is.null(model) || !is(model, 'model'))
        stop("model must be an object of type 'model'")
    
    if (length(model$dim.names)==0)
        list()
    else
        model$dim.names
}

##--------------------------------##
##-- SCALES and TRANSFORMATIONS --##
##--------------------------------##

check.model.scale.valid <- function(scale)
{
    ALLOWED.SCALES = c('identity','log','logit')
    if (!is.character(scale) || length(scale) != 1 || is.na(scale) ||
        all(scale != ALLOWED.SCALES))
        stop(paste0("'scale' must be one of ",
                    paste0("'", ALLOWED.SCALES, "'", collapse=', ')))
}

get.transform.to.model.scale.function <- function(scale)
{
    if (scale=='identity')
        function(x){x}
    else if (scale=='log')
        log
    else if (scale=='logit')
        function(p){log(p)-log(1-p)}
    else 
        stop(paste0("Invalid model scale: '", scale, "'"))
}

get.transform.from.model.scale.function <- function(scale)
{
    if (scale=='identity')
        function(x){x}
    else if (scale=='log')
        exp
    else if (scale=='logit')
        function(lo){1 / (1 + exp(-lo))}
    else 
        stop(paste0("Invalid model scale: '", scale, "'"))
}

get.model.scale.transformation.function <- function(model, alpha.name)
{
    stop("deprecated - now we do this inside the project function")
    if (is.null(model) || !is(model, 'model'))
        stop("model must be an object of type 'model'")
    
    
    # check alphas
    valid.alphas = get.alpha.names.for.model(model)
    invalid.alphas = setdiff(alpha.name, valid.alphas)
    if (length(invalid.alphas)>0)
        stop(paste0("Invalid alpha(s) for model of type '", model$type, "': ",
                    paste0("'", invalid.alphas, "'", collapse=', ')))
    
    
    if (!is.null(model$scales.for.alphas))
        get.transform.to.model.scale.function(model$scales.for.alphas[alpha.name])
    else if (!is.null(model$scale))
        get.transform.to.model.scale.function(model$scale)
    else
        stop("the scale slot has not been set on this model")
}

##------------------------##
##-- PROJECT FROM MODEL --##
##------------------------##

#'@param future.slope May be NULL (missing)
project.from.model <- function(model,
                               years,
                               alphas,
                               future.slope,
                               future.slope.after.year,
                               dim.names)
{
    if (is.null(model) || !is(model, 'model'))
        stop("model must be an object of class 'model'")
    if (length(years)==0 || !is.numeric(years) || any(is.na(years)))
        stop("years must be a non-empty, non-NA numeric vector")
    if (!is.null(alphas) && !is.list(alphas))
        stop("alphas must be a list")
    if (!is.list(dim.names) || length(dim.names)==0 || is.null(names(dim.names)) || any(names(dim.names)==''))
        stop("dim.names must be a non-empty, named list")
    
    # check alphas
    valid.alphas = get.alpha.names.for.model(model)
    invalid.alphas = setdiff(names(alphas), valid.alphas)
    if (length(invalid.alphas)>0)
        stop(paste0("Invalid alpha(s) for model of type '", model$type, "': ",
                    paste0("'", invalid.alphas, "'", collapse=', ')))
    
    # Invoke the model-specific project function
    if (model$type=='logistic.tail.model')
        project.from.logistic.tail.model(model=model,
                                         years=years,
                                         alphas=alphas,
                                         future.slope=future.slope,
                                         future.slope.after.year=future.slope.after.year,
                                         dim.names=dim.names)
    else if (model$type=='logistic.model')
        project.from.logistic.model(model=model,
                                    years=years,
                                    alphas=alphas,
                                    future.slope=future.slope,
                                    future.slope.after.year=future.slope.after.year,
                                    dim.names=dim.names)
    else if (model$type=='log.rate.model')
        project.from.log.rate.model(model, 
                                    years=years, 
                                    alphas=alphas,
                                    future.slope=future.slope,
                                    future.slope.after.year=future.slope.after.year,
                                    dim.names=dim.names)
    else if (model$type=='natural.spline.model' || model$type=='natural.spline.modified.tail.model')
        project.from.natural.spline.model(model, 
                                          years=years, 
                                          alphas=alphas,
                                          future.slope=future.slope,
                                          future.slope.after.year=future.slope.after.year,
                                          dim.names=dim.names)
    else if (model$type=='logistic.spline.model')
        project.from.logistic.spline.model(model, 
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
    transformation.fn = function(x){
        model$min.proportion + (model$max.proportion-model$min.proportion) / (1+exp(-x))
    }
    
    project.from.transformed.intercept.slope.model(model=model,
                                                   years=years,
                                                   alphas=alphas,
                                                   future.slope=future.slope,
                                                   future.slope.after.year=future.slope.after.year,
                                                   dim.names=dim.names,
                                                   transform.alphas.function = log,
                                                   model.to.result.transformation.function=transformation.fn)
}

project.from.log.rate.model <- function(model, years, 
                                        alphas,
                                        future.slope, future.slope.after.year,
                                        dim.names)
{
    project.from.transformed.intercept.slope.model(model=model,
                                                   years=years,
                                                   alphas=alphas,
                                                   future.slope=future.slope,
                                                   future.slope.after.year=future.slope.after.year,
                                                   dim.names=dim.names,
                                                   transform.alphas.function = log,
                                                   model.to.result.transformation.function=exp)
}

project.from.transformed.intercept.slope.model <- function(model, years, 
                                                           alphas,
                                                           future.slope, 
                                                           future.slope.after.year,
                                                           dim.names,
                                                           transform.alphas.function,
                                                           model.to.result.transformation.function)
{
    intercept = add.alphas.to.array(arr=model$intercept,
                                    alphas=alphas$intercept,
                                    target.dim.names = dim.names,
                                    transform.alphas.function = transform.alphas.function)
    
    slope = add.alphas.to.array(arr=model$slope,
                                alphas=alphas$slope,
                                target.dim.names = dim.names,
                                transform.alphas.function = transform.alphas.function)
   
    future.slope = transform.alphas.function(future.slope)
    
    lapply(years, function(year){
        
        x = intercept + 
            slope * (year - model$anchor.year)
        
        if (!is.null(future.slope) && year > future.slope.after.year)
            x = x + future.slope * (year - future.slope.after.year)
        
        model.to.result.transformation.function(x)
    })
}


project.from.logistic.tail.model <- function(model, 
                                             years, 
                                             alphas,
                                             future.slope, 
                                             future.slope.after.year,
                                             dim.names)
{
    #-- Add Alphas to intercept and slope --#
    intercept = exp(add.alphas.to.array(arr=model$intercept,
                                    alphas=alphas$intercept,
                                    transform.alphas.function = log,
                                    target.dim.names = dim.names))
    
    
    slope = exp(add.alphas.to.array(arr=model$slope,
                                alphas=alphas$slope,
                                transform.alphas.function = log,
                                target.dim.names = dim.names))
    
    #-- Fold in additional.slope.after.year --#
    logistic.slope.sans.additional = slope * model$p.span / (model$logistic.after.p - model$p.min) /
        (model$p.max - model$logistic.after.p)
    
    logistic.slope.with.additional = (slope * future.slope) * model$p.span / (model$logistic.after.p - model$p.min) /
        (model$p.max - model$logistic.after.p)
    
    
    p.at.additional.year = intercept + slope * (future.slope.after.year - model$anchor.year)
    logistic.after.year = model$anchor.year + (model$logistic.after.p - intercept) / slope
    mask = p.at.additional.year < model$logistic.after.p
    logistic.after.year.after.additional = future.slope.after.year + (model$logistic.after.p - p.at.additional.year) / (slope * future.slope)
    logistic.after.year[mask] = logistic.after.year.after.additional[mask]
    
    logistic.intercept = log(model$logistic.after.p - model$p.min) - log(model$p.max - model$logistic.after.p) -
        logistic.slope.sans.additional * (pmin(logistic.after.year, future.slope.after.year) - model$anchor.year) -
        logistic.slope.with.additional * pmax(0, logistic.after.year - future.slope.after.year)
    
    lapply(years, function(year){
        
        # Calculate the RV
        # sub in anything past the use logistic tail threshold
        
        rv = model$intercept + 
            model$slope * (year-model$anchor.year) +
            future.slope * max(0, year-future.slope.after.year)
        
        mask = rv > model$logistic.after.p
        
        # The value based off logistic model
        log.ors = logistic.intercept[mask] +
            logistic.slope.sans.additional[mask] * (pmin(year, future.slope.after.year)-model$anchor.year) +
            logistic.slope.with.additional[mask] * pmax(0, year-future.slope.after.year)
        rv[mask] = model$min.p + model$p.span / (1 + exp(-log.ors))
        
        rv[rv<model$min.p] = model$min.p
        rv
    })
}


project.from.natural.spline.model <- function(model, 
                                              years, 
                                              alphas,
                                              future.slope,
                                              future.slope.after.year,
                                              dim.names)
{
    #-- Set up functions --#
    
    back.transform = get.transform.from.model.scale.function(model$scale)
    forward.transform = get.transform.to.model.scale.function(model$scale)
    if (model$scale == 'log' || model$scale == 'logit')
    {
        transform.alphas.function = log
        future.slope = log(future.slope)
    }
    else
        transform.alphas.function = NULL
    
    #-- Add Alphas to Knots --#
    knot.values = lapply(1:length(model$knot.values), function(k){
        
        alphas.for.knot = alphas[[paste0('knot',k)]]
        if (is.null(alphas.for.knot))
            expand.population(model$knot.values[[k]])
        else
            add.alphas.to.array(arr=model$knot.values[[k]],
                                alphas=alphas.for.knot,
                                transform.alphas.function = transform.alphas.function,
                                target.dim.names = dim.names)
    })
    
    knot.times = model$knot.times
    
    #-- Set up before and after multipliers (if present) --#
    
    if (!is.null(model$before.multiplier) || !is.null(model$after.multiplier))
    {
        if (!is.null(model$before.multiplier))
        {
            alphas.for.multiplier = alphas$before.multiplier
            if (is.null(alphas.for.multiplier))
                before.multiplier = expand.population(model$before.multiplier, dim.names)
            else
                before.multiplier = add.alphas.to.array(arr=before.multiplier,
                                                        alphas=alphas.for.multiplier,
                                                        transform.alphas.function = log,
                                                        target.dim.names = dim.names)
            
            knot.times = c(model$before.time, knot.times)
            knot.values = c(list(forward.transform(exp(before.multiplier) * (back.transform(knot.values[1]) - 
                                                                            back.transform(knot.values[2])))),
                            knot.values)
        }
        
        if (!is.null(model$after.multiplier))
        {
            alphas.for.multiplier = alphas$after.multiplier
            if (is.null(alphas.for.multiplier))
                after.multiplier = expand.population(model$after.multiplier)
            else
                after.multiplier = add.alphas.to.array(arr=after.multiplier,
                                                      alphas=alphas.for.multiplier,
                                                      transform.alphas.function = log,
                                                      target.dim.names = dim.names)
            
            knot.times = c(knot.times, model$after.time)
            knot.values = c(knot.values,
                            list(list(forward.transform(exp(after.multiplier) * (back.transform(knot.values[length(knot.values)]) - 
                                                                                 back.transform(knot.values[length(knot.values)-1]))))))
        }
    }
    
    #-- Fit the spline for each value in the array and project --#
    n = length(knot.values[[1]])
    
    future.slope.to.add = rep(future.slope, length(years))
    future.slope.to.add[years <= future.slope.after.year] = 0
        
    arr.rv = sapply(1:n, function(i){
        knot.values.for.i = sapply(knot.values, function(v){v[i]})
        pre.transformed.values = spline(knot.times, knot.values.for.i, xout = years, method='natural')$y + 
            future.slope.to.add
        back.transform(pre.transformed.values)
    })
    
    
    lapply(1:length(years), function(y){
        sub.rv = arr.rv[,i]
        dim(sub.rv) = sapply(dim.names, length)
        dimnames(sub.rv) = dim.names
        
        sub.rv
    })
}

project.from.logistic.spline.model <- function(model,
                                               years, 
                                               alphas,
                                               future.slope, 
                                               future.slope.after.year,
                                               dim.names)
{
    #-- Prep work --#
    if (model$alphas.are.multipliers)
    {
        transform.alphas.function = log
        future.slope = log(future.slope)
    }
    else
        transform.alphas.function = NULL
    
    
    #-- Add Alphas to Knots --#
    knot.values = lapply(1:length(model$knot.values), function(k){
        
        alphas.for.knot = alphas[[paste0('knot',k)]]
        if (is.null(alphas.for.knot))
            expand.population(model$knot.values[[k]])
        else
            add.alphas.to.array(arr=model$knot.values[[k]],
                                alphas=alphas.for.knot,
                                transform.alphas.function = transform.alphas.function,
                                target.dim.names = dim.names)
    })
    
    knot.times = model$knot.times
    
    #-- Set fraction asymptote --#

    if (is.null(alphas[['fraction.of.asymptote.before.start']]))
        fraction.of.asymptote.before.start = expand.population(model$fraction.of.asymptote.before.start)
    else
        fraction.of.asymptote.before.start = add.alphas.to.array(arr=model$fraction.of.asymptote.before.start,
                                                                 alphas=alphas[['fraction.of.asymptote.before.start']],
                                                                 transform.alphas.function = log,
                                                                 target.dim.names = dim.names)
    
    if (is.null(alphas[['fraction.of.asymptote.after.end']]))
        fraction.of.asymptote.after.end = expand.population(model$fraction.of.asymptote.after.end)
    else
        fraction.of.asymptote.after.end = add.alphas.to.array(arr=model$fraction.of.asymptote.after.end,
                                                                 alphas=alphas[['fraction.of.asymptote.after.end']],
                                                                 transform.alphas.function = log,
                                                                 target.dim.names = dim.names)
    
    
    if (length(knot.values)==3)
    {
        if (is.null(alphas[['fraction.of.asymptote.for.change.dir']]))
            fraction.of.asymptote.for.change.dir = expand.population(model$fraction.of.asymptote.for.change.dir)
        else
            fraction.of.asymptote.for.change.dir = add.alphas.to.array(arr=model$fraction.of.asymptote.for.change.dir,
                                                                     alphas=alphas[['fraction.of.asymptote.for.change.dir']],
                                                                     transform.alphas.function = log,
                                                                     target.dim.names = dim.names)
    }
    
    # check asymptotes
    
    if (any(fraction.of.asymptote.before.start + fraction.of.asymptote.after.end)>1)
        stop("fraction.of.asymptote.before.start and fraction.of.asymptote.after.end cannot sum to more than one")
    if (length(knot.values)==3)
    {
        if (any(fraction.of.asymptote.before.start + fraction.of.asymptote.for.change.dir)>1)
            stop("fraction.of.asymptote.before.start and fraction.of.asymptote.for.change.dir cannot sum to more than one")
        if (any(fraction.of.asymptote.after.end + fraction.of.asymptote.for.change.dir)>1)
            stop("fraction.of.asymptote.after.end and fraction.of.asymptote.for.change.dir cannot sum to more than one")
    }
    
    #-- Fit the spline for each value in the array and project --#
    
    transform.fn = get.transform.from.model.scale.function(model$scale)
    
    arr.rv = sapply(1:n, function(i){
        knot.values.for.i = sapply(knot.values, function(v){v[i]})
        if (length(knot.values.for.i)==2)
            calculate.change.ratios.logistic(r0 = knot.values.for.i[1],
                                             r1 = knot.values.for.i[2],
                                             times = years,
                                             t0 = knot.times[1],
                                             t1 = knot.times[2],
                                             fraction.of.asymptote.after.end=0.05,
                                             fraction.of.asymptote.before.start=0.025)
        else
            calculate.change.ratios.two.logistic(r1,
                                                 r2,
                                                 times=0:t2,
                                                 r0=1,
                                                 t0=0,
                                                 t1=5,
                                                 t2=10,
                                                 fraction.of.asymptote.after.end=0.05,
                                                 fraction.of.asymptote.before.start=0.025,
                                                 fraction.of.asymptote.for.change.dir=0.02)
    })
    
    
    lapply(1:length(years), function(y){
        sub.rv = arr.rv[,i]
        dim(sub.rv) = sapply(dim.names, length)
        dimnames(sub.rv) = dim.names
        
        sub.rv
    })
}


##----------------------------##
##-- UNDER-THE HOOD HELPERS --##
##----------------------------##

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


add.alphas.to.array <- function(arr,
                                alphas,
                                target.dim.names,
                                transform.alphas.function,
                                error.prefix='')
{
    arr = expand.population(arr, target.dim.names)
    
    interaction.effects = alphas$interaction.effects
    alphas = alphas$main.effects
    if (!is.null(transform.alphas.function))
        alphas = lapply(alphas, transform.alphas.function)
    
    if (length(alphas)>0)
    {
        dim.names = target.dim.names
        
        all.alpha = alphas$all
        if (!is.null(all.alpha))
            arr = arr + all.alpha
        
        alphas = alphas[names(alphas)!='all']
        
        if (length(alphas)>0)
        {
            alpha.dims = lapply(names(alphas), function(alpha.name){
                dim = (1:length(dim.names))[names(dim.names)==alpha.name]
                if (length(dim)==0)
                    stop(paste0(error.prefix,
                                "Could not add alphas to array. Dimension '", 
                                alpha.name, "' is not present in the target array"))
                
                rep(dim, length(alphas[[alpha.name]]))
            })
            
            alpha.indices = lapply(1:length(alphas), function(i){
                dim.values = dim.names[[ names(alphas)[i] ]]
                sapply(names(alphas[[i]]), function(alpha.value.name){
                    mask = dim.values==alpha.value.name
                    if (!any(mask))
                        stop(paste0(error.prefix,
                                    "Could not add alphas to array. The '", names(alphas)[i],
                                    "' dimension does not contain the value '", alpha.value.name,
                                    "' in the target array"))
                    (1:length(dim.values))[mask][1]
                })
            })
            
            n.in.dim = sapply(dim.names, length)
            n.before.dim = cumprod(n.in.dim) / n.in.dim
            n.after.dim = rev(cumprod(rev(n.in.dim))) / n.in.dim
            
            arr = arr + do_add_alphas_to_arr(alpha_values = as.numeric(unlist(alphas)),
                                             alpha_dims = as.integer(unlist(alpha.dims)),
                                             alpha_indices = as.integer(unlist(alpha.indices)),
                                             n_before_dim = as.integer(n.before.dim),
                                             n_in_dim = as.integer(n.in.dim),
                                             n_after_dim = as.integer(n.after.dim))
        }
    }

    if (length(interaction.effects)>0)
    {
        arr = add.interaction.alphas.to.array(arr=arr,
                                              interaction.alphas=interaction.effects,
                                              transform.alphas.function = transform.alphas.function,
                                              error.prefix = error.prefix)
    }
    
    arr
}


multiply.alphas.into.array <- function(arr,
                                       alphas,
                                       target.dim.names,
                                       transform.alphas.function)
{
    stop('deprecated')
    do.join.alphas.to.array(arr=arr,
                            alphas=alphas,
                            target.dim.names=target.dim.names,
                            join.function='multiply',
                            transform.alphas.function=transform.alphas.function)
}

do.join.alphas.to.array <- function(arr,
                                    alphas,
                                    target.dim.names,
                                    join.function=c('add','multiply')[1],
                                    transform.alphas.function=NULL)
{
    stop('deprecated')
    arr = expand.population(arr, target.dim.names)
 
    collapse.sex.risk = alphas$interact.sex.risk
    interaction.effects = alphas$interaction.effects
    alphas = alphas$main.effects
    if (!is.null(transform.alphas.function))
        alphas = lapply(alphas, transform.alphas.function)
       
    if (!is.null(alphas))
    {
        if (collapse.sex.risk)
            dim.names = collapse.dim.names.sex.risk(target.dim.names)
        else
            dim.names = target.dim.names
        
        all.alpha = alphas$all
        if (!is.null(all.alpha))
        {
            if (join.function=='add')
                arr = arr + all.alpha
            else if (join.function=='multiply')
                arr = arr * all.alpha
            else
                stop("join.function must be either 'add' or 'multiply'")
        }
        alphas = alphas[names(alphas)!='all']
        
        if (length(alphas)>0)
        {
            alpha.dims = lapply(names(alphas), function(alpha.name){
                dim = (1:length(dim.names))[names(dim.names)==alpha.name]
                if (length(dim)==0)
                    stop(paste0("Could not add alphas to array. Dimension '", alpha.name, "' is not present in the target array"))
                
                rep(dim, length(alphas[[alpha.name]]))
            })
            
            alpha.indices = lapply(1:length(alphas), function(i){
                dim.values = dim.names[[ names(alphas)[i] ]]
                sapply(names(alphas[[i]]), function(alpha.value.name){
                    mask = dim.values==alpha.value.name
                    if (!any(mask))
                        stop("Could not add alphas to array. The '", names(alphas)[i],
                             "' dimension does not contain the value '", alpha.value.name,
                             "' in the target array")
                    (1:length(dim.values))[mask][1]
                })
            })
            
            n.in.dim = sapply(dim.names, length)
            n.before.dim = cumprod(n.in.dim) / n.in.dim
            n.after.dim = rev(cumprod(rev(n.in.dim))) / n.in.dim
            
            if (join.function=='add')
            {
                arr = arr + do_add_alphas_to_arr(alpha_values = as.numeric(unlist(alphas)),
                                                 alpha_dims = as.integer(unlist(alpha.dims)),
                                                 alpha_indices = as.integer(unlist(alpha.indices)),
                                                 n_before_dim = as.integer(n.before.dim),
                                                 n_in_dim = as.integer(n.in.dim),
                                                 n_after_dim = as.integer(n.after.dim))
            }
            else if (join.function=='multiply')
            {
                arr = arr * do_multiply_alphas_into_arr(alpha_values = as.numeric(unlist(alphas)),
                                                        alpha_dims = as.integer(unlist(alpha.dims)),
                                                        alpha_indices = as.integer(unlist(alpha.indices)),
                                                        n_before_dim = as.integer(n.before.dim),
                                                        n_in_dim = as.integer(n.in.dim),
                                                        n_after_dim = as.integer(n.after.dim))
            }
            else
                stop("join.function must be either 'add' or 'multiply'")
        }
    }
    
    if (length(interaction.effects)>0)
    {
        if (join.function=='add')
            arr = add.interaction.alphas.to.array(arr=arr,
                                                  interaction.alphas=interaction.effects,
                                                  transform.alphas.function = transform.alphas.function)
        else
            arr = multiply.interaction.alphas.into.array(arr=arr,
                                                  interaction.alphas=interaction.effects,
                                                  transform.alphas.function = transform.alphas.function)
    }
    
    arr
}



# interaction.alphas is a list, where each element is one interaction term
# each element has two sub-elements
# $dim.values - a named list. The name of each element is a dimension, and the value is the specific index/name into that list
# $value - the value of the alpha to add

add.interaction.alphas.to.array <- function(arr,
                                           interaction.alphas,
                                           transform.alphas.function,
                                           error.prefix)
{
    dim.names = dimnames(arr)
    dims = sapply(dim.names, length)
    
    for (one.interaction in interaction.alphas[[2]])
    {
        dim1 = (1:length(dims))[one.interaction$dim1 == names(dim.names)]
        if (length(dim1)==0)
            stop(paste0(error.prefix,
                        "Cannot add interaction alphas for dimension '",
                        one.interaction$dim1, "' - it is not present in the target.dim.names"))
        dim2 = (1:length(dims))[one.interaction$dim2 == names(dim.names)]
        if (length(dim2)==0)
            stop(paste0(error.prefix,
                        "Cannot add interaction alphas for dimension '",
                        one.interaction$dim2, "' - it is not present in the target.dim.names"))
        
        dim1.values = sapply(one.interaction$dim1.values, function(v){
            mask = dim.names[[dim1]] == v
            if (!any(mask))
                stop(paste0(error.prefix,
                            "Could not add interaction alphas to array. The '", one.interaction$dim1,
                            "' dimension does not contain the value '", v,
                            "' in the target array"))
        })
        
        dim2.values = sapply(one.interaction$dim2.values, function(v){
            mask = dim.names[[dim2]] == v
            if (!any(mask))
                stop(paste0(error.prefix,
                            "Could not add interaction alphas to array. The '", one.interaction$dim2,
                            "' dimension does not contain the value '", v,
                            "' in the target array"))
        })
        
        do_add_interaction_alphas_to_arr(arr=arr,
                                         dims = dims,
                                         dim1 = dim1,
                                         dim1_values = dim1.values,
                                         dim2 = dim2,
                                         dim2_values = dim2.values,
                                         values = transform.alphas.function(one.interaction$values))
    }
    
    arr
}

multiply.interaction.alphas.into.array <- function(arr,
                                            interaction.alphas,
                                            transform.alphas.function)
{
    stop('deprecated')
    for (one.alpha in interaction.alphas)
    {
        if (length(one.alpha$dim.values)!=2)
            stop(paste0("We are only set up to do interaction alphas for two-way interactions at this point. ",
                        "Cannot multiply alphas for ", 
                        paste0(names(one.alpha$dim.values), "='", one.alpha$dim.values, "'",
                               collapse = ' x ')))
        
        mask = get.two.dim.access.indices(dim.names=dimnames(arr),
                                          dim1=names(one.alpha$dim.values)[1],
                                          dim.value1=one.alpha$dim.values[1],
                                          dim2=names(one.alpha$dim.values)[2],
                                          dim.value2=one.alpha$dim.values[2])
        
        if (is.null(transform.alphas.function))
            arr[mask] = arr[mask] * one.alpha$value
        else
            arr[mask] = arr[mask] * transform.alphas.function(one.alpha$value)
    }
    
    arr
}
