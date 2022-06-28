
# If NULL, applies to all
create.target.population <- function(ages=NULL,
                                     races=NULL,
                                     locations=NULL,
                                     subpopulations=NULL,
                                     sexes=NULL,
                                     risks=NULL,
                                     non.hiv.subsets=NULL,
                                     continuum=NULL,
                                     cd4=NULL,
                                     hiv.subsets=NULL)
{
    pop1 = list(ages=ages,
              races=races,
              locations=locations,
              subpopulations=subpopulations,
              sexes=sexes,
              risks=risks,
              non.hiv.subsets=non.hiv.subsets,
              continuum=continuum,
              cd4=cd4,
              hiv.subsets=hiv.subsets)
    
    pop1 = rv$pop1[!sapply(pop1, is.null)]
    rv = list(pop1=pop1,
              pop2=NULL,
              combine.function='identity')
    
    class(rv) = 'target.population'
    rv
}

#-- COMBINING --#
union.target.populations <- function(pop1, pop2)
{
    rv = pop1 | pop2    
    class(rv) = 'target.population'
    rv
}

intersect.target.populations <- function(pop1, pop2)
{
    rv = pop1 & pop2    
    class(rv) = 'target.population'
    rv
}

diff.target.populations <- function(pop1, pop2)
{
    rv = pop1 & !pop2    
    class(rv) = 'target.population'
    rv
}


#-- RESOLVING --#

resolve.target.population <- function(target.population,
                                      settings,
                                      desired.dimensions=NULL)
{
    # Check arguments
    if (!is(target.population, 'target.population'))
        stop("target.population must be an object of class 'target.population'")
    if (!is(settings, 'jheem.settings'))
        stop("settings must be an object of class 'jheem.settings'")

    # Get the dimensions
    if (is.null(desired.dimensions))
    {
        desired.dimensions = get.target.population.dimensions(target.population)
    }
    else if (!is.character(desired.dimensions) || any(is.na(desired.dimensions)))
        stop("desired.dimensions must be a non-NA character vector")
    else if (length(setdiff(desired.dimensions, names(settings$DIMENSION.NAMES)))>0)
        stop(paste0("Invalid desired dimension(s): ",
                    paste0("'", setdiff(desired.dimensions, names(settings$DIMENSION.NAMES)), "'", collapse=', '),
                    ". desired.dimensions must be a subset of <",
                    paste0("'", names(settings$DIMENSION.NAMES), "'", collapse=", "),
                    ">"))
    
     
    if (rv$combine.function=='identity')
    {
        # Make sure target.pop's dimensions are a subset of the settings dimensions
        if (length(setdiff(names(target.population$pop1), names(settings$DIMENSION.NAMES)))>0)
            stop(paste0("The following dimension(s) are specified in target.populations but not in the settings: ",
                        paste0("'", setdiff(names(target.population$pop1), names(settings$DIMENSION.NAMES)), "'", collapse=', ')))
        
        sapply(names(target.population$pop1), function(d){
            if (length(setdiff(target.population$pop1[[d]], settings$DIMENSION.NAMES[[d]]))>0)
                stop(paste0("Invalid value(s) for dimension '", d, "' in target population: ",
                            paste0("'", setdiff(target.population$pop1[[d]], settings$DIMENSION.NAMES[[d]]), "'", collapse=', '),
                            ". These values are not specified as dimension values in settings."))
        })
        
        # Set up the rv
        dim.names = settings$DIMENSION.NAMES[desired.dimensions]
        rv = array(FALSE, dim=sapply(dim.names, length), dimnames=dim.names)
        
        # Overwrite TRUE into the selected dimensions
        d.rv.for.d.tpop = sapply(1:n.dim.arr, function(d.arr){
            (1:n.dim)[names(dimnames(arr))[d.arr]==names(betas)]
        })
WE ARE HERE        
        rv[indices] = TRUE
                
        # Return
        rv
    }
    else
    {
        # recurse
        pop1 = resolve.target.population(target.population$pop1, settings=settings)
        pop2 = resolve.target.population(target.population$pop2, settings=settings)
        
        if (rv$combine.function=='union')
            pop1 | pop2
        else if (rv$combine.function=='intersect')
            pop1 & pop2
        else if (rv$combine.function=='diff')
            pop1 & !pop2
        else
            stop(paste0("Unrecognized combine.function in target.population object: '", 
                        target.population$combine.function, "'"))
    }
}


get.target.population.dimensions <- function(target.population)
{
    if (!is(target.population, 'target.population'))
        stop("target.population must be an object of class 'target.population'")
    
    if (rv$combine.function=='identity')
        names(rv$pop1)
    else #recurse
        unique(c(get.target.population.dimensions(target.population$pop1),
                 get.target.population.dimensions(target.populat)))
}