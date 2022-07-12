
# This file provides a general interface between the
# (narrow) definition of the JHEEM and the broader way in which 
# it is used by all the setup code


get.sim.version <- function(sim)
{
    if (!is(sim, 'jheem.results'))
        stop("sim must be an object of class 'jheem.results'")
    
    if (is.null(sim$version))
        get.components.version(attr(sim, 'components'))
    else
        sim$version
}

get.sim.settings <- function(sim, version.manager=VERSION.MANAGER)
{
    get.settings.for.version(get.sim.version(sim), version.manager=version.manager)
}

get.sim.components <- function(sim)
{
    if (!is(sim, 'jheem.results'))
        stop("sim must be an object of class 'jheem.results'")
    
    attr(sim, 'components')
}