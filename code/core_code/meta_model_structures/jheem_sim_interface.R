
# This file provides a general interface between the
# (narrow) definition of the JHEEM and the broader way in which 
# it is used by all the setup code

##---------------##
##-- FROM SIMS --##
##---------------##

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


##---------------------##
##-- FROM COMPONENTS --##
##---------------------##

#formulated for backwards compatibility
get.components.version <- function(components)
{
    if (is.null(components[['version']]))
    {
        if (is.null(components[['settings']]) || is.null(components[['settings']]$VERSION))
            'collapsed_1.0' #did not have version explicitly stored
        else
            components[['settings']]$VERSION
    }
    else
        components$version
}

get.components.settings <- function(components, version.manager=VERSION.MANAGER)
{
    get.settings.for.version(get.components.version(components), version.manager=version.manager)
}

get.components.transition.mapping <- function(components, version.manager=VERSION.MANAGER)
{
    settings = get.components.settings(components, version.manager=version.manager)
    transition.mapping = settings$transition.mapping
    
    transition.mapping
}