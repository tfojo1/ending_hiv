

create.version.manager <- function()
{
    list(get.components.functions=list(),
         parameters.priors=list(),
         parameter.sampling.blocks=list())
}


register.get.components.function <- function(version.manager,
                                             fn,
                                             version)
{
    to.add = list(fn)
    names(to.add) = version
    version.manager$get.components.functions = c(version.manager$get.components.functions,
                                                 to.add)
    
    version.manager
}

get.components.function.for.version <- function(version.manager, version)
{
    version.manager$get.components.functions[[version]]
}

# PARAMETERS PRIOR
register.parameters.prior <- function(version.manager,
                                             prior,
                                             version)
{
    to.add = list(prior)
    names(to.add) = version
    version.manager$parameters.priors = c(version.manager$parameters.priors,
                                                 to.add)
    
    version.manager
}

get.parameters.prior.for.version <- function(version.manager, version)
{
    version.manager$parameters.priors[[version]]
}

# VAR BLOCKS
register.parameter.sampling.blocks <- function(version.manager,
                                      blocks,
                                      version)
{
    to.add = list(blocks)
    names(to.add) = version
    version.manager$parameter.sampling.blocks = c(version.manager$parameter.sampling.blocks,
                                          to.add)
    
    version.manager
}

get.parameter.sampling.blocks.for.version <- function(version.manager, version)
{
    version.manager$parameter.sampling.blocks[[version]]
}





##-- MAKE THE DEFAULT VERSION MANAGER --##
VERSION.MANAGER = create.version.manager()