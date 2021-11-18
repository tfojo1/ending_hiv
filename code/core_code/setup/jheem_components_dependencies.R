##-- JHEEM COMPONENTS DEPENDENCIES --##

## The elements of a JHEEM Components object are not 1:1 mapped to the eventual JHEEM
##  object when it is created. Some of the components require relatively 'expensive'
##  computations to convert into parameters for the JHEEM
## We therefore set up a process by which we can pre-compute some JHEEM parameters
##  from the components. If the underlying components are subsequently changed, then
##  we need to re-compute the JHEEM parameters.
## This introduces dependencies: JHEEM parameters depend on one or more elements
##  from a JHEEM components object. This file tracks and manages those dependencies


get.components.dependencies <- function(components,
                                        param.name)
{
    
}

clear.components.dependencies <- function(components,
                                          param.name)
{
    
}