

create.jheem.components <- function()
{
    
}


set.components.parameters <- function(components,
                                      param.name,
                                      value)
{
    components$parameters[[param.name]] = value
    components = clear.components.dependencies(param.name)
    
    components
}


get.components.dependencies <- function(components,
                                        param.name)
{
    
}

clear.components.dependencies <- function(components,
                                          param.name)
{
    
}