

create.intervention.static.settings <- function(name.chain,
                                                value,
                                                dependencies=name.chain[1])
{
    rv = list(name.chain=name.chain,
              value=value,
              to.resolve=recursive.get.to.resolve.names(value),
              dependencies=dependencies)
    
    class(rv) = 'static.settings'
    rv
}


static.settings.is.resolved <- function(static.settings)
{
    length(static.settings$to.resolve) == 0    
}

resolve.intervention.static.settings <- function(static.settings,
                                                 parameters)
{
    for (param.name in static.settings$to.resolve)
    {
        if (any(names(parameters)==param.name))
        {
            param.value = parameters[param.name]
            if (is.na(param.value))
                stop("'parameters' cannot contain NA values to resolve")
            
            static.settings$value = recursive.resolve.element(static.settings$value, param.name, param.value)
            
            static.settings$to.resolve = setdiff(static.settings$to.resolve, param.name)
        }
    }
    
    static.settings
}

recursive.get.to.resolve.names <- function(elem)
{
    if (is(elem, 'list'))
        unlist(sapply(elem, recursive.get.to.resolve.names))
    else
    {
        need.to.resolve.mask = need.to.resolve(elem)
        elem[need.to.resolve.mask]
    }
}

recursive.resolve.element <- function(elem, param.name, param.value)
{
    if (is(elem, 'list'))
    {
        for (i in 1:length(elem))
            elem[[i]] = recursive.resolve.element(elem[[i]], param.name, param.value)
    }
    else
        resolve.element(elem, param.name, param.value)
}