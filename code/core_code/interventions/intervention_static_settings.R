

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

static.settings.equal <- function(ss1, ss2)
{
    if (length(ss1$name.chain)!=length(ss2$name.chain) || !all(ss1$name.chain==ss2$name.chain))
        return (F)
    
    if (!setequal(ss1$dependencies, ss2$dependencies) ||
        !setequal(ss1$to.resolve, ss2$to.resolve))
        return (F)
    
    if (!recursive.lists.equal(ss1$value, ss2$value))
        return (F)
    
    T
}

recursive.lists.equal <- function(l1, l2)
{
    if (is(l1, 'list') && is(l2, 'list')) #recurse
    {
        if (length(l1) != length(l2))
            F
        else
            all(sapply(1:length(l1), function(i){
                recursive.lists.equal(l1[[i]], l2[[i]])
            }))
    }
    else if (any(class(l1) != class(l2)))
        F
    else if (length(l1) != length(l2))
        F
    else
        all(l1==l2)
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