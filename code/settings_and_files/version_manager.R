
##-- CONSTRUCTOR --##

create.version.manager <- function()
{
    rv = list(versions=character(),
              prior.versions=list())
    for (element.name in VERSION.MANAGER.ELEMENTS)
        rv[[element.name]] = list()
    
    class(rv) = 'version.manager'
    rv
}

VERSION.MANAGER.ELEMENTS = c(
    'settings', 'get.components.function', 'parameters.prior','parameter.sampling.blocks',
    'projection.update.components.function', 'projection.parameters.distribution',
    'directory.suffix','prior.versions'
)

##-- TO REGISTER A VERSION --##

register.version <- function(version,
                             settings,
                             directory.suffix,
                             prior.versions=character(),
                             version.manager = VERSION.MANAGER)
{
    version.manager$versions = union(version.manager$versions, version)
    for (element.name in VERSION.MANAGER.ELEMENTS)
        version.manager[[version]][[element.name]] = NULL
    
    if (!is(settings, 'jheem.settings'))
        stop("'settings' must be an object of class 'jheem.settings'")
    
    version.manager = do.register.for.version(version=version,
                                              element.name='settings',
                                              element.value=settings,
                                              element.class='jheem.settings',
                                              version.manager=version.manager)
    version.manager = do.register.for.version(version=version,
                                              element.name='directory.suffix',
                                              element.value=directory.suffix,
                                              element.class='character',
                                              element.length=1,
                                              require.unique = T,
                                              version.manager=version.manager)
    version.manager = do.register.for.version(version=version,
                                              element.name='prior.versions',
                                              element.value=prior.versions,
                                              element.class='character',
                                              version.manager=version.manager)
    
    version.manager
}

##-- SPECIFIC GETTERS --##

get.prior.versions <- function(version,
                               recursive=T,
                               version.manager=VERSION.MANAGER)
{
    prior.versions = do.get.for.version(version=version,
                                        element.name='prior.versions',
                                        version.manager=version.manager)
    
    if (recursive && length(prior.versions)>0)
    {
        prior.index = 1
        while (prior.index <= length(prior.versions))
        {
            to.add = get.prior.versions(version=prior.versions[prior.index],
                                        recursive=T,
                                        version.manager=version.manager)
            
            prior.versions = c(prior.versions, setdiff(to.add, prior.versions))
            prior.index = prior.index + 1
        }
    }
    
    prior.versions
}

get.settings.for.version <- function(version, version.manager = VERSION.MANAGER)
{
    do.get.for.version(version=version,
                       element.name='settings',
                       version.manager=version.manager)
}

get.directory.suffix.for.version <- function(version, version.manager = VERSION.MANAGER)
{
    do.get.for.version(version=version,
                       element.name='directory.suffix',
                       version.manager=version.manager)
}


get.components.function.for.version <- function(version, version.manager = VERSION.MANAGER,
                                                pull.previous.version.value.if.missing = T)
{
    do.get.for.version(version=version,
                       element.name='get.components.function',
                       version.manager=version.manager,
                       pull.previous.version.value.if.missing = pull.previous.version.value.if.missing)
}

get.parameters.prior.for.version <- function(version, version.manager = VERSION.MANAGER,
                                             pull.previous.version.value.if.missing = T)
{
    do.get.for.version(version=version,
                       element.name='parameters.prior',
                       version.manager=version.manager,
                       pull.previous.version.value.if.missing = pull.previous.version.value.if.missing)
}

get.parameter.sampling.blocks.for.version <- function(version, version.manager = VERSION.MANAGER,
                                                      pull.previous.version.value.if.missing = T)
{
    do.get.for.version(version=version,
                       element.name='parameter.sampling.blocks',
                       version.manager=version.manager,
                       pull.previous.version.value.if.missing = pull.previous.version.value.if.missing)
}

get.projection.prarameters.distribution.for.version <- function(version, 
                                                    pull.from.previous.version.if.missing=T,
                                                    version.manager = VERSION.MANAGER)
{
    do.get.for.version(version=version,
                       element.name='projection.parameters.distribution',
                       pull.previous.version.value.if.missing = pull.from.previous.version.if.missing,
                       version.manager=version.manager)
}

get.projection.update.components.function.for.version <- function(version, 
                                                      pull.from.previous.version.if.missing=T,
                                                      version.manager = VERSION.MANAGER)
{
    do.get.for.version(version=version,
                       element.name='projection.update.components.function',
                       pull.previous.version.value.if.missing = pull.from.previous.version.if.missing,
                       version.manager=version.manager)
}


##-- SPECIFIC SETTERS --##

register.get.components.function <- function(version,
                                             fn,
                                             version.manager = VERSION.MANAGER,
                                             join.with.previous.version.function=F)
{
    do.register.for.version(version=version,
                            element.name='get.components.function',
                            element.value=fn,
                            element.class='function',
                            version.manager=version.manager,
                            join.with.previous.version.value = join.with.previous.version.function,
                            join.function = join.get.components.functions)
}

register.parameters.prior <- function(version,
                                      prior,
                                      version.manager = VERSION.MANAGER,
                                      join.with.previous.version.prior = F)
{
    check.join.function = function(dist1, dist2)
    {
        if (length(intersect(dist1@var.names, dist2@var.names)))
            stop(paste0("Cannot join prior distribution with that of previous version, because they have overlapping variable name(s): ",
                        paste0("'", intersect(dist1@var.names, dist2@var.names), "'", collapse=', ')))
    }
    
    do.register.for.version(version=version,
                            element.name='parameters.prior',
                            element.value=prior,
                            element.class='Distribution',
                            version.manager=version.manager,
                            join.with.previous.version.value = join.with.previous.version.prior,
                            join.function = join.distributions,
                            check.join = check.join.function
                            )
}

register.parameter.sampling.blocks <- function(version,
                                               blocks,
                                               version.manager = VERSION.MANAGER,
                                               join.with.previous.version.sampling.blocks=F)
{
    do.register.for.version(version=version,
                            element.name='parameter.sampling.blocks',
                            element.value=blocks,
                            element.class='list',
                            version.manager=version.manager,
                            join.with.previous.version.value = join.with.previous.version.sampling.blocks,
                            join.function=c)
}


register.projection.update.components.function <- function(version,
                                                           fn,
                                                           version.manager = VERSION.MANAGER,
                                                           join.with.previous.version.function=F)
{
    do.register.for.version(version=version,
                            element.name='projection.update.components.function',
                            element.value=fn,
                            element.class='function',
                            version.manager=version.manager,
                            join.with.previous.version.value = join.with.previous.version.function,
                            join.function = join.get.components.functions)
}

register.projection.parameters.distribution <- function(version,
                                      distribution,
                                      version.manager = VERSION.MANAGER,
                                      join.with.previous.version.distribution = F)
{
    check.join.function = function(dist1, dist2)
    {
        if (length(intersect(dist1@var.names, dist2@var.names)))
            stop(paste0("Cannot join projection parameters distribution with that of previous version, because they have overlapping variable name(s): ",
                        paste0("'", intersect(dist1@var.names, dist2@var.names), "'", collapse=', ')))
    }
    
    do.register.for.version(version=version,
                            element.name='projection.parameters.distribution',
                            element.value=distribution,
                            element.class='Distribution',
                            version.manager=version.manager,
                            join.with.previous.version.value = join.with.previous.version.distribution,
                            join.function = join.distributions,
                            check.join = check.join.function
    )
}



##-- GENERIC HELPER GETTER AND SETTER --##

do.register.for.version <- function(version,
                                    element.name,
                                    element.value,
                                    element.class,
                                    element.length=NULL,
                                    require.unique=F,
                                    version.manager,
                                    join.with.previous.version.value=F,
                                    join.function = NULL,
                                    check.join = function(previous.value, element.value){} #prints an error message if appropriate
                                    )
{
    if (!is(version.manager, 'version.manager'))
        stop("'version.manager' must be an object of class 'version.manager'")
    if (all(version.manager$versions!=version))
        stop(paste0("'", version, "' has not been registered as a version with this version manager"))
    if (all(element.name != VERSION.MANAGER.ELEMENTS))
        stop(paste0("'", element.name, "' is not a valid element.name for version managers"))
    
    if (!is(element.value, element.class))
        stop(paste0("The value for '", element.name, "' must be an object of class '", element.class, 
                    "', but its class is: ", 
                    paste0("'", class(element.value), "'", collapse = ' + ')))
    if (!is.null(element.length) && length(element.value)!=element.length)
        stop(paste0("The value for '", element.name, "' must be a ", element.class, " vector of length ", 
                    element.length, ", but it has length ", length(element.value)))
    
    if (require.unique)
    {
        other.versions = setdiff(version.manager$versions, version)
        if (length(other.versions)>0)
        {
            matching.mask = sapply(version.manager[[element.name]][other.versions], function(elem){
                elem==element.value
            })
            
            if (any(matching.mask))
                stop(paste0("Cannot register '", element.value, "' as '", element.name, "' for version '",
                            version, "' with this version.manager. It has already been registered for version '",
                            paste0(other.versions[matching.mask], collapse=', '), "."))
        }
    }
    
    if (join.with.previous.version.value)
    {
        previous.versions = get.prior.versions(version, recursive=F, version.manager = version.manager)
        if (length(previous.versions)==0)
            stop(paste0("Cannot join ", element.name, " with that of previous version, because there is no previous version registered for '", version, "'"))
        if (length(previous.versions)>1)
            stop(stop(paste0("Cannot join ", element.name, " with that of previous version, because there is more than one previous version for '", 
                             version, "': ",
                             paste0("'", previous.versions, "'", collapse=', '))))
        
        previous.value = do.get.for.version(version=previous.versions,
                                            element.name=element.name,
                                            version.manager=version.manager)
        
        check.join(previous.value, element.value)
        
        if (is.null(join.function) || !is.function(join.function))
            stop("You must specify a valid function that takes two arguments as the join.function")
        
        orig.element.value = element.value
        element.value = join.function(previous.value, orig.element.value)
        
        if (!is(element.value, element.class))
            stop(paste0("The value for the joined-with-previous '", element.name, "' must be an object of class '", element.class, 
                        "', but its class is: ", 
                        paste0("'", class(element.value), "'", collapse = ' + ')))
        if (!is.null(element.length) && length(element.value)!=element.length)
            stop(paste0("The value for the joined-with-previous '", element.name, "' must be a ", element.class, " vector of length ", 
                        element.length, ", but it has length ", length(element.value)))
    }
    
    version.manager[[element.name]][[version]] = element.value
    version.manager
}

do.get.for.version <- function(version,
                               element.name,
                               version.manager,
                               pull.previous.version.value.if.missing=F,
                               allow.null=F)
{
    if (!is(version.manager, 'version.manager'))
        stop("'version.manager' must be an object of class 'version.manager'")
    if (all(version.manager$versions!=version))
        stop(paste0("'", version, "' has not been registered as a version with this version manager"))
    if (all(element.name != VERSION.MANAGER.ELEMENTS))
        stop(paste0("'", element.name, "' is not a valid element.name for version managers"))
    
    rv = version.manager[[element.name]][[version]]
    if (is.null(rv))
    {
        if (pull.previous.version.value.if.missing)
        {
            previous.versions = get.prior.versions(version, recursive=F, version.manager = version.manager)
            if (length(previous.versions)==0)
                stop(paste0("Cannot pull ", element.name, " from previous version, because there is no previous version registered for '", version, "'"))
            if (length(previous.versions)>1)
                stop(stop(paste0("Cannot pull ", element.name, " from previous version, because there is more than one previous version for '", 
                                 version, "': ",
                                 paste0("'", previous.versions, "'", collapse=', '))))
            
            rv = do.get.for.version(version=previous.versions,
                                    element.name=element.name,
                                    version.manager=version.manager,
                                    pull.previous.version.value.if.missing=T,
                                    allow.null=T)
            
            if (!allow.null && is.null(rv))
                stop(paste0("No '", element.name, "' for version '", version, 
                            "' or for its parent version ('", previous.versions,
                            "') has been registered with this version manager"))
        }
        else if (!allow.null)
            stop(paste0("No '", element.name, "' for version '", version, "' has been registered with this version manager"))
    }
    
    rv
}


##-- TO DAISY-CHAIN COMPONENTS FUNCTIONS --##

join.get.components.functions <- function(f1, f2)
{
    function(parameters, components,
             data.managers = ALL.DATA.MANAGERS)
    {
        components = f1(parameters, components, data.managers)
        components = f2(parameters, components, data.managers)
        
        components
    }
}

##-- MAKE THE DEFAULT VERSION MANAGER --##
VERSION.MANAGER = create.version.manager()

