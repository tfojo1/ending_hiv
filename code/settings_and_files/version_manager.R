
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
                                              version.manager=version.manager)
    version.manager = do.register.for.version(version=version,
                                              element.name='directory.suffix',
                                              element.value=directory.suffix,
                                              require.unique = T,
                                              version.manager=version.manager)
    version.manager = do.register.for.version(version=version,
                                              element.name='prior.versions',
                                              element.value=prior.versions,
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


get.components.function.for.version <- function(version, version.manager = VERSION.MANAGER)
{
    do.get.for.version(version=version,
                       element.name='get.components.function',
                       version.manager=version.manager)
}

get.parameters.prior.for.version <- function(version, version.manager = VERSION.MANAGER)
{
    do.get.for.version(version=version,
                       element.name='parameters.prior',
                       version.manager=version.manager)
}

get.parameter.sampling.blocks.for.version <- function(version, version.manager = VERSION.MANAGER)
{
    do.get.for.version(version=version,
                       element.name='parameter.sampling.blocks',
                       version.manager=version.manager)
}


##-- SPECIFIC SETTERS --##

register.get.components.function <- function(version,
                                             fn,
                                             version.manager = VERSION.MANAGER)
{
    do.register.for.version(version=version,
                            element.name='get.components.function',
                            element.value=fn,
                            version.manager=version.manager)
}

register.parameters.prior <- function(version,
                                      prior,
                                      version.manager = VERSION.MANAGER)
{
    do.register.for.version(version=version,
                            element.name='parameters.prior',
                            element.value=prior,
                            version.manager=version.manager)
}

register.parameter.sampling.blocks <- function(version,
                                               blocks,
                                               version.manager = VERSION.MANAGER)
{
    do.register.for.version(version=version,
                            element.name='parameter.sampling.blocks',
                            element.value=blocks,
                            version.manager=version.manager)
}




##-- GENERIC HELPER GETTER AND SETTER --##

do.register.for.version <- function(version,
                                    element.name,
                                    element.value,
                                    require.unique=F,
                                    version.manager)
{
    if (!is(version.manager, 'version.manager'))
        stop("'version.manager' must be an object of class 'version.manager'")
    if (all(version.manager$versions!=version))
        stop(paste0("'", version, "' has not been registered as a version with this version manager"))
    if (all(element.name != VERSION.MANAGER.ELEMENTS))
        stop(paste0("'", element.name, "' is not a valid element.name for version managers"))
    
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
    
    version.manager[[element.name]][[version]] = element.value
    version.manager
}

do.get.for.version <- function(version,
                               element.name,
                               version.manager)
{
    if (!is(version.manager, 'version.manager'))
        stop("'version.manager' must be an object of class 'version.manager'")
    if (all(version.manager$versions!=version))
        stop(paste0("'", version, "' has not been registered as a version with this version manager"))
    if (all(element.name != VERSION.MANAGER.ELEMENTS))
        stop(paste0("'", element.name, "' is not a valid element.name for version managers"))
    
    rv = version.manager[[element.name]][[version]]
    if (is.null(rv))
        stop(paste0("No '", element.name, "' for version '", version, "' has been registered with this version manager"))
    
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

