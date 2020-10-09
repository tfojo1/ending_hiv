
compress.simset <- function(simset,
                            keep.cdc=F,
                            keep.non.cdc=T,
                            keep.years=2008:2030,
                            keep.dimensions=c('year','age','race','subpopulation','sex','risk','continuum'),
                            compress.continuum.to.diagnosed.vs.undiagnosed=T)
{
    extend.simulations(simset, function(sim, parameters){
        compress.jheem.results(sim,
                               keep.cdc=keep.cdc,
                               keep.non.cdc=keep.non.cdc,
                               keep.years=keep.years,
                               keep.dimensions=keep.dimensions,
                               compress.continuum.to.diagnosed.vs.undiagnosed=compress.continuum.to.diagnosed.vs.undiagnosed)
    })
}

compress.jheem.results <- function(sim,
                                   keep.cdc=F,
                                   keep.non.cdc=T,
                                   keep.years=2000:2030,
                                   keep.dimensions=c('year','age','race','subpopulation','sex','risk','continuum'),
                                   compress.continuum.to.diagnosed.vs.undiagnosed=T)
{
    keep.dimensions = union('year', keep.dimensions)
    arr.mask = sapply(sim, is, 'array')
    cdc.mask = grepl('cdc', names(sim))
    transition.cdc.mask = grepl('cdc', names(sim$tracked.transitions))
    
    #-- Remove CDC or non-CDC arrays --#
    if (!keep.cdc)
    {
        remove.names = names(sim)[arr.mask & cdc.mask]
        remove.transition.names = names(sim$tracked.transitions)[transition.cdc.mask]
    }
    else if (!keep.non.cdc)
    {
        remove.names = names(sim)[arr.mask & !cdc.mask]
        remove.transition.names = names(sim$tracked.transitions)[!transition.cdc.mask]
    }
    else
        remove.names = remove.transition.names = character()
    
    for (name in remove.names)
        sim[[name]] = NULL
    for (name in remove.transition.names)
        sim$tracked.transitions[[name]] = NULL
    arr.mask = sapply(sim, is, 'array')
    
    #-- Collapse arrays --#
    for (arr.name in names(sim)[arr.mask])
        sim[[arr.name]] = compress.jheem.array(sim[[arr.name]], 
                                               sim=sim,
                                               keep.years = keep.years,
                                               keep.dimensions=keep.dimensions,
                                               compress.continuum.to.diagnosed.vs.undiagnosed = compress.continuum.to.diagnosed.vs.undiagnosed)
    
    for (arr.name in names(sim$tracked.transitions))
        sim$tracked.transitions[[arr.name]] = compress.jheem.array(tracked.transitions$arr, 
                                                                   keep.years = keep.years,
                                                                   keep.dimensions=keep.dimensions,
                                                                   compress.continuum.to.diagnosed.vs.undiagnosed = compress.continuum.to.diagnosed.vs.undiagnosed)
    
    #-- Redo the jheem results metadata --#
    
    sim$years = intersect(sim$years, keep.years)
    
    if (compress.continuum.to.diagnosed.vs.undiagnosed && any(keep.dimensions=='continuum'))
    {
        sim$diagnosed.continuum.states = 'diagnosed'
        sim$continuum = c('undiagnosed','diagnosed')
    }
    
    compressed.dimensions = character()
    if (all(keep.dimensions != 'age') && length(sim$ages)>1)
        compressed.dimensions = c(compressed.dimensions, 'ages')
    if (all(keep.dimensions != 'race') && length(sim$races)>1)
        compressed.dimensions = c(compressed.dimensions, 'races')
    if (all(keep.dimensions != 'subpopulation') && length(sim$subpopulations)>1)
        compressed.dimensions = c(compressed.dimensions, 'subpopulations')
    if (all(keep.dimensions != 'sex') && length(sim$sexes)>1)
        compressed.dimensions = c(compressed.dimensions, 'sexes')
    if (all(keep.dimensions != 'sex') && length(sim$cdc.sexes)>1)
        compressed.dimensions = c(compressed.dimensions, 'cdc.sexes')
    if (all(keep.dimensions != 'risk') && length(sim$risks)>1)
        compressed.dimensions = c(compressed.dimensions, 'risks')
    if (all(keep.dimensions != 'risk') && length(sim$cdc.risks)>1)
        compressed.dimensions = c(compressed.dimensions, 'cdc.risks')
    if (all(keep.dimensions != 'hiv.subset') && length(sim$hiv.susbset)>1)
        compressed.dimensions = c(compressed.dimensions, 'hiv.subsets')
    if (all(keep.dimensions != 'cd4') && length(sim$cd4)>1)
        compressed.dimensions = c(compressed.dimensions, 'cd4')
    if (all(keep.dimensions != 'continuum') && length(sim$continuum)>1)
        compressed.dimensions = c(compressed.dimensions, 'continuum')
    if (all(keep.dimensions != 'hiv.subset') && length(sim$hiv.subsets)>1)
        compressed.dimensions = c(compressed.dimensions, 'hiv.subsets')
    
    for (dimension in compressed.dimensions)
        sim[[dimension]] = 'all'

    #-- Compress Components --#
    #attr(sim, 'components') = compress.components(attr(sim, 'components'))
    
    #-- Return --#
    sim
}

subset.simset.by.year <- function(sim, years=2018:2020)
{
    compress.simset(sim,
                    keep.cdc=T,
                    keep.non.cdc=T,
                    keep.years=years,
                    keep.dimensions=c('year', 'age', 'race', 'subpopulation', 'sex', 'risk',
                                      'non.hiv.subset', 'continuum', 'cd4', 'hiv.subset'),
                    compress.continuum.to.diagnosed.vs.undiagnosed=F)
}

subset.jheem.results.by.year <- function(sim, years=2018:2020)
{
    compress.jheem.results(sim,
                           keep.cdc=T,
                           keep.non.cdc=T,
                           keep.years=years,
                           keep.dimensions=c('year', 'age', 'race', 'subpopulation', 'sex', 'risk',
                                             'non.hiv.subset', 'continuum', 'cd4', 'hiv.subset'),
                           compress.continuum.to.diagnosed.vs.undiagnosed=F)
}

#not using this anymore - they weren't big enough to make it worthwhile
compress.components <- function(components)
{
    to.retain = c('background.prep.years',
                  'background.prep.coverage',
                  'background.suppression',
                  'background.testing',
                  names(components)[grepl('foreground', names(components))],
                  'jheem',
                  'background.change.to.years'
                  )
    
    
    components[to.retain]
}

compress.jheem.array <- function(arr,
                                 sim,
                                 keep.years,
                                 keep.dimensions,
                                 compress.continuum.to.diagnosed.vs.undiagnosed)
{
    dim.names = dimnames(arr)
    
    keep.years = intersect(as.character(keep.years),
                           dimnames(arr)[['year']])
    if (length(keep.years) < dim(arr)['year'])
    {
        dim(arr) = c(dim(arr)[1], prod(dim(arr)[-1]))
        dimnames(arr) = list(year=dim.names[['year']], other=NULL)
        
        dim.names[['year']] = keep.years
        arr = arr[keep.years,]
        
        dim(arr) = sapply(dim.names, length)
        dimnames(arr) = dim.names
    }
    
    to.keep = sapply(names(dim.names), function(name){any(name==keep.dimensions)})
    length.1 = dim(arr) == 1
    
    non.compress.dimensions = names(dim.names)[to.keep | length.1]
    compress.dimensions = names(dim.names)[!to.keep & !length.1]

    if (length(compress.dimensions)>0)
    {
        dim.names[compress.dimensions] = 'all'
    
        arr = apply(arr, non.compress.dimensions, sum)
        dim(arr) = sapply(dim.names, length)
        dimnames(arr) = dim.names
    }
    
    if (compress.continuum.to.diagnosed.vs.undiagnosed && any(non.compress.dimensions=='continuum') &&
        length('continuum')>1)
    {
        non.continuum.dimensions = setdiff(names(dim.names), 'continuum')
        undiagnosed.continuum.states = setdiff(dim.names[['continuum']], sim$diagnosed.continuum.states)
        
        arr = apply(arr, non.continuum.dimensions, function(x){
            c(undiagnosed=sum(x[undiagnosed.continuum.states]),
              diagnosed=sum(x[sim$diagnosed.continuum.states])
              )
        })
        new.dim.names = dimnames(arr)
        names(new.dim.names)[1] = 'continuum'
        dim(arr) = sapply(new.dim.names, length)
        dimnames(arr) = new.dim.names
        
        arr = apply(arr, names(dim.names), function(x){x})
    }
    
    arr
}