

# An intervention consists of a combination of
# 1) Target populations to wh



##------------------##
##-- CONSTRUCTORS --##
##------------------##

#An intervention object is a list with two components, $raw and $processed
# Each of these is a list with one or more elements corresponding to types of unit interventions (testing, prep, suppression, etc)
# Each of these lists contains two lists: $target.populations and $unit.interventions

#'@param ... Must be either target.population or unit.intervention objects or lists containing only target.population or unit.intervention objects
#'@details Creates an intervention object that applies all the unit interventions contained in ... to all the target populations
#'
#applies the unit interventions to all target populations
create.intervention <- function(...)
{
    # Parse arguments
    args = flatten.list(...)
    
    target.populations = args[sapply(args, is, 'target.population')]
    intervention.units = args[sapply(args, is, 'intervention.unit')]
    
    # Check  types
    if ((length(target.populations) + length(intervention.units)) < length(args))
        stop("The arguments to create.intervention must be either target.population or unit.intervention objects or lists containing only target.population or unit.intervention objects")
    if (length(target.populations)==0)
        stop("There must be at least one target population passed as an argument to create.intervention")
    if (length(intervention.units)==0)
        stop("There must be at least one intervention unit passed as an argument to create.intervention")
    
    # Put it into the structure
    unit.types = sapply(intervention.units, function(unit){unit$type})
    
    rv = list(raw=list())
    for (unit in intervention.units)
    {
        if (is.null(rv$raw[[unit$type]]))
            rv$raw[[unit$type]] = list(target.populations=list(),
                                       intervention.units=list())
        
        for (target.population in target.populations)
        {
            rv$raw[[unit$type]]$target.populations = c(rv$raw[[unit$type]]$target.populations, list(target.population))
            rv$raw[[unit$type]]$intervention.units = c(rv$raw[[unit$type]]$intervention.units, list(unit))
        }
    }
    
    intervention.consistency.check(rv)
    
    rv = process.intervention(rv)
    class(rv) = 'intervention'
    rv
}

join.interventions <- function(...)
{
    interventions.to.join = flatten.list(...)
    if (any(!sapply(interventions.to.join, is, 'intervention')))
        stop("The arguments to join interventions must be only intervention objects or lists containing only intervention objects")
    
    rv = interventions.to.join[[1]]
    rv$processed = NULL
    if (length(interventions.to.join)>1)
    {
        for (int in interventions.to.join[-1])
        {
            for (type in names(int$raw))
            {
                if (is.null(rv$raw[[type]]))
                    rv$raw[[type]] = list(target.populations=list(),
                                               intervention.units=list())
                
                sub = int$raw[[type]]
                rv$raw[[type]]$target.populations = c(rv$raw[[type]]$target.populations, int$raw[[type]]$target.populations)
                rv$raw[[type]]$intervention.units = c(rv$raw[[type]]$intervention.units, int$raw[[type]]$intervention.units)
            }
        }
    }
    
    intervention.consistency.check(rv)
    rv = process.intervention(rv)
    
    class(rv) = 'intervention'
    rv
}

create.null.intervention <- function()
{   
    rv = list(raw=list())
    class(rv) = 'intervention'
    rv
}

intervention.consistency.check <- function(intervention)
{
    sapply(names(intervention$raw), function(type){
        bucket = intervention$raw[[type]]
        pop.hashes = sapply(bucket$target.populations, logical.to.6.bit)
        if (max(table(pop.hashes))>1)
            stop("An intervention can only have one sub-intervention per population, but the intervention has more than one sub-intervention of type ", type,
                 " for certain sub-populatfions")
    })
}


interventions.equal <- function(int1, int2)
{
    if (!length(int1$raw) == length(int2$raw))
        return (F)
    
    if (length(setdiff(names(int1$raw), names(int2$raw)))>0)
        return (F)
    
    if (any(!sapply(names(int1$raw), function(type){
            t1 = int1$raw[[type]]
            t2 = int2$raw[[type]]
            
            if (length(t1$target.populations) != length(t2$target.populations))
                return (F)
            if (length(t1$intervention.units) != length(t2$intervention.units))
                return (F)
            
            if (any(!sapply(1:length(t1$target.populations), function(i){
                    target.populations.equal(t1$target.populations[[i]], t2$target.populations[[i]])
                })))
                return (F)
            
            if (any(!sapply(1:length(t1$intervention.units), function(i){
                    intervention.units.equal(t1$intervention.units[[i]], t2$intervention.units[[i]])
                })))
                return (F)
            
            return(T)
        })))
        return (F)
    
    return (T)
}

is.null.intervention <- function(int)
{
    length(int$raw) == 0
}

get.intervention.unit.types <- function(int)
{
    names(int$raw)
}

##------------------##
##-- DESCRIPTIONS --##
##------------------##

get.intervention.description <- function(int,
                                         delimiter='\n',
                                         bullet.pre=' - ',
                                         bullet.post='',
                                         pre.header='',
                                         post.header='',
                                         pre.list='',
                                         post.list='',
                                         pre.target.pop='',
                                         post.target.pop='')
{
    rv = NULL
    for (type in names(int$raw))
    {
        if (is.null(rv))
            rv = ''
        else
            rv = paste0(rv, delimiter)
        
        #rv = paste0(rv, pre.header, INTERVENTION.UNIT.TYPE.PRETTY.NAMES[type], ":", post.header)
        category = int$raw[[type]]$intervention.units[[1]]$name.metadata$category
        rv = paste0(rv, pre.header, category,":", post.header)
        
        subset = int$raw[[type]]
        
        rv = paste0(rv, pre.list)
        for (i in 1:length(subset$target.populations))
        {
            rv = paste0(rv, delimiter, bullet.pre)
            rv = paste0(rv, pre.target.pop,
                        target.population.name(subset$target.populations[[i]]), ": ",
                        post.target.pop)
            rv = paste0(rv, get.intervention.unit.name(subset$intervention.units[[i]],
                                                       include.start.time=T))
            rv = paste0(rv, bullet.post)
        }
        rv = paste0(rv, post.list)
    }
    
    rv
}

get.intervention.description.table <- function(int,
                                               empty.value='',
                                               include.start.time=F,
                                               include.end.time=F,
                                               round.digits=2)
{
    target.populations = get.unique.target.populations(int)
    
    # for now we are using a shortcut assumptions that types is 1:1 with categories
    types = names(int$raw)
    rv = sapply(types, function(type){
        subset = int$raw[[type]]
        sapply(target.populations, function(tpop){
            tpop.mask = sapply(subset$target.populations, target.populations.equal, tpop)
            tpop.index = (1:length(subset$target.populations))[tpop.mask]
            
            if (any(tpop.mask))
                get.intervention.unit.name(subset$intervention.units[[tpop.index]],
                                           include.start.time=include.start.time,
                                           include.end.time=include.end.time,
                                           round.digits=round.digits)
            else
                empty.value
        })
    })
    
    dim(rv) = c(target.population = length(target.populations), intervention.type=length(types))
    
    dimnames(rv) = list(NULL, intervention.type=types)
    attr(rv, 'unit.types') = sapply(types, function(type){
        int$raw[[type]]$intervention.units[[1]]$name.metadata$category
    })
    attr(rv, 'target.populations') = target.populations
#    attr(rv, 'target.population.names') = sapply(target.populations, target.population.name)
    
    rv
    
}

get.intervention.html.description <- function(int,
                                              delimiter='',
                                              bullet.pre='<li>',
                                              bullet.post='</li>',
                                              pre.header='<b>',
                                              post.header='</b>',
                                              pre.list='<ul>',
                                              post.list='</ul>',
                                              pre.target.pop="<i>",
                                              post.target.pop="</i>")
{
    get.intervention.description(int,
                                 delimiter=delimiter,
                                 bullet.pre=bullet.pre,
                                 bullet.post=bullet.post,
                                 pre.header=pre.header,
                                 post.header=post.header,
                                 pre.list=pre.list,
                                 post.list=post.list,
                                 pre.target.pop=pre.target.pop,
                                 post.target.pop=post.target.pop)
}

get.intervention.description.by.target <- function(int,
                                                   include.start.text=T,
                                                   tpop.delimeter='\n',
                                                   unit.delimiter=', ',
                                                   pre='',
                                                   post='',
                                                   bullet.pre=' - ',
                                                   bullet.post='',
                                                   pre.header='',
                                                   post.header=': ')
{
    tpops = get.target.populations.for.intervention(int)
    
    rv = pre
    for (i in 1:length(tpops))
    {
        tpop = tpops[[i]]
        if (i>1)
            rv = paste0(rv, tpop.delimeter)
        
        #figure out if we can use the same year for all of them
        units.for.tpop = list()
        for (sub in int$raw)
        {
            tpop.indices = (1:length(sub$target.populations))[sapply(sub$target.populations, target.populations.equal, tpop)]
            if (length(tpop.indices)>0)
                units.for.tpop = c(units.for.tpop, list(sub$intervention.units[[tpop.indices]]))
        }
        
        all.unit.years.same = all(sapply(units.for.tpop, function(unit){
            length(unit$years)==1 && 
                length(unit$years)==length(units.for.tpop[[1]]$years) &&
                all(unit$years == units.for.tpop[[1]]$years)
        }))
        
        all.unit.starts.same = all(sapply(units.for.tpop, function(unit){
            unit$start.year == units.for.tpop[[1]]$start.year
        }))
        
        units.text = ''
        for (i in 1:length(units.for.tpop))
        {
            if (units.text != '')
                units.text = paste0(units.text, unit.delimiter)
            units.text = paste0(units.text, get.intervention.unit.name(units.for.tpop[[i]], 
                                                             include.start.time=include.start.text && (!all.unit.starts.same || i==1),
                                                             include.end.time=!all.unit.years.same || i==length(units.for.tpop),
                                                             round.digits=1))
        }

        rv = paste0(rv,
                    bullet.pre,
                    pre.header,
                    target.population.name(tpop),
                    post.header,
                    units.text,
                    bullet.post)
    }
    
    rv = paste0(rv, post)
    rv
}

get.intervention.html.description.by.target <- function(int,
                                                        include.start.text=T,
                                                   tpop.delimeter='',
                                                   unit.delimiter=', ',
                                                   pre='<ul>',
                                                   post='</ul>',
                                                   bullet.pre='<li>',
                                                   bullet.post='</li>',
                                                   pre.header='<b>',
                                                   post.header=':</b> ')
{
    get.intervention.description.by.target(int,
                                           include.start.text=include.start.text,
                                           tpop.delimeter=tpop.delimeter,
                                           unit.delimiter=unit.delimiter,
                                           pre=pre,
                                           post=post,
                                           bullet.pre=bullet.pre,
                                           bullet.post=bullet.post,
                                           pre.header=pre.header,
                                           post.header=post.header)
}

##------------------------------##
##-- THE INTERVENTION MANAGER --##
##------------------------------##

create.intervention.manager <- function()
{
    list(intervention=list(),
         code=character(),
         name=character())
}

RESERVED.INTERVENTION.CODES = c('baseline','seed','full')

register.intervention <- function(int, 
                                  code,
                                  name,
                                  short.name=name,
                                  manager=INTERVENTION.MANAGER.1.0,
                                  allow.intervention.multiple.names=F)
{
    if (!is(int, 'intervention'))
        stop("'int' must be of class 'intervention'")
    
    if (any(code==RESERVED.INTERVENTION.CODES))
        stop(paste0("'", code, "' is a reserved keyword and cannot be used as an intervention code"))

    # Check if already added under a different name or code

    if (!allow.intervention.multiple.names)
    {
        mask = sapply(manager$intervention, function(comp){
            interventions.equal(int, comp)
        })
        
        if (any(mask))
        {
            if (manager$name[mask] == name)
            {
                if (manager$code[mask] == code)
                    return (manager)
                stop(paste0("Attempted to add an intervention (with code '",
                            code, "') which is already present with code '",
                            manager$code[mask], "'. There can only be one code per intervention."))
            }
            else
                stop(paste0("Attempted to add an intervention (under the name '",
                            name, "') which is already present under the name '",
                            manager$name[mask], "'. There can only be one name per intervention."))
        }
    }
    
    # Check to confirm that name and code are unique
#    if (any(manager$name==name))
 #       stop(paste0("A different intervention is already present under the name, '",
  #                  name, "'. Intervention names must be unique"))
    if (any(manager$code==code))
        stop(paste0("A different intervention is already present under the code, '",
                    code, "'. Intervention codes must be unique"))
    
    # Add it in and return
    manager$intervention = c(manager$intervention, list(int))
    names(manager$intervention) = code
    
    manager$code = c(manager$code, code)
    
    manager$name = c(manager$name, name)
    names(manager$name) = manager$code
    
    manager$short.name = c(manager$short.name, short.name)
    names(manager$short.name) = manager$code
    
    
    manager
}



get.intervention.code <- function(int, manager=INTERVENTION.MANAGER.1.0)
{   
    mask = sapply(manager$intervention, function(comp){
        interventions.equal(int, comp)
    })
    if (any(mask))
        as.character(manager$code[mask])
    else
        NULL
}

get.intervention.name <- function(int, manager=INTERVENTION.MANAGER.1.0,
                                  throw.error.if.multiple.names=T)
{   
    if (is.null(int))
        return ("No Intervention")
    
    mask = sapply(manager$intervention, function(comp){
        interventions.equal(int, comp)
    })
    if (any(mask))
    {
        if (sum(mask)>1 && throw.error.if.multiple.names)
            stop(paste0("More than one name for the given intervention has been registered: ",
                        paste0("'", manager$name[mask], "'", collapse=", ")))
        as.character(manager$name[mask])
    }
    else
        NULL
}

get.intervention.short.name <- function(int, manager=INTERVENTION.MANAGER.1.0)
{   
    if (is.null(int))
        return ("No Intervention")
    
    mask = sapply(manager$intervention, function(comp){
        interventions.equal(int, comp)
    })
    if (any(mask))
        as.character(manager$short.name[mask])
    else
        NULL
}

intervention.from.code <- function(code, manager=INTERVENTION.MANAGER.1.0)
{
    mask = manager$code==code
    if (any(mask))
        manager$intervention[[ (1:length(manager$intervention))[mask] ]]
    else
        NULL
}

intervention.from.short.name <- function(short.name, manager=INTERVENTION.MANAGER.1.0)
{
    code = intervention.short.name.to.code(short.name, manager=manager)
    if (is.null(code))
        NULL
    else
        intervention.from.code(code, manager=manager)
}

intervention.code.to.name <- function(code, manager=INTERVENTION.MANAGER.1.0)
{
    mask = manager$code==code
    if (any(mask))
        as.character(manager$name[mask])
    else
        NULL
}

intervention.code.to.short.name <- function(code, manager=INTERVENTION.MANAGER.1.0)
{
    mask = manager$code==code
    if (any(mask))
        as.character(manager$short.name[mask])
    else
        NULL
}

intervention.name.to.code <- function(name, manager=INTERVENTION.MANAGER.1.0)
{
    mask = manager$name==name
    if (any(mask))
        as.character(manager$code[[ (1:length(manager$code))[mask] ]])
    else
        NULL
}

intervention.short.name.to.code <- function(name, manager=INTERVENTION.MANAGER.1.0)
{
    mask = manager$short.name==name
    if (any(mask))
        as.character(manager$code[[ (1:length(manager$code))[mask] ]])
    else
        NULL
}

#returns an indexing such that, if applied to the list of interventions,
#interventions appear in the order they do in manager
order.interventions <- function(interventions,
                                manager=INTERVENTION.MANAGER.1.0,
                                decreasing=F)
{
    codes = sapply(interventions, get.intervention.code)
    all.values = 1:length(manager$code)
    names(all.values) = manager$code
    
    values = all.values[codes]
    order(values, decreasing = decreasing)
}

get.target.populations.for.intervention <- function(int)
{
    if (length(int$raw)==0)
        list()
    else
    {
        tpops = int$raw[[1]]$target.populations
        if (length(int$raw)>1)
        {
            for (i in 2:length(int$raw))
            {
                tpops2 = int$raw[[i]]$target.populations
                already.included = sapply(tpops2, function(tpop){
                    any(sapply(tpops, target.populations.equal, tpop))
                })
                tpops = c(tpops, tpops2[!already.included])
            }
        }
        
        tpops
    }
}

union.intervention.lists <- function(int.list.1, int.list.2,
                                     manager = INTERVENTION.MANAGER.1.0)
{
    codes1 = sapply(int.list.1, get.intervention.code, manager = manager)
    codes2 = sapply(int.list.2, get.intervention.code, manager = manager)
    
    codes.all = union(codes1, codes2)
    
    lapply(codes.all, intervention.from.code, manager = manager)
}

##------------------------------------##
##-- PROCESS AN INTERVENTION OBJECT --##
##------------------------------------##

process.intervention <- function(intervention)
{
    if (is.null.intervention(intervention))
        intervention$processed = list()
    else
    {
        intervention$processed = lapply(names(intervention$raw), function(type){
            sub = intervention$raw[[type]]
            
            dim.names = dimnames(sub$target.populations[[1]])
            rv = list()
            rv$start.times = rv$end.times = 
              rv$min.rates = rv$max.rates =
              rate.template.arr = array(as.numeric(NA), dim=sapply(dim.names, length), dimnames=dim.names)
            rv$start.times[] = rv$end.times[] = rv$max.rates[] = Inf
            rv$min.rates[] = -Inf
            
            character.na=character(); character.na[1] = NA
            rv$apply.functions = array(character.na, dim=sapply(dim.names, length), dimnames=dim.names)
            rv$allow.less.than.otherwise = array(F, dim=sapply(dim.names, length), dimnames=dim.names)
            
            rv$times = sort(unique(c(
                unlist(sapply(sub$intervention.units, function(unit){unit$years})),
                sapply(sub$intervention.units, function(unit){unit$start.year})
                )))
            rv$rates = lapply(rv$times, function(time){rate.template.arr})
            
            for (j in length(sub$target.populations):1)
            {
                tpop = sub$target.populations[[j]]
                unit = sub$intervention.units[[j]]
                rv$start.times[tpop] = unit$start.year
                rv$end.times[tpop] = unit$end.year
                rv$apply.functions[tpop] = unit$apply.function
                rv$allow.less.than.otherwise[tpop] = unit$allow.less.than.otherwise
                rv$min.rates[tpop] = unit$min.rate
                rv$max.rates[tpop] = unit$max.rate
                
                rates.for.tpop = interpolate.parameters(values=unit$rates,
                                                        value.times = unit$years,
                                                        desired.times = rv$times,
                                                        return.list=F)
                rates.for.tpop[rv$times<unit$years[1]] = NA
                
                for (i in 1:length(rates.for.tpop))
                    rv$rates[[i]][tpop] = rates.for.tpop[i]
            }
            
            rv
        })
        
        names(intervention$processed) = names(intervention$raw)
    }

    intervention
}

##----------------------------------##
##-- SUMMARIZE TARGET POPULATIONS --##
##----------------------------------##

get.unique.target.populations <- function(int)
{
    all.target.populations = list()
    for (subset in int$raw)
        all.target.populations = c(all.target.populations, subset$target.populations)
    all.target.population.hashes = sapply(all.target.populations, target.population.hash)
    
    #unique
    target.population.hashes = unique(all.target.population.hashes)
    target.populations = lapply(target.population.hashes, function(hash){
        all.target.populations[all.target.population.hashes==hash][[1]]
    })
    
    names(target.populations) = NULL
    
    target.populations
}

##-----------------------##
##-- LOW-LEVEL HELPERS --##
##-----------------------##

flatten.list <- function(...)
{
    args = list(...)
    rv = list()
    for (elem in args)
    {
        if (is(elem, 'list'))
        {
            for (sub.elem in elem)
            {
                if (is(sub.elem, 'list'))
                    rv = c(rv, flatten.list(sub.elem))
                else
                    rv = c(rv, list(sub.elem))
            }
        }
        else
            rv = c(rv, list(elem))
    }
    
    rv
}







##----------------------##
##-- SETUP COMPONENTS --##
##----------------------##

setup.components.for.intervention <- function(components,
                                              intervention,
                                              overwrite.prior.intervention=F)
{
    if (is.null(intervention) || is.null.intervention(intervention))
        return (components)
    
    # Set up
    idu.states = 'active_IDU'
    
    # Testing
    if (!is.null(intervention$processed$testing))
    {
        diagnosed.states = components$settings$DIAGNOSED_STATES
        rates = lapply(intervention$processed$testing$rates, function(r)
        {
            r = expand.population.to.hiv.positive(components$jheem, r)
            r[,,,,,diagnosed.states,,] = 0
            
            r
        })
        
        start.times = expand.population.to.hiv.positive(components$jheem, intervention$processed$testing$start.times)
        start.times[,,,,,diagnosed.states,,] = Inf
        end.times = expand.population.to.hiv.positive(components$jheem, intervention$processed$testing$end.times)
        end.times[,,,,,diagnosed.states,,] = Inf
        
        min.rates = expand.population.to.hiv.positive(components$jheem, intervention$processed$testing$min.rates)
        min.rates[,,,,,diagnosed.states,,] = -Inf
        max.rates = expand.population.to.hiv.positive(components$jheem, intervention$processed$testing$max.rates)
        max.rates[,,,,,diagnosed.states,,] = -Inf
        
        apply.functions = expand.character.array(expand.population.to.hiv.positive,
                                                 components$jheem,
                                                 intervention$processed$testing$apply.functions)
        apply.functions[,,,,,diagnosed.states,,] = NA
        allow.less = expand.population.to.hiv.positive(components$jheem, 
                                                       intervention$processed$testing$allow.less.than.otherwise)
        
        
        components = set.foreground.rates(components, 'testing',
                                          rates = rates,
                                          years = intervention$processed$testing$times,
                                          start.years = start.times,
                                          end.years = end.times,
                                          apply.functions = apply.functions,
                                          allow.foreground.less = allow.less,
                                          overwrite.previous = overwrite.prior.intervention,
                                          foreground.min = min.rates,
                                          foreground.max = max.rates)
    }
    
   
    
    # Suppression
    if (!is.null(intervention$processed$suppression))
    {
        undiagnosed.states = setdiff(components$settings$CONTINUUM_OF_CARE, components$settings$DIAGNOSED_STATES) #setdiff(dimnames(start.times)[['continuum']], 'diagnosed')
        suppressed.proportions = lapply(intervention$processed$suppression$rates, function(r)
        {
            r = expand.population.to.hiv.positive(components$jheem, r)
            r[,,,,,undiagnosed.states,,] = 0
            
            r
        })
        
        start.times = expand.population.to.hiv.positive(components$jheem, intervention$processed$suppression$start.times)
        start.times[,,,,,undiagnosed.states,,] = Inf
        end.times = expand.population.to.hiv.positive(components$jheem, intervention$processed$suppression$end.times)
        end.times[,,,,,undiagnosed.states,,] = Inf
        
        min.rates = expand.population.to.hiv.positive(components$jheem, intervention$processed$suppression$min.rates)
        min.rates[,,,,,undiagnosed.states,,] = -Inf
        max.rates = expand.population.to.hiv.positive(components$jheem, intervention$processed$suppression$max.rates)
        max.rates[,,,,,undiagnosed.states,,] = -Inf
        
        apply.functions = expand.character.array(expand.population.to.hiv.positive,
                                                 components$jheem,
                                                 intervention$processed$suppression$apply.functions)
        apply.functions[,,,,,undiagnosed.states,,] = NA
        allow.less = expand.population.to.hiv.positive(components$jheem, 
                                                                  intervention$processed$suppression$allow.less.than.otherwise)
        
        components = set.foreground.rates(components, 'suppression',
                                          rates = suppressed.proportions,
                                          years = intervention$processed$suppression$times,
                                          start.years = start.times,
                                          end.years = end.times,
                                          apply.functions = apply.functions,
                                          allow.foreground.less = allow.less,
                                          overwrite.previous = overwrite.prior.intervention,
                                          foreground.min = min.rates,
                                          foreground.max = max.rates)
    }
    
    # PrEP
    if (!is.null(intervention$processed$prep))
    {
        rates = lapply(intervention$processed$prep$rates, expand.population.to.hiv.negative, jheem=components$jheem)
        
        start.times = expand.population.to.hiv.negative(components$jheem, intervention$processed$prep$start.times)
        end.times = expand.population.to.hiv.negative(components$jheem, intervention$processed$prep$end.times)
        min.rates = expand.population.to.hiv.negative(components$jheem, intervention$processed$prep$min.rates)
        max.rates = expand.population.to.hiv.negative(components$jheem, intervention$processed$prep$max.rates)
        apply.functions = expand.character.array(expand.population.to.hiv.negative,
                                                 components$jheem,
                                                 intervention$processed$prep$apply.functions)
        allow.less = expand.population.to.hiv.negative(components$jheem, 
                                                                  intervention$processed$prep$allow.less.than.otherwise)
        
        components = set.foreground.rates(components, 'prep',
                                          rates = rates,
                                          years = intervention$processed$prep$times,
                                          start.years = start.times,
                                          end.years = end.times,
                                          apply.functions = apply.functions,
                                          allow.foreground.less = allow.less,
                                          overwrite.previous = overwrite.prior.intervention,
                                          foreground.min = min.rates,
                                          foreground.max = max.rates)
    }
  
    # Needle Exchange
    if (!is.null(intervention$processed$needle.exchange))
    {
        rates = lapply(intervention$processed$needle.exchange$rates, function(r){
            expand.population.to.general(jheem=components$jheem, r)[,,,,idu.states]
        })
        
        start.times = expand.population.to.general(components$jheem, intervention$processed$needle.exchange$start.times)[,,,,idu.states]
        end.times = expand.population.to.general(components$jheem, intervention$processed$needle.exchange$end.times)[,,,,idu.states]
        min.rates = expand.population.to.general(components$jheem, intervention$processed$needle.exchange$min.rates)[,,,,idu.states]
        max.rates = expand.population.to.general(components$jheem, intervention$processed$needle.exchange$max.rates)[,,,,idu.states]
        apply.functions = expand.character.array(expand.population.to.general,
                                                 components$jheem,
                                                 intervention$processed$needle.exchange$apply.functions)[,,,,idu.states]
        allow.less = expand.population.to.general(components$jheem, 
                                                  intervention$processed$needle.exchange$allow.less.than.otherwise)[,,,,idu.states]
        
        components = set.foreground.rates(components, 'needle.exchange',
                                          rates = rates,
                                          years = intervention$processed$needle.exchange$times,
                                          start.years = start.times,
                                          end.years = end.times,
                                          apply.functions = apply.functions,
                                          allow.foreground.less = allow.less,
                                          overwrite.previous = overwrite.prior.intervention,
                                          foreground.min = min.rates,
                                          foreground.max = max.rates)
    }
    
    # IDU Transitions
    for (idu.trans in c('idu.incidence','idu.remission','idu.relapse'))
    {
        state.for.trans = c(idu.incidence='never_idu',
                            idu.remission='active_IDU',
                            idu.relapse='IDU_in_remission'
                            )[idu.trans]
        if (!is.null(intervention$processed[[idu.trans]]))
        {
            rates = lapply(intervention$processed[[idu.trans]]$rates, function(r){
                expand.population.to.general(jheem=components$jheem, r)[,,,,state.for.trans]
            })
            
            start.times = expand.population.to.general(components$jheem, intervention$processed[[idu.trans]]$start.times)[,,,,state.for.trans]
            end.times = expand.population.to.general(components$jheem, intervention$processed[[idu.trans]]$end.times)[,,,,state.for.trans]
            min.rates = expand.population.to.general(components$jheem, intervention$processed[[idu.trans]]$min.rates)[,,,,state.for.trans]
            max.rates = expand.population.to.general(components$jheem, intervention$processed[[idu.trans]]$max.rates)[,,,,state.for.trans]
            apply.functions = expand.character.array(expand.population.to.general,
                                                     components$jheem,
                                                     intervention$processed[[idu.trans]]$apply.functions)[,,,,state.for.trans]
            allow.less = expand.population.to.general(components$jheem, 
                                                                 intervention$processed[[idu.trans]]$allow.less.than.otherwise)[,,,,state.for.trans]

            components = set.foreground.rates(components, idu.trans,
                                              rates = rates,
                                              years = intervention$processed[[idu.trans]]$times,
                                              start.years = start.times,
                                              end.years = end.times,
                                              apply.functions = apply.functions,
                                              allow.foreground.less = allow.less,
                                              overwrite.previous = overwrite.prior.intervention,
                                              foreground.min = min.rates,
                                              foreground.max = max.rates)
        }
    }
    
    # Sexual Transmission
    if (!is.null(intervention$processed$heterosexual.transmission) ||
        !is.null(intervention$processed$msm.transmission))
    {
        if (!is.null(intervention$processed$heterosexual.transmission) &&
            any(intervention$processed$heterosexual.transmission$apply.functions=='absolute', na.rm=T))
            stop("'absolute' apply.function cannot be used for heterosexual.transmission")
        
        if (!is.null(intervention$processed$msm.transmission) &&
            any(intervention$processed$msm.transmission$apply.functions=='absolute', na.rm=T))
            stop("'absolute' apply.function cannot be used for msm.transmission")
        
        merged.times = union(intervention$processed$heterosexual.transmission$times,
                             intervention$processed$msm.transmission$times)
        rates = lapply(merged.times, function(time){
            r.het = r.msm = NULL
            if (!is.null(intervention$processed$heterosexual.transmission) && 
                any(intervention$processed$heterosexual.transmission$times==time))
            {
                i = (1:length(intervention$processed$heterosexual.transmission$times))[intervention$processed$heterosexual.transmission$times==time][1]
                r.het = intervention$processed$heterosexual.transmission$rates[[i]]
            }
            
            if (!is.null(intervention$processed$msm.transmission) && 
                any(intervention$processed$msm.transmission$times==time))
            {
                i = (1:length(intervention$processed$msm.transmission$times))[intervention$processed$msm.transmission$times==time][1]
                r.msm = intervention$processed$msm.transmission$rates[[i]]
            }
            
            merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                           heterosexual.arr = r.het,
                                                           msm.arr = r.msm,
                                                           default.value = NaN)
        })
        
        start.times = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                     heterosexual.arr = intervention$processed$heterosexual.transmission$start.times,
                                                                     msm.arr = intervention$processed$msm.transmission$start.times,
                                                                     default.value = Inf)
        end.times = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                   heterosexual.arr = intervention$processed$heterosexual.transmission$end.times,
                                                                   msm.arr = intervention$processed$msm.transmission$end.times,
                                                                   default.value = Inf)
        min.rates = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                   heterosexual.arr = intervention$processed$heterosexual.transmission$min.rates,
                                                                   msm.arr = intervention$processed$msm.transmission$min.rates,
                                                                   default.value = -Inf)
        max.rates = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                   heterosexual.arr = intervention$processed$heterosexual.transmission$max.rates,
                                                                   msm.arr = intervention$processed$msm.transmission$max.rates,
                                                                   default.value = Inf)
        character.na = character(); character.na[1] = NA
        apply.functions = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                         heterosexual.arr = intervention$processed$heterosexual.transmission$apply.functions,
                                                                         msm.arr = intervention$processed$msm.transmission$apply.functions,
                                                                         default.value = character.na)
        allow.less = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                    heterosexual.arr = intervention$processed$heterosexual.transmission$allow.less.than.otherwise,
                                                                    msm.arr = intervention$processed$msm.transmission$allow.less.than.otherwise,
                                                                    default.value = F)

        components = set.foreground.rates(components, 'sexual.transmission',
                                          rates = rates,
                                          years = merged.times,
                                          start.years = start.times,
                                          end.years = end.times,
                                          apply.functions = apply.functions,
                                          allow.foreground.less = allow.less,
                                          overwrite.previous = overwrite.prior.intervention,
                                          foreground.min = min.rates,
                                          foreground.max = max.rates)
    }
    
    # IDU Transmission
    if (!is.null(intervention$processed$idu.transmission))
    {
        if (any(intervention$processed$idu.transmission$apply.functions=='absolute', na.rm=T))
            stop("'absolute' apply.function cannot be used for idu.transmission")
        
        rates = lapply(intervention$processed$idu.transmission$rates, function(r){
            expand.array.to.contact(r[,,,idu.states])
        })
        
        start.times = expand.array.to.contact(intervention$processed$idu.transmission$start.times[,,,idu.states])
        end.times = expand.array.to.contact(intervention$processed$idu.transmission$end.times[,,,idu.states])
        min.rates = expand.array.to.contact(intervention$processed$idu.transmission$min.rates[,,,idu.states])
        max.rates = expand.array.to.contact(intervention$processed$idu.transmission$max.rates[,,,idu.states])
        apply.functions = expand.array.to.contact(intervention$processed$idu.transmission$apply.functions[,,,idu.states])
        allow.less = expand.array.to.contact(intervention$processed$idu.transmission$allow.less.than.otherwise[,,,idu.states])

        components = set.foreground.rates(components, 'idu.transmission',
                                          rates = rates,
                                          years = intervention$processed$idu.transmission$times,
                                          start.years = start.times,
                                          end.years = end.times,
                                          apply.functions = apply.functions,
                                          allow.foreground.less = allow.less,
                                          overwrite.previous = overwrite.prior.intervention,
                                          foreground.min = min.rates,
                                          foreground.max = max.rates)
    }
    
    
    components
}

##------------##
##-- NAMING --##
##------------##



get.intervention.filename <- function(int)
{
    name = get.intervention.name(int)
    gsub(' ', '_', name)
}

##-------------##
##-- HELPERS --##
##-------------##

expand.character.array <- function(expand.fn, jheem, arr)
{
    char.values = unique(as.character(arr))
    value.indices = 1:length(char.values)
    names(value.indices) = char.values
    
    int.arr = value.indices[arr]
    dim(int.arr) = dim(arr)
    dimnames(int.arr) = dimnames(arr)
    
    int.expanded = expand.fn(jheem, int.arr)
    
    rv = char.values[int.expanded]
    dim(rv) = dim(int.expanded)
    dimnames(rv) = dimnames(int.expanded)
    rv
}

merge.heterosexual.and.msm.transmission.arrays <- function(jheem, heterosexual.arr, msm.arr,
                                                           default.value=NaN)
{
    rv = get.contact.array.skeleton(jheem, value=default.value,
                                    age=T, race=T, sex=T, risk=T)
    
    if (!is.null(heterosexual.arr))
    {
        heterosexual.arr = expand.array.to.contact(heterosexual.arr)
        
        male = c('heterosexual_male','msm')
        rv[,,'female',,,,male,] = heterosexual.arr[,,'female',,,,male,]
        rv[,,male,,,,'female',] = heterosexual.arr[,,male,,,,'female',]
    }
    
    if (!is.null(msm.arr))
    {
        msm.arr = expand.array.to.contact(msm.arr)
        rv[,,'msm',,,,'msm',] = msm.arr[,,'msm',,,,'msm',]
    }
    
    rv
}

expand.array.to.contact <- function(arr)
{
    from.dimnames = to.dimnames = dimnames(arr)
    names(from.dimnames) = paste0(names(from.dimnames), '.from')
    names(to.dimnames) = paste0(names(to.dimnames), '.to')
    
    dim(arr) = sapply(to.dimnames, length)
    dimnames(arr) = to.dimnames
    
    target.dimnames = c(from.dimnames, to.dimnames)
    
    was.logical = is(arr[1], 'logical')
    was.character = is(arr[1], 'character')
    
    if (was.logical)
        arr[] = as.numeric(arr)
    
    if (was.character)
    {
        index.to.char = unique(arr)
        char.to.index = (1:length(index.to.char))
        names(char.to.index) = index.to.char
            
        arr = as.integer(char.to.index[arr])
        dim(arr) = sapply(from.dimnames, length)
        dimnames(arr) = from.dimnames
    }
    
    rv = expand.population(source.population = arr,
                      target.dim.names = target.dimnames)
    
    if (was.logical)
        rv[] = as.logical(rv)
    if (was.character)
    {
        rv = as.character(index.to.char[rv])
        dim(rv) = sapply(target.dimnames, length)
        dimnames(rv) = target.dimnames
    }
        
    rv
}