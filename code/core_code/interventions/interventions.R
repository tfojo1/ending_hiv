

# An intervention consists of a combination of
# 1) Target populations to wh

##-----------------------##
##-- CLASS DEFINITIONS --##
##-----------------------##

setClass('intervention',
         representation=list(
             type='character'))

setClass('standard_intervention',
         contains='intervention',
         representation=list(
             raw='list',
             processed='list',
             static.settings='list',
             parameter.distributions='list'
        ))

setClass('null_intervention',
         contains='intervention')

setGeneric('prepare.intervention.for.simset',
           def=function(intervention, simset){standardGeneric('prepare.intervention.for.simset')})
setMethod('prepare.intervention.for.simset',
          signature=c('intervention'),
def = function(intervention, simset){
    intervention
})

##------------------##
##-- CONSTRUCTORS --##
##------------------##

#An intervention object is a list with two components, $raw and $processed
# Each of these is a list with one or more elements corresponding to types of unit interventions (testing, prep, suppression, etc)
# Each of these lists contains two lists: $target.populations and $unit.interventions

#'@param ... Must be either (a) target.population, (b) unit.intervention objects, (c) distribution objects or lists containing only target.population, unit.intervention objects, and distribution objects
#'@details Creates an intervention object that applies all the unit interventions contained in ... to all the target populations. Distributions are stored against resolving variables in the intervention units in the future
#'
#applies the unit interventions to all target populations
create.intervention <- function(...)
{
    # Parse arguments
    args = flatten.list(...)
    
    target.populations = args[sapply(args, is, 'target.population')]
    intervention.units = args[sapply(args, is, 'intervention.unit')]
    static.settings = args[sapply(args, is, 'static.settings')]
    distributions = args[sapply(args, is, 'Distribution')]
    
    # Check  types
    if ((length(target.populations) + length(intervention.units) + length(distributions) + length(static.settings)) < length(args))
        stop("The arguments to create.intervention must be either target.population, unit.intervention objects, static.settings objects, Distribution objects, or lists containing only target.population, unit.intervention objects, and Distribution objects")
    if (length(static.settings)==0 && length(intervention.units)==0)
        stop("An intervention must contain at least one intervention.unit or static.settings")
    if (length(target.populations)==0 && (length(static.settings) + length(distributions))<length(args))
        stop("Unless an intervention consists of only static settings, there must be at least one target population passed as an argument to create.intervention")
    if (length(intervention.units)==0)
        stop("Unless an intervention consists of only static settings there must be at least one intervention unit passed as an argument to create.intervention")
    for (dist in distributions)
    {
        if (is.null(dist@var.names))
            stop("Distributions used to construct an intervention must have named variables")
    }
    
    # Put it into the structure
    unit.types = sapply(intervention.units, function(unit){unit$type})
    raw = list()
    
    for (unit in intervention.units)
    {
        if (is.null(raw[[unit$type]]))
            raw[[unit$type]] = list(target.populations=list(),
                                       intervention.units=list())
        
        for (target.population in target.populations)
        {
            raw[[unit$type]]$target.populations = c(raw[[unit$type]]$target.populations, list(target.population))
            raw[[unit$type]]$intervention.units = c(raw[[unit$type]]$intervention.units, list(unit))
        }
    }
    
    rv = new('standard_intervention',
             type='standard',
            raw=raw,
            processed=list(),
            static.settings=static.settings,
            parameter.distributions=distributions)
    
    process.intervention(rv)
}

join.interventions <- function(...)
{
    interventions.to.join = flatten.list(...)
    if (length(interventions.to.join)==0)
        stop("Must pass at least one intervention to join")
    
    if (any(!sapply(interventions.to.join, is, 'intervention')))
        stop("The arguments to join interventions must be only intervention objects or lists containing only intervention objects")
    
    int.type = interventions.to.join[[1]]@type
    if (length(interventions.to.join)>1)
    {
        for (int in interventions.to.join[-1])
        {
            if (int@type != int.type)
                stop("All interventions to join must be the same type")
        }
    }
    
    rv = interventions.to.join[[1]]
    if (length(interventions.to.join)>1)
    {
        for (int in interventions.to.join[-1])
        {
            rv = do.join.interventions(rv, int)
        }
    }
    
    rv
}

setGeneric('do.join.interventions',
           def=function(int1,int2){standardGeneric('do.join.interventions')})
setMethod('do.join.interventions',
          signature = 'intervention',
          def = function(int1, int2){
              stop(paste0("Joining interventions has not been defined for interventions of class '", class(int1), "'"))
          })
setMethod('do.join.interventions',
          signature = 'standard_intervention',
def = function(int1, int2)
{
    if (!is(int2, 'standard_intervention'))
        stop("Both interventions must be of class 'standard_intervention'")
    
    int1@processed = list()
    
    for (type in names(int1@raw))
    {
        if (is.null(int1@raw[[type]]))
            int1@raw[[type]] = list(target.populations=list(),
                                  intervention.units=list())
        
        sub = int2@raw[[type]]
        int1@raw[[type]]$target.populations = c(int1@raw[[type]]$target.populations, int2@raw[[type]]$target.populations)
        int1@raw[[type]]$intervention.units = c(int1@raw[[type]]$intervention.units, int2@raw[[type]]$intervention.units)
    }
    
    int1@static.settings = c(int1@static.settings,
                             int2@static.settings)
    int1@parameter.distributions = c(int1@parameter.distributions,
                                     int2@parameter.distributions)
    
    
    intervention.consistency.check(int1)
    process.intervention(int1, overwrite.previous.processed = T)
})

setMethod('do.join.interventions',
          signature = 'null_intervention',
def = function(int1, int2){
    stop("Cannot join interventions to a null intervention")
})


create.null.intervention <- function()
{   
    new('null_intervention', type='null')
}

intervention.consistency.check <- function(intervention, allow.multiple.interventions=T)
{
    sapply(names(intervention@raw), function(type){
        bucket = intervention@raw[[type]]
        pop.hashes = sapply(bucket$target.populations, logical.to.6.bit)
        if (!allow.multiple.interventions && max(table(pop.hashes))>1)
            stop("An intervention can only have one sub-intervention per population, but the intervention has more than one sub-intervention of type ", type,
                 " for certain sub-populatfions")
    })
}

setGeneric('interventions.equal',
           def=function(int1,int2){standardGeneric('interventions.equal')})
setMethod('interventions.equal',
          signature = 'null_intervention',
def = function(int1, int2)
{
    is(int2, 'null_intervention')
})
setMethod('interventions.equal',
          signature = 'standard_intervention',
def = function(int1, int2)
{
    if (!is(int2, 'standard_intervention'))
        return (F)
    
    # Test that overall lengths match
    if (length(int1@raw) != length(int2@raw) ||
        length(int1@static.settings) != length(int2@static.settings) || 
        length(int1@parameter.distributions) != length(int2@parameter.distributions))
        return (F)
    
    if (length(int1@raw)>0 && length(setdiff(names(int1@raw), names(int2@raw)))>0)
        return (F)

    # Test static settings for equality    
    if (length(int1@static.settings)>0)
    {
        static.settings.equality.matrix = sapply(1:length(int1@static.settings), function(i){
            sapply(1:length(int2@static.settings), function(j){
                static.settings.equal(int1@static.settings[[i]], int2@static.settings[[j]])
            })
        })
        dim(static.settings.equality.matrix) = rep(length(int1@static.settings),2)
        if (any(rowSums(static.settings.equality.matrix)!=1) ||
            any(colSums(static.settings.equality.matrix)!=1))
            return (F)
    }
    
    # Test Distributions for equality
    
    # Test intervention units/target populations for equality
    if (length(int1@raw)>0)
    {
        if (any(!sapply(names(int1@raw), function(type){
                t1 = int1@raw[[type]]
                t2 = int2@raw[[type]]
                
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
    }
    
    # Return true if we haven't flagged anything up to now
    return (T)
})

is.null.intervention <- function(int)
{
    is(int, 'null_intervention')
    #length(int@raw) == 0
}

setGeneric('get.intervention.unit.types', 
           def=function(int){standardGeneric('get.intervention.unit.types')})
setMethod('get.intervention.unit.types',
          signature='standard_intervention',
def = function(int)
{
    names(int@raw)
})

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
    for (type in names(int@raw))
    {
        if (is.null(rv))
            rv = ''
        else
            rv = paste0(rv, delimiter)
        
        #rv = paste0(rv, pre.header, INTERVENTION.UNIT.TYPE.PRETTY.NAMES[type], ":", post.header)
        category = int@raw[[type]]$intervention.units[[1]]$name.metadata$category
        rv = paste0(rv, pre.header, category,":", post.header)
        
        subset = int@raw[[type]]
        
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
    types = names(int@raw)
    rv = sapply(types, function(type){
        subset = int@raw[[type]]
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
        int@raw[[type]]$intervention.units[[1]]$name.metadata$category
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
        for (sub in int@raw)
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
        int.names = unique(manager$name[mask])
        
        if (length(int.names)>1 && throw.error.if.multiple.names)
            stop(paste0("More than one name for the given intervention has been registered: ",
                        paste0("'", manager$name[mask], "'", collapse=", ")))
        as.character(int.names)
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

intervention.from.code <- function(code, manager=INTERVENTION.MANAGER.1.0,
                                   throw.error.if.not.found=T)
{
    mask = manager$code==code
    if (any(mask))
        manager$intervention[[ (1:length(manager$intervention))[mask] ]]
    else if (throw.error.if.not.found)
        stop(paste0("No intervention with code '", code, "' has been registered"))
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
    if (length(int@raw)==0)
        list()
    else
    {
        tpops = int@raw[[1]]$target.populations
        if (length(int@raw)>1)
        {
            for (i in 2:length(int@raw))
            {
                tpops2 = int@raw[[i]]$target.populations
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

intervention.is.processed <- function(intervention)
{
    setequal(names(intervention@raw), names(intervention@processed))
}

process.intervention <- function(intervention,
                                 to.process = names(intervention@raw),
                                 overwrite.previous.processed=F)
{
    if (is.null.intervention(intervention))# || intervention.is.resolved(intervention))
        intervention@processed = list()
    else
    {
        all.resolved.by.type = sapply(to.process, function(type){
            sub.units = intervention@raw[[type]]$intervention.units
            all(sapply(sub.units, intervention.unit.is.resolved))
        })
        
        if (overwrite.previous.processed)
            intervention@processed = list()
        to.process = to.process[all.resolved.by.type]
        to.process = setdiff(to.process, names(intervention@processed))
        
        intervention@processed = c(intervention@processed,
                                   lapply(to.process, function(type){
            sub = intervention@raw[[type]]
            
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
        }))
        
        names(intervention@processed) = to.process
    }

    intervention
}


##-----------------------------##
##-- RESOLVING INTERVENTIONS --##
##-----------------------------##

resolve.intervention <- function(intervention, parameters)
{
    for (type in names(intervention@raw))
    {
        for (i in 1:length(intervention@raw[[type]]$intervention.units))
        {
            if (!intervention.unit.is.resolved(intervention@raw[[type]]$intervention.units[[i]]))
                intervention@raw[[type]]$intervention.units[[i]] = resolve.intervention.unit(
                    unit=intervention@raw[[type]]$intervention.units[[i]],
                    parameters=parameters
                )
        }
    }
    
    intervention@static.settings = lapply(intervention@static.settings, function(ss){
        resolve.intervention.static.settings(static.settings=ss,
                                parameters=parameters)
    })
    
    intervention = process.intervention(intervention, overwrite.previous.processed = T)
    intervention
}


setGeneric('intervention.is.resolved',
           def=function(intervention){standardGeneric('intervention.is.resolved')})
setMethod('intervention.is.resolved',
          signature='standard_intervention',
def=function(intervention)
{
    all.resolved.by.type = sapply(names(intervention@raw), function(type){
        sub.units = intervention@raw[[type]]$intervention.units
        all(sapply(sub.units, intervention.unit.is.resolved))
    })
    
    static.settings.resolved = sapply(intervention@static.settings, static.settings.is.resolved)
    
    all(all.resolved.by.type) && all(static.settings.resolved)
})

setMethod('intervention.is.resolved',
          signature='null_intervention',
def=function(intervention)
{
    T  
})

get.intervention.unresolved.var.names <- function(intervention)
{
    unresolved.from.units = unlist(sapply(intervention@raw, function(sub){
        unlist(sapply(sub$intervention.units, get.intervention.unit.unresolved.var.names))
    }))

    unresolved.from.static.settings = unlist(sapply(intervention@static.settings, function(ss){
        ss$to.resolve
    }))
    
    unique(c(unresolved.from.units, unresolved.from.static.settings))
}
              
##----------------------------------##
##-- SUMMARIZE TARGET POPULATIONS --##
##----------------------------------##

get.unique.target.populations <- function(int)
{
    all.target.populations = list()
    for (subset in int@raw)
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
