

##-------------------------------------##
##-- MAPPINGS FOR TARGET POPULATIONS --##
##-  This is the 'exported' interface -##
##-------------------------------------##

target.population.hash <- function(tpop)
{
    logical.to.6.bit(tpop)
}

target.population.to.code <- function(tpop, manager=TARGET.POPULATION.MANAGER.1.0)
{
    hash = logical.to.6.bit(tpop)
    manager$code[hash]
}

target.population.from.code <- function(code, manager=TARGET.POPULATION.MANAGER.1.0)
{
    mask = manager$code==code
    if (any(mask))
        manager$population[[ (1:length(manager$population))[mask] ]]
    else
        NULL
}

target.population.code.to.name <- function(code, manager=TARGET.POPULATION.MANAGER.1.0)
{
    mask = manager$code==code
    manager$name[mask]
}

target.population.name.to.code <- function(name, manager=TARGET.POPULATION.MANAGER.1.0)
{
    mask = manager$name==name
    if (any(mask))
        manager$code[[ (1:length(manager$code))[mask] ]]
    else
        NULL
}

target.population.name <- function(tpop, manager=TARGET.POPULATION.MANAGER.1.0)
{
    hash = logical.to.6.bit(tpop)
    manager$name[hash]
}

target.populations.equal <- function(tpop1, tpop2)
{
    logical.to.6.bit(tpop1) == logical.to.6.bit(tpop2)
}

#returns an indexing such that, if applied to the list of interventions,
#interventions appear in the order they do in manager
order.target.populations <- function(target.populations,
                                manager=TARGET.POPULATION.MANAGER.1.0,
                                decreasing=F)
{
    codes = sapply(target.populations, logical.to.6.bit)
    all.values = 1:length(manager$hash)
    names(all.values) = manager$hash
    
    values = all.values[codes]
    order(values, decreasing = decreasing)
}


##---------------------------------------##
##-- ADDING DEFINED TARGET POPULATIONS --##
##---------------------------------------##

create.target.population.manager <- function()
{
    list(population=list(),
         code=character(),
         name=character(),
         hash=character())
}

DISALLOWED.TPOP.CODE.CHARACTERS = c("_")

add.target.population <- function(tpop, 
                                  code,
                                  name,
                                  manager=TARGET.POPULATION.MANAGER.1.0)
{
    if (!is(tpop, 'target.population'))
        stop("'tpop' must be of class 'target.population'")
    hash = logical.to.6.bit(tpop)
    
    # Check if already added under a different name or code
    mask = manager$hash==hash
    if (any(mask))
    {
        if (manager$name[mask] == name)
        {
            if (manager$code[mask] == code)
                return (manager)
            stop(paste0("Attempted to add a target population (with code '",
                        code, "') which is already present with code '",
                        manager$code[mask], "'. There can only be one code per target population."))
        }
        else
            stop(paste0("Attempted to add a target population (under the name '",
                        name, "') which is already present under the name '",
                        manager$name[mask], "'. There can only be one name per target population."))
    }
    
    # Check that the code does not have any disallowed characters
    for (ch in DISALLOWED.TPOP.CODE.CHARACTERS)
        if (grepl(ch, code))
            stop(paste0("Target population codes cannot contain '", ch, "'"))
    
    # Check to confirm that name and code are unique
    if (any(manager$name==name))
        stop(paste0("A different target population is already present under the name, '",
                    name, "'. Target population names must be unique"))
    if (any(manager$code==code))
        stop(paste0("A different target population is already present under the code, '",
                    code, "'. Target population codes must be unique"))

    # Add it in and return
    manager$hash = c(manager$hash, hash)
    
    manager$population = c(manager$population, list(tpop))
    names(manager$population) = manager$hash
    
    manager$name = c(manager$name, name)
    names(manager$name) = manager$hash
    
    manager$code = c(manager$code, code)
    names(manager$code) = manager$hash
    
    manager
}

CHARS.6.BIT = '1234567890<>abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
logical.to.6.bit <- function(v,
                             to.char=T)
{
    nums = 1 + sapply(1:ceiling(length(v)/6), function(i){
        subset = as.numeric(v[as.numeric(1:6 + (i-1)*6)])
        subset[is.na(subset)] = 0
        sum(c(1,2,4,8,16,32)*subset)
    })
    
    if (to.char)
        paste0(sapply(nums, function(num){substr(CHARS.6.BIT,num,num)}), collapse='')
    else
        nums
}

##----------------------------------------------##
##-- CREATING TARGET POPULATIONS FROM SCRATCH --##
##----------------------------------------------##

create.target.population <- function(settings=SETTINGS,
                                     ages=settings$AGES$labels,
                                     races=settings$RACES,
                                     sexes=settings$SEXES,
                                     risks=settings$RISK_STRATA)
{
    dim.names = list(age=settings$AGES$labels,
                     race=settings$RACES,
                     sex=settings$SEXES,
                     risk=settings$RISK_STRATA)
    
    rv = array(F, dim=sapply(dim.names, length), dimnames=dim.names)
    rv[ages, races, sexes, risks] = T
    
    class(rv) = 'target.population'
    rv
}

union.target.populations <- function(pop1, pop2)
{
    rv = pop1 | pop2    
    class(rv) = 'target.population'
    rv
}

intersect.target.populations <- function(pop1, pop2)
{
    rv = pop1 & pop2    
    class(rv) = 'target.population'
    rv
}

diff.target.populations <- function(pop1, pop2)
{
    rv = pop1 & !pop2    
    class(rv) = 'target.population'
    rv
}

