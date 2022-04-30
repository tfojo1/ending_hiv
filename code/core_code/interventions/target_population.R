

##-------------------------------------##
##-- MAPPINGS FOR TARGET POPULATIONS --##
##-  This is the 'exported' interface -##
##-------------------------------------##

target.population.hash <- function(tpop)
{
    logical.to.6.bit(tpop)
}

target.population.from.hash <- function(hash, manager=TARGET.POPULATION.MANAGER.1.0)
{
    manager$population[hash]
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

##------------------##
##-- DEFAULT NAME --##
##------------------##

#Only works for tpops that have uniform marginals across age, race, and sex+risk
default.target.population.name <- function(tpop,
                                           ages = SETTINGS$AGES,
                                           years.old.text = 'years old',
                                           lt.text = "<",
                                           gte.pre.text = "\U2265",
                                           gte.post.text = "",#+",
                                           interval.text = '-',
                                           
                                           race.names = c(black='Black', hispanic='Hispanic', other='Other'),
                                           
                                           idu.combinator=" + ",
                                           
                                           everyone.text='Everyone',
                                           all.prefix = 'All',
                                           people.text = 'Individuals')
{
    age.mask = apply(tpop, 'age', any)
    age.text = make.default.tpop.age.text(age.mask=age.mask,
                                          ages=ages,
                                          years.old.text = years.old.text,
                                          lt.text = lt.text,
                                          gte.pre.text = gte.pre.text,
                                          gte.post.text = gte.post.text,
                                          interval.text = interval.text)
    
    race.mask = apply(tpop, 'race', any)
    race.text = make.default.tpop.race.text(race.mask = race.mask[names(race.names)],
                                            race.names = race.names)
    
    mask.mat = apply(tpop, c('sex','risk'), any)
    sex.risk.text = make.default.tpop.sex.risk.text(mask.mat = mask.mat,
                                                    
                                                    idu.combinator=idu.combinator)
    
    
    # Put it together
    if (age.text=='All')
    {
        if (race.text=='All')
        {
            if (sex.risk.text=='All')
                everyone.text
            else
                paste0(all.prefix, " ", sex.risk.text)
        }
        else
        {   
            if (sex.risk.text=='All')
                paste0(race.text, " ", people.text)
            else
                paste0(race.text, " ", sex.risk.text)
        }
    }
    else
    {
        if (race.text=='All')
        {
            if (sex.risk.text=='All')
                paste0(people.text, " ", age.text)
            else
                paste0(sex.risk.text, " ", age.text)
        }
        else
        {   
            if (sex.risk.text=='All')
                paste0(race.text, " ", people.text, " ", age.text)
            else
                paste0(race.text, " ", sex.risk.text, " ", age.text)
        }
    }
}


#testing default age text
if (1==2)
{
    tpop = create.target.population()
    default.target.population.name(tpop)
    
    tpop = create.target.population(ages=2:4)
    default.target.population.name(tpop)
    
    tpop = create.target.population(ages=1:4)
    default.target.population.name(tpop)
    
    tpop = create.target.population(ages=2:5)
    default.target.population.name(tpop)
    
    tpop = create.target.population(ages=2:4, races='black')
    default.target.population.name(tpop)
    
    tpop = create.target.population(ages=2:4, races=c('black','other'))
    default.target.population.name(tpop)
    
    tpop = create.target.population(sexes='msm')
    default.target.population.name(tpop)
    
    tpop = union.target.populations(create.target.population(sexes='msm'),
                                    create.target.population(risks='active_IDU'))
    default.target.population.name(tpop)
    
    
    tpop = union.target.populations(create.target.population(sexes='msm', risks='never_IDU'),
                                    create.target.population(risks='active_IDU'))
    default.target.population.name(tpop)
    
    tpop = union.target.populations(create.target.population(sexes='msm'),
                                    create.target.population(risks=c('active_IDU','prior_IDU')))
    default.target.population.name(tpop)
    
    tpop = create.target.population(sexes=c('female','heterosexual_male'))
    default.target.population.name(tpop)
    
}

make.default.tpop.age.text <- function(age.mask,
                                       ages=SETTINGS$AGES,
                                       years.old.text = 'years',
                                       lt.text = "<",
                                       gte.pre.text = "",#\U2265",
                                       gte.post.text = "+",
                                       interval.text = '-')
{
    age.indices = (1:length(age.mask))[age.mask]
    n.ages = length(ages$lowers)
    
    if (all(age.mask))
        "All"
    else
    {
        lower.indices = upper.indices = age.indices
        if (length(age.indices)>1)
        {
            can.merge.down = c(F, age.indices[-1]==(age.indices[-length(age.indices)]+1))
            can.merge.up= c(can.merge.down[-1], F)
            for (i in (1:length(age.indices))[can.merge.down])
                lower.indices[i] = lower.indices[i-1]
            
            lower.indices = lower.indices[!can.merge.up]
            upper.indices = upper.indices[!can.merge.up]
        }
        
        components = sapply(1:length(lower.indices), function(i){
            if (ages$lowers[lower.indices[i]]==0)
                paste0(lt.text, ages$uppers[upper.indices[i]], " ", years.old.text)
            else if (ages$uppers[upper.indices[i]]==Inf)
                paste0(gte.pre.text, ages$lowers[lower.indices[i]], gte.post.text, " ", years.old.text)
            else
                paste0(ages$lowers[lower.indices[i]], interval.text, ages$uppers[upper.indices[i]], " ", years.old.text)
        })
        
        if (length(components)==1)
            components
        else if (length(components)==2)
            paste0(components[1], " and ", components[2])
        else
            paste0(paste0(components[-length(components)], collapse=', '),
                   ", and ", components[length(components)])
    }
}

make.default.tpop.race.text <- function(race.mask,
                                        race.names=c("Black", "Hispanic", "Other"))
{
    races = race.names[race.mask]
    
    if (length(races)==length(race.names))
        'All'
    else if (length(races)==1)
        races
    else if (length(races)==2)
        paste0(races[1], " and ", races[2])
    else
        paste0(paste0(races[1:(length(races)-1)], collapse=', '),
               ", and ", races[length(races)])
}

#indexed [sex,risk]
# where sex is one of 'msm', 'heterosexual_male', 'female'
# and risk is one of 'active_IDU', 'IDU_in_remission', 'never_IDU'
make.default.tpop.sex.risk.text <- function(mask.mat,
                                            all.name='All',
                                            active.idu.prefix='active',
                                            prior.idu.prefix='prior',
                                            idu.name='IDU',
                                            msm.name='MSM',
                                            heterosexual.name='heterosexual',
                                            heterosexual.name.plural='heterosexuals',
                                            female.name='female',
                                            female.name.plural='females',
                                            male.name='male',
                                            male.names='males',
                                            hyphenator='-',
                                            idu.combinator="+")
{
    # Check for all
    if (all(mask.mat))
        return (all.name)
    
    # Check for male or female
    if (all(rowSums(mask.mat)==3 | rowSums(mask.mat)==0)) #for each sex, either all risk or none
    {
        if (mask.mat['female',1] && !mask.mat['msm',1] && !mask.mat['heterosexual_male',1])
            return (female.name.plural)
        
        if (!mask.mat['female',1] && mask.mat['msm',1] && mask.mat['heterosexual_male',1])
            return (male.name.plural)
    }
    
    components = character()
    
    # Check MSM
    if (mask.mat['msm','never_IDU'])
        components = c(components, msm.name)
    
    # Check IDU
    if (all(mask.mat[,'active_IDU']) && all(mask.mat[,'IDU_in_remission'])) 
    {
        components = c(components,
                       paste0(active.idu.prefix, hyphenator, " and ", prior.idu.prefix, hyphenator, idu.name))
    }
    else if (all(mask.mat[,'active_IDU']) && !any(mask.mat[,'IDU_in_remission']))
    {
        components = c(components,
                       paste0(active.idu.prefix, hyphenator, idu.name))
    }
    else if (!any(mask.mat[,'active_IDU']) && all(mask.mat[,'IDU_in_remission']))
    {
        components = c(components,
                       paste0(prior.idu.prefix, hyphenator, idu.name))
    }
    else #do it piecemeal for MSM and heterosexuals
    {
        # Check just male IDU
        male = c('msm', 'heterosexual_male')
        if (all(mask.mat[male, 'active_IDU']) && !mask.mat['female', 'active_IDU'] &&
            all(mask.mat[male, 'IDU_in_remission'] && !mask.mat['female', 'IDU_in_remission']))
        {
            components = c(components,
                           paste0(male.name, " ", active.idu.prefix, hyphenator, " and ", prior.idu.prefix, hyphenator, idu.name))
        }
        else if (all(mask.mat[male, 'active_IDU']) && !mask.mat['female', 'active_IDU'] &&
                 !any(mask.mat[,'IDU_in_remission']))
        {
            components = c(components,
                           paste0(male.name, " ", active.idu.prefix, hyphenator, idu.name))
        }
        else if (all(mask.mat[male, 'IDU_in_remission']) && !mask.mat['female', 'IDU_in_remission'] &&
                 !any(mask.mat[,'active_IDU']))
        {
            components = c(components,
                           paste0(male.name, " ", prior.idu.prefix, hyphenator, idu.name))
        }
        else
        {
            # Check MSM-IDU
            if (mask.mat['msm','active_IDU'])
            {
                if (mask.mat['msm', 'IDU_in_remission'])
                    components = c(components,
                                   paste0(msm.name, idu.combinator, active.idu.prefix, hyphenator, " and ", prior.idu.prefix, hyphenator, idu.name))
                else
                    components = c(components,
                                   paste0(msm.name, idu.combinator, active.idu.prefix, hyphenator, idu.name))
            }
            else if (mask.mat['msm', 'IDU_in_remission'])
                components = c(components,
                               paste0(msm.name, idu.combinator, prior.idu.prefix, hyphenator, idu.name))
        
            # Check heterosexual IDU
            if (mask.mat['female','active_IDU'] && mask.mat['female','IDU_in_remission'] && 
                mask.mat['heterosexual_male','active_IDU'] && mask.mat['heterosexual_male','IDU_in_remission'])
                components = c(components,
                               paste0(heterosexual.name, " ", active.idu.prefix, hyphenator, " and ", prior.idu.prefix, hyphenator, idu.name))
            else if (mask.mat['female','active_IDU'] && mask.mat['female','IDU_in_remission'] && 
                !mask.mat['heterosexual_male','active_IDU'] && !mask.mat['heterosexual_male','IDU_in_remission'])
                components = c(components,
                               paste0(female.name, " ", active.idu.prefix, hyphenator, " and ", prior.idu.prefix, hyphenator, idu.name))
            else if (!mask.mat['female','active_IDU'] && !mask.mat['female','IDU_in_remission'] && 
                mask.mat['heterosexual_male','active_IDU'] && mask.mat['heterosexual_male','IDU_in_remission'])
                components = c(components,
                               paste0(heterosexual.name, " ", male.name, " ", active.idu.prefix, hyphenator, " and ", prior.idu.prefix, hyphenator, idu.name))
            else
            {
                if (mask.mat['female','active_IDU'])
                {
                    if (mask.mat['heterosexual_male','active_IDU'])
                        components = c(components,
                                       paste0(heterosexual.name, " ", active.idu.prefix, hyphenator, idu.name))
                    else
                        components = c(components,
                                       paste0(female.name, " ", active.idu.prefix, hyphenator, idu.name))
                }
                else if (mask.mat['heterosexual_male','active_IDU'])
                    components = c(components,
                                   paste0(heterosexual.name, " ", male.name, active.idu.prefix, hyphenator, idu.name))
                
                
                if (mask.mat['female','IDU_in_remission'])
                {
                    if (mask.mat['heterosexual_male','IDU_in_remission'])
                        components = c(components,
                                       paste0(heterosexual.name, " ", prior.idu.prefix, hyphenator, idu.name))
                    else
                        components = c(components,
                                       paste0(female.name, " ", prior.idu.prefix, hyphenator, idu.name))
                }
                else if (mask.mat['heterosexual_male','IDU_in_remission'])
                    components = c(components,
                                   paste0(heterosexual.name, " ", male.name, prior.idu.prefix, hyphenator, idu.name))
            }
        }
    }
    
    # Check heterosexual
    if (mask.mat['female','never_IDU'])
    {
        if (mask.mat['heterosexual_male', 'never_IDU'])
            components = c(components, heterosexual.name.plural)
        else
            components = c(components,
                           paste0(heterosexual.name, " ", female.name.plural))
    }
    else if (mask.mat['heterosexual_male', 'never_IDU'])
        components = c(components,
                       paste0(heterosexual.name, " ", male.name.plural))
    
    
    if (length(components)==1)
        components
    else if (length(components)==2)
        paste0(components[1], " and ", components[2])
    else
        paste0(paste0(components[-length(components)], collapse=', '),
               ", and ", components[length(components)])
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

