

##---------------##
##-- STRUCTURE --##
##---------------##

##----------------------##
##-- SETUP COMPONENTS --##
##----------------------##

setup.components.for.intervention <- function(components,
                                              intervention)
{
    if (is.null(intervention))
        return (components)
    
    # Testing
    if (!all(sapply(intervention$testing.frequency, function(x){all(is.na(x))})))
    {
        testing.rates = lapply(1:length(intervention$testing.years), function(y)
        {
            testing.mat = get.general.population.skeleton(components$jheem,
                                                          value=as.numeric(NA))
            for (j in 1:length(intervention$target.populations))
            {
                t.pop = intervention$target.populations[[j]]
                testing.mat[t.pop] =
                    1 / intervention$testing.frequency[[j]][y]
            }
            
            testing.mat
        })
        
        components = set.foreground.hiv.testing.rates(components,
                                                      testing.rates = testing.rates,
                                                      years = intervention$testing.years)
    }
    
    # Suppression
    if (!all(sapply(intervention$suppressed.proportion, function(x){all(is.na(x))})))
    {
        suppressed.proportions = lapply(1:length(intervention$suppressed.years), function(y)
        {
            suppressed.mat = get.hiv.positive.population.skeleton(components$jheem,
                                                                  value=as.numeric(NA))
            gen.mat = get.general.population.skeleton(components$jheem,
                                                      value=as.numeric(NA))
            for (j in 1:length(intervention$target.populations))
            {
                t.pop = intervention$target.populations[[j]]
                gen.mat[t.pop] = intervention$suppressed.proportion[[j]][y]
            }
            
            suppressed.mat[t.pop$ages, t.pop$races,, t.pop$sexes, t.pop$risks, 'diagnosed',,] = gen.mat
            suppressed.mat
        })
        
        components = set.foreground.suppression(components,
                                                suppressed.proportions = suppressed.proportions,
                                                years = intervention$suppressed.years)
    }
    
    # PrEP
    if (!all(sapply(intervention$prep.coverage, function(x){all(is.na(x))})))
    {
        prep.coverages = lapply(1:length(intervention$prep.years), function(y)
        {
            prep.mat = get.general.population.skeleton(components$jheem,
                                                       value=as.numeric(NA))
            
            for (j in 1:length(intervention$target.populations))
            {
                t.pop = intervention$target.populations[[j]]
                prep.mat[t.pop] = intervention$prep.coverage[[j]][y]
            }
            
            prep.mat
        })
        
        components = set.foreground.prep.coverage(components,
                                                  prep.coverage = prep.coverages,
                                                  years = intervention$prep.years)
    }
    
    components
}

##------------##
##-- NAMING --##
##------------##

get.intervention.name.from.components <- function(verson,
                                                  location
                                                  )
{
    
}

get.intervention.name <- function(int)
{
    if (is.null(int))
        "No Intervention"
}

get.intervention.filename <- function(int)
{
    name = get.intervention.name(int)
    gsub(' ', '_', name)
}