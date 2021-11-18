
##-- JHEEM COMPONENTS --##

## A JHEEM components object contains all the elements needed to set up a JHEEM Object
## Since parameters cannot be REMOVED from a JHEEM object once it is set up, we use
##  a components object to hold all the parameters, or variables which are used to 
##  create parameters, which we may change some or all of.
## Then at the last minute, we crunch all the value sin components and
##  create a JHEEM object from it

## There are, in general, two kinds of things we set in a jheem.components object:
## 1) Parameters - these are specific values that are set (eg a transmission rate),
##    and generally don't require data pulls from data managers
## 2) Data - these are more complex data structures, like underlying populations or 
##    population-specific rates, that typically require a data pull from a data manager,
##    and are generally not set by directly passing a value. We generally expect that these
##    will be set up once for components; although they can be re-set, we gear our
##    coding efficiency on the assumption that they will only be set up once
## 
## Also, there is a third sub-category in the jheem.components object   
## 3) Elements - these are the parameters that will go directly into the JHEEM, 
##    calculated from parameters and data

#Initializes a jheem.components object
create.jheem.components <- function()
{
    rv = list(data=list(),
              parameters=list(),
              elements=list())   
}

##-- SET UP PARAMETERS --##

#Sets a parameter for a jheem.components object
set.components.parameter <- function(components,
                                      param.name,
                                      value)
{
    components$parameters[[param.name]] = value
    components = clear.components.dependencies(param.name)
    
    components
}


##-----------------##
##-- SET UP DATA --##
##-----------------##

#If use.birth.rates is TRUE, sets fertility rates as birth rates (ie, per total population)
# Otherwise, sets age-specific fertility rates for females
setup.fertility <- function(components,
                            data.managers,
                            use.birth.rates=T)
{
    if (is.null(components$fips))
        stop("components has not been initialized as JHEEM components")
    
    components$use.birth.rates = use.birth.rates
    
    if (use.birth.rates)
    {
        birth.rates = get.birth.rates(data.managers$fertility,
                                      fips=components$fips,
                                      census=data.managers$census.collapsed,
                                      aggregate.counties = T)
        birth.rates = array(birth.rates, dim=c(race=length(birth.rates)), dimnames=list(race=names(birth.rates)))
        pop.by.race = apply(components$populations$all.races, 'race', sum)
        pop.by.race = array(pop.by.race, dim=c(race=length(pop.by.race)), dimnames=list(race=names(pop.by.race)))
        birth.rates = collapse.races.for.rates(pop.by.race, rates=birth.rates)
        
        components$fertility = birth.rates
    }
    else
    {
        stop("Fertility rates (as opposed to birth rates) is not implemented at this time")
    }
    
    components = clear.dependent.values(components, 'fertility')
    
    components
}
