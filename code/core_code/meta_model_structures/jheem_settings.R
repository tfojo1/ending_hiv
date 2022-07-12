
##------------------##
##-- CONSTRUCTORS --##
##------------------##

# The base constructor
# Returns the version manager with the new version registered
create.jheem.settings <- function(version,
                                  prior.versions,
                                  directory.suffix,
                                  
                                  age.cutoffs,
                                  races,
                                  locations,
                                  subpopulations,
                                  sexes,
                                  risks,
                                  non.hiv.subsets,
                                  continuum.of.care,
                                  cd4.strata,
                                  hiv.subsets,
                                  
                                  first.diagnosed.continuum.states,
                                  diagnosed.continuum.states,
                                  undiagnosed.from.prep.continuum.states,
                                  undiagnosed.no.prep.continuum.states,
                                  engaged.continuum.states,
                                  suppressed.continuum.states,
                                  
                                  acute.cd4.strata,
                                  chronic.cd4.strata,
                                  
                                  active.idu.risk.states,
                                  
                                  is.continuum.collapsed,
                                  
                                  transition.mapping,
                                  
                                  additional.components.values = list(), #will be passed whole-hog to components objects
                                  
                                  version.manager = VERSION.MANAGER
                                  )
{
    # Some error checking
    if (!is.numeric(age.cutoffs) || length(age.cutoffs)==0 || any(is.na(age.cutoffs)))
        stop("age.cutoffs must be a non-empty numeric vector with no NA values")
    if (!is.character(races) || length(races)==0 || any(is.na(races)))
        stop("races must be a non-empty character vector with no NA values")
    if (!is.character(locations) || length(locations)==0 || any(is.na(locations)))
        stop("locations must be a non-empty character vector with no NA values")
    if (!is.character(subpopulations) || length(subpopulations)==0 || any(is.na(subpopulations)))
        stop("subpopulations must be a non-empty character vector with no NA values")
    if (!is.character(sexes) || length(sexes)==0 || any(is.na(sexes)))
        stop("sexes must be a non-empty character vector with no NA values")
    if (!is.character(risks) || length(risks)==0 || any(is.na(risks)))
        stop("risks must be a non-empty character vector with no NA values")
    if (!is.character(hiv.subsets) || length(hiv.subsets)==0 || any(is.na(hiv.subsets)))
        stop("hiv.subsets must be a non-empty character vector with no NA values")
    
    if (!is.character(continuum.of.care) || length(continuum.of.care)==0 || any(is.na(continuum.of.care)))
        stop("continuum.of.care must be a non-empty numeric vector with no NA values")
    if (!is.character(cd4.strata) || length(cd4.strata)==0 || any(is.na(cd4.strata)))
        stop("cd4.strata must be a non-empty character vector with no NA values")
    if (!is.character(hiv.subsets) || length(hiv.subsets)==0 || any(is.na(hiv.subsets)))
        stop("hiv.subsets must be a non-empty character vector with no NA values")
    
    if (!is.character(diagnosed.continuum.states) || length(diagnosed.continuum.states)==0 ||
        any(is.na(diagnosed.continuum.states)) || !is.subset(continuum.of.care, diagnosed.continuum.states))
        stop("diagnosed.continuum.states must be a non-empty character vector with no NA values that is a subset of continuum.of.care")
    if (!is.character(first.diagnosed.continuum.states) || length(first.diagnosed.continuum.states)==0 ||
        any(is.na(first.diagnosed.continuum.states)) || !is.subset(diagnosed.continuum.states, first.diagnosed.continuum.states))
        stop("first.diagnosed.continuum.states must be a non-empty character vector with no NA values that is a subset of diagnosed.continuum.states")
    
    if (!is.character(undiagnosed.no.prep.continuum.states) || any(is.na(undiagnosed.no.prep.continuum.states)) || 
        !is.subset(continuum.of.care, undiagnosed.no.prep.continuum.states) || 
        length(intersect(undiagnosed.no.prep.continuum.states, diagnosed.continuum.states))>0)
        stop("undiagnosed.no.prep.continuum.states must be a non-empty character vector with no NA values that is a subset of continuum.of.care but does NOT overlap with diagnosed.continuum.states It may be empty.")
    if (!is.character(undiagnosed.from.prep.continuum.states) || any(is.na(undiagnosed.from.prep.continuum.states)) || 
        !is.subset(continuum.of.care, undiagnosed.from.prep.continuum.states) ||
        length(intersect(undiagnosed.no.prep.continuum.states, diagnosed.continuum.states))>0 ||
        length(intersect(undiagnosed.no.prep.continuum.states, undiagnosed.from.prep.continuum.states))>0)
        stop("undiagnosed.from.prep.continuum.states must be a non-empty character vector with no NA values that is a subset of continuum.of.care and does NOT overlap with diagnosed.continuum.states or undiagnosed.no.prep.continuum.states. It may be empty.")
    
    if (!(length(engaged.continuum.states)==1 && is.na(engaged.continuum.states)) &&
        (!is.character(engaged.continuum.states) || length(engaged.continuum.states)==0 ||
         any(is.na(engaged.continuum.states)) || !is.subset(continuum.of.care, engaged.continuum.states)))
        stop("engaged.continuum.states must be either be NA or a non-empty character vector with no NA values that is a subset of continuum.of.care")
    if (!(length(suppressed.continuum.states)==1 && is.na(suppressed.continuum.states)) &&
        (!is.character(suppressed.continuum.states) || length(suppressed.continuum.states)==0 ||
         any(is.na(suppressed.continuum.states)) || !is.subset(continuum.of.care, suppressed.continuum.states)))
        stop("suppressed.continuum.states must be either be NA or a non-empty character vector with no NA values that is a subset of continuum.of.care")
    
    if (!is.character(acute.cd4.strata) || any(is.na(acute.cd4.strata)) || 
        !is.subset(cd4.strata, acute.cd4.strata))
        stop("acute.cd4.strata must be a character vector with no NA values that is a subset of cd4.strata. It may be empty")
    if (!is.character(chronic.cd4.strata) || any(is.na(chronic.cd4.strata)) || 
        !is.subset(cd4.strata, chronic.cd4.strata))
        stop("chronic.cd4.strata must be a character vector with no NA values that is a subset of cd4.strata. It may be empty")
    
    if (!is.character(active.idu.risk.states) || any(is.na(active.idu.risk.states)) || 
        !is.subset(risks, active.idu.risk.states))
        stop("active.idu.risk.states must be a character vector with no NA values that is a subset of risks It may be empty")
    
    if (!is.logical(is.continuum.collapsed) || length(is.continuum.collapsed) != 1)
        stop("is.continuum.collapsed must be a scalar logical value")
    
    if (!is(transition.mapping, 'transition.mapping'))
        stop("transmission.mapping must be of class 'transmission.mapping'")
    
    if (!is.list(additional.components.values) || 
        (length(additional.components.values)>0 && 
             (is.null(names(additional.components.values)) || 
              any(is.na(names(additional.components.values))) ||
              any(names(additional.components.values)=='')))
        )
        stop("additional.components.values must be a named list")
    
    # Make the settings object
    age.strata = make.age.strata(age.cutoffs)
    settings = list(VERSION = version,
              
              AGE_CUTOFFS = age.cutoffs,
              AGES = age.strata,
              RACES = races,
              LOCATIONS = locations,
              SUBPOPULATIONS = subpopulations,
              SEXES = sexes,
              RISK_STRATA = risks,
              NON_HIV_SUBSETS = non.hiv.subsets,
              
              CONTINUUM_OF_CARE = continuum.of.care,
              CD4_STRATA = cd4.strata,
              HIV_SUBSETS = hiv.subsets,
              
              DIMENSION.NAMES = list(age=age.strata$labels,
                                     race=races,
                                    # location=locations,
                                     subpopulation=subpopulations,
                                     sex=sexes,
                                     risk=risks,
                                     non.hiv.subset=non.hiv.subsets,
                                     continuum=continuum.of.care,
                                     cd4=cd4.strata,
                                     hiv.subset=hiv.subsets),
              
              FIRST_DIAGNOSED_STATE = first.diagnosed.continuum.states,
              DIAGNOSED_STATES = diagnosed.continuum.states,
              UNDIAGNOSED_STATES = setdiff(continuum.of.care, diagnosed.continuum.states),
              UNDIAGNOSED_FROM_PREP = undiagnosed.from.prep.continuum.states,
              UNDIAGNOSED_NO_PREP = undiagnosed.no.prep.continuum.states,
              SUPPRESSED_STATES = suppressed.continuum.states,
              ENGAGED_STATES = engaged.continuum.states,
              DISENGAGED_STATES = setdiff(continuum.of.care, engaged.continuum.states),
              
              ACUTE_STATES = acute.cd4.strata,
              CHRONIC_STATES = chronic.cd4.strata,
              
              ACTIVE_IDU_STATE = active.idu.risk.states,
              
              IS_CONTINUUM_COLLAPSED = is.continuum.collapsed,
              
              additional.components.values = additional.components.values
    )
    
    settings$dimension.names.by.subgroup = list(
        hiv.negative = settings$DIMENSION.NAMES[c('age','race',#'location',
                                                  'subpopulation','sex','risk','non.hiv.subset')],
        hiv.positive = settings$DIMENSION.NAMES[c('age','race',#'location',
                                                  'subpopulation','sex','risk','continuum','cd4','hiv.subset')]
    )
    
    settings$transition.mapping = check.transition.mapping.against.settings(transition.mapping, settings)
    
    
    class(settings) = 'jheem.settings'
    
    # Register and return
    version.manager = register.version(version=version,
                                       settings=settings,
                                       prior.versions=prior.versions,
                                       directory.suffix = directory.suffix,
                                       version.manager = version.manager)
    
    version.manager
}

# A copy constructor
# to overwrite a setting on the template, pass a new value (eg to age.cutoffs, races, etc)
# to extend the setting, use extra.<setting>
# Cannot both overwrite and extend
# Returns the version manager with the new version registered
copy.and.modify.jheem.settings <- function(template.settings,
                                           version,
                                           prior.versions=template.settings$VERSION,
                                           directory.suffix,
                                           
                                           age.cutoffs=c(template.settings$AGE_CUTOFFS, extra.age.cutoffs),
                                           races=c(template.settings$RACES, extra.races),
                                           locations=c(template.settings$LOCATIONS, extra.locations),
                                           subpopulations=c(template.settings$SUBPOPULATIONS, extra.subpopulations),
                                           sexes=c(template.settings$SEXES, extra.sexes),
                                           risks=c(template.settings$RISK_STRATA, extra.risks),
                                           non.hiv.subsets=c(template.settings$NON_HIV_SUBSETS, extra.non.hiv.subsets),
                                           continuum.of.care=c(template.settings$CONTINUUM_OF_CARE, extra.continuum.of.care),
                                           cd4.strata=c(template.settings$CD4_STRATA, extra.cd4.strata),
                                           hiv.subsets=c(template.settings$HIV_SUBSETS, extra.hiv.subsets),
                                           
                                           first.diagnosed.continuum.states=c(template.settings$FIRST_DIAGNOSED_STATE, extra.first.diagnosed.continuum.states),
                                           diagnosed.continuum.states=c(template.settings$DIAGNOSED_STATES, extra.diagnosed.continuum.states),
                                           undiagnosed.from.prep.continuum.states=c(template.settings$UNDIAGNOSED_FROM_PREP, extra.undiagnosed.from.prep.continuum.states),
                                           undiagnosed.no.prep.continuum.states=c(template.settings$UNDIAGNOSED_NO_PREP, extra.undiagnosed.no.prep.continuum.states),
                                           engaged.continuum.states=c(template.settings$ENGAGED_STATES, extra.engaged.continuum.states),
                                           suppressed.continuum.states=c(template.settings$SUPPRESSED_STATES, extra.suppressed.continuum.states),
                                           
                                           acute.cd4.strata=c(template.settings$ACUTE_STATES, extra.acute.cd4.strata),
                                           chronic.cd4.strata=c(template.settings$CHRONIC_STATES, extra.chronic.cd4.strata),
                                           
                                           active.idu.risk.states=c(template.settings$ACTIVE_IDU_STATE, extra.active.idu.risk.states),
                                           
                                           is.continuum.collapsed=template.settings$IS_CONTINUUM_COLLAPSED,
                                           
                                           transition.mapping=template.settings$transition.mapping,
                                           
                                           additional.components.values = c(template.settings$additional.components.values,
                                                                            extra.additional.components.values),
                                           
                                           extra.age.cutoffs=numeric(),
                                           extra.races=character(),
                                           extra.locations=character(),
                                           extra.subpopulations=character(),
                                           extra.sexes=character(),
                                           extra.risks=character(),
                                           extra.non.hiv.subsets=character(),
                                           extra.continuum.of.care=character(),
                                           extra.cd4.strata=character(),
                                           extra.hiv.subsets=character(),
                                           
                                           extra.first.diagnosed.continuum.states=character(),
                                           extra.diagnosed.continuum.states=character(),
                                           extra.undiagnosed.from.prep.continuum.states=character(),
                                           extra.undiagnosed.no.prep.continuum.states=character(),
                                           extra.engaged.continuum.states=character(),
                                           extra.suppressed.continuum.states=character(),
                                           
                                           extra.acute.cd4.strata=character(),
                                           extra.chronic.cd4.strata=character(),
                                           
                                           extra.active.idu.risk.states=character(),
                                           
                                           extra.additional.components.values = list(),
                                           
                                           version.manager = VERSION.MANAGER)
{
    # Error checking
    if (!tail.subset.equal(age.cutoffs, extra.age.cutoffs))
        stop("You cannot set both 'age.cutoffs' and 'extra.age.cutoffs'")
    if (!tail.subset.equal(races, extra.races))
        stop("You cannot set both 'races' and 'extra.races'")
    if (!tail.subset.equal(locations, extra.locations))
        stop("You cannot set both 'locations' and 'extra.locations'")
    if (!tail.subset.equal(subpopulations, extra.subpopulations))
        stop("You cannot set both 'subpopulations' and 'extra.subpopulations'")
    if (!tail.subset.equal(sexes, extra.sexes))
        stop("You cannot set both 'sexes' and 'extra.sexes'")
    if (!tail.subset.equal(risks, extra.risks))
        stop("You cannot set both 'risks' and 'extra.risks'")
    if (!tail.subset.equal(non.hiv.subsets, extra.non.hiv.subsets))
        stop("You cannot set both 'non.hiv.subsets' and 'extra.non.hiv.subsets'")
    
    if (!tail.subset.equal(continuum.of.care, extra.continuum.of.care))
        stop("You cannot set both 'continuum.of.care' and 'extra.continuum.of.care'")
    if (!tail.subset.equal(cd4.strata, extra.cd4.strata))
        stop("You cannot set both 'cd4.strata' and 'extra.cd4.strata'")
    if (!tail.subset.equal(hiv.subsets, extra.hiv.subsets))
        stop("You cannot set both 'hiv.subsets' and 'extra.hiv.subsets'")
    
    
    if (!tail.subset.equal(first.diagnosed.continuum.states, extra.first.diagnosed.continuum.states))
        stop("You cannot set both 'first.diagnosed.continuum.states' and 'extra.first.diagnosed.continuum.states'")
    if (!tail.subset.equal(diagnosed.continuum.states, extra.diagnosed.continuum.states))
        stop("You cannot set both 'diagnosed.continuum.states' and 'extra.diagnosed.continuum.states'")
    if (!tail.subset.equal(undiagnosed.from.prep.continuum.states, extra.undiagnosed.from.prep.continuum.states))
        stop("You cannot set both 'undiagnosed.from.prep.continuum.states' and 'extra.undiagnosed.from.prep.continuum.states'")
    if (!tail.subset.equal(undiagnosed.no.prep.continuum.states, extra.undiagnosed.no.prep.continuum.states))
        stop("You cannot set both 'undiagnosed.no.prep.continuum.states' and 'extra.undiagnosed.no.prep.continuum.states'")
    if (!tail.subset.equal(engaged.continuum.states, extra.engaged.continuum.states))
        stop("You cannot set both 'engaged.continuum.states' and 'extra.engaged.continuum.states'")
    if (!tail.subset.equal(suppressed.continuum.states, extra.suppressed.continuum.states))
        stop("You cannot set both 'suppressed.continuum.states' and 'extra.suppressed.continuum.states'")
    
    if (!tail.subset.equal(acute.cd4.strata, extra.acute.cd4.strata))
        stop("You cannot set both 'acute.cd4.strata' and 'extra.acute.cd4.strata'")
    if (!tail.subset.equal(acute.cd4.strata, extra.acute.cd4.strata))
        stop("You cannot set both 'acute.cd4.strata' and 'extra.acute.cd4.strata'")
    
    if (!tail.subset.equal(active.idu.risk.states, extra.active.idu.risk.states))
        stop("You cannot set both 'active.idu.risk.states' and 'extra.active.idu.risk.states'")
    
    if (length(additional.components.values) != length(template.settings$additional.components.values) &&
        length(extra.additional.components.values) != 0)
        stop("You cannot set both 'additional.components.values' and 'extra.additional.components.values'")
    
    # Call the constructor    
    create.jheem.settings(version=version,
                          prior.versions=prior.versions,
                          directory.suffix=directory.suffix,
                          
                          age.cutoffs=age.cutoffs,
                          races=races,
                          locations=locations,
                          subpopulations=subpopulations,
                          sexes=sexes,
                          risks=risks,
                          non.hiv.subsets=non.hiv.subsets,
                          continuum.of.care=continuum.of.care,
                          cd4.strata=cd4.strata,
                          hiv.subsets=hiv.subsets,
                          
                          first.diagnosed.continuum.states=first.diagnosed.continuum.states,
                          diagnosed.continuum.states=diagnosed.continuum.states,
                          undiagnosed.from.prep.continuum.states=undiagnosed.from.prep.continuum.states,
                          undiagnosed.no.prep.continuum.states=undiagnosed.no.prep.continuum.states,
                          engaged.continuum.states=engaged.continuum.states,
                          suppressed.continuum.states=suppressed.continuum.states,
                          
                          acute.cd4.strata=acute.cd4.strata,
                          chronic.cd4.strata=chronic.cd4.strata,
                          
                          active.idu.risk.states=active.idu.risk.states,
                          
                          is.continuum.collapsed=is.continuum.collapsed,
                          
                          transition.mapping = transition.mapping,
                          
                          additional.components.values = additional.components.values,
                          
                          version.manager = version.manager
    )
}

# A helper to get the JHEEM version from a sim
# This is a separate function to be backwards compatible with sims from 
#   version 'collapsed_1.0' which did not have an explicit version stored
get.sim.version <- function(sim)
{
    if (!is(sim, 'jheem.results'))
        stop("sim must be of class 'jheem.results'")
    else if (is.null(sim$version))
    {
        if (get.settings.for.version$IS_CONTINUUM_COLLAPSED)
            'collapsed_1.0'
        else
            stop("No version was set for this simulation")
    }
    else
        sim$version
}

# low level helper for checking
tail.subset.equal <- function(set, tail)
{
    n.tail = length(tail)
    n.set = length(set)
    
    if (n.tail==0)
        return (T)
    if (n.set < n.tail)
        return (F)
    
    tail.indices = (1+n.set-n.tail):n.set
    subset = set[tail.indices]
    
    all((is.na(subset) & is.na(tail)) |
            (!is.na(subset) & !is.na(tail) & subset==tail))
}

is.subset <- function(set, potential.subset)
{
    length(setdiff(potential.subset, set))==0
}

