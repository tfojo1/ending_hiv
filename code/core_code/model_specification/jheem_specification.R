
CURRENT.CODE.ITERATION = '2.0'

##------------------##
##-- CONSTRUCTORS --##
##------------------##

# The base constructor
# Returns the version manager with the new version registered
#'@param state.mappings A list to alias single states or groups of states with a name. Should be a named list, where the name of each element is the 
create.jheem.specification <- function(version,
                                       prior.versions,
                                       directory.suffix,
                                       file.version,
                                       
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
                                       
                                       transmission.modes = c('sexual','idu'),
                                       
                                       state.name.mapping,
                                       
                                       is.continuum.collapsed,
                                       
                                       transition.mapping,
                                       
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
    
    if (!is.logical(is.continuum.collapsed) || length(is.continuum.collapsed) != 1)
        stop("is.continuum.collapsed must be a scalar logical value")
    
    if (!is(transition.mapping, 'transition.mapping'))
        stop("transmission.mapping must be of class 'transmission.mapping'")
    
    if (!is(state.name.mapping, 'state.name.mapping'))
        stop("'state.name.mapping' must be an object of class state.name.mapping")
    
    if (!is.character(transmission.modes) || length(transmission.modes)==0 || any(is.na(transmission.modes)))
        stop("transmission.modes must be a non-empty character vector with no NA values")
    
    # Make age strata
    
    age.strata = make.age.strata(age.cutoffs)
    
    
    # Make the dimension names list
    
    dimension.names = dimension.names.from = dimension.names.to =
        list(age=age.strata$labels,
             race=races,
             # location=locations,
             subpopulation=subpopulations,
             sex=sexes,
             risk=risks,
             non.hiv.subset=non.hiv.subsets,
             continuum=continuum.of.care,
             cd4=cd4.strata,
             hiv.subset=hiv.subsets)
    names(dimension.names.from) = paste0(names(dimension.names.from), '.from')
    names(dimension.names.to) = paste0(names(dimension.names.to), '.to')
    dimension.names = c(dimension.names, dimension.names.from, dimension.names.to)
    
    
    # Make the specification object
    specification = list(VERSION = version,
              
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
              
              DIMENSION.NAMES = dimension.names,
              
              FIRST_DIAGNOSED_STATE = state.name.mapping$first.diagnosed.states,
              DIAGNOSED_STATES = state.name.mapping$diagnosed.states,
              UNDIAGNOSED_STATES = setdiff(continuum.of.care, state.name.mapping$diagnosed.states),
              UNDIAGNOSED_FROM_PREP = state.name.mapping$undiagnosed.from.prep.states,
              UNDIAGNOSED_NO_PREP = state.name.mapping$undiagnosed.no.prep.states,
              SUPPRESSED_STATES = state.name.mapping$suppressed.states,
              ENGAGED_STATES = state.name.mapping$engaged.states,
              DISENGAGED_STATES = setdiff(continuum.of.care, state.name.mapping$engaged.states),
              
              ACUTE_STATES = state.name.mapping$acute.cd4.states,
              CHRONIC_STATES = state.name.mapping$chronic.cd4.states,
              
              ACTIVE_IDU_STATE = state.name.mapping$active.idu.risk.states,
              
              IS_CONTINUUM_COLLAPSED = is.continuum.collapsed,
              
              TRANSMISSION_MODES = transmission.modes
    )
    
    specification$dimension.names.by.subgroup = list(
        hiv.negative = specification$DIMENSION.NAMES[c('age','race',#'location',
                                                  'subpopulation','sex','risk','non.hiv.subset')],
        hiv.positive = specification$DIMENSION.NAMES[c('age','race',#'location',
                                                  'subpopulation','sex','risk','continuum','cd4','hiv.subset')],
        all = specification$DIMENSION.NAMES,
        general = specification$DIMENSION.NAMES[c('age','race',#'location',
                                             'subpopulation','sex','risk')]
    )
    
    class(specification) = 'jheem.specification'
    
    
    specification$state.name.mapping = check.state.name.mapping.against.specification(state.name.mapping,
                                                                            specification=specification)
    
    specification$transition.mapping = finalize.transmission.mapping(transition.mapping, 
                                                                            specification=specification)

    
    
    # Register and return
    version.manager = register.version(version=version,
                                       specification=specification,
                                       prior.versions=prior.versions,
                                       directory.suffix = directory.suffix,
                                       file.version=file.version,
                                       version.manager = version.manager)
    
    version.manager
}

# A copy constructor
# to overwrite a setting on the template, pass a new value (eg to age.cutoffs, races, etc)
# to extend the setting, use extra.<setting>
# Cannot both overwrite and extend
# Returns the version manager with the new version registered
copy.and.modify.jheem.specification <- function(template.specification,
                                           version,
                                           prior.versions=template.specification$VERSION,
                                           directory.suffix,
                                           file.version,
                                           state.name.mapping = NULL,
                                           
                                           age.cutoffs=c(template.specification$AGE_CUTOFFS, extra.age.cutoffs),
                                           races=c(template.specification$RACES, extra.races),
                                           locations=c(template.specification$LOCATIONS, extra.locations),
                                           subpopulations=c(template.specification$SUBPOPULATIONS, extra.subpopulations),
                                           sexes=c(template.specification$SEXES, extra.sexes),
                                           risks=c(template.specification$RISK_STRATA, extra.risks),
                                           non.hiv.subsets=c(template.specification$NON_HIV_SUBSETS, extra.non.hiv.subsets),
                                           continuum.of.care=c(template.specification$CONTINUUM_OF_CARE, extra.continuum.of.care),
                                           cd4.strata=c(template.specification$CD4_STRATA, extra.cd4.strata),
                                           hiv.subsets=c(template.specification$HIV_SUBSETS, extra.hiv.subsets),
                                           
                                           is.continuum.collapsed=template.specification$IS_CONTINUUM_COLLAPSED,

                                           transmission.modes = c(template.specification$TRANSMISSION_MODES, extra.transmission.modes),
                                           
                                           transition.mapping=copy.transition.mapping(template.specification$transition.mapping,
                                                                                      version = version),
                                           
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
                                           extra.transmission.modes=character(),

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
    
    if (!tail.subset.equal(transmission.modes, extra.transmission.modes))
        stop("You cannot set both 'transmission.modes' and 'extra.transmission.modes'")
    
    if (is.null(state.name.mapping))
        state.name.mapping = template$state.name.mapping
    else if (!is(state.name.mapping, 'state.name.mapping'))
        stop("'state.name.mapping' must be an object of class state.name.mapping")
    else
        state.name.mapping = merge.state.name.mappings(template$state.name.mapping,
                                                       state.name.mapping)
    
    # Call the constructor    
    create.jheem.specification(version=version,
                          prior.versions=prior.versions,
                          directory.suffix=directory.suffix,
                          file.version=file.version,
                          
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
                          
                          state.name.mapping = state.name.mapping,

                          is.continuum.collapsed=is.continuum.collapsed,
                          
                          transition.mapping = transition.mapping,
                          
                          version.manager = version.manager
    )
}

##------------------------##
##-- STATE NAME MAPPING --##
##------------------------##

REQUIRED.STATE.NAME.MAPPINGS = c(
    'first.diagnosed.states',
    'diagnosed.states',
    'undiagnosed.from.prep.states',
    'undiagnosed.no.prep.states',
    'engaged.states',
    'suppressed.states',
    'acute.cd4.states',
    'chronic.cd4.states',
    'active.idu.risk.states'
)

create.state.name.mapping <- function(first.diagnosed.states=NULL,
                                      diagnosed.states=NULL,
                                      undiagnosed.from.prep.states=NULL,
                                      undiagnosed.no.prep.states=NULL,
                                      engaged.states=NULL,
                                      suppressed.states=NULL,
                                      acute.cd4.states=NULL,
                                      chronic.cd4.states=NULL,
                                      active.idu.states=NULL,
                                      ...)
{
    # Pull in the named arguments
    rv = list(first.diagnosed.states = first.diagnosed.states,
              diagnosed.states = diagnosed.states,
              undiagnosed.from.prep.states = undiagnosed.from.prep.states,
              undiagnosed.no.prep.states = undiagnosed.no.prep.states,
              
              engaged.states = engaged.states,
              suppressed.states = suppressed.states,
              
              acute.cd4.states = acute.cd4.states,
              chronic.cd4.states = chronic.cd4.states,
              
              active.idu.states = active.idu.states
    )
    
    rv = rv[!sapply(rv, is.null)]
    
    # Pull in ... arguments
    args = list(...)
    if (length(args)>0)
    {
        if (is.null(names(args)) || any(is.na(names(args))) || any(names(args)==''))
            stop("arguments passed to ... must be named")
        if (max(table(names(args)))>1 || length(intersect(names(args), names(rv)))>0)
            stop("argument names for ... must be unique")
        
        rv = c(rv, args)
    }
    
    # Check validity of arguments
    invalid.mask = sapply(rv, function(value){
        length(value)== 0 ||
            (!is.character(value) && (length(value)>1 || !is.na(value))) || 
            (length(value)>1 && any(is.na(value))) ||
            (!all(is.na(value)) && max(table(value))>1)
    })
    
    if (any(invalid.mask))
        stop(paste0("Values for the state.name.mapping must be character vectors with unique values (NAs allowed if only a single, NA element).",
                    " The values for ",
                    paste0("'", names(rv)[invalid.mask], "'", collapse=', '),
                    " was/were invalid"))
    
    # Set class and return
    class(rv) = 'state.name.mapping'
    rv
}

REQUIRED.STATE.MAPPING.NAMES = c(
    'first.diagnosed.states',
    'diagnosed.states',
    'undiagnosed.from.prep.states',
    'undiagnosed.no.prep.states',
    'engaged.states',
    'suppressed.states',
    'acute.cd4.states',
    'chronic.cd4.states',
    'active.idu.states'
)

check.state.name.mapping.against.specification <- function(state.name.mapping, specification)
{
    if (!is(state.name.mapping, 'state.name.mapping'))
        stop("'state.name.mapping' must be an object of class 'state.name.mapping'")
    
    if (!is(specification, 'jheem.specification'))
        stop("'specification' must be an object of class 'jheem.specification'")
    
    # Check that state names are valid
    valid.state.names = unlist(specification$DIMENSION.NAMES)
    sapply(names(state.name.mapping), function(name){
        invalid.states = setdiff(state.name.mapping[[name]][!is.na(state.name.mapping[[name]])],
                                 valid.state.names)
        
        if (length(invalid.states)>0)
            stop(paste0("Invalid state(s) specified in state.name.mapping for '",
                        name, "': ",
                        paste0("'", invalid.states, "'", collapse=', ')))
    })
    
    # Make sure all required names are present
    
    missing.names = setdiff(REQUIRED.STATE.MAPPING.NAMES, names(state.name.mapping))
    if (length(missing.names))
        stop(paste0("The following name(s) is/are required but is/are missing from the state.name.mapping: ",
                    paste0("'", missing.names, "'", collapse=', ')))
    
    # Check specific values
    
    if (any(is.na(state.name.mapping$diagnosed.states)) || 
        !is.subset(specification$CONTINUUM_OF_CARE, state.name.mapping$diagnosed.states))
        stop("diagnosed.states must be a non-empty character vector with no NA values that is a subset of continuum.of.care")
    if (any(is.na(state.name.mapping$first.diagnosed.states)) || 
        !is.subset(state.name.mapping$diagnosed.states, state.name.mapping$first.diagnosed.states))
        stop("first.diagnosed.states must be a non-empty character vector with no NA values that is a subset of diagnosed.states")
    
    if (any(is.na(state.name.mapping$undiagnosed.no.prep.states)) || 
        !is.subset(specification$CONTINUUM_OF_CARE, state.name.mapping$undiagnosed.no.prep.states) || 
        length(intersect(state.name.mapping$undiagnosed.no.prep.states, state.name.mapping$diagnosed.states))>0)
        stop("undiagnosed.no.prep.states must be a non-empty character vector with no NA values that is a subset of continuum.of.care but does NOT overlap with diagnosed.states.")
    if (any(is.na(state.name.mapping$undiagnosed.from.prep.states)) || 
        !is.subset(specification$CONTINUUM_OF_CARE, state.name.mapping$undiagnosed.from.prep.states) ||
        length(intersect(state.name.mapping$undiagnosed.no.prep.states, state.name.mapping$diagnosed.states))>0 ||
        length(intersect(state.name.mapping$undiagnosed.no.prep.states, state.name.mapping$undiagnosed.from.prep.states))>0)
        stop("undiagnosed.from.prep.states must be a non-empty character vector with no NA values that is a subset of continuum.of.care and does NOT overlap with diagnosed.states or undiagnosed.no.prep.states.")
    
    if (!(length(state.name.mapping$engaged.states)==1 && is.na(state.name.mapping$engaged.states)) &&
        (any(is.na(state.name.mapping$engaged.states)) || !is.subset(specification$CONTINUUM_OF_CARE, state.name.mapping$engaged.states)))
        stop("engaged.states must be either be NA or a non-empty character vector with no NA values that is a subset of continuum.of.care")
    if (!(length(state.name.mapping$suppressed.states)==1 && is.na(state.name.mapping$suppressed.states)) &&
        (any(is.na(state.name.mapping$suppressed.states)) || !is.subset(specification$CONTINUUM_OF_CARE, state.name.mapping$suppressed.states)))
        stop("suppressed.states must be either be NA or a non-empty character vector with no NA values that is a subset of continuum.of.care")
    
    if (any(is.na(state.name.mapping$acute.cd4.strata)) || 
        !is.subset(specification$CD4_STRATA, state.name.mapping$acute.cd4.strata))
        stop("acute.cd4.strata must be a character vector with no NA values that is a subset of cd4.strata.")
    if (any(is.na(state.name.mapping$chronic.cd4.strata)) || 
        !is.subset(specification$CD4_STRATA, state.name.mapping$chronic.cd4.strata))
        stop("chronic.cd4.strata must be a character vector with no NA values that is a subset of cd4.strata.")
    
    if (any(is.na(state.name.mapping$active.idu.states)) || 
        !is.subset(specification$RISK_STRATA, state.name.mapping$active.idu.states))
        stop("active.idu.states must be a character vector with no NA values that is a subset of risks")
    
    
    # Return
    state.name.mapping
}

# Values from mapping2 overwrite values from mapping1
merge.state.name.mappings <- function(mapping1, mapping2)
{
    if (!is(mapping1, 'state.name.mapping') || !is(mapping2, 'state.name.mapping'))
        stop("'mapping1' and 'mapping2' must be state.name.mapping objects")
    
    rv = mapping1
    rv[names(mapping2)] = mapping2
    
    rv
}

##-----------------------##
##-- LOW-LEVEL HELPERS --##
##-----------------------##

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

