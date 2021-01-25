# EndingHIV model ####
'# EndingHIV Model::RShiny Interface'


# Constants (TEMPORARY) ####
AGES = list(
  name='age-groups',
  shortName='age',
  label='Age',
  choices=c(
    age1='13-24 years',
    age2='25-34 years',
    age3='35-44 years',
    age4='45-54 years',
    age5='55+ years') )

RACES = list(
  name='racial-groups',
  shortName='race',
  label='Race',
  choices=c(
    black="Black",
    hispanic="Hispanic",
    other="Other") )

SEXES = list(
  name='sex',
  shortName='sex',
  label='Sex',
  choices=c(
    male='Male',
    female='Female') )

RISKS = list(
  name='risk-groups',
  shortName='risk',
  label='Risk Factor',
  choices=c(
    msm="MSM",
    idu="IDU",
    msm_idu="MSM+IDU",
    heterosexual="Heterosexual") )

RISKS2 = list(
  name='risk-groups',
  shortName='risk',
  label='Risk Factor',
  choices=c(
    msm="MSM",
    iduActive="Active IDU",
    iduPrior="Prior IDU",
    msm_iduActive="MSM + active IDU",
    msm_iduPrior="MSM + prior IDU",
    heterosexual="Heterosexual") )

DIMENSION.VALUES = list(
  age=AGES,
  race=RACES,
  sex=SEXES,
  risk=RISKS)

DIMENSION.VALUES2 = list(
  age=AGES,
  race=RACES,
  sex=SEXES,
  risk=RISKS2)

DIMENSIONS = c(
  age='Age',
  race='Race',
  sex='Sex',
  risk='HIV Risk Factor')

DATA.TYPES = c(
    incidence="HIV Incidence",
    new='Reported Diagnoses',
    prevalence='Estimated Prevalence',
    mortality='HIV Mortality',
    suppression='Viral HIV Suppression',
    diagnosed="Awareness of HIV Diagnosis")

customInterventions.demog.defaults <- function() {
  defaults = list()
  for (dimension in names(DIMENSION.VALUES2)) {
    choices = DIMENSION.VALUES2[[dimension]][['choices']] 
    # defaults[[diminsion]] = lapply(choices, function(choice) {
    #   FALSE
    # })
    # for (choice in choices) {
    #   defaults[[diminsion]][[choice]] = FALSE
    # }
    defaults[[dimension]] = rep('', length(choices))
  }
  # Not sure why this needed to be repeated: 
  for (dimension in names(DIMENSION.VALUES2)) {
    choices = DIMENSION.VALUES2[[dimension]][['choices']] 
    defaults[[dimension]] = rep('', length(choices))
  }
  defaults
}

# Support functions 1: FUNCTIONS TO GET THE OPTIONS FOR ARGUMENTS ####

#'@description Get the versions of the model which are available for simulations to come fromrep
#'@param version The indicator for the version of the model (corresponds to one of the values of names(get.version.options))
#'@return A named character vector. The values of the vector are 'displayable' names of places; the names of the vector are the location codes to be passed to plot.simulations
get.version.options <- function()
{
  c('1.0'='1.0')
}

#'@description Get the locations for which simulations are available for a
#' given model version
#'@param version The indicator for the version of the model. Corresponds
#' to one of the values of names(get.version.options)
#'@return A named character vector. The values of the vector are 'display
#'able' names of places; the names of the vector are the location codes to
#' be passed to plot.simulations
get.location.options <- function(version)
{
  # list(
  #   '12580'=list(
  #     name='12580',
  #     label='Baltimore-Columbia-Towson, MD'),
  #   '33100'=list(
  #     name='33100',
  #     label='Miami-Fort Lauderdale-Pompano Beach, FL')
  # )
  sims.names = sims.list()
  locations = get.locations.from.filenames(sims.names)
  locations = unique(locations)
  location.names = unlist(msa.names(locations))
  names(location.names) = locations
  location.names = sort(location.names)
  
  location.names
}

#'@description Get the potential formats for plots
#'@param version The indicator for the version of the model. Corresponds
#' to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of
#' names(get.location.options(version))
#'@return A named character vector. The values of the vector are 
#''displayable' descriptions of plot format; the names of the vector are
#' the location codes to be passed to plot.simulations
get.plot.format.options <- function(version, location)
{
  # TODO: @ Todd: Is it ok for labels to come first?
  # I found flipping these easier. but if we do want names on left and
  # labels on right, we should declare it as so and then invert it when
  # returning for my purposes.
  # https://rdrr.io/cran/searchable/man/invert.html
  c('individual.simulations'='Individual Simulations',
    'mean.and.interval'='Mean and Prediction Interval',
    'median.and.interval'='Median and Prediction Interval'
    )
}

#'@description Get the data types available for plotting for a location
#'@param version The indicator for the version of the model. Corresponds
#' to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of
#' names(get.location.options(version))
#'@return A named vector of dimension names. The values of the vector are
#' 'displayable' descriptions of data types; the names of the vector are
#'  the codes to be passed to plot.simulations
get.data.type.options <- function(version, location)
{
  DATA.TYPES
}

#'@description Get the years available for plotting for a location
#'@param version The indicator for the version of the model. Corresponds
#' to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of
#' names(get.location.options(version))
#'@return A numeric vector of potential years for the location
get.year.options <- function(version, location)
{
  2010:2030
}

#'@description Get the potential names of dimensions by which a plot can
#' be 'facetted' (split into separate panels)
#'@param version The indicator for the version of the model. Corresponds
#' to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of
#' names(get.location.options(version))
#'@return A named vector of dimension names. The values of the vector are
#' 'displayable' options; the names of the vector are the codes to be
#'  passed to plot.simulations
get.facet.by.options <- function(version, location)
{
  DIMENSIONS
}

#'@description Get the potential names of dimensions by which a plot can
#' be 'split' (with a separate line on each panel)
#'@param version The indicator for the version of the model. Corresponds
#' to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of
#' names(get.location.options(version))
#'@return A named vector of dimension names. The values of the vector are
#' 'displayable' options; the names of the vector are the codes to be
#'  passed to plot.simulations
get.split.by.options <- function(version, location)
{
  DIMENSIONS
}

#'@description Get the possible interventions that can be plotted for a
#' location
#'@param version The indicator for the version of the model. Corresponds
#' to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of
#' names(get.location.options(version))
#'@return A named list of intervention objects. The names of the list are
#' what is to be passed to plot.simulations
# An intervention object is a list with the following components:
# $target.groups - a text description of which demographic subgroups
#  targeted on the intervention
# $testing.frequency - the averagte number of tests per year for targeted
#  subgroups. May be NA
# $suppressed.proportion - the fraction (0 to 1) of people with HIV in
#  targeted subgroups who have suppressed viral loads. May be NA
# $prep.coverage - the fraction (0 to 1) of people without HIV in targeted
#  subgroups who are prescribed and adherent to PrEP. May be NA
# $intervention.start.year - the year at which the interventions begin to
#  ramp up
# $intervention.implemented.year - the year at which the interventions are
#  fully ramped up
get.intervention.options <- function(
  version, location, return.intervention.objects = F)
{
    
    if (length(location)>0 && location !='')
    {
        sims.names = sims.list()
        sims.locations = get.locations.from.filenames(sims.names)
        sims.names = sims.names[sims.locations==location]
        interventions = get.interventions.from.filenames(sims.names)
        interventions = interventions[!sapply(interventions, is.null.intervention)]
        
        o = order.interventions(interventions)
        interventions = interventions[o]
        
        if (return.intervention.objects)
            rv = interventions
        else
        {
            rv = lapply(interventions, function(int){
                list(name=get.intervention.code(int),
                     label=get.intervention.short.name(int),
                     description=get.intervention.description(int))
            })
            names(rv)=sapply(rv, function(elem){elem$label})
        }
    }
    else
        rv = list()
    
    rv
}

get.interventions.simpleList <- function(version, location) {
  options = get.intervention.options(version, location)
  simpleList <- sapply(options, function(option) {
    option[['label']]
  })
  simpleList = c('None', simpleList)
  names(simpleList) = c('none', names(options))
  
  simpleList
}

#'@description Get the potential values (which can be subsetted) for each
#' dimension
#'@param version The indicator for the version of the model. Corresponds
#' to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of
#' names(get.location.options(version))
#'@return A named list of character vectors. 
#' The names of the list correspond to names(get.facet.by.options()) or
#'  names(get.split.by.options())
#' Each element in the list is a named character vector of 
#' possible values. 
#'  names(get.dimension.value.options()[[d]]) correspond to the values of
#'   get.facet.by.options() or get.split.by.options(), and should be
#'    passed to plot.simulations() function via the dimension.subsets
#'     argument
#'  the values of get.dimension.value.options()[[d]] are 'displayable'
#'   value names
get.dimension.value.options <- function(
  version, location, msm_idu_mode=FALSE)
{
  if (msm_idu_mode == FALSE)
    DIMENSION.VALUES
  else
    DIMENSION.VALUES2
}

#'@description Get the potential summary statistics to display
#'@param version The indicator for the version of the model. Corresponds
#' to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of
#' names(get.location.options(version))
#'@return A named character vector. The values of the vector are 'display
#'able' options; the names of the vector are the codes to be passed to
#' plot.simulations
get.summary.statistic.options <- function(version, location)
{
  c(none='None',
    incidence.reduction='Reduction in Incidence')
}

# Support functions 2: FUNCTIONS TO DEFINE WHICH OPTIONS APPLY ####
#'@description Whether an "Interval Coverage" quantity applies for a given
#' plot format
#'@param plot.format The name of a plot format. One of names(get.plo
#'t.format.options())
#'@return A boolean indicator of whether an "Interval Coverage" quantity
#' applies for the given plot format
plot.interval.coverage.applies.to.plot.format <- function(plot.format)
{
  c(mean.and.interval=T,
    median.and.interval=T,
    individual.simulations=F)
}


#@joe
# 1. click simulate
# 2. find out what datasets need to be fetched: 
#   get.sims.to.load
# 3. load 
# 4. pass data to plot
get.sim.filenames.to.load <- function(
  version,
  location,
  intervention.codes)
{
    baseline.filename = get.simset.filename(version=version,
                                            location=location,
                                            intervention=NULL)
    
    other.filenames = sapply(intervention.codes, function(code){
      #code = intervention.short.name.to.code(int.name)
      get.simset.filename(location = location,
                          intervention.code = code)
    })

    c(baseline.filename, other.filenames)
}


# Main Function: THE PLOT FUNCTION ####
#'@param description The function that actually generates plots
#'
#' THE FUNDAMENTAL ARGUMENTS THAT DEFINE THE PLOT
#'@param version The indicator for the version of the model. Corresponds
#' to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of
#' names(get.location.options(version))
#'@param intervention.name
#'@param years The numeric years to plot
#'@param data.types The names of 
#'@param facet.by [char[]] The names of dimensions according to which to
#' 'facet' (a separate panel for each) - this should be a subset of name
#' s(get.facet.by.options(location))
#'@param split.by [char[]] The names of dimensions according which to
#' split the plot (a separate line on each panel) - this should be a
#'  subset of names(get.split.by.options(location))
#'@param dimension.subsets [type] A named list of which values for each
#' dimension to include in the plot. The names of the list are the named
#' dimensions as given by names(get.dimension.value.options()), and the
#'  values for dimension d must be a subset of get.dimension.values()[[d]]
#'@param plot.format [type] The character string indicating the format for
#' the plot - this should be one of 
#' names(get.plot.format.options(location))
#'@param show.truth [type] Whether to include "truth" (known from epi
#' surveillance) in the plot
#'
#' ARGUMENTS THAT DEFINE STATISTICS
#'@param plot.interval.coverage A fraction (0 to 1) which the plotted
#' prediction interval should cover (only applies to some plot formats)
#'@param summary.statistic The name of the summary statistic to show.
#' Should be one of names(get.summary.statistic.options(location))
#'@param summary.statistic.interval.coverage A fraction (0 to 1) which the
#' interval for the summary statistic should cover
#'
#' THE STYLE ARGUMENTS
#'@param baseline.color The color with which to plots the baseline 
#'(pre-intervention) simulations
#'@param truth.color The color with which to plot truth (when available)
#'@param intervention.colors A named vector of colors, one for each
#' intervention in intervention.names. The names of the vector should be
#'intervention.names
#'@param plot.interval.alpha The alpha value (opacity) for prediction
#' intervals (if shown)
#'@param simulation.alpha The alpha value (opacity) for individual
#' simulation plots
#'@param simulation.line.size The line size for plotted simulations
#'@param truth.point.size The point size for plotted 'truth' (epi
#' surveillance) values#'
#'
#'@return A list with three values:
#' $plot - a plotly object
#' $change.df - a data frame
#' $notes - a character vector (which may be empty) of notes
make.simulations.plot.and.table <- function(
  cache,
  # Private meta params
  version,
  # Public params; Selectable in UI
  location,
  intervention.codes,
  years,
  data.types,
  facet.by,
  split.by,
  dimension.subsets,  # TODO: Problem? all 4 vals are null
  plot.format,
  
  label.change=T,
  change.years=c(2020,2030),
  change.decrease.is.positive=F,
  
  show.truth=T,
  plot.interval.coverage=0.95,
  
  plot.interval.alpha=0.25,
  simulation.alpha=0.125,
  simulation.line.size=if (plot.format=='individual.simulations') 2 else 5,
  truth.point.size=10
) {
  #Hard Overrides for now
  #plot.format = 'individual.simulations'
  filenames = get.sim.filenames.to.load(
      version=version,
      location=location,
      intervention.codes=intervention.codes)
  simsets = lapply(filenames, function(file) {
    key = simsetFilenameToCacheKey(file)
    simset = cache$get(key)
    simset })
  
  #map ages
  age.mapping = names(PRETTY.NAMES$age)
  names(age.mapping) = paste0('age', 1:5)
  if (!is.null(dimension.subsets) && !is.null(dimension.subsets$age))
      dimension.subsets$age = age.mapping[dimension.subsets$age]
  
  #Figure out coloring
  if (length(simsets)<=2 && length(split.by)>0)
      color.by = 'split'
  else
      color.by = 'intervention'
  
  withProgress(
    min=0, max=1, value = 0, 
    message="Building Figure and Table...", {
      rv = do.plot.simulations(
        simsets,
        years=years,
        data.types=data.types,
        facet.by,
        split.by,
        dimension.subsets,
        #for now, going to override the plot formats
        plot.format=plot.format, 
        
        show.truth=T,
        
        plot.interval.coverage=plot.interval.coverage,
        #summary.statistic=summary.statistic,
        #summary.statistic.interval.coverage=summary.statistic.interval.coverage,
        
        colors=pal_jama(),
        
        plot.interval.alpha=plot.interval.alpha,
        simulation.alpha=simulation.alpha,
        simulation.line.size=simulation.line.size,
        truth.point.size=truth.point.size,
        
        color.by = color.by,
        
        label.change = label.change,
        change.years = change.years,
        change.decrease.is.positive = change.decrease.is.positive,
        
        progress.update = setProgress,
        
        data.type.names = DATA.TYPES,
        return.change.data.frame = T)
      
      setProgress(value=1)
    })
    
    rv$notes = c('test note 1', 'test note 2')
    rv
}
