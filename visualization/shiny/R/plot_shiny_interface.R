# EndingHIV model ####
'# EndingHIV Model::RShiny Interface'

library('ggplot2')

source("R/server.utils.R")

test.config.on = FALSE

# Constants (TEMPORARY) ####
AGES = list(
  name='age-groups',
  label='Age groups',
  choices=c(
    age1='13-24 years',
    age2='25-34 years',
    age3='35-44 years',
    age4='45-54 years',
    age5='55+ years') )

RACES = list(
  name='racial-groups',
  label='Racial groups',
  choices=c(
    black="Black",
    hispanic="Hispanic",
    other="Other") )

SEXES = list(
  name='sex',
  label='Gender',
  choices=c(
    male='Male',
    female='Female') )

RISKS = list(
  name='risk-groups',
  label='Risk groups',
  choices=c(
    msm="MSM",
    idu="IDU",
    msm_idu="MSM+IDU",
    heterosexual="Heterosexual") )

DIMENSION.VALUES = list(
  age=AGES,
  race=RACES,
  sex=SEXES,
  risk=RISKS)

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

# Support functions 1: FUNCTIONS TO GET THE OPTIONS FOR ARGUMENTS ####

#'@description Get the versions of the model which are available for simulations to come from
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
get.intervention.options <- function(version, location)
{
    
 #   desc = 'line one\nline two\nline 3'
  #  strsplit(desc, split = '\n')
   # data.frame(name='test_name',
    #           label='test_label',
     #          description='test_description')
    
    
  # TODO: @Todd: Might need to include this 'name' bit for my dynamic
  # rendering.
  
    
    sims.names = sims.list()
    sims.locations = get.locations.from.filenames(sims.names)
    sims.names = sims.names[sims.locations==location]
    interventions = get.interventions.from.filenames(sims.names)
    
    df = data.frame()
   
  no.int = list(
    name='no_intervention',
    label='No intervention',
    description='This is the description text for: No intervention',
    target.groups=character(),
    testing.frequency=NA,
    suppressed.proportion=NA,
    prep.coverage=NA,
    intervention.start.year=2021,
    intervention.implemented.year=2022)
  
  int.1 = list(
    name='young_black_msm_testing_1py_0.8_suppressed_0.25_prep',
    label='Young Black MSM testing (1py 0.8suppressed 0.25prep)',
    description='This is the description text for: Young Black MSM testing (1py 0.8suppressed 0.25prep)',
    target.groups='Black MSM <35yo',
    testing.frequency=1,
    suppressed.proportion=0.8,
    prep.coverage=0.25,
    intervention.start.year=2021,
    intervention.implemented.year=2022)
  
  int.2 = list(
    name='all_msm_idu_testing_1py_0.9_suppressed_0.5_prep',
    label='All MSM & IDU testing (1py 0.9suppressed 0.5prep)',
    description='This is the description text for: All MSM & IDU testing (1py 0.9suppressed 0.5prep)',
    target.groups='All MSM and IDU',
    testing.frequency=1,
    suppressed.proportion=0.9,
    prep.coverage=0.5,
    intervention.start.year=2021,
    intervention.implemented.year=2022)
  
  list(
    # 'no_intervention'=no.int,
    'young_black_msm_testing_1py_0.8_suppressed_0.25_prep'=int.1,
    'all_msm_idu_testing_1py_0.9_suppressed_0.5_prep'=int.2)
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
get.dimension.value.options <- function(version, location)
{
  DIMENSION.VALUES
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
  intervention.names)
{
    '1.0_12060_baseline.Rdata'
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
#'@return A list with two values:
#' $plot - a ggplot object
#' $notes - a character vector (which may be empty) of notes
plot.simulations <- function(
  cache,
  # Private meta params
  version,
  # Public params; Selectable in UI
  location,
  intervention.names,
  years,
  data.types,
  facet.by,
  split.by,
  dimension.subsets,  # TODO: Problem? all 4 vals are null
  plot.format,
  # Private params
  show.truth=T,
  #
  plot.interval.coverage=0.95,
  summary.statistic='none',
  summary.statistic.interval.coverage=0.95,
  #
  baseline.color='blue',
  truth.color='green',
  intervention.colors='red',
  plot.interval.alpha=0.2,
  simulation.alpha=0.2,
  simulation.line.size=0.1,
  truth.point.size=3
) {
#  browser()  # TODO: debug
  
  baseline.simset = sims.load('1.0_12060_baseline.Rdata', cache)
    intervention.simsets = NULL
    # plot = do.plot.simulations(baseline.simset,
    
    simsets = list(baseline.simset)
    
    
    #map ages
    age.mapping = names(PRETTY.NAMES$age)
    names(age.mapping) = paste0('age', 1:5)
    if (!is.null(dimension.subsets) && !is.null(dimension.subsets$age))
        dimension.subsets$age = age.mapping[dimension.subsets$age]
    
    if (1==1)
    plot = do.plot.simulations(simsets,
                               
                               years=years,
                               data.types=data.types,
                               facet.by,
                               split.by,
                               dimension.subsets,
                               plot.format='individual.simulations', #for now, going to override the plot formats
                               
                               show.truth=T,
                               
                               plot.interval.coverage=plot.interval.coverage,
                               summary.statistic=summary.statistic,
                               summary.statistic.interval.coverage=summary.statistic.interval.coverage,
                               
                               colors=pal_jama(),
                               
                               plot.interval.alpha=plot.interval.alpha,
                               simulation.alpha=simulation.alpha,
                               simulation.line.size=simulation.line.size,
                               truth.point.size=truth.point.size)
    
    else
    {
        plot = qplot(1:10, 10:1)
        print('putting a test plot for now')
    }
    
  list(
    plot=plot,
    notes=c('test note 1', 'test note 2'))
}

# Tests: TEST CODE ####
# test.config.on = FALSE
# test.config.on = TRUE
if (test.config.on == T) {
  version = names(get.version.options())[1]
  location = names(get.location.options(version))[1]
  interventions = get.intervention.options(version, location)
  
  print('test.me')
  test.me = plot.simulations(
    version=version,
    location=location,
    intervention.names = names(interventions)[1],
    years = get.year.options(version, location),
    data.types = get.data.type.options(version, location)[1:2],
    facet.by = names(get.facet.by.options(version, location))[1],
    split.by = names(get.split.by.options(version, location))[2],
    dimension.subsets=get.dimension.value.options(version, location),
    plot.format = names(get.plot.format.options(version, location))[1],
    plot.interval.coverage=0.95,
    summary.statistic=get.summary.statistic.options(version, location)[1],
    summary.statistic.interval.coverage=0.95,
    baseline.color='blue',
    intervention.colors='red',
    plot.interval.alpha=0.2,
    simulation.alpha=0.2,
    simulation.line.size=0.1,
    show.truth=T,
    truth.point.size=5
  )$plot
  
  View(test.me)
}

# Export ####
model.todd = plot.simulations

# Scratch ####
