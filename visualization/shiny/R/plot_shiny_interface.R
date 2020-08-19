# EndingHIV model ####
'# EndingHIV Model::RShiny Interface
'

# TODO @jef: Need to replace boilerplate func 'model' object with a matching one
# from EndingHIV. I do not believe that the main plot function here will serve
# that purpose without repurposing. So I might create a placeholder 'model' object
# out of parts of that function, and/or with some of my own modifications.
# Will still be able to use the body of the plot function for actual plotting.
# -jef 2020.08.10

# Constants (TEMPORARY) ####
# @jef: The constants below are just to let this file be self-contained
# eventually they will be abstracted into a different file structure, so 
# do not assume these will be here. -tj
# @tj: Understood! -jef 

# TODO @jef: Use these constants as a quick face-validation reference for UI
AGES = c(
  age1='13-24 years',
  age2='25-34 years',
  age3='35-44 years',
  age4='45-54 years',
  age5='55+ years')

RACES = c(
  black="Black",
  hispanic="Hispanic",
  other="Other")

SEXES = c(
  male='Male',
  female='Female')

RISKS = c(
  msm="MSM",
  idu="IDU",
  msm_idu="MSM+IDU",
  heterosexual="Heterosexual")

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
  new='Reported Diagnoses',
  prevalence='Estimated Prevalence',
  mortality='HIV Mortality',
  suppression='Viral HIV Suppression',
  awareness="Awareness of HIV Diagnosis",
  incidence="HIV Incidence")

# Support functions 1: FUNCTIONS TO GET THE OPTIONS FOR ARGUMENTS ####

#'@description Get the versions of the model which are available for simulations to come from
#'@param version The indicator for the version of the model (corresponds to one of the values of names(get.version.options))
#'@return A named character vector. The values of the vector are 'displayable' names of places; the names of the vector are the location codes to be passed to plot.simulations
get.version.options <- function()
{
  c('1.0'='1.0')
}

#'@description Get the locations for which simulations are available for a given model version
#'@param version The indicator for the version of the model. Corresponds to one of the values of names(get.version.options)
#'@return A named character vector. The values of the vector are 'displayable' names of places; the names of the vector are the location codes to be passed to plot.simulations
get.location.options <- function(version)
{
  c('12580'='Baltimore-Columbia-Towson, MD',
    '33100'='Miami-Fort Lauderdale-Pompano Beach, FL')
}

#'@description Get the potential formats for plots
#'@param version The indicator for the version of the model. Corresponds to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of names(get.location.options(version))
#'@return A named character vector. The values of the vector are 'displayable' descriptions of plot format; the names of the vector are the location codes to be passed to plot.simulations
get.plot.format.options <- function(version, location)
{
  c(mean.and.interval='Mean and Prediction Interval',
    median.and.interval='Median and Prediction Interval',
    individual.simulations='Individual Simulations')
}

#'@description Get the data types available for plotting for a location
#'@param version The indicator for the version of the model. Corresponds to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of names(get.location.options(version))
#'@return A named vector of dimension names. The values of the vector are 'displayable' descriptions of data types; the names of the vector are the codes to be passed to plot.simulations
get.data.type.options <- function(version, location)
{
  DATA.TYPES
}

#'@description Get the years available for plotting for a location
#'@param version The indicator for the version of the model. Corresponds to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of names(get.location.options(version))
#'@return A numeric vector of potential years for the location
get.year.options <- function(version, location)
{
  1970:2030
}

#'@description Get the potential names of dimensions by which a plot can be 'facetted' (split into separate panels)
#'@param version The indicator for the version of the model. Corresponds to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of names(get.location.options(version))
#'@return A named vector of dimension names. The values of the vector are 'displayable' options; the names of the vector are the codes to be passed to plot.simulations
get.facet.by.options <- function(version, location)
{
  DIMENSIONS
}

#'@description Get the potential names of dimensions by which a plot can be 'split' (with a separate line on each panel)
#'@param version The indicator for the version of the model. Corresponds to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of names(get.location.options(version))
#'@return A named vector of dimension names. The values of the vector are 'displayable' options; the names of the vector are the codes to be passed to plot.simulations
get.split.by.options <- function(version, location)
{
  DIMENSIONS
}

#'@description Get the possible interventions that can be plotted for a location
#'@param version The indicator for the version of the model. Corresponds to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of names(get.location.options(version))
#'@return A named list of intervention objects. The names of the list are what is to be passed to plot.simulations
# An intervention object is a list with the following components:
# $target.groups - a text description of which demographic subgroups targeted on the intervention
# $testing.frequency - the averagte number of tests per year for targeted subgroups. May be NA
# $suppressed.proportion - the fraction (0 to 1) of people with HIV in targeted subgroups who have suppressed viral loads. May be NA
# $prep.coverage - the fraction (0 to 1) of people without HIV in targeted subgroups who are prescribed and adherent to PrEP. May be NA
# $intervention.start.year - the year at which the interventions begin to ramp up
# $intervention.implemented.year - the year at which the interventions are fully ramped up
get.intervention.options <- function(version, location)
{
  no.int = list(target.groups=character(),
                testing.frequency=NA,
                suppressed.proportion=NA,
                prep.coverage=NA,
                intervention.start.year=2021,
                intervention.implemented.year=2022)
  
  int.1 = list(target.groups='Black MSM <35yo',
               testing.frequency=1,
               suppressed.proportion=0.8,
               prep.coverage=0.25,
               intervention.start.year=2021,
               intervention.implemented.year=2022)
  
  int.2 = list(target.groups='All MSM and IDU',
               testing.frequency=1,
               suppressed.proportion=0.9,
               prep.coverage=0.5,
               intervention.start.year=2021,
               intervention.implemented.year=2022)
  
  list('no_intervention'=no.int,
       'young_black_msm_testing_1py_0.8_suppressed_0.25_prep'=int.1,
       'all_msm_idu_testing_1py_0.9_suppressed_0.5_prep'=int.2)
  
}

#'@description Get the potential values (which can be subsetted) for each dimension
#'@param version The indicator for the version of the model. Corresponds to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of names(get.location.options(version))
#'@return A named list of character vectors. 
#' The names of the list correspond to names(get.facet.by.options()) or names(get.split.by.options())
#' Each element in the list is a named character vector of possible values. 
#'  names(get.dimension.value.options()[[d]]) correspond to the values of get.facet.by.options() or get.split.by.options(), and should be passed to plot.simulations() function via the dimension.subsets argument
#'  the values of get.dimension.value.options()[[d]] are 'displayable' value names
get.dimension.value.options <- function(version, location)
{
  DIMENSION.VALUES
}

#'@description Get the potential summary statistics to display
#'@param version The indicator for the version of the model. Corresponds to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of names(get.location.options(version))
#'@return A named character vector. The values of the vector are 'displayable' options; the names of the vector are the codes to be passed to plot.simulations
get.summary.statistic.options <- function(version, location)
{
  c(none='None',
    incidence.reduction='Reduction in Incidence')
}

# Support functions 2: FUNCTIONS TO DEFINE WHICH OPTIONS APPLY ####

#'@description Whether an "Interval Coverage" quantity applies for a given plot format
#'@param plot.format The name of a plot format. One of names(get.plot.format.options())
#'@return A boolean indicator of whether an "Interval Coverage" quantity applies for the given plot format
plot.interval.coverage.applies.to.plot.format <- function(plot.format)
{
  c(mean.and.interval=T,
    median.and.interval=T,
    individual.simulations=F)
}

# Tests: TEST CODE ####
if (1==2)
{
  version = names(get.version.options())[1]
  location = names(get.location.options(version))[1]
  interventions = get.intervention.options(version, location)
  
  plot.simulations(
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
}

# Main Function: THE PLOT FUNCTION ####

#'@param description The function that actually generates plots
#'
#' THE FUNDAMENTAL ARGUMENTS THAT DEFINE THE PLOT
#'@param version The indicator for the version of the model. Corresponds to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of names(get.location.options(version))
#'@param intervention.name
#'@param years The numeric years to plot
#'@param data.types The names of 
#'@param facet.by [char[]] The names of dimensions according to which to 'facet' (a separate panel for each) - this should be a subset of names(get.facet.by.options(location))
#'@param split.by [char[]] The names of dimensions according which to split the plot (a separate line on each panel) - this should be a subset of names(get.split.by.options(location))
#'@param dimension.subsets [type] A named list of which values for each dimension to include in the plot. The names of the list are the named dimensions as given by names(get.dimension.value.options()), and the values for dimension d must be a subset of get.dimension.values()[[d]]
#'@param plot.format [type] The character string indicating the format for the plot - this should be one of names(get.plot.format.options(location))
#'@param show.truth [type] Whether to include "truth" (known from epi surveillance) in the plot
#'
#' ARGUMENTS THAT DEFINE STATISTICS
#'@param plot.interval.coverage A fraction (0 to 1) which the plotted prediction interval should cover (only applies to some plot formats)
#'@param summary.statistic The name of the summary statistic to show. Should be one of names(get.summary.statistic.options(location))
#'@param summary.statistic.interval.coverage A fraction (0 to 1) which the interval for the summary statistic should cover
#'
#' THE STYLE ARGUMENTS
#'@param baseline.color The color with which to plots the baseline (pre-intervention) simulations
#'@param truth.color The color with which to plot truth (when available)
#'@param intervention.colors A named vector of colors, one for each intervention in intervention.names. The names of the vector should be intervention.names
#'@param plot.interval.alpha The alpha value (opacity) for prediction intervals (if shown)
#'@param simulation.alpha The alpha value (opacity) for individual simulation plots
#'@param simulation.line.size The line size for plotted simulations
#'@param truth.point.size The point size for plotted 'truth' (epi surveillance) values#'
#'
#'@return A list with two values:
#' $plot - a ggplot object
#' $notes - a character vector (which may be empty) of notes
plot.simulations <- function(
  version,
  location,
  intervention.names,
  years,
  data.types,
  facet.by,
  split.by,
  dimension.subsets,
  plot.format,
  #
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
  truth.point.size=5
) {
  dimensions = unique(c(facet.by, split.by))  # char[]
  dimension.values = dimension.subsets
  
  #This is just a temporary hack to give us a test plot
  base.df = NULL
  for (d in 1:length(data.types)) {
    for (i in 1:length(intervention.names)) {
      for (year in years) {
        # browser()
        base.df = rbind(
          base.df,
          suppressWarnings(data.frame(
            data_type=data.types[d],
            intervention=names(intervention.names[i]),
            year=year,
            value=100 * d + 10 * i + 2 - 2*(1:length(year))
          ))
        )  
    }}}
  
  df = base.df
  for (d in dimensions) {
    base.df = df
    df = NULL
    for (i in 1:length(dimension.values[[d]])) {
      one.df = base.df
      one.df[,d] = dimension.values[[d]][i]
      one.df$value = one.df$value + 20*i - 20
      df = rbind(df, one.df) }}
  
  if (length(split.by)==0)
    df$split.by='all'
  else if (length(split.by)==1)
    df$split.by=paste0(split.by, '=', df[,split.by])
  else {
    # TODO: Error in `[.data.frame`(df, , split.by[1]) : undefined columns selected
    browser()
    
    df$split.by=paste0(split.by[1], '=', df[,split.by[1]])
    for (split in split.by[-1])
      df$split.by = paste0(df$split.by, ', ', split, '=', df[,split]) }
  
  facet.formula = as.formula(
    paste0('~data_type + ', paste0(facet.by, collapse='+')))
  
  plot = ggplot(
    df, aes(year, value, color=intervention, group=split.by)) + 
    geom_line() +
    facet_wrap(facet.formula)
  
  # Return: List[ggplot(), char[]] ####
  list(
    plot=plot,
    notes=c('test note 1', 'test note 2'))
}

# Boilerplate Model ####
model.boilerplate <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {

    # ODEs
    # On campus students
    dS_on <- -((beta_student_to_student*(I_off+I_on) + (beta_on_to_on*I_on) + (beta_saf*I_saf))*S_on)/N - community*S_on  - testing*(1-p_asympt_stu)*I_on*sensitivity*(contacts-R0_on_to_on-R0_student_to_student)*p_contacts_reached + 1/isolation*Q_on*((contacts-R0_on_to_on-R0_student_to_student)/contacts)
    dE_on <-  ((beta_student_to_student*(I_off+I_on) + (beta_on_to_on*I_on) + (beta_saf*I_saf))*S_on)/N + community*S_on - 1/latent*E_on  - screening*E_on*sensitivity - testing*(1-p_asympt_stu)*I_on*sensitivity*(R0_on_to_on+R0_student_to_student)*p_contacts_reached #last term represents contract tracing
    dI_on <- 1/latent*E_on - testing*(1-p_asympt_stu)*sensitivity*I_on - 1/infectious*I_on - screening*I_on*sensitivity
    dR_on <- 1/infectious*I_on + 1/isolation*P_on + 1/isolation*Q_on*((R0_on_to_on+R0_student_to_student)/contacts)

    dP_on <- testing*(1-p_asympt_stu)*sensitivity*I_on + screening*(E_on+I_on)*sensitivity - 1/isolation*P_on
    dQ_on <- testing*(1-p_asympt_stu)*I_on*sensitivity*contacts*p_contacts_reached  - 1/isolation*Q_on

    dIcum_on <- (1-p_asympt_stu)*((beta_student_to_student*(I_off+I_on) + (beta_on_to_on*I_on) + (beta_saf*I_saf))*S_on)/N
    dPcum_on <- testing*(1-p_asympt_stu)*I_on*sensitivity + screening*(E_on+I_on)*sensitivity
    dQcum_on <- testing*(1-p_asympt_stu)*I_on*sensitivity*contacts
    dHcum_on <- ((1-p_asympt_stu)*((beta_student_to_student*(I_off+I_on) + (beta_on_to_on*I_on) + (beta_saf*I_saf))*S_on)/N)*p_hosp_stu
    dDcum_on <- ((1-p_asympt_stu)*((beta_student_to_student*(I_off+I_on) + (beta_on_to_on*I_on) + (beta_saf*I_saf))*S_on)/N)*p_death_stu

    # Off campus students
    dS_off <- -((beta_student_to_student*(I_off+I_on)+(beta_saf*I_saf))*S_off)/N - community*S_off - testing*(1-p_asympt_stu)*I_off*sensitivity*(contacts - R0_student_to_student)*p_contacts_reached + 1/isolation*Q_off*((contacts-R0_student_to_student)/contacts)
    dE_off <-  ((beta_student_to_student*(I_off+I_on)+(beta_saf*I_saf))*S_off)/N + community*S_off - (1/latent)*E_off - screening*E_off*sensitivity - testing*(1-p_asympt_stu)*I_off*sensitivity*(R0_student_to_student)*p_contacts_reached
    dI_off <- 1/latent*E_off - testing*(1-p_asympt_stu)*I_off*sensitivity - 1/infectious*I_off - screening*I_off*sensitivity
    dR_off <- 1/infectious*I_off + 1/isolation*P_off + 1/isolation*Q_off*(R0_student_to_student/contacts)

    dP_off <- testing*(1-p_asympt_stu)*I_off*sensitivity - 1/isolation*P_off + screening*(E_off+I_off)*sensitivity
    dQ_off <- testing*(1-p_asympt_stu)*I_off*sensitivity*contacts*p_contacts_reached - 1/isolation*Q_off

    dIcum_off = (1-p_asympt_stu)*((beta_student_to_student*(I_off+I_on)+(beta_saf*I_saf))*S_off)/N
    dPcum_off <- testing*(1-p_asympt_stu)*I_off*sensitivity + screening*(E_off+I_off)*sensitivity
    dQcum_off <-testing*(1-p_asympt_stu)*I_off*sensitivity*contacts
    dHcum_off <- ((1-p_asympt_stu)*((beta_student_to_student*(I_off+I_on)+(beta_saf*I_saf))*S_off)/N)*p_hosp_stu
    dDcum_off <- ((1-p_asympt_stu)*((beta_student_to_student*(I_off+I_on)+(beta_saf*I_saf))*S_off)/N)*p_death_stu



    # Staff and faculty
    dS_saf <- -beta_saf*(I_on+I_off+I_saf)*S_saf/N - community*S_saf - testing*(1-p_asympt_saf)*I_saf*sensitivity*(contacts-R0_saf)*p_contacts_reached + 1/isolation*Q_saf*((contacts-R0_saf)/contacts)
    dE_saf <-  beta_saf*(I_on+I_off+I_saf)*S_saf/N + community*S_saf - 1/latent*E_saf - screening*E_saf*sensitivity - testing*(1-p_asympt_saf)*I_saf*sensitivity*(R0_saf)*p_contacts_reached
    dI_saf <- 1/latent*E_saf - testing*(1-p_asympt_saf)*I_saf*sensitivity- 1/infectious*I_saf - screening*I_saf*sensitivity
    dR_saf <- 1/infectious*I_saf + 1/isolation*P_saf + 1/isolation*Q_saf*(R0_saf/contacts)

    dP_saf <- testing*(1-p_asympt_saf)*I_saf*sensitivity - 1/isolation*P_saf + screening*(E_saf+I_saf)*sensitivity
    dQ_saf <- testing*(1-p_asympt_saf)*I_saf*sensitivity*contacts*p_contacts_reached - 1/isolation*Q_saf

    dIcum_saf = (1-p_asympt_saf)*beta_saf*(I_on+I_off+I_saf)*S_saf/N
    dPcum_saf <- testing*(1-p_asympt_saf)*I_saf*sensitivity  + screening*(E_saf+I_saf)*sensitivity
    dHcum_saf <- ((1-p_asympt_saf)*beta_saf*(I_on+I_off+I_saf)*S_saf/N)*p_hosp_saf
    dDcum_saf <- ((1-p_asympt_saf)*beta_saf*(I_on+I_off+I_saf)*S_saf/N)*p_death_saf

    dTest <- N*screening + ((I_on+I_off+I_saf)*testing) + N*ili*ifelse(testing > 0, 1, 0)

    state_list <- c(dS_on,dE_on,dI_on,dP_on,dR_on,dIcum_on, dPcum_on, dQ_on, dQcum_on, dHcum_on, dDcum_on,
                    dS_off,dE_off,dI_off,dP_off,dR_off,dIcum_off, dPcum_off, dQ_off, dQcum_off, dHcum_off, dDcum_off,
                    dS_saf,dE_saf,dI_saf,dP_saf,dR_saf,dIcum_saf,dPcum_saf, dQ_saf, dHcum_saf, dDcum_saf, dTest)
    out <- list(state_list)
    return(out)
  })
}

# Export ####
model.todd = plot.simulations  # TODO @jef: is this true?
model.boilerplate = model.boilerplate
model = model.boilerplate
# Scratch ####
