
source('code/settings_and_files/setup_versions.R')
source('code/core_code/meta_model_structures/jheem_sim_interface.R')

source('code/processing/postprocessing.R')
source('code/processing/outcome_formatting.R')
source('code/processing/generalized_extract_results.R')

source('code/core_code/data_managers/locale_mappings.R')
source('code/core_code/data_managers/hiv_surveillance_manager.R')
source('code/core_code/data_managers/census_totals.R')
source('code/core_code/setup/setup_jheem_from_components.R')
source('code/core_code/setup/interpolating.R')
source('code/core_code/interventions/interventions.R')

library(ggplot2)
library(jheem)
library(bayesian.simulations)

library(data.table)
library(ggsci)
library(plotly)

source('code/processing/visualization/plot_simulations.R')

if (!exists('msa.surveillance'))
    load('cached/msa.surveillance.Rdata')

if (!exists('state.surveillance'))
    load('cached/state.surveillance.Rdata')

if (!exists('county.surveillance'))
    load('cached/county.surveillance.Rdata')

if (!exists('CENSUS.TOTALS'))
    load('cached/census_totals.Rdata')

if (!exists('DEFAULT.LOCALE.MAPPING'))
    load('cached/DEFAULT.LOCALE.MAPPING.Rdata')

DEFAULT.AGES = c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years")
DEFAULT.RACES = c('black', 'hispanic', 'other')
DEFAULT.SEXES = c('male','female')
DEFAULT.RISKS = c('msm', 'idu', 'msm_idu', 'heterosexual')

##-----------------------##
##-- WRAPPER FUNCTIONS --##
##-----------------------##

plot.simulations.total <- function(simsets,
                                   years=2010:2020,
                                   data.types=names(DATA.TYPE.NAMES)[1:5],
                                   dimension.subsets=NULL,
                                   plot.format='individual.simulations',
                                   show.truth=T,
                                   ...)
{
    do.plot.simulations(simsets,
                        years = years,
                        data.types = data.types,
                        facet.by=NULL,
                        split.by=NULL,
                        dimension.subsets=dimension.subsets,
                        plot.format=plot.format,
                        show.truth=show.truth,
                        ...)
}

plot.simulations.flex <- function(simsets,
                                  years=2010:2020,
                                  data.types=c('new','prevalence'),
                                  split.by=NULL,
                                  facet.by=NULL,
                                  ages=DEFAULT.AGES,
                                  races=DEFAULT.RACES,
                                  sexes=DEFAULT.SEXES,
                                  risks=DEFAULT.RISKS,
                                  plot.format='individual.simulations',
                                  show.truth=T,
                                  ...)
{
    do.plot.simulations(simsets,
                        years = years,
                        data.types = data.types,
                        facet.by=facet.by,
                        split.by=split.by,
                        dimension.subsets=list(age=ages,
                                               race=races,
                                               sex=sexes,
                                               risk=risks),
                        plot.format=plot.format,
                        show.truth=show.truth,
                        ...)
}

plot.simulations.race.risk <- function(simsets,
                                       years=2010:2020,
                                       data.types=c('new','prevalence'),
                                       ages=DEFAULT.AGES,
                                       races=DEFAULT.RACES,
                                       sexes=DEFAULT.SEXES,
                                       risks=DEFAULT.RISKS,
                                       plot.format='individual.simulations',
                                       show.truth=T,
                                       ...
)
{
    do.plot.simulations(simsets,
                        years = years,
                        data.types = data.types,
                        facet.by='risk',
                        split.by='race',
                        dimension.subsets=list(age=ages,
                                               race=races,
                                               sex=sexes,
                                               risk=risks),
                        plot.format=plot.format,
                        show.truth=show.truth,
                        ...)
}