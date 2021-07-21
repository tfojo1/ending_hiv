source('code/systematic_calibration/postprocessing.R')
source('code/data_managers/locale_mappings.R')
source('code/data_managers/hiv_surveillance_manager.R')
source('code/data_managers/census_totals.R')
source('code/setup/setup_jheem_from_components.R')
source('code/setup/interpolating.R')
source('code/interventions/interventions.R')

library(ggplot2)
library(jheem)
library(bayesian.simulations)

library(data.table)
library(ggsci)
library(plotly)

source('code/visualization/plot_simulations.R')

if (!exists('msa.surveillance'))
    load('visualization/shiny/resources/msa_surveillance.Rdata')

if (!exists('state.surveillance'))
    load('visualization/shiny/resources/state_surveillance.Rdata')

if (!exists('CENSUS.TOTALS'))
    CENSUS.TOTALS = ALL.DATA.MANAGERS$census.totals
#    load('visualization/shiny/resources/census_totals.Rdata')

if (!exists('DEFAULT.LOCALE.MAPPING'))
    load('visualization/shiny/resources/locale_mapping.Rdata')

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