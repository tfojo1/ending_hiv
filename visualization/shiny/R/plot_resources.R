##------------------------------------------##
##            plot_resources.R              ##
##                                          ##
##   Loads the libraries, source files and  ##
##   data objects needed to run plot code   ##
##------------------------------------------##


##---------------##
##-- LIBRARIES --##
##---------------##

library(jheem)
library(distributions)
library(bayesian.simulations)

library(plotly)
library(data.table)
library(ggsci)


##------------------##
##-- DATA OBJECTS --##
##------------------##

load('resources/msa_surveillance.Rdata')
load('resources/state_surveillance.Rdata')
load('resources/census_totals.Rdata')
load('resources/locale_mapping.Rdata')


##-----------------##
##-- SOURCE CODE --##
##-----------------##

source('R/model_code/plot_simulations.R')
source('R/model_code/postprocessing.R')
source('R/model_code/locale_mappings.R')
source('R/model_code/hiv_surveillance_manager.R')
source('R/model_code/census_totals.R')
source('R/model_code/setup_jheem_from_components.R')
source('R/model_code/interpolating.R')
source('R/model_code/file_manager.R')
source('R/model_code/default_jheem_settings.R')

source('R/model_code/intervention_units.R')
source('R/model_code/target_population.R')
source('R/model_code/interventions.R')
source('R/model_code/intervention_presets.R')
 