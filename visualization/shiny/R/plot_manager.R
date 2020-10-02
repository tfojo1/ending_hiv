# Imports ####
# Libraries
library('jheem')  # installable package is in /packages/
library('distributions')  # installable package is in /packages/
library('bayesian.simulations')  # installable package is in /packages/
library('data.table')

source('R/server.utils.R')

# install.packages('tmvtnorm')
# install.packages('matrixcalc')
# install.packages(
#   'packages/distributions_0.1.0.tar.gz',
#   repos=NULL,
#   type='source' )
# install.packages(
#   'packages/bayesian.simulations_0.2.2.tar.gz',
#   repos=NULL,
#   type='source' )

# TODO: Install the final package needed:
# install.packages(
#   'packages/jheem_0.1.0.tar.gz',
#   repos=NULL,
#   type='source')


# LOAD DATA OBJECTS ####
#for (static in static.list())
#  static.load(static)
load('resources/msa_surveillance.Rdata')
load('resources/state_surveillance.Rdata')
load('resources/census_totals.Rdata')
load('resources/locale_mapping.Rdata')

# More library loads ####


# Source ####
source('R/model_code/plot_simulations.R')
source('R/model_code/postprocessing.R')
source('R/model_code/locale_mappings.R')
source('R/model_code/hiv_surveillance_manager.R')
source('R/model_code/census_totals.R')
source('R/model_code/setup_jheem_from_components.R')
source('R/model_code/interpolating.R')
source('R/model_code/file_manager.R')

# TODO: @Todd: Temporarily disabled; files were not located.
# source('R/model_code/intervention_units.R')
# source('R/model_code/target_population.R')
# source('R/model_code/interventions.R')
# source('R/model_code/intervention_presets.R')
 