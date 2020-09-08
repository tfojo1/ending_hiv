# Imports ####
# Libraries
# TODO: jheem is a gig. should just be a code
library('jheem')
library('distributions')
library('bayesian.simulations')
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
#   'jheem',
#   repos=NULL,
#   type='source')


# LOAD DATA OBJECTS ####
for (static in static.list())
  static.load(static)
# load('visualization/resources/msa_surveillance.Rdata')
# load('visualization/resources/state_surveillance.Rdata')
# load('visualization/resources/census_totals.Rdata')
# load('visualization/resources/locale_mapping.Rdata')

# More library loads ####


# Source ####
tryCatch({
  # Loading when server is not loaded:
  source('code/systematic_calibration/postprocessing.R')
  # TODO: this need jheem:
  source('code/data_managers/locale_mappings.R')
  
  source('code/data_managers/hiv_surveillance_manager.R')
  source('code/data_managers/census_totals.R')
  source('code/setup/setup_jheem_from_components.R')
  # TODO: this need jheem:
  # source('code/visualization/plot_simulations.R')
}, 
warning=function(w) {}, 
error=function(e) {
  # Loading when server is loaded:
  source('../../code/systematic_calibration/postprocessing.R')
  # TODO: this need jheem:
  source('../../code/data_managers/locale_mappings.R')
  
  source('../../code/data_managers/hiv_surveillance_manager.R')
  source('../../code/data_managers/census_totals.R')
  source('../../code/setup/setup_jheem_from_components.R')
}, 
finally={} )