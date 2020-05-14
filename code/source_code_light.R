
# This file sources all the code and loads all the cached objects
# necessary to run simulations

#-- Libraries --#
library(jheem)
library(bayesian.simulations)
library(ggplot2)

#-- Source Data Managers --#
load('cached/DEFAULT.LOCALE.MAPPING.Rdata')
load('cached/SUBSTATE.TO.COUNTY.MAPPING.Rdata')

source('code/data_managers/locale_mappings.R')
source('code/data_managers/census_manager.R')
source('code/data_managers/census_totals.R')
source('code/data_managers/mortality_manager.R')
source('code/data_managers/natality_manager.R')
source('code/data_managers/idu_manager.R')
source('code/data_managers/pairing_manager.R')
source('code/data_managers/prep_manager.R')
source('code/data_managers/continuum_manager.R')
source('code/data_managers/hiv_surveillance_manager.R')

#-- Load cached data managers --#
load('cached/msa.surveillance.Rdata')
load('cached/state.surveillance.Rdata')
load('cached/ALL.DATA.MANAGERS.Rdata')


#-- Set-Up --#
source('code/setup/default_jheem_settings.R')
source('code/setup/base_parameters.R')
source('code/setup/setup_helpers.R')
source('code/setup/smoothing_proportions.R')
source('code/setup/logit_transformations.R')
source('code/setup/setup_jheem_from_components.R')
source('code/setup/setup_jheem_components.R')
source('code/setup/setup_components_for_locale.R')
source('code/setup/setup_initial_components.R')

#-- Calibration --#
source('code/calibration/calibrated_parameters_68_helpers.R')
source('code/calibration/calibrated_parameters_68.R')
source('code/estimate_cdc_errors.R')
source('code/calibration/likelihoods.R')

#-- Plots --#
source('code/plots.R')

#-- Interventions --#
source('code/interventions/interventions_setup.R')
source('code/interventions/interventions_for_simset.R')

#-- Redo Surveillance Manager to get rid of old version stuck somewhere in loaded object --#
source('code/data_managers/hiv_surveillance_manager.R')

