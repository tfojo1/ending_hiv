
# This file sources all the code and loads all the cached objects
# necessary to run simulations

#-- Libraries --#
library(jheem)
library(bayesian.simulations)
library(ggplot2)

#-- Source Data Managers --#
load('cached/DEFAULT.LOCALE.MAPPING.Rdata')
load('cached/SUBSTATE.TO.COUNTY.MAPPING.Rdata')

source('code/calibration/version_manager.R')

source('code/data_managers/locale_mappings.R')
source('code/data_managers/census_manager.R')
source('code/data_managers/census_totals.R')
source('code/data_managers/mortality_manager.R')
source('code/data_managers/natality_manager.R')
source('code/data_managers/idu_manager.R')
source('code/data_managers/pairing_manager.R')
source('code/data_managers/prep_manager_2.R')
source('code/data_managers/continuum_manager_2.R')
source('code/data_managers/hiv_surveillance_manager.R')

source('code/targets/target_msas.R')

#-- Load cached data managers --#
load('cached/msa.surveillance.Rdata')
load('cached/county.surveillance.Rdata')
load('cached/state.surveillance.Rdata')
load('cached/national.surveillance.Rdata')
load('cached/ALL.DATA.MANAGERS.Rdata')   


#-- Set-Up --#
source('code/setup/interpolating.R')
source('code/setup/default_jheem_settings.R')
source('code/setup/base_parameters.R')
source('code/setup/setup_helpers.R')
source('code/setup/logit_transformations.R')
source('code/setup/setup_jheem_from_components.R')
source('code/setup/setup_jheem_components.R')
source('code/setup/setup_components_for_locale.R')
source('code/setup/setup_initial_components.R')

#-- Calibration --#
source('code/calibration/calibrated_parameters_113_helpers.R')
source('code/calibration/calibrated_parameters_117g.R')
source('code/estimate_cdc_errors.R')

source('code/calibration/likelihoods_2.R')
source('code/calibration/likelihoods_nested_location.R')
source('code/calibration/likelihood_master.R')

#-- Systematic --#
source('code/systematic_calibration/systematic_settings.R')
source('code/systematic_calibration/postprocessing.R')
source('code/systematic_calibration/file_manager.R')
source('code/systematic_calibration/systematic_calibration.R')
source('code/systematic_calibration/starting_value_generator.R')
source('code/visualization/compression.R')
source('code/time_text.R') #from commoncode

#-- Interventions --#
source('code/interventions/target_population.R')
source('code/interventions/interventions.R')
source('code/interventions/intervention_units.R')
source('code/interventions/intervention_static_settings.R')
source('code/interventions/create_standard_intervention_presets.R')
source('code/interventions/intervention_presets.R')
source('code/interventions/interventions_for_simset.R')
source('code/interventions/systematic_intervention_sets.R')
source('code/interventions/systematic_interventions.R')

#-- Redo Surveillance Manager to get rid of old version stuck somewhere in loaded object --#
source('code/data_managers/hiv_surveillance_manager.R')

