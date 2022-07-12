
# This file sources all the code and loads all the cached objects
# necessary to run simulations

#-- Libraries --#
library(jheem)
library(bayesian.simulations)
library(ggplot2)

#-- Source Low-Level Helpers --#
source('code/helpers/list_helpers.R')
source('code/helpers/access_helpers.R')

#-- Source Settings --#
source('code/settings_and_files/version_manager.R')
source('code/settings_and_files/systematic_settings.R')
source('code/settings_and_files/file_manager.R')

#-- Source Data Managers --#
load('cached/DEFAULT.LOCALE.MAPPING.Rdata')
load('cached/SUBSTATE.TO.COUNTY.MAPPING.Rdata') # can we get rid of this? maybe

source('code/core_code/data_managers/locale_mappings.R')
source('code/core_code/data_managers/census_manager.R')
source('code/core_code/data_managers/census_totals.R')
source('code/core_code/data_managers/mortality_manager.R')
source('code/core_code/data_managers/natality_manager.R')
source('code/core_code/data_managers/idu_manager.R')
source('code/core_code/data_managers/pairing_manager.R')
source('code/core_code/data_managers/prep_manager_2.R')
source('code/core_code/data_managers/continuum_manager_3.R')
source('code/core_code/data_managers/hiv_surveillance_manager.R')

source('code/calibration/target_msas.R')

#-- Load cached data managers --#
load('cached/msa.surveillance.Rdata')
load('cached/county.surveillance.Rdata')
load('cached/state.surveillance.Rdata')
load('cached/national.surveillance.Rdata')
load('cached/ALL.DATA.MANAGERS.Rdata')   


#-- Set-Up --#
source('code/core_code/meta_model_structures/logit_transformations.R')
source('code/core_code/meta_model_structures/models.R')
source('code/core_code/meta_model_structures/jheem_sim_interface.R')

source('code/core_code/setup/interpolating.R')
source('code/core_code/setup/base_parameters.R')
source('code/core_code/setup/setup_helpers.R')
source('code/core_code/setup/setup_jheem_from_components.R')
source('code/core_code/setup/setup_jheem_components.R')
source('code/core_code/setup/setup_components_for_locale.R')
source('code/core_code/setup/setup_initial_components.R')

#-- Calibration --#
source('code/calibration/parameter_mappings/calibrated_parameters_helpers.R')

source('code/calibration/likelihoods/estimate_cdc_errors.R')
source('code/calibration/likelihoods/likelihoods_2.R')
source('code/calibration/likelihoods/likelihoods_nested_location2.R')
source('code/calibration/likelihoods/likelihood_master.R')

#-- Versions --#
source('code/core_code/meta_model_structures/jheem_settings.R')
source('code/core_code/meta_model_structures/transition_mappings.R')

# Original (EHE)
source('code/applications/ehe/ehe_jheem_settings.R')
source('code/calibration/parameter_mappings/calibrated_parameters_118.R')

# Expanded Continuum
source('code/applications/expanded_continuum/expanded_continuum_jheem_settings.R')
source('code/calibration/parameter_mappings/calibrated_parameters_expanded_2.R')


#-- Systematic --#
source('code/processing//postprocessing.R')
source('code/execution//systematic_calibration.R')
source('code/execution/starting_values/starting_value_generator.R')
source('code/core_code/setup/compression.R')
source('code/core_code/time_text.R') #from commoncode, for printing updates

#-- Interventions --#
source('code/core_code/interventions/target_population.R')
source('code/core_code/interventions/interventions.R')
source('code/core_code/interventions/intervention_units.R')
source('code/core_code/interventions/intervention_static_settings.R')
#source('code/interventions/create_standard_intervention_presets.R')
#source('code/interventions/intervention_presets.R')
source('code/core_code/interventions/intervention_simset_interface.R')
source('code/core_code/interventions/intervention_defaults.R')
source('code/execution/prepare_simset_for_projections.R')
#source('code/interventions/systematic_intervention_sets.R')
source('code/execution/run_systematic_interventions.R')
