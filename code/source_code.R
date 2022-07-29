
# This file sources all the code and loads all the cached objects
# necessary to run simulations

#-- Libraries --#

library(ggplot2)
library(Rcpp)
source('code/core_code/data_managers/package_manager.R')
load.packages(packages.to.load = c('jheem',
                                   'distributions',
                                   'bayesian.simulations'))

#-- Source Low-Level Helpers --#
sourceCpp('code/helpers/misc_helpers.cpp')
source('code/helpers/list_helpers.R')
sourceCpp('code/helpers/access_helpers.cpp')
source('code/helpers/access_helpers.R')
source('code/helpers/distribution_helpers.R')

#-- Source Settings --#
source('code/settings_and_files/systematic_settings.R')
source('code/settings_and_files/file_manager.R')

#-- Load cached data managers --#
source('code/core_code/data_managers/cache_manager.R')
load.elements.from.cache(elements.to.load = c('msa.surveillance',
                                              'county.surveillance',
                                              'state.surveillance',
                                              'national.surveillance',
                                              'ALL.DATA.MANAGERS',
                                              'DEFAULT.LOCALE.MAPPING')
                         )

#-- Source Data Managers --#
source('code/core_code/data_managers/locale_mappings.R')
source('code/core_code/data_managers/census_manager.R')
source('code/core_code/data_managers/census_totals.R')
source('code/core_code/data_managers/mortality_manager.R')
source('code/core_code/data_managers/natality_manager.R')
source('code/core_code/data_managers/idu_manager.R')
source('code/core_code/data_managers/pairing_manager.R')
source('code/core_code/data_managers/prep_manager_2.R')
source('code/core_code/data_managers/continuum_manager_3.R')
source('code/core_code/data_managers/comorbidities_manager.R')
source('code/core_code/data_managers/hiv_surveillance_manager.R')

source('code/calibration/target_msas.R')

#-- Set-Up --#
source('code/core_code/meta_model_structures/jheem_sim_interface.R')
source('code/core_code/meta_model_structures/logit_transformations.R')
sourceCpp('code/core_code/meta_model_structures/models.cpp')
source('code/core_code/meta_model_structures/models.R')

source('code/core_code/setup/interpolating.R')
source('code/core_code/setup/base_parameters.R')
source('code/core_code/setup/setup_helpers.R')
source('code/core_code/setup/setup_jheem_from_components.R')
source('code/core_code/setup/setup_jheem_components.R')
source('code/core_code/setup/setup_components_for_locale.R')
source('code/core_code/setup/setup_initial_components.R')

#-- Calibration --#
sourceCpp('code/calibration/likelihoods/likelihoods.cpp')
source('code/calibration/likelihoods/estimate_cdc_errors.R')
source('code/calibration/likelihoods/likelihoods_2.R')
source('code/calibration/likelihoods/likelihoods_nested_location2.R')
source('code/calibration/likelihoods/likelihood_master.R')

#-- Versions --#
source('code/settings_and_files/setup_versions.R')

#-- Systematic --#
source('code/processing//postprocessing.R')
source('code/execution//systematic_calibration.R')
source('code/execution/starting_values/starting_value_generator.R')
source('code/core_code/setup/compression.R')
source('code/core_code/time_text.R') #from commoncode, for printing updates

#-- Interventions --#
source('code/core_code/interventions/target_population2.R')
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
