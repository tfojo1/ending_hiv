
RESOURCE.DIR = 'visualization/shiny/resources/'
CODE.DIR = 'visualization/shiny/R/model_code/'
OVERWRITE.RESOURCES = F

# Resources
file.copy('cached/msa.surveillance.Rdata', file.path(RESOURCE.DIR, 'msa_surveillance.Rdata'), overwrite=OVERWRITE.RESOURCES)
file.copy('cached/state.surveillance.Rdata', file.path(RESOURCE.DIR, 'state_surveillance.Rdata'), overwrite=OVERWRITE.RESOURCES)
file.copy('cached/DEFAULT.LOCALE.MAPPING.Rdata', file.path(RESOURCE.DIR, 'locale_mapping.Rdata'), overwrite=OVERWRITE.RESOURCES)
file.copy('cached/census_totals.Rdata', file.path(RESOURCE.DIR, 'census_totals.Rdata'), overwrite=OVERWRITE.RESOURCES)

# Source Files
#file.copy('code/visualization/plot_simulations.R', file.path(CODE.DIR, 'plot_simulations.R'), overwrite = T)
file.copy('code/systematic_calibration/postprocessing.R', file.path(CODE.DIR, 'postprocessing.R'), overwrite = T)
file.copy('code/data_managers/locale_mappings.R', file.path(CODE.DIR, 'locale_mappings.R'), overwrite = T)
file.copy('code/data_managers/hiv_surveillance_manager.R', file.path(CODE.DIR, 'hiv_surveillance_manager.R'), overwrite = T)
file.copy('code/data_managers/census_totals.R', file.path(CODE.DIR, 'census_totals.R'), overwrite = T)
file.copy('code/setup/setup_jheem_from_components.R', file.path(CODE.DIR, 'setup_jheem_from_components.R'), overwrite = T)
file.copy('code/setup/interpolating.R', file.path(CODE.DIR, 'interpolating.R'), overwrite = T)
file.copy('code/systematic_calibration/file_manager.R', file.path(CODE.DIR, 'file_manager.R'), overwrite=T)

file.copy('code/interventions/intervention_units.R', file.path(CODE.DIR, 'intervention_units.R'), overwrite=T)
file.copy('code/interventions/target_population.R', file.path(CODE.DIR, 'target_population.R'), overwrite=T)
file.copy('code/interventions/interventions.R', file.path(CODE.DIR, 'interventions.R'), overwrite=T)
file.copy('code/interventions/intervention_presets.R', file.path(CODE.DIR, 'intervention_presets.R'), overwrite=T)