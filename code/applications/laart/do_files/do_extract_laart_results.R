 


source('code/source_code.R')

SRC.DIRECTORY = file.path(SIMULATIONS.DIR, 'quick_laart')
DST.DIRECTORY = file.path(RESULTS.DIR, 'laart')

source('code/processing/outcome_formatting.R')
source('code/processing/generalized_extract_results.R')

source('code/applications/laart/laart_jheem_settings.R')
source('code/applications/laart/laart_parameters.R')
source('code/applications/laart/laart_parameter_mapping.R')
source('code/applications/laart/laart_interventions.R')

LOCATIONS = c(BALTIMORE.MSA, LA.MSA, ATLANTA.MSA)[1] #eventually will be = TARGET.MSAS

# extract top line outcomes
top.line.results = generalized.extract.results(dir = SRC.DIRECTORY,
                                               version = 'laart',
                                               locations = LOCATIONS,
                                               interventions = LAART.INTERVENTIONS,
                                               outcomes = c('incidence','new','prevalence','suppression'),
                                               years=2020:2035)

# extract the number of PWH in each continuum compartment
prevalence.by.continuum = generalized.extract.results(dir = SRC.DIRECTORY,
                                                      version = 'laart',
                                                      locations = LOCATIONS,
                                                      interventions = LAART.INTERVENTIONS,
                                                      outcomes = c('hiv.positive.population'),
                                                      keep.dimensions = c('year','continuum'),
                                                      years=2020:2035)

#get parameters
filename = get.simset.filename(location=LOCATIONS[1], intervention.code='noint', version='laart')
load(file.path(SRC.DIRECTORY, LOCATIONS[1], filename))
parameter.names = get.projection.prarameters.distribution.for.version('laart')@var.names
parameters = simset@parameters[,parameter.names]

# save
if (!dir.exists(DST.DIRECTORY))
    dir.create(DST.DIRECTORY, recursive = T)
save(top.line.results, prevalence.by.continuum, parameters,
     file=file.path(DST.DIRECTORY, 'results.Rdata'))
