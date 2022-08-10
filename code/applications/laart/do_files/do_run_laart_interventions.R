

SRC.DIRECTORY = 'Q:JHEEM/simulations/baseline_quick_laart'
DST.DIRECTORY = 'Q:JHEEM/simulations/quick_laart'

source('code/source_code.R')
source('code/applications/laart/laart_jheem_settings.R')
source('code/applications/laart/laart_parameters.R')
source('code/applications/laart/laart_parameter_mapping.R')
source('code/applications/laart/create_laart_interventions.R')

LOCATIONS = c(BALTIMORE.MSA, LA.MSA, ATLANTA.MSA) #eventually will be = TARGET.MSAS

run.multiple.systematic.interventions(version='laart',
                                      locations=LOCATIONS,
                                      interventions=LAART.INTERVENTIONS,
                                      src.dir = SRC.DIRECTORY,
                                      dst.dir = DST.DIRECTORY)