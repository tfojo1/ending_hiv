source('code/source_code.R')

SRC.DIRECTORY = file.path(SIMULATIONS.DIR, 'baseline_quick_expanded')
DST.DIRECTORY = file.path(SIMULATIONS.DIR, 'baseline_quick_laart')


source('code/applications/laart/laart_jheem_settings.R')
source('code/applications/laart/laart_parameters.R')
source('code/applications/laart/laart_parameter_mapping.R')

LOCATIONS = TARGET.MSAS[c('Atlanta', 'Baltimore','LA', 'Miami')]

for (loc in LOCATIONS)
{
    print(paste0("Preparing LAART baseline simset for ", msa.names(loc), " (", loc, ")"))
    src.filename = get.full.filename(location=loc, version='ex1.0')
    load(file.path(SRC.DIRECTORY, src.filename))
    simset = prepare.simset.for.intervention(simset, update.version='laart')
    save.simset(simset, full=T, dir=DST.DIRECTORY)
}

