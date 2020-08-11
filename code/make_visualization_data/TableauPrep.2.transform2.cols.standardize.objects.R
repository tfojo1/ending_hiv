'Standardize cols'

# Settings and Imports ####
.sep = '/'
# options(stringsAsFactors=FALSE)

library('here')

# Source, input, and output files
.src.dir.path = paste(here(), 'code', 'make_visualization_data', sep=.sep)
.src.objects.filename = 'TableauPrep.2.transform1.widen.objects.R'
.src.objects.path = paste(.src.dir.path, .src.objects.filename, sep=.sep)
source(.src.objects.path)

data.sim.transform2.in = data.sim.transform1.out
data.epi.transform2.in = data.epi.in
rm(data.sim.transform1.out)
rm(data.epi.in)
# View(data.sim.transform2.in)
# View(data.epi.transform2.in)

# Config variables ####
.defaultNAToken.asNA = NA
.defaultNAToken.asInt = -77
.defaultNAToken = .defaultNAToken.asNA

.defaultNAToken.byType = list(
  integer=.defaultNAToken,
  datetime=.defaultNAToken,
  string=.defaultNAToken,
  float=.defaultNAToken)

.data.cols.inputOutputMapping.existing = list(  # list[str: str]
  year=list(
    renameTo='pos.time.year',
    required=TRUE,
    defaultValues.common=.defaultNAToken.byType[['datetime']]),
  location_code=list(
    renameTo='pos.location.id',
    required=TRUE,
    defaultValues.common=.defaultNAToken.byType[['integer']]),
  location_id=list(
    renameTo='pos.location.id',
    required=TRUE,
    defaultValues.common=-.defaultNAToken.byType[['integer']]),
  data_type=list(
    renameTo='label.data.measureName',
    required=TRUE,
    defaultValues.common=.defaultNAToken.byType[['string']]),
  intervention_id=list(
    renameTo='label.intervention.id',
    required=TRUE,
    defaultValues.common=1),
  intervention_code=list(
    renameTo='label.intervention.id',
    required=TRUE,
    defaultValues.common=1),
  simulation=list(
    renameTo='label.simulationNumber',
    required=TRUE,
    defaultValues.common=.defaultNAToken.byType[['integer']]),
  stratum=list(
    renameTo='label.person.stratum.id',
    required=TRUE,
    defaultValues.common=.defaultNAToken.byType[['integer']]),
  age=list(
    renameTo='label.person.stratum.age',
    required=TRUE,
    defaultValues.common=.defaultNAToken.byType[['string']]),
  race=list(
    renameTo='label.person.stratum.race',
    required=TRUE,
    defaultValues.common=.defaultNAToken.byType[['string']]),
  sex=list(
    renameTo='label.person.stratum.sex',
    required=TRUE,
    defaultValues.common=.defaultNAToken.byType[['string']]),
  risk=list(
    renameTo='label.person.stratum.risk',
    required=TRUE,
    defaultValues.common=.defaultNAToken.byType[['string']]),
  value=list(
    renameTo='value.mean',
    required=TRUE,
    defaultValues.common=.defaultNAToken.byType[['float']]),
  mean=list(
    renameTo='value.mean',
    required=TRUE,
    defaultValues.common=.defaultNAToken.byType[['float']]),
  median=list(
    renameTo='value.median',
    required=TRUE,
    defaultValues.common=.defaultNAToken.byType[['float']]),
  interval_lower_95=list(
    renameTo='value.interval_lower_95',
    required=TRUE,
    defaultValues.common=.defaultNAToken.byType[['float']]),
  interval_upper_95=list(
    renameTo='value.interval_upper_95',
    required=TRUE,
    defaultValues.common=.defaultNAToken.byType[['float']]),
  interval_lower_50=list(
    renameTo='value.interval_lower_50',
    required=TRUE,
    defaultValues.common=.defaultNAToken.byType[['float']]),
  interval_upper_50=list(
    renameTo='value.interval_upper_50',
    required=TRUE,
    defaultValues.common=.defaultNAToken.byType[['float']]))

.data.cols.inputOutputMapping.toMove = list(
  source=list(
    renameTo='label.data.source2',
    required=TRUE,
    defaultValues.common=.defaultNAToken.byType[['string']]))

.data.cols.inputOutputMapping.toAdd = list(
  label.data.source=list(
    renameTo='label.data.source',
    required=TRUE,
    defaultValues.common=.defaultNAToken.byType[['string']]))

.data.cols.inputOutputMapping.proto = c(
  .data.cols.inputOutputMapping.existing,
  .data.cols.inputOutputMapping.toMove,
  .data.cols.inputOutputMapping.toAdd)

# Add incidental new cols to the overall mapping, just in case already in
.data.cols.inputOutputMapping = .data.cols.inputOutputMapping.proto
for (col in names(.data.cols.inputOutputMapping)) {
  col.specs = .data.cols.inputOutputMapping[[col]]
  .data.cols.inputOutputMapping[[col.specs[['renameTo']]]] = col.specs
}

# cols.names.source.variations = c('source', )
.data.cols.inputOutputMapping[[
  'label.data.source']][['defaultValues.sim']] = 'sim'
.data.cols.inputOutputMapping[[
  'label.data.source']][['defaultValues.epi']] = 'epi'
.data.cols.inputOutputMapping[[
  'label.data.source2']][['defaultValues.sim']] = 'abm'

# Transform ####
data.sim.standardized = TableauPrep.cols.standardize(
  df=data.sim.transform2.in,  # data.frame
  whichDataset='sim',  # char[epi||sim]
  inputOutputMapping=.data.cols.inputOutputMapping)  # list[str: str]
data.epi.standardized = TableauPrep.cols.standardize(
  df=data.epi.transform2.in,  # data.frame
  whichDataset='epi',  # char[epi||sim]
  inputOutputMapping=.data.cols.inputOutputMapping)  # list[str: str]
# View(data.sim.standardized)
# View(data.epi.standardized)

# Export
data.sim.transform2.out = data.sim.standardized
data.epi.transform2.out = data.epi.standardized
# View(data.sim.transform2.out)
# View(data.epi.transform2.out)
rm(data.sim.transform2.in)
rm(data.epi.transform2.in)
rm(data.sim.standardized)
rm(data.epi.standardized)
