'Join data and tables'

# Settings and Imports ####
.sep = '/'
# options(stringsAsFactors=FALSE)

library('dplyr')
library('here')

# Source, input, and output files
.src.dir.path = paste(here(), 'code', 'make_visualization_data', sep=.sep)
.src.objects.filename = 'TableauPrep.2.transform2.cols.standardize.objects.R'
.src.objects.path = paste(.src.dir.path, .src.objects.filename, sep=.sep)
source(.src.objects.path)

data.sim.transform3.in = data.sim.transform2.out  # data.frame
data.epi.transform3.in = data.epi.transform2.out  # data.frame
.defaultNAToken.byType = .defaultNAToken.byType  # list
rm(data.sim.transform2.out)
rm(data.epi.transform2.out)
# View(data.sim.transform3.in)
# View(data.epi.transform3.in)

.tables.remappings = list(
  code='id',
  stratification='id')

table.interventions2 = TableauPrep.tables.cols.standardize(
  df=table.interventions,
  cols.prefixes.namespace='label',
  cols.affixes.table.name='intervention',
  remappings=.tables.remappings)
table.locations2 = TableauPrep.tables.cols.standardize(
  df=table.locations,
  cols.prefixes.namespace='pos',
  cols.affixes.table.name='location',
  remappings=.tables.remappings)
table.strata2 = TableauPrep.tables.cols.standardize(
  df=table.strata,
  cols.prefixes.namespace='label',
  cols.affixes.table.name='person.stratum',
  remappings=.tables.remappings)

# Config variables ####
.join.fieldList.df_and_keys = list(
  'interventions'=list(
    # 'primaryKey'='id',
    'primaryKey'='label.intervention.id',
    'foreignKey'='label.intervention.id',
    'df'=table.interventions2,
    'join'=TRUE),
  'locations'=list(
    # 'primaryKey'='code',
    'primaryKey'='pos.location.id',
    'foreignKey'='pos.location.id',
    'df'=table.locations2,
    'join'=TRUE),
  'strata'=list(
    # 'primaryKey'='stratification',
    'primaryKey'='label.person.stratum.id',
    'foreignKey'='label.person.stratum.id',
    'df'=table.strata2,
    'join'=TRUE))

# TODO: Make sure resulting columns and values are good. 
# there may be an issue here. is the the step i need to fix?
# 1. i'm seeing 'abm' in source2 of epi. And intervention 0 instead of 1.
# Transform: Join
data.sim.joined = TableauPrep.joinTables.all(
  df=data.sim.transform3.in,
  fields.df_and_keys=.join.fieldList.df_and_keys,
  naTokenList=.defaultNAToken.byType)
data.epi.joined = TableauPrep.joinTables.all(
  df=data.epi.transform3.in,
  fields.df_and_keys=.join.fieldList.df_and_keys,
  naTokenList=.defaultNAToken.byType)
# View(data.sim.joined)
# View(data.epi.joined)

# Export
data.sim.transform3.out = data.sim.joined
data.epi.transform3.out = data.epi.joined
# View(data.sim.transform3.out)
# View(data.epi.transform3.out)
rm(data.sim.joined)
rm(data.epi.joined)
