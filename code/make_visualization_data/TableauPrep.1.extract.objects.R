# Settings and Imports ####
.sep = '/'
# options(stringsAsFactors=FALSE)

library('here')

# Source, input, and output files
.src.functions.filename = 'TableauPrep.functions.R'
.input.data.epi.raw.filename = 'Epi_data.csv'
.input.data.sim.raw.filename = 'Summarized_Simulations.csv'
.input.table.interventions.filename = 'Interventions.csv'
.input.table.locations.filename = 'Locations.csv'
.input.table.strata.filename = 'Strata.csv'

.src.dir.path = paste(here(), 'code', 'make_visualization_data', sep=.sep)
.input.dir.path = paste(here(), 'visualization', 'raw', sep=.sep)
.output.dir.path = paste(here(), 'visualization', 'tableau', 'data', sep=.sep)
.input.data.epi.raw.file.path = 
  paste(.input.dir.path, .input.data.epi.raw.filename, sep=.sep)  # char[]
.input.data.sim.raw.file.path = 
  paste(.input.dir.path, .input.data.sim.raw.filename, sep=.sep)  # char[]
.src.functions.path = paste(.src.dir.path, .src.functions.filename, sep=.sep)
.src.objects.path = paste(.src.dir.path, .src.objects.filename, sep=.sep)

.input.data.epi.raw.file.path = 
  paste(.input.dir.path, .input.data.epi.raw.filename, sep=.sep)  # char[]
.input.data.sim.raw.file.path = 
  paste(.input.dir.path, .input.data.sim.raw.filename, sep=.sep)  # char[]
.input.table.interventions.path = paste(
  .input.dir.path, .input.table.interventions.filename, sep=.sep)
.input.table.locations.path = paste(
  .input.dir.path, .input.table.locations.filename, sep=.sep)
.input.table.strata.path = paste(
  .input.dir.path, .input.table.strata.filename, sep=.sep)
  
source(.src.functions.path)

data.epi.in = read.csv(.input.data.epi.raw.file.path)
data.sim.in = read.csv(.input.data.sim.raw.file.path)
table.interventions = read.csv(.input.table.interventions.path)
table.locations = read.csv(.input.table.locations.path)
table.strata = read.csv(.input.table.strata.path)

