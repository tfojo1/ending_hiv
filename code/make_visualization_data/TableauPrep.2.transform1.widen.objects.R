'Pivot wider: simulation dataset; pivot on `statistic` field'

# Settings and Imports ####
.sep = '/'
# options(stringsAsFactors=FALSE)

library('here')
library('tidyr')

# .output.filename = 'Summarized_Simulations-pivoted.csv'
# .output.path = paste(here(), .output.dir.path, .output.filename, sep=.sep)
.src.objects.filename = 'TableauPrep.1.extract.objects.R'
.src.dir.path = paste(here(), 'code', 'make_visualization_data', sep=.sep)
.src.objects.path = paste(.src.dir.path, .src.objects.filename, sep=.sep)
source(.src.objects.path)

# Variables
.pivotCols.names = 'statistic'
.pivotCols.values = 'value'

# Read
# View(data.sim.in)

# Transform
.cols.statistic.factorValues = unique(
  data.sim.in[[.pivotCols.names]])
data.sim.transform1.pivoted = pivot_wider(
  data.sim.in, 
  names_from=.pivotCols.names, 
  values_from=.pivotCols.values) 
# View(data.sim.transform1.pivoted)

# Note: I moved this operation to the next transform file.
# data.sim.transform1.pivoted.cols.names = sapply(
#   names(data.sim.transform1.pivoted),
#   TableauPrep.cols.output.names.get.i)
# names(data.sim.transform1.pivoted) = data.sim.transform1.pivoted.cols.names

data.sim.transform1.out = data.sim.transform1.pivoted
# View(data.sim.transform1.out)
rm(data.sim.in)
rm(data.sim.transform1.pivoted)


# Output: CSV
# write.csv(TableauPrep.simData.out, .output.path)
