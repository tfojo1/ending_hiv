'Union epi and sim datasets'

# Settings and Imports ####
.sep = '/'
# options(stringsAsFactors=FALSE)

library('here')

# Source, input, and output files
.src.objects.filename = 'TableauPrep.2.transform3.join.objects.R'
.output.filename = 'tableauData.unioned.csv'
.src.dir.path = paste(here(), 'code', 'make_visualization_data', sep=.sep)
.src.objects.path = paste(.src.dir.path, .src.objects.filename, sep=.sep)
source(.src.objects.path)

.output.path = paste(.output.dir.path, .output.filename, sep=.sep)

# Datasets
data.epi.transform3.in = data.epi.transform3.out
data.sim.transform3.in = data.sim.transform3.out
rm(data.epi.transform3.out)
rm(data.sim.transform3.out)
# View(data.epi.transform3.in)
# View(data.sim.transform3.in)

# Transform: Union
data.merged.df = mergeData.union(
  data.epi.transform3.in, 
  data.sim.transform3.in)
View(data.merged.df)

# Output: CSV ####
write.csv(data.merged.df, .output.path, row.names=FALSE)
