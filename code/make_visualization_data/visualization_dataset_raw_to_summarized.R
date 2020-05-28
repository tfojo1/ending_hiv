# Settings and Imports ####
# Imports: Packages
library('here')
# Settings
setwd(here())
options(stringsAsFactors=FALSE)
# Constants
PROJECT_DIR = here()  # char[] = path to R project
.OS_PATH_SEPARATOR.WIN = '\\'  # Windows
.OS_PATH_SEPARATOR.MAC = '/'  # Unix-like systems


# Config variables ####
.sep = .OS_PATH_SEPARATOR.MAC  # Choose based on your system
.inputFolderName = 'temp'  # Choose w/e; but make sure it's in .gitignore
.data.raw.file.name = 'ending-hiv data - simulated - raw.csv'
.data.summary.file.name = 'ending-hiv data - simulated - summarized.csv'
inputFolderPath = paste(PROJECT_DIR, .inputFolderName, sep=.sep)
data.raw.file.path = 
  paste(inputFolderPath, .data.raw.file.name, sep=.sep)  # char[]
data.summary.file.path = 
  paste(inputFolderPath, .data.summary.file.name, sep=.sep)  # char[]

# Functions ####
dataConversion.fromRawDf.toSummarizedDf <- function(
  rawDf  # data.frame
) {  # Returns --> data.frame
  summaryDf = data.frame()
  
  # to-do
  
  return(summaryDf)  # data.frame
}

# Environment ####
data.raw.df = read.csv(data.raw.file.path)
data.summary.df = data.frame()
data.summary.df = dataConversion.fromRawDf.toSummarizedDf(
  data.raw.file.path)

# Outputs ####
# Output files
write.csv(data.summary.df, data.summary.file.path)

# Temp scratch area ####
# TODO: Convert here and then add to function