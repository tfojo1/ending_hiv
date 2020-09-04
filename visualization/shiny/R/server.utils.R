'Server utils'
# Import ####
# - aws.s3 docs: https://github.com/cloudyr/aws.s3
install.packages('aws.s3')
library('aws.s3')

source('env.R')
# - env.R has to be created manually. It is ignored from the repository 
# for security reasons. Please create env.R at the location of the 
# working directory, copy/paste the following placeholder text, and
# replace with the correct values.
# Sys.setenv(
#   "AWS_ACCESS_KEY_ID"="mykey",
#   "AWS_SECRET_ACCESS_KEY"="mysecretkey",
#   "AWS_DEFAULT_REGION"="us-east-1",
#   "AWS_SESSION_TOKEN"="mytoken")


# Constants ####
config.test.testMode.on = TRUE
config.test.runTests = TRUE
config.test.filenames = c(
  'version(int)_city(str)_intervention(str).Rdata',
  '1_Baltimore_No-intervention.Rdata',
  '1_Baltimore_No-intervention (1).Rdata',
  '1_Baltimore_No-intervention (2).Rdata',
  '1_Baltimore_No-intervention (3).Rdata',
  '1_Baltimore_No-intervention (4).Rdata',
  '1_Baltimore_No-intervention (5).Rdata' )

config.test.static.filenames = c('file 1', 'file 2')

# Utils ####
#'@description TODO: 
#'@param xxx TODO
#'@return List of filenames
sims.list <- function(
  apiKey='TODO: load from env',  # TODO
  test.on=config.test.testMode.on
) {
  if (test.on == T) {
    return(config.test.filenames)
    
  } else {
    return(NULL)  # TODO
  }
  
}

#'@description TODO:
#'@param xxx TODO
#'@return List of filenames
static.list <- function(
  apiKey='TODO: load from env',  # TODO
  test.on=config.test.testMode.on
) {
  if (test.on == T) {
    return(config.test.static.filenames)
    
  } else {
    return(NULL)  # TODO
  }
  
}

#'@description TODO
#'@param filenames: char[]: 
#'@param all: 
#'@return TODO
static.load <- function(
  filenames,  # char[]
  all=FALSE,
  apiKey='TODO: load from env',  # TODO
  test.on=config.test.testMode.on
) {
  if (test.on == T) {
    if (all == T) {
      return(sims.load.all())
    } else {
      report = list(
        'loads.success'=c(),
        'loads.fail'=c() )
      
      for (file in filenames) {
        if (file %in% config.test.filenames) {
          report[['loads.success']] = c(report[['loads.success']], file)
        } else {
          report[['loads.fail']] = c(report[['loads.fail']], file)
        }
      }
      
      return(report)      
    }
    
  } else {
    return(NULL)  # TODO
  }
  
}

#'@description TODO
#'@param filenames: char[]: version_city_intervention (int)_(str)_(str)
#' (version city and intervention cant use underscore)
#'@param all: 
#'@return TODO
sims.load <- function(
  filenames,  # char[]
  all=FALSE,
  apiKey='TODO: load from env',  # TODO
  test.on=config.test.testMode.on
) {
  if (test.on == T) {
    if (all == T) {
      return(sims.load.all())
    } else {
      report = list(
        'loads.success'=c(),
        'loads.fail'=c() )
      
      for (file in filenames) {
        if (file %in% config.test.filenames) {
          report[['loads.success']] = c(report[['loads.success']], file)
        } else {
          report[['loads.fail']] = c(report[['loads.fail']], file)
        }
      }
      
      return(report)      
    }
    
  } else {
    return(NULL)  # TODO
  }
  
}

#'@description TODO
#'@param TODO
#'@return TODO
sims.load.all <- function(
  apiKey='TODO: load from env',  # TODO
  test.on=config.test.testMode.on
) {
  if (test.on == T) {
    report = list(
      'loads.success'=config.test.filenames,
      'loads.fail'=c() )
    return(report)
    
  } else {
    return(NULL)  # TODO
  }
}

# Test ####
if (config.test.runTests == T) {
  print("I wonder what beautiful data is up in the cloud waiting for me.")
  print('$ sims.list()')
  print(sims.list())
  print(''); print(''); print('')
  
  print("Hmm, let's load the second and third ones.")
  print("$sims.load('1_Baltimore_No-intervention.Rdata','1_Baltimore_No-intervention (1)))).Rdata')")
  print(sims.load(c(
    "1_Baltimore_No-intervention.Rdata",         
    "1_Baltimore_No-intervention (1)))).Rdata")))
  print(''); print(''); print('')
  
  print("Oops. Some extra parentheses at end of one of those. Well, let's just load everything all at once.")
  print('$sims.load.all()')
  print(sims.load.all())
}

# Scratch ####
