'Server utils'

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

# Utils ####
#'@description TODO: Also @Todd, for now, it will be easier to keep a flat
#'directory for file storage rather than write a recursive function.
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

#'@description TODO
#'@param filenames: char[]: version_city_intervention (int)_(str)_(str)
#' (version city and intervention cant use underscore)
#'@param all: logical: TODO: @Todd I thought this could make things 
#'easier to include this option if you want, where we just load them
#'all; or maybe load all of a single version. We could also implement
#'complex filtering later if we want to really add on some nifty extra 
#'quality of life features; but probably not needed.
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
