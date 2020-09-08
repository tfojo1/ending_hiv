'Server utils
- AWS Buckets: https://s3.console.aws.amazon.com/s3/buckets/
  endinghiv/?region=us-east-1&tab=overview'
# Import ####
# - aws.s3 docs: https://github.com/cloudyr/aws.s3
library('aws.iam')
library('aws.s3')

source('env.R')
# - env.R has to be created manually. It is ignored from the repository 
# for security reasons. Please create env.R at the location of the 
# working directory, copy/paste the following placeholder text, and
# replace with the correct values.
# Sys.setenv(
#   "AWS_ACCESS_KEY_ID"="mykey",
#   "AWS_SECRET_ACCESS_KEY"="mysecretkey",
#   "AWS_DEFAULT_REGION"="us-east-1")

# Constants ####
config.test.testMode.on = FALSE
config.test.runTests = FALSE
config.test.filenames = c(
  'version(int)_city(str)_intervention(str).Rdata',
  '1_Baltimore_No-intervention.Rdata',
  '1_Baltimore_No-intervention (1).Rdata',
  '1_Baltimore_No-intervention (2).Rdata',
  '1_Baltimore_No-intervention (3).Rdata',
  '1_Baltimore_No-intervention (4).Rdata',
  '1_Baltimore_No-intervention (5).Rdata' )

config.test.static.filenames = c('file 1', 'file 2')

# BUCKET.NAME.GENERAL = 'endinghiv'
BUCKET.NAME.SIMS = 'endinghiv.sims'
BUCKET.NAME.STATIC = 'endinghiv.static'

# Utils ####
s3.list <- function(
  test.on=config.test.testMode.on,
  bucket.name
) {
  if (test.on == T) 
    return(config.test.filenames)
  else {
    items.list = get_bucket(bucket=bucket.name)
    items.names = sapply(items.list, function(x) x$Key )
    return(items.names) }
}

sims.list <- function(
  test.on=config.test.testMode.on,
  bucket.name=BUCKET.NAME.SIMS
) {
  s3.list(test.on, bucket.name)
}

static.list <- function(
  test.on=config.test.testMode.on,
  bucket.name=BUCKET.NAME.STATIC
) {
  s3.list(test.on, bucket.name)
}

s3.save <- function(
  objOrFilepath,
  s3Obj.filename=NULL,
  bucket.name
) {
  if (class(objOrFilepath) %in% c('character')) {
    # put_object() stores a local file into an S3 bucket. The multipart
    #  = TRUE argument can be used to upload large files in pieces.    
    if (is.null(s3Obj.filename))
      s3Obj.filename = objOrFilepath
    put_object(
      file=objOrFilepath,
      object=s3Obj.filename,
      bucket=bucket.name,
      multipart=FALSE)
  } else {
    # s3save() saves one or more in-memory R objects to an .Rdata file
    # in S3 (analogously to save()). s3saveRDS() is an analogue for
    # saveRDS(). s3load() loads one or more objects into memory from an
    # .Rdata file stored in S3 (analogously to load()). s3readRDS() is
    # an analogue for readRDS()
    s3save(
      object=objOrFilepath, 
      bucket=bucket.name)
  }
}

sims.save <- function(
  objOrFilepath,
  s3Obj.filename=NULL,
  bucket.name=BUCKET.NAME.SIMS
) {
  s3.save(objOrFilepath, s3Obj.filename, bucket.name)
}

static.save <- function(
  objOrFilepath,
  s3Obj.filename=NULL,
  bucket.name=BUCKET.NAME.STATIC
) {
  s3.save(objOrFilepath, s3Obj.filename, bucket.name)
}

s3.load <- function(
  filenames,  # char[]
  bucket.name,
  test.on=config.test.testMode.on
) {
  if (test.on == T) {
    report = list(
      'loads.success'=c(),
      'loads.fail'=c() )
    for (file in filenames) {
      if (file %in% config.test.filenames)
        report[['loads.success']] = c(report[['loads.success']], file)
      else
        report[['loads.fail']] = c(report[['loads.fail']], file)
      return(report) }
    
  } else {
    s3load(
      object=filenames, 
      bucket=bucket.name)
  }
}

#'@param filenames: char[]: version_city_intervention (int)_(str)_(str)
#' (version city and intervention cant use underscore)
sims.load <- function(
  filenames,  # char[]
  bucket.name=BUCKET.NAME.SIMS
) {
  s3.load(
    filenames,
    bucket.name=bucket.name)
}

static.load <- function(
  filenames,  # char[]
  bucket.name=BUCKET.NAME.STATIC
) {
  s3.load(
    filenames,
    bucket.name=bucket.name)
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
}

# Scratch ####

# Temp testing:
sims.save(
  objOrFilepath='This is a test 2.',
  s3Obj.filename='dfssdffsffs')
simsList = sims.list()
static.save(
  objOrFilepath='This is a test.',
  s3Obj.filename=testfile.name)
staticList = static.list()

# TODO: @Todd: I got an error when loading the plain text file I saved,
# but not sure if this will be a problem with the objects you save. We
# should check. Additionally, it may be better for you to upload objects
# directly: https://s3.console.aws.amazon.com/s3/buckets/endinghiv.sims/?region=us-east-1&tab=overview
# sim.test = sims.load(testfile.name)
# static.test = sims.load(testfile.name)
xxx = s3load(
  object=testfile.name,
  bucket='endinghiv.static')
