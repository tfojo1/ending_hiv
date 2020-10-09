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
# BUCKET.NAME.GENERAL = 'endinghiv'
CACHE = list()
BUCKET.NAME.SIMS = 'endinghiv.sims'
BUCKET.NAME.STATIC = 'endinghiv.static'

# Utils ####
invert.keyVals <- function(x) {
  y=names(x)
  names(y) = as.character(x)
  y
}

s3.list <- function(
  bucket.name
) {
    items.list = get_bucket(bucket=bucket.name)
    items.names = sapply(items.list, function(x) x$Key )
    return(items.names)
}

sims.list <- function(
  bucket.name=BUCKET.NAME.SIMS
) {
  s3.list(bucket.name)
}

static.list <- function(
  bucket.name=BUCKET.NAME.STATIC
) {
  s3.list(bucket.name)
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

# CACHE.sims.update <- function(key, obj) {
#   CACHE[[key]] = obj
#   CACHE
# }


simsetFilenameToCacheKey <- function(filename) {
  filename = tolower(filename)
  filename = str_replace_all(filename, "[[:punct:]]", "")
  filename
}

is.sim.cached <- function(filenames,
                          cache)
{
    filename.keys = simsetFilenameToCacheKey(filenames)
    cached.keys = cache$keys()
    rv = sapply(filename.keys, function(fkey){
        any(fkey == cached.keys)
    })
    names(rv) = filenames
    rv
}



update.sims.cache <- function(
    filenames,  # char
    cache,  # DiskCache[R6]
    bucket.name=BUCKET.NAME.SIMS)
{
    for (filename in filenames) {
        if (!(simsetFilenameToCacheKey(filename) %in% cache$keys())) {
          print(paste0("'", filename, "' not in cache. Pulling from S3s"))
            # simset = s3load(
            s3load(
                filename,
                bucket=bucket.name,
                envir=environment())
            cache$set(simsetFilenameToCacheKey(filename), simset)
        } else
          print(paste0("'", filename, "'in cache. Grabbing."))
    }
        
    cache
}

#'@param filename: char[]: version_city_intervention (int)_(str)_(str)
#' (version city and intervention cant use underscore)
sims.load <- function(
  filename,  # char
  cache
) {
  cache[[filename]]
}

static.load <- function(
  filename,  # char[]
  bucket.name=BUCKET.NAME.STATIC
) {
  s3load(
    filename,
    bucket=bucket.name,
    ennvir=environment())
}

# Test ####
cacheTest <- function (
  cache=list()
) {
  # browser()
  t1 <- proc.time()
  print('Loading from AWS')
  fetched.1 = sims.load(filename='int1.Rdata', cache=cache)
  # simset = fetched.1[['simset']]
  cache = fetched.1[['cache']]
  t2 <- proc.time()
  elapsed.1 = (t2 - t1)[['elapsed']]
  print('Elapsed: ')
  print(elapsed.1)
  
  # browser()
  t1b <- proc.time()
  print('Loading from cache')
  fetched.2 = sims.load(filename='int1.Rdata', cache=cache)
  # simset = fetched.2[['simset']]
  cache = fetched.2[['cache']]
  t2b <- proc.time()
  elapsed.2 = (t2b - t1b)[['elapsed']]
  print('Elapsed: ')
  print(elapsed.2)
  
  print('Cache loading took this fraction of time compared to AWS:' )
  print(elapsed.2 / elapsed.1)
}

# Scratch ####
# TODO: Examine results with @Todd
# cacheTest()
