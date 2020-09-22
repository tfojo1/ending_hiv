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

#'@param filenames: char[]: version_city_intervention (int)_(str)_(str)
#' (version city and intervention cant use underscore)
sims.load <- function(
  filenames,  # char[]
  bucket.name=BUCKET.NAME.SIMS
) {
  s3load(
    filenames,
    bucket=bucket.name,
    envir = environment())
#    ennvir=parent.frame())
    
    simset
}

# TODO: add docs
static.load <- function(
  filenames,  # char[]
  bucket.name=BUCKET.NAME.STATIC
) {
  # TODO: handle the static loading issue; for some reason wrong bucket name?
  # - needs to load env
  
  # List of 5
  # $ Code      : chr "NoSuchBucket"
  # $ Message   : chr "The specified bucket does not exist"
  # $ BucketName: chr "census_totals.Rdata"
  # $ RequestId : chr "E8CDAC4870DC2F76"
  # $ HostId    : chr "V33XWtVPZs7R6s+uwfOf7xTXBG01RxVJQ0pheBT9IYBZ3lh4799HlY77lwBbnTcpc3OGgQS76a8="
  # - attr(*, "headers")=List of 6
  # ..$ x-amz-request-id : chr "E8CDAC4870DC2F76"
  # ..$ x-amz-id-2       : chr "V33XWtVPZs7R6s+uwfOf7xTXBG01RxVJQ0pheBT9IYBZ3lh4799HlY77lwBbnTcpc3OGgQS76a8="
  # ..$ content-type     : chr "application/xml"
  # ..$ transfer-encoding: chr "chunked"
  # ..$ date             : chr "Tue, 15 Sep 2020 19:41:23 GMT"
  # ..$ server           : chr "AmazonS3"
  # ..- attr(*, "class")= chr [1:2] "insensitive" "list"
  # - attr(*, "class")= chr "aws_error"
  # - attr(*, "request_canonical")= chr "GET\n/census_totals.Rdata/census_totals.Rdata\n\nhost:s3.amazonaws.com\nx-amz-date:20200915T194124Z\n\nhost;x-a"| __truncated__
  # - attr(*, "request_string_to_sign")= chr "AWS4-HMAC-SHA256\n20200915T194124Z\n20200915/us-east-1/s3/aws4_request\n5b9852fed23c6edaa5cf56bc72a0f9419a330d6"| __truncated__
  # - attr(*, "request_signature")= chr "AWS4-HMAC-SHA256 Credential=AKIAIRMV4HP5F3IOTSNA/20200915/us-east-1/s3/aws4_request,SignedHeaders=host;x-amz-da"| __truncated__
  # NULL
  # Error in parse_aws_s3_response(r, Sig, verbose = verbose) : 
  #   Not Found (HTTP 404).
  s3load(
    filenames,
    bucket=bucket.name,
    ennvir=parent.frame())
}

# Test ####
# n/a

# Scratch ####
# # Temp testing:
# testfile.name = xxx
# sims.save(
#   objOrFilepath='This is a test 2.',
#   s3Obj.filename='dfssdffsffs')
# simsList = sims.list()
# static.save(
#   objOrFilepath='This is a test.',
#   s3Obj.filename=testfile.name)
# staticList = static.list()
# 
# # TODO: @Todd: I got an error when loading the plain text file I saved,
# # but not sure if this will be a problem with the objects you save. We
# # should check. Additionally, it may be better for you to upload objects
# # directly: https://s3.console.aws.amazon.com/s3/buckets/endinghiv.sims/?region=us-east-1&tab=overview
# # sim.test = sims.load(testfile.name)
# # static.test = sims.load(testfile.name)
# xxx = s3load(
#   object=testfile.name,
#   bucket='endinghiv.static')

