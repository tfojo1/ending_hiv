'Server utils
- AWS Buckets: https://s3.console.aws.amazon.com/s3/buckets/
  endinghiv/?region=us-east-1&tab=overview'
# Imports ####
# - aws.s3 docs: https://github.com/cloudyr/aws.s3
library(aws.iam)
library(aws.s3)
library(stringr)
library('RPostgreSQL')
library('RPostgres')
library('remotes')
library('DBI')
library('stringr')

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
DB_TABLENAME_PRESETS = 'option_presets'
DB_TABLENAME_CONTACT = 'contact_form'
DB_FIELD_PRESETS = 'urlQueryParamString'
# All button inputIds should be added here:
PRESET_IGNORE_LIST = c(
  'plotly_relayout-A', 
  '.clientValue-default-plotlyCrosstalkOpts',
  'reset_main',
  'downloadButton.plot',
  'createPresetId',
  'createPresetId1',
  'createPresetId2',
  'reset_main_sidebar')

# Utils: misc ####
invert.keyVals <- function(x) {
  y=names(x)
  names(y) = as.character(x)
  y
}

# Utils: AWS S3 Storage ####
s3.list <- function(
  bucket.name
) {
    items.list = get_bucket(bucket=bucket.name, max=Inf)
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

# Utils: Heroku PostgreSQL DB ####
# More potential funcs: dbGetQuery, dbSendQuery
db.connect <- function() {
  dbConnect(
    RPostgres::Postgres(), 
    dbname=Sys.getenv("DB_database"), 
    host=Sys.getenv("DB_host"), 
    port=Sys.getenv("DB_port"), 
    user=Sys.getenv("DB_user"), 
    password=Sys.getenv("DB_password"))
}

db.presets.read.all <- function (
  connection=db.connect(),
  table.name=DB_TABLENAME_PRESETS
) {
  dbReadTable(conn=connection, name=table.name)
}

db.write.rows <- function (
  data.df,  # data.frame
  table.name  # char
) {
  # Insert example: https://jarrettmeyer.com/2018/11/08/
  #r-postgresql-insert
  # Insert 2: dbWriteTable(co`n, name = c("myschema","fruits"), 
  #value = dt2insert,append=TRUE,row.names=FALSE,overwrite=FALSE)
  # https://www.datacareer.de/blog/connect-to-postgresql-with-r-a-
  # step-by-step-example/
  
  connection = db.connect()
  dbWriteTable(
    connection, 
    name=table.name, 
    value=data.df,
    append=TRUE,
    row.names=FALSE,
    overwrite=FALSE)
}

db.write.contactForm <- function(
  name,
  email,
  message,
  version=as.character(get.version.options()[1]),
  date=Sys.Date()
) {
  data.df = data.frame(
    'name'=name,
    'email'=email,
    'message'=message,
    'version'=version,
    'date'=date)
  db.write.rows(
    data.df=data.df,
    table.name=DB_TABLENAME_CONTACT)
}

db.write.queryString <- function(
  queryString,
  version=as.character(get.version.options()[1]),
  date=Sys.Date()) {
  # to-do(minor): @TF/Todd: I set the database to accept non-UNIQUE entries,
  # so we might get multiple queryStrings written to the db that are
  # redudnant. For now I'm allowing it and just returning the most 
  # recent ID of such equivalent entries, so it has the same result.
  # I did this lazily for now because I didn't want to handle edge
  # cases where the record exists; didn't want to risk the session
  # crashing.
  # Write
  data.df = data.frame(
    'urlQueryParamString'=queryString,
    'version'=version,
    'date'=date)
  
  db.write.rows(
    data.df=data.df,
    table.name=DB_TABLENAME_PRESETS)
  
  # Get auto-assigned ID
  table = db.presets.read.all()
  matches = table[table[, 'urlQueryParamString'] == queryString,]
  matchId = matches$id[1]
  matchId
}

presets.urlQueryParamString.create <- function(
  shiny.input,
  ignoreList=PRESET_IGNORE_LIST) {
  
  queryVec = sapply(names(shiny.input), function(key) {
    val = shiny.input[[key]]
    if (key %in% ignoreList) {
    } else if (length(val) > 1) {
      prefix = paste0('&', key, '=')
      csv = paste(val, collapse = ',')
      csv = str_sub(csv, end=-1)
      paramStr = paste0(prefix, csv)
    } else
      paramStr = paste0('&', key, '=', shiny.input[[key]])
  })
  
  for (ele in ignoreList)
    if (ele %in% names(queryVec))
      queryVec[[ele]] = NULL
  
    queryStr = paste(queryVec, collapse='')
  queryStr = str_sub(queryStr, start=2)
  queryStr
}

presets.urlQueryParamString.parse <- function(
  queryString
) {  # --> vec[named]
  presets = list()
  # #to-do: if it starts w/ ?, get rid of
  # delimit queryParams &
  queryVec = str_split(queryString, '&')[[1]]
  # delimit keyVals =
  for (keyVals in queryVec) {
      kv = str_split(keyVals, '=')[[1]]
      key = kv[1]
      val = kv[2]
      # delimit lists ,
      if (grepl(',', val))
        val = str_split(val, ',')[[1]]
      if (toupper(val) %in% c('TRUE', 'FALSE'))
        val = as.logical(val)
      presets[[key]] = val
  }
  presets
}

handleCreatePreset <- function(input) {
  queryStr = presets.urlQueryParamString.create(input)
  presetId = db.write.queryString(queryStr)
  url = paste0('https://jheem.shinyapps.io/EndingHIV?preset=', 
               as.character(presetId))
  msg = paste0('<p>Preset created! You can instantly reload the state of this app in the future via the url:</p><p><a href="', url, '">', url, '</a></p>')
  showMessageModal(message=msg)    
}

getPresetIdFromUrl <- function(session) {
  presetId = NULL
  presetKeyUsed = NULL
  presetPermutations = c(
    'preset', 'presetId', 'presetID', 'presetID', 'presetid',
    'PRESETID', 'preset_id', 'preset_ID', 'PRESET_ID')
  
  urlParams = parseQueryString(
    session$clientData$url_search)
  for (permu in presetPermutations)
    if (permu %in% names(urlParams)) {
      presetKeyUsed = permu
      break
  }
  # presetId = parseQueryString(
  #   session$clientData$url_search)[[presetKeyUsed]]
  if (!(is.null(presetKeyUsed)))
    presetId = urlParams[[presetKeyUsed]]
  
  presetId
}

# Test ####
parseQueryTest <- function() {
  myStr = "sidebarItemExpanded=&interval_coverage=95&risk-groups=msm,idu,msm_idu,heterosexua&reset_main=0&age-groups=age1,age2,age3,age4,age&epidemiological-indicators=incidence,ne&downloadButton.plot=0&plot_format=individual.simulations&createPresetId=1&split=&no_intervention_checkbox=TRUE&geographic_location=12060&toggle_main=Figure&side_menu=main&reset_main_sidebar=0&sidebarCollapsed=FALSE&change_years=2020,203&demog.selectAll=FALSE&label_change=TRUE&use_intervention_2=FALSE&facet=&sex=male,femal&racial-groups=black,hispanic,othe"
  presets.urlQueryParamString.parse(myStr)
}

# db.write.test <- function () {
#   db.write.queryString('testQueryString')
# }

# cacheTest <- function (
#   cache=list()
# ) {
#   # browser()
#   t1 <- proc.time()
#   print('Loading from AWS')
#   fetched.1 = sims.load(filename='int1.Rdata', cache=cache)
#   # simset = fetched.1[['simset']]
#   cache = fetched.1[['cache']]
#   t2 <- proc.time()
#   elapsed.1 = (t2 - t1)[['elapsed']]
#   print('Elapsed: ')
#   print(elapsed.1)
#   
#   # browser()
#   t1b <- proc.time()
#   print('Loading from cache')
#   fetched.2 = sims.load(filename='int1.Rdata', cache=cache)
#   # simset = fetched.2[['simset']]
#   cache = fetched.2[['cache']]
#   t2b <- proc.time()
#   elapsed.2 = (t2b - t1b)[['elapsed']]
#   print('Elapsed: ')
#   print(elapsed.2)
#   
#   print('Cache loading took this fraction of time compared to AWS:' )
#   print(elapsed.2 / elapsed.1)
# }

