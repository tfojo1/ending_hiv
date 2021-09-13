

###################################
####---------------------------####
####---- READING FUNCTIONS ----####
####---------------------------####
###################################

##-----------------------------------------##
##-- THE MAIN READ FUNCTION and WRAPPERS --##
##-----------------------------------------##

read.surveillance.manager <- function(dir,
                                      location.field,
                                      location.mapping.fn,
                                      verbose=T)
{
    # Set up rv
    surv = list()
    
    # Get list of files to read
    files = list.files(dir, recursive = F, include.dirs = F)
    files = files[!sapply(file.path(dir, files), dir.exists)]
    
    
    # Read the files
    if (verbose)
        print(paste0("PROCESSING ", length(files), " surveillance files from directory ",
                     "'", dir, "'"))
    
    for (i in 1:length(files))
    {
        file = files[i]
        if (verbose)
            print(paste0("-Processing file '", file, "' (", i, " of ", length(files), ")"))
        
        surv = read.surveillance.file.to.manager(file=file.path(dir, file),
                                                 surv=surv,
                                                 years=NULL,
                                                 locations=NULL,
                                                 location.field=location.field,
                                                 location.mapping.fn=location.mapping.fn,
                                                 age.mapping=AGE.MAPPING,
                                                 race.mapping=BHO.RACE.MAPPING,
                                                 sex.mapping=SEX.MAPPING,
                                                 risk.mapping=RISK.MAPPING.OTHER.TO.HET,
                                                 tolerate.missing.locations=F,
                                                 tolerate.missing.outcome.types=F,
                                                 tolerate.missing.outcome.values=T,
                                                 tolerate.missing.fields=F,
                                                 accomodate.additional.years=T,
                                                 accomodate.additional.locations=T,
                                                 verbose=verbose)
    }
    
    if (verbose)
        print("ALL DONE reading surveillance files. Packaging it up.")
    
    # Add empty params list
    #  (for compatibility - the params only apply to msa surveillance)
    surv$params = list()
    
    # Return
    surv
}

read.state.surveillance.manager <- function(dir='cleaned_data/hiv_surveillance/state/',
                                            verbose=T)
{
    read.surveillance.manager(dir=dir,
                              location.field='FIPS',
                              location.mapping.fn = state.fips.to.abbreviation,
                              verbose=verbose)
}

##---------------------------------##
##-- MAPPINGS and DATA CONSTANTS --##
##---------------------------------##

DIMENSION.ORDER = c('year','location','sex','age','race','risk')

MISSING.NUMBER = c('Data suppressed', 'Data not available')

AGE.MAPPING = c(
    '13-24' = '13-24 years',
    '25-34' = '25-34 years',
    '35-44' = '35-44 years',
    '45-54' = '45-54 years',
    '55+' = '55+ years',
    'Ages 13 years and older' = 'all'
)

# The order of these is the order we lay out our array
BHO.RACE.MAPPING = c(
    'Black/African American' = 'black',
    
    'Hispanic/Latino' = 'hispanic',
    
    'American Indian/Alaska Native' = 'other',
    'Asian' = 'other',
    'Multiple races' = 'other',
    'Native Hawaiian/Other Pacific Islander' = 'other',
    'White' = 'other',
    
    'All races/ethnicities' = 'all'
)

# The order of these is the order we lay out our array
SEX.MAPPING = c(
    'Female' = 'female',
    'Male' = 'male',
    'Both sexes' = 'all'
)

# The order of these is the order we lay out our array
RISK.MAPPING.OTHER.TO.HET = c(
    'Male-to-male sexual contact' = 'msm',
    'Injection drug use' = 'idu',
    'Male-to-male sexual contact and injection drug use' = 'msm_idu',
    'Heterosexual contact' = 'heterosexual',
    'Other' = 'heterosexual',
    'All transmission categories' = 'all'
)

# The raw values we need to extract from each row, by data type
OUTCOME.MAPPING = c(
    'Knowledge of Status' = 'diagnosed',
    'HIV diagnoses' = 'new',
    'HIV prevalence' = 'prevalence',
    'HIV deaths' = 'mortality',
    'Linkage to HIV care' = 'linkage',
    'Receipt of HIV medical care' = 'engagement',
    'HIV viral suppression' = 'suppression',
    'Estimated HIV prevalence (undiagnosed and diagnosed)' = 'estimated.prevalence'
)

RAW.VALUES.FOR.OUTCOME = list(
    diagnosed = c('.numerator' = 'Cases',
                  '.denominator' = 'Population'),
    
    new = 'Cases',
    prevalence = 'Cases',
    mortality = 'Cases',
    
    linkage = c('.numerator' = 'Cases',
                '.denominator' = 'Population'),

    engagement = c(.numerator = 'Cases',
                   .denominator = 'Population'),
    
    suppression = c(.numerator = 'Cases',
                    .denominator = 'Population'),
    
    estimated.prevalence = c('Cases',
                             '.ci.lower' = 'Cases.LCI',
                             '.ci.upper' = 'Cases.UCI')
)

CALCULATE.VALUES.FOR.OUTCOME.FROM.NUMERATOR.AND.DENOMINATOR = list(
    diagnosed = 'diagnosed',
    new = character(),
    prevalence = character(),
    mortality = character(),
    linkage = 'linkage',
    engagement = 'engagement',
    suppression = 'suppression',
    estimated.prevalence = character()
)




# Some sanity checks to make sure we haven't left anything out
if (length(setdiff(OUTCOME.MAPPING, names(RAW.VALUES.FOR.OUTCOME)))>0)
    stop("**Failed Sanity Check: Missing values from 'RAW.VALUES.FOR.OUTCOME' **")

if (length(setdiff(OUTCOME.MAPPING, names(CALCULATE.VALUES.FOR.OUTCOME.FROM.NUMERATOR.AND.DENOMINATOR)))>0)
    stop("**Failed Sanity Check: Missing values from 'CALCULATE.VALUES.FOR.OUTCOME.FROM.NUMERATOR.AND.DENOMINATOR' **")

##------------------------------------##
##-- THE WORK-HORSE HELPER FUNCTION --##
##--   (to read and parse a file)   --##
##------------------------------------##

read.surveillance.file.to.manager <- function(file,
                                              surv,
                                              years,
                                              locations,
                                              location.field,
                                              location.mapping.fn,
                                              age.mapping,
                                              race.mapping,
                                              sex.mapping,
                                              risk.mapping,
                                              tolerate.missing.locations,
                                              tolerate.missing.outcome.types,
                                              tolerate.missing.outcome.values,
                                              tolerate.missing.fields,
                                              accomodate.additional.years,
                                              accomodate.additional.locations,
                                              add.repeated.entries=T,
                                              verbose)
{
    #-- Read the data file --#
    if (verbose)
        print("   Reading file")
    df = read.csv(file, stringsAsFactors = F)
    
    
    if (verbose)
        print(paste0("   Prepping variables for ", format(dim(df)[1], big.mark=','), " rows"))
    
    #-- Map location --#
    df = map.surveillance.df.field(df,
                                   field.to.map=location.field,
                                   map.to.field='location',
                                   mapping=location.mapping.fn,
                                   throw.errors = !tolerate.missing.locations,
                                   data.name.for.error = 'Location')
    if (is.null(locations))
        locations = unique(df$location)
    else if (accomodate.additional.locations)
        locations = union(locations, unique(df$location))
    else
        df = df[sapply(df$location, function(loc){any(loc==locations)}),]
    
    #-- Prune years --#
    if (is.null(years))
        years = as.character(sort(unique(df$Year)))
    else
    {
        years = as.character(years)
        if (accomodate.additional.years)
            years = union(years,
                          as.character(sort(unique(df$Year))))
        else
            df = df[sapply(df$Year, function(y){any(y==years)})]
    }

    #-- Map outcome types --#
    df = map.surveillance.df.field(df,
                                   field.to.map='Indicator',
                                   map.to.field='outcome',
                                   mapping=OUTCOME.MAPPING,
                                   throw.errors = !tolerate.missing.outcome.types,
                                   data.name.for.error = 'Indicator')
    
    #-- Parse outcome values --#
    outcome.fields = unique(unlist(RAW.VALUES.FOR.OUTCOME[unique(df$outcome)]))
    for (one.outcome.field in outcome.fields)
    {
        df[,one.outcome.field] = parse.surveillance.numbers(df[,one.outcome.field],
                                                            pct.to.fraction = F)
    }
    
    #-- Clear rows where any outcome is missing --#
    any.outcome.missing = sapply(1:dim(df)[1], function(i){
        any(is.na(df[ i, RAW.VALUES.FOR.OUTCOME[[ df$outcome[i] ]] ]))
    })
    
    if (any(any.outcome.missing))
    {
        if (tolerate.missing.outcome.values)
            df = df[!any.outcome.missing,]
        else
            stop(paste0(sum(any.outcome.missing),
                        " row(s) are missing outcome values"))
    }
    
    #-- Map Age --#
    df = map.surveillance.df.field(df,
                                   field.to.map='Age.Group',
                                   map.to.field='age',
                                   mapping=age.mapping,
                                   throw.errors = !tolerate.missing.fields,
                                   data.name.for.error = 'Age Group')
    
    #-- Map Race --#
    df = map.surveillance.df.field(df,
                                   field.to.map='Race.Ethnicity',
                                   map.to.field='race',
                                   mapping=race.mapping,
                                   throw.errors = !tolerate.missing.fields,
                                   data.name.for.error = 'Race')
    
    #-- Map Sex --#
    df = map.surveillance.df.field(df,
                                   field.to.map='Sex',
                                   map.to.field='sex',
                                   mapping=sex.mapping,
                                   throw.errors = !tolerate.missing.fields,
                                   data.name.for.error = 'Sex')
    
    #-- Map Risk --#
    df = map.surveillance.df.field(df,
                                   field.to.map='Transmission.Category',
                                   map.to.field='risk',
                                   mapping=risk.mapping,
                                   throw.errors = !tolerate.missing.fields,
                                   data.name.for.error = 'Transmission Category')
    
    #-- Set up dim names --#
    master.dim.names = list(
        year=years,
        location=locations,
        sex=setdiff(unique(sex.mapping),'all'),
        age=setdiff(unique(age.mapping),'all'),
        race=setdiff(unique(race.mapping),'all'),
        risk=setdiff(unique(risk.mapping),'all')
    )[DIMENSION.ORDER]
    
    #-- Parse the data, row by row --#
    if (verbose)
        print(paste0("   Processing ", format(dim(df)[1], big.mark=','), " rows after prep"))
    for (i in 1:dim(df)[1])
    {
        # Figure out which dimensions are marginalized
        marginal.by.dim = c(
            sex = df$sex[i]=='all',
            age = df$age[i]=='all',
            race = df$race[i]=='all',
            risk = df$risk[i]=='all'
        )
        
        # Get the suffix of the element (array) in the list we will access
        if (all(marginal.by.dim))
            arr.suffix = 'all'
        else if (!any(marginal.by.dim))
            arr.suffix = 'master'
        else
        {
            suffix.components = DIMENSION.ORDER[sapply(DIMENSION.ORDER, function(catg){
                !is.na(marginal.by.dim[catg]) && marginal.by.dim[catg]
            })]
            arr.suffix = paste0(suffix.components, collapse='.')
        }
        
        # Create the dimension names we need for the array
        dim.names.mask = c(
            year=T,
            location=T,
            !marginal.by.dim
        )[DIMENSION.ORDER]
        
        # Pull each raw value for the outcome
        raw.values = RAW.VALUES.FOR.OUTCOME[[ df$outcome[i] ]]
        
        for (value.index in 1:length(raw.values))
        {
            outcome.name = paste0(df$outcome[i], names(raw.values)[value.index])
            field = raw.values[value.index]
            
            arr.name = paste0(outcome.name, ".", arr.suffix)
        
    
            # Make sure the array can accomodate these dimensions
            surv[[arr.name]] = create.or.expand.surveillance.array(surv[[arr.name]],
                                                                   new.dim.names=master.dim.names[dim.names.mask])

            # Plug it in
            arr.index = get.surveillance.array.index(surv[[arr.name]],
                                                     year=df$Year[i],
                                                     location=df$location[i],
                                                     sex=df$sex[i],
                                                     age=df$age[i],
                                                     race=df$race[i],
                                                     risk=df$risk[i])
            value = df[i, field]
            if (!add.repeated.entries || is.na(surv[[arr.name]][arr.index]))
                surv[[arr.name]][arr.index] = value
            else
                surv[[arr.name]][arr.index] = surv[[arr.name]][arr.index] + value
        }
        
        # Crunch the calculated values
        calculated.values = CALCULATE.VALUES.FOR.OUTCOME.FROM.NUMERATOR.AND.DENOMINATOR[[ df$outcome[i] ]]
        for (outcome.name in calculated.values)
        {
            arr.name = paste0(outcome.name, ".", arr.suffix)
            
            # Make sure the array can accomodate these dimensions
            surv[[arr.name]] = create.or.expand.surveillance.array(surv[[arr.name]],
                                                                   new.dim.names=master.dim.names[dim.names.mask])
            
            # Get the numerator and denominator ready
            numerator.arr.name = paste0(outcome.name, '.numerator.', arr.suffix)
            denominator.arr.name = paste0(outcome.name, '.denominator.', arr.suffix)
            
            
            arr.index = get.surveillance.array.index(surv[[arr.name]],
                                                     year=df$Year[i],
                                                     location=df$location[i],
                                                     sex=df$sex[i],
                                                     age=df$age[i],
                                                     race=df$race[i],
                                                     risk=df$risk[i])
            
            # Divide and plug it in
            surv[[arr.name]][arr.index] = surv[[numerator.arr.name]][arr.index] / surv[[denominator.arr.name]][arr.index]
        }
    }
    
    #-- Return --#
    if (verbose)
        print("   Done")
    surv
}

##--------------------------------##
##-- HELPERS FOR THE WORK-HORSE --##
##--------------------------------##

# This function takes one (CDC-generated) column, and maps it to the 
#   variable names we will need to process the file
# Checks for mapping errors and either throws error or prunes dataset as requested
map.surveillance.df.field <- function(df,
                                      field.to.map,
                                      map.to.field,
                                      mapping,
                                      throw.errors=F,
                                      data.name.for.error)
{
    #-- Map the field --#
    if (is(mapping, 'function'))
        mapped = mapping(df[,field.to.map])
    else
        mapped = mapping[df[,field.to.map]]
    
    #-- Check for NAs and handle --#
    unmapped.mask = is.na(mapped)
    if (any(unmapped.mask))
    {
        if (throw.errors)
        {
            unmapped.values = unique(df[unmapped.mask,field.to.map])
            stop(paste0("Unknown ", data.name.for.error, "(s): ",
                        paste0("'", unmapped.values, "'", collapse=', ')))
        }
        else
        {
            mapped = mapped[!unmapped.mask]
            df = df[!unmapped.mask,]
        }
    }
    
    #-- Return --#
    df[,map.to.field] = mapped
    df
}

create.or.expand.surveillance.array <- function(arr,
                                    new.dim.names)
{
    
    if (is.null(arr))
    {
        array(NaN, dim=sapply(new.dim.names, length), dimnames = new.dim.names)
    }
    else if (list.is.subset(new.dim.names, dimnames(arr)))
    {
        arr
    }
    else
    {
        old.dim.names = dimnames(arr)
        new.dim.name.names = names(new.dim.names)
        new.dim.names = lapply(names(new.dim.names), function(dim){
            rv = union(old.dim.names[[dim]], new.dim.names[[dim]])
            if (dim=='year')
                rv = sort(rv)
            rv
        })
        names(new.dim.names) = new.dim.name.names
        
        rv = array(NaN, dim=sapply(new.dim.names, length), dimnames=new.dim.names)
        
        # Set up the numbers needed to map new to old
        old.n.dim = length(old.dim.names)
        old.dim = sapply(old.dim.names, length)
        old.dim.before = c(1, cumprod(old.dim)[-old.n.dim])
        
        new.n.dim = length(new.dim.names)
        new.dim = sapply(new.dim.names, length)
        new.dim.before = c(1, cumprod(new.dim)[-new.n.dim])
        
        old.to.new.dim.values = lapply(names(old.dim.names), function(one.dim.name){
            sapply(old.dim.names[[one.dim.name]], function(old.val){
                (1:length(new.dim.names[[one.dim.name]]))[ old.val == new.dim.names[[one.dim.name]] ]
            })
        })
        
        old.to.new.indices = sapply(1:length(arr), function(i){
            old.dim.values = sapply(1:old.n.dim, function(d){
                 (ceiling( i / old.dim.before[d] ) - 1) %% old.dim[d] + 1
            })
            
            new.dim.values = sapply(1:old.n.dim, function(d){
                old.to.new.dim.values[[d]][old.dim.values[d]]
            })
            
            1 + sum((new.dim.values-1) * new.dim.before)
        })
        
        rv[old.to.new.indices] = as.numeric(arr)
        
        rv
    }
}

list.is.subset <- function(l.sub, l.super)
{
    all(sapply(names(l.sub), function(elem.name){
        length(setdiff(l.sub[[elem.name]], l.super[[elem.name]]))==0
    }))
}

##-----------------------##
##-- LOW-LEVEL HELPERS --##
##-----------------------##

parse.surveillance.numbers <- function(values,
                                       pct.to.fraction=F,
                                       allow.warnings = F)
{
    for (missing.value in MISSING.NUMBER)
        values[!is.na(values) & values==missing.value] = NA
    
    values = gsub(",", '', values)
    
    if (allow.warnings)
        rv = as.numeric(values)
    else
    {
        tryCatch({
            rv = as.numeric(values)
        },
        warning = function(w){
            stop("Some surveillance numbers could not be parsed")
        })
    }
    
    if (pct.to.fraction)
        rv = rv / 100
    
    rv
}

get.surveillance.array.index <- function(arr,
                                         ...)
{
    args = list(...)
    args = args[args!='all']
    
    dim.names = dimnames(arr)
    n.dims = length(dim.names)
    dims = sapply(dim.names, length)
    dims.before = c(1, cumprod(dims)[-n.dims])
    
    dim.values = sapply(names(dim.names), function(dim.name){
        values.for.dim = (1:length(dim.names[[dim.name]]))[dim.names[[dim.name]]==args[[dim.name]]]
    })
    
    1 + sum((dim.values-1) * dims.before)
}