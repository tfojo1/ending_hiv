

# This is also set in parallel in hiv_surveillance_manager.R
SURVEILLANCE.DELIMITER = ';'

###################################
####---------------------------####
####---- READING FUNCTIONS ----####
####---------------------------####
###################################

##--------------------------------------------##
##-- THE GENERAL READ FUNCTION and WRAPPERS --##
##--------------------------------------------##

read.surveillance.manager <- function(dir,
                                      location.field,
                                      location.mapping.fn,
                                      sources,
                                      urls,
                                      details,
                                      verbose=T,
                                      test=F)
{
    # Set up rv
    surv = create.surveillance.manager()
    
    # Get list of files to read
    files = list.files(dir, recursive = F, include.dirs = F)
    files = files[!sapply(file.path(dir, files), dir.exists)]
    
    if (test)
        files = files[grepl('sle', files)][1:3]
    
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
                                                 sources=sources,
                                                 urls=urls,
                                                 details=details,
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
                                            sources="CDC",
                                            urls="https://gis.cdc.gov/grasp/nchhstpatlas/main.html",
                                            details='CDC - HIV Atlas',
                                            verbose=T,
                                            test=F)
{
    read.surveillance.manager(dir=dir,
                              location.field='FIPS',
                              location.mapping.fn = state.fips.to.abbreviation,
                              sources=sources,
                              urls=urls,
                              details=details,
                              verbose=verbose,
                              test=test)
}

read.county.surveillance.manager <- function(dir='cleaned_data/hiv_surveillance/county/',
                                             sources="CDC",
                                             urls="https://gis.cdc.gov/grasp/nchhstpatlas/main.html",
                                             details='CDC - HIV Atlas',
                                             verbose=T,
                                             test=F)
{
    read.surveillance.manager(dir=dir,
                              location.field='FIPS',
                              location.mapping.fn = function(x){x},
                              sources=sources,
                              urls=urls,
                              details=details,
                              verbose=verbose,
                              test=test)
}

##---------------------------##
##-- MSA READING FUNCTIONS --##
##---------------------------##

read.msa.surveillance.manager <- function(dir='cleaned_data/',
                                          use.adjusted.estimate=F,
                                          first.total.prevalence.year=2007,
                                          first.total.new.year=2008,
                                          include.children.in.prevalence=F,
                                          verbose=F,
                                          
                                          new.sources='CDC',
                                          new.urls=DEFAULT.NEW.DATA.URLS,
                                          new.details=DEFAULT.NEW.DATA.DETAILS,
                                          
                                          prevalence.sources='CDC',
                                          prevalence.urls=DEFAULT.PREVALENCE.DATA.URLS,
                                          prevalence.details=DEFAULT.PREVALENCE.DATA.DETAILS,
                                          
                                          mortality.sources=prevalence.sources,
                                          mortality.urls=prevalence.urls,
                                          mortality.details=prevalence.details,
                                          
                                          cumulative.aids.mortality.source='CDC',
                                          cumulative.aids.mortality.url='https://wonder.cdc.gov/AIDSPublic.html',
                                          cumulative.aids.mortality.details='CDC Wonder, AIDS Public Use Data to 2002',
                                          
                                          aids.diagnoses.source=cumulative.aids.mortality.source,
                                          aids.diagnoses.url=cumulative.aids.mortality.url,
                                          aids.diagnoses.details=cumulative.aids.mortality.details)
{
    rv = create.surveillance.manager()
    
    #-- READ IN FILES --#
    sub.dirs = list.dirs(file.path(dir, 'hiv_surveillance/msa/msa_surveillance_reports'))
    
    filenames.full = unlist(sapply(sub.dirs, list.files, include.dirs=F, no..=T, pattern='.csv$', full.names=T))
    filenames = unlist(sapply(sub.dirs, list.files, include.dirs=F, no..=T, pattern='.csv$'))
    
    is.total = grepl('total', filenames)
    
    dfs = lapply(1:length(filenames.full), function(i){
        #        print(paste0('Reading ', filenames.full[i]))
        read.msa.file(filenames.full[i], allow.misses = is.total[i], verbose=verbose)
    })
    all.codes = unique(unlist(lapply(dfs, function(df){df$code})))
    
    #-- CATEGORIZE WHAT DATA IS IN EACH FILE --#
    is.prevalence = grepl('prevalence', filenames, ignore.case = T)
    is.incidence = grepl('new', filenames, ignore.case = T)
    is.mortality = grepl('death', filenames, ignore.case = T)
    
    has.age = grepl('age', filenames, ignore.case=T)
    has.race = grepl('race', filenames, ignore.case=T)
    has.risk = grepl('risk', filenames, ignore.case=T)
    
    for.female = grepl('female', filenames, ignore.case=T)
    for.male = grepl('male', filenames, ignore.case=T) & !for.female
    
    for.black = grepl('black', filenames, ignore.case=T)
    for.hispanic = grepl('hispanic', filenames, ignore.case=T)
    for.white = grepl('white', filenames, ignore.case=T)
    
    incidence.year = gsub('.*([0-9][0-9][0-9][0-9]) new.*', '\\1', filenames)
    incidence.year[!is.incidence] = NA
    
    prevalence.year = gsub('.*([0-9][0-9][0-9][0-9]) prevalence.*', '\\1', filenames)
    prevalence.year[!is.prevalence] = NA
    
    mortality.year = gsub('.*([0-9][0-9][0-9][0-9]) death.*', '\\1', filenames)
    mortality.year[!is.mortality] = NA
    
    #-- SET UP SOURCES, URLs, and DETAILS --#
    
    # Check names
    
    check.new.years = sort(unique(incidence.year[!is.na(incidence.year)]))
    check.prevalence.years = sort(unique(prevalence.year[!is.na(prevalence.year)]))
    check.mortality.years = sort(unique(mortality.year[!is.na(mortality.year)]))
    
    if (length(new.sources)==1)
    {
        new.sources = rep(new.sources, length(check.new.years))
        names(new.sources) = as.character(check.new.years)
    }
    if (length(prevalence.sources)==1)
    {
        prevalence.sources = rep(prevalence.sources, length(check.prevalence.years))
        names(prevalence.sources) = as.character(check.prevalence.years)
    }
    if (length(mortality.sources)==1)
    {
        mortality.sources = rep(mortality.sources, length(check.mortality.years))
        names(mortality.sources) = as.character(check.mortality.years)
    }
    
    if (is.null(names(new.sources)))
        stop("'new.sources' must have names corresponding to years")
    if (is.null(names(new.urls)))
        stop("'new.urls' must have names corresponding to years")
    if (is.null(names(new.details)))
        stop("'new.details' must have names corresponding to years")
    
    if (is.null(names(prevalence.sources)))
        stop("'prevalence.sources' must have names corresponding to years")
    if (is.null(names(prevalence.urls)))
        stop("'prevalence.urls' must have names corresponding to years")
    if (is.null(names(prevalence.details)))
        stop("'prevalence.details' must have names corresponding to years")
    
    if (is.null(names(mortality.sources)))
        stop("'mortality.sources' must have names corresponding to years")
    if (is.null(names(mortality.urls)))
        stop("'mortality.urls' must have names corresponding to years")
    if (is.null(names(mortality.details)))
        stop("'mortality.details' must have names corresponding to years")
    
    # Register for code
    rv = add.surveillance.mapping.codes(rv, new.sources)
    rv = add.surveillance.mapping.codes(rv, new.urls)
    rv = add.surveillance.mapping.codes(rv, new.details)
    
    rv = add.surveillance.mapping.codes(rv, prevalence.sources)
    rv = add.surveillance.mapping.codes(rv, prevalence.urls)
    rv = add.surveillance.mapping.codes(rv, prevalence.details)
    
    rv = add.surveillance.mapping.codes(rv, mortality.sources)
    rv = add.surveillance.mapping.codes(rv, mortality.urls)
    rv = add.surveillance.mapping.codes(rv, mortality.details)
    
    rv = add.surveillance.mapping.codes(rv, cumulative.aids.mortality.source)
    rv = add.surveillance.mapping.codes(rv, cumulative.aids.mortality.url)
    rv = add.surveillance.mapping.codes(rv, cumulative.aids.mortality.details)
    
    rv = add.surveillance.mapping.codes(rv, aids.diagnoses.source)
    rv = add.surveillance.mapping.codes(rv, aids.diagnoses.url)
    rv = add.surveillance.mapping.codes(rv, aids.diagnoses.details)
    
    
    # Replace the source/url/details strings with codes
    new.sources = get.surveillance.mapping.codes(rv, new.sources, collapse=F)
    new.urls = get.surveillance.mapping.codes(rv, new.urls, collapse=F)
    new.details = get.surveillance.mapping.codes(rv, new.details, collapse=F)
    
    prevalence.sources = get.surveillance.mapping.codes(rv, prevalence.sources, collapse=F)
    prevalence.urls = get.surveillance.mapping.codes(rv, prevalence.urls, collapse=F)
    prevalence.details = get.surveillance.mapping.codes(rv, prevalence.details, collapse=F)
    
    mortality.sources = get.surveillance.mapping.codes(rv, mortality.sources, collapse=F)
    mortality.urls = get.surveillance.mapping.codes(rv, mortality.urls, collapse=F)
    mortality.details = get.surveillance.mapping.codes(rv, mortality.details, collapse=F)
    
    cumulative.aids.mortality.source = get.surveillance.mapping.codes(rv, cumulative.aids.mortality.source, collapse=F)
    cumulative.aids.mortality.url = get.surveillance.mapping.codes(rv, cumulative.aids.mortality.url, collapse=F)
    cumulative.aids.mortality.details = get.surveillance.mapping.codes(rv, cumulative.aids.mortality.details, collapse=F)
    
    aids.diagnoses.source = get.surveillance.mapping.codes(rv, aids.diagnoses.source, collapse=F)
    aids.diagnoses.url = get.surveillance.mapping.codes(rv, aids.diagnoses.url, collapse=F)
    aids.diagnoses.details = get.surveillance.mapping.codes(rv, aids.diagnoses.details, collapse=F)
    
    #check that we have the right years for sources, etc
   
    if (length(setdiff(check.new.years, names(new.sources)))>0)
        stop(paste0("No source has been given for new diagnoses in 'new.sources' for the year(s): ",
                    paste0(setdiff(check.new.years, names(new.sources))), collapse=', '))
    if (length(setdiff(check.new.years, names(new.urls)))>0)
        stop(paste0("No url has been given for new diagnoses in 'new.urls' for the year(s): ",
                    paste0(setdiff(check.new.years, names(new.urls))), collapse=', '))
    if (length(setdiff(check.new.years, names(new.details)))>0)
        stop(paste0("No details have been given for new diagnoses in 'new.details' for the year(s): ",
                    paste0(setdiff(check.new.years, names(new.details))), collapse=', '))
    
    if (length(setdiff(check.prevalence.years, names(prevalence.sources)))>0)
        stop(paste0("No source has been given for prevalence in 'prevalence.sources' for the year(s): ",
                    paste0(setdiff(check.prevalence.years, names(prevalence.sources))), collapse=', '))
    if (length(setdiff(check.prevalence.years, names(prevalence.urls)))>0)
        stop(paste0("No url has been given for prevalence in 'prevalence.urls' for the year(s): ",
                    paste0(setdiff(check.prevalence.years, names(prevalence.urls))), collapse=', '))
    if (length(setdiff(check.prevalence.years, names(prevalence.details)))>0)
        stop(paste0("No details have been given for prevalence in 'prevalence.details' for the year(s): ",
                    paste0(setdiff(check.prevalence.years, names(prevalence.details))), collapse=', '))
    
    if (length(setdiff(check.mortality.years, names(mortality.sources)))>0)
        stop(paste0("No source has been given for mortality in 'mortality.sources' for the year(s): ",
                    paste0(setdiff(check.mortality.years, names(mortality.sources))), collapse=', '))
    if (length(setdiff(check.mortality.years, names(mortality.urls)))>0)
        stop(paste0("No url has been given for mortality in 'mortality.urls' for the year(s): ",
                    paste0(setdiff(check.mortality.years, names(mortality.urls))), collapse=', '))
    if (length(setdiff(check.mortality.years, names(mortality.details)))>0)
        stop(paste0("No details have been given for mortality in 'mortality.details' for the year(s): ",
                    paste0(setdiff(check.mortality.years, names(mortality.details))), collapse=', '))
    
    #-- SET UP RV ARRAY SKELETONS --#
    rv$params=list(use.adjusted.estimate=use.adjusted.estimate,
                          correct.new.to.county.level=F,
                          correct.prevalence.to.county.level=F)
    
    all.incidence.years = sort(unique(incidence.year[is.incidence]))
    all.prevalence.years = sort(unique(prevalence.year[is.prevalence]))
    all.mortality.years = sort(unique(mortality.year[is.mortality]))
    
    sexes = c('female', 'male')
    ages = c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years')
    races = c('black', 'hispanic', 'white', 'american_indian_or_alaska_native', 'asian')
    races.limited = c(races[1:3],'other')
    race.order = c(black=3, hispanic=4, white=5, american_indian_or_alaska_native=1, asian=2)
    risks = c('msm', 'idu', 'msm_idu', 'heterosexual', 'other')
    risk.order = c(msm=1, idu=2, msm_idu=3, heterosexual=4, other=5)
    
    # Total
    dim.names = list(year=all.incidence.years, location=as.character(all.codes))
    rv$new.all = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$source$new.all = rv$url$new.all = rv$details$new.all =
        array(as.character(NA), dim=sapply(dim.names, length), dimnames=dim.names)
    
    dim.names = list(year=all.prevalence.years, location=as.character(all.codes))
    rv$prevalence.all = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$source$prevalence.all = rv$url$prevalence.all = rv$details$prevalence.all =
        array(as.character(NA), dim=sapply(dim.names, length), dimnames=dim.names)
    
    # Sex
    dim.names = list(year=all.incidence.years, location=as.character(all.codes), sex=sexes)
    rv$new.sex = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$source$new.sex = rv$url$new.sex = rv$details$new.sex =
        array(as.character(NA), dim=sapply(dim.names, length), dimnames=dim.names)
    
    dim.names = list(year=all.prevalence.years, location=as.character(all.codes), sex=sexes)
    rv$prevalence.sex = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$source$prevalence.sex = rv$url$prevalence.sex = rv$details$prevalence.sex =
        array(as.character(NA), dim=sapply(dim.names, length), dimnames=dim.names)
    
    # Sex x Age
    dim.names = list(year=all.incidence.years, location=as.character(all.codes), sex=sexes, age=ages)
    rv$new.sex.age = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$source$new.sex.age = rv$url$new.sex.age = rv$details$new.sex.age =
        array(as.character(NA), dim=sapply(dim.names, length), dimnames=dim.names)
    
    dim.names = list(year=all.prevalence.years, location=as.character(all.codes), sex=sexes, age=ages)
    rv$prevalence.sex.age = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$source$prevalence.sex.age = rv$url$prevalence.sex.age = rv$details$prevalence.sex.age =
        array(as.character(NA), dim=sapply(dim.names, length), dimnames=dim.names)
    
    # Sex x Race
    dim.names = list(year=all.incidence.years, location=as.character(all.codes), sex=sexes, race=races)
    rv$new.sex.race = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$source$new.sex.race = rv$url$new.sex.race = rv$details$new.sex.race =
        array(as.character(NA), dim=sapply(dim.names, length), dimnames=dim.names)
    
    dim.names = list(year=all.prevalence.years, location=as.character(all.codes), sex=sexes, race=races)
    rv$prevalence.sex.race = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$source$prevalence.sex.race = rv$url$prevalence.sex.race = rv$details$prevalence.sex.race =
        array(as.character(NA), dim=sapply(dim.names, length), dimnames=dim.names)
    
    # Sex x Risk
    dim.names = list(year=all.incidence.years, location=as.character(all.codes), sex=sexes, risk=risks)
    rv$new.sex.risk = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$source$new.sex.risk = rv$url$new.sex.risk = rv$details$new.sex.risk =
        array(as.character(NA), dim=sapply(dim.names, length), dimnames=dim.names)
    
    dim.names = list(year=all.prevalence.years, location=as.character(all.codes), sex=sexes, risk=risks)
    rv$prevalence.sex.risk = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$source$prevalence.sex.risk = rv$url$prevalence.sex.risk = rv$details$prevalence.sex.risk =
        array(as.character(NA), dim=sapply(dim.names, length), dimnames=dim.names)
    
    # Race x risk
    dim.names = list(year=all.incidence.years, location=as.character(all.codes), race=races.limited, risk=risks)
    rv$new.race.risk = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$source$new.race.risk = rv$url$new.race.risk = rv$details$new.race.risk =
        array(as.character(NA), dim=sapply(dim.names, length), dimnames=dim.names)
    
    dim.names = list(year=all.prevalence.years, location=as.character(all.codes), race=races.limited, risk=risks)
    rv$prevalence.race.risk = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$source$prevalence.race.risk = rv$url$prevalence.race.risk = rv$details$prevalence.race.risk =
        array(as.character(NA), dim=sapply(dim.names, length), dimnames=dim.names)
    
    # Mortality
    dim.names = list(year=as.character(all.mortality.years), location=as.character(all.codes), sex=sexes)
    rv$mortality.sex = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$source$mortality.sex = rv$url$mortality.sex = rv$details$mortality.sex =
        array(as.character(NA), dim=sapply(dim.names, length), dimnames=dim.names)
    
    # New
    for (year in all.incidence.years)
    {
        if (verbose)
            print(paste0("Reading Incidence for year ", year))
        two.estimates = year <= 2014
        has.rank = year > 2010
        
        #Total
        mask = is.incidence & !is.na(incidence.year) & incidence.year==year & is.total
        
        if (sum(mask)>1)
            stop(paste0("Multiple files with incidence for year ", year, " for total"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            codes = intersect(all.codes, df$code)
            
            if (year>=2008)
            {
                rv$new.all[year,codes] = df[codes,2 + as.numeric(use.adjusted.estimate&&two.estimates)]
                
                #if the adjusted is missing, use the non-adjusted
                if (use.adjusted.estimate && two.estimates)
                {
                    na.mask = is.na(rv$new.all[year,codes])
                    codes.adjusted = intersect(codes, df$code[na.mask])
                    rv$new.all[year,codes.adjusted] = as.matrix(df[codes.adjusted, 2])
                }
            }
            else
                rv$new.all[year,codes] = as.matrix(df[codes,4])
            
            rv$source$new.all[year,codes] = new.sources[year]
            rv$url$new.all[year,codes] = new.urls[year]
            rv$details$new.all[year,codes] = new.details[year]
        }
        
        #Female
        mask = is.incidence & !is.na(incidence.year) & incidence.year==year & for.female & !has.age & !has.race & !has.risk
        if (sum(mask)>1)
            stop(paste0("Multiple files with incidence for year ", year, " for female alone"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$new.sex[year,df$code,'female'] = as.matrix(df[,2 + use.adjusted.estimate])
            else
                rv$new.sex[year,df$code,'female'] = as.matrix(df[,2])
            
            rv$source$new.sex[year,df$code,'female'] = new.sources[year]
            rv$url$new.sex[year,df$code,'female'] = new.urls[year]
            rv$details$new.sex[year,df$code,'female'] = new.details[year]
        }
        
        #Male
        mask = is.incidence & !is.na(incidence.year) & incidence.year==year & for.male & !has.age & !has.race & !has.risk
        if (sum(mask)>1)
            stop(paste0("Multiple files with incidence for year ", year, " for male alone"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$new.sex[year,df$code,'male'] = as.matrix(df[,2 + use.adjusted.estimate])
            else
                rv$new.sex[year,df$code,'male'] = as.matrix(df[,2])
            
            rv$source$new.sex[year,df$code,'male'] = new.sources[year]
            rv$url$new.sex[year,df$code,'male'] = new.urls[year]
            rv$details$new.sex[year,df$code,'male'] = new.details[year]
        }
        
        #Female x age
        mask = is.incidence & !is.na(incidence.year) & incidence.year==year & has.age & for.female
        if (sum(mask)>1)
            stop(paste0("Multiple files with incidence for year ", year, " for female x age"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$new.sex.age[year,df$code,'female',] = as.matrix(df[,1+(1:5)*(3+has.rank)-(2+has.rank) + use.adjusted.estimate])
            else
                rv$new.sex.age[year,df$code,'female',] = as.matrix(df[,1+(1:5)*3-2])
            
            rv$source$new.sex.age[year,df$code,'female',] = new.sources[year]
            rv$url$new.sex.age[year,df$code,'female',] = new.urls[year]
            rv$details$new.sex.age[year,df$code,'female',] = new.details[year]
        }
        
        #Male x age
        mask = is.incidence & !is.na(incidence.year) & incidence.year==year & has.age & for.male
        if (sum(mask)>1)
            stop(paste0("Multiple files with incidence for year ", year, " for male x age"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$new.sex.age[year,df$code,'male',] = as.matrix(df[,1+(1:5)*(3+has.rank)-(2+has.rank) + use.adjusted.estimate, ])
            else
                rv$new.sex.age[year,df$code,'male',] = as.matrix(df[,1+(1:5)*3-2])
            
            rv$source$new.sex.age[year,df$code,'male',] = new.sources[year]
            rv$url$new.sex.age[year,df$code,'male',] = new.urls[year]
            rv$details$new.sex.age[year,df$code,'male',] = new.details[year]
        }
        
        #Female x race
        mask = is.incidence & !is.na(incidence.year) & incidence.year==year & has.race & for.female
        if (sum(mask)>1)
            stop(paste0("Multiple files with incidence for year ", year, " for female x race"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$new.sex.race[year,df$code,'female',races] = as.matrix(df[,1+race.order[races]*(3+has.rank)-(2+has.rank) + use.adjusted.estimate])
            else
                rv$new.sex.race[year,df$code,'female',races] = as.matrix(df[,1+race.order[races]*3-2])
            
            rv$source$new.sex.race[year,df$code,'female',races] = new.sources[year]
            rv$url$new.sex.race[year,df$code,'female',races] = new.urls[year]
            rv$details$new.sex.race[year,df$code,'female',races] = new.details[year]
        }
        
        #Male x race
        mask = is.incidence & !is.na(incidence.year) & incidence.year==year & has.race & for.male
        if (sum(mask)>1)
            stop(paste0("Multiple files with incidence for year ", year, " for male x race"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$new.sex.race[year,df$code,'male',races] = as.matrix(df[,1+race.order[races]*(3+has.rank)-(2+has.rank) + use.adjusted.estimate])
            else
                rv$new.sex.race[year,df$code,'male',races] = as.matrix(df[,1+race.order[races]*3-2])
            
            rv$source$new.sex.race[year,df$code,'male',races] = new.sources[year]
            rv$url$new.sex.race[year,df$code,'male',races] = new.urls[year]
            rv$details$new.sex.race[year,df$code,'male',races] = new.details[year]
        }
        
        #Female x risk
        mask = is.incidence & !is.na(incidence.year) & incidence.year==year & has.risk & for.female
        if (sum(mask)>1)
            stop(paste0("Multiple files with incidence for year ", year, " for female x risk"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$new.sex.risk[year,df$code,'female',c('idu','heterosexual', 'other')] = as.matrix(df[,1+(1:3)*2-1 + use.adjusted.estimate])
            else
                rv$new.sex.risk[year,df$code,'female',c('idu','heterosexual', 'other')] = as.matrix(df[,2:4])
            
            rv$new.sex.risk[year,df$code,'female',c('msm','msm_idu')] = 0
            
            rv$source$new.sex.risk[year,df$code,'female',c('idu','heterosexual', 'other','msm','msm_idu')] = new.sources[year]
            rv$url$new.sex.risk[year,df$code,'female',c('idu','heterosexual', 'other','msm','msm_idu')] = new.urls[year]
            rv$details$new.sex.risk[year,df$code,'female',c('idu','heterosexual', 'other','msm','msm_idu')] = new.details[year]
        }
        
        #Male x risk
        mask = is.incidence & !is.na(incidence.year) & incidence.year==year & has.risk & for.male
        if (sum(mask)>1)
            stop(paste0("Multiple files with incidence for year ", year, " for male x risk"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$new.sex.risk[year,df$code,'male',c('msm','idu','msm_idu','heterosexual','other')] = as.matrix(df[,1+(1:5)*2-1 + use.adjusted.estimate])
            else
                rv$new.sex.risk[year,df$code,'male',c('msm','idu','msm_idu','heterosexual','other')] = as.matrix(df[,2:6])
            
            rv$source$new.sex.risk[year,df$code,'male',c('msm','idu','msm_idu','heterosexual','other')] = new.sources[year]
            rv$url$new.sex.risk[year,df$code,'male',c('msm','idu','msm_idu','heterosexual','other')] = new.urls[year]
            rv$details$new.sex.risk[year,df$code,'male',c('msm','idu','msm_idu','heterosexual','other')] = new.details[year]
        }
        
        #Black x risk
        mask = is.incidence & !is.na(incidence.year) & incidence.year==year & has.risk & for.black
        if (sum(mask)>1)
            stop(paste0("Multiple files with incidence for year ", year, " for black x risk"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if(two.estimates)
                rv$new.race.risk[year,df$code,'black',c('msm','idu','msm_idu','heterosexual','other')] = as.matrix(df[,1+(1:5)*2-1 + use.adjusted.estimate])
            else
                rv$new.race.risk[year,df$code,'black',c('msm','idu','msm_idu','heterosexual','other')] = as.matrix(df[,2:6])
            
            rv$source$new.race.risk[year,df$code,'black',c('msm','idu','msm_idu','heterosexual','other')] = new.sources[year]
            rv$url$new.race.risk[year,df$code,'black',c('msm','idu','msm_idu','heterosexual','other')] = new.urls[year]
            rv$details$new.race.risk[year,df$code,'black',c('msm','idu','msm_idu','heterosexual','other')] = new.details[year]
        }
        
        #Hispanic x risk
        mask = is.incidence & !is.na(incidence.year) & incidence.year==year & has.risk & for.hispanic
        if (sum(mask)>1)
            stop(paste0("Multiple files with incidence for year ", year, " for hispanic x risk"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if(two.estimates)
                rv$new.race.risk[year,df$code,'hispanic',c('msm','idu','msm_idu','heterosexual','other')] = as.matrix(df[,1+(1:5)*2-1 + use.adjusted.estimate])
            else
                rv$new.race.risk[year,df$code,'hispanic',c('msm','idu','msm_idu','heterosexual','other')] = as.matrix(df[,2:6])
            
            rv$source$new.race.risk[year,df$code,'hispanic',c('msm','idu','msm_idu','heterosexual','other')] = new.sources[year]
            rv$url$new.race.risk[year,df$code,'hispanic',c('msm','idu','msm_idu','heterosexual','other')] = new.urls[year]
            rv$details$new.race.risk[year,df$code,'hispanic',c('msm','idu','msm_idu','heterosexual','other')] = new.details[year]
        }
        
        #White x risk
        mask = is.incidence & !is.na(incidence.year) & incidence.year==year & has.risk & for.white
        if (sum(mask)>1)
            stop(paste0("Multiple files with incidence for year ", year, " for white x risk"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if(two.estimates)
                rv$new.race.risk[year,df$code,'white',c('msm','idu','msm_idu','heterosexual','other')] = as.matrix(df[,1+(1:5)*2-1 + use.adjusted.estimate])
            else
                rv$new.race.risk[year,df$code,'white',c('msm','idu','msm_idu','heterosexual','other')] = as.matrix(df[,2:6])
            
            rv$source$new.race.risk[year,df$code,'white',c('msm','idu','msm_idu','heterosexual','other')] = new.sources[year]
            rv$url$new.race.risk[year,df$code,'white',c('msm','idu','msm_idu','heterosexual','other')] = new.urls[year]
            rv$details$new.race.risk[year,df$code,'white',c('msm','idu','msm_idu','heterosexual','other')] = new.details[year]
        }
    }
    
    #New - Other x risk
    rv$new.race.risk[,,'other',] = apply(rv$new.sex.risk, c(1,2,4), sum) - apply(rv$new.race.risk[,,c('black','hispanic','white'),], c(1,2,4), sum)
    
    rv$source$new.race.risk[,,'other',] = new.sources[dimnames(rv$source$new.race.risk)$year]
    rv$source$new.race.risk[,,'other',][is.na(rv$new.race.risk[,,'other',])] = NA
    rv$url$new.race.risk[,,'other',] = new.urls[dimnames(rv$url$new.race.risk)$year]
    rv$url$new.race.risk[,,'other',][is.na(rv$new.race.risk[,,'other',])] = NA
    rv$details$new.race.risk[,,'other',] = new.details[dimnames(rv$details$new.race.risk)$year]
    rv$details$new.race.risk[,,'other',][is.na(rv$new.race.risk[,,'other',])] = NA
    
    
    # Prevalence
    for (year in all.prevalence.years)
    {
        if (verbose)
            print(paste0("Reading Prevalence for year ", year))
        two.estimates = year < 2014
        has.rank = year > 2009 || year == 2008 || year == 2007
        
        #Total
        mask = is.prevalence & !is.na(prevalence.year) & prevalence.year==year & is.total
        
        if (sum(mask)>1)
            stop(paste0("Multiple files with incidence for year ", year, " for total"))
        else if (sum(mask) && year >= first.total.prevalence.year)
        {
            df = dfs[mask][[1]]
            codes = intersect(all.codes, df$code)
            if (year>=2007)
            {
                
                #use the non-adjusted for all
                rv$prevalence.all[year,codes] = as.matrix(df[codes, 4 + two.estimates + has.rank])
                #rv$prevalence.all[year,codes] = as.matrix(df[codes, 4 + two.estimates + has.rank + as.numeric(use.adjusted.estimate&&two.estimates)])
                
                #if the adjusted is missing, use the non-adjusted
                if (use.adjusted.estimate)
                {
                    na.mask = is.na(rv$prevalence.all[year,codes])
                    codes.adjusted = intersect(codes, df$code[na.mask])
                    rv$prevalence.all[year,codes.adjusted] = as.matrix(df[codes.adjusted, 4 + two.estimates + has.rank])
                }
            }
            else
                rv$prevalence.all[year,codes] = as.matrix(df[codes,6+2*as.numeric(include.children.in.prevalence)])
            
            rv$source$prevalence.all[year,codes] = prevalence.sources[year]
            rv$url$prevalence.all[year,codes] = prevalence.urls[year]
            rv$details$prevalence.all[year,codes] = prevalence.details[year]
        }
        
        #Female
        mask = is.prevalence & !is.na(prevalence.year) & prevalence.year==year & for.female & !has.age & !has.race & !has.risk
        if (sum(mask)>1)
            stop(paste0("Multiple files with prevalence for year ", year, " for female alone"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$prevalence.sex[year,df$code,'female'] = as.matrix(df[,6 + use.adjusted.estimate])
            else
                rv$prevalence.sex[year,df$code,'female'] = as.matrix(df[,5])
            
            rv$source$prevalence.sex[year,df$code,'female'] = prevalence.sources[year]
            rv$url$prevalence.sex[year,df$code,'female'] = prevalence.urls[year]
            rv$details$prevalence.sex[year,df$code,'female'] = prevalence.details[year]
        }
        
        #Male
        mask = is.prevalence & !is.na(prevalence.year) & prevalence.year==year & for.male & !has.age & !has.race & !has.risk
        if (sum(mask)>1)
            stop(paste0("Multiple files with prevalence for year ", year, " for male alone"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$prevalence.sex[year,df$code,'male'] = as.matrix(df[,6 + use.adjusted.estimate])
            else
                rv$prevalence.sex[year,df$code,'male'] = as.matrix(df[,5])
            
            rv$source$prevalence.sex[year,df$code,'male'] = prevalence.sources[year]
            rv$url$prevalence.sex[year,df$code,'male'] = prevalence.urls[year]
            rv$details$prevalence.sex[year,df$code,'male'] = prevalence.details[year]
        }
        
        #Female x age
        mask = is.prevalence & !is.na(prevalence.year) & prevalence.year==year & has.age & for.female
        if (sum(mask)>1)
            stop(paste0("Multiple files with prevalence for year ", year, " for female x age"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$prevalence.sex.age[year,df$code,'female',] = as.matrix(df[,1+(1:5)*3-2 + use.adjusted.estimate])
            else
                rv$prevalence.sex.age[year,df$code,'female',] = as.matrix(df[,1+(1:5)*2-1])
            
            rv$source$prevalence.sex.age[year,df$code,'female',] = prevalence.sources[year]
            rv$url$prevalence.sex.age[year,df$code,'female',] = prevalence.urls[year]
            rv$details$prevalence.sex.age[year,df$code,'female',] = prevalence.details[year]
        }
        
        #Male x age
        mask = is.prevalence & !is.na(prevalence.year) & prevalence.year==year & has.age & for.male
        if (sum(mask)>1)
            stop(paste0("Multiple files with prevalence for year ", year, " for male x age"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$prevalence.sex.age[year,df$code,'male',] = as.matrix(df[,1+(1:5)*3-2 + use.adjusted.estimate])
            else
                rv$prevalence.sex.age[year,df$code,'male',] = as.matrix(df[,1+(1:5)*2-1])
            
            rv$source$prevalence.sex.age[year,df$code,'male',] = prevalence.sources[year]
            rv$url$prevalence.sex.age[year,df$code,'male',] = prevalence.urls[year]
            rv$details$prevalence.sex.age[year,df$code,'male',] = prevalence.details[year]
        }
        
        #Female x race
        mask = is.prevalence & !is.na(prevalence.year) & prevalence.year==year & has.race & for.female
        if (sum(mask)>1)
            stop(paste0("Multiple files with prevalence for year ", year, " for female x race"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$prevalence.sex.race[year,df$code,'female',races] = as.matrix(df[,1+race.order[races]*3-2 + use.adjusted.estimate])
            else
                rv$prevalence.sex.race[year,df$code,'female',races] = as.matrix(df[,1+race.order[races]*2-1])
            
            rv$source$prevalence.sex.race[year,df$code,'female',races] = prevalence.sources[year]
            rv$url$prevalence.sex.race[year,df$code,'female',races] = prevalence.urls[year]
            rv$details$prevalence.sex.race[year,df$code,'female',races] = prevalence.details[year]
        }
        
        #Male x race
        mask = is.prevalence & !is.na(prevalence.year) & prevalence.year==year & has.race & for.male
        if (sum(mask)>1)
            stop(paste0("Multiple files with prevalence for year ", year, " for male x race"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$prevalence.sex.race[year,df$code,'male',races] = as.matrix(df[,1+race.order[races]*3-2 + use.adjusted.estimate])
            else
                rv$prevalence.sex.race[year,df$code,'male',races] = as.matrix(df[,1+race.order[races]*2-1])
            
            rv$source$prevalence.sex.race[year,df$code,'male',races] = prevalence.sources[year]
            rv$url$prevalence.sex.race[year,df$code,'male',races] = prevalence.urls[year]
            rv$details$prevalence.sex.race[year,df$code,'male',races] = prevalence.details[year]
        }
        
        #Female x risk
        mask = is.prevalence & !is.na(prevalence.year) & prevalence.year==year & has.risk & for.female
        if (sum(mask)>1)
            stop(paste0("Multiple files with prevalence for year ", year, " for female x risk"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$prevalence.sex.risk[year,df$code,'female',c('idu','heterosexual', 'other')] = as.matrix(df[,1+(1:3)*2-1 + use.adjusted.estimate])
            else
                rv$prevalence.sex.risk[year,df$code,'female',c('idu','heterosexual', 'other')] = as.matrix(df[,2:4])
            
            rv$prevalence.sex.risk[year,df$code,'female',c('msm','msm_idu')] = 0
            
            rv$source$prevalence.sex.risk[year,df$code,'female',c('idu','heterosexual', 'other','msm','msm_idu')] = prevalence.sources[year]
            rv$url$prevalence.sex.risk[year,df$code,'female',c('idu','heterosexual', 'other','msm','msm_idu')] = prevalence.urls[year]
            rv$details$prevalence.sex.risk[year,df$code,'female',c('idu','heterosexual', 'other','msm','msm_idu')] = prevalence.details[year]
        }
        
        #Male x risk
        mask = is.prevalence & !is.na(prevalence.year) & prevalence.year==year & has.risk & for.male
        if (sum(mask)>1)
            stop(paste0("Multiple files with prevalence for year ", year, " for male x risk"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$prevalence.sex.risk[year,df$code,'male',c('msm','idu','msm_idu','heterosexual','other')] = as.matrix(df[,1+(1:5)*2-1 + use.adjusted.estimate])
            else
                rv$prevalence.sex.risk[year,df$code,'male',c('msm','idu','msm_idu','heterosexual','other')] = as.matrix(df[,2:6])

            rv$source$prevalence.sex.risk[year,df$code,'male',c('msm','idu','msm_idu','heterosexual','other')] = prevalence.sources[year]
            rv$url$prevalence.sex.risk[year,df$code,'male',c('msm','idu','msm_idu','heterosexual','other')] = prevalence.urls[year]
            rv$details$prevalence.sex.risk[year,df$code,'male',c('msm','idu','msm_idu','heterosexual','other')] = prevalence.details[year]
        }
        
        #Black x risk
        mask = is.prevalence & !is.na(prevalence.year) & prevalence.year==year & has.risk & for.black
        if (sum(mask)>1)
            stop(paste0("Multiple files with prevalence for year ", year, " for black x risk"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$prevalence.race.risk[year,df$code,'black',c('msm','idu','msm_idu','heterosexual','other')] = as.matrix(df[,1+(1:5)*2-1 + use.adjusted.estimate])
            else
                rv$prevalence.race.risk[year,df$code,'black',c('msm','idu','msm_idu','heterosexual','other')] = as.matrix(df[,2:6])
            
            rv$source$prevalence.race.risk[year,df$code,'black',c('msm','idu','msm_idu','heterosexual','other')] = prevalence.sources[year]
            rv$url$prevalence.race.risk[year,df$code,'black',c('msm','idu','msm_idu','heterosexual','other')] = prevalence.urls[year]
            rv$details$prevalence.race.risk[year,df$code,'black',c('msm','idu','msm_idu','heterosexual','other')] = prevalence.details[year]
        }
        
        #Hispanic x risk
        mask = is.prevalence & !is.na(prevalence.year) & prevalence.year==year & has.risk & for.hispanic
        if (sum(mask)>1)
            stop(paste0("Multiple files with prevalence for year ", year, " for hispanic x risk"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$prevalence.race.risk[year,df$code,'hispanic',c('msm','idu','msm_idu','heterosexual','other')] = as.matrix(df[,1+(1:5)*2-1 + use.adjusted.estimate])
            else
                rv$prevalence.race.risk[year,df$code,'hispanic',c('msm','idu','msm_idu','heterosexual','other')] = as.matrix(df[,2:6])

            rv$source$prevalence.race.risk[year,df$code,'hispanic',c('msm','idu','msm_idu','heterosexual','other')] = prevalence.sources[year]
            rv$url$prevalence.race.risk[year,df$code,'hispanic',c('msm','idu','msm_idu','heterosexual','other')] = prevalence.urls[year]
            rv$details$prevalence.race.risk[year,df$code,'hispanic',c('msm','idu','msm_idu','heterosexual','other')] = prevalence.details[year]
        }
        
        #White x risk
        mask = is.prevalence & !is.na(prevalence.year) & prevalence.year==year & has.risk & for.white
        if (sum(mask)>1)
            stop(paste0("Multiple files with prevalence for year ", year, " for white x risk"))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$prevalence.race.risk[year,df$code,'white',c('msm','idu','msm_idu','heterosexual','other')] = as.matrix(df[,1+(1:5)*2-1 + use.adjusted.estimate])
            else
                rv$prevalence.race.risk[year,df$code,'white',c('msm','idu','msm_idu','heterosexual','other')] = as.matrix(df[,2:6])
        
            rv$source$prevalence.race.risk[year,df$code,'white',c('msm','idu','msm_idu','heterosexual','other')] = prevalence.sources[year]
            rv$url$prevalence.race.risk[year,df$code,'white',c('msm','idu','msm_idu','heterosexual','other')] = prevalence.urls[year]
            rv$details$prevalence.race.risk[year,df$code,'white',c('msm','idu','msm_idu','heterosexual','other')] = prevalence.details[year]
        }
    }
    
    #prevalence - Other x risk
    rv$prevalence.race.risk[,,'other',] = apply(rv$prevalence.sex.risk, c(1,2,4), sum) - apply(rv$prevalence.race.risk[,,c('black','hispanic','white'),], c(1,2,4), sum)
    
    rv$source$prevalence.race.risk[,df$code,'other',] = prevalence.sources[dimnames(rv$source$prevalence.race.risk)$year]
    rv$source$prevalence.race.risk[,df$code,'other',][is.na(rv$prevalence.race.risk[,df$code,'other',])] = NA
    rv$url$prevalence.race.risk[,df$code,'other',] = prevalence.urls[dimnames(rv$url$prevalence.race.risk)$year]
    rv$url$prevalence.race.risk[,df$code,'other',][is.na(rv$prevalence.race.risk[,df$code,'other',])] = NA
    rv$details$prevalence.race.risk[,df$code,'other',] = prevalence.details[dimnames(rv$details$prevalence.race.risk)$year]
    rv$details$prevalence.race.risk[,df$code,'other',][is.na(rv$prevalence.race.risk[,df$code,'other',])] = NA
    
    
    #-- MORTALITY --#
    for (year in all.mortality.years)
    {
        if (verbose)
            print(paste0("Reading Mortality for year ", year))
        
        #Female x age
        mask = is.mortality & !is.na(mortality.year) & mortality.year==year
        if (sum(mask)>1)
            stop(paste0("Multiple files with mortality for year ", year))
        else if (sum(mask))
        {
            df = dfs[mask][[1]]
            if (two.estimates)
                rv$mortality.sex[year,df$code,c('male', 'female')] = as.matrix(df[,c(2,6) + use.adjusted.estimate])
            else
                rv$mortality.sex[year,df$code,c('male', 'female')] = as.matrix(df[,c(2,5)])
            
            
            rv$source$mortality.sex[year,df$code,c('male','female')] = mortality.sources[year]
            rv$url$mortality.sex[year,df$code,c('male','female')] = mortality.urls[year]
            rv$details$mortality.sex[year,df$code,c('male','female')] = mortality.details[year]
        }
    }
    
    #Cumulative AIDS Mortality
    rv$cumulative.aids.mortality.sex.race.risk = read.cum.aids.mortality(file=file.path(dir, 'hiv_surveillance/msa/aids_mortality_pre_2000.txt'))
    dim.names = c(list(year='2000'), dimnames(rv$cumulative.aids.mortality.sex.race.risk))
    dim(rv$cumulative.aids.mortality.sex.race.risk) = sapply(dim.names, length)
    dimnames(rv$cumulative.aids.mortality.sex.race.risk) = dim.names
    
    rv$source$cumulative.aids.mortality.sex.race.risk = 
        rv$url$cumulative.aids.mortality.sex.race.risk =
        rv$details$cumulative.aids.mortality.sex.race.risk =
        array(as.character(NA), dim=sapply(dim.names, length), dimnames=dim.names)
    
    rv$source$cumulative.aids.mortality.sex.race.risk[!is.na(rv$cumulative.aids.mortality.sex.race.risk)] = cumulative.aids.mortality.source
    rv$url$cumulative.aids.mortality.sex.race.risk[!is.na(rv$cumulative.aids.mortality.sex.race.risk)] = cumulative.aids.mortality.url
    rv$details$cumulative.aids.mortality.sex.race.risk[!is.na(rv$cumulative.aids.mortality.sex.race.risk)] = cumulative.aids.mortality.details
    
    
    #-- AGGREGATE FOR THE TOTAL POPULATION NUMBERS and AGE, RACE, RISK --#
    
    # Mortality
    rv$mortality.all = apply(rv$mortality.sex, c('year','location'), sum)
    
    rv$source$mortality.all = apply(rv$source$mortality.sex, c('year','location'), join.mappings)
    rv$url$mortality.all = apply(rv$url$mortality.sex, c('year','location'), join.mappings)
    rv$details$mortality.all = apply(rv$details$mortality.sex, c('year','location'), join.mappings)
    
    
    # New - age
    rv$new.age = apply(rv$new.sex.age, c('year','location','age'), sum)
    
    rv$source$new.age = apply(rv$source$new.sex.age, c('year','location','age'), join.mappings)
    rv$url$new.age = apply(rv$url$new.sex.age, c('year','location','age'), join.mappings)
    rv$details$new.age = apply(rv$details$new.sex.age, c('year','location','age'), join.mappings)
    
    
    # Prevalence - age
    rv$prevalence.age = apply(rv$prevalence.sex.age, c('year','location','age'), sum)
    
    rv$source$prevalence.age = apply(rv$source$prevalence.sex.age, c('year','location','age'), join.mappings)
    rv$url$prevalence.age = apply(rv$url$prevalence.sex.age, c('year','location','age'), join.mappings)
    rv$details$prevalence.age = apply(rv$details$prevalence.sex.age, c('year','location','age'), join.mappings)
    
    
    # New - race
    rv$new.race = max.marginal.sum(rv$new.race.risk,
                                   rv$new.sex.race,
                                   'race')
    
    if (length(dimnames(rv$new.race)$race)==length(dimnames(rv$new.race.risk)) &&
        all(dimnames(rv$new.race)$race==dimnames(rv$new.race.risk)$race))
        sum.from = 'new.race.risk'
    else
        sum.from = 'new.sex.race'
    
    rv$source$new.race = apply(rv$source[[sum.from]], c('year','location','race'), join.mappings)
    rv$url$new.race = apply(rv$url[[sum.from]], c('year','location','race'), join.mappings)
    rv$details$new.race = apply(rv$details[[sum.from]], c('year','location','race'), join.mappings)
    
    
    # Prevalence - race
    rv$prevalence.race = max.marginal.sum(rv$prevalence.race.risk,
                                          rv$prevalence.sex.race,
                                          'race')
    
    if (length(dimnames(rv$prevalence.race)$race)==length(dimnames(rv$prevalence.race.risk)) &&
        all(dimnames(rv$prevalence.race)$race==dimnames(rv$prevalence.race.risk)$race))
        sum.from = 'prevalence.race.risk'
    else
        sum.from = 'prevalence.sex.race'
    rv$source$prevalence.race = apply(rv$source[[sum.from]], c('year','location','race'), join.mappings)
    rv$url$prevalence.race = apply(rv$url[[sum.from]], c('year','location','race'), join.mappings)
    rv$details$prevalence.race = apply(rv$details[[sum.from]], c('year','location','race'), join.mappings)
    
    
    # New - risk
    rv$new.risk = max.marginal.sum(rv$new.race.risk,
                                   rv$new.sex.risk,
                                   'risk')
    
    rv$source$new.risk = apply(rv$source$new.sex.risk, c('year','location','risk'), join.mappings)
    rv$url$new.risk = apply(rv$url$new.sex.risk, c('year','location','risk'), join.mappings)
    rv$details$new.risk = apply(rv$details$new.sex.risk, c('year','location','risk'), join.mappings)
    
    
    # Prevalence - risk
    rv$prevalence.risk = max.marginal.sum(rv$prevalence.race.risk,
                                          rv$prevalence.sex.risk,
                                          'risk')
    
    rv$source$prevalence.risk = apply(rv$source$prevalence.sex.risk, c('year','location','risk'), join.mappings)
    rv$url$prevalence.risk = apply(rv$url$prevalence.sex.risk, c('year','location','risk'), join.mappings)
    rv$details$prevalence.risk = apply(rv$details$prevalence.sex.risk, c('year','location','risk'), join.mappings)
    
    
    #-- AIDS DIAGNOSES --#
    old.aids.diagnoses = read.aids.diagnoses(file.path(dir, 'hiv_surveillance/msa/aids_diagnoses_pre_2002.txt'))
    move.from.new.to.aids.years = dimnames(rv$new.all)[['year']][as.numeric(dimnames(rv$new.all)[['year']])<first.total.new.year]
    aids.years = sort(union(dimnames(old.aids.diagnoses)[['year']], move.from.new.to.aids.years))
    aids.locations = sort(union(dimnames(old.aids.diagnoses)[['location']], dimnames(rv$new.all)[['location']]))
    
    dim.names = list(year=aids.years, location=aids.locations)
    rv$aids.diagnoses.all = array(as.numeric(NA), dim=sapply(dim.names, length), dimnames=dim.names)
    rv$source$aids.diagnoses.all =
        rv$url$aids.diagnoses.all = 
        rv$details$aids.diagnoses.all = 
        array(as.character(NA), dim=sapply(dim.names, length), dimnames=dim.names)
    
    # Pull from new
    rv$aids.diagnoses.all[intersect(aids.years, dimnames(rv$new.all)[['year']]), 
                          dimnames(rv$new.all)[['location']] ] =
        rv$new.all[intersect(aids.years, dimnames(rv$new.all)[['year']]),]
    rv$source$aids.diagnoses.all[intersect(aids.years, dimnames(rv$new.all)[['year']]), 
                                 dimnames(rv$new.all)[['location']] ] = 
        rv$source$new.all[intersect(aids.years, dimnames(rv$new.all)[['year']]), 
                          dimnames(rv$new.all)[['location']] ]
    rv$url$aids.diagnoses.all[intersect(aids.years, dimnames(rv$new.all)[['year']]), 
                                 dimnames(rv$new.all)[['location']] ] = 
        rv$url$new.all[intersect(aids.years, dimnames(rv$new.all)[['year']]), 
                          dimnames(rv$new.all)[['location']] ]
    rv$details$aids.diagnoses.all[intersect(aids.years, dimnames(rv$new.all)[['year']]), 
                                 dimnames(rv$new.all)[['location']] ] = 
        rv$details$new.all[intersect(aids.years, dimnames(rv$new.all)[['year']]), 
                          dimnames(rv$new.all)[['location']] ]
    
    # Pull from AIDS Dx Data
    rv$aids.diagnoses.all[intersect(aids.years, dimnames(old.aids.diagnoses)[['year']]), 
                          dimnames(old.aids.diagnoses)[['location']] ] =
        old.aids.diagnoses[intersect(aids.years, dimnames(old.aids.diagnoses)[['year']]),]
    rv$source$aids.diagnoses.all[intersect(aids.years, dimnames(old.aids.diagnoses)[['year']]), 
                                 dimnames(old.aids.diagnoses)[['location']] ] =
        aids.diagnoses.source
    rv$url$aids.diagnoses.all[intersect(aids.years, dimnames(old.aids.diagnoses)[['year']]), 
                                 dimnames(old.aids.diagnoses)[['location']] ] =
        aids.diagnoses.url
    rv$details$aids.diagnoses.all[intersect(aids.years, dimnames(old.aids.diagnoses)[['year']]), 
                                 dimnames(old.aids.diagnoses)[['location']] ] =
        aids.diagnoses.details
    
    # Remove from new
    rv$new.all[move.from.new.to.aids.years,] = NA
    
    rv$source$new.all[move.from.new.to.aids.years,] = as.character(NA)
    rv$url$new.all[move.from.new.to.aids.years,] = as.character(NA)
    rv$details$new.all[move.from.new.to.aids.years,] = as.character(NA)
    
    #-- RETURN IT --#
    rv
}

#the year here is the year for new cases
# prevalence and mortality are from before
OFF.YEAR.URLS = c(
    '1993' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-1993-vol-5-4.pdf",
    '1994' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-1994-vol-6-2.pdf",
    '1995' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-1995-vol-7-2.pdf",
    '1996' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-1996-vol-8-2.pdf",
    '1997' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-1997-vol-9-2.pdf",
    '1998' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-1998-vol-10-2.pdf",
    '1999' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-1999-vol-11-2.pdf",
    
    '2000' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2000-vol-12-2.pdf",
    '2001' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2001-vol-13-2.pdf",
    '2002' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2002-vol-14.pdf",
    '2003' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2003-vol-15.pdf",
    '2004' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2004-vol-16.pdf",
    '2005' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2005-vol-17.pdf",
    '2006' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2006-vol-18.pdf",
    '2007' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2007-vol-19.pdf",
    
    '2008' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2008-vol-20.pdf",
    '2009' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2009-vol-21.pdf",
    
    '2010' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-supplemental-report-vol-18-1.pdf",
    '2011' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-supplemental-report-vol-18-8.pdf",
    
    '2012' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2012-vol-24.pdf",
    
    '2013' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-supplemental-report-vol-20-4.pdf",
    '2014' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-supplemental-report-vol-21-1.pdf",
    '2015' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-supplemental-report-vol-22-1.pdf",
    '2016' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-supplemental-report-vol-23-2.pdf",
    '2017' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-supplemental-report-vol-24-2.pdf"
)

SAME.YEAR.URLS = c(
    '2018' = "https://www.cdc.gov/hiv/pdf/library/reports/surveillance-data-tables/vol-1-no-3/cdc-hiv-surveillance-tables-vol-1-no-3.pdf"
)

DEFAULT.NEW.DATA.URLS = c(OFF.YEAR.URLS, SAME.YEAR.URLS)

DEFAULT.NEW.DATA.DETAILS = rep("MSA Surveillance Reports, after 2013", length(DEFAULT.NEW.DATA.URLS))
names(DEFAULT.NEW.DATA.DETAILS) = names(DEFAULT.NEW.DATA.URLS)
DEFAULT.NEW.DATA.DETAILS[as.numeric(names(DEFAULT.NEW.DATA.DETAILS))<2014] = 'MSA Surveillance Reports, 2008-2013'
DEFAULT.NEW.DATA.DETAILS[as.numeric(names(DEFAULT.NEW.DATA.DETAILS))<2008] = 'MSA Surveillance Reports, before 2008'

DEFAULT.PREVALENCE.DATA.URLS = c(OFF.YEAR.URLS, SAME.YEAR.URLS)
names(DEFAULT.PREVALENCE.DATA.URLS) = c(as.character(as.numeric(names(OFF.YEAR.URLS))-1), names(SAME.YEAR.URLS))

DEFAULT.PREVALENCE.DATA.DETAILS = DEFAULT.NEW.DATA.DETAILS
names(DEFAULT.PREVALENCE.DATA.DETAILS) = names(DEFAULT.PREVALENCE.DATA.URLS)

##--------------------##
##-- ADD LOCAL DATA --##
##--------------------##

add.all.local.msa.data <- function(surv,
                                   dir='cleaned_data/continuum/msa',
                                   verbose=T)
{
    add.all.local.data(surv,
                       dir,
                       verbose=verbose)
}

add.all.local.state.data <- function(surv,
                                     dir='cleaned_data/continuum/state',
                                     verbose=T)
{
    add.all.local.data(surv,
                       dir,
                       verbose=verbose)
}

add.all.local.county.data <- function(surv,
                                     dir='cleaned_data/continuum/county',
                                     verbose=T)
{
    add.all.local.data(surv,
                       dir,
                       verbose=verbose)
}

add.all.local.data <- function(surv,
                               dir='cleaned_data/continuum/msa',
                               verbose=T)
{
    stratified.files = list.files(file.path(dir, 'stratified'))
    total.files = list.files(file.path(dir, 'total'))
    
    locations = unique(c(gsub('.csv', '', stratified.files), gsub('.csv', '', total.files)))
    
    for (location in locations)
    {
        total.file = file.path(dir, 'total', paste0(location, '.csv'))
        stratified.file = file.path(dir, 'stratified', paste0(location, '.csv'))
        
        if (!file.exists(total.file))
            total.file = NULL
        if (!file.exists(stratified.file))
            stratified.file = NULL
        
        print(location)
        surv = add.local.data.one.location(surv,
                                           location=location,
                                           total.file=total.file,
                                           stratified.file=stratified.file,
                                           verbose=verbose)
    }
    
    surv
}

add.local.data.one.location <- function(surv,
                                        location,
                                        total.file=NULL,
                                        stratified.file=NULL,
                                        verbose=T)
{
    if (verbose)
    {
        if (is.na(msa.names(location))) 
            print(paste0("Processing data for location '", location, "'"))
        else
            print(paste0("Processing data for MSA ", location, " (", msa.names(location), ")"))
    }
    
    # Read stratified file    
    if (!is.null(stratified.file))
    {
        COL.NAME.MAPPING = c(Data_Type='data.type',
                             Year='year',
                             Total='total',
                             Black='black',
                             Hispanic='hispanic',
                             Other='other',
                             Male='male',
                             Female='female',
                             '13-24'='13-24 years',
                             '25-34'='25-34 years',
                             '35-44'='35-44 years',
                             '45-54'='45-54 years',
                             '55+'='55+ years',
                             MSM='msm',
                             IDU='idu',
                             'MSM+IDU'='msm_idu',
                             Heterosexual='heterosexual',
                             Months='months',
                             Source='source',
                             URL='url',
                             Details='details')
        
        REQUIRED.FOR.STRATIFIED = COL.NAME.MAPPING[4:(length(COL.NAME.MAPPING)-3)]
        
        df = read.csv(stratified.file, stringsAsFactors = F, header = F)
        col.names = df[,1]
        if (length(setdiff(col.names, names(COL.NAME.MAPPING)))>1)
            stop(paste0("Invalid Row Names in file '", file, "': ",
                        paste0("'", setdiff(col.names, names(COL.NAME.MAPPING)), "'", collapse=', ')))
        df = as.data.frame(t(df[,-1]), stringsAsFactors = F)
        dimnames(df)[[2]]=COL.NAME.MAPPING[col.names]
        
        df = df[apply(!is.na(df), 1, any),]
        df = df[,!is.na(names(df))]
        
        for (col in 2:dim(df)[2])
        {
            if (names(df)[col] != 'source' && names(df)[col] != 'url' && names(df)[col] != 'details')
                df[,col] = as.numeric(as.character(df[,col]))
        }
        
        #     if (any(df$data.type=='aware' | df$data.type=='suppressed' | df$data.type=='prevalent'))
        #     {
        df = df[df$data.type=='aware' | df$data.type=='suppressed' | df$data.type=='prevalent' | 
                    df$data.type=='linked' | df$data.type=='new' | df$data.type=='engaged',]
        df = df[,!is.na(names(df))]
        df = df[apply(!is.na(df), 1, any),]
        
        for (i in 1:dim(df)[1])
        {
            if (df$data.type[i]=='aware')
                data.type = 'diagnosed'
            else if (df$data.type[i]=='prevalent')
                data.type = 'prevalence.for.continuum'
            else if (df$data.type[i]=='suppressed')
                data.type = 'suppression'
            else if (df$data.type[i]=='linked')
                data.type = 'linkage'
            else if (df$data.type[i]=='new')
                data.type = 'new.for.continuum'
            else if (df$data.type[i]=='engaged')
                data.type = 'engagement'
            else
                stop("Invalid data.type")
            
            if (any(!is.na(df[i,c('female','male')])))
                surv = add.surveillance.data(surv,
                                             data.type=data.type,
                                             location=location,
                                             years=df$year[i],
                                             values=df[i,c('female','male')],
                                             source.for.all=df$source[i],
                                             url.for.all=df$url[i],
                                             details.for.all=df$details[i],
                                             by.sex=T)
            
            if (any(!is.na(df[i,c('black','hispanic','other')])))
                surv = add.surveillance.data(surv,
                                             data.type=data.type,
                                             location=location,
                                             years=df$year[i],
                                             values=df[i,c('black','hispanic','other')],
                                             source.for.all=df$source[i],
                                             url.for.all=df$url[i],
                                             details.for.all=df$details[i],
                                             by.race=T)
            
            if (any(!is.na(df[i,c('msm','idu','msm_idu','heterosexual')])))
                surv = add.surveillance.data(surv,
                                             data.type=data.type,
                                             location=location,
                                             years=df$year[i],
                                             values=df[i,c('msm','idu','msm_idu','heterosexual')],
                                             source.for.all=df$source[i],
                                             url.for.all=df$url[i],
                                             details.for.all=df$details[i],
                                             by.risk=T)
            
            if (any(!is.na(df[i,c('13-24 years','25-34 years','35-44 years','45-54 years','55+ years')])))
                surv = add.surveillance.data(surv,
                                             data.type=data.type,
                                             location=location,
                                             years=df$year[i],
                                             values=df[i,c('13-24 years','25-34 years','35-44 years','45-54 years','55+ years')],
                                             source.for.all=df$source[i],
                                             url.for.all=df$url[i],
                                             details.for.all=df$details[i],
                                             by.age=T)
            
            if (!is.na(df[i,'total']))
                surv = add.surveillance.data(surv,
                                             data.type=data.type,
                                             location=location,
                                             years=df$year[i],
                                             source.for.all=df$source[i],
                                             url.for.all=df$url[i],
                                             details.for.all=df$details[i],
                                             values=df[i,'total'])
            #}
        }
    }
    
    #Read total file
    if (!is.null(total.file))
    {
        df = read.csv(total.file, stringsAsFactors = F)
        df = df[apply(!is.na(df), 1, any),]
        
        if (any(is.na(df[,1])))
            stop(paste0("Error in processing total file for '", location, "' - missing years. This probably means that the csv file '", total.file,"' is malformed"))
            
        if (any(names(df)=='aware'))
            surv = add.surveillance.data(surv,
                                         data.type='diagnosed',
                                         location=location,
                                         years=df[,1],
                                         source.for.year=df$source,
                                         url.for.year=df$url,
                                         details.for.year=df$details,
                                         values=df$aware)
        
        if (any(names(df)=='suppressed'))
            surv = add.surveillance.data(surv,
                                         data.type='suppression',
                                         location=location,
                                         years=df[,1],
                                         source.for.year=df$source,
                                         url.for.year=df$url,
                                         details.for.year=df$details,
                                         values=df$suppressed)
        if (any(names(df)=='prevalent'))
            surv = add.surveillance.data(surv,
                                         data.type='prevalence.for.continuum',
                                         location=location,
                                         years=df[,1],
                                         source.for.year=df$source,
                                         url.for.year=df$url,
                                         details.for.year=df$details,
                                         values=df$prevalent)
        if (any(names(df)=='linked'))
            surv = add.surveillance.data(surv,
                                         data.type='linkage',
                                         location=location,
                                         years=df[,1],
                                         source.for.year=df$source,
                                         url.for.year=df$url,
                                         details.for.year=df$details,
                                         values=df$linked)
        if (any(names(df)=='new'))
            surv = add.surveillance.data(surv,
                                         data.type='new.for.continuum',
                                         location=location,
                                         years=df[,1],
                                         source.for.year=df$source,
                                         url.for.year=df$url,
                                         details.for.year=df$details,
                                         values=df$new)
        
        if (any(names(df)=='engaged'))
            surv = add.surveillance.data(surv,
                                         data.type='engagement',
                                         location=location,
                                         years=df[,1],
                                         source.for.year=df$source,
                                         url.for.year=df$url,
                                         details.for.year=df$details,
                                         values=df$engaged)
    }
    
    surv
}

add.surveillance.data <- function(surv,
                                  data.type,
                                  location,
                                  years,
                                  values,
                                  by.race=F,
                                  by.age=F,
                                  by.sex=F,
                                  by.risk=F,
                                  source.for.all=NA,
                                  url.for.all=NA,
                                  details.for.all=NA,
                                  source.for.year=NA,
                                  url.for.year=NA,
                                  details.for.year=NA)
{   
    years = as.character(years)
    location = as.character(location)
    
    #-- Map the source and url info --#
    
    # Add mappings
    surv = add.surveillance.mapping.codes(surv, source.for.all)
    surv = add.surveillance.mapping.codes(surv, source.for.year)
    surv = add.surveillance.mapping.codes(surv, url.for.all)
    surv = add.surveillance.mapping.codes(surv, url.for.year)
    surv = add.surveillance.mapping.codes(surv, details.for.all)
    surv = add.surveillance.mapping.codes(surv, details.for.year)
    
    # Combine and get codes for sources
    if (length(source.for.year) > 0)
    {
        if (is.null(names(source.for.year)) && length(source.for.year)==length(years))
            names(source.for.year) = as.character(years)
            
        if (is.null(names(source.for.year)))
            stop("'source.for.year' must be either a named vector or a vector with the same length as 'years'")
    }
    source.codes = sapply(years, function(year){
        join.mappings(c(get.surveillance.mapping.codes(surv, source.for.all, collapse=T),
                        get.surveillance.mapping.codes(surv, source.for.year[as.character(year)], collapse=T)))
    })
    names(source.codes) = as.character(years)
    
    # Combine and get codes for URLs
    if (length(url.for.year) > 0)
    {
        if (is.null(names(url.for.year)) && length(url.for.year)==length(years))
            names(url.for.year) = as.character(years)
        
        if (is.null(names(url.for.year)))
            stop("'url.for.year' must be either a named vector or a vector with the same length as 'years'")
    }
    url.codes = sapply(years, function(year){
        join.mappings(c(get.surveillance.mapping.codes(surv, url.for.all, collapse = T), 
                        get.surveillance.mapping.codes(surv, url.for.year[as.character(year)], collapse=T)))
    })
    names(url.codes) = as.character(years)
    
    # Combine and get codes for URLs
    if (length(details.for.year) > 0)
    {
        if (is.null(names(details.for.year)) && length(details.for.year)==length(years))
            names(details.for.year) = as.character(years)
        
        if (is.null(names(details.for.year)))
            stop("'details.for.year' must be either a named vector or a vector with the same length as 'years'")
    }
    details.codes = sapply(years, function(year){
        join.mappings(c(get.surveillance.mapping.codes(surv, details.for.all, collapse = T), 
                        get.surveillance.mapping.codes(surv, details.for.year[as.character(year)], collapse=T)))
    })
    names(details.codes) = as.character(years)
    
    
    #-- Set up dim names --#
    if (by.race)
    {
        data.name = paste0(data.type, '.race')
        to.add.dim.names = list(year=as.character(years), location=location, 
                                race=c('black','hispanic','other'))
    }
    else if (by.age)
    {
        data.name = paste0(data.type, '.age')
        to.add.dim.names = list(year=as.character(years), location=location, 
                                age=c('13-24 years','25-34 years','35-44 years','45-54 years','55+ years'))
    }
    else if (by.sex)
    {
        data.name = paste0(data.type, '.sex')
        to.add.dim.names = list(year=as.character(years), location=location, 
                                sex=c('female','male'))
    }
    else if (by.risk)
    {
        data.name = paste0(data.type, '.risk')
        to.add.dim.names = list(year=as.character(years), location=location, 
                                risk=c('msm','idu','msm_idu','heterosexual'))
    }
    else
    {
        data.name = paste0(data.type, '.all')
        to.add.dim.names = list(year=as.character(years), location=location)
    }
    
    if (by.race || by.sex || by.age || by.risk)
    {
        if (is.null(dim(values)))
        {
            if (length(years)==1)
                values = as.numeric(values[to.add.dim.names[[3]]])
            else
                stop("'values' must have two dimensions")
        }
        else if (length(years)==1)
            values = as.numeric(values[1, to.add.dim.names[[3]]])
        else
            values = as.numeric(values[as.character(years), to.add.dim.names[[3]]])
    }
    else
    {
        if (!is.null(names(values)))
            values = as.numeric(values[as.character(years)])
    }
    
    dim(values) = sapply(to.add.dim.names, length)
    dimnames(values) = to.add.dim.names
    
    old.values = surv[[data.name]]
    old.source.values = surv$source[[data.name]]
    old.url.values = surv$url[[data.name]]
    old.details.values = surv$details[[data.name]]
    
    if (is.null(old.values))
        new.dim.names = to.add.dim.names
    else
    {
        old.dim.names = new.dim.names = dimnames(old.values)
        if (length(setdiff(to.add.dim.names[['year']], old.dim.names[['year']]))>0)
            new.dim.names[['year']] = sort(union(old.dim.names[['year']],
                                                 to.add.dim.names[['year']]))
        if (length(setdiff(to.add.dim.names[['location']], old.dim.names[['location']]))>0)
            new.dim.names[['location']] = union(old.dim.names[['location']],
                                                to.add.dim.names[['location']])
    }
    
    surv[[data.name]] = array(as.numeric(NA),
                              dim=sapply(new.dim.names, length),
                              dimnames=new.dim.names)
    surv$source[[data.name]] =
        surv$url[[data.name]] = 
        surv$details[[data.name]] = array(as.character(NA),
                                          dim=sapply(new.dim.names, length),
                                          dimnames=new.dim.names)
    
    
    if (by.race || by.sex || by.age || by.risk)
    {
        if (!is.null(old.values))
            surv[[data.name]][old.dim.names[['year']], old.dim.names[['location']],] = old.values[old.dim.names[['year']], old.dim.names[['location']],]
        if (!is.null(old.source.values))
            surv$source[[data.name]][old.dim.names[['year']], old.dim.names[['location']],] = old.source.values[old.dim.names[['year']], old.dim.names[['location']],]
        if (!is.null(old.url.values))
            surv$url[[data.name]][old.dim.names[['year']], old.dim.names[['location']],] = old.url.values[old.dim.names[['year']], old.dim.names[['location']],]
        if (!is.null(old.details.values))
            surv$details[[data.name]][old.dim.names[['year']], old.dim.names[['location']],] = old.details.values[old.dim.names[['year']], old.dim.names[['location']],]
        
        surv[[data.name]][as.character(years), location,] = values[as.character(years), location,]
        
        for (year in years)
        {
            surv$source[[data.name]][as.character(year), location,] = source.codes[as.character(year)]
            surv$url[[data.name]][as.character(year), location,] = url.codes[as.character(year)]
            surv$details[[data.name]][as.character(year), location,] = details.codes[as.character(year)]
        }
    }
    else
    {
        if (!is.null(old.values))
            surv[[data.name]][old.dim.names[['year']], old.dim.names[['location']]] = old.values[old.dim.names[['year']], old.dim.names[['location']]]
        if (!is.null(old.source.values))
            surv$source[[data.name]][old.dim.names[['year']], old.dim.names[['location']]] = old.source.values[old.dim.names[['year']], old.dim.names[['location']]]
        if (!is.null(old.url.values))
            surv$url[[data.name]][old.dim.names[['year']], old.dim.names[['location']]] = old.url.values[old.dim.names[['year']], old.dim.names[['location']]]
        if (!is.null(old.details.values))
            surv$details[[data.name]][old.dim.names[['year']], old.dim.names[['location']]] = old.details.values[old.dim.names[['year']], old.dim.names[['location']]]
        
        surv[[data.name]][as.character(years), location] = values[as.character(years), location]
        surv$source[[data.name]][as.character(years), location] = source.codes
        surv$url[[data.name]][as.character(years), location] = url.codes
        surv$details[[data.name]][as.character(years), location] = details.codes
    }
    
    surv
}


##------------------------------##
##-- SOURCE, URL, and DETAILS --##
##------------------------------##

create.surveillance.manager <- function()
{
    list(
        code.map = character(),
        
        source=list(),
        url=list(),
        details=list()
    )
}

add.surveillance.mapping.codes <- function(surv,
                                           values.to.add)
{
    for (value in values.to.add)
    {
        if (!is.na(value) && all(surv$code.map!=value))
            surv$code.map = c(surv$code.map, value)
    }
    
    surv
}

get.surveillance.mapping.codes <- function(surv,
                                           values,
                                           collapse)
{
    rv = sapply(values, function(value){
        if (is.na(value) || is.null(value))
            NA
        else
        {
            mask = surv$code.map == value
            if (any(mask))
                (1:length(mask))[mask][1]
            else
                stop(paste0("'", value, "' is not a registered code in the surveillance manager"))
        }
    })
    
    
    rv = rv[!is.na(rv)]
    
    if (collapse)
        paste0(rv, collapse=SURVEILLANCE.DELIMITER)
    else
    {
        names(rv) = names(values)
        rv
    }
}

join.mappings <- function(codes)
{
    if (is.character(codes))
        codes = unlist(strsplit(codes, SURVEILLANCE.DELIMITER, fixed=T))
    codes = codes[!is.na(codes) & codes != '' & codes != 'NA']

    paste0(unique(codes), collapse=SURVEILLANCE.DELIMITER)
}

arr.join.mappings <- function(...)
{
    args = list(...)
    if (length(args)==0)
        stop("No arguments passed to arr.join.mappings")
    else if (length(args)==1)
        args[[1]]
    else if (length(args)==2)
    {
        codes1 = args[[1]]
        codes2 = args[[2]]
        
        if (length(codes1) != length(codes2))
            stop("Non-conforming arrays in arr.join.mappings")
        
        rv = sapply(1:length(codes1), function(index){
            join.mappings(c(codes1[index], codes2[index]))
        })
    
        dim(rv) = dim(codes1)
        dimnames(rv) = dimnames(codes2)
    
        rv
    }
    else
        arr.join.mappings(args[[1]], args[[-1]])
}

# returns a list with two elements
# $surv - the updated surveillance manager
# $arr - the updated array (with codes corresponding to the updated surveillance manager)
import.surveillance.mapping.codes <- function(import.to.surv,
                                              arr,
                                              import.from.surv)
{
    codes.to.import = unique(unlist(strsplit(as.character(arr), split=SURVEILLANCE.DELIMITER, fixed=T)))
    codes.to.import = codes.to.import[!is.na(codes.to.import) & codes.to.import != '' & codes.to.import!='NA']
    codes.to.import = as.integer(codes.to.import)
    
    values.to.import = import.from.surv$code.map[codes.to.import]
    import.to.surv = add.surveillance.mapping.codes(import.to.surv,
                                                    values.to.add = values.to.import)
    code.mapping = get.surveillance.mapping.codes(import.to.surv, values=values.to.import, collapse=F)
    
    names(code.mapping) = as.character(codes.to.import)
    
    new.arr = sapply(as.character(arr), function(val){
        codes = strsplit(val, split=SURVEILLANCE.DELIMITER, fixed=T)[[1]]
        join.mappings(code.mapping[codes])
    })
    dim(new.arr) = dim(arr)
    dimnames(new.arr) = dimnames(arr)
    
    list(surv=import.to.surv,
         arr=new.arr)
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
                                              sources,
                                              urls,
                                              details,
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
    #-- Add Mappings for source and url --#
    surv = add.surveillance.mapping.codes(surv, sources)
    surv = add.surveillance.mapping.codes(surv, urls)
    surv = add.surveillance.mapping.codes(surv, details)
    
    source.code = get.surveillance.mapping.codes(surv, sources, collapse=T)
    url.code = get.surveillance.mapping.codes(surv, urls, collapse=T)
    details.code = get.surveillance.mapping.codes(surv, details, collapse=T)
    
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
                !is.na(marginal.by.dim[catg]) && !marginal.by.dim[catg]
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
            surv$source[[arr.name]] = create.or.expand.surveillance.array(surv$source[[arr.name]],
                                                                          new.dim.names=master.dim.names[dim.names.mask],
                                                                          default.value = as.character(NA))
            surv$url[[arr.name]] = create.or.expand.surveillance.array(surv$url[[arr.name]],
                                                                       new.dim.names=master.dim.names[dim.names.mask],
                                                                       default.value = as.character(NA))
            surv$details[[arr.name]] = create.or.expand.surveillance.array(surv$details[[arr.name]],
                                                                       new.dim.names=master.dim.names[dim.names.mask],
                                                                       default.value = as.character(NA))

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
            
            surv$source[[arr.name]][arr.index] = source.code
            surv$url[[arr.name]][arr.index] = url.code
            surv$details[[arr.name]][arr.index] = details.code
        }
        
        # Crunch the calculated values
        calculated.values = CALCULATE.VALUES.FOR.OUTCOME.FROM.NUMERATOR.AND.DENOMINATOR[[ df$outcome[i] ]]
        for (outcome.name in calculated.values)
        {
            arr.name = paste0(outcome.name, ".", arr.suffix)
            
            # Make sure the array can accomodate these dimensions
            surv[[arr.name]] = create.or.expand.surveillance.array(surv[[arr.name]],
                                                                   new.dim.names=master.dim.names[dim.names.mask])
            surv$source[[arr.name]] = create.or.expand.surveillance.array(surv$source[[arr.name]],
                                                                          new.dim.names=master.dim.names[dim.names.mask],
                                                                          default.value = as.character(NA))
            surv$url[[arr.name]] = create.or.expand.surveillance.array(surv$url[[arr.name]],
                                                                       new.dim.names=master.dim.names[dim.names.mask],
                                                                       default.value = as.character(NA))
            surv$details[[arr.name]] = create.or.expand.surveillance.array(surv$details[[arr.name]],
                                                                           new.dim.names=master.dim.names[dim.names.mask],
                                                                           default.value = as.character(NA))
            
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
            
            surv$source[[arr.name]][arr.index] = source.code
            surv$url[[arr.name]][arr.index] = url.code
            surv$details[[arr.name]][arr.index] = details.code
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
                                                new.dim.names,
                                                default.value=NaN)
{
    if (is.null(arr))
    {
        array(default.value, dim=sapply(new.dim.names, length), dimnames = new.dim.names)
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
        
        rv = array(default.value, dim=sapply(new.dim.names, length), dimnames=new.dim.names)
        
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

subset.surveillance.array.by.location <- function(arr,
                                                  locations)
{
    rv.dim.names = dimnames(arr)
    rv.dim.names$location = locations
    
    collapsed.dim.names = c(dimnames(arr)[1:2],
                            other=NULL)
    collapsed.dim = c(dim(arr)[1:2],
                      other = prod(dim(arr))/prod(dim(arr)[1:2])
    )
    
    dim(arr) = collapsed.dim
    dimnames(arr) = collapsed.dim.names
    
    arr = arr[,locations,]
    
    dim(arr) = sapply(rv.dim.names, length)
    dimnames(arr) = rv.dim.names
    
    arr
}

overwrite.surveillance.array.by.location <- function(arr,
                                                     new.data)
{
    rv.dim.names = dimnames(arr)
    
    collapsed.dim.names = c(dimnames(arr)[1:2],
                            other=NULL)
    collapsed.dim = c(dim(arr)[1:2],
                      other = prod(dim(arr))/prod(dim(arr)[1:2])
    )
    
    
    dim(arr) = collapsed.dim
    dimnames(arr) = collapsed.dim.names
    
    new.locations = dimnames(new.data)$location
    arr[,new.locations,] = as.numeric(new.data)
    
    dim(arr) = sapply(rv.dim.names, length)
    dimnames(arr) = rv.dim.names
    
    arr
}

list.is.subset <- function(l.sub, l.super)
{
    all(sapply(names(l.sub), function(elem.name){
        length(setdiff(l.sub[[elem.name]], l.super[[elem.name]]))==0
    }))
}

##-----------------##
##-- ADDING PrEP --##
##-----------------##

add.prep.data <- function(surv,
                          prep.manager,
                          locations = dimnames(surv$new.all)[['location']],
                          
                          source='AIDSVu',
                          url='https://aidsvu.org/resources/#/datasets',
                          details='AIDSVu Local PrEP Mapping')
{
    # Set up codes for source, url, details
    surv = add.surveillance.mapping.codes(surv, values.to.add=c(source, url, details))
    source = get.surveillance.mapping.codes(surv, source, collapse=T)
    url = get.surveillance.mapping.codes(surv, url, collapse=T)
    details = get.surveillance.mapping.codes(surv, details, collapse=T)
    
    # All
    all.dim.names = list(year=prep.manager$years,
                         location=locations,
                         sex=dimnames(prep.manager$prep.sex)[['sex']],
                         age=dimnames(prep.manager$prep.age)[['age']])
    surv$prep.all = sapply(locations, get.prep.data, prep.manager=prep.manager, na.rm=T)
    dim(surv$prep.all) = sapply(all.dim.names[c('year','location')], length)
    dimnames(surv$prep.all) = all.dim.names[c('year','location')]
    
    surv$source$prep.all = surv$url$prep.all = surv$details$prep.all =
        array(as.character(NA), dim=dim(surv$prep.all), dimnames=dimnames(surv$prep.all))
    surv$source$prep.all[!is.na(surv$prep.all)] = source
    surv$url$prep.all[!is.na(surv$prep.all)] = url
    surv$details$prep.all[!is.na(surv$prep.all)] = details
    
    # By Sex
    surv$prep.sex = sapply(locations, get.prep.data, prep.manager=prep.manager, sex=T, na.rm=T)
    dim(surv$prep.sex) = sapply(all.dim.names[c('year','sex','location')], length)
    dimnames(surv$prep.sex) = all.dim.names[c('year','sex','location')]
    surv$prep.sex = apply(surv$prep.sex, c('year','location','sex'), function(x){x})
    
    surv$source$prep.sex = surv$url$prep.sex = surv$details$prep.sex =
        array(as.character(NA), dim=dim(surv$prep.sex), dimnames=dimnames(surv$prep.sex))
    surv$source$prep.sex[!is.na(surv$prep.sex)] = source
    surv$url$prep.sex[!is.na(surv$prep.sex)] = url
    surv$details$prep.sex[!is.na(surv$prep.sex)] = details
    
    
    # By Age
    surv$prep.age = sapply(locations, get.prep.data, prep.manager=prep.manager, age=T, na.rm=T)
    dim(surv$prep.age) = sapply(all.dim.names[c('year','age','location')], length)
    dimnames(surv$prep.age) = all.dim.names[c('year','age','location')]
    surv$prep.age = apply(surv$prep.age, c('year','location','age'), function(x){x})
    
    surv$source$prep.age = surv$url$prep.age = surv$details$prep.age =
        array(as.character(NA), dim=dim(surv$prep.age), dimnames=dimnames(surv$prep.age))
    surv$source$prep.age[!is.na(surv$prep.age)] = source
    surv$url$prep.age[!is.na(surv$prep.age)] = url
    surv$details$prep.age[!is.na(surv$prep.age)] = details
    
    #Normalize to total (in case missing data)
    #    total.by.location = pmax(surv$prep.all,
    #                             pmax(rowSums(surv$prep.sex, dims=2),
    #                                  rowSums(surv$prep.age, dims=2),
    #                                  na.rm=T),
    #                             na.rm=T)
    
    #    surv$prep.all = total.by.location
    
    
    # Return
    surv
}

##--------------------------##
##-- MSA-SPECIFIC HELPERS --##
##--------------------------##


read.msa.file <- function(file, verbose=T, allow.misses=F)
{
    if (verbose)
        print(paste0("Reading MSA file: '", file, "'"))
    df = read.csv(file, stringsAsFactors = F)
    
    df[,1] = gsub('â€“', '-', df[,1])
    df[,1] = gsub('â€”', '-', df[,1])
    division.mask = grepl('Division', df[,1])
    df[!division.mask,1] = gsub('[a-z]$', '', df[!division.mask,1])
    
    #specific fixes
    df[df[,1]=='Louisiville, KY-IN',1] = 'Louisville, KY'
    
    df[,1] = map.state.abbreviations(df[,1])
    
    for (col in 2:dim(df)[2])
    {
        vals = df[,col]
        vals = gsub(' ', '', vals)
        vals = gsub(',', '', vals)
        vals[vals=='â€”' | vals=='-'] = NA
        vals[vals=='â€“' | vals=='-'] = NA
        df[,col] = as.numeric(vals)
    }
    
    #Map to MSA CBSAs/Division Codes
    df$code = NA
    df$code[!division.mask] = cbsa.for.msa.name(df[!division.mask,1], allow.wrong.state = T)
    
    for (i in (1:dim(df)[1])[division.mask])
        df$code[i] = df$code[i-1]
    
    #    has.total.row = any(grepl('total', df[,1], ignore.case = T))
    #    if ((has.total.row && sum(is.na(df$code))>4) ||
    #        (!has.total.row && sum(is.na(df$code)>0)))
    if (!allow.misses && any(is.na(df$code) &
                             !(grepl('total', df[,1], ignore.case = T) |
                               grepl('metropolitan', df[,1], ignore.case = T) |
                               grepl('countie', df[,1], ignore.case = T)
                             )))
    {
    #    stop('missing codes in df - consider putting a browser statement here')
        print("MISSING CODES IN df - going to browser()")
         browser()
         
         df[is.na(df$code),]
    }
    
    division.mask = division.mask[!is.na(df$code)]
    df = df[!is.na(df$code),]
    
    code.counts = table(df$code)
    known.divisions = unique(df$code[division.mask])
    
    unknown.divisions = setdiff(names(code.counts[code.counts>1]), known.divisions)
    division.mask = division.mask | sapply(df$code, function(code){any(code==unknown.divisions)})
    
    if (any(division.mask))
    {
        division.name = gsub(' Division', '', df[division.mask,1])
        division.name = gsub(',.*$', '', division.name)
        df$code[division.mask] = msa.division.code.for.name(division.name,
                                                            df$code[division.mask])
    }
    
    
    #We should only have 4 NA codes = subtotals x3, and total
    #   if (sum(is.na(df$code))>0)#4)
    #      browser()
    #        stop("Unable to match some MSA names")
    df = df[!is.na(df$code),]
    division.mask = division.mask[!is.na(df$code)]
    
    code.counts = table(df$code)
    if (any(code.counts>1))
    {
        duplicate.codes = names(code.counts[code.counts>1])
        for (code in duplicate.codes)
        {
            first.index = (1:dim(df)[1])[df$code==code][1]
            tally.indices = 2:(dim(df)[2]-1)
            
            if (length(tally.indices)==1)
                df[first.index,tally.indices] = sum(df[df$code==code,tally.indices])
            else
                df[first.index,tally.indices] = colSums(df[df$code==code,tally.indices])
            df = df[(1:dim(df)[1]) != first.index | df$code != code,]
        }
    }
    
    dimnames(df)[[1]] = as.character(df$code)
    
    #Return
    df
}

map.state.abbreviations <- function(msa.strings)
{
    state.regex = '(^.*,) *(.+)$'
    state.mask = grepl(state.regex, msa.strings) & !grepl('populatio', msa.strings) &
        !grepl('^.*, .*-.*$', msa.strings)
    
    unabbreviated.states = gsub(state.regex, '\\2', msa.strings[state.mask])
    
    matched.states = robust.match.state.name(unabbreviated.states)
    
    rv = msa.strings
    rv[state.mask] = paste0(gsub(state.regex, '\\1 ', rv[state.mask]), matched.states)
    
    
    rv
}


read.cum.aids.mortality <- function(file='../data2/HIV_Surveillance/by_msa/msa_mortality_pre_2000.txt')
{
    df = read.table(file, sep='\t', stringsAsFactors = F, header = T)
    df$code = cbsa.for.msa.name(df$Location)
    df = df[!is.na(df$code),]
    
    SEX.CODES = c('male','male','male','female')
    RISK.CODES = c('msm',
                   'idu',
                   'msm_idu',
                   'heterosexual', #hemophilia
                   'heterosexual',
                   NA,
                   'heterosexual', #blood
                   'unknown',
                   'heterosexual', #pediatric hemophilia
                   'heterosexual', #mother
                   'heterosexual', #pediatric blood
                   'heterosexual') #other peds
    RACE.CODES = c('1002-5'='other', #American Indian
                   'A-PI'='other', #Asian
                   '2054-5'='black',
                   '2135-2'='hispanic',
                   '2106-3'='other', #White
                   'U'='other') #unknown
    
    dim.names = list(location=unique(df$code),
                     sex=c('male','female'),
                     race=c('black','hispanic','other'),
                     risk=c('msm','idu','msm_idu','heterosexual','unknown'))
    rv = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    for (i in 1:dim(df)[1])
    {
        rv[df$code[i], SEX.CODES[df$Sex.and.Sexual.Orientation.Code[i]], RACE.CODES[df$Race.or.Ethnicity.Code[i]], RISK.CODES[df$HIV.Exposure.Category.Code[i]]] =
            rv[df$code[i], SEX.CODES[df$Sex.and.Sexual.Orientation.Code[i]], RACE.CODES[df$Race.or.Ethnicity.Code[i]], RISK.CODES[df$HIV.Exposure.Category.Code[i]]] +
            df$Cases[i]
    }
    
    risk.multiplier = rowSums(rv) / rowSums(rv[,,,1:4])
    rv = rv[,,,1:4] * risk.multiplier
    
    rv
}


read.aids.diagnoses <- function(file='../data2/alt_Surveillance/by_msa/aids_diagnoses_pre_2002.txt')
{
    df = read.table(file, sep='\t', stringsAsFactors = F, header = T)
    df$code = cbsa.for.msa.name(df$Location)
    df = df[!is.na(df$code),]
    df$Year.Reported = as.character(df$Year.Reported)
    df$Year.Reported[df$Year.Reported=='Before 1982'] = '1981'
    
    dim.names = list(year = sort(unique(df$Year.Reported)), location=unique(df$code))
    rv = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    for (i in 1:dim(df)[1])
        rv[df$Year.Reported[i], df$code[i]] = rv[df$Year.Reported[i], df$code[i]] + df$Cases[i]
    
    rv
}


import.one.county.msa.data <- function(msa.surveillance,
                                       county.surveillance,
                                       overwrite=F)
{   
    to.update = names(county.surveillance)[sapply(county.surveillance, is.array)]
    
    all.msas = character()
    for (elem in msa.surveillance)
    {
        if (is.array(elem))
            all.msas = union(all.msas, dimnames(elem)$location)
    }
    
    is.one.county = sapply(all.msas, function(msa){
        length(counties.for.msa(msa))==1
    })
    all.msas = all.msas[is.one.county]
    all.counties = sapply(all.msas, counties.for.msa)
    
    for (elem.name in to.update)
    {
        print(elem.name)
        
        # Pull the relevant data from county and msa surveillance
        msa.elem = msa.surveillance[[elem.name]]
        county.elem = county.surveillance[[elem.name]]
        
        msa.source = msa.surveillance$source[[elem.name]]
        county.source = county.surveillance$source[[elem.name]]
        
        msa.url = msa.surveillance$url[[elem.name]]
        county.url = county.surveillance$url[[elem.name]]
        
        msa.details = msa.surveillance$details[[elem.name]]
        county.details = county.surveillance$details[[elem.name]]
        
        
        # Pull the locations we're going to use
        have.county = sapply(all.counties, function(county){
            any(county==dimnames(county.elem)$location)
        })
        
        msas = all.msas[have.county]
        counties = all.counties[have.county]
        
        if (length(msas)>0)
        {
            # subset the county data
            county.elem.subset = subset.surveillance.array.by.location(county.elem,
                                                                       locations=counties)
            county.source.subset = subset.surveillance.array.by.location(county.source,
                                                                         locations=counties)
            county.url.subset = subset.surveillance.array.by.location(county.url,
                                                                         locations=counties)
            county.details.subset = subset.surveillance.array.by.location(county.details,
                                                                         locations=counties)
            
            # expand dimensions of msa data to incorporate all county years
            combined.years = sort(union(dimnames(county.elem)$year,
                                        dimnames(msa.elem)$year))
            
            new.msa.dim.names = dimnames(county.elem)
            new.msa.dim.names$year = combined.years
            new.msa.dim.names$location = msas
            
            new.msa.elem = create.or.expand.surveillance.array(msa.elem,
                                                               new.dim.names=new.msa.dim.names)
            new.msa.source = create.or.expand.surveillance.array(msa.source,
                                                                 new.dim.names=new.msa.dim.names,
                                                                 default.value=as.character(NA))
            new.msa.url = create.or.expand.surveillance.array(msa.url,
                                                                 new.dim.names=new.msa.dim.names,
                                                                 default.value=as.character(NA))
            new.msa.details = create.or.expand.surveillance.array(msa.details,
                                                                 new.dim.names=new.msa.dim.names,
                                                                 default.value=as.character(NA))
            
            
            # align two subsets so there is a 1:1 correspondence between counties/msas
            new.msa.elem.subset = subset.surveillance.array.by.location(new.msa.elem,
                                                                        locations=msas)
            
            new.msa.source.subset = subset.surveillance.array.by.location(new.msa.source,
                                                                        locations=msas)
            new.msa.url.subset = subset.surveillance.array.by.location(new.msa.url,
                                                                        locations=msas)
            new.msa.details.subset = subset.surveillance.array.by.location(new.msa.details,
                                                                        locations=msas)
            
            
            new.county.subset.dim.names = dimnames(county.elem.subset)
            new.county.subset.dim.names$year = combined.years
            
            county.elem.subset = create.or.expand.surveillance.array(county.elem.subset,
                                                                     new.dim.names=new.county.subset.dim.names)
            
            county.source.subset = create.or.expand.surveillance.array(county.source.subset,
                                                                       new.dim.names=new.county.subset.dim.names)
            county.url.subset = create.or.expand.surveillance.array(county.url.subset,
                                                                    new.dim.names=new.county.subset.dim.names)
            county.details.subset = create.or.expand.surveillance.array(county.details.subset,
                                                                        new.dim.names=new.county.subset.dim.names)
            
            # figure out what to copy and copy it
            if (overwrite)
                update.mask = !is.na(county.elem.subset)
            else
                update.mask = !is.na(county.elem.subset) & is.na(new.msa.elem.subset)
            
            new.msa.elem.subset[update.mask] = county.elem.subset[update.mask]
            new.msa.source.subset[update.mask] = county.source.subset[update.mask]
            new.msa.url.subset[update.mask] = county.url.subset[update.mask]
            new.msa.details.subset[update.mask] = county.details.subset[update.mask]
            
            # put it back into the full msa array and overwrite into the msa.surveillance object
            msa.surveillance[[elem.name]] = overwrite.surveillance.array.by.location(arr = new.msa.elem,
                                                                                     new.data = new.msa.elem.subset)
            
            msa.surveillance$source[[elem.name]] = 
                overwrite.surveillance.array.by.location(arr = new.msa.source,
                                                         new.data = new.msa.source.subset)
            msa.surveillance$url[[elem.name]] = 
                overwrite.surveillance.array.by.location(arr = new.msa.url,
                                                         new.data = new.msa.url.subset)
            msa.surveillance$details[[elem.name]] = 
                overwrite.surveillance.array.by.location(arr = new.msa.details,
                                                         new.data = new.msa.details.subset)
        }
    }

    msa.surveillance
}


##-------------------------##
##-- CORRECTING TO TOTAL --##
##-------------------------##


correct.to.county.totals <- function(msa.surveillance,
                                     county.surveillance,
                                     correct.new.to.county.level=T,
                                     correct.prevalence.to.county.level=T,
                                     verbose=T)
{
    rv = msa.surveillance
    county.surveillance.appended = county.surveillance
    county.surveillance.appended$code.map = paste0(county.surveillance.appended$code.map, " (summed county totals)")
    
    rv$params$correct.new.to.county.level=correct.new.to.county.level
    rv$params$correct.prevalence.to.county.level=correct.prevalence.to.county.level
    
    #-- AGGREGATED COUNTY-LEVEL TOTALS --#
    all.codes = dimnames(rv$new.all)[['location']]
    if (correct.new.to.county.level || correct.prevalence.to.county.level)
        msa.by.county = parse.total.msa.data.from.counties(county.surveillance = county.surveillance,
                                                          msas=as.character(all.codes))
    
    if (correct.new.to.county.level)
    {
        if (verbose)
            print("Correcting New Diagnoses to Aggregate County-Level Data")
        
        new.names.to.correct = names(rv)[grepl('new.', names(rv))]
        
        for (name in new.names.to.correct)
            rv[[name]] = correct.array.to.total(to.correct = rv[[name]],
                                                target.to = msa.by.county$new.all,
                                                backup.1 = rv$new.sex,
                                                backup.2 = rv$new.race,
                                                backup.3 = rv$new.risk,
                                                backup.4 = rv$new.sex.age)
        
        rv$new.all = msa.by.county$new.all
        
        update = import.surveillance.mapping.codes(import.to.surv=rv,
                                                   arr=msa.by.county$source$new.all,
                                                   import.from.surv = county.surveillance)
        rv = update$surv
        rv$source$new.all = update$arr
        
        update = import.surveillance.mapping.codes(import.to.surv=rv,
                                                   arr=msa.by.county$url$new.all,
                                                   import.from.surv = county.surveillance)
        rv = update$surv
        rv$url$new.all = update$arr
        
        update = import.surveillance.mapping.codes(import.to.surv=rv,
                                                   arr=msa.by.county$details$new.all,
                                                   import.from.surv = county.surveillance.appended)
        rv = update$surv
        rv$details$new.all = update$arr
        
    }
    if (correct.prevalence.to.county.level)
    {
        if (verbose)
            print("Correcting Prevalence to Aggregate County-Level Data")
        
        prevalence.names.to.correct = names(rv)[grepl('prevalence.', names(rv))]
        
        for (name in prevalence.names.to.correct)
            rv[[name]] = correct.array.to.total(to.correct = rv[[name]],
                                                target.to = msa.by.county$prevalence.all,
                                                backup.1 = rv$prevalence.sex,
                                                backup.2 = rv$prevalence.race,
                                                backup.3 = rv$prevalence.risk,
                                                backup.4 = rv$prevalence.sex.age)
        
        
        rv$prevalence.all = msa.by.county$prevalence.all
        
        update = import.surveillance.mapping.codes(import.to.surv=rv,
                                                   arr=msa.by.county$source$prevalence.all,
                                                   import.from.surv = county.surveillance)
        rv = update$surv
        rv$source$prevalence.all = update$arr
        
        update = import.surveillance.mapping.codes(import.to.surv=rv,
                                                   arr=msa.by.county$url$prevalence.all,
                                                   import.from.surv = county.surveillance)
        rv = update$surv
        rv$url$prevalence.all = update$arr
        
        update = import.surveillance.mapping.codes(import.to.surv=rv,
                                                   arr=msa.by.county$details$prevalence.all,
                                                   import.from.surv = county.surveillance.appended)
        rv = update$surv
        rv$details$prevalence.all = update$arr
    }
    
    rv
} 

#uses backups to calculate scaling factor if there are some NAs in target.to
correct.array.to.total <- function(to.correct,
                                   target.to,
                                   backup.1=NULL,
                                   backup.2=NULL,
                                   backup.3=NULL,
                                   backup.4=NULL)
{
    # Set up our access indices and dimension names
    to.correct.years = intersect(dimnames(to.correct)[['year']], dimnames(target.to)[['year']])
    to.correct.locations = intersect(dimnames(to.correct)[['location']], dimnames(target.to)[['location']])
    orig.dim = dim(to.correct)
    orig.dim.names = dimnames(to.correct)
    dim(to.correct) = c(dim(to.correct)[1:2], prod(dim(to.correct))/prod(dim(to.correct)[1:2]))
    dimnames(to.correct) = c(orig.dim.names[1:2], list(NULL))
    
    # Set up correct factors
    to.correct.totals = rowSums(to.correct, dims=2)[to.correct.years, to.correct.locations]
    correct.factor = target.to[to.correct.years, to.correct.locations] / to.correct.totals
    if (!is.null(backup.1) && any(is.na(correct.factor)))
        correct.factor[is.na(correct.factor)] = target.to[to.correct.years, to.correct.locations][is.na(correct.factor)] /
        rowSums(backup.1, dims=2)[to.correct.years, to.correct.locations][is.na(correct.factor)]
    if (!is.null(backup.2) && any(is.na(correct.factor)))
        correct.factor[is.na(correct.factor)] = target.to[to.correct.years, to.correct.locations][is.na(correct.factor)] /
        rowSums(backup.2, dims=2)[to.correct.years, to.correct.locations][is.na(correct.factor)]
    if (!is.null(backup.3) && any(is.na(correct.factor)))
        correct.factor[is.na(correct.factor)] = target.to[to.correct.years, to.correct.locations][is.na(correct.factor)] /
        rowSums(backup.3, dims=2)[to.correct.years, to.correct.locations][is.na(correct.factor)]
    if (!is.null(backup.4) && any(is.na(correct.factor)))
        correct.factor[is.na(correct.factor)] = target.to[to.correct.years, to.correct.locations][is.na(correct.factor)] /
        rowSums(backup.4, dims=2)[to.correct.years, to.correct.locations][is.na(correct.factor)]
    
    # Make the correction
    corrected = to.correct
    corrected[to.correct.years, to.correct.locations, ] = as.numeric(correct.factor) *
        as.numeric(to.correct[to.correct.years, to.correct.locations, ])
    
    # Reset original dims
    dim(corrected) = orig.dim
    dimnames(corrected) = orig.dim.names
    
    corrected
}

parse.total.msa.data.from.counties <- function(msas,
                                               county.surveillance,
                                               ignore.missing=T)
{
    new.years = dimnames(county.surveillance$new.all)$year
    prev.years = dimnames(county.surveillance$prevalence.all)$year
    
    new.dim.names = list(year=new.years, location=msas)
    prev.dim.names = list(year=prev.years, location=msas)
    
    rv = list(source=list(),
              url=list(),
              details=list())
    rv$new.all = array(as.numeric(-1), dim=sapply(new.dim.names, length), dimnames=new.dim.names)
    rv$prevalence.all = array(as.numeric(-1), dim=sapply(prev.dim.names, length), dimnames=prev.dim.names)
    
    rv$source$new.all = rv$url$new.all = rv$details$new.all =
        array(as.character(NA), dim=sapply(new.dim.names, length), dimnames=new.dim.names)
    rv$source$prevalence.all = rv$url$prevalence.all = rv$details$prevalence.all =
        array(as.character(NA), dim=sapply(prev.dim.names, length), dimnames=prev.dim.names)
    
    # New (plus source, url, details)
    rv$new.all = sapply(msas, function(msa){
        msa.counties = counties.for.msa.or.division(msa)
        if (length(msa.counties)==1)
            county.surveillance$new.all[,msa.counties]
        else
            rowSums(county.surveillance$new.all[,msa.counties], na.rm=ignore.missing)
    })
    rv$source$new.all = sapply(msas, function(msa){
        msa.counties = counties.for.msa.or.division(msa)
        if (length(msa.counties)==1)
            county.surveillance$source$new.all[,msa.counties]
        else
            apply(county.surveillance$source$new.all[,msa.counties], 1, join.mappings)
    })
    rv$url$new.all = sapply(msas, function(msa){
        msa.counties = counties.for.msa.or.division(msa)
        if (length(msa.counties)==1)
            county.surveillance$url$new.all[,msa.counties]
        else
            apply(county.surveillance$url$new.all[,msa.counties], 1, join.mappings)
    })
    rv$details$new.all = sapply(msas, function(msa){
        msa.counties = counties.for.msa.or.division(msa)
        if (length(msa.counties)==1)
            county.surveillance$details$new.all[,msa.counties]
        else
            apply(county.surveillance$details$new.all[,msa.counties], 1, join.mappings)
    })
    
    
    dim(rv$new.all) = dim(rv$source$new.all) = dim(rv$url$new.all) = dim(rv$details$new.all) =
        sapply(new.dim.names, length)
    dimnames(rv$new.all) = dimnames(rv$source$new.all) = dimnames(rv$url$new.all) = dimnames(rv$details$new.all) =
        new.dim.names
    
    # Prevalence (plus source, url, details)
    rv$prevalence.all = sapply(msas, function(msa){
        msa.counties = counties.for.msa.or.division(msa)
        if (length(msa.counties)==1)
            county.surveillance$prevalence.all[,msa.counties]
        else
            rowSums(county.surveillance$prevalence.all[,msa.counties], na.rm=ignore.missing)
    })
    rv$source$prevalence.all = sapply(msas, function(msa){
        msa.counties = counties.for.msa.or.division(msa)
        if (length(msa.counties)==1)
            county.surveillance$source$prevalence.all[,msa.counties]
        else
            apply(county.surveillance$source$prevalence.all[,msa.counties], 1, join.mappings)
    })
    rv$url$prevalence.all = sapply(msas, function(msa){
        msa.counties = counties.for.msa.or.division(msa)
        if (length(msa.counties)==1)
            county.surveillance$url$prevalence.all[,msa.counties]
        else
            apply(county.surveillance$url$prevalence.all[,msa.counties], 1, join.mappings)
    })
    rv$details$prevalence.all = sapply(msas, function(msa){
        msa.counties = counties.for.msa.or.division(msa)
        if (length(msa.counties)==1)
            county.surveillance$details$prevalence.all[,msa.counties]
        else
            apply(county.surveillance$details$prevalence.all[,msa.counties], 1, join.mappings)
    })
    
    dim(rv$prevalence.all) = dim(rv$source$prevalence.all) = dim(rv$url$prevalence.all) = dim(rv$details$prevalence.all) =
        sapply(prev.dim.names, length)
    dimnames(rv$prevalence.all) = dimnames(rv$source$prevalence.all) = dimnames(rv$url$prevalence.all) = dimnames(rv$details$prevalence.all) = 
        prev.dim.names
    
    
    rv
}


#note = arr1 must have other as race, arr2 can have asian + american_indian
max.marginal.sum <- function(arr1, arr2, keep=c('race','risk'))
{
    rv = apply(arr1, c('year','location', keep), sum)
    summed.2 = apply(arr2, c('year', 'location', keep), sum)
    
    arr1.2d = rowSums(rv, dims=2)
    arr2.2d = rowSums(summed.2, dims=2)
    
    replace.mask = is.na(arr1.2d) | (!is.na(arr1.2d) & !is.na(arr2.2d) & arr2.2d>arr1.2d)
    
    for (value in dimnames(rv)[[keep]])
    {
        if (keep=='race' && value=='other' && any(dimnames(summed.2)[['race']]=='asian'))
            rv[,,value][replace.mask] = summed.2[,,'asian'][replace.mask] +
                summed.2[,,'american_indian_or_alaska_native'][replace.mask]
        else
            rv[,,value][replace.mask] = summed.2[,,value][replace.mask]
    }
    
    rv
}

##---------------------------------##
##-- POST-PROCESSING AGGREGATORS --##
##---------------------------------##

aggregate.surveillance.race.as.bho <- function(surv,
                                               ignore.na.races=c('american_indian_or_alaska_native', 'asian'))
{
    for (elem.name in names(surv))
    {
        elem = surv[[elem.name]]        
        
        if (is.array(elem) && any(names(dim(elem))=='race'))
        {
            print(paste0('Aggregating ', elem.name))
            
            source = surv$source[[elem.name]]
            url = surv$url[[elem.name]]
            details = surv$details[[elem.name]]
            
            new.dim.names = dimnames(elem)
            new.dim.names[['race']] = c('black','hispanic','other')
            
            new.elem = array(0, dim=sapply(new.dim.names, length), dimnames=new.dim.names)
            new.source = new.url = new.details = 
                array(as.character(NA), dim=sapply(new.dim.names, length), dimnames=new.dim.names)
            
            for (race in dimnames(elem)[['race']])
            {
                if (any(new.dim.names[['race']] == race) && race != 'other')
                {
                    access(new.elem, race=race) = access(elem, race=race)
                    
                    new.source[surveillance.access.indices(new.source, race=race)] =
                        source[surveillance.access.indices(source, race=race)]
                    new.url[surveillance.access.indices(new.url, race=race)] =
                        url[surveillance.access.indices(url, race=race)]
                    new.details[surveillance.access.indices(new.details, race=race)] =
                        details[surveillance.access.indices(details, race=race)]
                }
                else
                {
                    other.race = access(elem, race=race)
                    
                    
                    if (any(race==ignore.na.races))
                        other.race[is.na(other.race)] = 0
                    access(new.elem, race='other') = access(new.elem, race='other') + other.race
                    
                    
                    new.source[surveillance.access.indices(new.source, race='other')] =
                        arr.join.mappings(new.source[surveillance.access.indices(new.source, race='other')],
                                          source[surveillance.access.indices(source, race=race)])
                    
                    new.url[surveillance.access.indices(new.url, race='other')] =
                        arr.join.mappings(new.url[surveillance.access.indices(new.url, race='other')],
                                          url[surveillance.access.indices(url, race=race)])
                    
                    new.details[surveillance.access.indices(new.details, race='other')] =
                        arr.join.mappings(new.details[surveillance.access.indices(new.details, race='other')],
                                          details[surveillance.access.indices(details, race=race)])
                }
            }
            
            surv[[elem.name]] = new.elem
            
            surv$source[[elem.name]] = new.source
            surv$url[[elem.name]] = new.url
            surv$details[[elem.name]] = new.details
        }
    }
    
    surv
}

#... are the named values to access
surveillance.access.indices <- function(arr, ...)
{
    indices = array(1:length(arr), dim=dim(arr), dimnames=dimnames(arr))
    as.integer(access(indices, ...))
}

aggregate.surveillance.other.risk.as.heterosexual <- function(surv)
{
    for (elem.name in names(surv))
    {
        elem = surv[[elem.name]]
        
        if (is.array(elem) && any(names(dim(elem))=='risk') && any(dimnames(elem)[['risk']] == 'other'))
        {
            source = surv$source[[elem.name]]
            url = surv$url[[elem.name]]
            details = surv$details[[elem.name]]
            
            new.risk.dim.names = setdiff(dimnames(elem)[['risk']], 'other')
            
            new.elem = access(elem, risk=new.risk.dim.names)
            new.source = access(source, risk=new.risk.dim.names)
            new.url = access(url, risk=new.risk.dim.names)
            new.details = access(details, risk=new.risk.dim.names)
            
            other.risk = access(elem, risk='other')
            other.risk[is.na(other.risk)] = 0
            
            access(new.elem, risk='heterosexual') = access(elem, risk='heterosexual') + other.risk
            
            new.source[surveillance.access.indices(new.source, risk='heterosexual')] =
                arr.join.mappings(new.source[surveillance.access.indices(new.source, risk='heterosexual')],
                                  source[surveillance.access.indices(source, risk='other')])
            
            new.url[surveillance.access.indices(new.url, risk='heterosexual')] =
                arr.join.mappings(new.url[surveillance.access.indices(new.url, risk='heterosexual')],
                                  url[surveillance.access.indices(url, risk='other')])
           
            new.details[surveillance.access.indices(new.details, risk='heterosexual')] =
                arr.join.mappings(new.details[surveillance.access.indices(new.details, risk='heterosexual')],
                                  details[surveillance.access.indices(details, risk='other')])
            
            # Overwrite with the new
            surv[[elem.name]] = new.elem
            
            surv$source[[elem.name]] = new.source
            surv$url[[elem.name]] = new.url
            surv$details[[elem.name]] = new.details
        }
    }
    
    surv
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