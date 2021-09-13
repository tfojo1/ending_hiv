
##-----------------##
##-- THE GETTERS --##
##-----------------##
ALLOWED.DATA.TYPES = c('prevalence','new','mortality',
                       'diagnosed','diagnosed.ci.lower','diagnosed.ci.upper',
                       'suppression','suppression.ci.lower','suppression.ci.upper', 'prevalence.for.continuum',
                       'estimated.prevalence', 'estimated.prevalence.ci.lower','estimated.prevalence.ci.upper', 'estimated.prevalence.rse',
                       'cumulative.aids.mortality', 'aids.diagnoses',
                       'linkage','engagement','new.for.continuum', 'prep')

get.surveillance.data <- function(surv=msa.surveillance,
                                  location.codes=BALTIMORE.MSA,
                                  data.type=c('new', 'prevalence','mortality','diagnosed')[1],
                                  years=NULL,
                                  age=F,
                                  race=F,
                                  sex=F,
                                  risk=F,
                                  aggregate.locations=T,
                                  aggregate.years=F,
                                  throw.error.if.missing.years=F,
                                  throw.error.if.missing.data=T,
                                  na.rm=F)
{
    #Check the data.type argument
    
    if (all(data.type!=ALLOWED.DATA.TYPES))
        stop(paste0("data.type must be one of: ",
                    paste0(paste0("'", ALLOWED.DATA.TYPES, "'"), collapse=", ")))
    
    if (class(data.type) != 'character' || length(data.type)>1)
        stop("data.type must be a character of length one")
    
    
    categories = c('sex','age','race','risk')[c(sex, age, race, risk)]
    
    #If there is a master array with all the data, pull from there
    if (!is.null(surv[[paste0(data.type, '.master')]]))
    {
        data = surv[[paste0(data.type, '.master')]]
        from.master = T
    }
    else #pull data type and categories together into a name
    {
        data.suffix = paste0(categories, collapse='.')
        if (data.suffix=='')
            data.suffix  = 'all'
        
        data.name = paste0(data.type, '.', data.suffix)
        
        #Check to make sure the requested data exists
        data = surv[[data.name]]
        
        data.type.name = data.type
        if (data.type=='new')
            data.type.name = 'new cases'
        if (is.null(data))
        {
            if (throw.error.if.missing.data)
                stop(paste0("This surveillance manager does not have any data on '",
                            data.type.name, "' for ",
                            ifelse(data.suffix=='all',
                                   "the aggregate population.",
                                   paste0("strata of ",
                                          paste0(categories, collapse = ' x '))
                            )))
            else
                return (NULL)
        }
        
        from.master = F
    }
    
    #check to make sure all location codes are present in the data
    missing.code = sapply(location.codes, function(code){
        all(dimnames(data)[['location']]!=code)
    })
    
    if (any(missing.code))
    {
        if (throw.error.if.missing.data)
            stop(paste0("The following location code(s) are not present in the requested data: ",
                        paste0(location.codes[missing.code], collapse=', ')))
        else
            return (NULL)
    }
    
    #Pull and check the years
    if (is.null(years))
        years = dimnames(data)[['year']]
    
    years = as.character(years)
    missing.years = sapply(years, function(year){
        all(dimnames(data)[['year']] != year)
    })
    
    if (any(missing.years))
    {
        if (throw.error.if.missing.years)
            stop(paste0("The following year(s) are not present in the requested data: ",
                        paste0(years[missing.years], collapse=', ')))
    }
    
    requested.years = years
    years = years[!missing.years]
    
    if (length(years)==0)
    {
        if (throw.error.if.missing.data)
            stop("No data is available for any of the requested years")
        else
            return (NULL)
    }
    
    #pull the data
    target.dimnames = dimnames(data)[c('year','location',categories)]
    target.dimnames[['year']] = years
    target.dimnames[['location']] = location.codes
    
    if (from.master)
    {
        data = data[years, location.codes,,,,]
        data = apply(data, intersect(names(target.dimnames), names(dimnames(data))), sum)
    }
    else
    {
        overwrite.dimnames = c(dimnames(data)[1:2],list(other=NULL))
        overwrite.dim = c(dim(data)[1:2], other=as.numeric(prod(dim(data))/dim(data)[1]/dim(data)[2]))
        dim(data) = overwrite.dim
        dimnames(data) = overwrite.dimnames
        
        data = data[years, location.codes, ]
    }
    
    dim(data) = sapply(target.dimnames, length)
    dimnames(data) = target.dimnames
    
    
    #collapse as needed and return
    keep.dimnames = dimnames(data)
    keep.dimnames.names = names(keep.dimnames)
    
    if (aggregate.locations)
        keep.dimnames.names = setdiff(keep.dimnames.names, 'location')
    if (aggregate.years)
        keep.dimnames.names = setdiff(keep.dimnames.names, 'year')
    
    keep.dimnames = keep.dimnames[keep.dimnames.names]
    
    data = apply(data, keep.dimnames.names, sum, na.rm=na.rm)
    dim(data) = sapply(keep.dimnames, length)
    dimnames(data) = keep.dimnames
    
    if (setequal(years, requested.years))
    {
        attr(data, 'years') = as.numeric(years)
        data
    }
    else
    {
        rv.dim.names = dimnames(data)
        rv.dim.names[['year']] = as.character(requested.years)
        rv = array(as.numeric(NA), dim=sapply(rv.dim.names, length), dimnames=rv.dim.names)
        access(rv, year=as.character(years)) = data
        rv
    }
}

get.surveillance.data.source <- function(surv=msa.surveillance,
                                  location.codes=BALTIMORE.MSA,
                                  data.type=c('new', 'prevalence','mortality','diagnosed')[1],
                                  years=NULL,
                                  age=F,
                                  race=F,
                                  sex=F,
                                  risk=F,
                                  na.rm=F,
                                  wrap='\\n')
{
    data = get.surveillance.data(surv=surv, location.codes=location.codes, data.type=data.type,
                                 years=years,
                                 age=age, race=race, sex=sex, risk=risk,
                                 throw.error.if.missing.data = F, throw.error.if.missing.years = F)
    
    if (is.null(data) || all(is.na(data)))
        'N/A'
    else if (data.type=='prep')
        'AidsVu'
    else if (data.type=='linkage' || data.type=='suppression')
        'Local Health Department'
    else
        'CDC'
}

get.surveillance.data.rate <- function(surv,
                                       location.codes,
                                       years=NULL,
                                       data.type=c('new', 'prevalence','mortality')[1],
                                       age=F,
                                       race=F,
                                       sex=F,
                                       aggregate.years=F,
                                       census)
{
    numerators = get.surveillance.data(surv, location.codes=location.codes,
                                       years=years, data.type=data.type,
                                       age=age, race=race, sex=sex,
                                       aggregate.locations=T,
                                       aggregate.years=aggregate.years)
    
    years = attr(numerators, 'years')
    
    denominators = get.census.data(census, years=years,
                                   fips=counties.for.msa(location.codes),
                                   aggregate.counties = T,
                                   aggregate.years = aggregate.years,
                                   aggregate.ages=!age,
                                   aggregate.races=!race,
                                   aggregate.sexes=!sex)
    
    if (!is.null(dim(denominators)))
        denominators = apply(denominators, names(dimnames(numerators)), function(x){x})
    
    numerators / denominators
}

has.location.surveillance <- function(surv, location)
{
    any(sapply(surv, function(elem){
        !is.null(dim(elem)) && !is.null(dimnames(elem)[['location']]) && any(location==dimnames(elem)[['location']])
    }))
}


get.state.averaged.knowledge.of.status <- function(msa,
                                                   state.surveillance,
                                                   years,
                                                   census.totals = ALL.DATA.MANAGERS$census.totals, 
                                                   throw.error.if.missing=F)
{
    states = states.for.msa(msa)
    if (length(states)==1)
        get.surveillance.data(state.surveillance, states, data.type = 'diagnosed', years=years, throw.error.if.missing.data = throw.error.if.missing)
    else
    {
        p = get.surveillance.data(state.surveillance, states, data.type = 'diagnosed', years=years, aggregate.locations = F, throw.error.if.missing.data = throw.error.if.missing)
        
        counties = counties.for.msa(msa)
        county.populations = get.census.totals(census.totals, location=counties, years=years, collapse.counties=F)
        states.by.county = state.for.county(counties)
        state.populations.from.counties = sapply(states, function(st){
            sapply(years, function(year){
                sum(county.populations[as.character(year), states.by.county==st])
            })
        })
        
        rowSums(p * state.populations.from.counties / rowSums(state.populations.from.counties))
    }
}

##---------------------------##
##-- READ MSA SURVEILLANCE --##
##---------------------------##


read.msa.surveillance <- function(dir='cleaned_data/',
                                  use.adjusted.estimate=F,
                                  first.total.prevalence.year=2007,
                                  first.total.new.year=2008,
                                  include.children.in.prevalence=F,
                                  verbose=F)
{
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
    
    #-- SET UP RV ARRAY SKELETONS --#
    rv = list(params=list(use.adjusted.estimate=use.adjusted.estimate,
                          correct.new.to.county.level=F,
                          correct.prevalence.to.county.level=F))
    
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
    
    dim.names = list(year=all.prevalence.years, location=as.character(all.codes))
    rv$prevalence.all = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    
    # Sex
    dim.names = list(year=all.incidence.years, location=as.character(all.codes), sex=sexes)
    rv$new.sex = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    
    dim.names = list(year=all.prevalence.years, location=as.character(all.codes), sex=sexes)
    rv$prevalence.sex = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    
    # Sex x Age
    dim.names = list(year=all.incidence.years, location=as.character(all.codes), sex=sexes, age=ages)
    rv$new.sex.age = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    
    dim.names = list(year=all.prevalence.years, location=as.character(all.codes), sex=sexes, age=ages)
    rv$prevalence.sex.age = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    
    # Sex x Race
    dim.names = list(year=all.incidence.years, location=as.character(all.codes), sex=sexes, race=races)
    rv$new.sex.race = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    
    dim.names = list(year=all.prevalence.years, location=as.character(all.codes), sex=sexes, race=races)
    rv$prevalence.sex.race = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    
    # Sex x Risk
    dim.names = list(year=all.incidence.years, location=as.character(all.codes), sex=sexes, risk=risks)
    rv$new.sex.risk = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    
    dim.names = list(year=all.prevalence.years, location=as.character(all.codes), sex=sexes, risk=risks)
    rv$prevalence.sex.risk = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    
    # Race x risk
    dim.names = list(year=all.incidence.years, location=as.character(all.codes), race=races.limited, risk=risks)
    rv$new.race.risk = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    
    dim.names = list(year=all.prevalence.years, location=as.character(all.codes), race=races.limited, risk=risks)
    rv$prevalence.race.risk = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    
    # Mortality
    dim.names = list(year=as.character(all.mortality.years), location=as.character(all.codes), sex=sexes)
    rv$mortality.sex = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    
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
                    codes = intersect(codes, df$code[na.mask])
                    rv$new.all[year,codes] = as.matrix(df[codes, 2])
                }
            }
            else
                rv$new.all[year,codes] = as.matrix(df[codes,4])
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
        }
    }

    #New - Other x risk
    rv$new.race.risk[,,'other',] = apply(rv$new.sex.risk, c(1,2,4), sum) - apply(rv$new.race.risk[,,c('black','hispanic','white'),], c(1,2,4), sum)


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
                    codes = intersect(codes, df$code[na.mask])
                    rv$prevalence.all[year,codes] = as.matrix(df[codes, 4 + two.estimates + has.rank])
                }
            }
            else
                rv$prevalence.all[year,codes] = as.matrix(df[codes,6+2*as.numeric(include.children.in.prevalence)])
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
        }
    }

    #prevalence - Other x risk
    rv$prevalence.race.risk[,,'other',] = apply(rv$prevalence.sex.risk, c(1,2,4), sum) - apply(rv$prevalence.race.risk[,,c('black','hispanic','white'),], c(1,2,4), sum)


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
        }
    }

    #Cumulative AIDS Mortality
    rv$cumulative.aids.mortality.sex.race.risk = read.cum.aids.mortality(file=file.path(dir, 'hiv_surveillance/msa/aids_mortality_pre_2000.txt'))
    dim.names = c(list(year='2000'), dimnames(rv$cumulative.aids.mortality.sex.race.risk))
    dim(rv$cumulative.aids.mortality.sex.race.risk) = sapply(dim.names, length)
    dimnames(rv$cumulative.aids.mortality.sex.race.risk) = dim.names
    
    
    #-- AGGREGATE FOR THE TOTAL POPULATION NUMBERS and AGE, RACE, RISK --#
    rv$mortality.all = apply(rv$mortality.sex, c('year','location'), sum)

    rv$new.age = apply(rv$new.sex.age, c('year','location','age'), sum)
    rv$prevalence.age = apply(rv$prevalence.sex.age, c('year','location','age'), sum)

    rv$new.race = max.marginal.sum(rv$new.race.risk,
                                   rv$new.sex.race,
                                   'race')

    rv$prevalence.race = max.marginal.sum(rv$prevalence.race.risk,
                                          rv$prevalence.sex.race,
                                          'race')

    rv$new.risk = max.marginal.sum(rv$new.race.risk,
                                   rv$new.sex.risk,
                                   'risk')

    rv$prevalence.risk = max.marginal.sum(rv$prevalence.race.risk,
                                   rv$prevalence.sex.risk,
                                   'risk')

    #-- AIDS DIAGNOSES --#
    old.aids.diagnoses = read.aids.diagnoses(file.path(dir, 'hiv_surveillance/msa/aids_diagnoses_pre_2002.txt'))
    move.from.new.to.aids.years = dimnames(rv$new.all)[['year']][as.numeric(dimnames(rv$new.all)[['year']])<first.total.new.year]
    aids.years = sort(union(dimnames(old.aids.diagnoses)[['year']], move.from.new.to.aids.years))
    aids.locations = sort(union(dimnames(old.aids.diagnoses)[['location']], dimnames(rv$new.all)[['location']]))

    dim.names = list(year=aids.years, location=aids.locations)
    rv$aids.diagnoses.all = array(as.numeric(NA), dim=sapply(dim.names, length), dimnames=dim.names)
    
    rv$aids.diagnoses.all[intersect(aids.years, dimnames(rv$new.all)[['year']]), 
                          dimnames(rv$new.all)[['location']] ] =
        rv$new.all[intersect(aids.years, dimnames(rv$new.all)[['year']]),]
    
    rv$aids.diagnoses.all[intersect(aids.years, dimnames(old.aids.diagnoses)[['year']]), 
                          dimnames(old.aids.diagnoses)[['location']] ] =
        old.aids.diagnoses[intersect(aids.years, dimnames(old.aids.diagnoses)[['year']]),]
    
    rv$new.all[move.from.new.to.aids.years,] = NA


    #-- RETURN IT --#
    rv
}

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
                             Months='months')
        
        REQUIRED.FOR.STRATIFIED = COL.NAME.MAPPING[4:(length(COL.NAME.MAPPING)-1)]
        
        df = read.csv(stratified.file, stringsAsFactors = F, header = F)
        col.names = df[,1]
        if (length(setdiff(col.names, names(COL.NAME.MAPPING)))>1)
            stop(paste0("Invalid Row Names in file '", file, "': ",
                        paste0("'", setdiff(col.names, names(COL.NAME.MAPPING)), "'", collapse=', ')))
        df = as.data.frame(t(df[,-1]), stringsAsFactors = F)
        dimnames(df)[[2]]=COL.NAME.MAPPING[col.names]
        
        df = df[apply(!is.na(df), 1, any),]
        
        for (col in 2:dim(df)[2])
            df[,col] = as.numeric(as.character(df[,col]))
        
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
                                                 by.sex=T)
                
                if (any(!is.na(df[i,c('black','hispanic','other')])))
                    surv = add.surveillance.data(surv,
                                                 data.type=data.type,
                                                 location=location,
                                                 years=df$year[i],
                                                 values=df[i,c('black','hispanic','other')],
                                                 by.race=T)
                
                if (any(!is.na(df[i,c('msm','idu','msm_idu','heterosexual')])))
                    surv = add.surveillance.data(surv,
                                                 data.type=data.type,
                                                 location=location,
                                                 years=df$year[i],
                                                 values=df[i,c('msm','idu','msm_idu','heterosexual')],
                                                 by.risk=T)
                
                if (any(!is.na(df[i,c('13-24 years','25-34 years','35-44 years','45-54 years','55+ years')])))
                    surv = add.surveillance.data(surv,
                                                 data.type=data.type,
                                                 location=location,
                                                 years=df$year[i],
                                                 values=df[i,c('13-24 years','25-34 years','35-44 years','45-54 years','55+ years')],
                                                 by.age=T)
                
                if (!is.na(df[i,'total']))
                    surv = add.surveillance.data(surv,
                                                  data.type=data.type,
                                                  location=location,
                                                  years=df$year[i],
                                                  values=df[i,'total'])
            #}
        }
    }
    
    #Read total file
    if (!is.null(total.file))
    {
        df = read.csv(total.file, stringsAsFactors = F)
        df = df[apply(!is.na(df), 1, any),]
        if (any(names(df)=='aware'))
            surv = add.surveillance.data(surv,
                                  data.type='diagnosed',
                                  location=location,
                                  years=df[,1],
                                  values=df$aware)
        if (any(names(df)=='suppressed'))
            surv = add.surveillance.data(surv,
                                  data.type='suppression',
                                  location=location,
                                  years=df[,1],
                                  values=df$suppressed)
        if (any(names(df)=='prevalent'))
            surv = add.surveillance.data(surv,
                                         data.type='prevalence.for.continuum',
                                         location=location,
                                         years=df[,1],
                                         values=df$prevalent)
        if (any(names(df)=='linked'))
            surv = add.surveillance.data(surv,
                                         data.type='linkage',
                                         location=location,
                                         years=df[,1],
                                         values=df$linked)
        if (any(names(df)=='new'))
            surv = add.surveillance.data(surv,
                                         data.type='new.for.continuum',
                                         location=location,
                                         years=df[,1],
                                         values=df$new)
        
        if (any(names(df)=='engaged'))
            surv = add.surveillance.data(surv,
                                         data.type='engagement',
                                         location=location,
                                         years=df[,1],
                                         values=df$engaged)
    }
    
    surv
}

add.prep.data <- function(surv,
                          prep.manager,
                          locations = dimnames(surv$new.all)[['location']])
{
    all.dim.names = list(year=prep.manager$years,
                         location=locations,
                         sex=dimnames(prep.manager$prep.sex)[['sex']],
                         age=dimnames(prep.manager$prep.age)[['age']])
    surv$prep.all = sapply(locations, get.prep.data, prep.manager=prep.manager, na.rm=T)
    dim(surv$prep.all) = sapply(all.dim.names[c('year','location')], length)
    dimnames(surv$prep.all) = all.dim.names[c('year','location')]
    
    surv$prep.sex = sapply(locations, get.prep.data, prep.manager=prep.manager, sex=T, na.rm=T)
    dim(surv$prep.sex) = sapply(all.dim.names[c('year','sex','location')], length)
    dimnames(surv$prep.sex) = all.dim.names[c('year','sex','location')]
    surv$prep.sex = apply(surv$prep.sex, c('year','location','sex'), function(x){x})
    
    surv$prep.age = sapply(locations, get.prep.data, prep.manager=prep.manager, age=T, na.rm=T)
    dim(surv$prep.age) = sapply(all.dim.names[c('year','age','location')], length)
    dimnames(surv$prep.age) = all.dim.names[c('year','age','location')]
    surv$prep.age = apply(surv$prep.age, c('year','location','age'), function(x){x})
    
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

add.surveillance.data <- function(surv,
                                  data.type,
                                  location,
                                  years,
                                  values,
                                  by.race=F,
                                  by.age=F,
                                  by.sex=F,
                                  by.risk=F)
{
    years = as.character(years)
    location = as.character(location)
    
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
    
    if (by.race || by.sex || by.age || by.risk)
    {
        if (!is.null(old.values))
            surv[[data.name]][old.dim.names[['year']], old.dim.names[['location']],] = old.values[old.dim.names[['year']], old.dim.names[['location']],]
        surv[[data.name]][as.character(years), location,] = values[as.character(years), location,]
    }
    else
    {
        if (!is.null(old.values))
            surv[[data.name]][old.dim.names[['year']], old.dim.names[['location']]] = old.values[old.dim.names[['year']], old.dim.names[['location']]]
        surv[[data.name]][as.character(years), location] = values[as.character(years), location]
    }
    
    surv
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
        stop('missing codes in df - consider putting a browser statement here')
   #     browser()

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


read.total.msa.data.from.counties <- function(msas,
                                              county.file='cleaned_data/hiv_surveillance/county/all_county_08_18_diagnoses_and_prevalence.csv',
                                              ignore.missing=T)
{
    NEW.INDICATOR = 'HIV diagnoses'
    PREV.INDICATOR = 'HIV prevalence'
    
    df = read.csv(county.file, stringsAsFactors = F)
    df$msa = msa.for.county(df$FIPS)
    df = df[!is.na(df$msa),]
    df$Cases[df$Cases=='Data suppressed'] = NA
    df$Cases = gsub(",", "", df$Cases)
    df$Cases = as.numeric(df$Cases)
    
    new.years = as.character(unique(df$Year[df$Indicator==NEW.INDICATOR]))
    prev.years = as.character(unique(df$Year[df$Indicator==PREV.INDICATOR]))

    new.dim.names = list(year=new.years, location=msas)
    prev.dim.names = list(year=prev.years, location=msas)
    
    rv = list()
    rv$new.all = array(as.numeric(-1), dim=sapply(new.dim.names, length), dimnames=new.dim.names)
    rv$prevalence.all = array(as.numeric(-1), dim=sapply(prev.dim.names, length), dimnames=prev.dim.names)

    for (msa in msas)
    {
        msa.counties = as.integer(counties.for.msa.or.division(msa))
        msa.df = df[sapply(df$FIPS, function(one.fips){any(one.fips==msa.counties)}),]
        
        if (dim(msa.df)[1]>0)
        {
            for (i in 1:dim(msa.df)[1])
            {
                if (msa.df$Indicator[i]==NEW.INDICATOR)
                    dataset.name = 'new.all'
                else if (msa.df$Indicator[i]==PREV.INDICATOR)
                    dataset.name = 'prevalence.all'
                else
                    dataset.name = NULL
                
                if (!is.null(dataset.name) &&
                    (!ignore.missing || !is.na(msa.df$Cases[i])))
                {
                    year = as.character(msa.df$Year[i])
                    rv[[dataset.name]][year,msa] = rv[[dataset.name]][year,msa] + msa.df$Cases[i]
                }
            }
        }
    }
    
    #Set NA for clearly incorrect Cook county prevalence 2010-2011 
    rv$prevalence.all[c('2010','2011'),'16980'] = NA
    
    rv$new.all[rv$new.all<0] = NA
    rv$prevalence.all[rv$prevalence.all<0] = NA
    
    rv$new.all = rv$new.all+1
    rv$prevalence.all = rv$prevalence.all+1
    
    rv
}

##---------------------------------##
##-- POST-PROCESSING AGGREGATORS --##
##---------------------------------##

aggregate.surveillance.race.as.bho <- function(surv,
                                               ignore.na.races=c('american_indian_or_alaska_native', 'asian'))
{
    lapply(surv, function(elem){
        
        if (any(names(dim(elem))=='race'))
        {
            new.dim.names = dimnames(elem)
            new.dim.names[['race']] = c('black','hispanic','other')
            
            rv = array(0, dim=sapply(new.dim.names, length), dimnames=new.dim.names)
            for (race in dimnames(elem)[['race']])
            {
                if (any(new.dim.names[['race']] == race) && race != 'other')
                    access(rv, race=race) = access(elem, race=race)
                else
                {
                    other.race = access(elem, race=race)
                    if (any(race==ignore.na.races))
                        other.race[is.na(other.race)] = 0
                    access(rv, race='other') = access(rv, race='other') + other.race
                }
            }
            
            rv
        }
        else
            elem
    })
}

aggregate.surveillance.other.risk.as.heterosexual <- function(surv)
{
    lapply(surv, function(elem){
        if (any(names(dim(elem))=='risk') && any(dimnames(elem)[['risk']] == 'other'))
        {
            new.risk.dim.names = setdiff(dimnames(elem)[['risk']], 'other')
            
            rv = access(elem, risk=new.risk.dim.names)
            
            other.risk = access(elem, risk='other')
            other.risk[is.na(other.risk)] = 0
            
            access(rv, risk='heterosexual') = access(rv, risk='heterosexual') + other.risk
            
            rv
        }
        else
            elem
    })
}


##-------------------------##
##-- CORRECTING TO TOTAL --##
##-------------------------##


correct.to.county.totals <- function(rv,
                                     correct.new.to.county.level=T,
                                     correct.prevalence.to.county.level=T,
                                     county.file='cleaned_data/hiv_surveillance/county/all_county_08_18_diagnoses_and_prevalence.csv',
                                     verbose=T)
{
    rv$params$correct.new.to.county.level=correct.new.to.county.level
    rv$params$correct.prevalence.to.county.level=correct.prevalence.to.county.level
    
    #-- AGGREGATED COUNTY-LEVEL TOTALS --#
    all.codes = dimnames(rv$new.all)[['location']]
    if (correct.new.to.county.level || correct.prevalence.to.county.level)
        msa.by.county = read.total.msa.data.from.counties(county.file = county.file,
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


##-----------------------------##
##-- READ STATE SURVEILLANCE --##
##-----------------------------##

OLD.read.state.surveillance.manager <- function(dir='cleaned_data',
                                            verbose=T)
{
    if (verbose)
        print("Reading state surveillance data (reported, prevalence, deaths)")
    files = list.files(file.path(dir, 'hiv_surveillance/state'))
    is.female = grepl('female', files, ignore.case = T)

    dfs = lapply(file.path(dir, 'hiv_surveillance/state', files), read.hiv.atlas.file) 
    
    df = NULL
    for (i in 1:length(dfs))
    {
        if (is.female[i])
            dfs[[i]]$Sex = 'female'
        else
            dfs[[i]]$Sex = 'male'
        
        df = rbind(df, dfs[[i]])
    }

    df$State = state.name.to.abbreviation(df$Geography)
    states = unique(df$State)
    years = as.character(unique(df$Year))
    df$Age.Group = paste0(df$Age.Group, " years")
    ages = sort(unique(as.character(df$Age.Group)))

    dim.names = list(year=years, location=states, age=ages, race=c('black','hispanic','other'),
                     sex=c('male','female'), risk=c('msm','idu','msm_idu','heterosexual'))


    rv = list()
    rv$new.master = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$prevalence.master = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$mortality.master = array(0, dim=sapply(dim.names, length), dimnames=dim.names)

    for (i in 1:dim(df)[1])
    {
        if (df$Indicator[i]=="HIV deaths")
            elem = 'mortality.master'
        else if (df$Indicator[i]=='HIV diagnoses')
            elem = 'new.master'
        else if (df$Indicator[i]=='HIV prevalence')
            elem = 'prevalence.master'
        else
            stop("Only indicators 'HIV deaths', 'HIV diagnoses', and 'HIV prevalence' are supported")

        if (grepl('hispanic', df$Race.Ethnicity[i], ignore.case = T))
            race.to = 'hispanic'
        else if (grepl('black', df$Race.Ethnicity[i], ignore.case = T))
            race.to = 'black'
        else
            race.to = 'other'

        if (grepl('injection', df$Transmission.Category[i], ignore.case = T))
        {
            if (grepl('male-to-male', df$Transmission.Category[i], ignore.case = T))
                risk.to = 'msm_idu'
            else
                risk.to = "idu"
        }
        else if (grepl('male-to-male', df$Transmission.Category[i], ignore.case = T))
            risk.to = 'msm'
        else
            risk.to = 'heterosexual'
        
        rv[[elem]][as.character(df$Year[i]), df$State[i], df$Age.Group[i], race.to, df$Sex[i], risk.to] = df$Cases[i] +
            rv[[elem]][as.character(df$Year[i]), df$State[i], df$Age.Group[i], race.to, df$Sex[i], risk.to]

    }

    ##-- Estimated Prevalence and Diagnosis --##
    
    if (verbose)
        print("Reading state estimated prevalence and diagnosis rate")
    df.dx = read.hiv.atlas.file(file.path(dir, 'continuum/state/state_diagnosis_all.csv'))
    df.dx$State = state.name.to.abbreviation(df.dx$Geography)
    
    dx.dim.names = list(year=sort(unique(df.dx$Year)), location=unique(df.dx$State))
    rv$estimated.prevalence.all = rv$estimated.prevalence.ci.lower.all =
        rv$estimated.prevalence.ci.upper.all = rv$estimated.prevalence.rse.all =
        rv$diagnosed.all =
        array(0, dim=sapply(dx.dim.names, length), dimnames=dx.dim.names)

    for (i in 1:dim(df.dx)[1])
    {
        if (df.dx$Indicator[i]=='Knowledge of Status')
        {
            rv$estimated.prevalence.rse.all[as.character(df.dx$Year[i]), df.dx$State[i]] = df.dx$RSE[i]
            rv$diagnosed.all[as.character(df.dx$Year[i]), df.dx$State[i]] = df.dx$Percent[i]/100 
        }
        else
        {
            rv$estimated.prevalence.all[as.character(df.dx$Year[i]), df.dx$State[i]] = df.dx$Cases[i]
            rv$estimated.prevalence.ci.lower.all[as.character(df.dx$Year[i]), df.dx$State[i]] = df.dx$Cases.LCI[i]
            rv$estimated.prevalence.ci.upper.all[as.character(df.dx$Year[i]), df.dx$State[i]] = df.dx$Cases.UCI[i]
        }
    }

    rv
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

read.state.diagnoses <- function(file)
{
    df = read.csv(file, stringsAsFactors = F)
    regex = '([0-9]+\\.*[0-9]*) \\(.*'
    pct = rep(NA, dim(df)[1])
    matches = grepl(regex, df[,4])
    
    pct[matches] = as.numeric(gsub(regex, '\\1', df[matches,4]))
    years = as.character(df$Year)
    states = state.name.to.abbreviation(df$Geography)
    
    dim.names = list(year=sort(unique(years)), location=unique(states))
    rv = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    
    for (i in 1:length(pct))
        rv[as.character(years[i]), states[i]] = pct[i]/100
    
    rv
}


read.state.file <- function(file)
{
    df = read.csv(file, stringsAsFactors = F)
    cases = gsub(',','', df$Cases)
    na.cases = cases=='Data suppressed'
    df$Cases = as.numeric(NA)
    df$Cases[!na.cases] = as.numeric(cases[!na.cases])
    df$State = state.name.to.abbreviation(df$Geography)
    df$Age.Group = paste0(df$Age.Group, ' years')
    df$Year = as.character(df$Year)

    df
}

read.state.estimated.prevalence.file <- function(file)
{

    df = read.csv(file, stringsAsFactors = F)
    df$State = state.name.to.abbreviation(df$Geography)
    cases = df$Cases..95..CI.RSE..

    regex = "([0-9,]+[0-9]) \\(([0-9,]+[0-9]) - ([0-9,]+[0-9]), ([0-9,\\.]+[0-9])\\)"

    df$estimate = as.numeric(gsub(',', '', gsub(regex, '\\1', cases)))
    df$ci.lower = as.numeric(gsub(',', '', gsub(regex, '\\2', cases)))
    df$ci.upper = as.numeric(gsub(',', '', gsub(regex, '\\3', cases)))
    df$rse = as.numeric(gsub(',', '', gsub(regex, '\\4', cases)))

    df
}

##--------------------------------##
##-- READ NATIONAL SURVEILLANCE --##
##--------------------------------##

read.national.surveillance.manager <- function(dir='cleaned_data',
                                               aggregate.race.as.bho=T,
                                               verbose=T)
{
    rv = list()
    
    if (verbose)
        print("Reading Suppression Data")
    rv = read.national.suppression(rv,
                                   file.path(dir, 'continuum/national/suppression'),
                                   collapse.race.as.bho = aggregate.race.as.bho)
    
    if (verbose)
        print("Reading Diagnosis Data")
    rv = read.national.diagnoses(rv,
                                 dir=file.path(dir, 'continuum/national/diagnosis'),
                                 aggregate.race.as.bho = aggregate.race.as.bho)
    
    rv
}

read.national.suppression <- function(surv,
                                      dir='cleaned_data/continuum/national/suppression',
                                      collapse.race.as.bho=T)
{
    files = list.files(dir)
    files = files[grepl('[0-9][0-9][0-9][0-9].*', files)]
    years = substr(files, 1,4)
    
    dfs = lapply(file.path(dir, files), read.national.continuum.file)
    
    if (!collapse.race.as.bho)
        stop("We are currently only set up to collapse races as black/hispanic/other")
    
    dim.names.all = list(year = sort(years),
                         location='national',
                         age=c('13-24 years','25-34 years','35-44 years','45-54 years','55+ years'),
                         race=c('black','hispanic','other'),
                         sex=c('female','male'),
                         risk=c('msm','idu','msm_idu','heterosexual'))
    
    surv$suppression.all = surv$prevalence.for.continuum.all = 
        array(as.numeric(NA),
              dim=sapply(dim.names.all[c('year','location')], length),
              dimnames=dim.names.all[c('year','location')])
    surv$suppression.sex =  surv$prevalence.for.continuum.sex = 
        array(as.numeric(NA),
              dim=sapply(dim.names.all[c('year','location','sex')], length),
              dimnames=dim.names.all[c('year','location','sex')])
    surv$suppression.age =  surv$prevalence.for.continuum.age = 
        array(as.numeric(NA),
              dim=sapply(dim.names.all[c('year','location','age')], length),
              dimnames=dim.names.all[c('year','location','age')])
    surv$suppression.race =  surv$prevalence.for.continuum.race = 
        array(as.numeric(NA),
              dim=sapply(dim.names.all[c('year','location','race')], length),
              dimnames=dim.names.all[c('year','location','race')])
    surv$suppression.risk = surv$prevalence.for.continuum.risk = 
        array(as.numeric(NA),
              dim=sapply(dim.names.all[c('year','location','risk')], length),
              dimnames=dim.names.all[c('year','location','risk')])
    surv$suppression.sex.risk =  surv$prevalence.for.continuum.sex.risk = 
        array(as.numeric(NA),
              dim=sapply(dim.names.all[c('year','location','sex','risk')], length),
              dimnames=dim.names.all[c('year','location','sex','risk')])
    
    race.mapping = c('American Indian/Alaska Native' = 'other',
                     'Asian' = 'other',
                     'Black/African American' = 'black',
                     'Hispanic/Latino' = 'hispanic',
                     'Native Hawaiian/Other Pacific Islander' = 'other',
                     'White' = 'other',
                     'Multiple races' = 'other')
    
    for (i in 1:length(dfs))
    {
        df = dfs[[i]]
        year = years[i]
        
        surv$suppression.all[year,1] = df$pct.suppressed[df$category=='Total']/100
        surv$prevalence.for.continuum.all[year,1] = df$n[df$category=='Total']
        
        surv$suppression.sex[year,1,'female'] = df$pct.suppressed[df$category=='Female']/100
        surv$prevalence.for.continuum.sex[year,1,'female'] = df$n[df$category=='Female']
        surv$suppression.sex[year,1,'male'] = df$pct.suppressed[df$category=='Male']/100
        surv$prevalence.for.continuum.sex[year,1,'male'] = df$n[df$category=='Male']
        
        surv$suppression.age[year,1,1] = df$pct.suppressed[df$category=='13-24']/100
        surv$prevalence.for.continuum.age[year,1,1] = df$n[df$category=='13-24']
        surv$suppression.age[year,1,2] = df$pct.suppressed[df$category=='25-34']/100
        surv$prevalence.for.continuum.age[year,1,2] = df$n[df$category=='25-34']
        surv$suppression.age[year,1,3] = df$pct.suppressed[df$category=='35-44']/100
        surv$prevalence.for.continuum.age[year,1,3] = df$n[df$category=='35-44']
        surv$suppression.age[year,1,4] = df$pct.suppressed[df$category=='45-54']/100
        surv$prevalence.for.continuum.age[year,1,4] = df$n[df$category=='45-54']
        surv$suppression.age[year,1,5] = df$pct.suppressed[df$category=='55+']/100
        surv$prevalence.for.continuum.age[year,1,5] = df$n[df$category=='55+']
        
        race.numerators = race.denominators = sapply(dim.names.all[['race']], function(x){0})
        for (race.from in names(race.mapping))
        {
            race.to = race.mapping[race.from]
            
            race.numerators[race.to] = race.numerators[race.to] + df$n.suppressed[df$category==race.from]
            race.denominators[race.to] = race.denominators[race.to] + df$n[df$category==race.from]
        }
        surv$suppression.race[year,1,] = race.numerators / race.denominators
        surv$prevalence.for.continuum.race[year,1,] = race.denominators
        
        #Risk and sex x risk
        surv$suppression.risk[year,1,'msm'] = surv$suppression.sex.risk[year,1,'male','msm'] =
            df$pct.suppressed[df$category=='msm']/100
        surv$prevalence.for.continuum.risk[year,1,'msm'] = surv$prevalence.for.continuum.sex.risk[year,1,'male','msm'] =
            df$n[df$category=='msm']
        surv$suppression.risk[year,1,'msm_idu'] = surv$suppression.sex.risk[year,1,'male','msm_idu'] =
            df$pct.suppressed[df$category=='idu.msm']/100
        surv$prevalence.for.continuum.risk[year,1,'msm_idu'] = surv$prevalence.for.continuum.sex.risk[year,1,'male','msm_idu'] =
            df$n[df$category=='idu.msm']
        
        surv$suppression.sex.risk[year,1,'female','msm'] = surv$suppression.sex.risk[year,1,'female','msm_idu'] = 0
        surv$prevalence.for.continuum.sex.risk[year,1,'female','msm'] = surv$prevalence.for.continuum.sex.risk[year,1,'female','msm_idu'] = 0
        
        surv$suppression.sex.risk[year,1,'female','idu'] = df$pct.suppressed[df$category=='idu.female']/100
        surv$prevalence.for.continuum.sex.risk[year,1,'female','idu'] = df$n[df$category=='idu.female']
        surv$suppression.sex.risk[year,1,'male','idu'] = df$pct.suppressed[df$category=='idu.male']/100
        surv$prevalence.for.continuum.sex.risk[year,1,'male','idu'] = df$n[df$category=='idu.male']
        
        surv$suppression.sex.risk[year,1,'female','heterosexual'] = df$pct.suppressed[df$category=='het.female']/100
        surv$prevalence.for.continuum.sex.risk[year,1,'female','heterosexual'] = df$n[df$category=='het.female']
        surv$suppression.sex.risk[year,1,'male','heterosexual'] = df$pct.suppressed[df$category=='het.male']/100
        surv$prevalence.for.continuum.sex.risk[year,1,'male','heterosexual'] = df$n[df$category=='het.male']
        
        surv$suppression.risk[year,1,'idu'] = (df$n.suppressed[df$category=='idu.female'] + df$n.suppressed[df$category=='idu.male']) /
            (df$n[df$category=='idu.female'] + df$n[df$category=='idu.male'])
        surv$prevalence.for.continuum.risk[year,1,'idu'] = df$n[df$category=='idu.female'] + df$n[df$category=='idu.male']
        surv$suppression.risk[year,1,'heterosexual'] = (df$n.suppressed[df$category=='het.female'] + df$n.suppressed[df$category=='het.male']) /
            (df$n[df$category=='het.female'] + df$n[df$category=='het.male'])
        surv$prevalence.for.continuum.risk[year,1,'heterosexual'] = df$n[df$category=='het.female'] + df$n[df$category=='het.male']
    }
    
    if (any(is.na(surv$suppression.all)))
        stop("NAs generated in reading national suppression data (totals)")
    if (any(is.na(surv$suppression.sex)))
        stop("NAs generated in reading national suppression data (by sex)")
    if (any(is.na(surv$suppression.age)))
        stop("NAs generated in reading national suppression data (by age)")
    if (any(is.na(surv$suppression.race)))
        stop("NAs generated in reading national suppression data (by race)")
    if (any(is.na(surv$suppression.risk)))
        stop("NAs generated in reading national suppression data (by risk)")
    if (any(is.na(surv$suppression.sex.risk)))
        stop("NAs generated in reading national suppression data (by sex x risk)")
        
    #Return
    surv
}

read.national.diagnoses <- function(surv,
                                    dir='cleaned_data/continuum/national/diagnosis/',
                                    aggregate.race.as.bho=T)
{
    
    df.all = read.hiv.atlas.file(file.path(dir, 'diagnosis_all.csv'))
    df.age = read.hiv.atlas.file(file.path(dir, 'diagnosis_by_age.csv'))
    df.sex = read.hiv.atlas.file(file.path(dir, 'diagnosis_by_sex.csv'))
    df.race = read.hiv.atlas.file(file.path(dir, 'diagnosis_by_race.csv'))
    df.risk = read.hiv.atlas.file(file.path(dir, 'diagnosis_by_risk.csv'))
    
    
    years = sort(unique(c(unique(df.all$Year),
                          unique(df.age$Year),
                          unique(df.sex$Year),
                          unique(df.race$Year),
                          unique(df.risk$Year))))
    
    dim.names.all = list(year = sort(years),
                         location='national',
                         age=c('13-24 years','25-34 years','35-44 years','45-54 years','55+ years'),
                         race=c('black','hispanic','other'),
                         sex=c('female','male'),
                         risk=c('msm','idu','msm_idu','heterosexual'))
    
    surv$diagnosed.all = array(as.numeric(NA),
                                 dim=sapply(dim.names.all[c('year','location')], length),
                                 dimnames=dim.names.all[c('year','location')])
    surv$diagnosed.sex = array(as.numeric(NA),
                                 dim=sapply(dim.names.all[c('year','location','sex')], length),
                                 dimnames=dim.names.all[c('year','location','sex')])
    surv$diagnosed.age = array(as.numeric(NA),
                                 dim=sapply(dim.names.all[c('year','location','age')], length),
                                 dimnames=dim.names.all[c('year','location','age')])
    surv$diagnosed.race = race.numerators = race.denominators =
        array(0,
              dim=sapply(dim.names.all[c('year','location','race')], length),
              dimnames=dim.names.all[c('year','location','race')])
    surv$diagnosed.risk = array(as.numeric(NA),
                                  dim=sapply(dim.names.all[c('year','location','risk')], length),
                                  dimnames=dim.names.all[c('year','location','risk')])
    
##    surv$diagnosed.sex.race.risk = sex.race.risk.numerators = sex.race.risk.denominators = 
#        array(as.numeric(0),
#              dim=sapply(dim.names.all[c('year','location','race','sex','risk')], length),
#              dimnames=dim.names.all[c('year','location','race','sex','risk')])
#    surv$diagnosed.sex.age.race = sex.age.race.numerators = sex.age.race.denominators = 
#        array(as.numeric(0),
#              dim=sapply(dim.names.all[c('year','location','age','race','sex')], length),
#              dimnames=dim.names.all[c('year','location','age','race','sex')])
    
    
    race.mapping = c('American Indian/Alaska Native' = 'other',
                     'Asian' = 'other',
                     'Black/African American' = 'black',
                     'Hispanic/Latino' = 'hispanic',
                     'Native Hawaiian/Other Pacific Islander' = 'other',
                     'White' = 'other',
                     'Multiple races' = 'other')
    
    age.mapping = c('13-24' = 1,
                    '25-34' = 2,
                    '35-44' = 3,
                    '45-54' = 4,
                    '55+' = 5)
    
    risk.mapping = c('Male-to-male sexual contact' = 'msm',
                     'Injection drug use' = 'idu',
                     'Male-to-male sexual contact and injection drug use' = 'msm_idu',
                     'Heterosexual contact' = 'heterosexual',
                     'Other' = 'other')
    
    sex.mapping = c('Male'='male','Female'='female')
    
    for (i in 1:dim(df.all)[1])
    {
        if (df.all$Indicator[i]=="Knowledge of Status")
            surv$diagnosed.all[as.character(df.all$Year[i]),1] = df.all$Percent[i]/100
    }
    
    for (i in 1:dim(df.age)[1])
    {
        if (df.age$Indicator[i]=="Knowledge of Status")
            surv$diagnosed.age[as.character(df.age$Year[i]),1,age.mapping[df.age$Age.Group[i]]] = df.age$Percent[i]/100
    }
    
    for (i in 1:dim(df.race)[1])
    {
        if (df.race$Indicator[i]=="Knowledge of Status" && !is.na(df.race$Percent[i]))
        {
            race.numerators[as.character(df.race$Year[i]),1,race.mapping[df.race$Race.Ethnicity[i]]] = 
                df.race$Cases[i] +
                race.numerators[as.character(df.race$Year[i]),1,race.mapping[df.race$Race.Ethnicity[i]]]
            
            race.denominators[as.character(df.race$Year[i]),1,race.mapping[df.race$Race.Ethnicity[i]]] = 
                df.race$Cases[i] * 100 / df.race$Percent[i] +
                race.denominators[as.character(df.race$Year[i]),1,race.mapping[df.race$Race.Ethnicity[i]]]
        }
    }
    surv$diagnosed.race = race.numerators / race.denominators
    
    for (i in 1:dim(df.risk)[1])
    {
        if (df.risk$Indicator[i]=="Knowledge of Status" &&
            any(risk.mapping[df.risk$Transmission.Category[i]] == dim.names.all[['risk']]))
            surv$diagnosed.risk[as.character(df.risk$Year[i]),1,risk.mapping[df.risk$Transmission.Category[i]]] = df.risk$Percent[i]/100
    }
    
    for (i in 1:dim(df.sex)[1])
    {
        if (df.sex$Indicator[i]=="Knowledge of Status")
            surv$diagnosed.sex[as.character(df.sex$Year[i]),1,sex.mapping[df.sex$Sex[i]]] = df.sex$Percent[i]/100
    }
    
    if (any(is.na(surv$diagnosed.all)))
        stop("NAs generated in reading national diagnosis data (totals)")
    if (any(is.na(surv$diagnosed.sex)))
        stop("NAs generated in reading national diagnosis data (by sex)")
    if (any(is.na(surv$diagnosed.age)))
        stop("NAs generated in reading national diagnosis data (by age)")
    if (any(is.na(surv$diagnosed.race)))
        stop("NAs generated in reading national diagnosis data (by race)")
    if (any(is.na(surv$diagnosed.risk)))
        stop("NAs generated in reading national diagnosis data (by risk)")
    if (any(is.na(surv$diagnosed.sex.risk)))
        stop("NAs generated in reading national diagnosis data (by sex x risk)")
        
    surv
}

read.national.continuum.file <- function(file)
{
    df = read.csv(file, stringsAsFactors = F)
    for (j in 2:dim(df)[2])
    {
        if (class(df[,j])=='character')
        {
            df[,j] = gsub(' |,','',df[,j])
            df[df[,j]=='',j] = NA
            df[,j] = as.numeric(df[,j])
        }
    }
    
    df$category = gsub('â€“', '-', df$category)
    
    df
}



HIV.ATLAS.NUMERIC.COLUMNS = c('Cases',
                              'Cases.LCI',
                              'Cases.UCI',
                              'Percent',
                              'Percent.LCI',
                              'Percent.UCI',
                              'Rate.per.100000',
                              'Rate.LCI',
                              'Rate.UCI',
                              'RSE',
                              'Population')
read.hiv.atlas.file <- function(file)
{
    print(paste0("Reading HIV Atlas File: ", file))
    df = read.csv(file, stringsAsFactors = F)
    cols = intersect(names(df), HIV.ATLAS.NUMERIC.COLUMNS)

    for (col in cols)
    {
        if (class(df[,col])=='character')
        {
            df[,col] = gsub(',| ','',df[,col])
            df[!is.na(df[,col]) & (df[,col]=='' | df[,col]=='Data suppressed'), col] = NA
            df[,col] = suppressWarnings(as.numeric(df[,col]))
        }
    }
    
    df
}

##------------------------##
##-- MISC OTHER HELPERS --##
##------------------------##


#pull out a location subset
subset.surveillance <- function(surv, codes)
{
    codes = as.character(codes)
    lapply(surv, function(elem){
        if (length(dim(elem))==2)
            elem[,codes]
        else if (length(dim(elem))==3)
            elem[,codes,]
        else if (length(dim(elem))==4)
            elem[,codes,,]
        else
            stop("The subset.surveillance function is currently only set up to handle components with 2, 3, or 4 dimensions")
    })
}

