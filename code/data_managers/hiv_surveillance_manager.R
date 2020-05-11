#source('../code/data_managers/locale_mappings.R')
#source('../code/setup/setup_helpers.R')

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
                                  throw.error.if.missing.data=T)
{
    #Check the data.type argument
    ALLOWED.DATA.TYPES = c('prevalence','new','mortality','diagnosed',
                           'estimated.prevalence', 'estimated.prevalence.ci.lower',
                           'estimated.prevalence.ci.upper', 'estimated.prevalence.rse',
                           'cumulative.aids.mortality', 'aids.diagnoses')
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
        stop(paste0("The following location code(s) are not present in the requested data: ",
                    paste0(location.codes[missing.code], collapse=', ')))

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
        stop("No data is available for any of the requested years")

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

    data = apply(data, keep.dimnames.names, sum)
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

has.location.surveillance <- function(surv, location)
{
    any(sapply(surv, function(elem){
        !is.null(dim(elem)) && !is.null(dimnames(elem)[['location']]) && any(location==dimnames(elem)[['location']])
    }))
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

aggregate.surveillance.race.as.bho <- function(surv)
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
                    access(rv, race='other') = access(rv, race='other') + access(elem, race=race)
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
            access(rv, risk='heterosexual') = access(rv, risk='heterosexual') + access(elem, risk='other')

            rv
        }
        else
            elem
    })
}

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

read.msa.surveillance <- function(dir='../data2/HIV_Surveillance/by_msa',
                                  use.adjusted.estimate=F,
                                  correct.new.to.county.level=T,
                                  correct.prevalence.to.county.level=T,
                                  county.file='../data2/HIV_Surveillance/by_county.csv',
                                  first.total.prevalence.year=2007,
                                  first.total.new.year=2008,
                                  verbose=F)
{
    #-- READ IN FILES --#
    sub.dirs = list.dirs(dir)

    filenames.full = unlist(sapply(sub.dirs, list.files, include.dirs=F, no..=T, pattern='.csv$', full.names=T))
    filenames = unlist(sapply(sub.dirs, list.files, include.dirs=F, no..=T, pattern='.csv$'))

    remove = filenames == 'state_diagnosed_data.csv' | grepl('ryan_white', filenames)
    filenames = filenames[!remove]
    filenames.full = filenames.full[!remove]

    is.total = grepl('total', filenames)

#    dfs = lapply(filenames.full, read.msa.file, verbose=verbose)
    dfs = lapply(1:length(filenames.full), function(i){
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
                          correct.new.to.county.level=correct.new.to.county.level,
                          correct.prevalence.to.county.level=correct.prevalence.to.county.level))

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
    include.children.in.prevalence = correct.prevalence.to.county.level
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
    rv$cumulative.aids.mortality.sex.race.risk = read.cum.aids.mortality(file=file.path(dir, 'msa_mortality_pre_2000.txt'))
    dim.names = c(list(year='2000'), dimnames(rv$cumulative.aids.mortality.sex.race.risk))
    dim(rv$cumulative.aids.mortality.sex.race.risk) = sapply(dim.names, length)
    dimnames(rv$cumulative.aids.mortality.sex.race.risk) = dim.names
    
    
    #-- AGGREGATE FOR THE TOTAL POPULATION NUMBERS and AGE, RACE, RISK --#
#    rv$new.all = apply(rv$new.sex, c('year','location'), sum)
#    rv$prevalence.all = apply(rv$prevalence.sex, c('year','location'), sum)
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


    #-- AGGREGATED COUNTY-LEVEL TOTALS --#
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
    }



    if (1==2) #not doing this anymore
    {
        if (verbose)
            print("Plugging in Aggregate County-Level Data")
        old.new.all = rv$new.all
        old.new.years = dimnames(old.new.all)['year']
        old.prev.all = rv$prevalence.all
        old.prev.years = dimnames(old.prev.all)['year']

        rv$new.all = msa.by.county$new.all
        rv$prevalence.all = msa.by.county$prevalence.all

        rv$new.all[old.new.years,][!is.na(old.new.all)] = old.new.all[!is.na(old.new.all)]
        rv$prevalence.all[old.prev.years,][!is.na(old.prev.all)] = old.prev.all[!is.na(old.prev.all)]
    }

    #-- AIDS DIAGNOSES --#
    old.aids.diagnoses = read.aids.diagnoses(file.path(dir, 'aids_diagnoses_pre_2002.txt'))
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

    #-- READ STATE-LEVEL PROPORTION DIAGNOSED --#
    state.diagnoses = read.state.diagnoses(paste0(dir, '/state_diagnosed_data.csv'))
    dim.names = list(year=dimnames(state.diagnoses)[['year']], location=dimnames(rv$new.all)[['location']])
    rv$diagnosed.all = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    for (msa in dim.names[['location']])
    {
        states = intersect(states.for.msa(division.to.msa(msa)), dimnames(state.diagnoses)[['location']])

        if (length(states)==1)
            rv$diagnosed.all[,msa] = state.diagnoses[,states]
        else if (length(states)>1)
            rv$diagnosed.all[,msa] = rowMeans(state.diagnoses[,states], na.rm = T)
    }


    #-- RETURN IT --#
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

read.aids.diagnoses <- function(file='../data2/HIV_Surveillance/by_msa/aids_diagnoses_pre_2002.txt')
{
    df = read.table(file, sep='\t', stringsAsFactors = F, header = T)
    df$code = cbsa.for.msa.name(df$Location)
    df = df[!is.na(df$code),]
    df$Year.Reported = as.character(df$Year.Reported)
    df$Year.Reported[df$Year.Reported=='Before 1982'] = '1981'
    
    dim.names = list(year = sort(unique(df$Year.Reported)), location=unique(df$code))
    rv = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    for (i in 1:dim(df)[1])
        rv[df$Year.Reported[i], df$code[i]] = df$Cases[i]
    
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
        browser()

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

read.state.surveillance.manager <- function(dir='../data2/HIV_Surveillance/by_state/')
{

    df.male = read.state.file(file.path(dir, 'All State male.csv'))
    df.female = read.state.file(file.path(dir, 'All State female.csv'))
    df.male$Sex='male'
    df.female$Sex='female'
    df = rbind(df.male, df.female)


    states = unique(df$State)
    years = as.character(unique(df$Year))
    ages = sort(unique(as.character(df$Age.Group)))

    dim.names = list(year=years, location=states, age=ages, race=c('black','hispanic','other'),
                     sex=c('male','female'), risk=c('MSM','IDU','MSM_IDU','heterosexual'))


    rv = list()
    rv$new.master = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$prevalence.master = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$mortality.master = array(0, dim=sapply(dim.names, length), dimnames=dim.names)

#df = df[df$Year=='2016' & df$State=='MD' & df$Sex=='male' & grepl('black',df$Race.Ethnicity, ignore.case = T) & df$Indicator=='HIV diagnoses',]
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
                risk.to = 'MSM_IDU'
            else
                risk.to = "IDU"
        }
        else if (grepl('male-to-male', df$Transmission.Category[i], ignore.case = T))
            risk.to = 'MSM'
        else
            risk.to = 'heterosexual'

        rv[[elem]][df$Year[i], df$State[i], df$Age.Group[i], race.to, df$Sex[i], risk.to] = df$Cases[i] +
            rv[[elem]][df$Year[i], df$State[i], df$Age.Group[i], race.to, df$Sex[i], risk.to]

 #       browser()
    }

    ##-- Estimated Prevalence --##

    df.est.prev = read.state.estimated.prevalence.file(file.path(dir, 'All State estimated prevalence.csv'))
    df.est.prev$Year = as.character(df.est.prev$Year)

    est.prev.dim.names = list(year=unique(df.est.prev$Year), location=unique(df.est.prev$State))
    rv$estimated.prevalence.all = rv$estimated.prevalence.ci.lower.all =
        rv$estimated.prevalence.ci.upper.all = rv$estimated.prevalence.rse.all =
        array(0, dim=sapply(est.prev.dim.names, length), dimnames=est.prev.dim.names)

    for (i in 1:dim(df.est.prev)[1])
    {
        rv$estimated.prevalence.all[df.est.prev$Year[i], df.est.prev$State[i]] = df.est.prev$estimate[i]
        rv$estimated.prevalence.ci.lower.all[df.est.prev$Year[i], df.est.prev$State[i]] = df.est.prev$ci.lower[i]
        rv$estimated.prevalence.ci.upper.all[df.est.prev$Year[i], df.est.prev$State[i]] = df.est.prev$ci.upper[i]
        rv$estimated.prevalence.rse.all[df.est.prev$Year[i], df.est.prev$State[i]] = df.est.prev$rse[i]
    }

    ##-- Diagnosed --##
    rv$diagnosed.all = read.state.diagnoses(paste0(dir, '/state_diagnosed_data.csv'))

    rv
}

read.total.msa.data.from.counties <- function(msas,
                                              county.file='../data2/HIV_Surveillance/by_county.csv',
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

    rv$new.all[rv$new.all<0] = NA
    rv$prevalence.all[rv$prevalence.all<0] = NA

    rv$new.all = rv$new.all+1
    rv$prevalence.all = rv$prevalence.all+1

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
