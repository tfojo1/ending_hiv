
library(sas7bdat)
#source('../code/data_managers/locale_mappings.R')


MAP.RACE.SEX.TO.SEX = c('male',
                    'female',
                    'male',
                    'female',
                    'male',
                    'female',
                    'male',
                    'female')

MAP.RACE.SEX.TO.RACE = c('white',
                         'white',
                         'black',
                         'black',
                         'american_indian_or_alaska_native',
                         'american_indian_or_alaska_native',
                         'asian',
                         'asian')

MAP.ETHNICITY.TO.HISPANIC = c(F,T)
HISPANIC = 'hispanic'

BRIDGED.RACE.PLUS.HISPANIC = c(unique(MAP.RACE.SEX.TO.RACE), HISPANIC)

# white_male=1,
# white_female=2,
# black_male=3,
# black_female=4,
# american_indian_or_alaska_native_male=5,
# american_indian_or_alaska_native_female=6,
# asian_male=7,
# asian_female=8

YEARS.0609 = c('2006'='POP2006',
               '2007'='POP2007',
               '2008'='POP2008',
               '2009'='POP2009')

YEARS.1017 = c('2010'='POP2010_jul',
               '2011'='POP2011',
               '2012'='POP2012',
               '2013'='POP2013',
               '2014'='POP2014',
               '2015'='POP2015',
               '2016'='POP2016',
               '2017'='POP2017')

#CDC census estimates (bridged race)
# https://www.cdc.gov/nchs/nvss/bridged_race.htm

read.census <- function(dir='../data2/Census/', update.frequency=0.05)
{
    #-- Read in the files --#
    print('Reading in 2006-2009 Data')
    data.0609 = read.sas7bdat(paste0(dir, '2006-2009.sas7bdat'))

    print('Reading in 2010-2017 Data')
    data.1017 = read.sas7bdat(paste0(dir, '2010-2017.sas7bdat'))
    names(data.1017)[names(data.1017)=='age'] = 'AGE'

    #-- Process FIPS codes --#
    data.0609$fips = combined.fips(data.0609$ST_FIPS, data.0609$CO_FIPS)
    data.0609$fips = fix.census.fips(data.0609$fips)

    data.1017$fips = combined.fips(data.1017$ST_FIPS, data.1017$CO_FIPS)
    data.1017$fips = fix.census.fips(data.1017$fips)


    #-- Set up indices --#
    years = c(names(YEARS.0609), names(YEARS.1017))
    race.names = BRIDGED.RACE.PLUS.HISPANIC
    sex.names = unique(MAP.RACE.SEX.TO.SEX)
    unique.fips = unique(c(unique(data.0609$fips), unique(data.1017$fips)))
    ages = 0:85
    age.names = as.character(ages)

    data.0609 = set.up.census.indices(data.0609, race.names=race.names, sex.names=sex.names, separate.hispanic=T)
    data.1017 = set.up.census.indices(data.1017, race.names=race.names, sex.names=sex.names, separate.hispanic=T)


    #-- Set up the array --#
    rv = list()
    dim.names = list(year=years, county=unique.fips, age=age.names, race=race.names, sex=sex.names)
    rv$data = array(0,
                    dim=sapply(dim.names, length),
                    dimnames = dim.names)


    #-- Map 2006-2009 Data --#
    N = dim(data.0609)[1]
    update.interval = ceiling(N * update.frequency)
    num.updates = ceiling(N/update.interval)
    year.names = names(YEARS.0609)

    print('Extracting 2006-2009 Data into Dataset...')
    for (update in 1:num.updates)
    {
        for (i in ((update-1)*update.interval + 1):min(N, update * update.interval))
        {
            rv$data[year.names, data.0609$fips[i], data.0609$age.index[i], data.0609$race.index[i], data.0609$sex.index[i]] =
                as.numeric(rv$data[year.names, data.0609$fips[i], data.0609$age.index[i], data.0609$race.index[i], data.0609$sex.index[i]] +
                            data.0609[i, YEARS.0609])
        }
        print(paste0('   ', round(100*update*update.frequency), '% done extracting 2006-2009 data'))
    }


    #-- Map 2010-2017 Data --#
    N = dim(data.1017)[1]
    update.interval = ceiling(N * update.frequency)
    num.updates = ceiling(N/update.interval)
    year.names = names(YEARS.1017)

    print('Extracting 2010-2017 Data into Dataset...')
    for (update in 1:num.updates)
    {
        for (i in ((update-1)*update.interval + 1):min(N, update * update.interval))
        {
            rv$data[year.names, data.0609$fips[i], data.0609$age.index[i], data.0609$race.index[i], data.0609$sex.index[i]] =
                as.numeric(rv$data[year.names, data.0609$fips[i], data.0609$age.index[i], data.0609$race.index[i], data.0609$sex.index[i]] +
                            data.1017[i, YEARS.1017])
        }
        print(paste0('   ', round(100*update*update.frequency), '% done extracting 2010-2017 data'))
    }


    #-- Package up and return --#

    rv$years = as.integer(year.names)

    rv$combined.fips = unique.fips

    rv$age.lowers = ages
    rv$age.uppers = ages+1
    rv$age.names = age.names

    rv$races = race.names
    rv$sexes = sex.names

    rv
}

#A helper
set.up.census.indices <- function(dataset,
                                  race.names,
                                  sex.names,
                                  separate.hispanic = T)
{
    #Age
    dataset$age.index = dataset$AGE + 1

    #Race
    race.names.to.index = 1:length(race.names)
    names(race.names.to.index) = race.names
    census.race.to.index = race.names.to.index[MAP.RACE.SEX.TO.RACE]
    dataset$race.index = census.race.to.index[dataset$RACESEX]

    if (separate.hispanic)
    {
        hispanic.index = (1:length(race.names))[race.names=='hispanic']
        dataset$race.index[MAP.ETHNICITY.TO.HISPANIC[dataset$hisp]] = hispanic.index
    }

    #Sex
    sex.names.to.index = 1:length(sex.names)
    names(sex.names.to.index) = sex.names
    census.sex.to.index = sex.names.to.index[MAP.RACE.SEX.TO.SEX]
    dataset$sex.index = census.sex.to.index[dataset$RACESEX]

    #Return the result
    dataset
}


#From Emory work:
# http://www.emorycamp.org/item.php?i=28
# https://publichealth.jmir.org/2016/1/e14/
read.msm.proportions <- function(dir='cleaned_data')
{
    msm.estimates = read.csv(paste0(dir, 'US MSM Estimates 2013.csv'))
    fips = combined.fips(msm.estimates$STATEFP, msm.estimates$COUNTYFP)
    rv = msm.estimates$MSM5YEAR/msm.estimates$ADULTMEN
    names(rv) = fips
    rv
}

stratify.census.msm <- function(census, msm.proportions)
{
    census$data = stratify.population.msm(census$data, msm.proportions)
    census$sexes = dimnames(census$data)[[5]]

    census
}

stratify.population.msm <- function(population, msm.proportions)
{
    new.sex.names = c('heterosexual_male', 'msm', 'female')
    new.dim.names = c(dimnames(population)[1:4], list(sex=new.sex.names))
    new.data = array(NA,
                     dim=sapply(new.dim.names, length),
                     dimnames = new.dim.names)

    msm.proportions = msm.proportions[dimnames(population)[[2]]]

    for (county in dimnames(population)[['county']])
    {
        new.data[,county,,,'female'] = population[,county,,,'female']
        new.data[,county,,,'msm'] = population[,county,,,'male'] * msm.proportions[county]
        new.data[,county,,,'heterosexual_male'] = population[,county,,,'male'] * (1 - msm.proportions[county])
    }

    new.data
}

collapse.census.ages <- function(census,
                                 age.cutoffs=c(13,25,35,45,55))
{
    n.ages = length(age.cutoffs)
    new.age.lowers = age.cutoffs
    new.age.uppers = c(age.cutoffs[-1],Inf)
    new.age.names = c(paste0(age.cutoffs[-n.ages], '-', age.cutoffs[-1]-1, ' years'), paste0(age.cutoffs[n.ages], '+ years'))

    new.from.old.fractions = lapply(1:n.ages, function(new.i){
        sapply(1:length(census$age.lowers), function(old.i){
            lower.bound = max(new.age.lowers[new.i], census$age.lowers[old.i])
            upper.bound = min(new.age.uppers[new.i], census$age.uppers[old.i], 100)
            max(0, (upper.bound-lower.bound) / (census$age.uppers[old.i]-census$age.lowers[old.i]))
        })
    })

    dim.names = c(dimnames(census$data)[1:2], list(age=new.age.names), dimnames(census$data)[4:5])
    new.data = array(NA,
                     dim=sapply(dim.names, length),
                     dimnames = dim.names)

    for (i in 1:n.ages)
    {
        new.data[,,i,,] = apply(census$data, c(1:2,4:5), function(vals){
            sum(vals * new.from.old.fractions[[i]])
        })
    }

    census$data = new.data

    census$age.lowers = new.age.lowers
    census$age.uppers = new.age.uppers
    census$age.names = new.age.names

    census
}

WBHO.mapping = c(african_american_nonhispanic='african_american',
                 white_nonhispanic='white',
                 native_american_nonhispanic='other',
                 hawaiian_pacific_islander_nonhispanic='other',
                 asian_american_nonhispanic='other',
                 multiracial_nonhispanic='other',
                 african_american_hispanic='hispanic',
                 white_hispanic='hispanic',
                 native_american_hispanic='hispanic',
                 hawaiian_pacific_islander_hispanic='hispanic',
                 asian_american_hispanic='hispanic',
                 multiracial_hispanic='hispanic'
)

collapse.census.reth <- function(census,
                                 collapse.to = WBHO.mapping)
{
    new.reth = unique(collapse.to)

    dim.names = c(dimnames(census$data)[1:4], list(new.reth))
    new.data = array(NA,
                     dim=sapply(dim.names, length),
                     dimnames = dim.names)

    for (reth in new.reth)
    {
        old.reth = names(collapse.to)[collapse.to==reth]
        if (length(old.reth)>1)
            new.data[,,,,reth] = apply(census$data[,,,,old.reth], 1:4, sum)
        else
            new.data[,,,,reth] = census$data[,,,,old.reth]
    }

    census$data = new.data

    census
}



# If all aggregate flags are off
#  Returns array indexed[year][county][age][sex][race]
# If aggregate flags set, the corresponding dimensions are summed out
get.census.data <- function(census,
                            years=census$years,
                            fips=census$combined.fips,
                            ages=census$age.names,
                            races=census$races,
                            sexes=census$sexes,
                            aggregate.years=F,
                            aggregate.counties=F,
                            aggregate.ages=F,
                            aggregate.races=F,
                            aggregate.sexes=F)
{
    fips = format.combined.fips(fips)

    rv = census$data[as.character(years), fips, as.character(ages), races, sexes]

    new.dimnames = c(list(year=as.character(years), county=fips, age=ages, race=races, sex=sexes))
    dim(rv) = sapply(new.dimnames, length)
    dimnames(rv) = new.dimnames

    aggregate.mask = c(aggregate.years, aggregate.counties, aggregate.ages, aggregate.races, aggregate.sexes)
    if (all(aggregate.mask))
        sum(rv)
    else
        apply(rv, (1:5)[!aggregate.mask], sum)
}

