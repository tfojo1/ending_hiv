#source('../code/data_managers/locale_mappings.R')
#source('../code/data_managers/census_manager.R')


read.prep.manager <- function(dir='../data2/PrEP/Zip3 PrEP/',
                              census)
{
    #Read in the files
    files = list.files(dir, full.names = T)

    df = NULL
    for (i in 1:length(files))
        df = rbind(df, read.csv(files[i], stringsAsFactors = F))

    #Leave out PR
    df = df[sapply(df$Zip3, function(z3){!any(state.for.county(counties.for.zip3(z3))=='PR')}),]

    #Set up indices
    zip3s = as.character(sort(unique(df$Zip3)))
    years = sort(unique(df$Year))
    ages = c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years')
    sexes = c('heterosexual_male', 'msm', 'female')
    races = census$races

    dim.names = list(year=as.character(years), zip3=zip3s, age=ages, race=races, sex=sexes)
    denominators.1 = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)

    dim.names$race = c('black','hispanic','other')
    numerators = denominators.2 = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)

    #Load up the denominators from the census
    for (i in 1:length(zip3s))
    {
        denominators.1[,i,,,] = get.census.data(census, years=years,
                                             fips=counties.for.zip3(zip3s[i]),
                                             sexes = sexes,
                                             races = races,
                                             aggregate.races = F, aggregate.counties = T)
    }
    denominators.without.race = apply(denominators.1, c('year','zip3','age','sex'), sum)

    denominators.2[,,,'black',] = denominators.1[,,,'black',]
    denominators.2[,,,'hispanic',] = denominators.1[,,,'hispanic',]
    denominators.2[,,,'other',] = apply(denominators.1, c('year','zip3','age','sex'), sum) - denominators.1[,,,'black',] - denominators.1[,,,'hispanic',]

    #Replace -1 numbers for total, male, and female
    total.female.users = sum(df$Female.PrEP.Users[df$Female.PrEP.Users>-1])
    total.male.users = sum(df$Male.PrEP.Users[df$Male.PrEP.Users>-1])
    proportion.users.male = total.male.users / (total.male.users + total.female.users)

    df$Zip3.PrEP.Users[df$Zip3.PrEP.Users==-1] = 2.5 #the average of 1,2,3,4
    missing.by.sex = df$Female.PrEP.Users==-1
    df$Male.PrEP.Users[missing.by.sex] = df$Zip3.PrEP.Users[missing.by.sex] * proportion.users.male
    df$Female.PrEP.Users[missing.by.sex] = df$Zip3.PrEP.Users[missing.by.sex] * (1-proportion.users.male)


    #Load up numerators for male/female
    numerators.male = array(NA, dim=sapply(dim.names[c('year','zip3')], length), dimnames=dim.names[c('year','zip3')])
    numerators.female = array(NA, dim=sapply(dim.names[c('year','zip3')], length), dimnames=dim.names[c('year','zip3')])

    for (i in 1:dim(df)[1])
    {
        numerators.male[as.character(df$Year[i]), as.character(df$Zip3[i])] = df$Male.PrEP.Users[i]
        numerators.female[as.character(df$Year[i]), as.character(df$Zip3[i])] = df$Female.PrEP.Users[i]
    }


    #Split the male into msm and heterosexual
    na.mask = is.na(numerators.female)
    global.female.prep.rate = sum(numerators.female[!na.mask]) / sum(apply(denominators.without.race[,,,'female'], 3, function(x){sum(x[!na.mask])}))
    expected.heterosexual.global.numerator = global.female.prep.rate * sum(apply(denominators.without.race[,,,'heterosexual_male'], 3, function(x){sum(x[!na.mask])}))
    assumed.msm.global.numerator = sum(numerators.male[!na.mask]) - expected.heterosexual.global.numerator
    assumed.global.msm.prep.rate = assumed.msm.global.numerator / sum(apply(denominators.without.race[,,,'msm'], 3, function(x){sum(x[!na.mask])}))

    zip3.female.prep.rate = numerators.female / apply(denominators.without.race[,,,'female'], c('year','zip3'), sum)
    numerators.heterosexual.male = sapply(zip3s, function(zip3){
        sapply(as.character(years), function(year){
            max(0,
                get.approximate.means.of.binomials.conditioned.on.sum(sum(denominators.without.race[year, zip3, , 'heterosexual_male']), zip3.female.prep.rate[year, zip3],
                                                      sum(denominators.without.race[year, zip3, , 'msm']), assumed.global.msm.prep.rate,
                                                      numerators.male[year, zip3])
            )
        })
    })
    numerators.msm = numerators.male - numerators.heterosexual.male

    #Age
    users.by.age = as.matrix(df[,c('Age.LE.24.PrEP.Users','Age.25.34.PrEP.Users','Age.35.44.PrEP.Users','Age.45.54.PrEP.Users','Age.55..PrEP.Users')])
    missing = users.by.age==-1
    missing.none = !apply(missing, 1, any)

    weights.for.age = colSums(users.by.age[missing.none,])
    unaccounted = pmax(0,df$Zip3.PrEP.Users - rowSums(users.by.age * !missing))


    for (i in 1:dim(df)[1])
    {
        if (!missing.none[i])
        {
            missing.mask = missing[i,]
            users.by.age[i,missing.mask] = unaccounted[i] * weights.for.age[missing.mask] / sum(weights.for.age[missing.mask])
        }
    }

    age.proportions.long.form = users.by.age / rowSums(users.by.age)
    proportions.by.age = array(NA, dim=sapply(dim.names[1:3], length), dimnames=dim.names[1:3])

    for (i in 1:dim(df)[1])
    {
        proportions.by.age[as.character(df$Year[i]), as.character(df$Zip3[i]),] = age.proportions.long.form[i,]
    }

    #Race
    # From Bush S, Magnuson D, Rawlings MK, et al. Racial characteristics of FTC/TDF for pre-exposure prophylaxis users in the U.S. Paper presented at: 2016 ASM Microbe; June 16-20, 2016; Boston. Session 371.
    # http://hivinsite.ucsf.edu/insite?page=hmq-1609-06
    # *Sharp increase beginning in 2014
    black.prep.numerator.proportion = 0.10
    hispanic.prep.numerator.proportion = 0.12
    other.prep.numerator.proportion = 1-black.prep.numerator.proportion-hispanic.prep.numerator.proportion
    #white.prep.numerator.proportion=0.74

    global.prep.numerator = sum(numerators.female[!na.mask] + numerators.male[!na.mask])
    global.black.denominator = sum(apply(denominators.1[,,,'black',], 3:4, function(x){sum(x[!na.mask])}))
    global.hispanic.denominator = sum(apply(denominators.1[,,,'hispanic',], 3:4, function(x){sum(x[!na.mask])}))
    global.other.denominator = sum(denominators.without.race) - global.black.denominator - global.hispanic.denominator

    global.black.prep.rate = global.prep.numerator * black.prep.numerator.proportion / global.black.denominator
    global.hispanic.prep.rate = global.prep.numerator * hispanic.prep.numerator.proportion / global.hispanic.denominator
    global.other.prep.rate = global.prep.numerator * other.prep.numerator.proportion / global.other.denominator

    global.prep.rates.by.race = c(black=global.black.prep.rate, hispanic=global.hispanic.prep.rate, other=global.other.prep.rate)
    #Pull it all together


    for (year in as.character(years))
    {
        for (zip3 in zip3s)
        {
            female.by.age = numerators.female[year, zip3] * proportions.by.age[year, zip3, ]
            het.male.by.age = numerators.heterosexual.male[year, zip3] * proportions.by.age[year, zip3, ]
            msm.by.age = numerators.msm[year, zip3] * proportions.by.age[year, zip3, ]

            for (age in 1:length(ages))
            {
                race.denominators = t(denominators.2[year, zip3, age,,])
#                race.denominators = cbind(black=race.denominators[,'black'],
#                                          hispanic=race.denominators[,'hispanic'],
#                                          other=rowSums(race.denominators)-race.denominators[,'black']-race.denominators[,'hispanic'])
                race.weights = race.denominators * rep(global.prep.rates.by.race, each=3)
                proportions.by.race = race.weights / rowSums(race.weights)

                numerators[year, zip3, age,, 'heterosexual_male'] = het.male.by.age[age] * proportions.by.race['heterosexual_male',]
                numerators[year, zip3, age,, 'msm'] = het.male.by.age[age] * proportions.by.race['msm',]
                numerators[year, zip3, age,, 'female'] = het.male.by.age[age] * proportions.by.race['female',]
            }
        }
    }


    #Package up and return
    rv = list(years=years,
              zip3s=zip3s,
              ages=ages,
              races=dim.names[['race']],
              sexes=sexes,
              rates=numerators/denominators.2)
}


get.prep.rates <- function(prep.manager,
                           counties,
                           years,
                           ages=prep.manager$ages,
                           races=prep.manager$races,
                           sexes=prep.manager$sexes,
                           throw.error.if.missing=T)
{
    not.in.manager.years = setdiff(years, prep.manager$years)
    if (length(not.in.manager.years)>0)
        stop(paste0("The following years are not represented in the PrEP manager: ",
                    paste0(not.in.manager.years, collapse=', ')))

    dim.names = list(year=as.character(years), county=counties, age=ages, race=races, sex=sexes)
    rv = array(as.numeric(NA), dim=sapply(dim.names,length), dimnames=dim.names)
    for (county in counties)
    {
        zip3s = zip3s.for.county(county)
        not.in.manager.mask = is.na(zip3s) | sapply(zip3s, function(z3){!any(z3==prep.manager$zip3s)})
        if (all(not.in.manager.mask))
        {
            if (throw.error.if.missing)
            stop(paste0("The following county does not have any PrEP data in the manager: ",
                        paste0(county.names(county), collapse=", ")))
        }
        else
        {
            zip3s = zip3s[!not.in.manager.mask]
    
            prep.rates = prep.manager$rates[as.character(years), zip3s, ages, races, sexes]
            prep.rates = apply(prep.rates, intersect(c('year','age','race','sex'), names(dimnames(prep.rates))), mean)
            rv[,county,,,] = prep.rates
        }
    }

    rv
}

#uses normal approximation to binomial
get.approximate.means.of.binomials.conditioned.on.sum <- function(n1, p1,
                                                                  n2, p2,
                                                                  x1.plus.x2,
                                                                  return.only.first.mean=T)
{
    mu1 = n1*p1
    mu2 = n2*p2
    v1 = n1 * p1 * (1-p1)
    v2 = n2 * p2 * (1-p2)

    mean.1 = mu1 + v1 / (v1 + v2) * (x1.plus.x2 - mu1 - mu2)
    if (return.only.first.mean)
        mean.1
    else
        c(mean.1=mean.1, mean.2=x1.plus.x2-mean.1)
}

get.means.of.binomials.conditioned.on.sum <- function(n1, p1,
                                                      n2, p2,
                                                      x1.plus.x2,
                                                      return.only.first.mean=T)
{
    x1 = 0:n1
    f.x1 = dbinom(x1, n1, p1) * dbinom(x1.plus.x2-x1, n2, p2)

#    x2 = 0:n2
#    f.x2 = dbinom(x2, n2, p2) * dbinom(x1.plus.x2-x2, n1, p1)

#    c(mean.1 = sum(x1*f.x1)/sum(f.x1),
#      mean.2 = sum(x2*f.x2)/sum(f.x1))

    mean.1 = sum(x1*f.x1)/sum(f.x1)
    if (return.only.first.mean)
        mean.1
    else
        c(mean.1=mean.1, mean.2=x1.plus.x2-mean.1)
}
