
if (1==2)
{
    cm = create.continuum.manager(national.surveillance = national.surveillance,
                                  suppression.anchor.year = 2020,
                                  testing.anchor.year = 2020)
    ALL.DATA.MANAGERS$continuum = cm
    save(ALL.DATA.MANAGERS, file='cached/ALL.DATA.MANAGERS.Rdata')
}

get.suppression.model <- function(cm,
                                  location)
{
    list(intercept=cm$suppression$stratified.log.odds.intercept,
         slope=cm$suppression$stratified.log.odds.slope,
         anchor.year = cm$suppression$anchor.year,
         max.proportion = cm$suppression$max.proportion)
}

#Returns a list with 3 components, representing a linear log-odds model of testing
# $intercept - an age x race x sex x risk array of log-odds intercepts
# $slope - an age x race x sex x risk array of log-odds slopes (relative to time)
# $anchor year - the year that corresponds to the intercept
get.testing.model <- function(cm,
                              location,
                              population)
{
    location.mask = cm$testing$total.ever.tested$location==location
    if (any(location.mask))
    {
        ever.tested = cm$testing$total.ever.tested$frac.ever[location.mask]
        years = cm$testing$total.ever.tested$year[location.mask] - cm$testing$anchor.year
        sample.size = cm$testing$total.ever.tested$sample.size[location.mask]
    }
    else #average fraction ever tested for locations in the same state
    {
        location.states = states.for.msa(location)
        locations = unique(cm$testing$total.ever.tested$location)
        same.state.mask = sapply(locations, function(loc){
            states.for.loc = states.for.msa(loc)
            any(sapply(location.states, function(st){
                any(st == states.for.loc)
            }))
        })
        if (any(same.state.mask))
        {
            locations = locations[same.state.mask]
            location.mask = sapply(cm$testing$total.ever.tested$location, function(loc){
                any(loc==locations)
            })
        }
        else
            location.mask = T
        
        years = sort(unique(cm$testing$total.ever.tested$year[location.mask]))
        ever.tested = sapply(years, function(year){
            year.mask = location.mask & cm$testing$total.ever.tested$year == year
            sum(cm$testing$total.ever.tested$frac.ever[year.mask] * cm$testing$total.ever.tested$sample.size[year.mask]) /
                sum(cm$testing$total.ever.tested$sample.size[year.mask])
        })
        sample.size = sapply(years, function(year){
            year.mask = location.mask & cm$testing$total.ever.tested$year == year
            ceiling(mean(cm$testing$total.ever.tested$sample.size[year.mask]))
        })
    }
    frac.12mo = 1-exp(cm$testing$rate.ever.to.12mo.mult * log(1-ever.tested))
    xs = round(frac.12mo*sample.size)
    
    summed.population = sum(population)
    fn <- function(params)
    {
        p.by.year = sapply(years, function(year){
            sum(population / (1 + exp(-(cm$testing$stratified.log.odds.intercept + params[1] + 
                                            year*cm$testing$stratified.log.odds.slope + year*params[2]))) * 
                    cm$testing$max.proportion) /
                summed.population
        })
        -sum(dbinom(xs, size=sample.size, prob=p.by.year, log=T))
    }

    opt = optim(par=c(0,0), 
                fn = fn,control = list(maxit=500))
    
    list(intercept = cm$testing$stratified.log.odds.intercept + opt$par[1],
         slope = cm$testing$stratified.log.odds.slope + opt$par[2],
         anchor.year = cm$testing$anchor.year,
         max.proportion = cm$testing$max.proportion)
}

logit <- function(x){log(x) - log(1-x)}
expit <- function(x){1/(1+exp(-x))}

##------------------------------------##
##-- CREATE A NEW CONTINUUM MANAGER --##
##------------------------------------##

create.continuum.manager <- function(dir='cleaned_data/', 
                                     settings = SETTINGS,
                                     national.surveillance = national.surveillance,
                                     suppression.anchor.year = 2020,
                                     testing.anchor.year = 2020,
                                     max.tested.proportion = 0.9,
                                     max.suppressed.proportion = 0.9,
                                     verbose=T)
{
    cm = list()
    
    if (verbose)
        print("Reading Suppression")
    cm = run.suppression.regressions(cm,
                                     national.surveillance = national.surveillance,
                                     max.suppressed.proportion=max.suppressed.proportion,
                                     anchor.year=suppression.anchor.year,
                                     settings=settings)
    
    if (verbose)
        print("Reading Testing")
    cm = run.testing.regressions(cm,
                                 dir=dir,
                                 verbose=verbose,
                                 max.tested.proportion=max.tested.proportion,
                                 anchor.year = testing.anchor.year,
                                 settings=settings)
    
    cm
}

##------------------------##
##-- SET UP SUPPRESSION --##
##------------------------##

run.suppression.regressions <- function(cm,
                                        national.surveillance,
                                        max.suppressed.proportion=0.9,
                                        anchor.year=2020,
                                        settings = SETTINGS)
{   
    #-- Fit Sex x Risk --#
    
    df = melt(national.surveillance$suppression.sex.risk)
    counts = melt(national.surveillance$prevalence.for.continuum.sex.risk)
    df$n = counts$value
    df = df[df$sex != 'female' | (df$risk!='msm' & df$risk!='msm_idu'),]
    
    df$value = pmin(df$value / max.suppressed.proportion, .9999)
    
    df$year = df$year - anchor.year
    
    df$msm = as.numeric(df$risk=='msm')
    df$msm_idu = as.numeric(df$risk=='msm_idu')
    df$heterosexual_female = as.numeric(df$risk=='heterosexual' & df$sex=='female')
    df$heterosexual_male = as.numeric(df$risk=='heterosexual' & df$sex=='male')
    df$idu_female = as.numeric(df$risk=='idu' & df$sex=='female')
    df$idu_male = as.numeric(df$risk=='idu' & df$sex=='male')
    
    fit.sex.risk = suppressWarnings(glm(value ~ msm + year:msm +
                  msm_idu + year:msm_idu +
                  heterosexual_female + year:heterosexual_female +
                  heterosexual_male + year:heterosexual_male +
                  idu_female + year:idu_female +
                  idu_male + year:idu_male +
                  0,
              data=df, family='binomial'))
    
    #-- The 'All' Data Frame --#
    
    df.all = melt(national.surveillance$suppression.all)
    df.all$n = national.surveillance$prevalence.for.continuum.all[,1]
    
    df.all$value = pmin(df.all$value / max.suppressed.proportion, .9999)
    
    #-- Fit Race --#
    
    df = melt(national.surveillance$suppression.race)
    df$n = melt(national.surveillance$prevalence.for.continuum.all)$value
    df$value = pmin(df$value / max.suppressed.proportion, .9999)
    
    df = rbind(df,
               data.frame(year=df.all$year,
                          location='national',
                          race='all',
                          value=df.all$value,
                          n=df.all$n))
    
    df$black = as.numeric(df$race=='black')
    df$hispanic = as.numeric(df$race=='hispanic')
    df$other = as.numeric(df$race=='other')
    
    df$year = df$year - anchor.year
    
    fit.race = suppressWarnings(glm(value ~ black + hispanic + other +
                       black:year + hispanic:year + other:year + year,
                   data=df, family='binomial', weight=n))
    
    #-- Fit Age --#
    
    df = melt(national.surveillance$suppression.age)
    df$n = melt(national.surveillance$prevalence.for.continuum.age)$value
    df$value = pmin(df$value / max.suppressed.proportion, .9999)
    
    ages = sort(unique(as.character(df$age)))
    
    df = rbind(df,
               data.frame(year=df.all$year,
                          location='national',
                          age='all',
                          value=df.all$value,
                          n=df.all$n))
    
    
    for (age in 1:length(ages))
        df[,paste0('age',age)] = as.numeric(df$age==ages[age])
    
    df$year = df$year - anchor.year
    
    ff = as.formula(paste0('value ~ ',
                           paste0('age', 1:length(ages), collapse=' + '),
                           " + ",
                           paste0('year:age', 1:length(ages), collapse=' + '),
                           ' + year'))
    
    fit.age = suppressWarnings(glm(ff, data=df, family='binomial'))
    
    
    
    #-- Unpack the intercepts and slopes into arrays indexed [age,race,sex,risk] --#
    cm$suppression = list(anchor.year=anchor.year,
                          max.proportion=max.suppressed.proportion)
    
    dim.names = list(age=settings$AGES$labels, race=settings$RACES, sex=settings$SEXES, risk=settings$RISK_STRATA)
    cm$suppression$stratified.log.odds.intercept = cm$suppression$stratified.log.odds.slope =
        array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    for (age in 1:length(dim.names[['age']]))
    {
        cm$suppression$stratified.log.odds.intercept[age,,,] = cm$suppression$stratified.log.odds.intercept[age,,,] + 
            fit.age$coefficients[paste0('age',age)]
        cm$suppression$stratified.log.odds.slope[age,,,] = cm$suppression$stratified.log.odds.slope[age,,,] + 
            fit.age$coefficients[paste0('age',age,':year')]
    }
    
    for (race in settings$RACES)
    {
        cm$suppression$stratified.log.odds.intercept[,race,,] = cm$suppression$stratified.log.odds.intercept[,race,,] + 
            fit.race$coefficients[race]
        cm$suppression$stratified.log.odds.slope[,race,,] = cm$suppression$stratified.log.odds.slope[,race,,] + 
            fit.race$coefficients[paste0(race,":year")]
    }
    
    idu.strata = setdiff(settings$RISK_STRATA, 'never_IDU')
    non.idu.strata = 'never_IDU'
    
    cm$suppression$stratified.log.odds.intercept[,,'female',non.idu.strata] = 
        cm$suppression$stratified.log.odds.intercept[,,'female',non.idu.strata] + 
        fit.sex.risk$coefficients['heterosexual_female']
    cm$suppression$stratified.log.odds.slope[,,'female',non.idu.strata] = 
        cm$suppression$stratified.log.odds.slope[,,'female',non.idu.strata] + 
        fit.sex.risk$coefficients['year:heterosexual_female']
    
    cm$suppression$stratified.log.odds.intercept[,,'heterosexual_male',non.idu.strata] = 
        cm$suppression$stratified.log.odds.intercept[,,'heterosexual_male',non.idu.strata] + 
        fit.sex.risk$coefficients['heterosexual_male']
    cm$suppression$stratified.log.odds.slope[,,'heterosexual_male',non.idu.strata] = 
        cm$suppression$stratified.log.odds.slope[,,'heterosexual_male',non.idu.strata] + 
        fit.sex.risk$coefficients['year:heterosexual_male']
    
    cm$suppression$stratified.log.odds.intercept[,,'female',idu.strata] = 
        cm$suppression$stratified.log.odds.intercept[,,'female',idu.strata] + 
        fit.sex.risk$coefficients['idu_female']
    cm$suppression$stratified.log.odds.slope[,,'female',idu.strata] = 
        cm$suppression$stratified.log.odds.slope[,,'female',idu.strata] + 
        fit.sex.risk$coefficients['year:idu_female']
    
    cm$suppression$stratified.log.odds.intercept[,,'heterosexual_male',idu.strata] = 
        cm$suppression$stratified.log.odds.intercept[,,'heterosexual_male',idu.strata] + 
        fit.sex.risk$coefficients['idu_male']
    cm$suppression$stratified.log.odds.slope[,,'heterosexual_male',idu.strata] = 
        cm$suppression$stratified.log.odds.slope[,,'heterosexual_male',idu.strata] + 
        fit.sex.risk$coefficients['year:idu_male']
    
    cm$suppression$stratified.log.odds.intercept[,,'msm',non.idu.strata] = 
        cm$suppression$stratified.log.odds.intercept[,,'msm',non.idu.strata] + 
        fit.sex.risk$coefficients['msm']
    cm$suppression$stratified.log.odds.slope[,,'msm',non.idu.strata] = 
        cm$suppression$stratified.log.odds.slope[,,'msm',non.idu.strata] + 
        fit.sex.risk$coefficients['msm:year']
    
    #-- Return --#
    cm
}

##-- SET UP TESTING --##

run.testing.regressions <- function(cm,
                                    dir='cleaned_data',
                                   verbose=T,
                                   anchor.year=2020,
                                   max.tested.proportion=0.9,
                                   settings=SETTINGS)
{
    #-- STEP 0: Set-Up --#
    #Read in our three main files:
        #NHBS Testing (ever tested and tested in past year, by msa and stratified)
    dfs = read.nhbs.testing(dir=file.path(dir, 'continuum/national/testing'), verbose=verbose)
        #BRFSS at MSA level (total ever tested)
    brfss.msa = read.brfss.msa(file.path(dir, 'continuum/msa/BRFSS_msa.csv'))
        #BRFSS at state level (ever tested by race)
    
    cm$testing = list()
    cm$testing$anchor.year = anchor.year
    cm$testing$max.proportion = max.tested.proportion
    
    #Save the BRFSS files
    cm$testing$total.ever.tested = brfss.msa
    names(cm$testing$total.ever.tested)[names(cm$testing$total.ever.tested)=='msa'] = 'location'
    
    #-- STEP 1: --#
    #Use NHBS data to estimate the relationship between probability of testing in the past year
    # and probability of testing ever (use a constant rate model)
    df.msa = dfs$msa
    fit.12mo.to.ever = lm(log(1-frac.12mo) ~ log(1-frac.ever)+0, data=df.msa)
        #this model assumes a constant rate = -log(1-p)
    cm$testing$rate.ever.to.12mo.mult = fit.12mo.to.ever$coefficients[1]
    
    #-- STEP 2: The Regression by Strata --#
    df.all = dfs$all
    df.all$rate = -log(1-df.all$frac.12mo)
    
    df.all$frac.12mo = pmin(df.all$frac.12mo / max.tested.proportion, .9999)
    df.all$year = df.all$year - anchor.year
    
    #massage our variables
    df.all$black = as.numeric(df.all$race=='black')
    df.all$hispanic = as.numeric(df.all$race=='hispanic')
    df.all$other = as.numeric(df.all$race=='other')
    
    df.all$msm = as.numeric(df.all$risk=='msm')
    df.all$idu = as.numeric(df.all$risk=='idu')
    df.all$heterosexual = as.numeric(df.all$risk=='heterosexual')
    
    df.all$age1 = as.numeric(df.all$age=='age1')
    df.all$age2 = as.numeric(df.all$age=='age2')
    df.all$age3 = as.numeric(df.all$age=='age3')
    df.all$age4 = as.numeric(df.all$age=='age4')
    df.all$age5 = as.numeric(df.all$age=='age5')
    
    df.all$female = as.numeric(df.all$sex=='female')
    df.all$male = as.numeric(df.all$sex=='male' & df.all$risk != 'msm')
    
    #Run the regression - an intercept and slope for each risk factor, 
    # plus ORs and slopes for age/race/sex
    fit.stratified.testing = suppressWarnings(glm(frac.12mo ~ +0 +
                                     msm + idu + heterosexual +
                                     msm:year + idu:year + heterosexual:year +
                                     black + hispanic + other +
                                     black:year + hispanic:year + other:year +
                                     female + male +
                                     female:year + male:year +
                                     age1 + age2 + age3 + age4 + age5 +
                                     age1:year + age2:year + age3:year + age4:year + age5:year,
                                 data=df.all, family='binomial', weight=n))
    
    # Unpack the intercepts and slopes into arrays indexed [age,race,sex,risk]
    dim.names = list(age=settings$AGES$labels, race=settings$RACES, sex=settings$SEXES, risk=settings$RISK_STRATA)
    cm$testing$stratified.log.odds.intercept = cm$testing$stratified.log.odds.slope =
        array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    for (age in 1:length(dim.names[['age']]))
    {
        cm$testing$stratified.log.odds.intercept[age,,,] = cm$testing$stratified.log.odds.intercept[age,,,] + 
            fit.stratified.testing$coefficients[paste0('age',age)]
        cm$testing$stratified.log.odds.slope[age,,,] = cm$testing$stratified.log.odds.slope[age,,,] + 
            fit.stratified.testing$coefficients[paste0('year:age',age)]
    }
        
    for (race in settings$RACES)
    {
        cm$testing$stratified.log.odds.intercept[,race,,] = cm$testing$stratified.log.odds.intercept[,race,,] + 
            fit.stratified.testing$coefficients[race]
        cm$testing$stratified.log.odds.slope[,race,,] = cm$testing$stratified.log.odds.slope[,race,,] + 
            fit.stratified.testing$coefficients[paste0('year:',race)]
    }
    
    cm$testing$stratified.log.odds.intercept[,,'female',] = cm$testing$stratified.log.odds.intercept[,,'female',] + 
        fit.stratified.testing$coefficients['female']
    cm$testing$stratified.log.odds.slope[,,'female',] = cm$testing$stratified.log.odds.slope[,,'female',] + 
        fit.stratified.testing$coefficients['year:female']
    
    cm$testing$stratified.log.odds.intercept[,,'heterosexual_male',] = cm$testing$stratified.log.odds.intercept[,,'heterosexual_male',] + 
        fit.stratified.testing$coefficients['male']
    cm$testing$stratified.log.odds.slope[,,'heterosexual_male',] = cm$testing$stratified.log.odds.slope[,,'heterosexual_male',] + 
        fit.stratified.testing$coefficients['year:male']
    
    non.active.idu = setdiff(settings$RISK_STRATA, 'active_IDU')
    
    cm$testing$stratified.log.odds.intercept[,,'msm',] = cm$testing$stratified.log.odds.intercept[,,'msm',] + 
        fit.stratified.testing$coefficients['msm']
    cm$testing$stratified.log.odds.slope[,,'msm',] = cm$testing$stratified.log.odds.slope[,,'msm',] + 
        fit.stratified.testing$coefficients['msm:year']
    
    #we're going to treat msm+idu as msm
    cm$testing$stratified.log.odds.intercept[,,c('female','heterosexual_male'),'active_IDU'] = 
        cm$testing$stratified.log.odds.intercept[,,c('female','heterosexual_male'),'active_IDU'] + 
        fit.stratified.testing$coefficients['idu']
    cm$testing$stratified.log.odds.slope[,,c('female','heterosexual_male'),'active_IDU'] = 
        cm$testing$stratified.log.odds.slope[,,c('female','heterosexual_male'),'active_IDU'] + 
        fit.stratified.testing$coefficients['idu:year']
    
    cm$testing$stratified.log.odds.intercept[,,c('female','heterosexual_male'),non.active.idu] = 
        cm$testing$stratified.log.odds.intercept[,,c('female','heterosexual_male'),non.active.idu] + 
        fit.stratified.testing$coefficients['heterosexual']
    cm$testing$stratified.log.odds.slope[,,c('female','heterosexual_male'),non.active.idu] = 
        cm$testing$stratified.log.odds.slope[,,c('female','heterosexual_male'),non.active.idu] + 
        fit.stratified.testing$coefficients['heterosexual:year']
    
    
    #-- Return --#
    cm
}

read.brfss.msa <- function(file='cleaned_data/continuum/msa/BRFSS_msa.csv')
{
    df = read.csv(file, stringsAsFactors = F)
    df = df[grepl('hiv', df$Question, ignore.case = T), ]
    df = df[df$Response=='Yes',]
    
    
    msa.n = msa.names(df$LocationID)
    mask = !is.na(msa.n)
    
    rv = data.frame(msa=cbsa.for.msa.name(msa.n[mask]),
               year=df[mask,'ï..Year'],
               frac.ever = df$Data_value[mask]/100,
               sample.size = as.numeric(gsub(',', '', df$Sample_Size[mask])),
               stringsAsFactors=F)
    rv[!is.na(rv$msa),]
}

read.brfss.state <- function(file='cleaned_data/continuum/state/BRFSS_state.csv')
{
    df = read.csv(file, stringsAsFactors = F)
    df = df[grepl('hiv', df$Question, ignore.case = T), ]
    df = df[df$Response=='Yes',]
    
    rv = data.frame(state=df$Locationabbr,
                    year=df['ï..Year'],
                    frac.ever = df$Data_value/100,
                    stratify.by = df$Break_Out_Category,
                    stratification = df$Break_Out,
                    stringsAsFactors=F)
    
    #need to do more here
}

read.nhbs.testing <- function(dir='cleaned_data/continuum/national/testing/',
                             verbose=T)
{
    files = list.files(dir)
    year = as.numeric(substr(files, 1, 4))
    risk = rep('heterosexual', length(files))
    risk[grepl('msm', files, ignore.case = T)] = 'msm'
    risk[grepl('idu', files, ignore.case = T)] = 'idu'
    
    all.dfs = lapply(1:length(files), function(i){
        if (verbose)
            print(paste0("Reading NHBS file on testing for ", year[i]))
        read.nhbs.testing.file(file.path(dir, files[i]), year=year[i], risk=risk[i])
    })
    
    if (verbose)
        print("Joining NHBS testing files")
    
    rv = all.dfs[[1]]
    if (length(all.dfs)>1)
    {
        for (i in 2:length(all.dfs))
        {
            for (name in names(rv))
            {
#                print(i)
#                print(name)
                rv[[name]] = rbind(rv[[name]], all.dfs[[i]][[name]])
            }
        }
    }
    
    rv$all = rbind(rv$race, rv$age, rv$sex)
    
    rv
}

read.nhbs.testing.file <- function(file,
                                   year,
                                   risk)
{
    df = read.csv(file, stringsAsFactors = F)
    
    for (i in 2:dim(df)[2])
    {
        df[,i] = gsub(',', '', df[,i])
        df[!is.na(df[,i]) & df[,i] == '', i] = NA
        df[,i] = as.numeric(df[,i])
    }
    
    N = dim(df)[1]
    header.rows = (1:N)[is.na(df$n.ever) & is.na(df$n.12mo)]
    total.row = (1:N)[grepl('Total', df[,1], ignore.case = T)][1]
    
    #-- Age --#
    age.index = (1:N)[grepl('age', df[,1], ignore.case = T)][1]
    last.age.index = header.rows[header.rows>age.index][1]-1
    
    if ((last.age.index-age.index)==6)
    {
        age1.indices = age.index + 1:2
        age1.mult = c(1,1)
        age2.indices = age.index + 3:4
        age2.mult = c(1,.5)
        age3.indices = age.index + 4:5
        age3.mult = c(.5, .5)
        age4.indices = age.index + 5:6
        age4.mult = c(.5, .25)
        age5.indices = age.index + 6
        age5.mult = .75
    }
    else
    {
        age1.indices = age.index + 1
        age1.mult = 1
        age2.indices = age.index + 2:3
        age2.mult = c(1,.5)
        age3.indices = age.index + 3:4
        age3.mult = c(.5, .5)
        age4.indices = age.index + 4:5
        age4.mult = c(.5, .25)
        age5.indices = age.index + 5
        age5.mult = .75
    }
    
    denominators = c(sum(df$n.total[age1.indices] * age1.mult),
                     sum(df$n.total[age2.indices] * age2.mult),
                     sum(df$n.total[age3.indices] * age3.mult),
                     sum(df$n.total[age4.indices] * age4.mult),
                     sum(df$n.total[age5.indices] * age5.mult))
    numerators.12 = c(sum(df$n.12mo[age1.indices] * age1.mult),
                      sum(df$n.12mo[age2.indices] * age2.mult),
                      sum(df$n.12mo[age3.indices] * age3.mult),
                      sum(df$n.12mo[age4.indices] * age4.mult),
                      sum(df$n.12mo[age5.indices] * age5.mult))
    numerators.ever = c(sum(df$n.ever[age1.indices] * age1.mult),
                        sum(df$n.ever[age2.indices] * age2.mult),
                        sum(df$n.ever[age3.indices] * age3.mult),
                        sum(df$n.ever[age4.indices] * age4.mult),
                        sum(df$n.ever[age5.indices] * age5.mult))
    
    df.age = data.frame(n=denominators,
                        frac.12mo=numerators.12/denominators,
                        frac.ever=numerators.ever/denominators,
                        year=year,
                        age=paste0('age',1:5),
                        race='all',
                        sex='all',
                        risk=risk,
                        msa='all',
                        stringsAsFactors = F)
    
    
    
    #-- Race--#
    first.race.index = (1:N)[grepl('race', df[,1], ignore.case = T)][1]+1
    last.race.index = header.rows[header.rows>first.race.index][1]-1
    
    black.index = (first.race.index:last.race.index)[grepl('black', df[first.race.index:last.race.index,1], ignore.case = T)]
    hispanic.index = (first.race.index:last.race.index)[grepl('hispanic', df[first.race.index:last.race.index,1], ignore.case = T)]
    other.indices = setdiff(first.race.index:last.race.index, c(black.index, hispanic.index))
    
    denominators = c(df$n.total[black.index], df$n.total[hispanic.index], sum(df$n.total[other.indices]))
    numerators.12 = c(df$n.12mo[black.index], df$n.12mo[hispanic.index], sum(df$n.12mo[other.indices]))
    numerators.ever = c(df$n.ever[black.index], df$n.ever[hispanic.index], sum(df$n.ever[other.indices]))
    
    
    df.race = data.frame(n=denominators,
                        frac.12mo=numerators.12/denominators,
                        frac.ever=numerators.ever/denominators,
                        year=year,
                        age='all',
                        race=c('black','hispanic','other'),
                        sex='all',
                        risk=risk,
                        msa='all',
                        stringsAsFactors = F)
    
    #Sex
    if (risk=='msm')
        df.sex = data.frame(n=df$n.total[total.row],
                             frac.12mo=df$n.12mo[total.row]/df$n.total[total.row],
                             frac.ever=df$n.ever[total.row]/df$n.total[total.row],
                             year=year,
                             age='all',
                             race='all',
                             sex='male',
                             risk=risk,
                             msa='all',
                            stringsAsFactors = F)
    else if (any(names(df)=='n.ever.male'))
        df.sex = data.frame(n=c(df$n.total.female[total.row], df$n.total.male[total.row]),
                             frac.12mo = c(df$n.12mo.female[total.row], df$n.12mo.male[total.row])/c(df$n.total.female[total.row], df$n.total.male[total.row]),
                             frac.ever = c(df$n.ever.female[total.row], df$n.ever.male[total.row])/c(df$n.total.female[total.row], df$n.total.male[total.row]),
                             year=year,
                             age='all',
                             race='all',
                            sex=c('female','male'),
                             risk=risk,
                             msa='all',
                            stringsAsFactors = F)
    else
    {
        female.mask = grepl('female', df[,1], ignore.case = T)
        male.mask = grepl('male', df[,1], ignore.case = T) & !female.mask
        
        df.sex = data.frame(n=c(df$n.total[female.mask], df$n.total[male.mask]),
                            frac.12mo=c(df$n.12mo[female.mask], df$n.12mo[male.mask])/c(df$n.total[female.mask], df$n.total[male.mask]),
                            frac.ever=c(df$n.ever[female.mask], df$n.ever[male.mask])/c(df$n.total[female.mask], df$n.total[male.mask]),
                            year=year,
                            age='all',
                            race='all',
                            sex=c('female','male'),
                            risk=risk,
                            msa='all',
                            stringsAsFactors = F)
    }
    
    #Risk
    df.risk = data.frame(n=df$n.total[total.row],
                        frac.12mo=df$n.12mo[total.row]/df$n.total[total.row],
                        frac.ever=df$n.ever[total.row]/df$n.total[total.row],
                        year=year,
                        age='all',
                        race='all',
                        sex='all',
                        risk=risk,
                        msa='all',
                        stringsAsFactors = F)
    
    #MSA
    first.msa.index = (1:N)[grepl('Metropolitan', df[,1], ignore.case = T) | grepl("City", df[,1], ignore.case = T)][1]+1
    last.msa.index = total.row-1
    msas = cbsa.for.msa.name(gsub('â€“', '-', df[first.msa.index:last.msa.index,1]))
    msa.indices = (first.msa.index:last.msa.index)[!is.na(msas)]
    msas = msas[!is.na(msas)]
    
    df.msa = data.frame(n=df$n.total[msa.indices],
                        frac.12mo=df$n.12mo[msa.indices]/df$n.total[msa.indices],
                        frac.ever=df$n.ever[msa.indices]/df$n.total[msa.indices],
                        year=year,
                        age='all',
                        race='all',
                        sex='all',
                        risk=risk,
                        msa=msas,
                        stringsAsFactors = F
                        )
    
    list(race=df.race,
         age=df.age,
         sex=df.sex,
         risk=df.risk,
         msa=df.msa)
}