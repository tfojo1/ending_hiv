
if (1==2)
{
    cm = create.continuum.manager(national.surveillance = national.surveillance,
                                  suppression.anchor.year = 2020,
                                  testing.anchor.year = 2010)
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
    list(intercept = cm$testing$stratified.log.odds.intercept,
         slope = cm$testing$stratified.log.odds.slope,
         anchor.year = cm$testing$anchor.year,
         max.proportion = cm$testing$max.proportion)

#    OLD.get.testing.model(cm, location, population)
}

logit <- function(x){log(x) - log(1-x)}
expit <- function(x){1/(1+exp(-x))}

##---------------------------##
##-- GETTING TESTING RATES --##
##---------------------------##

get.proportion.ever.tested <- function(cm, location)
{
    location.mask = cm$testing$total.ever.tested$location==location
    if (any(location.mask))
    {
        list(ever.tested = cm$testing$total.ever.tested$frac.ever[location.mask],
             sample.size = cm$testing$total.ever.tested$sample.size[location.mask],
             years = cm$testing$total.ever.tested$year[location.mask])
    }
    else
        NULL
}

get.proportion.ever.tested.in.state.msas <- function(cm, location)
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
    
    list(ever.tested=ever.tested,
         sample.size=sample.size,
         years=years)
}

get.proportion.ever.tested.in.states <- function(cm,
                                                 states,
                                                 age=F,
                                                 race=F,
                                                 sex=F)
{
    if (age)
    {
        if (length(states)==1)
            rv = list(numerators = cm$state.testing$numerators.age[,states,],
                 denominators = cm$state.testing$denominators.age[,states,])
        else
            rv = list(numerators = apply(cm$state.testing$numerators.age[,states,], c('year','age'), sum, na.rm=T),
                 denominators = apply(cm$state.testing$denominators.age[,states,], c('year','age'), sum, na.rm=T))
    }
    else if (race)
    {
        if (length(states)==1)
            rv = list(numerators = cm$state.testing$numerators.race[,states,],
                 denominators = cm$state.testing$denominators.race[,states,])
        else
            rv = list(numerators = apply(cm$state.testing$numerators.race[,states,], c('year','race'), sum, na.rm=T),
                 denominators = apply(cm$state.testing$denominators.race[,states,], c('year','race'), sum, na.rm=T))
    }
    else if (sex)
    {
        if (length(states)==1)
            rv = list(numerators = cm$state.testing$numerators.sex[,states,],
                      denominators = cm$state.testing$denominators.sex[,states,])
        else
            rv = list(numerators = apply(cm$state.testing$numerators.sex[,states,], c('year','sex'), sum, na.rm=T),
                      denominators = apply(cm$state.testing$denominators.sex[,states,], c('year','sex'), sum, na.rm=T))
    }
    else
        stop("One of the arguments 'age', 'sex', or 'race' must be set to TRUE")
    
    rv$years = as.numeric(dimnames(rv$numerators)[['year']])
    rv
}

##------------------------------------##
##-- CREATE A NEW CONTINUUM MANAGER --##
##------------------------------------##

create.continuum.manager <- function(dir='cleaned_data/', 
                                     settings = SETTINGS,
                                     national.surveillance = national.surveillance,
                                     suppression.anchor.year = 2020,
                                     testing.anchor.year = 2010,
                                     linkage.anchor.year = 2020,
                                     max.tested.proportion = 0.9,
                                     max.suppressed.proportion = 0.9,
                                     max.linked.proportion = 0.95,
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
    cm$state.testing = read.brfss.state(file = file.path(dir, 'continuum/state/BRFSS_state.csv'))
    
    
    if (verbose)
        print('Reading Linkage')
    cm = run.linkage.regressions(cm,
                                 dir=dir,
                                 anchor.year = linkage.anchor.year,
                                 max.linked.proportion = max.linked.proportion,
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
    
    # Save the log ORs for readability/reference (NOT used by model)
    cm$testing$log.ors = numeric()
    
    dim.names = list(age=settings$AGES$labels, race=settings$RACES, sex=settings$SEXES, risk=settings$RISK_STRATA)
    cm$suppression$stratified.log.odds.intercept = cm$suppression$stratified.log.odds.slope =
        array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    n.ages = length(dim.names[['age']])
    for (age in 1:n.ages)
    {
        cm$suppression$stratified.log.odds.intercept[age,,,] = cm$suppression$stratified.log.odds.intercept[age,,,] + 
            fit.age$coefficients[paste0('age',age)]
        cm$suppression$stratified.log.odds.slope[age,,,] = cm$suppression$stratified.log.odds.slope[age,,,] + 
            fit.age$coefficients[paste0('age',age,':year')]
    }
    cm$suppression$log.ors[paste0('age',1:n.ages)] = fit.age$coefficients[paste0('age',1:n.ages)]
    cm$suppression$log.ors[paste0('age',1:n.ages,"_slope")] = fit.age$coefficients[paste0('age',1:n.ages,":year")]
    
    for (race in settings$RACES)
    {
        cm$suppression$stratified.log.odds.intercept[,race,,] = cm$suppression$stratified.log.odds.intercept[,race,,] + 
            fit.race$coefficients[race]
        cm$suppression$stratified.log.odds.slope[,race,,] = cm$suppression$stratified.log.odds.slope[,race,,] + 
            fit.race$coefficients[paste0(race,":year")]
    }
    cm$suppression$log.ors[settings$RACES] = fit.race$coefficients[settings$RACES]
    cm$suppression$log.ors[paste0(settings$RACES,"_slope")] = fit.race$coefficients[paste0(settings$RACES,":year")]
    
    idu.strata = setdiff(settings$RISK_STRATA, 'never_IDU')
    non.idu.strata = 'never_IDU'
    
    
    # Non-IDU, heterosexual female
    cm$suppression$stratified.log.odds.intercept[,,'female',non.idu.strata] = 
        cm$suppression$stratified.log.odds.intercept[,,'female',non.idu.strata] + 
        fit.sex.risk$coefficients['heterosexual_female']
    cm$suppression$stratified.log.odds.slope[,,'female',non.idu.strata] = 
        cm$suppression$stratified.log.odds.slope[,,'female',non.idu.strata] + 
        fit.sex.risk$coefficients['year:heterosexual_female']
    cm$suppression$log.ors['heterosexual_female'] = fit.sex.risk$coefficients['heterosexual_female']
    cm$suppression$log.ors['heterosexual_female_slope'] = fit.sex.risk$coefficients['year:heterosexual_female']
    
    # Non-IDU, heterosexual male
    cm$suppression$stratified.log.odds.intercept[,,'heterosexual_male',non.idu.strata] = 
        cm$suppression$stratified.log.odds.intercept[,,'heterosexual_male',non.idu.strata] + 
        fit.sex.risk$coefficients['heterosexual_male']
    cm$suppression$stratified.log.odds.slope[,,'heterosexual_male',non.idu.strata] = 
        cm$suppression$stratified.log.odds.slope[,,'heterosexual_male',non.idu.strata] + 
        fit.sex.risk$coefficients['year:heterosexual_male']
    cm$suppression$log.ors['heterosexual_male'] = fit.sex.risk$coefficients['heterosexual_male']
    cm$suppression$log.ors['heterosexual_male_slope'] = fit.sex.risk$coefficients['year:heterosexual_male']
    
    # IDU female
    cm$suppression$stratified.log.odds.intercept[,,'female',idu.strata] = 
        cm$suppression$stratified.log.odds.intercept[,,'female',idu.strata] + 
        fit.sex.risk$coefficients['idu_female']
    cm$suppression$stratified.log.odds.slope[,,'female',idu.strata] = 
        cm$suppression$stratified.log.odds.slope[,,'female',idu.strata] + 
        fit.sex.risk$coefficients['year:idu_female']
    cm$suppression$log.ors['idu_female'] = fit.sex.risk$coefficients['idu_female']
    cm$suppression$log.ors['idu_female_slope'] = fit.sex.risk$coefficients['year:idu_female']
    
    # IDU, heterosexual male
    cm$suppression$stratified.log.odds.intercept[,,'heterosexual_male',idu.strata] = 
        cm$suppression$stratified.log.odds.intercept[,,'heterosexual_male',idu.strata] + 
        fit.sex.risk$coefficients['idu_male']
    cm$suppression$stratified.log.odds.slope[,,'heterosexual_male',idu.strata] = 
        cm$suppression$stratified.log.odds.slope[,,'heterosexual_male',idu.strata] + 
        fit.sex.risk$coefficients['year:idu_male']
    cm$suppression$log.ors['idu_male'] = fit.sex.risk$coefficients['idu_male']
    cm$suppression$log.ors['idu_male_slope'] = fit.sex.risk$coefficients['year:idu_male']
    
    # Non-IDU MSM
    cm$suppression$stratified.log.odds.intercept[,,'msm',non.idu.strata] = 
        cm$suppression$stratified.log.odds.intercept[,,'msm',non.idu.strata] + 
        fit.sex.risk$coefficients['msm']
    cm$suppression$stratified.log.odds.slope[,,'msm',non.idu.strata] = 
        cm$suppression$stratified.log.odds.slope[,,'msm',non.idu.strata] + 
        fit.sex.risk$coefficients['msm:year']
    cm$suppression$log.ors['msm'] = fit.sex.risk$coefficients['msm']
    cm$suppression$log.ors['msm_slope'] = fit.sex.risk$coefficients['msm:year']
    
    # IDU MSM
    cm$suppression$stratified.log.odds.intercept[,,'msm',idu.strata] = 
        cm$suppression$stratified.log.odds.intercept[,,'msm',idu.strata] + 
        fit.sex.risk$coefficients['msm_idu']
    cm$suppression$stratified.log.odds.slope[,,'msm',idu.strata] = 
        cm$suppression$stratified.log.odds.slope[,,'msm',idu.strata] + 
        fit.sex.risk$coefficients['year:msm_idu']
    cm$suppression$log.ors['msm_idu'] = fit.sex.risk$coefficients['msm_idu']
    cm$suppression$log.ors['msm_idu_slope'] = fit.sex.risk$coefficients['year:msm_idu']
    
    
    #-- Return --#
    cm
}

##-- SET UP LINKAGE --##

run.linkage.regressions <- function(cm,
                                    dir='cleaned_data',
                                    anchor.year=2020,
                                    max.linked.proportion=0.95,
                                    settings=SETTINGS
                                    )
{
    df = read.csv(file.path(dir, 'continuum/national/linkage/national_linkage.csv'),
                  stringsAsFactors = F, header = F)
    dimnames(df)[[1]] = df[,1]
    
    n.mask = df[2,]=='new'
    p.mask = df[2,]=='linked'
    
    
    #-- Fit age --#
    
    df.age = rbind(data.frame(value=as.numeric(df['13-24',p.mask]),
                              n=as.numeric(df['13-24',n.mask]),
                              year=as.numeric(df['year',p.mask])-anchor.year,
                              age1=1,
                              age2=0,
                              age3=0,
                              age4=0,
                              age5=0,
                              total=0),
                   data.frame(value=as.numeric(df['25-34',p.mask]),
                              n=as.numeric(df['25-34',n.mask]),
                              year=as.numeric(df['year',p.mask])-anchor.year,
                              age1=0,
                              age2=1,
                              age3=0,
                              age4=0,
                              age5=0,
                              total=0),
                   data.frame(value=as.numeric(df['35-44',p.mask]),
                              n=as.numeric(df['35-44',n.mask]),
                              year=as.numeric(df['year',p.mask])-anchor.year,
                              age1=0,
                              age2=0,
                              age3=1,
                              age4=0,
                              age5=0,
                              total=0),
                   data.frame(value=as.numeric(df['45-54',p.mask]),
                              n=as.numeric(df['45-54',n.mask]),
                              year=as.numeric(df['year',p.mask])-anchor.year,
                              age1=0,
                              age2=0,
                              age3=0,
                              age4=1,
                              age5=0,
                              total=0),
                   data.frame(value=as.numeric(df['>=55',p.mask]),
                              n=as.numeric(df['>=55',n.mask]),
                              year=as.numeric(df['year',p.mask])-anchor.year,
                              age1=0,
                              age2=0,
                              age3=0,
                              age4=0,
                              age5=1,
                              total=0),
                   data.frame(value=as.numeric(df['total',p.mask]),
                              n=as.numeric(df['total',n.mask]),
                              year=as.numeric(df['year',p.mask])-anchor.year,
                              age1=0,
                              age2=0,
                              age3=0,
                              age4=0,
                              age5=0,
                              total=1))
    
    df.age$value = pmin(0.999, df.age$value/max.linked.proportion)
    
    fit.age = glm(value ~ age1 + age2 + age3 + age4 + age5 +
                      year + year:age1 + year:age2 + year:age3 + year:age4 + year:age5, 
                  data=df.age, weights = df.age$n,
                  family='binomial')
    
    
    #-- Fit Race --#
    
    df.race = rbind(data.frame(value=as.numeric(df['black',p.mask]),
                              n=as.numeric(df['black',n.mask]),
                              year=as.numeric(df['year',p.mask])-anchor.year,
                              black=1,
                              hispanic=0,
                              other=0,
                              total=0),
                    data.frame(value=as.numeric(df['hispanic',p.mask]),
                               n=as.numeric(df['hispanic',n.mask]),
                               year=as.numeric(df['year',p.mask])-anchor.year,
                               black=0,
                               hispanic=1,
                               other=0,
                               total=0),
                    data.frame(value=as.numeric(df['other',p.mask]),
                               n=as.numeric(df['other',n.mask]),
                               year=as.numeric(df['year',p.mask])-anchor.year,
                               black=0,
                               hispanic=0,
                               other=1,
                               total=0),
                    data.frame(value=as.numeric(df['total',p.mask]),
                               n=as.numeric(df['total',n.mask]),
                               year=as.numeric(df['year',p.mask])-anchor.year,
                               black=0,
                               hispanic=0,
                               other=0,
                               total=1))
    
    df.race$value = pmin(0.999, df.race$value/max.linked.proportion)
    
    fit.race = glm(value ~ black + hispanic + other + year + year:black + year:hispanic + year:other,
                   data=df.race, weights = df.race$n,
                   family='binomial')
    
    
    #-- Fit Sex/Risk Factor --#
    
    df.sex.risk = rbind(data.frame(value=as.numeric(df['msm',p.mask]),
                               n=as.numeric(df['msm',n.mask]),
                               year=as.numeric(df['year',p.mask])-anchor.year,
                               msm=1,
                               msm_idu=0,
                               het_male=0,
                               het_female=0,
                               idu_male=0,
                               idu_female=0),
                    data.frame(value=as.numeric(df['msm-idu',p.mask]),
                               n=as.numeric(df['msm-idu',n.mask]),
                               year=as.numeric(df['year',p.mask])-anchor.year,
                               msm=0,
                               msm_idu=1,
                               het_male=0,
                               het_female=0,
                               idu_male=0,
                               idu_female=0),
                    data.frame(value=as.numeric(df['het-male',p.mask]),
                               n=as.numeric(df['het-male',n.mask]),
                               year=as.numeric(df['year',p.mask])-anchor.year,
                               msm=0,
                               msm_idu=0,
                               het_male=1,
                               het_female=0,
                               idu_male=0,
                               idu_female=0),
                    data.frame(value=as.numeric(df['het-female',p.mask]),
                               n=as.numeric(df['het-female',n.mask]),
                               year=as.numeric(df['year',p.mask])-anchor.year,
                               msm=0,
                               msm_idu=0,
                               het_male=0,
                               het_female=1,
                               idu_male=0,
                               idu_female=0),
                    data.frame(value=as.numeric(df['idu-male',p.mask]),
                               n=as.numeric(df['idu-male',n.mask]),
                               year=as.numeric(df['year',p.mask])-anchor.year,
                               msm=0,
                               msm_idu=0,
                               het_male=0,
                               het_female=0,
                               idu_male=1,
                               idu_female=0),
                    data.frame(value=as.numeric(df['idu-female',p.mask]),
                               n=as.numeric(df['idu-female',n.mask]),
                               year=as.numeric(df['year',p.mask])-anchor.year,
                               msm=0,
                               msm_idu=0,
                               het_male=0,
                               het_female=0,
                               idu_male=0,
                               idu_female=1))
    
    df.sex.risk$value = pmin(0.999, df.sex.risk$value/max.linked.proportion)
    
    fit.sex.risk = glm(value ~ msm + msm_idu + het_male + het_female + idu_male + idu_female + 
                          year:msm + year:msm_idu + year:het_male + year:het_female + year:idu_male + year:idu_female +0,
                       data=df.sex.risk, weights = df.sex.risk$n,
                       family='binomial')
    
    
    #-- Put it all together --#
    
    cm$linkage = list(anchor.year = anchor.year,
                      max.proportion = max.linked.proportion,
                      log.ors = numeric())
    
    dim.names = list(age=settings$AGES$labels, race=settings$RACES, sex=settings$SEXES, risks=settings$RISK_STRATA)
    cm$linkage$stratified.log.odds.slope = cm$linkage$stratified.log.odds.intercept =
        array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    # Age
    for (age in 1:5)
    {
        cm$linkage$stratified.log.odds.intercept[age,,,] = cm$linkage$stratified.log.odds.intercept[age,,,] +
            fit.age$coefficients[paste0('age',age)]
        cm$linkage$stratified.log.odds.slope[age,,,] = cm$linkage$stratified.log.odds.slope[age,,,] +
            fit.age$coefficients[paste0('age',age,':year')]
        
        cm$linkage$log.ors[paste0('age',age)] = fit.age$coefficients[paste0('age',age)]
        cm$linkage$log.ors[paste0('age',age,"_slope")] = fit.age$coefficients[paste0('age',age,':year')]
    }
    
    # Race
    cm$linkage$stratified.log.odds.intercept[,'black',,] = cm$linkage$stratified.log.odds.intercept[,'black',,] +
        fit.race$coefficients['black']
    cm$linkage$stratified.log.odds.slope[,'black',,] = cm$linkage$stratified.log.odds.slope[,'black',,] +
        fit.race$coefficients['black:year']
    cm$linkage$log.ors['black'] = fit.race$coefficients['black']
    cm$linkage$log.ors['black_slope'] = fit.race$coefficients['black:year']
    
    cm$linkage$stratified.log.odds.intercept[,'hispanic',,] = cm$linkage$stratified.log.odds.intercept[,'hispanic',,] +
        fit.race$coefficients['hispanic']
    cm$linkage$stratified.log.odds.slope[,'hispanic',,] = cm$linkage$stratified.log.odds.slope[,'hispanic',,] +
        fit.race$coefficients['hispanic:year']
    cm$linkage$log.ors['hispanic'] = fit.race$coefficients['hispanic']
    cm$linkage$log.ors['hispanic_slope'] = fit.race$coefficients['hispanic:year']
    
    cm$linkage$stratified.log.odds.intercept[,'other',,] = cm$linkage$stratified.log.odds.intercept[,'other',,] +
        fit.race$coefficients['other']
    cm$linkage$stratified.log.odds.slope[,'other',,] = cm$linkage$stratified.log.odds.slope[,'other',,] +
        fit.race$coefficients['other:year']
    cm$linkage$log.ors['other'] = fit.race$coefficients['other']
    cm$linkage$log.ors['other_slope'] = fit.race$coefficients['other:year']
    
    
    # Sex Risk
    idu = c('active_IDU','IDU_in_remission')
    non.idu = 'never_IDU'
    
    cm$linkage$stratified.log.odds.intercept[,,'msm',non.idu] = cm$linkage$stratified.log.odds.intercept[,,'msm',non.idu] +
        fit.sex.risk$coefficients['msm']
    cm$linkage$stratified.log.odds.slope[,,'msm',non.idu] = cm$linkage$stratified.log.odds.slope[,,'msm',non.idu] +
        fit.sex.risk$coefficients['msm:year']
    cm$linkage$log.ors['msm'] = fit.sex.risk$coefficients['msm']
    cm$linkage$log.ors['msm_slope'] = fit.sex.risk$coefficients['msm:year']
    
    cm$linkage$stratified.log.odds.intercept[,,'msm',idu] = cm$linkage$stratified.log.odds.intercept[,,'msm',idu] +
        fit.sex.risk$coefficients['msm_idu']
    cm$linkage$stratified.log.odds.slope[,,'msm',idu] = cm$linkage$stratified.log.odds.slope[,,'msm',idu] +
        fit.sex.risk$coefficients['msm_idu:year']
    cm$linkage$log.ors['msm_idu'] = fit.sex.risk$coefficients['msm_idu']
    cm$linkage$log.ors['msm_idu_slope'] = fit.sex.risk$coefficients['msm_idu:year']
    
    
    cm$linkage$stratified.log.odds.intercept[,,'heterosexual_male',non.idu] = cm$linkage$stratified.log.odds.intercept[,,'heterosexual_male',non.idu] +
        fit.sex.risk$coefficients['het_male']
    cm$linkage$stratified.log.odds.slope[,,'heterosexual_male',non.idu] = cm$linkage$stratified.log.odds.slope[,,'heterosexual_male',non.idu] +
        fit.sex.risk$coefficients['het_male:year']
    cm$linkage$log.ors['het_male'] = fit.sex.risk$coefficients['het_male']
    cm$linkage$log.ors['het_male_slope'] = fit.sex.risk$coefficients['het_male:year']
    
    cm$linkage$stratified.log.odds.intercept[,,'heterosexual_male',idu] = cm$linkage$stratified.log.odds.intercept[,,'heterosexual_male',idu] +
        fit.sex.risk$coefficients['idu_male']
    cm$linkage$stratified.log.odds.slope[,,'heterosexual_male',idu] = cm$linkage$stratified.log.odds.slope[,,'heterosexual_male',idu] +
        fit.sex.risk$coefficients['idu_male:year']
    cm$linkage$log.ors['idu_male'] = fit.sex.risk$coefficients['idu_male']
    cm$linkage$log.ors['idu_male_slope'] = fit.sex.risk$coefficients['idu_male:year']
    
    
    cm$linkage$stratified.log.odds.intercept[,,'female',non.idu] = cm$linkage$stratified.log.odds.intercept[,,'female',non.idu] +
        fit.sex.risk$coefficients['het_female']
    cm$linkage$stratified.log.odds.slope[,,'female',non.idu] = cm$linkage$stratified.log.odds.slope[,,'female',non.idu] +
        fit.sex.risk$coefficients['het_female:year']
    cm$linkage$log.ors['het_female'] = fit.sex.risk$coefficients['het_female']
    cm$linkage$log.ors['het_female_slope'] = fit.sex.risk$coefficients['het_female:year']
    
    cm$linkage$stratified.log.odds.intercept[,,'female',idu] = cm$linkage$stratified.log.odds.intercept[,,'female',idu] +
        fit.sex.risk$coefficients['idu_female']
    cm$linkage$stratified.log.odds.slope[,,'female',idu] = cm$linkage$stratified.log.odds.slope[,,'female',idu] +
        fit.sex.risk$coefficients['idu_female:year']
    cm$linkage$log.ors['idu_female'] = fit.sex.risk$coefficients['idu_female']
    cm$linkage$log.ors['idu_female_slope'] = fit.sex.risk$coefficients['idu_female:year']
    
    
    #-- Return it --#
    cm
}

##-- SET UP DISENGAGEMENT --##

setup.retention <- function(cm,
                                anchor.year=2010,
                                max.retained.proportion=0.95,
                                settings=SETTINGS)
{
    #-- Age --#
    
    or.age.yehia = c(age1 = 1,
                     age2 = mean(c(1,1.17)),
                     age3 = mean(c(1.17,1.41)),
                     age4 = mean(c(1.41,1.83)),
                     age5 = 1.83)
    or.age.yehia = or.age.yehia / or.age.yehia[3]
    or.age.rebeiro = 0.61 ^ (2:-2)
    or.age = (or.age.rebeiro+or.age.yehia)/2
    
    or.age.slope = (.8^(2:-2) / .78^(2:-2)) ^ (1/5)
    print("NEED TO FACTOR IN TOTAL SLOPE TO AGE SLOPES")
    
    or.age.slope = c(1,1,1,1,1)
    
    
    #-- Race --#
    
    or.black = mean(c(0.91,1/1.17))
    or.hispanic = 1.12
    or.other = 1
    
    or.slope.black = 1
    or.slope.hispanic = 1
    or.slope.other = 1
    
    
    #-- Sex/Risk --#
    
    or.msm = 1
    or.msm.idu = 0.86
    or.het.male = 0.84 * 0.86
    or.het.female = 0.84
    or.idu.male = mean(c(0.73, 1/1.33)) * .86
    or.idu.female = mean(c(0.73, 1/1.33))
    
    stratified.log.odds
    total.intercept
    
    #such that
    p = expit(total.intercept + stratified.log.odds)
    
    #find total.intercept such that
    # sum(p*n)/n ~= .7465
    
    
    
    cm$retention = list()
    
    dim.names = list(age=settings$AGES$labels, race=settings$RACES, sex=settings$SEXES, risk=settings$RISK_STRATA)
    cm$retention$stratified.log.odds.intercept = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    cm$retention$stratified.log.odds.slope = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
}

##-- SET UP TESTING --##

run.testing.regressions <- function(cm,
                                    dir='cleaned_data',
                                    verbose=T,
                                    anchor.year=2020,
                                    max.tested.proportion=0.9,
                                    settings=SETTINGS)
{
    # We our working towards a model such that
    # logit(p_12mo_s) = logit(p_12mo_prime_s) + mu
    # for some stratum s
    # where p_12mo represents best-guess prob testing (average of brfss and nhbs)
    #  and p_12mo_prime represents the (overly optimistic) prob testing by nhbs
    # we let logit(p_12mo_prime_s) = X beta
    
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
    #we are now using the reverse regression (ever ~ 12mo) but this is here for backward compatibility
    cm$testing$rate.ever.to.12mo.mult = fit.12mo.to.ever$coefficients[1]
    cm$testing$sd.rate.ever.to.12mo.mult = sqrt(vcov(fit.12mo.to.ever)[1,1])
    
    fit.ever.to.12mo = lm(log(1-frac.ever) ~ log(1-frac.12mo)+0, data=df.msa)
    cm$testing$ever.to.12mo.model = list(mean=fit.ever.to.12mo$coefficients[1],
                                         sd=sqrt(vcov(fit.ever.to.12mo)[1,1]))
    
    #-- STEP 2: Link NHBS and BRFSS to correct the overly optimistic NHBS estimates --#
    # This gives up the mu for our overriding equation
    
    df.nhbs.vs.brfss = data.frame(msa=df.msa$msa,
                                  nhbs.ever=df.msa$frac.ever,
                                  nhbs.12mo=df.msa$frac.12mo,
                                  year=df.msa$year,
                                  brfss.ever=sapply(1:dim(df.msa)[1], function(i){
                                      mask = brfss.msa$msa==df.msa$msa[i] & brfss.msa$year==df.msa$year[i]
                                      brfss.msa$frac.ever[i]
                                  }))
    
    logit = function(p){log(p)-log(1-p)}
    df.nhbs.vs.brfss$est.nhbs.12mo = 1 - (1-df.nhbs.vs.brfss$nhbs.ever) ^ cm$testing$rate.ever.to.12mo.mult
    df.nhbs.vs.brfss$est.brfss.12mo = 1 - (1-df.nhbs.vs.brfss$brfss.ever) ^ cm$testing$rate.ever.to.12mo.mult
    
    df.nhbs.vs.brfss$mean.ever = (df.nhbs.vs.brfss$brfss.ever + df.nhbs.vs.brfss$nhbs.ever) / 2
    df.nhbs.vs.brfss$est.mean.12mo = 1 - (1-df.nhbs.vs.brfss$mean.ever) ^ cm$testing$rate.ever.to.12mo.mult
    
    base.log.or = mean(logit(df.nhbs.vs.brfss$est.mean.12mo) - logit(df.nhbs.vs.brfss$est.nhbs.12mo))
    #    base.log.or = 0
    
    #-- STEP 3: The Regression by Strata --#
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
    
    #-- STEP 4: PUT IT ALL TOGETHER --#
    
    # Save the log ORs for readability/reference (NOT used by model)
    cm$testing$log.ors = numeric()
    
    # Unpack the intercepts and slopes into arrays indexed [age,race,sex,risk]
    dim.names = list(age=settings$AGES$labels, race=settings$RACES, sex=settings$SEXES, risk=settings$RISK_STRATA)
    cm$testing$stratified.log.odds.intercept = array(base.log.or, dim=sapply(dim.names, length), dimnames=dim.names)
    cm$testing$stratified.log.odds.slope = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    n.ages = length(dim.names[['age']])
    for (age in 1:n.ages)
    {
        cm$testing$stratified.log.odds.intercept[age,,,] = cm$testing$stratified.log.odds.intercept[age,,,] + 
            fit.stratified.testing$coefficients[paste0('age',age)]
        cm$testing$stratified.log.odds.slope[age,,,] = cm$testing$stratified.log.odds.slope[age,,,] + 
            fit.stratified.testing$coefficients[paste0('year:age',age)]
    }
    cm$testing$log.ors[paste0('age',1:n.ages)] = fit.stratified.testing$coefficients[paste0('age',1:n.ages)]
    cm$testing$log.ors[paste0('age',1:n.ages,'_slope')] = fit.stratified.testing$coefficients[paste0('year:age',1:n.ages)]
    
    
    for (race in settings$RACES)
    {
        cm$testing$stratified.log.odds.intercept[,race,,] = cm$testing$stratified.log.odds.intercept[,race,,] + 
            fit.stratified.testing$coefficients[race]
        cm$testing$stratified.log.odds.slope[,race,,] = cm$testing$stratified.log.odds.slope[,race,,] + 
            fit.stratified.testing$coefficients[paste0('year:',race)]
    }
    cm$testing$log.ors[paste0(settings$RACES)] = fit.stratified.testing$coefficients[settings$RACES]
    cm$testing$log.ors[paste0(settings$RACES,'_slope')] = fit.stratified.testing$coefficients[paste0('year:',settings$RACES)]
    
    
    cm$testing$stratified.log.odds.intercept[,,'female',] = cm$testing$stratified.log.odds.intercept[,,'female',] + 
        fit.stratified.testing$coefficients['female']
    cm$testing$stratified.log.odds.slope[,,'female',] = cm$testing$stratified.log.odds.slope[,,'female',] + 
        fit.stratified.testing$coefficients['year:female']
    cm$testing$log.ors['female'] = fit.stratified.testing$coefficients['female']
    cm$testing$log.ors['female_slope'] = fit.stratified.testing$coefficients['year:female']
    
    
    cm$testing$stratified.log.odds.intercept[,,'heterosexual_male',] = cm$testing$stratified.log.odds.intercept[,,'heterosexual_male',] + 
        fit.stratified.testing$coefficients['male']
    cm$testing$stratified.log.odds.slope[,,'heterosexual_male',] = cm$testing$stratified.log.odds.slope[,,'heterosexual_male',] + 
        fit.stratified.testing$coefficients['year:male']
    cm$testing$log.ors['heterosexual_male'] = fit.stratified.testing$coefficients['male']
    cm$testing$log.ors['heterosexual_male_slope'] = fit.stratified.testing$coefficients['year:male']
    
    non.active.idu = setdiff(settings$RISK_STRATA, 'active_IDU')
    
    cm$testing$stratified.log.odds.intercept[,,'msm',] = cm$testing$stratified.log.odds.intercept[,,'msm',] + 
        fit.stratified.testing$coefficients['msm']
    cm$testing$stratified.log.odds.slope[,,'msm',] = cm$testing$stratified.log.odds.slope[,,'msm',] + 
        fit.stratified.testing$coefficients['msm:year']
    cm$testing$log.ors['msm'] = fit.stratified.testing$coefficients['msm']
    cm$testing$log.ors['msm_slope'] = fit.stratified.testing$coefficients['msm:year']
    
    #we're going to treat msm+idu as msm (by omitting them from the code below adding idu log ORs)
    cm$testing$stratified.log.odds.intercept[,,c('female','heterosexual_male'),'active_IDU'] = 
        cm$testing$stratified.log.odds.intercept[,,c('female','heterosexual_male'),'active_IDU'] + 
        fit.stratified.testing$coefficients['idu']
    cm$testing$stratified.log.odds.slope[,,c('female','heterosexual_male'),'active_IDU'] = 
        cm$testing$stratified.log.odds.slope[,,c('female','heterosexual_male'),'active_IDU'] + 
        fit.stratified.testing$coefficients['idu:year']
    cm$testing$log.ors['idu'] = fit.stratified.testing$coefficients['idu']
    cm$testing$log.ors['idu_slope'] = fit.stratified.testing$coefficients['idu:year']
    
    cm$testing$stratified.log.odds.intercept[,,c('female','heterosexual_male'),non.active.idu] = 
        cm$testing$stratified.log.odds.intercept[,,c('female','heterosexual_male'),non.active.idu] + 
        fit.stratified.testing$coefficients['heterosexual']
    cm$testing$stratified.log.odds.slope[,,c('female','heterosexual_male'),non.active.idu] = 
        cm$testing$stratified.log.odds.slope[,,c('female','heterosexual_male'),non.active.idu] + 
        fit.stratified.testing$coefficients['heterosexual:year']
    cm$testing$log.ors['heterosexual'] = fit.stratified.testing$coefficients['heterosexual']
    cm$testing$log.ors['heterosexual_slope'] = fit.stratified.testing$coefficients['heterosexual:year']
    
    cm$testing$log.ors['base'] = base.log.or
    
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
    print("Reading BRFSS State-Level Data...")
    
    df = read.csv(file, stringsAsFactors = F)
    df = df[grepl('hiv', df$Question, ignore.case = T), ]
    df = df[df$Response=='Yes',]
    df = df[!is.na(df$Data_value),]
    df$Break_Out = tolower(df$Break_Out)
    df$year = as.character(df[,'ï..Year'])
    df$Sample_Size = as.numeric(gsub(',','',df$Sample_Size))
    df$frac = as.numeric(df$Data_value)/100
    
    
    df.race = df[grepl('race', df$Break_Out_Category, ignore.case = T),]
    df.sex = df[grepl('gender', df$Break_Out_Category, ignore.case = T),]
    df.age = df[grepl('age', df$Break_Out_Category, ignore.case = T),]
    
    dim.names.race = list(year=sort(unique(df$year)),
                          location=sort(unique(df$Locationabbr)),
                          race=c('black','hispanic','other'))
    dim.names.age = c(dim.names.race[1:2],
                      list(age=c('18-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years')))
    dim.names.sex = c(dim.names.race[1:2],
                      list(sex=c('male','female')))
    
    rv = list()
    rv$numerators.race = rv$denominators.race = array(0, dim=sapply(dim.names.race, length), dimnames=dim.names.race)
    rv$numerators.sex = rv$denominators.sex = array(0, dim=sapply(dim.names.sex, length), dimnames=dim.names.sex)
    rv$numerators.age = rv$denominators.age = array(0, dim=sapply(dim.names.age, length), dimnames=dim.names.age)
    
    
    print("Parsing BRFSS State-Level Data...")
    
    
    for (i in 1:dim(df.race)[1])
    {
        if (df.race$Break_Out[i]=='hispanic')
            race = 'hispanic'
        else if (df.race$Break_Out[i]=='black, non-hispanic')
            race = 'black'
        else
            race = 'other'
        
        rv$numerators.race[df.race$year[i], df.race$Locationabbr[i], race] = 
            rv$numerators.race[df.race$year[i], df.race$Locationabbr[i], race] + 
            df.race$Sample_Size[i] * df.race$frac[i]
        
        rv$denominators.race[df.race$year[i], df.race$Locationabbr[i], race] = 
            rv$denominators.race[df.race$year[i], df.race$Locationabbr[i], race] + 
            df.race$Sample_Size[i]
    }
    missing.mask = rv$denominators.race == 0
    rv$denominators.race[missing.mask] = rv$numerators.race[missing.mask] = NA
    
    age.mapping = c('18-24'=1,
                    '25-34'=2,
                    '35-44'=3,
                    '45-54'=4,
                    '55-64'=5,
                    '65+'=5)
    for (i in 1:dim(df.age)[1])
    {
        age = age.mapping[df.age$Break_Out[i]]
        
        rv$numerators.age[df.age$year[i], df.age$Locationabbr[i], age] = 
            rv$numerators.age[df.age$year[i], df.age$Locationabbr[i], age] + 
            df.age$Sample_Size[i] * df.age$frac[i]
        
        rv$denominators.age[df.age$year[i], df.age$Locationabbr[i], age] = 
            rv$denominators.age[df.age$year[i], df.age$Locationabbr[i], age] + 
            df.age$Sample_Size[i]
    }
    missing.mask = rv$denominators.age == 0
    rv$denominators.age[missing.mask] = rv$numerators.age[missing.mask] = NA
    
    for (i in 1:dim(df.sex)[1])
    {
        sex = df.sex$Break_Out[i]
        
        rv$numerators.sex[df.sex$year[i], df.sex$Locationabbr[i], sex] = 
            rv$numerators.sex[df.sex$year[i], df.sex$Locationabbr[i], sex] + 
            df.sex$Sample_Size[i] * df.sex$frac[i]
        
        rv$denominators.sex[df.sex$year[i], df.sex$Locationabbr[i], sex] = 
            rv$denominators.sex[df.sex$year[i], df.sex$Locationabbr[i], sex] + 
            df.sex$Sample_Size[i]
    }
    missing.mask = rv$denominators.sex == 0
    rv$denominators.sex[missing.mask] = rv$numerators.sex[missing.mask] = NA
    
    
    rv
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

# To figure out how much testing may be decreasing
if (1==2)
{
    dfs = read.nhbs.testing()
    
    times = get.testing.diffs(dfs, as.rates=F, relative=F, per.time=F, get.time=T)
    p.abs = get.testing.diffs(dfs, as.rates=F, relative=F, per.time=F)
    quantile(p.abs, .05)
    r.abs = get.testing.diffs(dfs, as.rates=T, relative=F, per.time=F)
    quantile(r.abs, .05)
    
    
    p.rel = get.testing.diffs(dfs, as.rates=F, relative=T, per.time=F)
    quantile(p.rel, .05)
    r.rel = get.testing.diffs(dfs, as.rates=T, relative=T, per.time=F)
    quantile(r.rel, .05)
    
    unique(paste0(dfs$risk$risk, " - ", dfs$risk$year))
}

get.testing.diffs <- function(dfs=read.nhbs.testing(),
                              as.rates=T,
                              relative=T,
                              per.time=T,
                              get.time=F)
{
    to.rate <- function(p){-log(1-p)}
    non.match.cols = c('n','frac.12mo','frac.ever','year','msa')
    diffs = unlist(lapply(setdiff(names(dfs), 'msa'), function(df.name){
        print(df.name)
        df = dfs[[df.name]]
        match.cols = setdiff(names(df), non.match.cols)
        #df = df[df$year==start.year | df$year==end.year,]
        ids = apply(df[,match.cols], 1, function(vals){paste0(vals, collapse='_')})
        
        unique.ids = unique(ids)
        sapply(unique.ids, function(id){
            id.mask = ids == id
            if (sum(id.mask)<2)
                stop(paste0("need two values for category '", id, "' in '", df.name, "' df"))
            
            end.year = max(df$year[id.mask])
            start.year = max(df$year[id.mask & df$year<end.year])
            
            start.val = df$frac.12mo[id.mask & df$year==start.year]
            end.val = df$frac.12mo[id.mask & df$year==end.year]
            
            if (length(start.val)!=1 || length(end.val)!=1|| is.na(start.val) || is.na(end.val))
                browser()
            
            if (get.time)
                return(end.year-start.year)
            
            if (relative)
            {
                if (as.rates)
                    rv = (to.rate(end.val) - to.rate(start.val)) / to.rate(start.val)
                else
                    rv = (end.val - start.val) / start.val
            }
            else
            {
                if (as.rates)
                    rv = (to.rate(end.val) - to.rate(start.val))
                else
                    rv = (end.val - start.val)
            }
            if (per.time)
                rv / (end.year-start.year)
            else
                rv
        })
    }))
    
    diffs
}