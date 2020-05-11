#source('../code/data_managers/locale_mappings.R')

library(reshape2)
library(dplyr)

##-----------------------------##
##-- UPDATE THE DATA MANAGER --##
##-----------------------------##

if (1==2)
{
    load('cached/ALL.DATA.MANAGERS.Rdata')

    cm =  read.continuum.manager(dir='../data2/Continuum/')
    ALL.DATA.MANAGERS$continuum = cm

    save(ALL.DATA.MANAGERS, file='cached/ALL.DATA.MANAGERS.Rdata')
}

##-------------##
##-- GETTERS --##
##-------------##

get.suppressed.proportions <- function(cm,
                                  msa.code,
                                  year,
                                  population,
                                  total.odds.z=0,
                                  black.or.z=0,
                                  hispanic.or.z=0,
                                  age1.or.z=0,
                                  age2.or.z=0,
                                  age4.or.z=0,
                                  age5.or.z=0,
                                  female.or.z=0,
                                  msm.or.z=0,
                                  idu.or.z=0,
                                  ages=c('13-24 years','25-34 years','35-44 years','45-54 years','55+ years'),
                                  races=c('black','hispanic','other'),
                                  sexes=c('heterosexual_male','msm','female'),
                                  idu.risk.strata=c('active_IDU','IDU_in_remission'),
                                  risks=c('never_IDU', idu.risk.strata))
{
    do.get.continuum.risks(cm,
                           data.type='suppression',
                           return.as.rates = F,
                           msa.code=msa.code,
                           year=year,
                           population=population,
                           total.odds.z=total.odds.z,
                           black.or.z=black.or.z,
                           hispanic.or.z=hispanic.or.z,
                           age1.or.z=age1.or.z,
                           age2.or.z=age2.or.z,
                           age4.or.z=age4.or.z,
                           age5.or.z=age5.or.z,
                           female.or.z=female.or.z,
                           msm.or.z=msm.or.z,
                           idu.or.z=idu.or.z,
                           ages=ages,
                           races=races,
                           sexes=sexes,
                           idu.risk.strata=idu.risk.strata,
                           risks=risks)
}

get.linkage.rates <- function(cm,
                              msa.code,
                              year,
                              population,
                              msm.odds.z=0,
                              idu.odds.z=0,
                              heterosexual.odds.z=0,
                              black.or.z=0,
                              hispanic.or.z=0,
                              age1.or.z=0,
                              age2.or.z=0,
                              age4.or.z=0,
                              age5.or.z=0,
                              female.or.z=0,
                              ages=c('13-24 years','25-34 years','35-44 years','45-54 years','55+ years'),
                              races=c('black','hispanic','other'),
                              sexes=c('heterosexual_male','msm','female'),
                              idu.risk.strata=c('active_IDU','IDU_in_remission'),
                              risks=c('never_IDU', idu.risk.strata))
{
    risk.mat = do.get.continuum.risks(cm,
                                      data.type='linkage',
                                      return.as.rates = T,
                                      msa.code=msa.code,
                                      year=year,
                                      population=population,
                                      total.odds.z=total.odds.z,
                                      black.or.z=black.or.z,
                                      hispanic.or.z=hispanic.or.z,
                                      age1.or.z=age1.or.z,
                                      age2.or.z=age2.or.z,
                                      age4.or.z=age4.or.z,
                                      age5.or.z=age5.or.z,
                                      female.or.z=female.or.z,
                                      msm.or.z=msm.or.z,
                                      idu.or.z=idu.or.z,
                                      ages=ages,
                                      races=races,
                                      sexes=sexes,
                                      idu.risk.strata=idu.risk.strata,
                                      risks=risks)

}

get.hiv.testing.proportions <- function(cm,
                                  msa.code,
                                  year,
                                  population,
                                  msm.odds.z=0,
                                  idu.odds.z=0,
                                  heterosexual.odds.z=0,
                                  black.or.z=0,
                                  hispanic.or.z=0,
                                  age1.or.z=0,
                                  age2.or.z=0,
                                  age4.or.z=0,
                                  age5.or.z=0,
                                  female.or.z=0,
                                  ages=c('13-24 years','25-34 years','35-44 years','45-54 years','55+ years'),
                                  races=c('black','hispanic','other'),
                                  sexes=c('heterosexual_male','msm','female'),
                                  idu.risk.strata=c('active_IDU','IDU_in_remission'),
                                  risks=c('never_IDU', idu.risk.strata))
{
    risk.mat = do.get.continuum.risks(cm,
                                      data.type='testing',
                                      return.as.rates = F,
                                      msa.code=msa.code,
                                      year=year,
                                      population=population,
                                      msm.odds.z=msm.odds.z,
                                      idu.odds.z=idu.odds.z,
                                      heterosexual.odds.z=heterosexual.odds.z,
                                      black.or.z=black.or.z,
                                      hispanic.or.z=hispanic.or.z,
                                      age1.or.z=age1.or.z,
                                      age2.or.z=age2.or.z,
                                      age4.or.z=age4.or.z,
                                      age5.or.z=age5.or.z,
                                      female.or.z=female.or.z,
                                      ages=ages,
                                      races=races,
                                      sexes=sexes,
                                      idu.risk.strata=idu.risk.strata,
                                      risks=risks)
}

get.hiv.testing.rates <- function(cm,
                                  msa.code,
                                  year,
                                  population,
                                  msm.odds.z=0,
                                  idu.odds.z=0,
                                  heterosexual.odds.z=0,
                                  black.or.z=0,
                                  hispanic.or.z=0,
                                  age1.or.z=0,
                                  age2.or.z=0,
                                  age4.or.z=0,
                                  age5.or.z=0,
                                  female.or.z=0,
                                  ages=c('13-24 years','25-34 years','35-44 years','45-54 years','55+ years'),
                                  races=c('black','hispanic','other'),
                                  sexes=c('heterosexual_male','msm','female'),
                                  idu.risk.strata=c('active_IDU','IDU_in_remission'),
                                  risks=c('never_IDU', idu.risk.strata))
{
    risk.mat = do.get.continuum.risks(cm,
                                      data.type='testing',
                                      return.as.rates = T,
                                      msa.code=msa.code,
                                      year=year,
                                      population=population,
                                      msm.odds.z=msm.odds.z,
                                      idu.odds.z=idu.odds.z,
                                      heterosexual.odds.z=heterosexual.odds.z,
                                      black.or.z=black.or.z,
                                      hispanic.or.z=hispanic.or.z,
                                      age1.or.z=age1.or.z,
                                      age2.or.z=age2.or.z,
                                      age4.or.z=age4.or.z,
                                      age5.or.z=age5.or.z,
                                      female.or.z=female.or.z,
                                      ages=ages,
                                      races=races,
                                      sexes=sexes,
                                      idu.risk.strata=idu.risk.strata,
                                      risks=risks)
}

get.testing.rate.years <- function(cm)
{
    intersect(
        as.numeric(dimnames(cm$testing$total.by.risk.mean.log.odds)[[1]]),
        as.numeric(dimnames(cm$testing$ors.12mo.mean.log.odds)[[1]])
    )
}

get.suppressed.proportion.years <- function(cm)
{
    intersect(
        as.numeric(dimnames(cm$suppression$total.mean.log.odds)[[1]]),
        as.numeric(dimnames(cm$suppression$ors.12mo.mean.log.odds)[[1]])
    )
}

get.linkage.years <- function(cm)
{
    intersect(
        as.numeric(dimnames(cm$linkage$total.mean.log.odds)[[1]]),
        as.numeric(dimnames(cm$linkage$ors.12mo.mean.log.odds)[[1]])
    )
}

##-------------##
##-- READERS --##
##-------------##

read.continuum.manager <- function(dir='../data2/Continuum/',
                                   surv=msa.surveillance)
{
    cm = list()
    cm = read.testing(cm, dir=dir)
    cm = read.linkage(cm, dir=dir)
    cm = read.suppression(cm, dir=dir)

#for now
    cm$retained = list()

    cm = set.site.specific.data(cm, surv)

    cm
}

read.testing <- function(cm, dir='../data2/Continuum/')
{
    agg.dir = paste0(dir, 'NHBS_reports/Aggregate Tables/')

    #-- Initial Set-Up --#
    cm$testing = list()

    or.names = c('black','hispanic','age1','age2','age4','age5','msm','female','idu')


    #-- Read from files --#
    df.msm.2011 = read.continuum.file(paste0(agg.dir, '2011 MSM all testing.csv'))
    df.idu.2012 = read.continuum.file(paste0(agg.dir, '2012 IDU all testing.csv'))
    df.het.2013 = read.continuum.file(paste0(agg.dir, '2013 heterosexual all testing.csv'))

    df.msm.2014 = read.continuum.file(paste0(agg.dir, '2014 MSM all testing.csv'))
    df.idu.2015 = read.continuum.file(paste0(agg.dir, '2015 IDU all testing.csv'))
    df.het.2016 = read.continuum.file(paste0(agg.dir, '2016 heterosexual all testing.csv'))

    #Not using currently
    #df.msm.2017 = read.continuum.file(paste0(agg.dir, '2017 MSM all testing.csv'))

    #-- Pull demographic-based ORs --#

    ors.2012 = calculate.testing.odds(df.msm.2011, df.idu.2012, df.het.2013)
    ors.2015 = calculate.testing.odds(df.msm.2014, df.idu.2015, df.het.2016)

    cm$testing = do.set.continuum.or.distributions(cm$testing,
                                                   log.means=ors.2012$mean.log.ors,
                                                   log.ses=ors.2012$se.log.ors,
                                                   msa.code='all',
                                                   year=2012,
                                                   months=12)

    cm$testing = do.set.continuum.or.distributions(cm$testing,
                                                   log.means=ors.2015$mean.log.ors,
                                                   log.ses=ors.2015$se.log.ors,
                                                   msa.code='all',
                                                   year=2015,
                                                   months=12)

    #-- Pull MSA-Specific Estimates --#

    msa.msm.2011 = parse.testing.odds.by.msa(df.msm.2011)
    msa.idu.2012 = parse.testing.odds.by.msa(df.idu.2012)
    msa.het.2013 = parse.testing.odds.by.msa(df.het.2013)

    msa.msm.2014 = parse.testing.odds.by.msa(df.msm.2014)
    msa.idu.2015 = parse.testing.odds.by.msa(df.idu.2015)
    msa.het.2016 = parse.testing.odds.by.msa(df.het.2016)

    msa.codes = sort(unique(c(msa.msm.2011$cbsa,
                              msa.idu.2012$cbsa,
                              msa.het.2013$cbsa,
                              msa.msm.2014$cbsa,
                              msa.idu.2015$cbsa,
                              msa.het.2016$cbsa)))

    dim.names = list(year=c('2012','2015'), msa=msa.codes, rf=c('heterosexual','msm','idu'))
    mean.log.odds.for.msa = se.log.odds.for.msa =
        array(NA, dim=sapply(dim.names, length), dimnames=dim.names)

    mean.log.odds.for.msa['2012', msa.msm.2011$cbsa, 'msm'] = msa.msm.2011$mean.log.odds
    se.log.odds.for.msa['2012', msa.msm.2011$cbsa, 'msm'] = msa.msm.2011$se.log.odds

    mean.log.odds.for.msa['2012', msa.idu.2012$cbsa, 'idu'] = msa.idu.2012$mean.log.odds
    se.log.odds.for.msa['2012', msa.idu.2012$cbsa, 'idu'] = msa.idu.2012$se.log.odds

    mean.log.odds.for.msa['2012', msa.idu.2012$cbsa, 'heterosexual'] = msa.het.2013$mean.log.odds
    se.log.odds.for.msa['2012', msa.idu.2012$cbsa, 'heterosexual'] = msa.het.2013$se.log.odds

    mean.log.odds.for.msa['2015', msa.msm.2014$cbsa, 'msm'] = msa.msm.2014$mean.log.odds
    se.log.odds.for.msa['2015', msa.msm.2014$cbsa, 'msm'] = msa.msm.2014$se.log.odds

    mean.log.odds.for.msa['2015', msa.idu.2015$cbsa, 'idu'] = msa.idu.2015$mean.log.odds
    se.log.odds.for.msa['2015', msa.idu.2015$cbsa, 'idu'] = msa.idu.2015$se.log.odds

    mean.log.odds.for.msa['2015', msa.het.2016$cbsa, 'heterosexual'] = msa.het.2016$mean.log.odds
    se.log.odds.for.msa['2015', msa.het.2016$cbsa, 'heterosexual'] = msa.het.2016$se.log.odds

    #Either we are missing none, one, or five
    #Toss the ones missing five
    missing = apply(is.na(mean.log.odds.for.msa), 'msa', sum)
    to.toss = missing > 1
    mean.log.odds.for.msa = mean.log.odds.for.msa[,!to.toss,]
    se.log.odds.for.msa = se.log.odds.for.msa[,!to.toss,]

    for (year in dimnames(mean.log.odds.for.msa)[[1]])
    {
        for (msa in dimnames(mean.log.odds.for.msa)[[2]])
        {
            cm$testing = do.set.total.odds.distribution.for.separate.totals(cm$testing,
                                                                            log.means=mean.log.odds.for.msa[year, msa, ],
                                                                            log.ses=se.log.odds.for.msa[year, msa, ],
                                                                            msa.code=msa,
                                                                            year=year,
                                                                            months=12)
        }
    }

    cm = do.testing.regressions(cm, ever.tested.file=paste0(dir, 'BRFSS/BRFSS HIV Test by MSA.csv'))

    #-- Return --#
    cm

}

do.testing.regressions <- function(cm, ever.tested.file='../data2/Continuum/BRFSS/BRFSS HIV Test by MSA.csv')
{
    #-- Parse the ever tested data --#
    ever.df = read.csv(ever.tested.file, stringsAsFactors = F)
    ever.df$Sample_Size = gsub(',', '', ever.df$Sample_Size)
    ever.df$Sample_Size = as.numeric(ever.df$Sample_Size)

    ever.df = data.frame(name=ever.df$Locationdesc,
#                         orig.code=ever.df$Locationabbr,
                         num.tested=ever.df$Data_value*ever.df$Sample_Size/100,
                         denominator=ever.df$Sample_Size,
                         year=as.integer(ever.df[,1]))
    ever.df$name = gsub(' Metropolitan Statistical Area', '', ever.df$name)
    ever.df$name = gsub(' Metropolitan Division.*', '', ever.df$name)
    ever.df$msa = cbsa.for.msa.name(ever.df$name)
    ever.df$msa[grepl('Silver Spring', ever.df$name)] = cbsa.for.msa.name('Washington, DC')

    ever.df = ever.df[!is.na(ever.df$msa),]
    ever.df = ever.df[,c('msa','year','num.tested','denominator')]

    #-- Collapse Repeats --#
    ever.df %>%
        group_by(year, msa) %>%
        summarise_all(sum) %>%
        data.frame() -> ever.df

    ever.df$frac.tested = ever.df$num.tested / ever.df$denominator
    ever.df$log.odds.ever.tested = ever.df$frac.tested / (1 - ever.df$frac.tested)

    #-- Get the testing data we do have into a usable data frame (two) --#
    df = NULL
    df.paired = data.frame(msa=dimnames(cm$testing$total.by.risk.mean.log.odds)[['msa']])
    for (year in dimnames(cm$testing$total.by.risk.mean.log.odds)[['year']])
    {
        ever.subset = ever.df[ever.df$year==as.numeric(year)+1,]
        ever.tested = ever.subset$log.odds.ever.tested
        names(ever.tested) = as.character(ever.subset$msa)

        df = rbind(df,
                   data.frame(year=year,
                              msa=dimnames(cm$testing$total.by.risk.mean.log.odds)[['msa']],
                              log.odds.het = cm$testing$total.by.risk.mean.log.odds[year,,'heterosexual'],
                              log.odds.msm = cm$testing$total.by.risk.mean.log.odds[year,,'msm'],
                              log.odds.idu = cm$testing$total.by.risk.mean.log.odds[year,,'idu'],
                              log.odds.ever.tested = ever.tested[dimnames(cm$testing$total.by.risk.mean.log.odds)[['msa']]]))

        one.df = data.frame(log.odds.het = cm$testing$total.by.risk.mean.log.odds[year,,'heterosexual'],
                            log.odds.msm = cm$testing$total.by.risk.mean.log.odds[year,,'msm'],
                            log.odds.idu = cm$testing$total.by.risk.mean.log.odds[year,,'idu'])
#                            log.odds.ever.tested = ever.tested[dimnames(cm$testing$total.by.risk.mean.log.odds)[['msa']]])
        names(one.df) = paste0(names(one.df), '.', year)
        df.paired = cbind(df.paired, one.df)
    }

    complete.df = df[!apply(is.na(df), 1, any),]
    complete.paired = df.paired[!apply(is.na(df.paired), 1, any),]

    #-- Fill in the missing het 2015 values --#
    fit.het.2015 = lm(log.odds.het.2015 ~ log.odds.het.2012 + log.odds.msm.2015 + log.odds.msm.2012 + log.odds.idu.2015 + log.odds.idu.2012, data=complete.paired)
    missing.het.codes = as.character(df.paired$msa[is.na(df.paired$log.odds.het.2015)])
    preds = predict(fit.het.2015, df.paired[missing.het.codes,], se.fit = T)

    cm$testing$total.by.risk.mean.log.odds['2015', missing.het.codes, 'heterosexual'] = preds$fit
    cm$testing$total.by.risk.se.log.odds['2015', missing.het.codes, 'heterosexual'] = preds$se.fit

    #-- Predict from ever testing --#
    complete.df$year = as.integer(as.character(complete.df$year))
    fit.het = lm(log.odds.het ~ log.odds.ever.tested + year, complete.df)
    fit.msm = lm(log.odds.msm ~ log.odds.ever.tested + year, complete.df)
    fit.idu = lm(log.odds.idu ~ log.odds.ever.tested + year, complete.df)

    preds.het = predict(fit.het, ever.df, se.fit = T)
    preds.msm = predict(fit.msm, ever.df, se.fit = T)
    preds.idu = predict(fit.idu, ever.df, se.fit = T)

    #-- Inflate the SEs --#
    # (this is a hack -- add the se from the log odds of ever testing, plus the se from the fit

    var.test = 1/ever.df$num.tested + 1/(ever.df$denominator-ever.df$num.tested)
    se.test = sqrt(var.test)
    ses.het = sqrt(preds.het$se.fit^2 + var.test)
    ses.msm = sqrt(preds.msm$se.fit^2 + var.test)
    ses.idu = sqrt(preds.idu$se.fit^2 + var.test)

    #-- Plug predictions into the CM --#

    for (i in 1:dim(ever.df)[1])
    {
        year = ever.df$year[i]-1
        if (is.null(do.get.total.odds.distribution(cm$testing, msa.code=ever.df$msa[i], year=year)))
        {
            log.means = c(heterosexual=as.numeric(preds.het$fit[i]),
                          msm=as.numeric(preds.msm$fit[i]),
                          idu=as.numeric(preds.idu$fit[i]))
            log.ses = c(heterosexual=as.numeric(ses.het[i]),
                        msm=as.numeric(ses.msm[i]),
                        idu=as.numeric(ses.idu[i]))

            cm$testing = do.set.total.odds.distribution.for.separate.totals(cm$testing,
                                                                            log.means=log.means,
                                                                            log.ses=log.ses,
                                                                            msa.code=ever.df$msa[i],
                                                                            year=year,
                                                                            month=12)
        }
    }

    #-- Return --#
    cm
}
read.linkage <- function(cm, dir)
{
    cm = read.linkage.by.demographics(cm, dir=paste0(dir, 'linkage_by_demographics/'))

    cm
}

read.linkage.by.demographics <- function(cm, dir='../data2/Continuum/linkage_by_demographics/')
{
    df.2012.3mo = read.continuum.file(paste0(dir, '2012_linkage_3mo.csv'))
    df.2013.3mo = read.continuum.file(paste0(dir, '2013_linkage_3mo.csv'))
    df.2013.1mo = read.continuum.file(paste0(dir, '2013_linkage_1mo.csv'))
    df.2014 = read.continuum.file(paste0(dir, '2014_linkage.csv'))
    df.2015 = read.continuum.file(paste0(dir, '2015_linkage.csv'))
    df.2016 = read.continuum.file(paste0(dir, '2016_linkage.csv'))


    parsed.3mo = list('2012'=parse.linkage.demographics(df.2012.3mo, T),
                      '2013'=parse.linkage.demographics(df.2013.3mo, T),
                      '2014'=parse.linkage.demographics(df.2014, T),
                      '2015'=parse.linkage.demographics(df.2015, T),
                      '2016'=parse.linkage.demographics(df.2016, T))

    parsed.1mo = list('2013'=parse.linkage.demographics(df.2013.1mo, F),
                      '2014'=parse.linkage.demographics(df.2014, F),
                      '2015'=parse.linkage.demographics(df.2015, F),
                      '2016'=parse.linkage.demographics(df.2016, F))

    cm$linkage = list()

    odds.3mo = lapply(parsed.3mo, function(parsed){do.calculate.demographic.odds.ratios(parsed$x, parsed$y)})
    odds.1mo = lapply(parsed.1mo, function(parsed){do.calculate.demographic.odds.ratios(parsed$x, parsed$y)})

    for (year in names(odds.3mo))
        cm$linkage = do.set.continuum.or.distributions(cm$linkage,
                                                       log.means = odds.3mo[[year]]$mean.log.ors,
                                                       log.ses = odds.3mo[[year]]$se.log.ors,
                                                       msa.code='all',
                                                       year=year,
                                                       months=3)

    for (year in names(odds.1mo))
        cm$linkage = do.set.continuum.or.distributions(cm$linkage,
                                                       log.means = odds.1mo[[year]]$mean.log.ors,
                                                       log.ses = odds.1mo[[year]]$se.log.ors,
                                                       msa.code='all',
                                                       year=year,
                                                       months=1)
    cm
}

read.suppression <- function(cm, dir)
{
    cm = read.suppression.by.demographics(cm, paste0(dir, 'suppression_by_demographics/'))

    cm
}

read.suppression.by.demographics <- function(cm, dir='../data2/Continuum/suppression_by_demographics/')
{
    files = list.files(dir)
    files = files[grepl('[0-9][0-9][0-9][0-9].*', files)]
    years = substr(files, 1,4)

    dfs = lapply(paste0(dir, files), read.continuum.file)
    parsed = lapply(dfs, parse.suppression.demographics)
    odds = lapply(parsed, function(pp){do.calculate.demographic.odds.ratios(pp$x, pp$y)})

    cm$suppression = list()

    for (i in 1:length(years))
        cm$suppression = do.set.continuum.or.distributions(cm$suppression,
                                                       log.means = odds[[i]]$mean.log.ors,
                                                       log.ses = odds[[i]]$se.log.ors,
                                                       msa.code='all',
                                                       year=years[i],
                                                       months=12)
    cm
}


##------------------------##
##-- HIGH-LEVEL HELPERS --##
##------------------------##

get.continuum.log.odds.and.ors <- function(cm,
                                           data.type=c('suppression','linkage','testing')[1],
                                           msa.code,
                                           years)
{
    if (data.type=='suppression')
        cm.subset = cm$suppression
    else if (data.type=='linkage')
        cm.subset = cm$linkage
    else if (data.type=='testing')
        cm.subset = cm$testing
    else
        stop("The get.continuum.log.odds.and.ors function only works for data.type 'suppression', 'linkage', or 'testing'")

    total.odds.dist = do.get.total.odds.distribution(cm.subset, msa.code, year)

    if (is.null(total.odds.dist))
        stop("The Continuum manager has no data on overall ", data.type," for MSA with code ", msa.code, " in year ", year)

#    if (separate.odds.by.risk)
#        total.odds.z = c(heterosexual=as.numeric(heterosexual.odds.z), msm=as.numeric(msm.odds.z), idu=as.numeric(idu.odds.z))

#    log.total.odds = total.odds.dist$mean + total.odds.z * total.odds.dist$se
#    total.odds = exp(log.total.odds)
    #this is now either a scalar for total, or a 3-length vector [het, msm, idu]


    #-- Pull Odds Ratios based on demographics --#
    or.dists = do.get.continuum.or.distributions(cm.subset, msa.code, year, months=total.odds.dist$months)

    if (is.null(or.dists))
        stop("The Continuum manager has no data on ", data.type," by demographic strata for MSA with code ", msa.code, " in year ", year)

 #   log.ors = or.dists$mean[names(or.zs)] + or.zs * or.dists$se[names(or.zs)]
  #  ors = exp(log.ors)

    or.mat = make.odds.ratio.matrix(black.or=ors['black'],
                                    hispanic.or=ors['hispanic'],
                                    age1.or=ors['age1'],
                                    age2.or=ors['age2'],
                                    age4.or=ors['age4'],
                                    age5.or=ors['age5'],
                                    msm.or=ors['msm'],
                                    female.or=ors['female'],
                                    idu.or=ors['idu'],
                                    msm.idu.or=ors['msm.idu'],
                                    ages=ages,
                                    races=races,
                                    sexes=sexes,
                                    idu.risk.strata=idu.risk.strata,
                                    risks=risks)

    list(total.odds.dist)
}

do.get.continuum.risks <- function(cm,
                                   data.type=c('suppression','linkage','testing')[1],
                                   return.as.rates=T,
                                   msa.code,
                                   year,
                                   population,
                                   total.odds.z=0,
                                   msm.odds.z=0,
                                   idu.odds.z=0,
                                   heterosexual.odds.z=0,
                                   black.or.z=0,
                                   hispanic.or.z=0,
                                   age1.or.z=0,
                                   age2.or.z=0,
                                   age4.or.z=0,
                                   age5.or.z=0,
                                   female.or.z=0,
                                   msm.or.z=0,
                                   idu.or.z=0,
                                   ages=c('13-24 years','25-34 years','35-44 years','45-54 years','55+ years'),
                                   races=c('black','hispanic','other'),
                                   sexes=c('heterosexual_male','msm','female'),
                                   idu.risk.strata=c('active_IDU','IDU_in_remission'),
                                   risks=c('never_IDU', idu.risk.strata))
{
    if (data.type=='suppression')
        cm.subset = cm$suppression
    else if (data.type=='linkage')
        cm.subset = cm$linkage
    else if (data.type=='testing')
        cm.subset = cm$testing
    else
        stop("The get.continuum.risks function only works for data.type 'suppression', 'linkage', or 'testing'")

    #-- Pull Total Odds --#

    total.odds.dist = do.get.total.odds.distribution(cm.subset, msa.code, year)

    if (is.null(total.odds.dist))
        stop("The Continuum manager has no data on overall ", data.type," for MSA with code ", msa.code, " in year ", year)
    
    separate.odds.by.risk = attr(total.odds.dist, 'by.risk')
    
    if (separate.odds.by.risk)
        total.odds.z = c(heterosexual=as.numeric(heterosexual.odds.z), msm=as.numeric(msm.odds.z), idu=as.numeric(idu.odds.z))

    log.total.odds = total.odds.dist$mean + total.odds.z * total.odds.dist$se
    total.odds = exp(log.total.odds)
    #this is now either a scalar for total, or a 3-length vector [het, msm, idu]


    #-- Pull Odds Ratios based on demographics --#

    or.zs = c(black=as.numeric(black.or.z),
              hispanic=as.numeric(hispanic.or.z),
              age1=as.numeric(age1.or.z),
              age2=as.numeric(age2.or.z),
              age4=as.numeric(age4.or.z),
              age5=as.numeric(age5.or.z),
              msm=ifelse(separate.odds.by.risk, 0, as.numeric(msm.or.z)), #msm and idu may be handled at the total odds level
              female=as.numeric(female.or.z),
              idu=ifelse(separate.odds.by.risk, 0, as.numeric(idu.or.z)))

    or.dists = do.get.continuum.or.distributions(cm.subset, msa.code, year, months=total.odds.dist$months)

    if (is.null(or.dists))
        stop("The Continuum manager has no data on ", data.type," by demographic strata for MSA with code ", msa.code, " in year ", year)

    log.ors = or.dists$mean[names(or.zs)] + or.zs * or.dists$se[names(or.zs)]
    ors = exp(log.ors)

    or.mat = make.odds.ratio.matrix(black.or=ors['black'],
                                    hispanic.or=ors['hispanic'],
                                    age1.or=ors['age1'],
                                    age2.or=ors['age2'],
                                    age4.or=ors['age4'],
                                    age5.or=ors['age5'],
                                    msm.or=ors['msm'],
                                    female.or=ors['female'],
                                    idu.or=ors['idu'],
                                    msm.idu.or=ors['msm.idu'],
                                    ages=ages,
                                    races=races,
                                    sexes=sexes,
                                    idu.risk.strata=idu.risk.strata,
                                    risks=risks)


    #-- Mash the ORs and the total odds together --#

    if (separate.odds.by.risk)
    {
        non.idu.risk.strata = setdiff(risks, idu.risk.strata)

        odds.mat.msm = scale.odds.matrix.to.total.odds(access(or.mat, sex='msm'), total.odds['msm'], access(population, sex='msm'))
        odds.mat.het = scale.odds.matrix.to.total.odds(access(or.mat, sex=c('heterosexual_male','female'), risk=non.idu.risk.strata),
                                                       total.odds['heterosexual'],
                                                       access(population, sex=c('heterosexual_male','female'), risk=non.idu.risk.strata))
        odds.mat.idu = scale.odds.matrix.to.total.odds(access(or.mat, sex=c('heterosexual_male','female'), risk=idu.risk.strata),
                                                       total.odds['idu'],
                                                       access(population, sex=c('heterosexual_male','female'), risk=idu.risk.strata))

        use.msm.idu = any(names(total.odds)=='msm.idu')
        if (use.msm.idu)
            odds.mat.msm.idu = scale.odds.matrix.to.total.odds(access(or.mat, sex=c('msm'), risk=idu.risk.strata),
                                                               total.odds['msm.idu'],
                                                               access(population, sex=c('msm'), risk=idu.risk.strata))
        
        
        odds.mat = array(NA, dim=dim(or.mat), dimnames=dimnames(or.mat))
        access(odds.mat, sex='msm') = odds.mat.msm
        access(odds.mat, sex=c('heterosexual_male','female'), risk=non.idu.risk.strata) = odds.mat.het
        access(odds.mat, sex=c('heterosexual_male','female'), risk=idu.risk.strata) = odds.mat.idu
        
        if (use.msm.idu)
            access(odds.mat, sex=c('msm'), risk=idu.risk.strata) = odds.mat.msm.idu
    }
    else
        odds.mat = scale.odds.matrix.to.total.odds(or.mat, total.odds, population)

    #-- Convert to risks or rates and return --#
    risk.mat = odds.mat / (1 + odds.mat)
    attr(risk.mat, 'time') = total.odds.dist$months/12

    if (return.as.rates)
    {
        rates.mat = -log(1-risk.mat) / attr(risk.mat, 'time')
        attr(rates.mat, 'time') = attr(risk.mat, 'time')
        rates.mat
    }
    else
        risk.mat
}

do.get.total.odds.distribution <- function(cm.subset,
                                           msa.code='all',
                                           year)
{
    year = as.character(year)
    
    if (any(dimnames(cm.subset$total.by.risk.mean.log.odds)[[1]]==year) &&
        any(dimnames(cm.subset$total.by.risk.mean.log.odds)[[2]]==msa.code) &&
        !is.na(cm.subset$total.by.risk.mean.log.odds[year, msa.code,]))
    {
        rv = list(mean=cm.subset$total.by.risk.mean.log.odds[year, msa.code,],
             se=cm.subset$total.by.risk.se.log.odds[year, msa.code,],
             months=cm.subset$total.by.risk.months[year, msa.code])
        attr(rv, 'by.risk') = T
        rv
    }
    else if (any(dimnames(cm.subset$total.mean.log.odds)[[1]]==year) &&
        any(dimnames(cm.subset$total.mean.log.odds)[[2]]==msa.code) &&
        !is.na(cm.subset$total.mean.log.odds[year, msa.code,]))
    {
        rv = list(mean=cm.subset$total.mean.log.odds[year, msa.code,],
             se=cm.subset$total.se.log.odds[year, msa.code,],
             months=cm.subset$total.months[year, msa.code])
        attr(rv, 'by.risk') = F
        rv
    }
    else if (msa.code != 'all')
        do.get.total.odds.distribution(cm.subset, 'all', year=year)
    else
        NULL
}

do.set.total.odds.distribution.from.prob <- function(cm.subset,
                                                     probs,
                                                     ns,
                                                     msa.code,
                                                     year,
                                                     months)
{
    if (any(probs > 1) || any(probs < 0))
        stop("probs must be between zero and 1")
    
    log.means = log(probs) - log(1-probs)
    
    if (length(ns) < length(probs))
    {
        if (length(ns) > 1)
            stop("'ns' must have either length 1, or length equal to probs")
        else
            ns = rep(ns, length(probs))
    }
    
    log.ses = sqrt(1/(ns*probs) + 1/(ns * (1-probs)))
    
    do.set.total.odds.distribution.for.single.total(cm.subset,
                                                    log.means = log.means,
                                                    log.ses = log.ses,
                                                    msa.code = msa.code,
                                                    year = year,
                                                    months = months)
}

do.set.or.distributions.from.probs<- function(cm.subset,
                                                    probs,
                                                    ns,
                                                    msa.code,
                                                    year,
                                                    months=12)
{
    if (any(probs > 1) || any(probs < 0))
        stop("probs must be between zero and 1")
    
    if (length(ns) < length(probs))
    {
        if (length(ns) > 1)
            stop("'ns' must have either length 1, or length equal to probs")
        else
            ns = rep(ns, length(probs))
        
        if (is.null(names(ns)))
            names(ns) = names(probs)
    }
    
    ors = do.calculate.demographic.odds.ratios(x = probs * ns,
                                               y = (1-probs) * ns)
    
    do.set.continuum.or.distributions(cm.subset,
                                      log.means = ors$mean.log.ors,
                                      log.ses = ors$se.log.ors,
                                      msa.code = msa.code,
                                      year = year,
                                      months = months)
    
}

do.set.total.odds.distribution.from.prob <- function(cm.subset,
                                                     probs,
                                                     ns,
                                                     msa.code,
                                                     year,
                                                     months)
{
    if (any(probs > 1) || any(probs < 0))
        stop("probs must be between zero and 1")

    log.means = log(probs) - log(1-probs)

    if (length(ns) < length(probs))
    {
        if (length(ns) > 1)
            stop("'ns' must have either length 1, or length equal to probs")
        else
            ns = rep(ns, length(probs))
    }

    log.ses = sqrt(1/(ns*probs) + 1/(ns * (1-probs)))

    
    do.set.total.odds.distribution.for.single.total(cm.subset,
                                                       log.means = log.means,
                                                       log.ses = log.ses,
                                                       msa.code = msa.code,
                                                       year = year,
                                                       months = months)
}

do.set.all.odds.distributions.from.probs<- function(cm.subset,
                                                    probs,
                                                    ns,
                                                    msa.code,
                                                    year,
                                                    months=12)
{
    if (any(probs > 1) || any(probs < 0))
        stop("probs must be between zero and 1")
    
    if (length(ns) < length(probs))
    {
        if (length(ns) > 1)
            stop("'ns' must have either length 1, or length equal to probs")
        else
            ns = rep(ns, length(probs))
        
        if (is.null(names(ns)))
            names(ns) = names(probs)
    }
    
    #totals
    if (any(names(probs)=='msm.idu'))
        total.indices = c('heterosexual', 'msm', 'idu', 'msm.idu')
    else
        total.indices = c('heterosexual', 'msm', 'idu')
    
    total.log.means = log(probs[total.indices]) - log(1-probs[total.indices])
    
    total.log.ses = sqrt(1/(ns[total.indices]*probs[total.indices]) + 
                             1/(ns[total.indices] * (1-probs[total.indices])))
    
    cm.subset = do.set.total.odds.distribution.for.separate.totals(cm.subset,
                                                                   log.means = total.log.means,
                                                                   log.ses = total.log.ses,
                                                                   msa.code = msa.code,
                                                                   year = year,
                                                                   months = months)
    
    #ors
    
    ors = do.calculate.demographic.odds.ratios(x = probs * ns,
                                               y = (1-probs) * ns)

    do.set.continuum.or.distributions(cm.subset,
                                      log.means = ors$mean.log.ors,
                                      log.ses = ors$se.log.ors,
                                      msa.code = msa.code,
                                      year = year,
                                      months = months)
    
}

do.set.total.odds.distribution.for.single.total <- function(cm.subset,
                                                            log.means,
                                                            log.ses,
                                                            msa.code,
                                                            year,
                                                            months)
{
    if (length(log.means) != length(log.ses))
        stop("log.means must have the same length as log.ses")
    if (length(log.means) < length(msa.code) || length(log.means) < length(year))
        stop("log.means and log.ses must have a value specified for each msa.code and year")

    if (length(msa.code) < length(log.means))
    {
        if (length(msa.code) > 1)
            stop("msa.code must have either length 1, or length equal to log.means and log.ses")
        else
            msa.code = rep(msa.code, length(log.means))
    }

    if (length(year) < length(log.means))
    {
        if (length(year) > 1)
            stop("'year' must have either length 1, or length equal to log.means and log.ses")
        else
            year = rep(year, length(log.means))
    }

    if (length(months) < length(log.means))
    {
        if (length(months) > 1)
            stop("'months' must have either length 1, or length equal to log.means and log.ses")
        else
            months = rep(months, length(log.means))
    }
    
    for (i in 1:length(log.means))
    {
        one.log.mean = c(total=as.numeric(log.means[i]))
        one.log.se = c(total=as.numeric(log.ses[i]))
        cm.subset = do.set.continuum.distributions(cm.subset,
                                       value.prefix = 'total',
                                       value.names = 'total',
                                       log.means = one.log.mean,
                                       log.ses = one.log.se,
                                       msa.code = msa.code[i],
                                       year = year[i],
                                       months = months[i]
        )
    }

    cm.subset
}

do.set.total.odds.distribution.for.separate.totals <- function(cm.subset,
                                                               log.means,
                                                               log.ses,
                                                               msa.code,
                                                               year,
                                                               months)
{
    if (length(log.means)==4)
        value.names = c('heterosexual', 'msm', 'idu', 'msm.idu')
    else
        value.names = c('heterosexual', 'msm', 'idu')
    
    do.set.continuum.distributions(cm.subset,
                                   value.prefix = 'total.by.risk',
                                   value.names = value.names,
                                   log.means = log.means,
                                   log.ses = log.ses,
                                   msa.code = msa.code,
                                   year = year,
                                   months = months
    )
}

do.get.continuum.or.distributions <- function(cm.subset, msa.code='all', year, months,
                                              backup.months = ifelse(months==3, 1, 3))
{
    year = as.character(year)

    mean.name = paste0('ors.', months, 'mo.mean.log.odds')
    se.name = paste0('ors.', months, 'mo.se.log.odds')

    rv = NULL

    non.msm.idu = setdiff(dimnames(cm.subset[[mean.name]])[[3]], 'msm.idu')
    #first, try to pull the year, location, and months we want
    if (any(dimnames(cm.subset[[mean.name]])[[1]]==year) &&
        any(dimnames(cm.subset[[mean.name]])[[2]]==msa.code) &&
        !any(is.na(cm.subset[[mean.name]][year, msa.code, non.msm.idu])))
    {
        rv = list(mean=cm.subset[[mean.name]][year, msa.code,],
                se=cm.subset[[se.name]][year, msa.code,])
    }

    #next, try from different setting for months
    if (is.null(rv) && !is.na(backup.months))
        rv = do.get.continuum.or.distributions(cm.subset, msa.code, year=year, months=backup.months, backup.months=NA)

    #next, try from nearest year
    if (is.null(rv) &&
        any(dimnames(cm.subset[[mean.name]])[[2]]==msa.code))
    {
        non.msm.idu = setdiff(dimnames(cm.subset[[mean.name]])[[3]], 'msm.idu')
        present.year.mask = apply(!is.na(cm.subset[[mean.name]][, msa.code, non.msm.idu]), 1, all)
        if (any(present.year.mask))
        {
            present.years = as.numeric(dimnames(cm.subset[[mean.name]])[['year']][present.year.mask])
            year = as.numeric(year)
            
            #linearly interpolate between the closest before and closest after
            if (all(present.years>year))
                rv = list(mean=cm.subset[[mean.name]][as.character(min(present.years)), msa.code, ],
                          se=cm.subset[[se.name]][as.character(min(present.years)), msa.code, ])
            else if (all(present.years<year))
                rv = list(mean=cm.subset[[mean.name]][as.character(max(present.years)), msa.code, ],
                          se=cm.subset[[se.name]][as.character(max(present.years)), msa.code, ])
            else
            {
                year.before = max(present.years[present.years<year])
                year.after = min(present.years[present.years>year])
                
                weight.before = (year.after - year) / (year.after - year.before)
                weight.after = (year - year.before) / (year.after - year.before)
                
                rv = list(mean = weight.before * cm.subset[[mean.name]][as.character(year.before), msa.code, ] +
                              weight.after * cm.subset[[mean.name]][as.character(year.after), msa.code, ],
                          se = weight.before * cm.subset[[se.name]][as.character(year.before), msa.code, ] +
                              weight.after * cm.subset[[se.name]][as.character(year.after), msa.code, ])
            }
        }
    }
  
    #last, try from the 'all' repository
    if (is.null(rv) && msa.code != 'all')
        rv = do.get.continuum.or.distributions(cm.subset, 'all', year=year, months=months, backup.months=backup.months)

    rv
}

do.set.continuum.or.distributions <- function(cm.subset,
                                              log.means,
                                              log.ses,
                                              msa.code,
                                              year,
                                              months)
{
    if (any(is.na(log.means[c('black','hispanic','age1','age2','age4','age5','msm','female','idu')])))
        stop("NA values in log means")
    do.set.continuum.distributions(cm.subset,
                                   value.prefix = paste0('ors.', months, 'mo'),
                                   value.names = c('black','hispanic','age1','age2','age4','age5','msm','female','idu','msm.idu'),
                                   log.means = log.means,
                                   log.ses = log.ses,
                                   msa.code = msa.code,
                                   year = year,
                                   months = months
    )
}

do.set.continuum.distributions <- function(cm.subset,
                                           value.prefix,
                                           value.names,
                                           log.means,
                                           log.ses,
                                           msa.code,
                                           year,
                                           months)
{
    msa.code = as.character(msa.code)
    year = as.character(year)
    base.values = sapply(value.names, function(name){0})

    means.name = paste0(value.prefix, '.mean.log.odds')
    ses.name = paste0(value.prefix, '.se.log.odds')
    months.name = paste0(value.prefix, '.months')

    if (is.null(cm.subset[[means.name]]))
    {
        dim.names = list(year=as.character(year),
                         msa=msa.code,
                         category=value.names)
    }
    else
    {
        dim.names = list(year=sort(unique(c(dimnames(cm.subset[[means.name]])[['year']], year))),
                         msa=sort(unique(c(dimnames(cm.subset[[means.name]])[['msa']], msa.code))),
                         category=value.names)
    }

    new.means = new.ses = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    new.months = array(NA, dim=sapply(dim.names[1:2], length), dimnames=dim.names[1:2])

    if (!is.null(cm.subset[[means.name]]))
    {
        new.means[dimnames(cm.subset[[means.name]])[[1]], dimnames(cm.subset[[means.name]])[[2]],] =
            cm.subset[[means.name]]
        new.ses[dimnames(cm.subset[[ses.name]])[[1]], dimnames(cm.subset[[ses.name]])[[2]],] =
            cm.subset[[ses.name]]
        new.months[dimnames(cm.subset[[ses.name]])[[1]], dimnames(cm.subset[[ses.name]])[[2]]] = cm.subset[[months.name]]
    }
    
    log.means = log.means[sapply(names(log.means), function(x){any(x==value.names)})]
    log.ses = log.ses[sapply(names(log.ses), function(x){any(x==value.names)})]

    new.log.means = new.log.ses = base.values
    new.log.means[names(log.means)] = log.means
    new.log.ses[names(log.ses)] = log.ses
    
    new.means[year, msa.code, ] = log.means
    new.ses[year, msa.code, ] = log.ses
    new.months[year, msa.code] = months

    cm.subset[[means.name]] = new.means
    cm.subset[[ses.name]] = new.ses
    cm.subset[[months.name]] = new.months

    cm.subset
}

##----------------------##
##-- CALCULATING ODDS --##
##----------------------##

do.calculate.demographic.odds.ratios <- function(x, y)
{
    mean.log.ors = se.log.ors = numeric()


    #Race
    mean.log.ors['black'] = log(x['black'] * y['other'] / y['black'] / x['other'])
    se.log.ors['black'] = sqrt(1/x['black'] + 1/y['black'] + 1/x['other'] + 1/y['other'])

    mean.log.ors['hispanic'] = log(x['hispanic'] * y['other'] / y['hispanic'] / x['other'])
    se.log.ors['hispanic'] = sqrt(1/x['hispanic'] + 1/y['hispanic'] + 1/x['other'] + 1/y['other'])


    #Age
    mean.log.ors['age1'] = log(x['age1'] * y['age3'] / y['age1'] / x['age3'])
    se.log.ors['age1'] = sqrt(1/x['age1'] + 1/y['age1'] + 1/x['age3'] + 1/y['age3'])

    mean.log.ors['age2'] = log(x['age2'] * y['age3'] / y['age2'] / x['age3'])
    se.log.ors['age2'] = sqrt(1/x['age2'] + 1/y['age2'] + 1/x['age3'] + 1/y['age3'])

    mean.log.ors['age4'] = log(x['age4'] * y['age3'] / y['age4'] / x['age3'])
    se.log.ors['age4'] = sqrt(1/x['age4'] + 1/y['age4'] + 1/x['age3'] + 1/y['age3'])

    mean.log.ors['age5'] = log(x['age5'] * y['age3'] / y['age5'] / x['age3'])
    se.log.ors['age5'] = sqrt(1/x['age5'] + 1/y['age5'] + 1/x['age3'] + 1/y['age3'])

    # Male/female
    mean.log.ors['female'] = log(x['female'] * y['male'] / y['female'] / x['male'])
    se.log.ors['female'] = sqrt(1/x['female'] + 1/y['female'] + 1/x['male'] + 1/y['male'])
      
    if (any(names(x)=='non.idu'))
    {
        #Sex
        mean.log.ors['msm'] = log(x['msm'] * y['male'] / y['msm'] / x['male'])
        se.log.ors['msm'] = sqrt(1/x['msm'] + 1/y['msm'] + 1/x['male'] + 1/y['male'])
    
        #IDU
        mean.log.ors['idu'] = log(x['idu'] * y['non.idu'] / y['idu'] / x['non.idu'])
        se.log.ors['idu'] = sqrt(1/x['idu'] + 1/y['idu'] + 1/x['non.idu'] + 1/y['non.idu'])
        
        #MSM-IDU
        mean.log.ors['msm.idu'] = NA
        se.log.ors['msm.idu'] = NA
    }
    else
    {
        #Sex
        mean.log.ors['msm'] = log(x['msm'] * y['heterosexual'] / y['msm'] / x['heterosexual'])
        se.log.ors['msm'] = sqrt(1/x['msm'] + 1/y['msm'] + 1/x['heterosexual'] + 1/y['heterosexual'])
        
        #IDU
        mean.log.ors['idu'] = log(x['idu'] * y['heterosexual'] / y['idu'] / x['heterosexual'])
        se.log.ors['idu'] = sqrt(1/x['idu'] + 1/y['idu'] + 1/x['heterosexual'] + 1/y['heterosexual'])
        
        #MSM-IDU
        mean.log.ors['msm.idu'] = log(x['msm.idu'] * y['heterosexual'] / y['msm.idu'] / x['heterosexual'])
        se.log.ors['msm.idu'] = sqrt(1/x['msm.idu'] + 1/y['msm.idu'] + 1/x['heterosexual'] + 1/y['heterosexual'])
    }
    
    if (sum(is.na(mean.log.ors))>1)
        browser()

    #Return
    list(mean.log.ors = mean.log.ors,
         se.log.ors = se.log.ors)
}

calculate.testing.odds <- function(df.msm, df.idu, df.het)
{
    parsed.msm = parse.testing.demographics(df.msm)
    parsed.idu = parse.testing.demographics(df.idu)
    parsed.het = parse.testing.demographics(df.het)
    x = parsed.msm$x + parsed.idu$x + parsed.het$x
    y = parsed.msm$y + parsed.idu$y + parsed.het$y


    x['idu'] = parsed.idu$x['all']
    y['idu'] = parsed.idu$y['all']

    x['non.idu'] = parsed.het$x['all']
    y['non.idu'] = parsed.het$y['all']
    
    do.calculate.demographic.odds.ratios(x, y)
}

##-----------------------------##
##-- PARSING ODDS FROM FILES --##
##-----------------------------##

parse.linkage.demographics <- function(df, use.3mo=T)
{
    x = integer()
    y = integer()

    x.col = paste0('n.test.', ifelse(use.3mo, '3mo', '1mo'))
    y.col = paste0('n.notest.', ifelse(use.3mo, '3mo', '1mo'))

    x['all'] = df[df$category=='total', x.col]
    y['all'] = df[df$category=='total', y.col]

    x['age1'] = df[df$category=='13-24', x.col]
    y['age1'] = df[df$category=='13-24', y.col]

    x['age2'] = df[df$category=='25-34', x.col]
    y['age2'] = df[df$category=='25-34', y.col]

    x['age3'] = df[df$category=='35-44', x.col]
    y['age3'] = df[df$category=='35-44', y.col]

    x['age4'] = df[df$category=='45-54', x.col]
    y['age4'] = df[df$category=='45-54', y.col]

    x['age5'] = df[df$category=='55+', x.col]
    y['age5'] = df[df$category=='55+', y.col]

    black.mask = grepl('black', df$category, ignore.case = T)
    x['black'] = df[black.mask, x.col]
    y['black'] = df[black.mask, y.col]

    hispanic.mask = grepl('hispanic', df$category, ignore.case = T)
    x['hispanic'] = df[hispanic.mask, x.col]
    y['hispanic'] = df[hispanic.mask, y.col]

    other.mask = grepl('american indian', df$category, ignore.case = T) |
        grepl('alaska native', df$category, ignore.case = T) |
        grepl('asian', df$category, ignore.case = T) |
        grepl('white', df$category, ignore.case=T) |
        grepl('multiple race', df$category, ignore.case = T) |
        grepl('pacific islander', df$category, ignore.case=T)

    x['other'] = sum(df[other.mask, x.col])
    y['other'] = sum(df[other.mask, y.col])

    x['female'] = df[df$category=='het.female', x.col]
    y['female'] = df[df$category=='het.female', y.col]

    x['male'] = df[df$category=='het.male', x.col]
    y['male'] = df[df$category=='het.male', y.col]

    x['msm'] = df[df$category=='msm', x.col]
    y['msm'] = df[df$category=='msm', y.col]

    x['idu'] = df[df$category=='idu.male', x.col] + df[df$category=='idu.female', x.col] + df[df$category=='idu.msm', x.col]
    y['idu'] = df[df$category=='idu.male', y.col] + df[df$category=='idu.female', y.col] + df[df$category=='idu.msm', y.col]

    x['non.idu'] = x['all'] - x['idu']
    y['non.idu'] = y['all'] - y['idu']

    list(x=x, n=x+y, y=y)
}

parse.suppression.demographics <- function(df, denominator=c('total','vl','cd4.or.vl')[1])
{
    x = integer()
    n = integer()

    x.col = 'n.suppressed'
    if (denominator=='vl')
        n.col = 'n.vl'
    else if (denominator=='cd4.or.vl')
        n.col = 'n.cd4.or.vl'
    else
        n.col = 'n'

    x['all'] = df[df$category=='Total', x.col]
    n['all'] = df[df$category=='Total', n.col]

    x['age1'] = df[df$category=='13-24', x.col]
    n['age1'] = df[df$category=='13-24', n.col]

    x['age2'] = df[df$category=='25-34', x.col]
    n['age2'] = df[df$category=='25-34', n.col]

    x['age3'] = df[df$category=='35-44', x.col]
    n['age3'] = df[df$category=='35-44', n.col]

    x['age4'] = df[df$category=='45-54', x.col]
    n['age4'] = df[df$category=='45-54', n.col]

    x['age5'] = df[df$category=='55+', x.col]
    n['age5'] = df[df$category=='55+', n.col]

    black.mask = grepl('black', df$category, ignore.case = T)
    x['black'] = df[black.mask, x.col]
    n['black'] = df[black.mask, n.col]

    hispanic.mask = grepl('hispanic', df$category, ignore.case = T)
    x['hispanic'] = df[hispanic.mask, x.col]
    n['hispanic'] = df[hispanic.mask, n.col]

    other.mask = grepl('american indian', df$category, ignore.case = T) |
        grepl('alaska native', df$category, ignore.case = T) |
        grepl('asian', df$category, ignore.case = T) |
        grepl('white', df$category, ignore.case=T) |
        grepl('multiple race', df$category, ignore.case = T) |
        grepl('pacific islander', df$category, ignore.case=T)

    x['other'] = sum(df[other.mask, x.col])
    n['other'] = sum(df[other.mask, n.col])

    x['female'] = df[df$category=='het.female', x.col]
    n['female'] = df[df$category=='het.female', n.col]

    x['male'] = df[df$category=='het.male', x.col]
    n['male'] = df[df$category=='het.male', n.col]

    x['msm'] = df[df$category=='msm', x.col]
    n['msm'] = df[df$category=='msm', n.col]

    x['idu'] = df[df$category=='idu.male', x.col] + df[df$category=='idu.female', x.col] + df[df$category=='idu.msm', x.col]
    n['idu'] = df[df$category=='idu.male', n.col] + df[df$category=='idu.female', n.col] + df[df$category=='idu.msm', n.col]

    x['non.idu'] = x['all'] - x['idu']
    n['non.idu'] = n['all'] - n['idu']

    list(x=x, n=n, y=n-x)
}

parse.testing.demographics <- function(df)
{
    x = integer()
    n = integer()

    x['all'] = df$n.12mo[df$category=='Total']
    n['all'] = df$n.total[df$category=='Total']

    if (any(df$category=='18-24'))
    {
        x['age1'] = df$n.12mo[df$category=='18-24']
        n['age1'] = df$n.total[df$category=='18-24']
    }
    else
    {
        x['age1'] = df$n.12mo[df$category=='18-19'] + df$n.12mo[df$category=='20-24']
        n['age1'] = df$n.total[df$category=='18-19'] + df$n.total[df$category=='20-24']
    }

    x['age2'] = df$n.12mo[df$category=='25-29']
    n['age2'] = df$n.total[df$category=='25-29']

    x['age3'] = df$n.12mo[df$category=='30-39']
    n['age3'] = df$n.total[df$category=='30-39']

    x['age4'] = df$n.12mo[df$category=='40-49']
    n['age4'] = df$n.total[df$category=='40-49']

    x['age5'] = df$n.12mo[df$category=='50+']
    n['age5'] = df$n.total[df$category=='50+']

    black.mask = grepl('black', df$category, ignore.case = T)
    x['black'] = df$n.12mo[black.mask]
    n['black'] = df$n.total[black.mask]

    hispanic.mask = grepl('hispanic', df$category, ignore.case = T)
    x['hispanic'] = df$n.12mo[hispanic.mask]
    n['hispanic'] = df$n.total[hispanic.mask]

    other.mask = grepl('american indian', df$category, ignore.case = T) |
        grepl('alaska native', df$category, ignore.case = T) |
        grepl('asian', df$category, ignore.case = T) |
        grepl('white', df$category, ignore.case=T) |
        grepl('multiple race', df$category, ignore.case = T) |
        grepl('pacific islander', df$category, ignore.case=T)
    x['other'] = sum(df$n.12mo[other.mask])
    n['other'] = sum(df$n.total[other.mask])

    if (any(names(df)=='n.12mo.female'))
    {
        x['female'] = df$n.12mo.female[df$category=='Total']
        n['female'] = df$n.total.female[df$category=='Total']

        x['male'] = df$n.12mo.male[df$category=='Total']
        n['male'] = df$n.total.male[df$category=='Total']

        x['msm'] = n['msm'] = 0
    }
    else if (any(tolower(df$category)=='female'))
    {
        x['female'] = df$n.12mo[tolower(df$category)=='female']
        n['female'] = df$n.total[tolower(df$category)=='female']

        x['male'] = df$n.12mo[tolower(df$category)=='male']
        n['male'] = df$n.total[tolower(df$category)=='male']

        x['msm'] = n['msm'] = 0
    }
    else
    {
        x['female'] = x['male'] = n['female'] = n['male'] = 0

        x['msm'] = df$n.12mo[df$category=='Total']
        n['msm'] = df$n.total[df$category=='Total']
    }

    list(x=x, n=n, y=n-x)
}

parse.testing.odds.by.msa <- function(df)
{
    first.msa.index = (1:dim(df)[1])[is.na(df$n.total)][sum(is.na(df$n.total))]+1
    last.msa.index = dim(df)[1]-1

    df = df[first.msa.index:last.msa.index,]
    df$category = gsub('â€“', '-', df$category)
    df = df[!grepl('Nassau', df$category),] #we won't bother trying to map this sub-MSA

    cbsas = cbsa.for.msa.name(df$category)
    missing = sapply(cbsas, function(x){is.null(x) || is.na(x)})
    if (any(missing))
        stop(paste0("Unable to match the following MSA names: ",
                    paste0(paste0("'", df$category[missing], "'"),
                           collapse=', ')))

    x = df$n.12mo
    y = df$n.total - x

    list(cbsa=cbsas,
         x = x,
         y = y,
         mean.log.odds = log(x) - log(y),
         se.log.odds = sqrt(1/x + 1/y))
}


##--------------------------------------##
##-- HELPERS for ODDS and ODDS RATIOS --##
##--------------------------------------##

make.odds.ratio.matrix <- function(black.or,
                                   hispanic.or,
                                   age1.or,
                                   age2.or,
                                   age4.or,
                                   age5.or,
                                   msm.or,
                                   female.or,
                                   idu.or,
                                   msm.idu.or,
                                   ages=c('13-24 years','25-34 years','35-44 years','45-54 years','55+ years'),
                                   races=c('black','hispanic','other'),
                                   sexes=c('heterosexual_male','msm','female'),
                                   idu.risk.strata=c('active_IDU','IDU_in_remission'),
                                   risks=c('never_IDU', idu.risk.strata),
                                   use.msm.or.for.msm.idu = T,
                                   stratified.by.risk=!is.na(msm.idu.or))
{
    age.or = c(age1.or, age2.or, 1, age4.or, age5.or)
    names(age.or) = ages
    
    non.idu = setdiff(risks, idu.risk.strata)[1]
    non.msm = setdiff(sexes, 'msm')
    
    if (stratified.by.risk)
    {
        race.or = c(black=as.numeric(black.or), hispanic=as.numeric(hispanic.or), other=1)[races]
        
        sex.or = c(heterosexual_male=1, msm=1, female=as.numeric(female.or))[sexes]
        
        risk.or = sapply(risks, function(r){1})
        
        dim.names = list(age=ages, race=races, sex=sexes, risk=risks)
        rv = outer(outer(outer(age.or, race.or), sex.or), risk.or)
        dim(rv) = sapply(dim.names, length)
        dimnames(rv) = dim.names
        
        rv[,,'msm', non.idu] = rv[,,'msm',non.idu] * msm.or
        rv[,,non.msm, idu.risk.strata] = rv[,,non.msm, idu.risk.strata] * idu.or
        rv[,,'msm', idu.risk.strata] = rv[,,'msm',idu.risk.strata] * msm.idu.or
    }
    else
    {
        race.or = c(black=as.numeric(black.or), hispanic=as.numeric(hispanic.or), other=1)[races]
    
        sex.or = c(heterosexual_male=1, msm=as.numeric(msm.or), female=as.numeric(female.or))[sexes]
    
        risk.or = sapply(risks, function(r){1})
        risk.or[idu.risk.strata] = idu.or
    
        dim.names = list(age=ages, race=races, sex=sexes, risk=risks)
        rv = outer(outer(outer(age.or, race.or), sex.or), risk.or)
        dim(rv) = sapply(dim.names, length)
        dimnames(rv) = dim.names
    
        if (use.msm.or.for.msm.idu)
        {
            for (idu in idu.risk.strata)
                rv[,,'msm',idu] = rv[,,'msm',non.idu]
        }
    }
    
    rv
}

make.log.or.matrix <- function(black.lor,
                               hispanic.lor,
                               age1.lor,
                               age2.lor,
                               age4.lor,
                               age5.lor,
                               msm.lor,
                               female.lor,
                               idu.lor,
                               msm.idu.lor,
                               heterosexual.lor=0,
                               ages=c('13-24 years','25-34 years','35-44 years','45-54 years','55+ years'),
                               races=c('black','hispanic','other'),
                               sexes=c('heterosexual_male','msm','female'),
                               idu.risk.strata=c('active_IDU','IDU_in_remission'),
                               risks=c('never_IDU', idu.risk.strata),
                               use.msm.lor.for.msm.idu = T,
                               stratified.by.risk=!is.na(msm.idu.lor))
{
    age.lor = c(age1.lor, age2.lor, 0, age4.lor, age5.lor)
    names(age.lor) = ages

    race.lor = c(black=as.numeric(black.lor), hispanic=as.numeric(hispanic.lor), other=0)[races]

    if (stratified.by.risk)
    {
        sex.lor = c(heterosexual_male=as.numeric(heterosexual.lor),
                    msm=as.numeric(heterosexual.lor),
                    female=as.numeric(heterosexual.lor + female.lor))[sexes]
        
        risk.lor = sapply(risks, function(r){0})
    }
    else
    {
        sex.lor = c(heterosexual_male=as.numeric(heterosexual.lor),
                    msm=as.numeric(msm.lor),
                    female=as.numeric(heterosexual.lor + female.lor))[sexes]
    
        risk.lor = sapply(risks, function(r){0})
        risk.lor[idu.risk.strata] = idu.lor
    }
    
    dim.names = list(age=ages, race=races, sex=sexes, risk=risks)
    rv = sapply(risks, function(risk){
            sapply(sexes, function(sex){
                sapply(races, function(race){
                    sapply(ages, function(age){
                        age.lor[age] + race.lor[race] + sex.lor[sex] + risk.lor[risk]
                    })
                })
            })
        })
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names

    if (stratified.by.risk)
    {
        non.idu = setdiff(risks, idu.risk.strata)
        non.msm = setdiff(sexes, 'msm')
        
        rv[,,'msm', non.idu] = rv[,,'msm',non.idu] + msm.lor
        rv[,,non.msm, idu.risk.strata] = rv[,,non.msm, idu.risk.strata] + idu.lor
        rv[,,'msm', idu.risk.strata] = rv[,,'msm',idu.risk.strata] + msm.idu.lor
    }
    else if (use.msm.lor.for.msm.idu)
    {
        non.idu = setdiff(risks, idu.risk.strata)[1]
        for (idu in idu.risk.strata)
            rv[,,'msm',idu] = rv[,,'msm',non.idu]
    }

    rv
}

scale.odds.matrix.to.total.odds <- function(or.mat,
                                            total.odds,
                                            population)
{
    total.risk = total.odds / (1+total.odds)
    fn = function(this.base.risk)
    {
        this.base.odds = this.base.risk / (1-this.base.risk)
        this.total.risk = sum(population * this.base.odds * or.mat / (1 + this.base.odds * or.mat)) / sum(population)

        (total.risk - this.total.risk)^2
    }

    base.risk = optimize(f=fn, interval=c(0,1))$minimum

    base.odds = base.risk / (1-base.risk)

    or.mat * base.odds
}


##------------------##
##-- FILE HELPERS --##
##------------------##

read.continuum.file <- function(file)
{
    df = read.csv(file, stringsAsFactors = F)
    for (j in 2:dim(df)[2])
    {
        if (class(df[,j])=='character')
        {
            df[,j] = gsub(',','',df[,j])
            df[df[,j]=='',j] = NA
            df[,j] = as.numeric(df[,j])
        }
    }

    df$category = gsub('â€“', '-', df$category)

    df
}


##----------------------------##
##--    LONG FUNCTION TO    --##
##-- SET SITE-SPECIFIC DATA --##
##----------------------------##


set.site.specific.data <- function(cm, surv)
{
    # Last page gives links to local health departments
    # https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2017-vol-29.pdf
    
    #-- BALTIMORE --#
    BALTIMORE.MSA = '12580'
    BALTIMORE.YEARS = 2010:2018
    #https://phpa.health.maryland.gov/OIDEOR/CHSE/Pages/statistics.aspx
    #https://phpa.health.maryland.gov/OIDEOR/CHSE/Pages/statistics.aspxhttps://phpa.health.maryland.gov/OIDEOR/CHSE/SiteAssets/Pages/statistics/Baltimore-Metro-HIV-Annual-Epidemiological-Profile-2018a.pdf
    #https://phpa.health.maryland.gov/OIDEOR/CHSE/SiteAssets/Pages/statistics/Baltimore-Metro-HIV-Annual-Epidemiological-Profile-2018a.pdf
    cm$linkage = do.set.total.odds.distribution.from.prob(cm$linkage,
                                                          probs=c(54,59,63,68,74,80,80,84,82)/100,
                                                          n=c(922,711,676,615,583,539,523,432,445),
                                                          msa.code = BALTIMORE.MSA,
                                                          year = BALTIMORE.YEARS,
                                                          months=1)
    BALTIMORE.PREVALENCE = c(16151,16482,16768,17008,17203,17367,17482,17546,17845)
    cm$suppression = do.set.total.odds.distribution.from.prob(cm$suppression,
                                                              probs=c(35,27,37,44,51,53,55,61,68)/100,
                                                              n=BALTIMORE.PREVALENCE,
                                                              msa.code = BALTIMORE.MSA,
                                                              year = BALTIMORE.YEARS,
                                                              months=12)
    cm$retention = do.set.total.odds.distribution.from.prob(cm$retention,
                                                            probs=c(60,54,68,73,72,72,71,76,81)/100,
                                                            n=BALTIMORE.PREVALENCE,
                                                            msa.code = BALTIMORE.MSA,
                                                            year = BALTIMORE.YEARS,
                                                            months=12)
    
    #-- WASHINGTON, DC --#
    DC.MSA = cbsa.for.msa.name('Washington, DC')
    DC.YEARS = 2010:2018
    #https://phpa.health.maryland.gov/OIDEOR/CHSE/Pages/statistics.aspx
    #https://phpa.health.maryland.gov/OIDEOR/CHSE/SiteAssets/Pages/statistics/Washington-MSA-NHAS-Progress-Table-2018.pdf
    cm$linkage = do.set.total.odds.distribution.from.prob(cm$linkage,
                                                          probs=c(56,48,56,66,70,75,75,85,84)/100,
                                                          n=c(707,611,598,623,595,575,529,537,483),
                                                          msa.code = DC.MSA,
                                                          year = DC.YEARS,
                                                          months=1)
    DC.PREVALENCE = c(8902,9382,9862,10369,10863,11302,11710,12116,12565)
    cm$suppression = do.set.total.odds.distribution.from.prob(cm$suppression,
                                                              probs=c(23,22,42,47,48,54,57,62,67)/100,
                                                              n=DC.PREVALENCE,
                                                              msa.code = DC.MSA,
                                                              year = DC.YEARS,
                                                              months=12)
    cm$retention = do.set.total.odds.distribution.from.prob(cm$retention,
                                                            probs=c(42,38,60,64,64,69,68,74,77)/100,
                                                            n=DC.PREVALENCE,
                                                            msa.code = DC.MSA,
                                                            year = DC.YEARS,
                                                            months=12)
    
    #-- NEW YORK CITY --#
    # https://www1.nyc.gov/site/doh/data/data-sets/hiv-aids-surveillance-and-epidemiology-reports.page
    # 2018 report
    NYC.MSA = '35620'
    
    #New York City numbers from the New York State reports
    #https://www.health.ny.gov/diseases/aids/general/statistics/
    #https://www.health.ny.gov/diseases/aids/general/statistics/cascade_reports/index.htm
    
    
    #Total Suppression
    # 2018 - table C - https://www.health.ny.gov/diseases/aids/general/statistics/cascade_reports/docs/linkage_2018.pdf
    # 2017 - from NYC report - https://www1.nyc.gov/assets/doh/downloads/pdf/dires/hiv-surveillance-annualreport-2017.pdf
     NYC.DIAGNOSIS =         c('2015'=0.93, '2016'=.93, '2017'=.93, '2018'=.93)
    
    NYC.NEW = c('2012'=2552, '2013'=2470, '2014'=2543, '2015'=2350, '2016'=2102, '2017'=2157, '2018'=1862)
    NYC.LINKAGE.30d = c('2014'=0.74, '2015'=0.73, '2016'=0.75, '2017'=0.80, '2018'=0.82)
    NYC.LINKAGE.90d = c('2012'=0.84, '2013'=0.83, '2014'=0.85, '2015'=0.85, '2016'=0.86, '2018'=0.92)
    
    NYC.PREVALENCE = c('2012'=100457, '2013'=87830, '2014'=86563, '2015'=85182, '2016'=85774, '2017'=125844, '2018'=82822)
    NYC.RETENTION.anycare = c('2012'=0.65, '2013'=0.78, '2014'=0.81, '2015'=0.81, '2016'=0.82, '2017'=0.86, '2018'=0.86)
    NYC.RETENTION.2tests = c('2012'=0.57, '2013'=0.68, '2014'=0.69, '2015'=0.67, '2016'=0.67, '2018'=0.71)
    
    NYC.SUPPRESSION = c('2012'=0.51, '2013'=0.62, '2014'=0.68, '2015'=0.67, '2016'=0.70, '2017'=0.74, '2018'=0.76)

    cm$linkage = do.set.total.odds.distribution.from.prob(cm$linkage,
                                                          probs=NYC.LINKAGE.90d,
                                                          n=NYC.NEW[names(NYC.LINKAGE.90d)],
                                                          msa.code = NYC.MSA,
                                                          year = names(NYC.LINKAGE.90d),
                                                          months=3)
    cm$suppression = do.set.total.odds.distribution.from.prob(cm$suppression,
                                                              probs=NYC.SUPPRESSION,
                                                              n=NYC.PREVALENCE[names(NYC.SUPPRESSION)],
                                                              msa.code = NYC.MSA,
                                                              year = names(NYC.SUPPRESSION),
                                                              months=12)
    cm$retention = do.set.total.odds.distribution.from.prob(cm$retention,
                                                            probs=NYC.RETENTION.2tests,
                                                            n=NYC.PREVALENCE[names(NYC.RETENTION.2tests)],
                                                            msa.code = NYC.MSA,
                                                            year = names(NYC.RETENTION.2tests),
                                                            months=12)
    
    #2018 - from NYC Data of those in care who are suppressed
    #       this makes the assumption everyone is equally likely to be in care
    cm$suppression = do.set.or.distributions.from.probs(cm$suppression,
                                                              year=2018,
                                                              probs=c(male=.87,
                                                                      female=.85,
                                                                      black=.83,
                                                                      hispanic=.87,
                                                                      other=.93,
                                                                      age1=(396*.74+8823*.78)/(296+8823),
                                                                      age2=(8823*.78+19726*.83)/(8823+19726),
                                                                      age3=(19726*.83+24749*.86)/(19726+24749),
                                                                      age4=(24749*.86+40750*.88)/(24749+40750),
                                                                      age5=(40750*.88+32774*.92)/(40750+32774),
                                                                      msm=0.89,
                                                                      idu=0.84,
                                                                      msm.idu=0.81,
                                                                      heterosexual=0.86
                                                              ),
                                                              ns=c(male=92044,
                                                                   female=33339,
                                                                   black=55345,
                                                                   hispanic=41908,
                                                                   other=26021,
                                                                   age1=396+.5*8823,
                                                                   age2=(8823+19726)/2,
                                                                   age3=(19726+24749)/2,
                                                                   age4=(24749+40750)/2,
                                                                   age5=.5*40750+32774,
                                                                   msm=52944,
                                                                   idu=14826,
                                                                   msm.idu=3142,
                                                                   heterosexual=24945),
                                                              msa.code = NYC.MSA)
    
    #2017 - from NYC Data of those in care who are suppressed
    #       this makes the assumption everyone is equally likely to be in care
    cm$suppression = do.set.or.distributions.from.probs(cm$suppression,
                                                        year=2017,
                                                        probs=c(male=.86,
                                                                female=.82,
                                                                black=.84*.90, #from figure 15.1
                                                                hispanic=.88*.92,
                                                                other=.89*.99,
                                                                age1=.71,
                                                                age2=.78,
                                                                age3=.83,
                                                                age4=.85,
                                                                age5=.88,
                                                                msm=0.88,
                                                                idu=0.80,
                                                                msm.idu=0.79,
                                                                heterosexual=0.83
                                                        ),
                                                        ns=c(male=91093,
                                                             female=33358,
                                                             black=54894,
                                                             hispanic=41284,
                                                             other=25898,
                                                             age1=446+.5*9277,
                                                             age2=(9277+19046)/2,
                                                             age3=(19046+25977)/2,
                                                             age4=(25977+40929)/2,
                                                             age5=.5*40929+30126,
                                                             msm=51591,
                                                             idu=15265,
                                                             msm.idu=3059,
                                                             heterosexual=24747),
                                                        msa.code = NYC.MSA)
    
    #2016 - from NYC Data of those in care who are suppressed
    #       this makes the assumption everyone is equally likely to be in care
    cm$suppression = do.set.or.distributions.from.probs(cm$suppression,
                                                        year=2016,
                                                        probs=c(male=.85,
                                                                female=.82,
                                                                black=.81, #from figure 15.1
                                                                hispanic=.85,
                                                                other=.92,
                                                                age1=.70,
                                                                age2=.78,
                                                                age3=.82,
                                                                age4=.85,
                                                                age5=.89,
                                                                msm=0.87,
                                                                idu=0.81,
                                                                msm.idu=0.81,
                                                                heterosexual=0.83
                                                        ),
                                                        ns=c(male=89397,
                                                             female=33253,
                                                             black=54302,
                                                             hispanic=40375,
                                                             other=25635,
                                                             age1=519+.5*9483,
                                                             age2=(9483+18308)/2,
                                                             age3=(18308+27462)/2,
                                                             age4=(27462+40696)/2,
                                                             age5=.5*40696+27322,
                                                             msm=49357,
                                                             idu=15594,
                                                             msm.idu=2777,
                                                             heterosexual=24383),
                                                        msa.code = NYC.MSA)

    #2015 - from NYC Data of those in care who are suppressed
    #       this makes the assumption everyone is equally likely to be in care
    cm$suppression = do.set.or.distributions.from.probs(cm$suppression,
                                                        year=2015,
                                                        probs=c(male=.84,
                                                                female=.81,
                                                                black=.79, #from figure 15.1
                                                                hispanic=.84,
                                                                other=.91,
                                                                age1=.67,
                                                                age2=.76,
                                                                age3=.81,
                                                                age4=.84,
                                                                age5=.88,
                                                                msm=0.86,
                                                                idu=0.80,
                                                                msm.idu=0.78,
                                                                heterosexual=0.82
                                                        ),
                                                        ns=c(male=87493,
                                                             female=33027,
                                                             black=53462,
                                                             hispanic=39590,
                                                             other=25198,
                                                             age1=660+.5*9656,
                                                             age2=(9656+17647)/2,
                                                             age3=(17647+29145)/2,
                                                             age4=(29145+29877)/2,
                                                             age5=.5*39877+24514,
                                                             msm=47432,
                                                             idu=15918,
                                                             msm.idu=2651,
                                                             heterosexual=24002),
                                                        msa.code = NYC.MSA)
    
    #2014 - from NYC Data of those in care who are suppressed
    #       this makes the assumption everyone is equally likely to be in care
    cm$suppression = do.set.or.distributions.from.probs(cm$suppression,
                                                        year=2014,
                                                        probs=c(male=.82,
                                                                female=.79,
                                                                black=.77, #from figure 15.1
                                                                hispanic=.82,
                                                                other=.89,
                                                                age1=.64,
                                                                age2=.73,
                                                                age3=.79,
                                                                age4=.82,
                                                                age5=.87,
                                                                msm=0.84,
                                                                idu=0.78,
                                                                msm.idu=0.77,
                                                                heterosexual=0.79
                                                        ),
                                                        ns=c(male=86651,
                                                             female=32899,
                                                             black=52792,
                                                             hispanic=38795,
                                                             other=24775,
                                                             age1=802+.5*9488,
                                                             age2=(9488+17177)/2,
                                                             age3=(17177+31084)/2,
                                                             age4=(31084+38727)/2,
                                                             age5=.5*38727+22130,
                                                             msm=46153,
                                                             idu=16191,
                                                             msm.idu=2635,
                                                             heterosexual=23665),
                                                        msa.code = NYC.MSA)
    
    #2013 - from NYC Data of those in care who are suppressed
    #       this makes the assumption everyone is equally likely to be in care
    cm$suppression = do.set.or.distributions.from.probs(cm$suppression,
                                                        year=2013,
                                                        probs=c(male=.79,
                                                                female=.75,
                                                                black=.74,
                                                                hispanic=.78,
                                                                other=.86,
                                                                age1=.57,
                                                                age2=.70,
                                                                age3=.75,
                                                                age4=.79,
                                                                age5=.84,
                                                                msm=0.81,
                                                                idu=0.75,
                                                                msm.idu=0.73,
                                                                heterosexual=0.76
                                                        ),
                                                        ns=c(male=84848,
                                                             female=32770,
                                                             black=52186,
                                                             hispanic=38063,
                                                             other=24376,
                                                             age1=931+.5*9294,
                                                             age2=(9294+16721)/2,
                                                             age3=(16821+33119)/2,
                                                             age4=(33119+37390)/2,
                                                             age5=.5*37390+19900,
                                                             msm=43940,
                                                             idu=16649,
                                                             msm.idu=2522,
                                                             heterosexual=23192),
                                                        msa.code = NYC.MSA)
    
    #-- RETURN --#
    
    cm
}