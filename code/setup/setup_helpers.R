#ource('../code/data_managers/census_manager.R')

##----------------------##
##-- COLLAPSING RACES --##
##----------------------##

collapse.races <- function(arr)
{
    dim.names = dimnames(arr)
    dim.names[['race']] = BLACK.HISPANIC.OTHER

    rv = array(NA, dim = sapply(dim.names, length), dimnames = dim.names)
    access(rv, race='black') = access(arr, race='black')
    access(rv, race='hispanic') = access(arr, race='hispanic')
    access(rv, race='other') = access(arr, race='white') + access(arr, race='asian') + access(arr, race='american_indian_or_alaska_native')

    rv
}

collapse.races.for.rates <- function(pop, rates)
{
    numerators = collapse.races(pop * rates)
    denominators = collapse.races(pop)
    numerators / denominators
}


##----------##
##-- PREP --##
##----------##

#sets both suscceptibility and new infection proportions accordingly
set.prep.coverage <- function(jheem, prep.coverage, year, settings, parameters)
{
    non.prep.risk = (1-prep.coverage)
    prep.risk = prep.coverage * parameters['prep.rr.heterosexual']
    access(prep.risk, sex='msm') = access(prep.coverage, sex='msm') * parameters['prep.rr.msm']

    susceptibility = non.prep.risk + prep.risk

    new.infection.proportions = get.new.infection.proportions.skeleton(jheem)# get.uniform.new.infection.proportions(jheem, initial.continuum = settings$UNDIAGNOSED_STATES) * 2

    access(new.infection.proportions, continuum = settings$UNDIAGNOSED_NO_PREP, cd4 = settings$CD4_STRATA[1]) =
        non.prep.risk / (prep.risk + non.prep.risk)
    access(new.infection.proportions, continuum = settings$UNDIAGNOSED_FROM_PREP, cd4 = settings$CD4_STRATA[1]) =
        prep.risk / (prep.risk + non.prep.risk)

    jheem = set.susceptibility(jheem, susceptibility, time=year+1)
    jheem = set.new.infection.proportions(jheem, new.infection.proportions, time=year+1)

    jheem
}


##---------------------------##
##-- SMOOTHING PROPORTIONS --##
##---------------------------##

smooth.proportions <- function(proportions,
                               max.proportion=1,
                               desired.years,
                               min.slope=0,
                               min.proportion=0,
                               model.on.log.odds=T)
{
    if (any(proportions>max.proportion))
        stop("A given proportion is greater than the max.proportion")

    n.dims = length(dim(proportions))
    year.dim = (1:n.dims)[names(dim(proportions))=='year']
    new.dim.names = dimnames(proportions)
    new.dim.names[['year']] = as.character(desired.years)
    year = as.integer(dimnames(proportions)[['year']])
    anchor.year = min(year)
    year = year-anchor.year

    proportions = apply(proportions, 1:length(dim(proportions)), function(x){min(max.proportion, max(min.proportion, x))})
    proportions = (proportions - min.proportion) / (max.proportion - min.proportion)

    rv = apply(proportions, (1:n.dims)[-year.dim], function(p){
        scaled.p = p / max.proportion
        if (model.on.log.odds)
        {
            scaled.log.odds = log(scaled.p) - log(1-scaled.p)
            fit = lm(scaled.log.odds ~ year)
            intercept = fit$coefficients[1]
            slope = max(min.slope, fit$coefficients[2])
            pred.log.odds = intercept + slope * (desired.years - anchor.year)
            1 / (1+exp(-pred.log.odds))
        }
        else
        {
            scaled.odds = scaled.p / (1-scaled.p)
            fit = lm(scaled.odds ~ year)
            intercept = fit$coefficients[1]
            slope = max(min.slope, fit$coefficients[2])
            pred.odds = intercept + slope * (desired.years - anchor.year)
            pred.odds / (1+pred.odds)
        }
    })

    rv = rv * (max.proportion - min.proportion) + min.proportion
    rv = apply(rv, 1:length(dim(rv)), function(x){min(max.proportion, max(min.proportion, x))})


    if (year.dim>1)
        rv = apply(rv, c(2:year.dim, 1, (year.dim+1):n.dims), function(x){x})

    dim(rv) = sapply(new.dim.names, length)
    dimnames(rv) = new.dim.names

    rv
}

compare.preds.to.smoothed <- function(preds, actual)
{
    df.preds = melt(preds)
    non.year.names = setdiff(names(df.preds), c('year','value'))
    if (length(non.year.names)>1)
        df.preds$stratum = apply(df.preds[,non.year.names], 1, function(z){paste0(z,collapse='-')})
    else if (length(non.year.names)==1)
        df.preds$stratum = df.preds[,non.year.names]
    else
        df.preds$stratum = 'all'
    df.preds$type = 'smoothed'

    df.actual = melt(actual)
    non.year.names = setdiff(names(df.actual), c('year','value'))
    if (length(non.year.names)>1)
        df.actual$stratum = apply(df.actual[,non.year.names], 1, function(z){paste0(z,collapse='-')})
    else if (length(non.year.names)==1)
        df.actual$stratum = df.actual[,non.year.names]
    else
        df.actual$stratum = 'all'
    df.actual$type = 'actual'

    df = rbind(df.preds[,c('year','value','stratum','type')],
               df.actual[,c('year','value','stratum','type')])

    ggplot(df, aes(year, value, color=type)) +
        geom_line(size=1) + geom_point(size=4) +
        facet_wrap(~stratum) + ylim(0,1)
}

##---------------------##
##-- MSM PROPORTIONS --##
##---------------------##

get.best.guess.msm.proportion <- function(fips,
                                          census,
                                          years)
{
#    props = read.msm.proportions()[as.character(fips)]
    props = ALL.DATA.MANAGERS$msm.proportions[as.character(fips)]
    
    males = get.census.data(census, years=years, fips=fips,
                            sexes = 'male',
                            aggregate.ages = T,
                            aggregate.years = T,
                            aggregate.races = T,
                            aggregate.sexes = T)

    sum(props*males) / sum(males)
}

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4516312/
MSM.PROPORTIONS = c(black=1-.806, hispanic=1-.854, other=1-.848)

# Assumes that within each county, relative risks of being MSM are as in MSM.PROPORTIONS
# and total risk of being MSM is as per read.msm.proportions
get.best.guess.msm.proportions.by.race <- function(fips,
                                          census,
                                          years)
{
#    props = read.msm.proportions()[as.character(fips)]
    props = ALL.DATA.MANAGERS$msm.proportions[as.character(fips)]
    males.raw = get.census.data(census, years=years, fips=fips,
                                sexes = 'male',
                                aggregate.ages = T,
                                aggregate.years = T,
                                aggregate.races = F,
                                aggregate.sexes = T)
    
    if (length(fips)==1)
        males = matrix(c(black=males.raw[,'black'],
                         hispanic=males.raw[,'hispanic'],
                         other=sum(males.raw[,setdiff(dimnames(males.raw)[[2]], c('black','hispanic'))])),
                       nrow=1)
    else
        males = cbind(black=males.raw[,'black'],
                      hispanic=males.raw[,'hispanic'],
                      other=rowSums(males.raw[,setdiff(dimnames(males.raw)[[2]], c('black','hispanic'))]))
    
    
    
    numerators = t(sapply(1:length(fips), function(i){
        p = props[i]
        pop = males[i,]
        p.mult = p * sum(pop) / sum(pop * MSM.PROPORTIONS)
        p.mult * MSM.PROPORTIONS * pop
    }))
    
    rv = colSums(numerators) / colSums(males)
    names(rv) = c('black','hispanic','other')

    rv
}

#does not take into account race differences in MSM prevalence
ORIG.get.best.guess.msm.proportions.by.race <- function(fips,
                                                   census,
                                                   years)
{
#    props = read.msm.proportions()[as.character(fips)]
    props = ALL.DATA.MANAGERS$msm.proportions[as.character(fips)]
    
    if (length(fips)==1)
        c(black=as.numeric(props), hispanic=as.numeric(props), other=as.numeric(props))
    else
    {
        males.raw = get.census.data(census, years=years, fips=fips,
                                    sexes = 'male',
                                    aggregate.ages = T,
                                    aggregate.years = T,
                                    aggregate.races = F,
                                    aggregate.sexes = T)
        
        males = cbind(black=males.raw[,'black'],
                      hispanic=males.raw[,'hispanic'],
                      other=rowSums(males.raw[,setdiff(dimnames(males.raw)[[2]], c('black','hispanic'))]))
        
        colSums(props*males) / colSums(males)
    }
}

stratify.males.to.msm.by.race <- function(population,
                                          msm.proportions)
{
#    msm.proportions = c(black=black.msm.proportion,
#                        hispanic=hispanic.msm.proportion,
#                        other=other.msm.proportion)

    dim.names = dimnames(population)
    dim.names[['sex']] = c('heterosexual_male','msm','female')
    rv = array(NA, dim=sapply(dim.names,length), dimnames=dim.names)

    access(rv, sex='female') = access(population, sex='female')
    for (race in names(msm.proportions))
    {
        access(rv, sex='msm', race=race) = msm.proportions[race] *
            access(population, sex='male', race=race)
        access(rv, sex='heterosexual_male', race=race) = (1-msm.proportions[race]) *
            access(population, sex='male', race=race)
    }

    rv
}

stratify.population.idu <- function(population,
                                    active.idu.prevalence,
                                    idu.ever.prevalence)
{
    dim.names.minus.risk = dimnames(population)
    dim.name.names.before.risk = intersect(names(dim.names.minus.risk), c('age','race','subpopulation','sex'))
    dim.name.names.after.risk = setdiff(names(dim.names.minus.risk), dim.name.names.before.risk)
    dim.names = c(dim.names.minus.risk[dim.name.names.before.risk],
                  list(risk=c('never_IDU', 'active_IDU', 'IDU_in_remission')),
                  dim.names.minus.risk[dim.name.names.after.risk])

    pre.idu.population = population
    population = array(0, dim=sapply(dim.names, length), dimnames=dim.names)

    pre.idu.population = expand.population(pre.idu.population, target.dim.names = dim.names.minus.risk)

    access(population, risk='never_IDU') = pre.idu.population *
        expand.population(1-idu.ever.prevalence, target.dim.names = dim.names.minus.risk)
    access(population, risk='active_IDU') = pre.idu.population *
        expand.population(active.idu.prevalence, target.dim.names = dim.names.minus.risk)
    access(population, risk='IDU_in_remission') = pre.idu.population *
        expand.population(idu.ever.prevalence-active.idu.prevalence, target.dim.names = dim.names.minus.risk)

    population
}

recategorize.to.cdc.risk.strata <- function(population)
{
    CDC.RISK = c('msm','idu', 'msm_idu', 'heterosexual')
    CDC.SEX = c('male','female')

    dim.names = dimnames(population)
    dim.names[['sex']] = CDC.SEX
    dim.names[['risk']] = CDC.RISK

    rv = array(0, dim=sapply(dim.names, length), dimnames=dim.names)

    #pull msm
    access(rv, sex='male', risk='msm') = access(population, sex='msm', risk='never_IDU')

    #pull idu
    access(rv, sex='female',risk='idu') = access(population, sex='female', risk='active_IDU') +
        access(population, sex='female', risk='IDU_in_remission')
    access(rv, sex='male',risk='idu') = access(population, sex='heterosexual_male', risk='active_IDU') +
        access(population, sex='heterosexual_male', risk='IDU_in_remission')

    #pull msm+idu
    access(rv, sex='female',risk='msm_idu') = access(population, sex='msm', risk='active_IDU') +
        access(population, sex='msm', risk='IDU_in_remission')

    #pull heterosexual
    access(rv, sex='female', risk='heterosexual') = access(population, sex='female', risk='never_IDU')
    access(rv, sex='male', risk='heterosexual') = access(population, sex='heterosexual_male', risk='never_IDU')

    #return
    rv
}
