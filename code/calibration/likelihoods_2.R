
#source('../code/data_managers/hiv_surveillance_manager.R')
#source('../code/estimating_errors/estimate_cdc_errors.R')

library(mvtnorm)
library(reshape2)
library(jheem)

##-----------------------------------------------------##
##-----------------------------------------------------##
##-- THE MAIN LIKELIHOOD COMPONENT CREATOR FUNCTIONS --##
##-----------------------------------------------------##
##
## These functions create and return functions to calculate the likelihood
## The returned function takes as input two parameters
##  (1) jheem.result
##  (2) log (optional, default value=T)
## And returns either a likelihood or log-likelihood based on the result
##
##-----------------------------------------------------##
##-----------------------------------------------------##


##----------------------------------------------------##
##--              GENERAL LIKELIHOOD                --##
##--                                                --##
##-- Used for new diagnoses, prevalence, mortality  --##
##-- and preserves correlations for stratifications --##
##----------------------------------------------------##

create.likelihood.function <- function(data.type=c('new','prevalence','mortality')[1],
                                       years=NULL,
                                       surv=msa.surveillance,
                                       location=BALTIMORE.MSA,
                                       by.total=F,
                                       by.sex=F,
                                       by.race=F,
                                       by.age=F,
                                       by.risk=F,
                                       by.sex.age=F,
                                       by.sex.race=F,
                                       by.sex.risk=F,
                                       by.race.risk=F,
                                       ages=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
                                       races=c('black','hispanic','other'),
                                       sexes=c('heterosexual_male','msm','female'),
                                       risks=c('never_IDU','active_IDU','IDU_in_remission'),
                                       sd.inflation=1,
                                       inflate.sd.by.n.obs.per.year=F,
                                       upweight.by.n.obs.per.year=T,
                                       sd.inflation.for.observations=1,
                                       population=get.census.totals(ALL.DATA.MANAGERS$census.totals, location),
                                       use.simulated.denominators=F,
                                       use.sim.msm.proportions=F,
                                       denominator.dimensions='year',
                                       aggregate.denominator.males=F,
                                       msm.cv=0,
                                       idu.cv=0.25,
                                       year.to.year.correlation=0,
                                       numerator.year.to.year.chunk.correlation=0,
                                       numerator.year.to.year.off.correlation=0,
                                       numerator.chunk.years=numeric(),
                                       numerator.sd = function(...){0},
                                       bias.fn = function(...){0},
                                       bias.sd = function(...){0})
{
#    print(denominator.dimensions)

    #If years parameter is null, use all years for which there are data
    data.for.all = get.surveillance.data(surv, location, data.type=data.type)
    available.years = attr(data.for.all, 'years')
    if (is.null(years))
        years = available.years
    else
        years = intersect(years, available.years)

    #Set up the population to use as denominators
#    population = apply(population, c('year','age','race','sex','risk'), sum)[as.character(years),,,,]
    population = access(population, year=as.character(years))
    target.dim.names = list(year=years,
                            age=SETTINGS$AGES$labels,
                            race=SETTINGS$RACES,
                            sex=SETTINGS$SEXES,
                            risk=SETTINGS$RISK_STRATA)#dimnames(population)
    population = apply(population, denominator.dimensions, sum)
    population = expand.population(population, target.dim.names = target.dim.names)
    if ((aggregate.denominator.males || use.sim.msm.proportions) && any(denominator.dimensions=='sex'))
    {
        males = population[,,,'msm',] + population[,,,'heterosexual_male',]
        population[,,,'msm',] = males
        population[,,,'heterosexual_male',] = males
    }

    #Set up denominator elements
    if (all(denominator.dimensions!='risk'))
        idu.cv = 0
    if (all(denominator.dimensions!='sex') || aggregate.denominator.males || use.sim.msm.proportions)
        msm.cv = 0

    fixed.denominator.elements = create.simple.denominator.elements(population,
                                                                    msm.cv = msm.cv, idu.cv = idu.cv)

    population.sums.by.year = apply(population, 'year', sum)

    ##---------------------------------------------##
    ##-- Set up the likelihood elements/indexing --##
    ##---------------------------------------------##

    likelihood.elements = create.likelihood.elements.for.data.type(data.type=data.type, years=years, surv=surv, location=location,
                                                                   by.total=by.total,
                                                                   by.sex=by.sex, by.race=by.race, by.age=by.age, by.risk=by.risk,
                                                                   by.sex.age=by.sex.age, by.sex.race=by.sex.race,
                                                                   by.sex.risk=by.sex.risk, by.race.risk=by.race.risk,
                                                                   ages=ages, races=races, sexes=sexes, risks=risks,
                                                                   numerator.sd = numerator.sd,
                                                                   bias.fn = bias.fn,
                                                                   bias.sd = bias.sd,
                                                                   numerator.year.to.year.chunk.correlation=numerator.year.to.year.chunk.correlation,
                                                                   numerator.year.to.year.off.correlation=numerator.year.to.year.off.correlation,
                                                                   numerator.chunk.years=numerator.chunk.years
    )

    if (is.null(likelihood.elements$numerator.covar.mat))
        numerator.covar.mat = NULL
    else
        numerator.covar.mat = likelihood.elements$numerator.covar.mat * sd.inflation.for.observations^2

    # Set up the correlation matrix, if needed
    if (year.to.year.correlation == 0)
        corr.mat = NULL
    else
        corr.mat = make.compound.symmetry.by.year.matrix(access(population, year=as.character(years)), correlation=year.to.year.correlation)

    ##----------------------------------------------------------------##
    ##-- Set up SD Inflation (if dependent on a data.type or n.obs) --##
    ##----------------------------------------------------------------##

    if (is(sd.inflation, 'character'))
    {
        data.for.all.for.sd.inflation = get.surveillance.data(surv, location, data.type=sd.inflation)
        sd.inflation.years = attr(data.for.all, 'years')
        sd.inflation.elements = create.likelihood.elements.for.data.type(data.type=sd.inflation,
                                                                         years=sd.inflation.years, surv=surv, location=location,
                                                                         by.total=by.total,
                                                                         by.sex=by.sex, by.race=by.race, by.age=by.age, by.risk=by.risk,
                                                                         by.sex.age=by.sex.age, by.sex.race=by.sex.race,
                                                                         by.sex.risk=by.sex.risk, by.race.risk=by.race.risk,
                                                                         ages=ages, races=races, sexes=sexes, risks=risks,
                                                                         numerator.sd = numerator.sd,
                                                                         numerator.year.to.year.chunk.correlation=numerator.year.to.year.chunk.correlation,
                                                                         numerator.year.to.year.off.correlation=numerator.year.to.year.off.correlation,
                                                                         numerator.chunk.years=numerator.chunk.years
                                                                         )

        sd.inflation = get.sd.inflation(match.to.response = likelihood.elements$response.vector,
                                        match.to.descriptions = likelihood.elements$descriptions,
                                        response.2 = sd.inflation.elements$response.vector,
                                        descriptions.2 = sd.inflation.elements$descriptions)

    }
    
    if (inflate.sd.by.n.obs.per.year || upweight.by.n.obs.per.year)
    {
        year = substr(likelihood.elements$descriptions, 1, 4)
        year.counts = table(year)
        
        if (upweight.by.n.obs.per.year)
            divisor = sqrt(max(year.counts))
        else
            divisor = 1
        
        orig.sd.inflation = sd.inflation
        sd.inflation = function(descriptions)
        {
            if (is(orig.sd.inflation, 'function'))
                orig.sd.inflation = orig.sd.inflation(descriptions)
            
            year = substr(descriptions, 1, 4)
            orig.sd.inflation * sqrt(year.counts[year]) / divisor
        }
    }
    
    ##------------------------------------------------------------------------------------##
    ##-- Set up a mask to pull out components we don't use in the transformation matrix --##
    ##------------------------------------------------------------------------------------##

    denominators.are.totals = length(denominator.dimensions)==1 & denominator.dimensions=='year'
    if (is.null(corr.mat) &&
        !use.simulated.denominators &&
        !(use.sim.msm.proportions && !aggregate.denominator.males && any(denominator.dimensions=='sex')))
    {
        transformation.mapping = make.transformation.mapping(likelihood.elements$transformation.matrix)

        likelihood.elements$transformation.matrix = likelihood.elements$transformation.matrix[,transformation.mapping$first.in.signature]
#        fixed.denominator.elements$denominator.vector = fixed.denominator.elements$denominator.vector[transformation.mapping$first.in.signature]

        if (!is.null(fixed.denominator.elements$denominator.covar.mat))
            fixed.denominator.elements$denominator.covar.mat = fixed.denominator.elements$denominator.covar.mat[transformation.mapping$first.in.signature,transformation.mapping$first.in.signature]
    }
    else
        transformation.mapping = NULL

    ##----------------------------------------------------##
    ##-- Make and return the actual likelihood function --##
    ##----------------------------------------------------##

    function(jheem.results, log=T){

        # Check if we terminated early
        if (jheem.results$terminated)
        {
            if (log)
                return (-Inf)
            else
                return (0)
        }

        # Set up the denominators
        if (denominators.are.totals)
            denominator.elements = fixed.denominator.elements
        else if (use.simulated.denominators)
        {
            stop("Need to update for denominator dimensions")
            sim.pop = extract.population.subset(jheem.results, years=years,
                                                per.population=NA,
                                                keep.dimensions = c('year','age','race','sex','risk'))

            sim.pop.sums.by.year = apply(sim.pop, 'year', sum)
            sim.pop = sim.pop * population.sums.by.year / sim.pop.sums.by.year

            denominator.elements = create.simple.denominator.elements(sim.pop,
                                                                      msm.cv = msm.cv, idu.cv = idu.cv)
        }
        else if (use.sim.msm.proportions && !aggregate.denominator.males && any(denominator.dimensions=='sex'))
            denominator.elements = create.simple.denominator.elements.from.msm.proportions(population,
                                                                                           msm.proportions=attr(jheem.results, 'msm.proportions.by.race'),
                                                                                           idu.cv = idu.cv)
        else
            denominator.elements = fixed.denominator.elements


        rates = pull.simulations.rates(jheem.results,
                                       data.type=data.type,
                                       years=years,
                                       denominator.dimensions=denominator.dimensions,
                                       aggregate.denominator.males=aggregate.denominator.males)

        # Pass it all to the sub-function to crunch
        likelihood.sub(rates,
                       transformation.matrix = likelihood.elements$transformation.matrix,
                       response.vector = likelihood.elements$response.vector,
                       denominator.vector = denominator.elements$denominator.vector,
                       denominator.covar.mat = denominator.elements$denominator.covar.mat,
                       numerator.covar.mat = numerator.covar.mat,
                       corr.mat = corr.mat,
                       sd.inflation=sd.inflation,
                       log=log,
                       sim=jheem.results,
                       transformation.mapping=transformation.mapping,
                       description= likelihood.elements$descriptions)
    }
}

##---------------------------##
##-- PROPORTION SUPPRESSED --##
##---------------------------##

create.suppressed.likelihood <- function(location,
                                         years=2010:2018,
                                         surv=msa.surveillance,
                                         numerator.year.to.year.chunk.correlation=0,
                                         numerator.year.to.year.off.correlation=0,
                                         numerator.chunk.years=numeric(),
                                         numerator.sd = function(...){0},
                                         sd.inflation=1,
                                         inflate.sd.by.n.obs.per.year=F,
                                         upweight.by.n.obs.per.year=T,
                                         backup.surv=state.surveillance,
                                         numerator.sd.inflation.if.backup=2,
                                         by.total=T,
                                         by.age=T,
                                         by.race=T,
                                         by.sex=T,
                                         by.risk=T)
{
    total.prevalence = get.surveillance.data(surv, location.codes = location, data.type='prevalence', years=years)
    total.prevalence = unlist(interpolate.parameters(values=total.prevalence[!is.na(total.prevalence)],
                                                     value.times=years[!is.na(total.prevalence)],
                                                     desired.times = years))
    
    if (is.null(get.surveillance.data(msa.surveillance, location, data.type='suppression', throw.error.if.missing.data = F)))
        # then use state level suppression rates and inflate the sd
    {
        if (is(numerator.sd, 'function'))
        {
            orig.numerator.sd = numerator.sd
            numerator.sd = function(...){numerator.sd.inflation.if.backup * orig.numerator.sd(...)}
        }
        else
            numerator.sd = numerator.sd.inflation.if.backup * numerator.sd
        
         surv.for.likelihood.elements = backup.surv
         states = states.for.msa(location)
         if (length(states)==0)
             stop(paste0("No data for location, '", location, "', and it is not an msa within a state"))
         else if (length(states)>1)
             stop(paste0("More than one state for location '", location, "' ('", msa.names(location), "')"))
         location.for.likelihood.elements = states
    }
    else
    {
        surv.for.likelihood.elements = surv
        location.for.likelihood.elements = location
    }
    
    likelihood.elements = create.likelihood.elements.for.data.type('suppression',
                                                                   surv=surv.for.likelihood.elements,
                                                                   location=location.for.likelihood.elements,
                                                                   years=years,
                                                                   by.total=by.total,
                                                                   by.sex=by.sex,
                                                                   by.race=by.race,
                                                                   by.age=by.age,
                                                                   by.risk=by.risk,
                                                                   by.sex.age=F,
                                                                   by.sex.race=F,
                                                                   by.sex.risk=F,
                                                                   by.race.risk=F,
                                                                   numerator.sd = numerator.sd,
                                                                   numerator.year.to.year.chunk.correlation=numerator.year.to.year.chunk.correlation,
                                                                   numerator.year.to.year.off.correlation=numerator.year.to.year.off.correlation,
                                                                   numerator.chunk.years=numerator.chunk.years)

    transformation.mapping = make.transformation.mapping(likelihood.elements$transformation.matrix)
    likelihood.elements$transformation.matrix = likelihood.elements$transformation.matrix[,transformation.mapping$first.in.signature]
    
    if (inflate.sd.by.n.obs.per.year || upweight.by.n.obs.per.year)
    {
        year = substr(likelihood.elements$descriptions, 1, 4)
        year.counts = table(year)
        
        if (upweight.by.n.obs.per.year)
            divisor = sqrt(max(year.counts))
        else
            divisor = 1
        
        orig.sd.inflation = sd.inflation
        sd.inflation = function(descriptions)
        {
            if (is(orig.sd.inflation, 'function'))
                orig.sd.inflation = orig.sd.inflation(descriptions)
            
            year = substr(descriptions, 1, 4)
            orig.sd.inflation * sqrt(year.counts[year]) / divisor
        }
    }
    
    function(sim, log=T)
    {
        # Check if we terminated early
        if (sim$terminated)
        {
            if (log)
                return (-Inf)
            else
                return (0)
        }
        
        
        #Pull rates from the simulation
        rates = as.numeric(extract.suppression(sim,
                                    years=years,
                                    continuum='diagnosed',
                                    keep.dimensions=c('year','age','race','sex','risk')))
        
        #Multiply the denominators (as fraction of total population) by the total population 
        # to get our denominators
        denominators = extract.prevalence(sim,
                                          years=years,
                                          continuum='diagnosed',
                                          keep.dimensions = c('year','age','race','sex','risk'),
                                          per.population=NA)
        denominators = as.numeric(denominators / rowSums(denominators) * total.prevalence)
        
        aggregated.denominators = likelihood.elements$transformation.matrix %*% sum.for.matrix.mapping(denominators, transformation.mapping)
        
        if (is.null(likelihood.elements$numerator.covar.mat))
            numerator.covar.mat = NULL
        else
            numerator.covar.mat = aggregated.denominators %*% t(aggregated.denominators) * likelihood.elements$numerator.covar.mat
        
        # Pass it all to the sub-function to crunch
        likelihood.sub(rates,
                       transformation.matrix = likelihood.elements$transformation.matrix,
                       response.vector = likelihood.elements$response.vector * aggregated.denominators,
                       denominator.vector = denominators,
                       denominator.covar.mat = NULL,
                       numerator.covar.mat = numerator.covar.mat,
                       corr.mat = NULL,
                       sd.inflation=sd.inflation,
                       log=log,
                       sim=sim,
                       transformation.mapping=transformation.mapping,
                       description=likelihood.elements$descriptions)
    }
}

##----------------------------##
##-- AWARENESS OF DIAGNOSIS --##
##----------------------------##

create.knowledge.of.status.likelihood <- function(location,
                                                  surv=msa.surveillance,
                                                  state.surv=state.surveillance,
                                                  census.totals=ALL.DATA.MANAGERS$census.totals,
                                                  knowledge.of.status.regressions=NULL,
                                                  years=2010:2018,
                                                  total.sd.inflation=1,
                                                  stratified.ors.sd.inflation=1,
                                                  total.rho=0.5,
                                                  total.numerator.sd=function(...){0},
                                                  total.sd.multiplier.if.state=3)
{
    if (is.null(knowledge.of.status.regressions))
        load('cached/knowledge_of_status_regressions.Rdata')
    
    #-- Set up for totals --#
    total.suppression = get.surveillance.data(surv, location.codes = location, data.type='diagnosed', 
                                              years=years, throw.error.if.missing.data = F)
    if (is.null(total.suppression))
    {
        total.suppression = get.state.averaged.knowledge.of.status(msa=location,
                                               years=years,
                                               census.totals=census.totals,
                                               state.surveillance = state.surv)
        sd.mult = total.sd.multiplier.if.state
    }
    else
        sd.mult = 1

    if (is.null(years))
        total.years = as.numeric(names(total.suppression))
    else
        total.years = years
    total.years = total.years[!is.na(total.suppression)]    
    total.suppression = total.suppression[!is.na(total.suppression)]
    
    total.sds = total.numerator.sd(total.suppression) * sd.mult
    if (length(total.sds)==1)
        total.sds = rep(total.sds, length(total.suppression))
    
    diagnosed.prevalence.for.total = get.surveillance.data(surv, location.codes = location, data.type='prevalence',
                                                           years=total.years)

    diagnosed.prevalence.for.total = unlist(interpolate.parameters(values=diagnosed.prevalence.for.total[!is.na(diagnosed.prevalence.for.total)],
                                                             value.times=total.years[!is.na(diagnosed.prevalence.for.total)],
                                                             desired.times = total.years))

    n.total = diagnosed.prevalence.for.total / total.suppression
    
    #-- Set up our 'years' argument (to include only years for which we have data) --# 
    if (is.null(years))
    {
        age.race.sex.msm.years = knowledge.of.status.regressions$age.race.sex.msm.years
        race.sex.idu.years = knowledge.of.status.regressions$race.sex.idu.years
    }
    else
    {
        age.race.sex.msm.years = intersect(years, knowledge.of.status.regressions$age.race.sex.msm.years)
        race.sex.idu.years = intersect(years, knowledge.of.status.regressions$race.sex.idu.years)
    }
    
    #-- Pull total prevalence for each year --#
    years = sort(union(age.race.sex.msm.years, race.sex.idu.years))
    
    total.prevalence.for.ors = get.surveillance.data(surv, location.codes = location, data.type='prevalence', years=years)
    total.prevalence.for.ors = unlist(interpolate.parameters(values=total.prevalence.for.ors[!is.na(total.prevalence.for.ors)],
                                                     value.times=years[!is.na(total.prevalence.for.ors)],
                                                     desired.times = years))
    
    function(sim, log=T, verbose=F)
    {
        #-- Pull the Prevalent Strata Sizes --#
        #Multiply the denominators (as fraction of total population) by the total population 
        # to get our denominators
        denominators = extract.prevalence(sim,
                                          years=years,
                                          continuum='diagnosed',
                                          keep.dimensions = c('year','age','race','sex','risk'),
                                          per.population=NA)
        denominators = as.numeric(denominators / rowSums(denominators) * total.prevalence.for.ors)
        
        #-- Set up a data frame that we're going to use to run regressions --#
        sim.diagnosed.prevalence = extract.prevalence(sim, years=years,
                                                      continuum='diagnosed',
                                                      keep.dimensions = c('year','age','race','sex','risk'),
                                                      per.population = NA)
        sim.prevalence = extract.prevalence(sim, years=years,
                                                      keep.dimensions = c('year','age','race','sex','risk'),
                                            per.population = NA)
        
        df = melt(sim.prevalence)
        df$fraction = as.numeric(sim.diagnosed.prevalence / sim.prevalence)

#if (any(df$fraction<0) || any(df$fraction>1))
#{
#    browser()
#    print("FRACTION OUT OF BOUNDS")
#    print(df[df$fraction<0 | df$fraction>1,])
#    save(sim, file=paste0('code/debugging/funny.fraction.aware_', Sys.Date(), '.Rdata'))
#}
        df$fraction = pmax(0, pmin(1, df$fraction))
        
        df$n = as.numeric(denominators)
        
        df$black = as.numeric(df$race=='black')
        df$hispanic = as.numeric(df$race=='hispanic')
        
        msm.mask = df$sex=='msm'
        idu.mask = df$risk!='never_IDU'
        
        df$msm = as.numeric(msm.mask & !idu.mask)
        df$msm_idu = as.numeric(msm.mask & idu.mask)
        df$heterosexual = as.numeric(!msm.mask & !idu.mask)
        df$idu = as.numeric(idu.mask & !msm.mask)
        
        df$age1 = as.numeric(df$age=='13-24 years')
        df$age2 = as.numeric(df$age=='25-34 years')
        df$age3 = as.numeric(df$age=='35-44 years')
        df$age4 = as.numeric(df$age=='45-54 years')
        df$age5 = as.numeric(df$age=='55+ years')
        
        df$female = as.numeric(df$sex=='female')
        
        #-- Do the Regressions on the sim --#
        fits.age.race.sex.msm = lapply(age.race.sex.msm.years, do.knowledge.age.race.sex.msm.regression, df=df)
        fits.race.sex.idu = lapply(race.sex.idu.years, do.knowledge.race.sex.idu.regression, df=df)
        
        #-- Calculate the likelihood of the 'true' log ORs given the mean and cov.mat from the fit to sim --#
        age.race.sex.likelihoods = sapply(1:length(age.race.sex.msm.years), function(i){
            obs.mask = knowledge.of.status.regressions$age.race.sex.msm.years==age.race.sex.msm.years[i]
            
            dmvnorm(x=as.numeric(knowledge.of.status.regressions$age.race.sex.msm[obs.mask][[1]]$mean),
                    mean=fits.age.race.sex.msm[[i]]$mean,
                    sigma = fits.age.race.sex.msm[[i]]$cov.mat*stratified.ors.sd.inflation,
                    log=log)
        })
        race.sex.idu.likelihoods = sapply(1:length(race.sex.idu.years), function(i){
            obs.mask = knowledge.of.status.regressions$race.sex.idu.years==race.sex.idu.years[i]
            
            dmvnorm(x=as.numeric(knowledge.of.status.regressions$race.sex.idu[obs.mask][[1]]$mean),
                    mean=fits.race.sex.idu[[i]]$mean,
                    sigma = fits.race.sex.idu[[i]]$cov.mat*stratified.ors.sd.inflation,
                    log=log)
        })

        #-- Calculate the likelihood for total --#

        p = extract.prevalence(sim, years=total.years, keep.dimensions = 'year', continuum='diagnosed') /
            extract.prevalence(sim, years=total.years, keep.dimensions = 'year')
        
        cov.mat = make.compound.symmetry.matrix(n.total * total.sds, rho=total.rho) +
            diag(n.total * p * (1-p))
        
        total.likelihood = dmvnorm(x=diagnosed.prevalence.for.total,
                                   mean=n.total*p,
                                   sigma=cov.mat*total.sd.inflation,
                                   log=log)
        
        if (verbose)
        {
            print(paste0('total.likelihood = ', total.likelihood))
            print(paste0('age.race.sex.likelihoods = ', sum(age.race.sex.likelihoods)))
            print(paste0('race.sex.idu.likelihoods = ', sum(race.sex.idu.likelihoods)))
        }
        
        #-- Combine it all and return --#
        if (log)
            sum(age.race.sex.likelihoods) + sum(race.sex.idu.likelihoods) + total.likelihood
        else
            prod(age.race.sex.likelihoods) * prod(race.sex.idu.likelihoods) * total.likelihood
        
    }
    
}



do.knowledge.of.status.regressions <- function(dir='cleaned_data/continuum/national/diagnosis/')
{
    #-- Read in the Datasets --#
    df.male.race.risk = read.hiv.atlas.file(file.path(dir, 'diagnosis_by_male_race_risk.csv'))
    df.female.race.risk = read.hiv.atlas.file(file.path(dir, 'diagnosis_by_female_race_risk.csv'))
    df.age.race.sex = read.hiv.atlas.file(file.path(dir, 'diagnosis_by_age_race_sex.csv'))
    df.msm.age.race = read.hiv.atlas.file(file.path(dir, 'diagnosis_by_msm_age_race.csv'))
    
    df.age.race.sex$src = 'age.race.sex'
    df.msm.age.race$src = 'msm.age.race'
    df.male.race.risk$src = 'race.sex.risk'
    df.female.race.risk$src = 'race.sex.risk'
    
    df.female.age.race = df.age.race.sex[df.age.race.sex$Sex=='Female',]
    df.non.msm.male.age.race = df.age.race.sex[df.age.race.sex$Sex=='Male',]
    indices.into.msm = sapply(1:dim(df.non.msm.male.age.race)[1], function(i){
        (1:dim(df.msm.age.race)[1])[df.msm.age.race$Year==df.non.msm.male.age.race$Year[i] &
                                        df.msm.age.race$Age.Group==df.non.msm.male.age.race$Age.Group[i] &
                                        df.msm.age.race$Race.Ethnicity==df.non.msm.male.age.race$Race.Ethnicity[i]]
    })
    df.non.msm.male.age.race$Population = df.non.msm.male.age.race$Population - df.msm.age.race$Population[indices.into.msm]
    df.non.msm.male.age.race$Cases = df.non.msm.male.age.race$Cases - df.msm.age.race$Cases[indices.into.msm]
    df.non.msm.male.age.race$Percent = df.non.msm.male.age.race$Cases / df.non.msm.male.age.race$Population * 100
    
    
    #-- Set up Cleaned Data Frames --#
    df.joined.age.race.sex = rbind(df.msm.age.race,
                                   df.female.age.race,
                                   df.non.msm.male.age.race)
    
    df.cleaned.age.race.sex = data.frame(year=df.joined.age.race.sex$Year,
                                         fraction=df.joined.age.race.sex$Percent/100,
                                         n=df.joined.age.race.sex$Population,
                                         black=as.numeric(grepl('black', df.joined.age.race.sex$Race.Ethnicity, ignore.case = T)),
                                         hispanic=as.numeric(grepl('hispanic', df.joined.age.race.sex$Race.Ethnicity, ignore.case = T)),
                                         age1=as.numeric(grepl('13-24', df.joined.age.race.sex$Age.Group)),
                                         age2=as.numeric(grepl('25-34', df.joined.age.race.sex$Age.Group)),
                                         age3=as.numeric(grepl('35-44', df.joined.age.race.sex$Age.Group)),
                                         age4=as.numeric(grepl('45-54', df.joined.age.race.sex$Age.Group)),
                                         age5=as.numeric(grepl('55+', df.joined.age.race.sex$Age.Group)),
                                         male=as.numeric(df.joined.age.race.sex=='Male'),
                                         female=as.numeric(df.joined.age.race.sex=='Female'),
                                         msm=as.numeric(df.joined.age.race.sex$src=='msm.age.race'),
                                         src = df.joined.age.race.sex$src
    )
    df.cleaned.age.race.sex$other = as.numeric(df.cleaned.age.race.sex$black==0 & df.cleaned.age.race.sex$hispanic==0)
    
    
    df.sex.race.risk = rbind(df.male.race.risk,
                             df.female.race.risk)
    msm.mask = grepl('male-to-male', df.sex.race.risk$Transmission.Category, ignore.case = T)
    idu.mask = grepl('injection', df.sex.race.risk$Transmission.Category, ignore.case = T)
    het.mask = grepl('heterosexual', df.sex.race.risk$Transmission.Category, ignore.case = T)
    
    df.cleaned.sex.race.risk = data.frame(year=df.sex.race.risk$Year,
                                          fraction=df.sex.race.risk$Percent/100,
                                          n=df.sex.race.risk$Population,
                                          black=as.numeric(grepl('black', df.sex.race.risk$Race.Ethnicity, ignore.case = T)),
                                          hispanic=as.numeric(grepl('hispanic', df.sex.race.risk$Race.Ethnicity, ignore.case = T)),
                                          male=as.numeric(df.sex.race.risk=='Male'),
                                          female=as.numeric(df.sex.race.risk=='Female'),
                                          msm=as.numeric(msm.mask & !idu.mask),
                                          idu=as.numeric(idu.mask & !msm.mask),
                                          msm_idu=as.numeric(idu.mask & msm.mask),
                                          heterosexual=as.numeric(het.mask),
                                          src = df.sex.race.risk$src
    )
    df.cleaned.sex.race.risk$other = as.numeric(df.cleaned.sex.race.risk$black==0 & df.cleaned.sex.race.risk$hispanic==0)
    
    
    
    #-- Fit a Regression for each Year --#
    
    age.race.sex.msm.years = sort(unique(df.cleaned.age.race.sex$year))
    race.sex.idu.years = sort(unique(df.cleaned.sex.race.risk$year))
   
    list(
        age.race.sex.msm.years = age.race.sex.msm.years,
        race.sex.idu.years = race.sex.idu.years,
        
        age.race.sex.msm = lapply(age.race.sex.msm.years, do.knowledge.age.race.sex.msm.regression, df=df.cleaned.age.race.sex),
        race.sex.idu = lapply(race.sex.idu.years, do.knowledge.race.sex.idu.regression, df=df.cleaned.sex.race.risk)
    )
}

do.knowledge.age.race.sex.msm.regression <- function(df,
                                                     year=NA)
{
    if (!is.na(year))
        df = df[df$year==year,]
    
    fit = suppressWarnings(glm(fraction ~ black + hispanic +
                               age1 + age2 + age4 + age5 + 
                               female +
                               msm + msm:black + msm:hispanic +
                               msm:age1 + msm:age2 + msm:age4 + msm:age5,
                           family=binomial(),
                           data=df,
                           weights = n))
    
    names.to.keep = names(fit$coefficients)[-1]
    
    list(mean=fit$coefficients[names.to.keep],
         cov.mat=vcov(fit)[names.to.keep, names.to.keep])
}

do.knowledge.race.sex.idu.regression <- function(df, year=NA)
{
    if (!is.na(year))
        df = df[df$year==year,]
    
    fit = suppressWarnings(glm(fraction ~ black + hispanic +
                                female +
                                msm + msm:black + msm:hispanic + 
                                idu + idu:black + idu:hispanic + msm_idu,
                            family=binomial(),
                            data=df,
                            weights = n))
    
    names.to.keep = c('idu', 'black:idu', 'hispanic:idu', 'msm_idu')
    
    
    list(mean=fit$coefficients[names.to.keep],
         cov.mat=vcov(fit)[names.to.keep, names.to.keep])
}

get.state.averaged.knowledge.of.status <- function(msa,
                                                   state.surveillance,
                                                   years,
                                                   census.totals = ALL.DATA.MANAGERS$census.totals)
{
    states = states.for.msa(msa)
    if (length(states)==1)
        get.surveillance.data(state.surveillance, states, data.type = 'diagnosed', years=years)
    else
    {
        p = get.surveillance.data(state.surveillance, states, data.type = 'diagnosed', years=years, aggregate.locations = F)
        
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

##---------------------------------##
##-- (Historical) AIDS DIAGNOSES --##
##---------------------------------##

create.aids.diagnoses.likelihood <- function(surv=msa.surveillance,
                                             location=BALTIMORE.MSA,
                                             numerator.sd=function(years,num){rep(0,length(num))},
                                             sd.inflation=1,
                                             years=1999:2002,
                                             population=get.census.totals(ALL.DATA.MANAGERS$census.totals,
                                                                          location, years=years, flatten.single.dim.array = T),
                                             hiv.to.aids.diagnoses.ratio=mean(c('1999'=1.45,
                                                                           '2000'=1.56,
                                                                           '2001'=1.51,
                                                                           '2002'=1.39,
                                                                           '2003'=1.35,
                                                                           '2004'=1.25)[as.character(years)]),
                                             hiv.to.aids.diagnoses.ratio.log.sd=0.5*log(1.1),
                                             rho=0.5
                                             )
{
    observed.aids = get.surveillance.data(location.codes = location, data.type='aids.diagnoses', years=years)
    obs.sds = numerator.sd(years, observed.aids)
    
    #transform to lognormal based on mean and sd
    cv = obs.sds / observed.aids
    log.var = log(1 + cv^2)
    log.mean = log(observed.aids) - log.var/2
  
    #add log ratio
    log.ratios = log(hiv.to.aids.diagnoses.ratio) - hiv.to.aids.diagnoses.ratio.log.sd^2/2
    log.mean = log.mean + log.ratios
    log.var = log.var + hiv.to.aids.diagnoses.ratio.log.sd^2

    #put back on exp scale
    obs = exp(log.mean + log.var/2)
    obs.var = (exp(log.var)-1) * exp(2*log.mean + log.var)
    
    function(sim, log=T, verbose=F)
    {
        numerators = extract.new.diagnoses(sim, years=years, keep.dimensions = 'year', per.population = NA)
        denominators = extract.population.subset(sim, years=years, keep.dimensions = 'year',
                                                per.population = NA)
        
        rates = numerators / denominators
        
      #  sds = sqrt(population * rates * (1-rates) * sd.inflation^2 + obs.var)

        
        cov.mat = make.compound.symmetry.matrix(sqrt(obs.var), rho) + 
            diag(population * rates * (1-rates) * sd.inflation^2)
        
        if (verbose)
            print(cbind(obs.aids=observed.aids, obs=obs, sim=rates*population))
        
        dmvnorm(x=as.numeric(obs),
                mean=as.numeric(rates*population),
                sigma=cov.mat, 
                log=log)
        
    }
}

##---------------------------------------##
##-- (Historical) CUMULATIVE MORTALITY --##
##---------------------------------------##

#.9 captured from https://jamanetwork.com/journals/jama/fullarticle/405140 (cited in 2001-2002 surveillance report)
create.cumulative.mortality.likelihood <- function(years=1981:2000,
                                                   surv=msa.surveillance,
                                                   location=BALTIMORE.MSA,
                                                   population=sum(get.census.totals(ALL.DATA.MANAGERS$census.totals,
                                                                                    location, years=years)),
                                                   rho=0.5,
                                                   numerator.sd=function(...){0},
                                                   sd.inflation=1,
                                                   fraction.captured=0.9*.85,
                                                   verbose=F) #.85 and .9 are AIDS reported and deaths captured from technical appendix of volume 14 report - 2001-2002. .95 is my guess for how many HIV deaths are in people with AIDS
{
    observed = get.surveillance.data(location.codes = location, data.type = 'cumulative.aids.mortality', sex=T,race=T,risk=T)[1,,,]
    observed = apply(observed, c('race','sex','risk'), function(x){x})
    observed[,'female',c('msm','msm_idu')] = NA
    mask = !is.na(observed)
    observed = observed[mask]
    
    numerator.sds = numerator.sd(year=rep(max(years), length(observed)), num=observed)
    
    function(sim, log=T)
    {
        numerators = extract.overall.hiv.mortality(sim, years=years, keep.dimensions = c('race','sex','risk'),
                                                   continuum = 'diagnosed',
                                                   use.cdc.categorizations = T, per.population = NA)
        denominator = extract.population.subset(sim, years=years, keep.dimensions = character(),
                                                 use.cdc.categorizations = T, per.population = NA)
        
        rates = numerators[mask] / denominator * fraction.captured
        
        sds = sqrt(population * rates * (1-rates) * sd.inflation^2 + numerator.sds^2)
        
 
#        cov.mat = make.compound.symmetry.matrix(numerator.sds, rho) +
 #           diag( population * rates * (1-rates) * sd.inflation^2 )
        
#        cov.mat = make.compound.symmetry.matrix(sqrt(population * rates * (1-rates) * sd.inflation^2), rho) +
        #   diag(numerator.sds^2)   
     #   cbind(observed, sim=rates*population, sd=sqrt(diag(cov.mat)), model=sqrt(population*rates*(1-rates)*sd.inflation^2),
      #        obs=numerator.sds)
      
        piecewise = dnorm(x = observed,
                          mean = rates * population, 
                          sd = sds,
                          log = log)

    if (verbose)
    {
        print(cbind(melt(numerators/denominator*population)[mask,], observed, d=piecewise))
        print(c(sim=sum(rates*population), obs=sum(observed)))
    }

        if (log)
            sum(piecewise)
        else
            prod(piecewise)
        
    #    dmvnorm(x = observed,
    #            mean = rates * population,
    #            sigma = cov.mat,
    #            log=log)
    }
}


##-----------------##
##-- IV DRUG USE --##
##-----------------##

create.idu.likelihood <- function(idu.manager=ALL.DATA.MANAGERS$idu,
                                  census=ALL.DATA.MANAGERS$census.full,
                                  location=BALTIMORE.MSA,
                                  years=2014:2016,
                                  log.sd=log(2)/2,
                                  verbose=F)
{
    fips = counties.for.msa(location)
    if (length(fips)==0)
        fips = counties.for.state(location)
    if (length(fips)==0)
        fips = location
    if (any(is.na(county.names(fips))))
        stop("Invalid location - does not match a state, MSA, or county")

    idu.abs.proportions = get.aggregate.idu.proportions(idu.manager=idu.manager,
                                                    census=census,
                                                    fips=fips,
                                                    years=years)

    idu.relative.proportions = get.relative.idu.proportions(idu.manager=idu.manager,
                                                           census=census,
                                                           fips=fips,
                                                           years=years)

    names(idu.relative.proportions$idu.30d.by.age) = paste0('active_', names(idu.relative.proportions$idu.30d.by.age))
    names(idu.relative.proportions$idu.prior.by.age) = paste0('remission_', names(idu.relative.proportions$idu.30d.by.age))

    names(idu.relative.proportions$idu.30d.by.race) = paste0('active_', names(idu.relative.proportions$idu.30d.by.race))
    names(idu.relative.proportions$idu.prior.by.race) = paste0('remission_', names(idu.relative.proportions$idu.prior.by.race))

    names(idu.relative.proportions$idu.30d.by.sex) = paste0('active_', names(idu.relative.proportions$idu.30d.by.sex))
    names(idu.relative.proportions$idu.prior.by.sex) = paste0('remission_', names(idu.relative.proportions$idu.prior.by.sex))

    function(sim, log=T)
    {
        MIN = 0.000000001
        #-- Total Prevalence --#
        sim.active.idu.prevalence = max(MIN,extract.population.subset(sim, years=years, risk='active_IDU', keep.dimensions=character()) /
            extract.population.subset(sim, years=years, keep.dimensions=character()))
        sim.idu.in.remission.prevalence = max(MIN, extract.population.subset(sim, years=years, risk='IDU_in_remission', keep.dimensions=character()) /
            extract.population.subset(sim, years=years, keep.dimensions=character()))

        likelihood.components = c(active_prevalence=dnorm(log(idu.abs.proportions$idu.30d), mean=log(sim.active.idu.prevalence), sd=log.sd, log=log),
                                  remission_prevalence=dnorm(log(idu.abs.proportions$idu.prior), mean=log(sim.idu.in.remission.prevalence), sd=log.sd, log=log))

        #-- Age Ratios --#
        sim.active.idu.by.age = pmax(MIN,extract.population.subset(sim, years=years, risk='active_IDU', keep.dimensions='age') /
            extract.population.subset(sim, years=years, keep.dimensions='age'))
        sim.idu.in.remission.by.age = pmax(MIN, extract.population.subset(sim, years=years, risk='IDU_in_remission', keep.dimensions='age') /
            extract.population.subset(sim, years=years, keep.dimensions='age'))

        mask = idu.relative.proportions$idu.30d.by.age != 1
        likelihood.components = c(likelihood.components, dnorm(log(idu.relative.proportions$idu.30d.by.age[mask]),
                                                               mean=log(sim.active.idu.by.age[mask]) - log(sim.active.idu.by.age[!mask]),
                                                               sd = log.sd, log=log))
        likelihood.components = c(likelihood.components, dnorm(log(idu.relative.proportions$idu.prior.by.age[mask]),
                                                               mean=log(sim.idu.in.remission.by.age[mask]) - log(sim.idu.in.remission.by.age[!mask]),
                                                               sd = log.sd, log=log))

        #-- Sex Ratios --#
        sim.active.idu.by.sex = pmax(MIN,extract.population.subset(sim, years=years, risk='active_IDU', keep.dimensions='sex') /
            extract.population.subset(sim, years=years, keep.dimensions='sex'))
        sim.idu.in.remission.by.sex = pmax(MIN,extract.population.subset(sim, years=years, risk='IDU_in_remission', keep.dimensions='sex') /
            extract.population.subset(sim, years=years, keep.dimensions='sex'))

        mask = idu.relative.proportions$idu.30d.by.sex != 1
        likelihood.components = c(likelihood.components, dnorm(log(idu.relative.proportions$idu.30d.by.sex[mask]),
                                                               mean=log(sim.active.idu.by.sex[mask]) - log(sim.active.idu.by.sex[!mask]),
                                                               sd = log.sd, log=log))
        likelihood.components = c(likelihood.components, dnorm(log(idu.relative.proportions$idu.prior.by.sex[mask]),
                                                               mean=log(sim.idu.in.remission.by.sex[mask] - log(sim.idu.in.remission.by.sex[!mask])),
                                                               sd = log.sd, log=log))

        #-- Race Ratios --#
        sim.active.idu.by.race = pmax(MIN,extract.population.subset(sim, years=years, risk='active_IDU', keep.dimensions='race') /
            extract.population.subset(sim, years=years, keep.dimensions='race'))
        sim.idu.in.remission.by.race = pmax(MIN,extract.population.subset(sim, years=years, risk='IDU_in_remission', keep.dimensions='race') /
            extract.population.subset(sim, years=years, keep.dimensions='race'))

        mask = idu.relative.proportions$idu.30d.by.race != 1
        likelihood.components = c(likelihood.components, dnorm(log(idu.relative.proportions$idu.30d.by.race[mask]),
                                                               mean=log(sim.active.idu.by.race[mask]) - log(sim.active.idu.by.race[!mask]),
                                                               sd = log.sd, log=log))
        likelihood.components = c(likelihood.components, dnorm(log(idu.relative.proportions$idu.prior.by.race[mask]),
                                                               mean=log(sim.idu.in.remission.by.race[mask]) - log(sim.idu.in.remission.by.race[!mask]),
                                                               sd = log.sd, log=log))

        if (verbose)
            print(likelihood.components)

        if (log)
            sum(likelihood.components)
        else
            prod(likelihood.components)
    }
}

##----------------------##
##----------------------##
##-- HELPER FUNCTIONS --##
##----------------------##
##----------------------##


# Joins multiple (independent) likelihood functions
create.joint.likelihood.function <- function(...)
{
    args = list(...)
    sub.likelihoods = list()
    for (i in 1:length(args))
    {
        if (is(args[[i]], 'list'))
            sub.likelihoods = c(sub.likelihoods, args[[i]])
        else
            sub.likelihoods = c(sub.likelihoods, args[i])
    }

    function(jheem.results, log=T, verbose=F){
        if (jheem.results$terminated)
        {
            if (log)
                return (-Inf)
            else
                return (0)
        }


        sub.values = sapply(sub.likelihoods, function(lik){
            lik(jheem.results, log=log)
        })

        if (verbose)
            print(paste0("Likelihood components: ",
                         paste0(names(sub.likelihoods), '=', sub.values, collapse = ', ')))

        if (log)
            sum(sub.values)
        else
            prod(sub.values)
    }
}

##------------------------------------------------##
##-- HELPERS TO REDUCE COMPUTATION FOR MATRICES --##
##------------------------------------------------##

#We have a matrix M, such that we multiply M by some vector V
# If two rows of M are identical (ie, two elements of V are always both or neither
#  included in each row of the matrix transformation), we can pre-sum the two elements
#  of V and remove one row from the matrix M
make.transformation.mapping <- function(transformation.matrix)
{
    n.col = ncol(transformation.matrix)
    col.signatures = apply(transformation.matrix, 2, paste0, collapse=',')#paste0(denominators, '-', apply(transformation.matrix, 2, paste0, collapse=','))
    unique.col.signatures = unique(col.signatures)
    n.signatures = length(unique.col.signatures)

    rv = list()
    rv$index.to.signature = sapply(col.signatures, function(signature){
        (1:n.signatures)[signature==unique.col.signatures]
    })

    rv$signature.to.index = lapply(1:n.signatures, function(i){
        (1:n.col)[rv$index.to.signature==i]
    })

    rv$collapsing.matrix = sapply(1:n.signatures, function(i){
        row = rep(0, n.col)
        row[rv$signature.to.index[[i]]] = 1
        row
    })

    rv$first.in.signature = sapply(rv$signature.to.index, function(s2i){s2i[1]})

    rv
}

sum.for.matrix.mapping <- function(values, mapping)
{
    sapply(mapping$signature.to.index, function(indices){
        sum(values[indices])
    })
}

collapse.for.matrix.mapping <- function(M, mapping)
{
    sapply(1:length(mapping$signature.to.index), function(j){
        sapply(1:length(mapping$signature.to.index), function(i){
            sum(M[mapping$signature.to.index[[i]],
                  mapping$signature.to.index[[j]]])
        })
    })
}

##-------------------------------------##
##-- HELPERS TO *EXECUTE* LIKELIHOOD --##
##-------------------------------------##


pull.simulations.rates <- function(jheem.results,
                                   data.type=c('new','prevalence','mortality','aware'),
                                   years,
                                   denominator.dimensions,
                                   aggregate.denominator.males=T)
{
    if (data.type=='new')
        numerators = extract.new.diagnoses(jheem.results,
                                      years=years,
                                      keep.dimensions = c('year', 'age','race','sex','risk'),
                                      per.population = NA)
    else if (data.type=='prevalence')
        numerators = extract.prevalence(jheem.results, continuum='diagnosed',
                                   years=years,
                                   keep.dimensions = c('year', 'age','race','sex','risk'),
                                   per.population = NA)
    else if (data.type=='mortality')
        numerators = extract.overall.hiv.mortality(jheem.results,
                                              years=years,
                                              keep.dimensions = c('year', 'age','race','sex','risk'),
                                              per.population = NA)
    else if (data.type=='aware')
        numerators = extract.prevalence(jheem.results, years=years,
                                        continuum = 'diagnosed',
                                        keep.dimensions = c('year', 'age','race','sex','risk'),
                                        per.population = NA)
    else
        stop("data.type must be one of 'new', 'prevalence', 'mortality', or 'aware'")

    if (data.type=='aware')
        denominators = extract.prevalence(jheem.results, years=years,
                                          keep.dimensions=denominator.dimensions,
                                          per.population = NA)    
    else
        denominators = extract.population.subset(jheem.results, years=years,
                                             keep.dimensions = denominator.dimensions)

    denominators = expand.population(denominators, target.dim.names = dimnames(numerators))

    if (aggregate.denominator.males && any(denominator.dimensions=='sex'))
    {
        males = denominators[,,,'msm',] + denominators[,,,'heterosexual_male',]
        denominators[,,,'msm',] = males
        denominators[,,,'heterosexual_male',] = males
    }

    numerators / denominators
}

pull.simulations.rates.per.total.population <- function(jheem.results,
                                   data.type=c('new','prevalence','mortality'),
                                   mapping=NULL,
                                   years)
{
    if (data.type=='new')
        numerators = extract.new.diagnoses(jheem.results,
                                           years=years,
                                           keep.dimensions = c('year', 'age','race','sex','risk'),
                                           per.population = NA)
    else if (data.type=='prevalence')
        numerators = extract.prevalence(jheem.results, continuum='diagnosed',
                                        years=years,
                                        keep.dimensions = c('year', 'age','race','sex','risk'),
                                        per.population = NA)
    else if (data.type=='mortality')
        numerators = extract.overall.hiv.mortality(jheem.results,
                                                   years=years,
                                                   keep.dimensions = c('year', 'age','race','sex','risk'),
                                                   per.population = NA)
    else
        stop("data.type must be one of 'new', 'prevalence', or 'mortality")

    denominators = extract.population.subset(jheem.results, years=years,
                                             keep.dimensions = 'year')

    sum.for.matrix.mapping(numerators / denominators, mapping)
}

#Once we have numerators and rates, run a multivariate normal that is summed up
likelihood.sub <- function(pre.transformation.rates,
                           transformation.matrix,
                           response.vector,
                           denominator.vector,
                           denominator.covar.mat=NULL,
                           numerator.covar.mat=NULL,
                           corr.mat=NULL,
                           transformation.mapping=NULL,
                           sd.inflation=1,
                           log=T,
                           sim=NULL, #these last two arguments are for debugging purposes
                           description=NULL)
{
    pre.transformation.rates = as.numeric(pre.transformation.rates)

    pre.transformation.rates = pmin(1,pmax(0, pre.transformation.rates))

    if (is.null(transformation.mapping))
    {
        n.p = pre.transformation.rates * denominator.vector
        n.p.1mp = pre.transformation.rates * (1 - pre.transformation.rates) * denominator.vector
    }
    else
    {
        n.p = sum.for.matrix.mapping(pre.transformation.rates* denominator.vector, transformation.mapping)
        n.p.1mp = sum.for.matrix.mapping(pre.transformation.rates * (1 - pre.transformation.rates)* denominator.vector,
                                       transformation.mapping)
    }
    
    if (is.null(corr.mat))
        binomial.variance.component = diag(n.p.1mp)
    else
    {
        sds = sqrt(n.p.1mp)
        binomial.variance.component = sds %*% t(sds) * corr.mat
    }

    mean.vector = transformation.matrix %*% n.p

    if (is.null(denominator.covar.mat))
    {
        covar.mat = transformation.matrix %*%
            binomial.variance.component %*%
            t(transformation.matrix)
    }
    else
    {
        denominator.variance.component = denominator.covar.mat *
            pre.transformation.rates %*% t(pre.transformation.rates)

        covar.mat = transformation.matrix %*%
            (binomial.variance.component + denominator.variance.component) %*%
            t(transformation.matrix)
    }

    if (is(sd.inflation, 'function'))
        sd.inflation = sd.inflation(description)

    if (length(sd.inflation)==1)
        covar.mat = covar.mat * sd.inflation^2
    else
        covar.mat = covar.mat * (sd.inflation %*% t(sd.inflation))

    if (!is.null(numerator.covar.mat))
        covar.mat = covar.mat + numerator.covar.mat

    #for debugging
    if (1==2)
    {
        catg.minus.year = unique(gsub('[0-9][0-9][0-9][0-9], ', '', description))
        sapply(catg.minus.year, function(catg){
            regex = gsub('\\+', '\\\\+', catg)
            mask = grepl(regex, description)
            dmvnorm(x=response.vector[mask],
                    mean=mean.vector[mask],
                    sigma=covar.mat[mask,mask],
                    log=log)
        })
        sapply(catg.minus.year, function(catg){
            regex = gsub('\\+', '\\\\+', catg)
            mask = grepl(regex, description)
            mean(mean.vector[mask])
        })

        cbind(mean=sapply(catg.minus.year, function(catg){
            regex = gsub('\\+', '\\\\+', catg)
            mask = grepl(regex, description)
            mean(mean.vector[mask])
        }),
        density=sapply(catg.minus.year, function(catg){
            regex = gsub('\\+', '\\\\+', catg)
            mask = grepl(regex, description)
            dmvnorm(x=response.vector[mask],
                    mean=mean.vector[mask],
                    sigma=covar.mat[mask,mask],
                    log=log)
        }))
    }

    if (1==2)
    {
        mask = grepl('idu', description) & !grepl('msm', description)
        print(paste0('idu: ', dmvnorm(x=response.vector[mask],
                                      mean=mean.vector[mask],
                                      sigma=covar.mat[mask,mask],
                                      log=log)))
        mask = grepl('msm', description) & !grepl('idu', description)
        print(paste0('msm: ', dmvnorm(x=response.vector[mask],
                                      mean=mean.vector[mask],
                                      sigma=covar.mat[mask,mask],
                                      log=log)))
        mask = grepl('idu', description) & grepl('msm', description)
        print(paste0('msm+idu: ', dmvnorm(x=response.vector[mask],
                                      mean=mean.vector[mask],
                                      sigma=covar.mat[mask,mask],
                                      log=log)))
        mask = grepl('het', description)
        print(paste0('het: ', dmvnorm(x=response.vector[mask],
                                      mean=mean.vector[mask],
                                      sigma=covar.mat[mask,mask],
                                      log=log)))
    }
    if (1==2)
    {
        mask = grepl('idu', description) & !grepl('msm', description) & grepl('black', description)
        print(paste0('black idu: ', dmvnorm(x=response.vector[mask],
                                            mean=mean.vector[mask],
                                            sigma=covar.mat[mask,mask],
                                            log=log)))
        mask = grepl('idu', description) & !grepl('msm', description) & grepl('hispanic', description)
        print(paste0('hispanic idu: ', dmvnorm(x=response.vector[mask],
                                            mean=mean.vector[mask],
                                            sigma=covar.mat[mask,mask],
                                            log=log)))
        mask = grepl('idu', description) & !grepl('msm', description) & grepl('other', description)
        print(paste0('other idu: ', dmvnorm(x=response.vector[mask],
                                            mean=mean.vector[mask],
                                            sigma=covar.mat[mask,mask],
                                            log=log)))
    }

    dmvnorm(x=as.numeric(response.vector),
            mean=as.numeric(mean.vector),
            sigma=covar.mat,
            log=log)
}



##-----------------------------------------------##
##-- HIGH-LEVEL HELPERS TO *CREATE* LIKELIHOOD --##
##-----------------------------------------------##

create.likelihood.elements.for.data.type <- function(data.type=c('new','prevalence', 'mortality'),
                                                     years,
                                                     surv,
                                                     location,
                                                     by.total,
                                                     by.sex,
                                                     by.race,
                                                     by.age,
                                                     by.risk,
                                                     by.sex.age,
                                                     by.sex.race,
                                                     by.sex.risk,
                                                     by.race.risk,
                                                     numerator.sd=function(...){0},
                                                     bias.fn=function(...){0},
                                                     bias.sd=function(...){0},
                                                     numerator.year.to.year.chunk.correlation=0,
                                                     numerator.year.to.year.off.correlation=0,
                                                     numerator.chunk.years=numeric(),
                                                     ages=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
                                                     races=c('black','hispanic','other'),
                                                     sexes=c('heterosexual_male','msm','female'),
                                                     risks=c('never_IDU','active_IDU','IDU_in_remission')
)
{
    #-- Set Up Indexing --#
    dim.names = list(year=as.character(years), age=ages, race=races, sex=sexes, risk=risks)
    jheem.skeleton = array(0,
                           dim=sapply(dim.names, length),
                           dimnames = dim.names)

    #-- Set Up Full Response Vector and Transformation Matrix --#

    response.vector = numeric()
    transformation.matrix = NULL
    numerator.covar.mat = NULL
    descriptions = character()

    if (by.total)
    {
        cdc.arr = get.surveillance.data(surv, location, data.type=data.type, years=years, throw.error.if.missing.data = F)
        if (!is.null(cdc.arr))
        {
            tmrv = make.transformation.matrix.and.response.vector(cdc.arr=cdc.arr, jheem.skeleton=jheem.skeleton)
    
            sds = numerator.sd(years=tmrv$year, num=tmrv$response.vector)
            if (length(sds)==1)
                sds = rep(sds, length(tmrv$response.vector))
            bias = bias.fn(years=tmrv$year, num=tmrv$response.vector)
            if (length(bias)==1)
                bias = rep(bias, length(tmrv$response.vector))
            bias.sds = bias.sd(years=tmrv$year, num=tmrv$response.vector)
            if (length(bias.sds)==1)
                bias.sds = rep(bias.sds, length(tmrv$response.vector))
            
            sds = sqrt(sds^2 + bias.sds^2)
            response.vector = c(response.vector, tmrv$response.vector + bias)
            transformation.matrix = rbind(transformation.matrix, tmrv$transformation.matrix)
            
            one.numerator.covar.mat = make.chunked.compound.symmetry.matrix(sds=sds,
                                                                            chunk.rho=numerator.year.to.year.chunk.correlation,
                                                                            non.chunk.rho=numerator.year.to.year.off.correlation,
                                                                            years=tmrv$year,
                                                                            categories=tmrv$non.year.descriptions,
                                                                            chunk.years=numerator.chunk.years)
    #        one.numerator.covar.mat = make.compound.symmetry.matrix(sds=sds,
    #                                                                rho=numerator.year.to.year.correlation)
    
            numerator.covar.mat = join.independent.covariance.matrices(numerator.covar.mat,
                                                                       one.numerator.covar.mat)
            descriptions = c(descriptions, tmrv$descriptions)
        }
    }

    if (by.sex)
    {
        cdc.arr = get.surveillance.data(surv, location, data.type=data.type, years=years, sex=T, throw.error.if.missing.data = F)
        if (!is.null(cdc.arr))
        {
            tmrv = make.transformation.matrix.and.response.vector(cdc.arr=cdc.arr, jheem.skeleton=jheem.skeleton)
            
            sds = numerator.sd(years=tmrv$year, num=tmrv$response.vector)
            if (length(sds)==1)
                sds = rep(sds, length(tmrv$response.vector))
            bias = bias.fn(years=tmrv$year, num=tmrv$response.vector)
            if (length(bias)==1)
                bias = rep(bias, length(tmrv$response.vector))
            bias.sds = bias.sd(years=tmrv$year, num=tmrv$response.vector)
            if (length(bias.sds)==1)
                bias.sds = rep(bias.sds, length(tmrv$response.vector))
            
            sds = sqrt(sds^2 + bias.sds^2)
            response.vector = c(response.vector, tmrv$response.vector + bias)
            transformation.matrix = rbind(transformation.matrix, tmrv$transformation.matrix)
            
            one.numerator.covar.mat = make.chunked.compound.symmetry.matrix(sds=sds,
                                                                            chunk.rho=numerator.year.to.year.chunk.correlation,
                                                                            non.chunk.rho=numerator.year.to.year.off.correlation,
                                                                            years=tmrv$year,
                                                                            categories=tmrv$non.year.descriptions,
                                                                            chunk.years=numerator.chunk.years)
    #        one.numerator.covar.mat = make.compound.symmetry.matrix(sds=sds,
    #                                                                rho=numerator.year.to.year.correlation)
    
            numerator.covar.mat = join.independent.covariance.matrices(numerator.covar.mat,
                                                                       one.numerator.covar.mat)
            descriptions = c(descriptions, tmrv$descriptions)
        }
    }

    if (by.race)
    {
        cdc.arr = get.surveillance.data(surv, location, data.type=data.type, years=years, race=T, throw.error.if.missing.data = F)
        if (!is.null(cdc.arr))
        {
            tmrv = make.transformation.matrix.and.response.vector(cdc.arr=cdc.arr, jheem.skeleton=jheem.skeleton)
            
            sds = numerator.sd(years=tmrv$year, num=tmrv$response.vector)
            if (length(sds)==1)
                sds = rep(sds, length(tmrv$response.vector))
            bias = bias.fn(years=tmrv$year, num=tmrv$response.vector)
            if (length(bias)==1)
                bias = rep(bias, length(tmrv$response.vector))
            bias.sds = bias.sd(years=tmrv$year, num=tmrv$response.vector)
            if (length(bias.sds)==1)
                bias.sds = rep(bias.sds, length(tmrv$response.vector))
            
            sds = sqrt(sds^2 + bias.sds^2)
            response.vector = c(response.vector, tmrv$response.vector + bias)
            transformation.matrix = rbind(transformation.matrix, tmrv$transformation.matrix)
            
            one.numerator.covar.mat = make.chunked.compound.symmetry.matrix(sds=sds,
                                                                            chunk.rho=numerator.year.to.year.chunk.correlation,
                                                                            non.chunk.rho=numerator.year.to.year.off.correlation,
                                                                            years=tmrv$year,
                                                                            categories=tmrv$non.year.descriptions,
                                                                            chunk.years=numerator.chunk.years)
    #        one.numerator.covar.mat = make.compound.symmetry.matrix(sds=sds,
    #                                                                rho=numerator.year.to.year.correlation)
    
            numerator.covar.mat = join.independent.covariance.matrices(numerator.covar.mat,
                                                                       one.numerator.covar.mat)
            descriptions = c(descriptions, tmrv$descriptions)
        }
    }

    if (by.age)
    {
        cdc.arr = get.surveillance.data(surv, location, data.type=data.type, years=years, age=T, throw.error.if.missing.data = F)
        if (!is.null(cdc.arr))
        {
            tmrv = make.transformation.matrix.and.response.vector(cdc.arr=cdc.arr, jheem.skeleton=jheem.skeleton)
            
            sds = numerator.sd(years=tmrv$year, num=tmrv$response.vector)
            if (length(sds)==1)
                sds = rep(sds, length(tmrv$response.vector))
            bias = bias.fn(years=tmrv$year, num=tmrv$response.vector)
            if (length(bias)==1)
                bias = rep(bias, length(tmrv$response.vector))
            bias.sds = bias.sd(years=tmrv$year, num=tmrv$response.vector)
            if (length(bias.sds)==1)
                bias.sds = rep(bias.sds, length(tmrv$response.vector))
            
            sds = sqrt(sds^2 + bias.sds^2)
            response.vector = c(response.vector, tmrv$response.vector + bias)
            transformation.matrix = rbind(transformation.matrix, tmrv$transformation.matrix)
            
            one.numerator.covar.mat = make.chunked.compound.symmetry.matrix(sds=sds,
                                                                            chunk.rho=numerator.year.to.year.chunk.correlation,
                                                                            non.chunk.rho=numerator.year.to.year.off.correlation,
                                                                            years=tmrv$year,
                                                                            categories=tmrv$non.year.descriptions,
                                                                            chunk.years=numerator.chunk.years)
    #        one.numerator.covar.mat = make.compound.symmetry.matrix(sds=sds,
    #                                                                rho=numerator.year.to.year.correlation)
    
            numerator.covar.mat = join.independent.covariance.matrices(numerator.covar.mat,
                                                                       one.numerator.covar.mat)
            descriptions = c(descriptions, tmrv$descriptions)
        }
    }

    if (by.risk)
    {
        cdc.arr = get.surveillance.data(surv, location, data.type=data.type, years=years, risk=T, throw.error.if.missing.data = F)
        if (!is.null(cdc.arr))
        {
            tmrv = make.transformation.matrix.and.response.vector(cdc.arr=cdc.arr, jheem.skeleton=jheem.skeleton)
            
            sds = numerator.sd(years=tmrv$year, num=tmrv$response.vector)
            if (length(sds)==1)
                sds = rep(sds, length(tmrv$response.vector))
            bias = bias.fn(years=tmrv$year, num=tmrv$response.vector)
            if (length(bias)==1)
                bias = rep(bias, length(tmrv$response.vector))
            bias.sds = bias.sd(years=tmrv$year, num=tmrv$response.vector)
            if (length(bias.sds)==1)
                bias.sds = rep(bias.sds, length(tmrv$response.vector))
            
            sds = sqrt(sds^2 + bias.sds^2)
            response.vector = c(response.vector, tmrv$response.vector + bias)
            transformation.matrix = rbind(transformation.matrix, tmrv$transformation.matrix)
            
            one.numerator.covar.mat = make.chunked.compound.symmetry.matrix(sds=sds,
                                                                            chunk.rho=numerator.year.to.year.chunk.correlation,
                                                                            non.chunk.rho=numerator.year.to.year.off.correlation,
                                                                            years=tmrv$year,
                                                                            categories=tmrv$non.year.descriptions,
                                                                            chunk.years=numerator.chunk.years)
    #        one.numerator.covar.mat = make.compound.symmetry.matrix(sds=sds,
    #                                                                rho=numerator.year.to.year.correlation)
    
            numerator.covar.mat = join.independent.covariance.matrices(numerator.covar.mat,
                                                                       one.numerator.covar.mat)
            descriptions = c(descriptions, tmrv$descriptions)
        }
    }

    if (by.sex.age)
    {
        cdc.arr = get.surveillance.data(surv, location, data.type=data.type, years=years, sex=T, age=T, throw.error.if.missing.data = F)
        if (!is.null(cdc.arr))
        {
            tmrv = make.transformation.matrix.and.response.vector(cdc.arr=cdc.arr, jheem.skeleton=jheem.skeleton)
            
            sds = numerator.sd(years=tmrv$year, num=tmrv$response.vector)
            if (length(sds)==1)
                sds = rep(sds, length(tmrv$response.vector))
            bias = bias.fn(years=tmrv$year, num=tmrv$response.vector)
            if (length(bias)==1)
                bias = rep(bias, length(tmrv$response.vector))
            bias.sds = bias.sd(years=tmrv$year, num=tmrv$response.vector)
            if (length(bias.sds)==1)
                bias.sds = rep(bias.sds, length(tmrv$response.vector))
            
            sds = sqrt(sds^2 + bias.sds^2)
            response.vector = c(response.vector, tmrv$response.vector + bias)
            transformation.matrix = rbind(transformation.matrix, tmrv$transformation.matrix)
            
            one.numerator.covar.mat = make.chunked.compound.symmetry.matrix(sds=sds,
                                                                            chunk.rho=numerator.year.to.year.chunk.correlation,
                                                                            non.chunk.rho=numerator.year.to.year.off.correlation,
                                                                            years=tmrv$year,
                                                                            categories=tmrv$non.year.descriptions,
                                                                            chunk.years=numerator.chunk.years)
    #        one.numerator.covar.mat = make.compound.symmetry.matrix(sds=sds,
    #                                                                rho=numerator.year.to.year.correlation)
    
            numerator.covar.mat = join.independent.covariance.matrices(numerator.covar.mat,
                                                                       one.numerator.covar.mat)
            descriptions = c(descriptions, tmrv$descriptions)
        }
    }

    if (by.sex.race)
    {
        cdc.arr = get.surveillance.data(surv, location, data.type=data.type, years=years, sex=T, race=T, throw.error.if.missing.data = F)
        if (!is.null(cdc.arr))
        {
            tmrv = make.transformation.matrix.and.response.vector(cdc.arr=cdc.arr, jheem.skeleton=jheem.skeleton)
            
            sds = numerator.sd(years=tmrv$year, num=tmrv$response.vector)
            if (length(sds)==1)
                sds = rep(sds, length(tmrv$response.vector))
            bias = bias.fn(years=tmrv$year, num=tmrv$response.vector)
            if (length(bias)==1)
                bias = rep(bias, length(tmrv$response.vector))
            bias.sds = bias.sd(years=tmrv$year, num=tmrv$response.vector)
            if (length(bias.sds)==1)
                bias.sds = rep(bias.sds, length(tmrv$response.vector))
            
            sds = sqrt(sds^2 + bias.sds^2)
            response.vector = c(response.vector, tmrv$response.vector + bias)
            transformation.matrix = rbind(transformation.matrix, tmrv$transformation.matrix)
            
            one.numerator.covar.mat = make.chunked.compound.symmetry.matrix(sds=sds,
                                                                            chunk.rho=numerator.year.to.year.chunk.correlation,
                                                                            non.chunk.rho=numerator.year.to.year.off.correlation,
                                                                            years=tmrv$year,
                                                                            categories=tmrv$non.year.descriptions,
                                                                            chunk.years=numerator.chunk.years)
    #        one.numerator.covar.mat = make.compound.symmetry.matrix(sds=sds,
    #                                                                rho=numerator.year.to.year.correlation)
    
            numerator.covar.mat = join.independent.covariance.matrices(numerator.covar.mat,
                                                                       one.numerator.covar.mat)
            descriptions = c(descriptions, tmrv$descriptions)
        }
    }

    if (by.sex.risk)
    {
        cdc.arr = get.surveillance.data(surv, location, data.type=data.type, years=years, sex=T, risk=T, throw.error.if.missing.data = F)
        if (!is.null(cdc.arr))
        {
            tmrv = make.transformation.matrix.and.response.vector(cdc.arr=cdc.arr, jheem.skeleton=jheem.skeleton)
            
            sds = numerator.sd(years=tmrv$year, num=tmrv$response.vector)
            if (length(sds)==1)
                sds = rep(sds, length(tmrv$response.vector))
            bias = bias.fn(years=tmrv$year, num=tmrv$response.vector)
            if (length(bias)==1)
                bias = rep(bias, length(tmrv$response.vector))
            bias.sds = bias.sd(years=tmrv$year, num=tmrv$response.vector)
            if (length(bias.sds)==1)
                bias.sds = rep(bias.sds, length(tmrv$response.vector))
            
            sds = sqrt(sds^2 + bias.sds^2)
            response.vector = c(response.vector, tmrv$response.vector + bias)
            transformation.matrix = rbind(transformation.matrix, tmrv$transformation.matrix)
            
            one.numerator.covar.mat = make.chunked.compound.symmetry.matrix(sds=sds,
                                                                            chunk.rho=numerator.year.to.year.chunk.correlation,
                                                                            non.chunk.rho=numerator.year.to.year.off.correlation,
                                                                            years=tmrv$year,
                                                                            categories=tmrv$non.year.descriptions,
                                                                            chunk.years=numerator.chunk.years)
    #        one.numerator.covar.mat = make.compound.symmetry.matrix(sds=sds,
    #                                                                rho=numerator.year.to.year.correlation)
    
            numerator.covar.mat = join.independent.covariance.matrices(numerator.covar.mat,
                                                                       one.numerator.covar.mat)
            descriptions = c(descriptions, tmrv$descriptions)
        }
    }

    if (by.race.risk)
    {
        cdc.arr = get.surveillance.data(surv, location, data.type=data.type, years=years, race=T, risk=T, throw.error.if.missing.data = F)
        if (!is.null(cdc.arr))
        {
            tmrv = make.transformation.matrix.and.response.vector(cdc.arr=cdc.arr, jheem.skeleton=jheem.skeleton)
            
            sds = numerator.sd(years=tmrv$year, num=tmrv$response.vector)
            if (length(sds)==1)
                sds = rep(sds, length(tmrv$response.vector))
            bias = bias.fn(years=tmrv$year, num=tmrv$response.vector)
            if (length(bias)==1)
                bias = rep(bias, length(tmrv$response.vector))
            bias.sds = bias.sd(years=tmrv$year, num=tmrv$response.vector)
            if (length(bias.sds)==1)
                bias.sds = rep(bias.sds, length(tmrv$response.vector))
            
            sds = sqrt(sds^2 + bias.sds^2)
            response.vector = c(response.vector, tmrv$response.vector + bias)
            transformation.matrix = rbind(transformation.matrix, tmrv$transformation.matrix)
            
            one.numerator.covar.mat = make.chunked.compound.symmetry.matrix(sds=sds,
                                                                            chunk.rho=numerator.year.to.year.chunk.correlation,
                                                                            non.chunk.rho=numerator.year.to.year.off.correlation,
                                                                            years=tmrv$year,
                                                                            categories=tmrv$non.year.descriptions,
                                                                            chunk.years=numerator.chunk.years)
    #        one.numerator.covar.mat = make.compound.symmetry.matrix(sds=sds,
    #                                                                rho=numerator.year.to.year.correlation)
    
            numerator.covar.mat = join.independent.covariance.matrices(numerator.covar.mat,
                                                                       one.numerator.covar.mat)
            descriptions = c(descriptions, tmrv$descriptions)
        }
    }

    missing.response = is.na(response.vector)
    response.vector = response.vector[!missing.response]
    transformation.matrix = transformation.matrix[!missing.response,]
    descriptions = descriptions[!missing.response]
    numerator.covar.mat = numerator.covar.mat[!missing.response,!missing.response]

    if (all(numerator.covar.mat==0))
        numerator.covar.mat = NULL

    list(transformation.matrix = transformation.matrix,
         response.vector = response.vector,
         numerator.covar.mat = numerator.covar.mat,
         descriptions = descriptions)
}

##---------------------------##
##-- PUBLIC-FACING HELPERS --##
##---------------------------##

get.data.cvs <- function(location=BALTIMORE.MSA, surv=msa.surveillance, years=NULL)
{
    rv = list()
    rv$all.new = 1/sqrt(get.surveillance.data(surv, location, years=years, data.type='new'))
    rv$all.prev = 1/sqrt(get.surveillance.data(surv, location, years=years, data.type='prevalence'))
    rv$all.mort = 1/sqrt(get.surveillance.data(surv, location, years=years, data.type='mortality'))
    rv
}

get.equalizing.cv.multipliers <- function(location, surv=msa.surveillance, years=NULL)
{
    cvs = get.data.cvs(location=location, surv=surv, years=years)
    mean.cvs = sapply(cvs, mean, na.rm=T)
    1/(mean.cvs/max(mean.cvs))
}


##----------------------------------------------##
##-- MID-LEVEL HELPERS TO *CREATE* LIKELIHOOD --##
##----------------------------------------------##

make.transformation.matrix.and.response.vector <- function(cdc.arr, jheem.skeleton,
                                                           ages=NULL, races=NULL,
                                                           sexes=NULL, risks=NULL)
{
    melted = melt(cdc.arr, as.is=T)

    if (!is.null(ages) && !is.null(melted$age))
        melted = melted[sapply(melted$age, function(age){any(age==ages)}),]
    if (!is.null(races) && !is.null(melted$race))
        melted = melted[sapply(melted$race, function(race){any(race==races)}),]
    if (!is.null(sexes) && !is.null(melted$sex))
        melted = melted[sapply(melted$sex, function(sex){any(sex==sexes)}),]
    if (!is.null(risks) && !is.null(melted$risk))
        melted = melted[sapply(melted$risk, function(risk){any(risk==risks)}),]

    transformation.matrix = NULL

    if (!is.null(melted$sex) && !is.null(melted$risk))
        melted = melted[melted$sex != 'female' | (melted$risk != 'msm' & melted$risk != 'msm_idu'),]

    for (row in 1:dim(melted)[1])
    {
        one.transformation = jheem.skeleton

        access(one.transformation,
               year=melted$year[row],
               age=melted$age[row],
               race=melted$race[row],
               sex=map.cdc.to.jheem.sex(melted$sex[row], melted$risk[row]),
               risk=map.cdc.to.jheem.risk(melted$risk[row])) = 1

        transformation.matrix = rbind(transformation.matrix, as.numeric(one.transformation))
    }

    if (dim(melted)[2]==1)
        descriptions = rep('all', dim(melted)[1])
    else if (dim(melted)[2]==2)
        descriptions = melted[,dimnames(melted)[[2]]!='value']
    else
        descriptions = apply(melted[,dimnames(melted)[[2]]!='value'], 1, function(row){
            paste0(row, collapse=', ')
        })
    
    if (dim(melted)[2]<=2)
        non.year.descriptions = rep('all', dim(melted)[1])
    else if (dim(melted)[2]==3)
        non.year.descriptions = melted[,dimnames(melted)[[2]]!='value' & dimnames(melted)[[2]]!='year']
    else
        non.year.descriptions = apply(melted[,dimnames(melted)[[2]]!='value' & dimnames(melted)[[2]]!='year'], 1, function(row){
            paste0(row, collapse=', ')
        })

    list(transformation.matrix = transformation.matrix,
         response.vector = as.matrix(melted$value, ncol=1),
         descriptions=descriptions,
         non.year.descriptions=non.year.descriptions,
         year=melted$year)
}

create.simple.denominator.elements <- function(population, msm.cv = msm.cv, idu.cv = idu.cv)
{
    diagonal = array(0, dim=dim(population), dimnames=dimnames(population))

    if (msm.cv!=0)
        access(diagonal, sex='msm') = access(diagonal, sex='msm') +
            access(population, sex='msm') * msm.cv

    if (idu.cv!=0)
    {
        access(diagonal, risk='active_IDU') = access(diagonal, risk='active_IDU') +
            access(population, risk='active_IDU') * idu.cv

        access(diagonal, risk='IDU_in_remission') = access(diagonal, risk='IDU_in_remission') +
            access(population, risk='IDU_in_remission') * idu.cv
    }

    denominator.covar.mat = diag(as.numeric(diagonal))
    if (all(denominator.covar.mat==0))
        denominator.covar.mat = NULL

    list(denominator.vector = as.numeric(population),
         denominator.covar.mat = denominator.covar.mat)
}

create.simple.denominator.elements.from.msm.proportions <- function(pre.msm.population, msm.proportions, idu.cv = 0.25)
{
    diagonal = array(0, dim=dim(pre.msm.population), dimnames=dimnames(pre.msm.population))

    for (race in names(msm.proportions))
    {
        pre.msm.population[,,race,'msm',] = msm.proportions[race] *
            pre.msm.population[,,race,'msm',]

        pre.msm.population[,,race,'heterosexual_male',] = msm.proportions[race] *
            pre.msm.population[,,race,'heterosexual_male',]
    }

    if (idu.cv!=0)
    {
        diagonal[,,,,'active_IDU'] = diagonal[,,,,'active_IDU'] +
            pre.msm.population[,,,,'active_IDU'] * idu.cv

        diagonal[,,,,'IDU_in_remission'] = diagonal[,,,,'IDU_in_remission'] +
            pre.msm.population[,,,,'IDU_in_remission'] * idu.cv
    }

    denominator.covar.mat = diag(as.numeric(diagonal))
    if (all(denominator.covar.mat==0))
        denominator.covar.mat = NULL

    list(denominator.vector = as.numeric(population),
         denominator.covar.mat = denominator.covar.mat)
}


make.compound.symmetry.by.year.matrix <- function(population,
                                                  correlation)
{
    N = prod(dim(population))
    N.year = dim(population)['year']
    N.other = N/N.year

    rv = array(0, dim=c(N.year, N.other, N.year, N.other))
    for (i in 1:N.other)
        rv[,i,,i] = correlation

    dim(rv) = c(N,N)
    diag(rv) = 1

    rv
}

#Base population age x race x sex(M/F)
make.denominator.elements <- function(base.population,
                                      msm.proportions.by.race,
                                      active.idu.prevalence,
                                      idu.ever.prevalence,
                                      target.population.dim.names,
                                      msm.cv,
                                      idu.cv)
{
    #-- Check arguments --#
    if (any(active.idu.prevalence<0) || any(active.idu.prevalence>1))
        stop('All values of active.idu.prevalence must be between 0 and 1')

    if (any(idu.ever.prevalence<0) || any(idu.ever.prevalence>1))
        stop('All values of idu.ever.prevalence must be between 0 and 1')

    if (any(active.idu.prevalence > idu.ever.prevalence))
        stop('All values of active.idu.prevalence must be less than or equal to the corresponding values in idu.ever.prevalence')

    if (any(components$proportions.msm.of.male>1) || any(components$proportions.msm.of.male<0))
        stop("MSM proportions must be between 0 and 1")

    if (any(components$proportions.msm.of.male>0.1))
        warning("Some MSM proportions have been set to be greater than 10% - is this intended?")


    prior.idu.prevalence = idu.ever.prevalence - prior.idu.prevalence

    #-- Set Up Hydrated Arrays --#
    target.from.dim.names = target.to.dim.names = target.population.dim.names
    names(target.from.dim.names) = paste0(names(target.from.dim.names), '.from')
    names(target.to.dim.names) = paste0(names(target.to.dim.names), '.to')
    hydrated.matrix.dim.names = c(target.from.dim.names, target.to.dim.names)

    hydrated.vector = array(0, dim=sapply(target.population.dim.names, length), dimnames=target.population.dim.names)
    hydrated.matrix = array(0, dim=sapply(hydrated.matrix.dim.names, length), dimnames=hydrated.matrix.dim.names)

    for (age in target.population.dim.names[['age']])
    {
        for (race in target.population.dim.names[['race']])
        {
            #-- Female --#
            p.active = access(active.idu.prevalence, age=age, race=race, sex='female')
            p.prior = access(prior.idu.prevalence, age=age, race=race, sex='female')
            base = base.population[age, race, 'female']

            hydrated.vector[age,race,'female','never_IDU'] = base * (1 - p.active - p.prior)
            hydrated.vector[age,race,'female','active_IDU'] = base * p.active
            hydrated.vector[age,race,'female','IDU_in_remission'] = base * p.prior

            hydrated.matrix[age,race,'female','never_IDU',age,race,'female','never_IDU'] = (base * idu.cv)^2 * (p.active^2 + p.prior^2)
            hydrated.matrix[age,race,'female','active_IDU',age,race,'female','active_IDU'] = (base * idu.cv)^2 * p.active^2
            hydrated.matrix[age,race,'female','IDU_in_remission',age,race,'female','IDU_in_remission'] = (base * idu.cv)^2 * p.prior^2
            hydrated.matrix[age,race,'female','never_IDU',age,race,'female','active_IDU'] =
                hydrated.matrix[age,race,'female','active_IDU',age,race,'female','never_IDU'] = -(base * idu.cv)^2 * p.active^2
            hydrated.matrix[age,race,'female','never_IDU',age,race,'female','IDU_in_remission'] =
                hydrated.matrix[age,race,'female','IDU_in_remission',age,race,'female','never_IDU'] = -(base * idu.cv)^2 * p.prior^2

            #-- Male --#
            p.msm = msm.proportions.by.race[race]
            base = base.population[age, race, 'male']
            p.active.het = access(active.idu.prevalence, age=age, race=race, sex='heterosexual_male')
            p.prior.het = access(prior.idu.prevalence, age=age, race=race, sex='heterosexual_male')
            p.active.msm = access(active.idu.prevalence, age=age, race=race, sex='msm')
            p.prior.msm = access(prior.idu.prevalence, age=age, race=race, sex='msm')

            hydrated.vector[age,race,'heterosexual_male','never_IDU'] = base * (1 - p.msm) * (1 - p.active.het - p.prior.het)
            hydrated.vector[age,race,'heterosexual_male','active_IDU'] = base * (1 - p.msm) * p.active.het
            hydrated.vector[age,race,'heterosexual_male','IDU_in_remission'] = base * (1 - p.msm) * p.prior.het

            hydrated.vector[age,race,'msm','never_IDU'] = base * p.msm * (1 - p.active.msm - p.prior.msm)
            hydrated.vector[age,race,'msm','active_IDU'] = base * p.msm * p.active.msm
            hydrated.vector[age,race,'msm','IDU_in_remission'] = base * p.msm * p.prior.msm

            #- The diagonals (variances) -#
            hydrated.matrix[age,race,'heterosexual_male','never_IDU',age,race,'female','never_IDU'] =
                base^2 * ( (p.msm * msm.cv)^2 + (1-p.msm)^2 * idu.cv^2 * (p.active.het^2 + p.prior.het^2) )
            hydrated.matrix[age,race,'heterosexual_male','active_IDU',age,race,'female','active_IDU'] =
                base^2 * ( (p.msm * msm.cv)^2 + (1-p.msm)^2 * idu.cv^2 * p.active.het^2 )
            hydrated.matrix[age,race,'heterosexual_male','IDU_in_remission',age,race,'female','IDU_in_remission'] =
                base^2 * ( (p.msm * msm.cv)^2 + (1-p.msm)^2 * idu.cv^2 * p.prior.het^2 )

            hydrated.matrix[age,race,'msm','never_IDU',age,race,'female','never_IDU'] =
                base^2 * ( (p.msm * msm.cv)^2 + p.msm^2 * idu.cv^2 * (p.active.msm^2 + p.prior.msm^2) )
            hydrated.matrix[age,race,'msm','active_IDU',age,race,'female','active_IDU'] =
                base^2 * ( (p.msm * msm.cv)^2 + p.msm^2 * idu.cv^2 * p.active.msm^2 )
            hydrated.matrix[age,race,'msm','IDU_in_remission',age,race,'female','IDU_in_remission'] =
                base^2 * ( (p.msm * msm.cv)^2 + p.msm^2 * idu.cv^2 * p.prior.msm^2 )

            #- Covariances by MSM -#

            #het with msm
                #never with x
            hydrated.matrix[age,race,'heterosexual_male','never_IDU',age,race,'msm','never_IDU'] =
                hydrated.matrix[age,race,'heterosexual_male','never_IDU',age,race,'msm','active_IDU'] =
                hydrated.matrix[age,race,'heterosexual_male','never_IDU',age,race,'msm','IDU_in_remission'] =
                #active with x
                hydrated.matrix[age,race,'heterosexual_male','active_IDU',age,race,'msm','never_IDU'] =
                hydrated.matrix[age,race,'heterosexual_male','active_IDU',age,race,'msm','active_IDU'] =
                hydrated.matrix[age,race,'heterosexual_male','active_IDU',age,race,'msm','IDU_in_remission'] =
                #remission with x
                hydrated.matrix[age,race,'msm','IDU_in_remission',age,race,'msm','never_IDU'] =
                hydrated.matrix[age,race,'msm','IDU_in_remission',age,race,'msm','active_IDU'] =
                hydrated.matrix[age,race,'msm','IDU_in_remission',age,race,'msm','IDU_in_remission'] =
            #msm with het
                hydrated.matrix[age,race,'msm','never_IDU',age,race,'heterosexual_male','never_IDU'] =
                hydrated.matrix[age,race,'msm','never_IDU',age,race,'heterosexual_male','active_IDU'] =
                hydrated.matrix[age,race,'msm','never_IDU',age,race,'heterosexual_male','IDU_in_remission'] =
                #active with x
                hydrated.matrix[age,race,'msm','active_IDU',age,race,'heterosexual_male','never_IDU'] =
                hydrated.matrix[age,race,'msm','active_IDU',age,race,'heterosexual_male','active_IDU'] =
                hydrated.matrix[age,race,'msm','active_IDU',age,race,'heterosexual_male','IDU_in_remission'] =
                #remission with x
                hydrated.matrix[age,race,'msm','IDU_in_remission',age,race,'heterosexual_male','never_IDU'] =
                hydrated.matrix[age,race,'msm','IDU_in_remission',age,race,'heterosexual_male','active_IDU'] =
                hydrated.matrix[age,race,'msm','IDU_in_remission',age,race,'heterosexual_male','IDU_in_remission'] =
            #the actual value
                -(base * p.msm * msm.cv)^2

            #- Covariances by IDU -#

            hydrated.matrix[age,race,'heterosexual_male','never_IDU',age,race,'heterosexual_male','active_IDU'] =
                hydrated.matrix[age,race,'heterosexual_male','active_IDU',age,race,'heterosexual_male','never_IDU'] =
                (base * p.msm * msm.cv)^2 - (base * (1-p.msm) * idu.cv * p.active.het)^2

            hydrated.matrix[age,race,'heterosexual_male','never_IDU',age,race,'heterosexual_male','IDU_in_remission'] =
                hydrated.matrix[age,race,'heterosexual_male','IDU_in_remission',age,race,'heterosexual_male','never_IDU'] =
                (base * p.msm * msm.cv)^2 - (base * (1-p.msm) * idu.cv * p.prior.het)^2

            hydrated.matrix[age,race,'heterosexual_male','active_IDU',age,race,'heterosexual_male','IDU_in_remission'] =
                hydrated.matrix[age,race,'heterosexual_male','IDU_in_remission',age,race,'heterosexual_male','active_IDU'] =
                (base * p.msm * msm.cv)^2


            hydrated.matrix[age,race,'msm','never_IDU',age,race,'msm','active_IDU'] =
                hydrated.matrix[age,race,'msm','active_IDU',age,race,'msm','never_IDU'] =
                (base * p.msm * msm.cv)^2 - (base * p.msm * idu.cv * p.active.msm)^2

            hydrated.matrix[age,race,'msm','never_IDU',age,race,'msm','IDU_in_remission'] =
                hydrated.matrix[age,race,'msm','IDU_in_remission',age,race,'msm','never_IDU'] =
                (base * p.msm * msm.cv)^2 - (base * p.msm * idu.cv * p.prior.msm)^2

            hydrated.matrix[age,race,'msm','active_IDU',age,race,'msm','IDU_in_remission'] =
                hydrated.matrix[age,race,'msm','IDU_in_remission',age,race,'msm','active_IDU'] =
                (base * p.msm * msm.cv)^2

        }
    }
}

get.sd.inflation <- function(match.to.response = likelihood.elements$response.vector,
                             match.to.descriptions = likelihood.elements$descriptions,
                             response.2 = sd.inflation.elements$response.vector,
                             descriptions.2 = sd.inflation.elements$descriptions)
{
    stop('we are not using this')
    match.to.year = as.numeric(substr(match.to.descriptions, 1,4))
    year.2 = as.numeric(substr(descriptions.2, 1,4))

    match.to.catg = substr(match.to.descriptions, 7,1000)
    catg.2 = substr(descriptions.2, 7,1000)

    match.to.cv = sqrt(match.to.response)
    cv.2 = sqrt(response.2)

    exact.match.indices = sapply(match.to.descriptions, function(match.to){
        (1:length(descriptions.2))[descriptions.2==match.to][1]
    })
    sd.inflation = cv.2[exact.match.indices] / match.to.cv

    no.exact.match = is.na(exact.match.indices)
    match.catg = lapply(match.to.catg[no.exact.match], function(match.to){

    })

    inexact.sd.inflation = sapply((1:length(match.to.descriptions))[no.exact.match], function(i){
        match.catg.indices = (1:length(catg.2))[catg.2==match.to.catg[i]]
        match.catg.years = year.2[match.catg.indices]
        year = match.to.year[i]

        min.year.dist = min(abs(year-match.catg.years))
        min.year.mask = abs(year-match.catg.years)==min.year.dist

        matched.cvs = cv.2[match.catg.indices][]
    })
    browser()
    #I gave up and abandoned this here
}

map.cdc.to.jheem.sex <- function(sex, risk)
{
    if (is.null(risk))
    {
        if (is.null(sex))
            NULL
        else
        {
            if (sex=='male')
                c('msm','heterosexual_male')
            else if (sex=='female')
                c('female')
        }
    }
    else
    {
        if (is.null(sex))
        {
            if (risk=='msm' || risk=='msm_idu')
                'msm'
            else if (risk=='idu' || risk=='heterosexual')
                c('heterosexual_male', 'female')
        }
        else
        {
            if (risk=='msm' || risk=='msm_idu')
                'msm'
            else if (sex=='male')
                'heterosexual_male'
            else if (sex=='female')
                'female'
        }
    }
}

map.cdc.to.jheem.risk <- function(risk)
{
    if (is.null(risk))
        NULL
    else
    {
        if (risk=='msm' || risk=='heterosexual')
            'never_IDU'
        else if (risk=='idu' || risk=='msm_idu')
            c('active_IDU', 'IDU_in_remission')
        else
            stop("Risk must be 'msm', 'idu', 'msm_idu', or 'heterosexual'")
    }
}



##----------------------------------------------##
##-- LOW-LEVEL HELPERS TO *CREATE* LIKELIHOOD --##
##----------------------------------------------##

make.compound.symmetry.matrix <- function(sds, rho)
{
    rho.mat = matrix(rho, nrow=length(sds), ncol=length(sds))
    diag(rho.mat) = 1
    sds %*% t(sds) * rho.mat
}

make.chunked.compound.symmetry.matrix <- function(sds,
                                                  chunk.rho,
                                                  non.chunk.rho,
                                                  years,
                                                  categories,
                                                  chunk.years)
{
    rho.mat = matrix(non.chunk.rho, nrow=length(years), ncol=length(years))

    for (chunk in chunk.years)
    {
        mask = sapply(years, function(year){any(year==chunk)})
        rho.mat[mask,mask] = chunk.rho
    }
    
    #set correlation to 0 for any non-equal categories
    equal.category.mask = sapply(categories, function(c1){
        categories==c1
    })
    rho.mat[!equal.category.mask] = 0

    diag(rho.mat) = 1
    sds %*% t(sds) * rho.mat
}

join.independent.covariance.matrices <- function(m1, m2)
{
    if (is.null(m1))
        return (m2)
    if (is.null(m2))
        return (m1)

    n1 = dim(m1)[1]
    n2 = dim(m2)[1]
    rv = matrix(0, nrow=n1+n2, ncol=n1+n2)

    rv[1:n1,1:n1] = m1
    rv[n1+1:n2,n1+1:n2] = m2

    rv
}
