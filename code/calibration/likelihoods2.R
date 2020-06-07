
##--------------------------------------##
##--     CREATING THE LIKELIHOOD      --##
##-- (components and actual function) --##
##--------------------------------------##

create.full.likelihood.components <- function()
{
    
}

create.likelihood.from.components <- function(likelihood.components)
{
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
        
        piecewise = sapply(likelihood.components, function(lik){
            
            if (lik$type=='general')
                calculate.general.likelihood(lik, sim, log=log)
        })
        
        if (log)
            sum(piecewise)
        else
            prod(piecewise)
    }
}

##----------------------------##
##-- THE GENERAL LIKELIHOOD --##
##----------------------------##


create.general.likelihood.components <- function(data.type=c('new','prevalence','mortality')[1],
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
                                      sd.inflation.for.observations=1,
                                      population=get.census.totals(ALL.DATA.MANAGERS$census.totals, location),
                                      denominator.dimensions='year',
                                      year.to.year.correlation=0,
                                      numerator.year.to.year.chunk.correlation=0,
                                      numerator.year.to.year.off.correlation=0,
                                      numerator.chunk.years=numeric(),
                                      numerator.sd = function(...){0},
                                      bias.fn = function(...){0},
                                      bias.sd = function(...){0})
{
    rv = list(type='general',
              data.type=data.type)
    
    ##---------------------------##
    ##-- PULL THE YEARS TO USE --##
    ##---------------------------##
    
    #If years parameter is null, use all years for which there are data
    data.for.all = get.surveillance.data(surv, location, data.type=data.type)
    available.years = attr(data.for.all, 'years')
    if (is.null(years))
        years = available.years
    else
        years = intersect(years, available.years)
    lik$years = years

    ##-----------------------------##
    ##-- SET UP THE DENOMINATORS --##    
    ##-----------------------------##
    
    #Set up the population to use as denominators
    population = access(population, year=as.character(years))
    target.dim.names = list(year=years,
                            age=SETTINGS$AGES$labels,
                            race=SETTINGS$RACES,
                            sex=SETTINGS$SEXES,
                            risk=SETTINGS$RISK_STRATA)#dimnames(population)
    population = apply(population, denominator.dimensions, sum)
    population = expand.population(population, target.dim.names = target.dim.names)

    denominator.elements = create.simple.denominator.elements(population,
                                                                    msm.cv = 0, idu.cv = 0)
    
    rv$denominator.vector = denominator.elements$denominator.vector
    rv$denominator.covar.mat = denominator.elements$denominator.covar.mat
    
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

    rv$response.vector = likelihood.elements$response.vector
    rv$transformation.matrix = likelihood.elements$transformation.matrix
    rv$descriptions = likelihood.elements$descriptions

        
    if (is.null(likelihood.elements$numerator.covar.mat))
        numerator.covar.mat = NULL
    else
        numerator.covar.mat = likelihood.elements$numerator.covar.mat * sd.inflation.for.observations^2
    rv$numerator.covar.mat = numerator.covar.mat
    
    # Set up the correlation matrix, if needed
    if (year.to.year.correlation == 0)
        corr.mat = NULL
    else
        corr.mat = make.compound.symmetry.by.year.matrix(access(population, year=as.character(years)), correlation=year.to.year.correlation)
    rv$corr.mat = corr.mat
    
    ##------------------------------------------------------------------------------------##
    ##-- Set up a mask to pull out components we don't use in the transformation matrix --##
    ##------------------------------------------------------------------------------------##
    
    denominators.are.totals = length(denominator.dimensions)==1 & denominator.dimensions=='year'
    if (is.null(corr.mat) &&
        !use.simulated.denominators &&
        !(use.sim.msm.proportions && !aggregate.denominator.males && any(denominator.dimensions=='sex')))
    {
        transformation.mapping = make.transformation.mapping(likelihood.elements$transformation.matrix,
                                                             likelihood.elements$denominator.vector)
        
        likelihood.elements$transformation.matrix = likelihood.elements$transformation.matrix[,transformation.mapping$first.in.signature]
        fixed.denominator.elements$denominator.vector = fixed.denominator.elements$denominator.vector[transformation.mapping$first.in.signature]
        
        if (!is.null(fixed.denominator.elements$denominator.covar.mat))
            fixed.denominator.elements$denominator.covar.mat = fixed.denominator.elements$denominator.covar.mat[transformation.mapping$first.in.signature,transformation.mapping$first.in.signature]
    }
    else
        transformation.mapping = NULL
    rv$transformation.mapping = transformation.mapping
    
    ##-----------------------------------------------------##
    ##-- CALCULATE THE SD INFLATION (IF FROM A FUNCTION) --##
    ##-----------------------------------------------------##
    
    if (is(sd.inflation, 'function'))
        sd.inflation = sd.inflation(rv$descriptions)
    rv$sd.inflation = sd.inflation
    
    
    ##------------##
    ##-- Return --##
    ##------------##
    
    rv
}

calculate.general.likelihood <- function(lik,
                                         sim,
                                         log=T)
{
    rates = pull.simulations.rates(sim,
                                   data.type=lik$data.type,
                                   years=lik$years,
                                   denominator.dimensions=lik$denominator.dimensions)
    
    # Pass it all to the sub-function to crunch
    general.likelihood.sub(rates,
                           transformation.matrix = lik$transformation.matrix,
                           response.vector = lik$response.vector,
                           denominator.vector = lik$denominator.vector,
                           denominator.covar.mat = lik$denominator.covar.mat,
                           numerator.covar.mat = lik$numerator.covar.mat,
                           corr.mat = lik$corr.mat,
                           sd.inflation = lik$sd.inflation,
                           log=log,
                           sim=sim,
                           transformation.mapping=lik$transformation.mapping,
                           description =lik$descriptions)
    
}

#Once we have numerators and rates, run a multivariate normal that is summed up
general.likelihood.sub <- function(pre.transformation.rates,
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
        p = pre.transformation.rates
        p.1mp = pre.transformation.rates * (1 - pre.transformation.rates)
    }
    else
    {
        p = sum.for.matrix.mapping(pre.transformation.rates, transformation.mapping)
        p.1mp = sum.for.matrix.mapping(pre.transformation.rates * (1 - pre.transformation.rates),
                                       transformation.mapping)
    }
    
    binomial.variance.component = denominator.vector * p.1mp
    
    if (is.null(corr.mat))
        binomial.variance.component = diag(binomial.variance.component)
    else
    {
        sds = sqrt(binomial.variance.component)
        binomial.variance.component = sds %*% t(sds) * corr.mat
    }
    
    mean.vector = transformation.matrix %*% (denominator.vector * p)
    
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
    
    if (length(sd.inflation)==1)
        covar.mat = covar.mat * sd.inflation^2
    else
        covar.mat = covar.mat * (sd.inflation %*% t(sd.inflation))
    
    if (!is.null(numerator.covar.mat))
        covar.mat = covar.mat + numerator.covar.mat
    
    dmvnorm(x=response.vector,
            mean=mean.vector,
            sigma=covar.mat,
            log=log)
}

##-- HELPERS FOR THE GENERAL LIKELIHOOD --##

create.simple.denominator.elements <- function(population, msm.cv = 0, idu.cv = 0)
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



##---------------------------##
##-- LIKELIHOOD FOR TOTALS --##
##---------------------------##

create.total.likelihood <- function(data.type,
                                    years=NULL,
                                    surv=msa.surveillance,
                                    location=BALTIMORE.MSA,
                                    population=get.census.totals(ALL.DATA.MANAGERS$census.totals, location),
                                    numerator.sd=function(...){0},
                                    sd.inflation=1,
                                    ar=T,
                                    rho=0.7)
{
    if (data.type!='new' && data.type!='prevalence' && data.type!='mortality')
        stop("data.type must be either 'new', 'prevalence', or 'diagnosed'")
    
    observed = get.surveillance.data(msa.surveillance, location=location, data.type=data.type)
    available.years = attr(observed, 'years')[!is.na(observed)]
    if (is.null(years))
        years = available.years
    else
        years = intersect(years, available.years)
    observed = as.numeric(observed[as.character(years)])
    
    if (is.null(dim(population)))
        denominators = population[as.character(years)]
    else
        denominators = apply(population, 'year', sum)[as.character(years)]
    
    if (ar)
    {
        year.diff = matrix(abs(rep(years, length(years)) - rep(years, each=length(years))),
                           nrow=length(years), ncol=length(years))
        cor.mat = rho^year.diff
    }
    else
    {
        cor.mat = matrix(rho, nrow=length(years), ncol=length(years))
        diag(cor.mat) = 1
    }
    
    numerator.sds = numerator.sd(year=years, num=observed)
    if (length(numerator.sds)==1)
        numerator.sds = rep(numerator.sds, length(years))
    
    function(sim, log=T)
    {
        if (data.type=='new')
            sim.rates = extract.new.diagnoses(sim,
                                              years=years,
                                              keep.dimensions = 'year',
                                              per.population = 1)
        else if (data.type=='prevalence')
            sim.rates = extract.prevalence(sim, continuum='diagnosed',
                                           years=years,
                                           keep.dimensions = 'year',
                                           per.population = 1)
        else if (data.type=='mortality')
            sim.rates = extract.overall.hiv.mortality(sim, continuum='diagnosed',
                                                      years=years,
                                                      keep.dimensions = 'year',
                                                      per.population = 1)
        else
            stop("data.type must be either 'new', 'prevalence', or 'diagnosed'")
        
        sds = sqrt(denominators * sim.rates * (1-sim.rates))
        mean.vector = denominators * sim.rates
        
        cov.mat = sds %*% t(sds) * cor.mat * sd.inflation + diag(numerator.sds^2)
        dmvnorm(x=observed, mean=mean.vector, sigma = cov.mat, log=log)
    }
}

##-----------------------------------------##
##-- LIKELIHOOD FOR PROPORTION DIAGNOSED --##
##-----------------------------------------##


create.diagnosed.likelihood <- function(years=NULL,
                                        surv=state.surveillance,
                                        location=BALTIMORE.MSA,
                                        numerator.sd=function(...){0},
                                        sd.inflation=1,
                                        ar=F,
                                        rho=0.55)
{
    means.and.sds = get.diagnosed.means.and.sds(location=location, surv=surv, years=years)
    mean.diagnosed = means.and.sds$means
    sds = means.and.sds$sds * sd.inflation
    years = means.and.sds$years
    
    not.na.mask = !is.na(mean.diagnosed)
    mean.diagnosed = mean.diagnosed[not.na.mask]
    
    if (ar)
    {
        year.diff = matrix(abs(rep(years, length(years)) - rep(years, each=length(years))),
                           nrow=length(years), ncol=length(years))
        cov.mat = sds %*% t(sds) * rho^year.diff
    }
    else
    {
        cov.mat = matrix(rho, nrow=length(years), ncol=length(years))
        diag(cov.mat) = 1
        cov.mat = sds %*% t(sds) * cov.mat
    }
    
    numerator.sds = numerator.sd(year=years, num=mean.diagnosed)
    if (length(numerator.sds)==1)
        numerator.sds = rep(numerator.sds, length(years))
    cov.mat = cov.mat + diag(numerator.sds^2)
    
    function(sim, log=T)
    {
        sim.diagnosed = extract.diagnosed.hiv(sim, years=years, keep.dimensions = 'year')
        dmvnorm(sim.diagnosed, mean=mean.diagnosed, sigma=cov.mat, log=log)
    }
}

##------------------------------------##
##-- LIKELIHOOD FOR IDU PROPORTIONS --##
##------------------------------------##

##----------------------------------------------##
##-- LIKELIHOOD FOR CUMULATIVE AIDS MORTALITY --##
##----------------------------------------------##

##-----------------------------------##
##-- LIKELIHOOD FOR AIDS DIAGNOSES --##
##-----------------------------------##


create.aids.diagnoses.likelihood <- function(surv=msa.surveillance,
                                             location=BALTIMORE.MSA,
                                             numerator.sd=function(years,num){rep(0,length(num))},
                                             sd.inflation=1,
                                             years=1999:2003,
                                             population=get.census.totals(ALL.DATA.MANAGERS$census.totals,
                                                                          location, years=years, flatten.single.dim.array = T),
                                             hiv.to.aids.diagnoses.ratio=c('1999'=1.45,
                                                                           '2000'=1.56,
                                                                           '2001'=1.51,
                                                                           '2002'=1.39,
                                                                           '2003'=1.35,
                                                                           '2004'=1.25)[as.character(years)],
                                             hiv.to.aids.diagnoses.ratio.log.sd=0.5*log(1.1),
                                             rho=0.5,
                                             verbose=F
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
    
    function(sim, log=T)
    {
        numerators = extract.new.diagnoses(sim, years=years, keep.dimensions = 'year', per.population = NA)
        denominators = extract.population.subset(sim, years=years, keep.dimensions = 'year',
                                                 per.population = NA)
        
        rates = numerators / denominators
        
        #  sds = sqrt(population * rates * (1-rates) * sd.inflation^2 + obs.var)
        
        
        cov.mat = make.compound.symmetry.matrix(sqrt(obs.var), rho) + 
            diag(population * rates * (1-rates) * sd.inflation^2)
        
        if (verbose)
            print(cbind(obs=obs, sim=rates*population))
        
        dmvnorm(x=as.numeric(obs),
                mean=as.numeric(rates*population),
                sigma=cov.mat, 
                log=log)
        
    }
}
