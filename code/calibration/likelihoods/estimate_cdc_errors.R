library(jheem) #for access
library(gee)

#source('../code/data_managers/hiv_surveillance_manager.R')
#load('../code/cached/msa.surveillance.Rdata')
#source('../code/baltimore.R')
#load('../code/cached/state.surveillance.Rdata')

get.total.numbers <- function(location=BALTIMORE.MSA, surv=msa.surveillance, years=NULL)
{
    inc = get.surveillance.data(surv, location, years=years, data.type='new')
    prev = get.surveillance.data(surv, location, years=years, data.type='prevalence')
    mort = get.surveillance.data(surv, location, years=years, data.type='mortality')

    list(new=inc[!is.na(inc)],
         prevalence=prev[!is.na(prev)],
         mortality=mort[!is.na(mort)])
}

get.cv.weights <- function(location=BALTIMORE.MSA,
                           surv=msa.surveillance,
                           state.surv=state.surveillance,
                           years=2010:2016,
                           weight.to='new')
{
    nums = get.total.numbers(location, surv, years)
    cvs = lapply(nums, function(one.nums){1/sqrt(one.nums)})
    mean.cvs = sapply(cvs, mean)

    dx.data = get.diagnosed.means.and.sds(location=location, surv=state.surv, years=years)


    mean.cvs = c(mean.cvs, diagnosed=mean(dx.data$sds/dx.data$means))

    mean.cvs[weight.to] / mean.cvs
}

get.diagnosed.ar.estimate <- function(surv=state.surveillance)
{
    df = melt(surv$diagnosed.all)
    df = df[!is.na(df$value),]

#    fit = geeglm(value~year, data=df, id = df$location, corstr='ar1')
#     fit = gee(value~year+location, data=df, id = df$location, corstr='AR-M')
    fit = gee(value~year, data=df, id = df$location, corstr='exchangeable')
    fit$working.correlation[2,1]
}

get.diagnosed.cvs <- function(location=BALTIMORE.MSA,
                              surv=state.surveillance,
                              years=NULL)
{
    dd = get.diagnosed.means.and.sds(location=location,
                                     surv=surv,
                                     years=years)
    
    dd$sds/dd$means
}

get.diagnosed.means.and.sds <- function(location=BALTIMORE.MSA,
                              surv=state.surveillance,
                              years=NULL)
{
    if (!any(dimnames(surv$diagnosed.all)[['location']]==location))
        location = states.for.msa(location)

    dx = get.surveillance.data(surv, location, years=years, data.type='diagnosed', aggregate.locations=F)
    years = attr(dx, 'years')

    est.prev = get.surveillance.data(surv, location, years=years, data.type='estimated.prevalence', aggregate.locations=F)
    ci.lower = get.surveillance.data(surv, location, years=years, data.type='estimated.prevalence.ci.lower', aggregate.locations=F)
    ci.upper = get.surveillance.data(surv, location, years=years, data.type='estimated.prevalence.ci.upper', aggregate.locations=F)
    est.sds = (ci.upper-ci.lower) / qnorm(0.975) / 2
    cvs = est.sds / est.prev

    dx.sds = cvs * dx

    list(means=rowMeans(dx),
         sds=sqrt(rowSums(dx.sds^2)/dim(dx)[2]^2),
         years=years)
}

#use years 2016
get.cdc.errors <- function(use.age.and.race=T, const.sd=0,
                           years=2016,
                           min.size=0, max.size=Inf,
                           use.total.msa.vs.atlas=T,
                           estimand=c('mult','exp','mult.exp')[1],
                           data.types=c('new','prevalence'))
{
    #-- Load up the two data sources (estimated and not) --#
    if (use.total.msa.vs.atlas)
    {
        load('cached_surveillance/msa.surveillance.not.estimated.Rdata')
        estimated.data = msa.surveillance
        load('cached_surveillance/msa.surveillance.estimated.correct.to.county.Rdata')
        nonestimated.data = msa.surveillance
    }
    else
    {
        load('cached_surveillance/msa.surveillance.estimated.Rdata')
        estimated.data = msa.surveillance
        load('cached_surveillance/msa.surveillance.not.estimated.Rdata')
        nonestimated.data = msa.surveillance
    }

    rv = numeric()
    for (data.type in data.types)
        rv = c(rv,
               get.cdc.errors.for.data.type(data.type, estimated.data, nonestimated.data,
                                       years=years,
                                       use.age.and.race = use.age.and.race,
                                       min.size=min.size, max.size=max.size,
                                       const.sd=const.sd,
                                       estimand=estimand))

    names(rv) = data.types
    rv
}

get.cdc.errors.for.data.type <- function(data.type,
                                         estimated.data,
                                         nonestimated.data,
                                         years,
                                         use.age.and.race=T,
                                         use.total=F,
                                         const.sd=0,
                                         min.size=10,
                                         max.size=Inf,
                                         estimand)
{
    # Which years have estimated vs not
    available.years = intersect(dimnames(nonestimated.data[[paste0(data.type, '.all')]])[['year']],
                      dimnames(estimated.data[[paste0(data.type, '.all')]])[['year']])
    if (is.null(years))
        years = available.years
    else
        years = intersect(years, available.years)

    if (length(years)>1)
    {
        mask = !apply(access(nonestimated.data[[paste0(data.type, '.all')]], year=years)==
                          access(estimated.data[[paste0(data.type, '.all')]], year=years),
                      1, all, na.rm=T)
        years = years[mask]
    }

    # Set up the vectors
    if (use.total)
    {
        noadj = as.numeric(nonestimated.data[[paste0(data.type, '.all')]][years,])
        adj = as.numeric(estimated.data[[paste0(data.type, '.all')]][years,])
    }
    else if (use.age.and.race)
    {
        noadj = c(as.numeric(nonestimated.data[[paste0(data.type, '.sex.race')]][years,,,]),
                  as.numeric(nonestimated.data[[paste0(data.type, '.sex.age')]][years,,,]))
        adj = c(as.numeric(estimated.data[[paste0(data.type, '.sex.race')]][years,,,]),
                as.numeric(estimated.data[[paste0(data.type, '.sex.age')]][years,,,]))
    }
    else
    {
        noadj = as.numeric(nonestimated.data[[paste0(data.type, '.sex')]][years,,])
        adj = as.numeric(estimated.data[[paste0(data.type, '.sex')]][years,,])
    }

    # Process it
    truth = adj
    measured = noadj
    error = measured - truth

    # apply min/max size
    mask = !is.na(truth) & !is.na(error) & truth >= min.size & truth <= max.size
    truth = truth[mask]
    measured = measured[mask]
    error = error[mask]

    # MLE
    if (estimand=='exp')
    {
        exp.mle = find.exp.sd.mle(errors=error, x=truth)

        names(exp.mle) = paste0('exp.', data.type)
        exp.mle
    }
    else if (estimand=='mult.exp')
    {
        mle = find.mult.exp.mle(errors=error, x=truth)

        names(mle) = paste0(names(mle), '.', data.type)
        mle
    }
    else #estimand = mult
    {
        if (const.sd==0)
            cv.mle = find.cv.mle(errors=error, x=truth)
        else
            cv.mle = find.cv.mle.given.const(errors=error, x=truth, const.sd=const.sd)

        names(cv.mle) = paste0('mult.', data.type)
        cv.mle
    }
}

get.cdc.error.correlations <- function(min.size=0, max.size=Inf,
                           use.total.msa.vs.atlas=T,
                           years=2010:2017)
{
    #-- Load up the two data sources (estimated and not) --#
    if (use.total.msa.vs.atlas)
    {
        load('../code/cached_surveillance/msa.surveillance.not.estimated.Rdata')
        estimated.data = msa.surveillance
        load('../code/cached_surveillance/msa.surveillance.estimated.correct.to.county.Rdata')
        nonestimated.data = msa.surveillance
    }
    else
    {
        load('../code/cached_surveillance/msa.surveillance.estimated.Rdata')
        estimated.data = msa.surveillance
        load('../code/cached_surveillance/msa.surveillance.not.estimated.Rdata')
        nonestimated.data = msa.surveillance
    }
    list(new=get.cdc.error.correlations.for.data.type('new', estimated.data, nonestimated.data,
                                       min.size=min.size, max.size=max.size, years=years),
      prevalence=get.cdc.error.correlations.for.data.type('prevalence', estimated.data, nonestimated.data,
                                              min.size=min.size, max.size=max.size, years=years))
}

get.cdc.error.correlations.for.data.type <- function(data.type,
                                         estimated.data,
                                         nonestimated.data,
                                         min.size=10,
                                         max.size=Inf,
                                         years)
{
    # Which years have estimated vs not
    possible.years = intersect(dimnames(nonestimated.data[[paste0(data.type, '.all')]])[['year']],
                      dimnames(estimated.data[[paste0(data.type, '.all')]])[['year']])
    mask = !apply(access(nonestimated.data[[paste0(data.type, '.all')]], year=possible.years)==
                      access(estimated.data[[paste0(data.type, '.all')]], year=possible.years),
                  1, all, na.rm=T)
    possible.years = possible.years[mask]

    if (is.null(years))
        years = possible.years
    else
        years = intersect(years, possible.years)

    # Set up the vectors
    noadj = t(nonestimated.data[[paste0(data.type, '.all')]][years,])
    adj = t(estimated.data[[paste0(data.type, '.all')]][years,])

    # Process it
    truth = adj
    measured = noadj
    error = measured - truth

    # apply min/max size
    mask = apply(!is.na(truth) & !is.na(error) & truth >= min.size & truth <= max.size, 1, all)
    truth = truth[mask,]
    measured = measured[mask,]
    error = error[mask,]

    std.error = error/truth
   # dimnames(std.error) = NULL


    if (data.type=='new')
    {
        y1.min = 2010
        y1.max = 2014
        y2.min = 2015
        y2.max = 2017
    }
    else
    {
        y1.min = 2010
        y1.max = 2013
        y2.min = 2014
        y2.max = 2016
    }

    raw.cor = cor(std.error)

    df1 = melt(std.error[,as.character(y1.min:y1.max)])
    df1 = df1[order(df1$location),]
    df2 = melt(std.error[,as.character(y2.min:y2.max)])
    df2 = df2[order(df2$location),]

    fit1 = gee(value~1, data=df1, id = df1$location, corstr='exchangeable')
    fit2 = gee(value~1, data=df2, id = df2$location, corstr='exchangeable')

    rv = list(raw=raw.cor,
         rho1=fit1$working.correlation[2,1],
         rho2=fit2$working.correlation[2,1]
    )

    names(rv)[2] = paste0('rho_',y1.min, "_", y1.max)
    names(rv)[3] = paste0('rho_',y2.min, "_", y2.max)

    rv
}

find.cv.mle <- function(errors, x, na.rm=T, remove.zero=T)
{
 #   return (empiric.find.cv.mle(errors, x, na.rm, remove.zero))
    if (na.rm)
    {
        mask = !is.na(x) & !is.na(errors)
        x = x[mask]
        errors = errors[mask]
    }
    else if (any(is.na(errors) | is.na(x)))
        stop("Cannot have NA values in errors or x")

    if (any(x==0))
    {
        if (remove.zero)
        {
            mask = x != 0
            x = x[mask]
            errors = errors[mask]
        }
        else
            stop("No values of x may be zero. Consider setting remove.zero=T")
    }

    mle.phi.sq = sum(errors^2/x^2) / length(errors)
    sqrt(mle.phi.sq)
}

empiric.find.cv.mle <- function(errors, x, na.rm=t, remove.zero=T)
{
    if (na.rm)
    {
        mask = !is.na(x) & !is.na(errors)
        x = x[mask]
        errors = errors[mask]
    }
    else if (any(is.na(errors) | is.na(x)))
        stop("Cannot have NA values in errors or x")


    if (any(x==0))
    {
        if (remove.zero)
        {
            mask = x != 0
            x = x[mask]
            errors = errors[mask]
        }
        else
            stop("No values of x may be zero. Consider setting remove.zero=T")
    }

    fn <- function(cv)
    {
        sds = cv * x
        -sum(dnorm(errors, 0, sds, log=T))
    }

    opt = optimize(f=fn, interval=c(0,100))
    opt$minimum
}

find.exp.sd.mle<-function(errors, x, na.rm=T, remove.lte.one=T, remove.zero=T)
{
    if (na.rm)
    {
        mask = !is.na(x) & !is.na(errors)
        x = x[mask]
        errors = errors[mask]
    }
    else if (any(is.na(errors) | is.na(x)))
        stop("Cannot have NA values in errors or x")


    if (any(x==0))
    {
        if (remove.zero)
        {
            mask = x != 0
            x = x[mask]
            errors = errors[mask]
        }
        else
            stop("No values of x may be zero. Consider setting remove.zero=T")
    }

    fn <- function(phi)
    {
        lik = sum(-phi*log(x) - 0.5*errors^2*x^(-2*phi))
        -lik
    }

    opt = optimize(f=fn, interval=c(0,100))
    opt$minimum
}

find.mult.exp.mle<-function(errors, x, na.rm=T, remove.lte.one=T, remove.zero=T, maxit=1000)
{
    if (na.rm)
    {
        mask = !is.na(x) & !is.na(errors)
        x = x[mask]
        errors = errors[mask]
    }
    else if (any(is.na(errors) | is.na(x)))
        stop("Cannot have NA values in errors or x")


    if (any(x==0))
    {
        if (remove.zero)
        {
            mask = x != 0
            x = x[mask]
            errors = errors[mask]
        }
        else
            stop("No values of x may be zero. Consider setting remove.zero=T")
    }

    fn <- function(params)
    {
        cv = exp(params[1])
        phi = exp(params[2])
#        sds = cv * x^phi
        sds = sqrt((cv*x)^2 + (x^phi)^2)

        lik = sum(dnorm(errors, 0, sds, log=T))
        -lik
    }

    opt = optim(c(log(1), log(0.5)), fn=fn,
                control=list(maxit=maxit))

    c(mult=exp(opt$par[1]),
         exp=exp(opt$par[2]))
}


find.const.plus.cv.mles <- function(errors, x, na.rm=T, maxit=500)
{
    if (na.rm)
    {
        mask = !is.na(x) & !is.na(errors)
        x = x[mask]
        errors = errors[mask]
    }
    else if (any(is.na(errors) | is.na(x)))
        stop("Cannot have NA values in errors or x")


    fn <- function(params)
    {
        theta = exp(params[1])
        phi = exp(params[2])
        sds = sqrt(theta^2 + (x*phi)^2)

        lik = sum(dnorm(errors, 0, sds, log=T))
        -lik
    }

    opt = optim(c(log(5), log(0.1)), fn=fn,
                control=list(maxit=maxit))

    list(const.sd=exp(opt$par[1]),
         cv=exp(opt$par[2]))
}

find.cv.mle.given.const <- function(errors, x, const.sd=5, na.rm=T, maxit=500)
{
    if (na.rm)
    {
        mask = !is.na(x) & !is.na(errors)
        x = x[mask]
        errors = errors[mask]
    }
    else if (any(is.na(errors) | is.na(x)))
        stop("Cannot have NA values in errors or x")


    fn <- function(phi)
    {
        sds = sqrt(const.sd^2 + (x*phi)^2)

        lik = sum(dnorm(errors, 0, sds, log=T))
        -lik
    }

    opt = optimise(f=fn, interval=c(0,1))

    opt$minimum
}

