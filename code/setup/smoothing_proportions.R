#source('../code/data_managers/continuum_manager.R')


smooth.proportions.by.strata <- function(continuum.manager,
                                         data.type=c('suppression','linkage','testing')[1],
                                         msa,
                                         max.proportion,
                                         desired.years,
                                         population,
                                         constrain.to.total=T,
                                         age1.log.or.intercept=0,
                                         age1.log.or.slope=0,
                                         use.mult.smoothing=F,
                                         min.slope=0,
                                         anchor.year=2020,
                                         extra.total.or=1,
                                         extra.msm.or=1,
                                         extra.idu.or=1,
                                         extra.black.or=1,
                                         extra.hispanic.or=1,
                                         extra.total.slope.or=1,
                                         extra.total.slope.after.anchor.or=1,
                                         black.log.or.slope=0,
                                         hispanic.log.or.slope=0,
                                         age1.msm.log.or.slope=0,
                                         age2.msm.log.or.slope=0,
                                         age1.msm.log.or.intercept=0,
                                         age2.msm.log.or.intercept=0)
{
    # Pull Data-type specific parameters
    if (data.type=='suppression')
    {
        cm.subset = continuum.manager$suppression
        years = get.suppressed.proportion.years(continuum.manager)
    }
    else if (data.type=='linkage')
    {
        cm.subset = continuum.manager$linkage
        years = get.linkage.years(continuum.manager)
    }
    else if (data.type=='testing')
    {
        cm.subset = continuum.manager$testing
        years = get.testing.rate.years(continuum.manager)
    }
    else
        stop("The smooth.proportions.by.strata function only works for data.type 'suppression', 'linkage', or 'testing'")


    population = apply(population, c('age','race','sex','risk'), sum)

    # Pull total proportion distributions
    total.odds.dists = lapply(years, function(year){
        total.odds.dist = do.get.total.odds.distribution(cm.subset, msa, year)
        total.odds.dist
    })
    total.mask = !sapply(total.odds.dists, is.null)
    if (!any(total.mask))
        stop("No data in the continuum manager for the msa: ", msa)

    n.top.line = length(total.odds.dists[total.mask][[1]]$mean)
    months = total.odds.dists[total.mask][[1]]$months

    # Pull stratum-specific log-ORs
    log.ors.by.stratum.list = lapply(years, function(year){
        or.dists = do.get.continuum.or.distributions(cm.subset, msa, year, months=months)
        if (is.null(or.dists))
            NULL
        else
            mat = make.log.or.matrix(black.lor = or.dists$mean['black'] + log(extra.black.or),
                                     hispanic.lor = or.dists$mean['hispanic'] + log(extra.hispanic.or),
                                     age1.lor = or.dists$mean['age1'],
                                     age2.lor = or.dists$mean['age2'],
                                     age4.lor = or.dists$mean['age4'],
                                     age5.lor = or.dists$mean['age5'],
                                     msm.lor = or.dists$mean['msm'] + log(extra.msm.or),
                                     female.lor = or.dists$mean['female'],
                                     idu.lor = or.dists$mean['idu'] + log(extra.idu.or),
                                     msm.idu.lor = or.dists$mean['msm.idu'] + log(extra.idu.or),
                                     use.msm.lor.for.msm.idu=F
            )
    })

    stratum.mask = sapply(log.ors.by.stratum.list, function(mat){
        !is.null(mat) & all(!is.na(mat))
    })

    strata.years = years[stratum.mask]
    log.ors.by.stratum = t(sapply(log.ors.by.stratum.list[stratum.mask], function(mat){
        mat
    }))
    
    target.dim.names = c(list(year=strata.years), dimnames(log.ors.by.stratum.list[stratum.mask][[1]]))
    dim(log.ors.by.stratum) = sapply(target.dim.names, length)
    dimnames(log.ors.by.stratum) = target.dim.names

    if (constrain.to.total)
        base.intercept = base.slope = 0
    else
    {
        base.intercept = log(extra.total.or)
        base.slope = log(extra.total.slope.or)
    }

    additional.intercepts = array(base.intercept, dim=sapply(target.dim.names[-1], length), dimnames=target.dim.names[-1])
    additional.intercepts[1,,,] = additional.intercepts[1,,,] + age1.log.or.intercept
    additional.intercepts[1,,'msm',] = additional.intercepts[1,,'msm',] + age1.msm.log.or.intercept
    additional.intercepts[2,,'msm',] = additional.intercepts[2,,'msm',] + age2.msm.log.or.intercept

    additional.slopes = array(base.slope, dim=sapply(target.dim.names[-1], length), dimnames=target.dim.names[-1])
    additional.slopes[1,,,] = additional.slopes[1,,,] +  age1.log.or.slope
    additional.slopes[,'black',,] = additional.slopes[,'black',,] + black.log.or.slope
    additional.slopes[,'hispanic',,] = additional.slopes[,'hispanic',,] + hispanic.log.or.slope

    additional.slopes[1,,'msm',] = additional.slopes[1,,'msm',] + age1.msm.log.or.slope
    additional.slopes[2,,'msm',] = additional.slopes[2,,'msm',] + age2.msm.log.or.slope

    #Mash them together for each top-line total proportion
    rv.dim.names = c(list(year=desired.years), target.dim.names[-1])
    rv = array(as.numeric(NA), dim=sapply(rv.dim.names, length), dimnames=rv.dim.names)
    for (i in 1:n.top.line)
    {
        total.log.odds = sapply(total.odds.dists, function(dist){
            if (is.null(dist))
                NA
            else
                dist$mean[i]
        })
        total.mask = !is.na(total.log.odds)
        total.log.odds = total.log.odds[total.mask]
        total.log.odds = total.log.odds + log(extra.total.or)

        total.years = years[total.mask]
        total.p = 1 / (1 + exp(-total.log.odds))

        if (n.top.line==1)
        {
            log.ors = log.ors.by.stratum
            pop = population
            add.intercepts = additional.intercepts
            add.slopes = additional.slopes
        }
        else
        {
            type = names(total.odds.dists[total.mask][[1]]$mean)[i]

            if (type=='heterosexual')
            {
                log.ors = log.ors.by.stratum[,,,c('heterosexual_male','female'),'never_IDU']
                pop = population[,,c('heterosexual_male','female'),'never_IDU']
                add.intercepts = additional.intercepts[,,c('heterosexual_male','female'),'never_IDU']
                add.slopes = additional.slopes[,,c('heterosexual_male','female'),'never_IDU']
            }
            else if (type=='msm')
            {
                log.ors = log.ors.by.stratum[,,,'msm','never_IDU']
                pop = population[,,'msm','never_IDU']
                add.intercepts = additional.intercepts[,,'msm','never_IDU']
                add.slopes = additional.slopes[,,'msm','never_IDU']
            }
            else if (type=='idu')
            {
                log.ors = log.ors.by.stratum[,,,,c('active_IDU','IDU_in_remission')]
                pop = population[,,,c('active_IDU','IDU_in_remission')]
                add.intercepts = additional.intercepts[,,,c('active_IDU','IDU_in_remission')]
                add.slopes = additional.slopes[,,,c('active_IDU','IDU_in_remission')]
            }
            else if (type=='msm.idu')
            {
                log.ors = log.ors.by.stratum[,,,'msm',c('active_IDU','IDU_in_remission')]
                pop = population[,,'msm',c('active_IDU','IDU_in_remission')]
                add.intercepts = additional.intercepts[,,'msm',c('active_IDU','IDU_in_remission')]
                add.slopes = additional.slopes[,,'msm',c('active_IDU','IDU_in_remission')]
            }
            else
                stop("Type must be either 'heterosexual', 'msm', 'idu', or 'msm.idu'")
        }

        if (use.mult.smoothing)
        {
            stop("We have not added additional total slopes into mult smoothing")
            smooth.fn = do.smooth.proportions.by.strata.mult
        }
        else
            smooth.fn = do.smooth.proportions.by.strata

        smoothed = smooth.fn(total.proportions = total.p,
                             total.years = total.years,
                             max.proportion = max.proportion,
                             constrain.to.total = constrain.to.total,
                             log.ors.by.stratum = log.ors,
                             strata.years = strata.years,
                             n.per.stratum = pop,
                             desired.years = desired.years,
                             additional.intercepts = add.intercepts,
                             additional.slopes = add.slopes,
                             anchor.year=anchor.year,
                             additional.total.slope.lor=log(extra.total.slope.or),
                             additional.total.slope.lor.after.anchor=log(extra.total.slope.after.anchor.or),
                             min.slope=min.slope)
        if (n.top.line==1)
            rv = smoothed
        else
        {
            if (type=='heterosexual')
            {
                rv[,,,c('heterosexual_male','female'),'never_IDU'] = smoothed
            }
            else if (type=='msm')
            {
                rv[,,,'msm','never_IDU'] = smoothed
            }
            else if (type=='idu')
            {
                rv[,,,,c('active_IDU','IDU_in_remission')] = smoothed
            }
            else if (type=='msm.idu')
            {
                rv[,,,'msm',c('active_IDU','IDU_in_remission')] = smoothed
            }
            else
                stop("Type must be either 'heterosexual', 'msm', 'idu', or 'msm.idu'")
        }
    }

    rv
}

#first dimension of strata is year
do.smooth.proportions.by.strata <- function(total.proportions,
                                            total.years,
                                            constrain.to.total=T,
                                            max.proportion,
                                            log.ors.by.stratum,
                                            strata.years,
                                            n.per.stratum,
                                            desired.years,
                                            anchor.year=min(desired.years, total.years, strata.years),
                                            additional.intercepts, #but get averaged to total
                                            additional.slopes,
                                            min.slope,
                                            additional.total.slope.lor.after.anchor=0,
                                            additional.total.slope.lor=0
)
{
    target.dim.names = dimnames(log.ors.by.stratum)
    target.dim.names[['year']] = as.character(desired.years)

    total.years.prime = total.years - anchor.year
    desired.years.prime = desired.years - anchor.year
    strata.years.prime = strata.years - anchor.year

    scaled.p = pmax(.005, pmin(.995, total.proportions/max.proportion))
    scaled.log.odds = log(scaled.p) - log(1-scaled.p)

    fit.total = lm(scaled.log.odds~total.years.prime)
    total.intercept = fit.total$coefficients[1]
    total.slope = fit.total$coefficients[2] + additional.total.slope.lor

    n.strata.years = length(strata.years)
    n.strata = length(log.ors.by.stratum)/n.strata.years
    strata.fits = lapply(1:n.strata, function(i){
        log.odds = log.ors.by.stratum[(i-1)*n.strata.years + 1:n.strata.years]
        lm(log.odds~strata.years.prime)
    })

    strata.weights = n.per.stratum / sum(n.per.stratum)

    raw.strata.intercepts = sapply(strata.fits, function(fit){fit$coefficients[1]}) + additional.intercepts
    raw.strata.slopes = sapply(strata.fits, function(fit){fit$coefficients[2]}) + additional.slopes

    if (constrain.to.total)
    {
        strata.intercept0 = total.intercept - sum(raw.strata.intercepts * strata.weights)
        strata.slope0 = total.slope - sum(raw.strata.slopes * strata.weights)
    }
    else
        strata.intercept0 = strata.slope0 = 0

    strata.intercepts = raw.strata.intercepts + strata.intercept0
    strata.slopes = raw.strata.slopes + strata.slope0

    if (constrain.to.total && total.slope < min.slope)
        strata.slopes[1:length(strata.slopes)] = 0
    else
    {
        counter = 1

        slope.too.low = strata.slopes < min.slope

        while(any(strata.slopes<min.slope))
        {
            if (counter>1000)
                strata.slopes[] = min.slope
            else
            {
                slope.too.low = slope.too.low | strata.slopes < min.slope

                if (constrain.to.total && all(strata.slopes <= min.slope) && any(strata.slopes < min.slope))
                    stop(paste0("Unable to achieve a total slope greater than ", min.slope))

                strata.slopes[slope.too.low] = min.slope

                if (any(!slope.too.low) && constrain.to.total)
                {
                    strata.slope0 = (total.slope - sum(strata.slopes * strata.weights)) / sum(strata.weights[!slope.too.low])
                    strata.slopes[!slope.too.low] = strata.slopes[!slope.too.low] + strata.slope0
                }

                counter = counter+1
            }
        }
    }
    
    smoothed.log.odds = sapply(1:n.strata, function(i){
        strata.intercepts[i] + desired.years.prime * strata.slopes[i] + pmax(0,desired.years.prime) * additional.total.slope.lor.after.anchor
    })

    smoothed.scaled.p = 1 / (1+exp(-smoothed.log.odds))
    smoothed.p = smoothed.scaled.p * max.proportion
    dim(smoothed.p) = sapply(target.dim.names, length)
    dimnames(smoothed.p) = target.dim.names

    smoothed.p
}

#first dimension of strata is year
do.smooth.proportions.by.strata.mult <- function(total.proportions,
                                            total.years,
                                            max.proportion,
                                            log.ors.by.stratum,
                                            strata.years,
                                            n.per.stratum,
                                            desired.years,
                                            anchor.year=min(desired.years, total.years, strata.years),
                                            additional.intercepts,
                                            additional.slopes
)
{
    stop("This is nto trustworthy")
    total.years.prime = total.years - anchor.year
    desired.years.prime = desired.years - anchor.year
    strata.years.prime = strata.years - anchor.year

    scaled.p = total.proportions/max.proportion
    scaled.log.odds = log(scaled.p) - log(1-scaled.p)

    fit.total = lm(scaled.log.odds~total.years.prime)
    total.intercept = fit.total$coefficients[1]
    total.slope = fit.total$coefficients[2]

    n.strata.years = length(strata.years)
    n.strata = length(log.ors.by.stratum)/n.strata.years
    strata.fits = lapply(1:n.strata, function(i){
        log.odds = log.ors.by.stratum[(i-1)*n.strata.years + 1:n.strata.years]
        lm(log.odds~strata.years.prime)
    })

    strata.weights = n.per.stratum / sum(n.per.stratum)

    raw.strata.intercepts = sapply(strata.fits, function(fit){fit$coefficients[1]}) + additional.intercepts
    raw.strata.slopes = sapply(strata.fits, function(fit){fit$coefficients[2]}) + additional.slopes

    strata.intercept0 = total.intercept - sum(raw.strata.intercepts * strata.weights)
    strata.slope.mult = total.slope / sum(raw.strata.slopes * strata.weights)

    strata.intercepts = raw.strata.intercepts + strata.intercept0
    strata.slopes = raw.strata.slopes * strata.slope.mult

    target.dim.names = dimnames(log.ors.by.stratum)
    target.dim.names[['year']] = as.character(desired.years)

    smoothed.log.odds = sapply(1:n.strata, function(i){
        strata.intercepts[i] + desired.years.prime * strata.slopes[i]
    })

    smoothed.scaled.p = 1 / (1+exp(-smoothed.log.odds))
    smoothed.p = smoothed.scaled.p * max.proportion
    dim(smoothed.p) = sapply(target.dim.names, length)
    dimnames(smoothed.p) = target.dim.names

    smoothed.p
}









