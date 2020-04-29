
#library(mcmc.sim)
library(ggplot2)
library(reshape2)
library(scales)
#source('../code/data_managers/hiv_surveillance_manager.R')

RED = '#C75D4D'
GREEN = '#458B00'
#GREEN = 'darkgreen'
BLUE = '#3278FA'
ORANGE = 'darkorange3'

DEFAULT.SHAPES = c(21,24,23,22,25)

TRUE.COLOR = GREEN
MODEL.COLOR = BLUE
MODEL.COLOR.2 = RED

plot.calibration.risk <- function(sims,
                                  risk=c('msm','idu','heterosexual'),
                                  years=2010:2020,
                                  data.types=c('new','prevalence'),
                                  surv=msa.surveillance,
                                  location=NULL,
                                  race=NULL,
                                  population=NULL,
                                  use.cdc=T,
                                  ci.coverage=0.95,
                                  ...
)
{
    plot.calibration(sims=sims,
                     split.by=NULL,
                     facet.by = 'risk',
                     risk=risk,
                     years=years,
                     data.types=data.types,
                     race=race,
                     surv=surv,
                     location=location,
                     population=population,
                     ci.coverage=ci.coverage,
                     ...)
}

plot.calibration.race <- function(sims,
                                  race=c('black','hispanic','other'),
                                  years=2010:2020,
                                  data.types=c('new','prevalence'),
                                  risk=c('msm','heterosexual','idu'),
                                  surv=msa.surveillance,
                                  location=NULL,
                                  population=NULL,
                                  use.cdc=T,
                                  ci.coverage=0.95,
                                  ...
)
{
    plot.calibration(sims=sims,
                     split.by=NULL,
                     facet.by = 'race',
                     risk=risk,
                     race=race,
                     years=years,
                     data.types=data.types,
                     surv=surv,
                     location=location,
                     population=population,
                     use.cdc=use.cdc,
                     ci.coverage=ci.coverage,
                     ...)
}

plot.calibration.race.risk <- function(sims,
                                       race=c('black','hispanic','other'),
                                       risk=c('msm','idu','heterosexual'),
                                       years=2010:2020,
                                       data.types=c('new','prevalence'),
                                       surv=msa.surveillance,
                                       location=NULL,
                                       population=NULL,
                                       use.cdc=T,
                                       ci.coverage=0.95,
                                       ...
)
{
    plot.calibration(sims=sims,
                     split.by= 'race',
                     facet.by = 'risk',
                     race=race,
                     risk=risk,
                     years=years,
                     data.types=data.types,
                     surv=surv,
                     location=location,
                     use.cdc=use.cdc,
                     ci.coverage=ci.coverage,
                     ...)
}

plot.calibration.risk.race <- function(sims,
                                  race=c('black','hispanic','other'),
                                  risk=c('msm','idu','heterosexual'),
                                  years=2010:2020,
                                  data.types=c('new','prevalence'),
                                  surv=msa.surveillance,
                                  location=NULL,
                                  population=NULL,
                                  use.cdc=T,
                                  ci.coverage=0.95,
                                  ...
)
{
    plot.calibration(sims=sims,
                     split.by= 'risk',
                     facet.by = 'race',
                     race=race,
                     risk=risk,
                     years=years,
                     data.types=data.types,
                     surv=surv,
                     location=location,
                     population=population,
                     use.cdc=use.cdc,
                     ci.coverage=ci.coverage,
                     ...)
}


plot.calibration.sex <- function(sims,
                                  sex=c('male','female'),
                                  years=2010:2020,
                                  data.types=c('new','prevalence','mortality'),
                                  surv=msa.surveillance,
                                  location=NULL,
                                  population=NULL,
                                  use.cdc=T,
                                  ci.coverage=0.95,
                                 ...
)
{
    plot.calibration(sims=sims,
                     split.by=NULL,
                     facet.by = 'sex',
                     sex=sex,
                     years=years,
                     data.types=data.types,
                     surv=surv,
                     location=location,
                     population=population,
                     use.cdc=use.cdc,
                     ci.coverage=ci.coverage,
                     ...)
}

plot.calibration.age <- function(sims,
#                                 age=c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years" ),
                                 years=2010:2020,
                                 data.types=c('new','prevalence'),
                                 surv=msa.surveillance,
                                 location=NULL,
                                 population=NULL,
                                 use.cdc=T,
                                 ci.coverage=0.95,
                                facet.data.type.first=T,
                                ncol=5,
                                 ...
)
{
    plot.calibration(sims=sims,
                     split.by=NULL,
                     facet.by = 'age',
                     years=years,
                     data.types=data.types,
                     surv=surv,
                     location=location,
                     population=population,
                     use.cdc=use.cdc,
                     ci.coverage=ci.coverage,
                     facet.data.type.first=facet.data.type.first,
                     ncol=ncol,
                     ...)
}


plot.calibration.sex.age <- function(sims,
                                     #                                 age=c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years" ),
                                     years=2010:2020,
                                     data.types=c('new','prevalence'),
                                     surv=msa.surveillance,
                                     location=NULL,
                                     population=NULL,
                                     use.cdc=T,
                                     ci.coverage=0.95,
                                     facet.data.type.first=T,
                                     ncol=5,
                                     ...
)
{
    plot.calibration(sims=sims,
                     split.by='sex',
                     facet.by = 'age',
                     years=years,
                     data.types=data.types,
                     surv=surv,
                     location=location,
                     population=population,
                     use.cdc=use.cdc,
                     ci.coverage=ci.coverage,
                     facet.data.type.first=facet.data.type.first,
                     ncol=ncol,
                     ...)
}

plot.calibration.age.sex <- function(sims,
                                 #                                 age=c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years" ),
                                 years=2010:2020,
                                 data.types=c('new','prevalence'),
                                 surv=msa.surveillance,
                                 location=NULL,
                                 population=NULL,
                                 use.cdc=T,
                                 ci.coverage=0.95,
                                 ...
)
{
    plot.calibration(sims=sims,
                     split.by='age',
                     facet.by = 'sex',
                     years=years,
                     data.types=data.types,
                     surv=surv,
                     location=location,
                     population=population,
                     use.cdc=use.cdc,
                     ci.coverage=ci.coverage,
                     ...)
}

plot.calibration.sex.risk <- function(sims,
                                     #                                 age=c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years" ),
                                     years=2010:2020,
                                     data.types=c('new','prevalence'),
                                     surv=msa.surveillance,
                                     location=NULL,
                                     population=NULL,
                                     use.cdc=T,
                                     ci.coverage=0.95,
                                     facet.data.type.first=T,
                                     ncol=3,
                                     ...
)
{
    plot.calibration(sims=sims,
                     split.by='sex',
                     facet.by = 'risk',
                     years=years,
                     data.types=data.types,
                     surv=surv,
                     location=location,
                     population=population,
                     use.cdc=use.cdc,
                     ci.coverage=ci.coverage,
                     facet.data.type.first=facet.data.type.first,
                     ncol=ncol,
                     ...)
}

plot.calibration.total <- function(sims,
                                   years=2010:2020,
                                   data.types=c('new','prevalence','mortality','diagnosed'),
                                   surv=msa.surveillance,
                                   location=NULL,
                                   population=NULL,
                                   use.cdc=T,
                                   ci.coverage=0.95,
                                   ...
)
{
    plot.calibration(sims=sims,
                     split.by=NULL,
                     facet.by = NULL,
                     years=years,
                     data.types=data.types,
                     surv=surv,
                     location=location,
                     population=population,
                     use.cdc=use.cdc,
                     ci.coverage=ci.coverage,
                     ...)
}

plot.calibration.idu.prevalence <- function(sims,
                                years=2014:2016,
                                x.variable=c('risk'),
                                population=BALTIMORE.POPULATION,
                                facet.by='race',
                                split.by=NULL,
                                use.cdc=F,
                                risk=c('Active_IDU', 'IDU_in_remission'),
                                plot.individual.simset.sims=F,
                                fixed.facet.scales=T,
                                use.bar=T,
                                ...)
{
    plot.calibration(sims,
                     data.types='population',
                     population=BALTIMORE.POPULATION,
                     years=years,
                     x.variable=x.variable,
                     facet.by=facet.by,
                     split.by=split.by,
                     use.cdc=use.cdc,
                     plot.individual.simset.sims=F,
                     denominator.dimensions = facet.by,
                     risk=risk,
                     fixed.facet.scales=fixed.facet.scales,
                     use.bar=use.bar,
                     ...)
}

plot.calibration.total.incidence <- function(sims,
                                   years=2010:2030,
                                   plot.cdc.new.with.incidence=F,
                                   surv=msa.surveillance,
                                   location=NULL,
                                   population=NULL,
                                   use.cdc=T,
                                   ci.coverage=0.95,
                                   ...
)
{
    plot.calibration(sims=sims,
                     split.by=NULL,
                     facet.by = NULL,
                     years=years,
                     data.types='incidence',
                     linetype.by.sim = F,
                     plot.cdc.new.with.incidence = plot.cdc.new.with.incidence,
                     surv=surv,
                     location=location,
                     population=population,
                     ci.coverage=ci.coverage,
                     ...)
}

plot.calibration.age.distribution <- function(sims,
                                              facet.by='sex',
                                              years=2013:2017,
                                              use.bar=T,
                                              data.types=c('new','prevalence'),
                                              ...)
{
    plot.calibration(sims = sims,
                     years=years,
                     data.types=data.types,
                     x.variable='age',
                     facet.by=facet.by,
                     split.by = NULL,
                     use.bar = use.bar,
                     ...)
}

plot.calibration <- function(sims,
                             data.types=c('new','prevalence','mortality','diagnosed','population')[1:2],
                             years=2010:2020,
                             split.by=NULL,
                             facet.by=c('risk'),
                             x.variables='year',
                             surv=msa.surveillance,
                             location=NULL,
                             population=NULL,#if(use.cdc) BALTIMORE.POPULATION.CDC else BALTIMORE.POPULATION,
                             risk=c('msm','idu','heterosexual'),
                             race=NULL,
                             sex=NULL,
                             use.cdc=T,
                             point.size=4,
                             ncol=NULL,
                             show.points=!(plot.individual.simset.sims && (is(sims, 'simset') || (is(sims, 'list') && is(sims[[1]], 'simset')))),
                             linetype.by.sim=F,#is(sims, 'list'),
                             facet.data.type.first=T,
                             use.total.population.as.denominator=T,
                             denominator.dimensions = 'year',
                             aggregate.denominator.males=T,
                             use.sim.msm.proportions=T,
                             show.rates=F,
                             ci.coverage=0.95,
                             ribbon.alpha=0.4,
                             plot.cdc.new.with.incidence=T,
                             plot.individual.simset.sims=T,
                             sim1.color=BLUE,
                             sim2.color=RED,
                             cdc.color=GREEN,
                             sim1.shape=21,
                             sim2.shape=22,
                             cdc.shape=23,
                             sim1.line.size=if (plot.individual.simset.sims && (is(sims, 'simset') || (is(sims, 'list') && is(sims[[1]], 'simset')))) 0.1 else 1,
                             sim2.line.size=sim1.line.size,
                             cdc.line.size=if (plot.individual.simset.sims && (is(sims, 'simset') || (is(sims, 'list') && is(sims[[1]], 'simset')))) 2 else 1,
                             sim1.alpha=if (plot.individual.simset.sims && (is(sims, 'simset') || (is(sims, 'list') && is(sims[[1]], 'simset')))) 0.1 else 1,
                             sim2.alpha=sim1.alpha,
                             cdc.alpha=1,
                             split.shapes=c(21,22,23,24,25),
                             shape.by.split = length(split.by)>0,
                             new.name = if (show.rates) 'New Diagnoses (per 100,000)' else if (normalize.within.facet) 'New Diagnoses (%)' else 'New Diagnoses (number of cases)',
                             prevalence.name = if (show.rates) 'Prevalence (per 100,000)' else if (normalize.within.facet) 'Prevalence (%)' else 'Prevalence (number of cases)',
                             mortality.name = if (show.rates) 'HIV Mortality (per 100,000 PWH)' else if (normalize.within.facet) 'HIV Mortality (%)' else 'HIV Mortality (number of cases)',
                             diagnosed.name = 'PWH with Diagnosed HIV (%)',
                             population.name = 'Population (%)',
                             incidence.name = if (show.rates) 'Incidence (per 100,000)' else 'Incidence (number of cases)',
                             cdc.label = 'CDC',
                             normalize.within.facet=F,
                             fixed.facet.scales=F,
                             use.bar=F

)
{
    #-- Set up the sims argument --#
    if (!is(sims, 'list'))
        sims = list(sims)

    input.is.simset = is(sims[[1]], 'simset')

    if (is.null(location) || is.na(location) || is(location, 'function'))
    {
        if (is(sims[[1]], 'jheem.results'))
            location = attr(sims[[1]], 'location')
        else if (is(sims[[1]], 'simset'))
            location = attr(sims[[1]]@simulations[[1]], 'location')
        else
            stop("sims must be either a jheem.results object or a simset, or a list containing only jheem.results objects or only simsets")
    }

    if (is.null(population))
        population = get.census.totals(ALL.DATA.MANAGERS$census.totals, location)

    if (is.null(names(sims)))
    {
        if (length(sims)==1 && input.is.simset)
            names(sims) = 'Simulation Set'
        else if (length(sims)==1)
            names(sims)='Simulation'
        else if (input.is.simset)
            names(sims) = paste0("Simulation Set ", 1:length(sims))
        else
            names(sims) = paste0("Simulation ", 1:length(sims))
    }

    #-- Check the location --#
    if (!is.null(surv) && !is.null(location) && !is.na(location))
    {
        if (!has.location.surveillance(surv, location))
            location = states.for.msa(location)
    }

    #-- Clean split.by and facet.by --#
    facet.by = order.jheem.dimensions(facet.by)
    split.by = order.jheem.dimensions(split.by)
    facet.by = setdiff(facet.by, split.by)

    facet.by = setdiff(facet.by, x.variables)
    split.by = setdiff(split.by, x.variables)

    all.dimensions = c(x.variables, split.by, facet.by)
    all.plus.denominator.dimensions = order.jheem.dimensions(union(denominator.dimensions, all.dimensions))

    #-- Set Up Aliases and Denominators by data type --#
    data.type.names = c(new=new.name,
                        prevalence=prevalence.name,
                        mortality=mortality.name,
                        diagnosed=diagnosed.name,
                        population=population.name,
                        incidence=incidence.name)
    if (show.rates)
        data.type.denominators = c(new=100000, prevalence=100000, mortality=100000, population=100, diagnosed=100, incidence=100000)
    else
        data.type.denominators = c(new=1, prevalence=1, mortality=1, population=100, diagnosed=100, incidence=1)

    race.names = c(black='Black',hispanic="Hispanic", other='Other')
    risk.names = c(msm='MSM', idu='IDU', msm_idu='MSM+IDU', heterosexual='Heterosexual',
                   idu_in_remission="IDU in Remission", active_idu='Active IDU', never_idu='Never IDU')

    #-- Set up data frame --#
    df = NULL

    #-- Other Settings and Checks --#
    if (use.bar)
        plot.individual.simset.sims=F

    #-- Set up dimensions --#
    types = c(names(sims), cdc.label)


    #-- Loop through and pull data --#s
    for (data.type in data.types)
    {
        for (type in types)
        {
            #Set up denominators
            #In general, we are going to pull the years we need from the population given
            #If years are missing from the given population, will just use the nearest year
            if (data.type=='diagnosed' || data.type=='population')
                denominators = 1
            else
                denominators = get.denominator.population(population=population,
                                                          years=years,
                                                          denominator.dimensions=denominator.dimensions,
                                                          aggregate.denominator.males = aggregate.denominator.males,
                                                          msm.proportions = if (use.sim.msm.proportions) attr(jheem.results, 'msm.proportions.by.race') else NULL,
                                                          non.year.target.dim.names = dimnames(population)[all.dimensions])

            if (type==cdc.label)
            {
                if (data.type=='population')
                {
                    truth.numerators = apply(access(population, year=as.character(years)), all.dimensions, sum)
                    if (is.null(dim(truth.numerators)))
                        truth.denominators = truth.numerators
                    else
                        truth.denominators = apply(truth.numerators, facet.by, sum)
                    truth.denominators = expand.population(truth.denominators, target.dim.names=dimnames(truth.numerators))
                    truth.numerators = truth.numerators/truth.denominators
                }
                else
                {
                    if (data.type=='incidence')
                    {
                        if (plot.cdc.new.with.incidence)
                            data.type.for.surveillance = 'new'
                        else
                            data.type.for.surveillance = NULL
                    }
                    else
                        data.type.for.surveillance = data.type

                    if (!is.null(data.type.for.surveillance))
                    {
                        truth.numerators = get.surveillance.data(surv, location.codes=location, data.type=data.type.for.surveillance,
                                                                 age=any(all.dimensions=='age'), race=any(all.dimensions=='race'),
                                                                 sex=any(all.dimensions=='sex'), risk=any(all.dimensions=='risk'),
                                                                 aggregate.locations = T, aggregate.years = F,
                                                                 throw.error.if.missing.data = F)
                    }
                    else
                        truth.numerators = NULL

                    if (!is.null(truth.numerators))
                    {
                        truth.years = intersect(years, as.numeric(dimnames(truth.numerators)[['year']]))
                        truth.numerators = access(truth.numerators, year=as.character(truth.years), collapse.length.one.dimensions = F)
                        truth.numerators = apply(truth.numerators, all.plus.denominator.dimensions, function(x){x})
                    }
                }

                if (!is.null(truth.numerators))
                {
                    if (is.null(dim(truth.numerators)))
                    {
                        truth.num.names = names(truth.numerators)
                        dim(truth.numerators) = c(year=length(truth.numerators))
                        dimnames(truth.numerators) = list(year=truth.num.names)
                    }


                    dimnames(truth.numerators) = to.lower.list(dimnames(truth.numerators))

                    if (!show.rates || data.type=='population' || data.type=='diagnosed')
                        values = truth.numerators
                    else if (any(all.dimensions=='sex') && use.cdc)
                    {
                        values = truth.numerators
                        access(values, sex='male') = access(truth.numerators, sex='male') / access(denominators, sex='male')
                        access(values, sex='female') = access(truth.numerators, sex='female') / access(denominators, sex='female')
                    }
                    else
                        values = truth.numerators /
                                    expand.population(access(denominators, year=dimnames(numerators)[['year']], collapse.length.one.dimensions = F),
                                                      dimnames(numerators))

                    #if (!any(all.dimensions=='year'))
                    #{
                    #    dim.names = dimnames(values)
                    #    dim.names = dim.names[names(dim.names) != 'year']
                    #    values = apply(values, names(dim.names), sum)
                    #    dim(values) = sapply(dim.names, length)
                    #    dimnames(values) = dim.names
                    #}
                    #else
                    values = apply(values, all.dimensions, sum)
                    if (length(all.dimensions)==1)
                    {
  #                      dim.names = list(names(values))
 #                       names(dim.names) = all.dimensions

   #                     values = array(values, dim=sapply(dim.names, length), dimnames=dim.names)
##                        dim.names = dimnames(values)[all.dimensions]

                        one.df = data.frame(z=names(values),
                                            value=values)
                        names(one.df)[1] = all.dimensions
                    }
                    else
                        one.df = melt(values)

                    one.df$group = rep(type, dim(one.df)[1])
                    one.df$ci.lower = one.df$ci.upper = rep(as.numeric(NA), dim(one.df)[1])
                }
                else
                    one.df = NULL
            }
            else
            {
                one.sim = sims[[type]]
                if (is(one.sim, 'simset'))
                {
                    if (plot.individual.simset.sims)
                    {
                        one.df = NULL
                        for (i in 1:sims[[type]]@n.sim)
                        {
                            sim = sims[[type]]@simulations[[i]]

                            values = get.sim.values(sim,
                                                    data.type=data.type,
                                                    years=years,
                                                    all.dimensions=all.dimensions,
                                                    use.cdc=use.cdc,
                                                    facet.by=facet.by,
                                                    denominators=denominators,
                                                    denominator.dimensions=denominator.dimensions,
                                                    show.rates=show.rates)

                            if (length(all.dimensions)==1)
                            {
                                dim.names = list(names(values))
                                names(dim.names) = all.dimensions

                                values = array(values, dim=sapply(dim.names, length), dimnames=dim.names)

                                one.sub.df = data.frame(zz=names(values),
                                                    value=values)
                                names(one.sub.df)[1] = all.dimensions
                            }
                            else
                                one.sub.df = melt(values)

                            one.sub.df$group = rep(paste0(type, '.', i), dim(one.sub.df)[1])
                            one.df = rbind(one.df, one.sub.df)
                        }

                        one.df$ci.lower = one.df$ci.upper = rep(as.numeric(NA), dim(one.df)[1])
                    }
                    else
                    {
                        if (show.rates)
                            true.denominators = 1
                        else
                            true.denominators = denominators

                        dist.summary = get.simset.projections(one.sim,
                                                              data.type=data.type,
                                                              years=years,
                                                              keep.dimensions=all.dimensions,
                                                              facet.by=facet.by,
                                                              use.cdc=use.cdc,
                                                              denominator.dimensions = denominator.dimensions,
                                                              true.denominators = true.denominators,
                                                              aggregate.denominator.males=aggregate.denominator.males,
                                                              use.sim.msm.proportions=use.sim.msm.proportions,
                                                              ci.coverage=ci.coverage)

                        one.df = melt(dist.summary$mean)
                        one.df$group = rep(type, dim(one.df)[1])

                        one.df$ci.lower = melt(dist.summary$ci.lower)$value
                        one.df$ci.upper = melt(dist.summary$ci.upper)$value

                    }
                }
                else
                {
                   values = get.sim.values(sims[[type]],
                                              data.type=data.type,
                                              years=years,
                                              all.dimensions=all.dimensions,
                                              use.cdc=use.cdc,
                                              facet.by=facet.by,
                                              denominators=denominators,
                                              denominator.dimensions=denominator.dimensions,
                                              show.rates=show.rates)

                    if (length(all.dimensions)==1)
                    {
                        dim.names = list(names(values))
                        names(dim.names) = all.dimensions

                        values = array(values, dim=sapply(dim.names, length), dimnames=dim.names)

                        one.df = data.frame(x=names(values),
                                            value=values)
                        names(one.df)[1] = x.variables[1]
                    }
                    else
                        one.df = melt(values)

                    one.df$group = rep(type, dim(one.df)[1])
                    one.df$ci.lower = one.df$ci.upper = rep(as.numeric(NA), dim(one.df)[1])
                }
            }

            if (!is.null(one.df) && dim(one.df)[1]>0)
            {
                one.df$value = one.df$value * data.type.denominators[data.type]
                one.df$ci.lower = one.df$ci.lower * data.type.denominators[data.type]
                one.df$ci.upper = one.df$ci.upper * data.type.denominators[data.type]

                if (!is.null(risk) && any(all.dimensions=='risk'))
                    one.df = one.df[sapply(tolower(one.df$risk), function(one.risk){any(one.risk==tolower(risk))}),]
                if (!is.null(race) && any(all.dimensions=='race'))
                    one.df = one.df[sapply(tolower(one.df$race), function(one.race){any(one.race==tolower(race))}),]
                if (!is.null(sex) && any(all.dimensions=='sex'))
                    one.df = one.df[sapply(tolower(one.df$sex), function(one.sex){any(one.sex==tolower(sex))}),]

                #      one.df$split.by = sapply(1:dim(one.df)[1], function(i){
                #          paste0(one.df[,split.by], collapse='\n')
                #     })

                if (any(all.dimensions=='risk'))
                    one.df$risk = risk.names[tolower(one.df$risk)]

                if (any(all.dimensions=='race'))
                    one.df$race = race.names[tolower(one.df$race)]

                if (!is.null(split.by) && length(split.by)>0)
                {
                    one.df$split.by = one.df[,split.by[1]]
                    if (length(split.by)>1)
                    {
                        for (i in 2:length(split.by))
                            one.df$split.by = paste0(one.df$split.by, ", ", one.df[,split.by[i]])
                    }
                }
                else
                    one.df$split.by = 'All'

                one.df$data.type=data.type.names[data.type]
                one.df$type = type

                if (facet.data.type.first)
                    one.df$facet.by = one.df$data.type
                else
                    one.df$facet.by = ''
                if (!is.null(facet.by) && length(facet.by)>0)
                {
                    for (i in 1:length(facet.by))
                    {
                        if (facet.data.type.first)
                            one.df$facet.by = paste0(one.df$facet.by, "\n", one.df[,facet.by[i]])
                        else
                            one.df$facet.by = paste0(one.df$facet.by, one.df[,facet.by[i]], "\n")
                    }
                }
                if (!facet.data.type.first)
                    one.df$facet.by = one.df$facet.by = paste0(one.df$facet.by, one.df$data.type)

                if (is.null(x.variables) || length(x.variables)==0)
                    one.df$x = 'All'
                else
                {
                    one.df$x = one.df[,x.variables[1]]
                    if (length(x.variables)>1)
                    {
                        for (i in 2:length(x.variables))
                            one.df$x = paste0(one.df$x, '-', one.df[,x.variables[i]])
                    }

                    if (all(x.variables=='year'))
                        one.df$x = as.numeric(as.character(one.df$x))
                }

                #-- Normalize within facet --#
                if (normalize.within.facet)
                {
                    one.df$value = sapply(1:dim(one.df)[1], function(i){
                        100*one.df$value[i] / sum(one.df$value[one.df$facet.by==one.df$facet.by[i] &
                                                               one.df$split.by==one.df$split.by[i] &
                                                               one.df$group==one.df$group[i]])
                    })
                }

                #-- Add it --#
                df = rbind(df, one.df)
            }
        }
    }


    #-- Make the Plot --#

    if (use.bar)
        plot.individual.simset.sims=F

    #Scales
    colors = rep(sim1.color, length(types))
    names(colors) = types
    if (length(types)==3)
        colors[types[2]] = sim2.color
    colors[cdc.label] = cdc.color

    type.guide.name = element_blank()
    if (shape.by.split)
    {
        n.split.by = length(unique(df$split.by))
        shapes = rep(split.shapes, ceiling(n.split.by/length(split.shapes)))
        shapes = shapes[1:n.split.by]
        names(shapes) = unique(df$split.by)
        shape.guide.name = paste0(toupper.first(split.by), collapse='/')
    }
    else
    {
        shapes = rep(sim1.shape, length(types))
        names(shapes) = types
        if (length(types)==3)
            shapes[types[2]] = sim2.shape
        shapes[cdc.label] = sim2.shape
        shape.guide.name = type.guide.name
    }

    line.sizes = rep(sim1.line.size, length(types))
    names(line.sizes) = types
    if (length(types)==3)
        line.sizes[types[2]] = sim2.line.size
    line.sizes[cdc.label] = cdc.line.size

    alphas = rep(sim1.alpha, length(types))
    names(alphas) = types
    if (length(types)==3)
        alphas[types[2]] = sim2.alpha
    alphas[cdc.label] = cdc.alpha


 #   if (facet.data.type.first)
  #      all.facet.by = c('data.type', facet.by)
   # else
    #    all.facet.by = c(facet.by, 'data.type')
    split.name = paste0(toupper(substr(split.by,1,1)), substr(split.by,2,nchar(split.by)))

    if (any(names(df)=='year'))
        df$year = as.numeric(as.character(df$year))
    if (!is.null(x.variables) && all(x.variables=='year'))
        df$x = as.numeric(df$x)

    df = df[!is.na(df$value),]

    #plot cdc on top
    df$group = paste0(df$group, '__', df$split.by)
    if (plot.individual.simset.sims)
    {
        df$type = factor(df$type, levels=rev(types))
        groups = unique(df$group)
        cdc.groups = groups[grepl(cdc.label, groups)]
        groups = c(setdiff(groups, cdc.groups), cdc.groups)

        df$group = factor(df$group, levels=groups)
    }


    if (use.bar)
    {
        rv = ggplot(df, aes(x, value, ymin=ci.lower, ymax=ci.upper, fill=type))
        rv = rv  + geom_bar(stat='identity', position = 'dodge') + geom_errorbar(position='dodge')
    }
    else
    {
        if (shape.by.split)
            rv = ggplot(df, aes(x, value, ymin=ci.lower, ymax=ci.upper, group=group, color=type, fill=type, shape=split.by))
        else
            rv = ggplot(df, aes(x, value, ymin=ci.lower, ymax=ci.upper, group=group, color=type, fill=type, shape=type))

        rv = rv + geom_ribbon(alpha=ribbon.alpha)

        if (linetype.by.sim)
            rv = rv + geom_line(aes(size=type, alpha=type, linetype=type))
        else
            rv = rv + geom_line(aes(size=type, alpha=type))

        if (show.points)
            rv = rv + geom_point(size=point.size, color='black')
    }

    rv = rv +
        ylim(0,NA) +
        theme(axis.text.x = element_text(angle = 45, hjust=1)) +
        xlab(paste0(toupper.first(x.variables), collapse='-')) +
        theme(panel.border=element_rect(colour="black",size=1,fill=NA),
              panel.background=element_blank(),
              strip.background = element_rect(color='black', size=1, fill='grey90'))

    if (!is.null(x.variables) && all(x.variables=='year'))
        rv = rv + scale_x_continuous(labels = int.breaks)

#    if (length(all.facet.by)>1 || length(data.types)>1)
    if (!is.null(df$facet.by) || length(data.types)>1)
        rv = rv + facet_wrap(~facet.by, scales=ifelse(fixed.facet.scales, 'fixed', 'free_y'), ncol=ncol) +
                theme(axis.title.y=element_blank())
    else
        rv = rv + ylab(data.type.names[data.types])


    #Add scales
    rv = rv + scale_color_manual(name=type.guide.name, values = colors) +
        scale_fill_manual(name=type.guide.name, values = colors) +
        scale_size_manual(guide=F, values=line.sizes) +
        scale_alpha_manual(guide=F, values=alphas)

    if (shape.by.split && length(unique(df$split.by))==1)
        rv = rv + scale_shape_manual(name=shape.guide.name, values = shapes, guide=F)
    else
        rv = rv + scale_shape_manual(name=shape.guide.name, values = shapes)

    rv
}



##-------------##
##-- HELPERS --##
##-------------##




int.breaks <- function(x)
{
    mask = round(x)==x
    rv = rep('', length(x))
    rv[mask] = as.character(round(x)[mask])

    rv
}

toupper.first <- function(str)
{
    paste0(toupper(substr(str,1,1)), substr(str,2,nchar(str)))
}


##-------------##
##-- HELPERS --##
##-------------##

get.sim.values <- function(sim,
                           data.type,
                           years,
                           all.dimensions,
                           use.cdc,
                           facet.by,
                           denominators,
                           denominator.dimensions,
                           show.rates)
{
    all.plus.denominator.dimensions = order.jheem.dimensions(union(denominator.dimensions, all.dimensions))

    years = intersect(years, sim$years)
    if (data.type=='diagnosed')
        model.rates = extract.diagnosed.hiv(sim, years=years, keep.dimensions=all.dimensions,
                                            per.population = 1, use.cdc.categorizations = use.cdc)
    else if (data.type=='population')
        model.rates = extract.population.subset(sim, years=years, keep.dimensions = all.dimensions,
                                                denominator.dimensions = facet.by, per.population = 1,
                                                use.cdc.categorizations = use.cdc)
    else
        model.rates = get.sim.projections(jheem.results=sim,
                                          data.type=data.type,
                                          years=years,
                                          keep.dimensions=all.plus.denominator.dimensions,
                                          facet.by=facet.by,
                                          use.cdc=use.cdc,
                                          denominator.dimensions = denominator.dimensions)

    if (show.rates || data.type=='population' || data.type=='diagnosed')
        values = model.rates
 #   else if (any(all.dimensions=='sex') && use.cdc)
 #   {
 #       values = model.rates

 #       if (any(dimnames(denominators)))
 #       access(values, sex='male') = access(model.rates, sex='male') * access(denominators, sex='male')
 #       access(values, sex='female') = access(model.rates, sex='female') * access(denominators, sex='female')
 #   }
    else
    {
        values = model.rates *
            expand.population(access(denominators, year=dimnames(model.rates)[['year']], collapse.length.one.dimensions = F),
                              dimnames(model.rates))
        values = apply(values, all.dimensions, sum)
    }

    values
}

get.simset.projections <- function(simset, data.type, years, keep.dimensions, facet.by,
                                   use.cdc,
                                   denominator.dimensions,
                                   true.denominators = 1,
                                   aggregate.denominator.males=T,
                                   use.sim.msm.proportions,
                                   ci.coverage)
{
    skeleton = get.sim.projections(simset@simulations[[1]],
                                   data.type=data.type, years=years, keep.dimensions=keep.dimensions,
                                   facet.by=facet.by, use.cdc=use.cdc, denominator.dimensions=denominator.dimensions,
                                   true.denominators=true.denominators,
                                   aggregate.denominator.males=aggregate.denominator.males,
                                   use.sim.msm.proportions=use.sim.msm.proportions)


    dist = extract.simset.distribution(simset, fn=get.sim.projections,
                            data.type=data.type, years=years, keep.dimensions=keep.dimensions,
                            facet.by=facet.by, use.cdc=use.cdc, denominator.dimensions=denominator.dimensions,
                            true.denominators=true.denominators,
                            aggregate.denominator.males=aggregate.denominator.males,
                            use.sim.msm.proportions=use.sim.msm.proportions)

    rv = list(mean=skeleton,
              ci.lower=skeleton,
              ci.upper=skeleton)

    means = get.means(dist)
    ci = get.equal.tailed.intervals(dist, coverage=ci.coverage)

    rv$mean[] = means[]
    rv$ci.lower[] = ci[1,]
    rv$ci.upper[] = ci[2,]

    rv


}

get.sim.projections <- function(jheem.results, data.type, years, keep.dimensions, facet.by,
                                use.cdc,
                                denominator.dimensions,
                                true.denominators = 1,
                                aggregate.denominator.males=T,
                                use.sim.msm.proportions)
{
    if (length(setdiff(years, jheem.results$years))>0)
    {
        years = intersect(years, jheem.results$years)
        if (!is.null(dim(true.denominators)) && any(names(dim(true.denominators))=='year'))
            true.denominators = access(true.denominators, year=as.character(years))
    }

    if (data.type=='diagnosed')
        extract.diagnosed.hiv(jheem.results, years=years, keep.dimensions=keep.dimensions,
                              per.population = 1, use.cdc.categorizations = use.cdc)
    else if (data.type=='population')
        extract.population.subset(jheem.results, years=years, keep.dimensions = keep.dimensions,
                                  denominator.dimensions = facet.by, per.population = 1,
                                  use.cdc.categorizations = use.cdc)
    else
    {
        if (data.type=='new')
            numerators = extract.new.diagnoses(jheem.results, years=years, keep.dimensions = keep.dimensions,
                                               per.population = NA, use.cdc.categorizations = use.cdc)
        else if (data.type=='incidence')
            numerators = extract.incidence(jheem.results, years=years, keep.dimensions = keep.dimensions,
                                               per.population = NA, use.cdc.categorizations = use.cdc)
        else if (data.type=='prevalence')
            numerators = extract.prevalence(jheem.results, continuum='diagnosed', years=years, keep.dimensions = keep.dimensions,
                                            per.population = NA, use.cdc.categorizations = use.cdc)
        else if (data.type=='mortality')
            numerators = extract.overall.hiv.mortality(jheem.results, continuum='diagnosed', years=years, keep.dimensions = keep.dimensions,
                                                       per.population = NA, use.cdc.categorizations = use.cdc)
        else
            stop("data.type must be either 'new', 'prevalence', 'mortality', 'diagnosed', 'incidence', or 'population'")


        keep.plus.denominator.dimensions = order.jheem.dimensions(union(denominator.dimensions, keep.dimensions))
        denominators = get.denominator.population(population=extract.population.subset(jheem.results, years=years, keep.dimensions = keep.plus.denominator.dimensions,
                                                                                       per.population = NA, use.cdc.categorizations = use.cdc),
                                                  years=years,
                                                  denominator.dimensions=denominator.dimensions,
                                                  aggregate.denominator.males = aggregate.denominator.males,
                                                  msm.proportions = if (use.sim.msm.proportions) attr(jheem.results, 'msm.proportions.by.race') else NULL,
                                                  non.year.target.dim.names = dimnames(numerators))

        if (is.null(dim(numerators)))
        {
            dim.names = list(names(numerators))
            names(dim.names) = keep.dimensions
            numerators = array(numerators, dim=sapply(dim.names, length), dimnames=dim.names)
        }

        if (is.null(dim(denominators)))
        {
            dim.names = list(names(denominators))
            names(dim.names) = keep.dimensions
            denominators = array(denominators, dim=sapply(dim.names, length), dimnames=dim.names)
        }

        denominators = apply(denominators, names(dimnames(numerators)), sum)

        numerators / denominators * as.numeric(true.denominators) #expand.population(true.denominators, target.dim.names = dimnames(numerators))
    }


}

get.denominator.population <- function(population,
                                       years,
                                       denominator.dimensions,
                                       aggregate.denominator.males=T,
                                       msm.proportions=NULL,
                                       non.year.target.dim.names=dimnames(population))
{
    # Check years
    if (is.null(dimnames(population)))
        available.years = as.numeric(names(population))
    else
        available.years = as.numeric(dimnames(population)[['year']])
    missing.years = setdiff(years, available.years)
    present.years = intersect(years, available.years)

    # Check if population is vector
    if (is.null(dim(population)))
    {
        pop.names = names(population)
        dim(population) = c(year=length(population))
        dimnames(population) = list(year=pop.names)

        non.year.target.dim.names = list()
    }


    # Set up dim names and rv
    non.year.target.dim.names = non.year.target.dim.names[intersect(names(non.year.target.dim.names),
                                                                    c('age','race','subpopulation','sex','risk'))]

    target.dim.names = c(list(year=as.character(years)), non.year.target.dim.names)
    rv = array(0, dim=sapply(target.dim.names, length), dimnames=target.dim.names)

    present.years.dim.names = c(list(year=as.character(present.years)), non.year.target.dim.names)

     # Pare down the given population, scale it up, and plug it in

#    population = apply(access(population, year=as.character(present.years), collapse.length.one.dimensions = F), denominator.dimensions, sum)
    population.dim.names = dimnames(population)[intersect(names(dimnames(population)), denominator.dimensions)]

    population = apply(population, denominator.dimensions, sum)
    dim(population) = sapply(population.dim.names, length)
    dimnames(population) = population.dim.names

    access(rv, year=as.character(present.years)) = expand.population(access(population, year=as.character(present.years), collapse.length.one.dimensions = F),
                                                                     target.dim.names = present.years.dim.names)


    # Fill in missing years with data from nearest year
    for (year in missing.years)
    {
        closest.year = available.years[order(abs(available.years-year))][1]

        closest.pop = access(population, year=as.character(closest.year), collapse.length.one.dimensions = F)

        if (length(dim(closest.pop))==1)
            access(rv, year=as.character(year)) = as.numeric(closest.pop)
        else
            access(rv, year=as.character(year)) = expand.population(closest.pop,
                                                                    target.dim.names=non.year.target.dim.names)
    }


    # Aggregate males if appropriate
    if ((aggregate.denominator.males || !is.null(msm.proportions)) && any(denominator.dimensions=='sex'))
    {
        stop("This is not going to work")
        males = access(rv, sex='msm') + access(rv, sex='heterosexual_male')
        access(rv, sex='msm') = males
        access(rv, sex='heterosexual_male') = males

        if (!is.null(msm.proportions) && !aggregate.denominator.males)
        {
            for (race in names(msm.proportions))
            {
                access(rv, race=race, sex='msm') = access(rv, race=race, sex='msm') * msm.proportions[race]
                access(rv, race=race, sex='heterosexual_male') = access(rv, race=race, sex='heterosexual_male') * msm.proportions[race]
            }
        }
    }

    # Return
    rv
}

to.lower.list <- function(l)
{
    lapply(l, function(elem){
        tolower(elem)
    })
}

order.jheem.dimensions <- function(dims,
                                   allowed=c('year', 'age','race','subpopulation','sex','risk',
                                             'non.hiv.subset','continuum','cd4','hiv.subset'))
{
    allowed[sapply(allowed, function(one.dim){any(one.dim==dims)})]
}
