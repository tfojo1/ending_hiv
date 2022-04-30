get.simset.incidence.reduction <- function(simset,
                                           year1=2020,
                                           year2=2030,
                                           ci.coverage=0.95,
                                           per.population=NA)
{
    
    dist = extract.simset.distribution(simset, get.incidence.reduction, year1=year1, year2=year2,
                                       per.population=per.population)
    
    means = get.means(dist)
    interval = get.intervals(dist, coverage = ci.coverage)[,1]
    
    c(mean.reduction = as.numeric(means[1]),
      ci.lower = as.numeric(interval[1]),
      ci.upper = as.numeric(interval[2]),
      success.probability = as.numeric(means[2]))
}

get.simset.incidence.dist <- function(simset,
                                 year=2030,
                                 per.population=NA,
                                 ages=NULL,
                                 races=NULL,
                                 sexes=NULL,
                                 risks=NULL,
                                 include.hiv.positive.in.denominator)
{
    extract.simset.distribution(simset, extract.incidence,
                                years=year,
                                ages=ages,
                                races=races,
                                sexes=sexes,
                                risks=risks,
                                per.population=per.population,
                                include.hiv.positive.in.denominator=include.hiv.positive.in.denominator)
}

get.incidence.reduction <- function(sim,
                                    year1=2020,
                                    year2=2030,
                                    goal=0.9,
                                    keep.dimensions=NULL,
                                    per.population=NA)
{
    if (all(sim$years != year1))
        stop("The year ", year1, " is not contained in the simulation")
    if (all(sim$years != year2))
        stop("The year ", year2, " is not contained in the simulation")
    inc = extract.incidence(sim, years=c(year1, year2), per.population = per.population, keep.dimensions = c('year', keep.dimensions))
    
    if (is.null(keep.dimensions))
    {
        reduction = (inc[1] - inc[2]) / inc[1]
        reduction = as.numeric(reduction)
    }
    else
        reduction = (inc[1,] - inc[2,]) / inc[1,]
    
    c(reduction=reduction, achieved.goal=reduction>=as.numeric(goal))
}

get.infections.averted <- function(reference.simset,
                                   intervention.simset,
                                   years=2020:2030,
                                   ages=NULL,
                                   races=NULL,
                                   sexes=NULL,
                                   risks=NULL,
                                   use.cdc.categorizations = F,
                                   return.dist=F
                                   )
{
    cum.inc.ref = sapply(reference.simset@simulations,
                         do.extract.incidence,
                         years=years, ages=ages, races=races,
                         sexes=sexes, risks=risks, use.cdc.categorizations=use.cdc.categorizations,
                         keep.dimensions=character())
    
    cum.inc.int = sapply(intervention.simset@simulations,
                         do.extract.incidence,
                         years=years, ages=ages, races=races,
                         sexes=sexes, risks=risks, use.cdc.categorizations=use.cdc.categorizations,
                         keep.dimensions=character())
    
    if (return.dist)
        Empiric.Distribution((cum.inc.ref-cum.inc.int)/cum.inc.ref)
    else
        mean((cum.inc.ref-cum.inc.int)/cum.inc.ref)
}

##-- AT THE SIMULAITON LEVEL --##

extract.total.incidence.reduction.20.30 <- function(sim)
{
    extract.total.incidence.reduction(sim, year1=2020, year2=2030)
}

extract.total.incidence.reduction <- function(sim,
                                        year1=2020,
                                        year2=2030)
{
    inc = extract.incidence(sim, years=c(year1, year2), per.population = NA, keep.dimensions = 'year')
    
    reduction = (inc[1] - inc[2]) / inc[1]
    reduction = as.numeric(reduction)
    
    reduction
}
