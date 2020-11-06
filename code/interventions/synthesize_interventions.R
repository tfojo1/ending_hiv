get.simset.incidence.reduction <- function(simset,
                                           year1=2020,
                                           year2=2030,
                                           ci.coverage=0.95)
{
    
    dist = extract.simset.distribution(simset, get.incidence.reduction, year1=year1, year2=year2)
    
    means = get.means(dist)
    interval = get.intervals(dist, coverage = ci.coverage)[,1]
    
    c(mean.reduction = as.numeric(means[1]),
      ci.lower = as.numeric(interval[1]),
      ci.upper = as.numeric(interval[2]),
      success.probability = as.numeric(means[2]))
}

get.incidence.reduction <- function(sim,
                                    year1=2020,
                                    year2=2030,
                                    goal=0.9,
                                    keep.dimensions=NULL)
{
    if (all(sim$years != year1))
        stop("The year ", year1, " is not contained in the simulation")
    if (all(sim$years != year2))
        stop("The year ", year2, " is not contained in the simulation")
    inc = extract.incidence(sim, years=c(year1, year2), per.population = NA, keep.dimensions = c('year', keep.dimensions))
    
    if (is.null(keep.dimensions))
    {
        reduction = (inc[1] - inc[2]) / inc[1]
        reduction = as.numeric(reduction)
    }
    else
        reduction = (inc[1,] - inc[2,]) / inc[1,]
    
    c(reduction=reduction, achieved.goal=reduction>=as.numeric(goal))
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
