

# returns a matrix of simulated parameter values
# simualtes such that, values will be generated in the same way as long as their underlying distribution has not changed
# resets the seed before exiting so as not to mess up subsequent randomness
# ie, subsequent random numbers don't depend on what is given in the distributions
simulate.values.from.distribution.fixed.seed <- function(distributions,
                                                         n,
                                                         seed = NULL)
{
    distributions = separate.joint.independent.distributions(distributions)
  
    to.reset.seed = round(runif(1, min=0, max=.Machine$integer.max))
    if (is.null(seed))
        seed = round(runif(1, min=0, max=.Machine$integer.max))

    values = NULL
    value.names = character()
    bounds = NULL
    for (i in 1:length(distributions))
    {
        dist = distributions[[i]]
        if (!is.null(seed))
        {
            dist.names = paste0(dist@var.names, collapse=',')
            dist.seed = as.integer( (sum(as.integer(charToRaw(dist.names))) + seed) %% .Machine$integer.max )
            set.seed(dist.seed)
        }
        
        values = cbind(values, generate.random.samples(dist, n))
        value.names = c(value.names, dist@var.names)

        bounds = cbind(bounds, get.support.bounds(dist@support))
    }
    
    #reset the seed
    if (!is.null(seed))
        set.seed(to.reset.seed)
    
    dimnames(values)[[2]] = value.names
    
    rv = list(values=values,
         names=value.names,
         lower.bounds=bounds[1,],
         upper.bounds=bounds[2,])
    
    names(rv$lower.bounds) = value.names
    names(rv$upper.bounds) = value.names
    
    rv
}

# Returns a list of distributions such that each distribution is NOT a joint independent distribution
separate.joint.independent.distributions <- function(distributions)
{
    if (is(distributions, "Distribution"))
        distributions = list(distributions)
    
    rv = list()
    for (dist in distributions)
    {
        rv = c(rv, separate.independent.distributions(dist))
    }
    
    rv
}