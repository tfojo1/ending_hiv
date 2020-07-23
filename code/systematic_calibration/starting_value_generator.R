#library(distributions)
#library(mcmc.sim)
source('code/source_code.R')
source('code/systematic_calibration/systematic_settings.R')

if (1==2)
{
    load('mcmc_runs/dallas.73_revised.lik_20000_2020-07-22.Rdata')
    mcmc = assemble.mcmc.from.cache('mcmc_runs/la.73_revised.lik_2020-07-22/',T)
    simset = extract.simset(mcmc, additional.burn=mcmc@n.iter * .25)
    
    sampling.dist = create.starting.sampling.distribution(simset, correlated.sd.inflation = .75, uncorrelated.sd.inflation = .5)   
    save(sampling.dist, file=file.path(SYSTEMATIC.ROOT.DIR, 'starting_value_generators/DEFAULT.Rdata'))
    
    #to add parameters
#    simset = add.parameters(simset, simset@parameters[,'global.trate'], parameter.names='global.trate.msm',
 #                           parameter.lower.bounds = 0, parameter.upper.bounds = Inf, parameter.transformations = 'log')
  #  simset = add.parameters(simset, simset@parameters[,'global.trate'], parameter.names='global.trate.heterosexual',
   #                         parameter.lower.bounds = 0, parameter.upper.bounds = Inf, parameter.transformations = 'log')
    #simset = add.parameters(simset, simset@parameters[,'global.trate'], parameter.names='global.trate.idu',
     #                       parameter.lower.bounds = 0, parameter.upper.bounds = Inf, parameter.transformations = 'log')
}

create.starting.sampling.distribution <- function(simset,
                                                  prior=parameters.prior,
                                                  parameter.names = prior@var.names,
                                                  msa=attr(simset@simulations[[1]], 'location'),
                                                  correlated.sd.inflation = 0.75,
                                                  uncorrelated.sd.inflation = 0.75)
{
    parameters = sapply(1:simset@n.parameters, function(i){
        simset@parameter.transformations[[i]]@transform(simset@parameters[,i])
    })
    dimnames(parameters)[[2]] = simset@parameter.names
    
    # Get empiric means and correlation
    means = colMeans(parameters)
    cov.mat = cov(parameters)
    cov.mat = cov.mat * correlated.sd.inflation +  diag(diag(cov.mat)) * uncorrelated.sd.inflation 
    #    diag(diag(cov.mat) + min(diag(cov.mat))/2) * uncorrelated.sd.inflation 
    #the +min(diag...) is just to avoid rounding error non-positive definiteness
    
    bounds = get.support.bounds(prior@support)
    dimnames(bounds)[[2]] = prior@var.names
    
    sampling.dist = Transformed.Multivariate.Normal.Distribution(mu=means[parameter.names],
                                                                 sigma=cov.mat[parameter.names, parameter.names],
                                                                 var.names = parameter.names,
                                                                 transformations = simset@parameter.transformations[parameter.names],
                                                                 lower=bounds[1,parameter.names],
                                                                 upper=bounds[2,parameter.names])
    
    #do a test
    x = generate.random.samples(sampling.dist, 10)
    
#    if (!is.null(msa))
 #       save(sampling.dist, file=file.path('code/systematic_calibration/starting_value_generators/',
  #                                         paste0(msa, '.Rdata')))
    
    sampling.dist
}


#OLD
get.starting.value.distribution <- function(simset.base,
                                            prior=parameters.prior,
                                            specified.generators=NULL,
                                            var.names.to.pull.from.prior=TO.PULL.FROM.PRIOR,
                                            correlated.sd.inflation = 1,
                                            uncorrelated.sd.inflation = 1)
{
    var.names.to.pull.from.prior = intersect(prior@var.names, var.names.to.pull.from.prior)

    parameters = sapply(1:simset.base@n.parameters, function(i){
        simset.base@parameter.transformations[[i]]@transform(simset.base@parameters[,i])
    })
    dimnames(parameters)[[2]] = simset@parameter.names

    # Get empiric means and correlation
    means = colMeans(parameters)
    cov.mat = cov(parameters)
    cov.mat = cov.mat * correlated.sd.inflation + diag(diag(cov.mat)) * uncorrelated.sd.inflation

    bounds = get.support.bounds(prior@support)
    dimnames(bounds)[[2]] = prior@var.names

    sampling.dist = Transformed.Multivariate.Normal.Distribution(mu=means[prior@var.names],
                                                                 sigma=cov.mat[prior@var.names,prior@var.names],
                                                                 var.names = prior@var.names,
                                                                 transformations = simset.base@parameter.transformations[prior@var.names],
                                                                 lower=bounds[1,],
                                                                 upper=bounds[2,])
 #   if (length(var.names.to.pull.from.prior)==0)
        prior.sub = prior
  #  else
   #     prior.sub = subset.distribution(prior, vars.to.keep = var.names.to.pull.from.prior)

    function(n)
    {
        rv = generate.random.samples(sampling.dist, max(2,n))[,prior@var.names]

        if (length(var.names.to.pull.from.prior)>0)
            rv[,var.names.to.pull.from.prior] = generate.random.samples(prior.sub, max(2,n))[,var.names.to.pull.from.prior]

        if (!is.null(specified.generators))
            rv[,specified.generators@var.names] = generate.random.samples(specified.generators, max(2,n))

        if (n==1)
            rv[1,]
        else
            rv
    }
}
