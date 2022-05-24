CACHE.FREQUENCY = 500





##-----------------##
##-- RUN INITIAL --##
##-----------------##

setup.initial.mcmc.for.msa <- function(msa,
                                       version='collapsed_1.0',
                                       likelihood=NULL,
                                       prior=get.parameters.prior.for.version(version),
                                       parameter.var.blocks = get.parameter.sampling.blocks.for.version(version),
                                       template.mcmc=NULL,
                                       chains=1,
                                       n.iter=20000,
                                       thin=20,
                                       burn=0,
                                       max.sim.time=20,
                                       save.dir=file.path(SYSTEMATIC.ROOT.DIR,
                                                          paste0('systematic_initial', 
                                                                 get.directory.suffix.for.version(version))),
                                       cache.dir=file.path(SYSTEMATIC.ROOT.DIR, 
                                                           paste0('systematic_caches',
                                                                  get.directory.suffix.for.version(version))),
                                       update.frequency=200,
                                       cache.frequency=CACHE.FREQUENCY,
                                       save.suffix='',
                                       
                                       target.acceptance.rate=0.1,
                                       SCALING.BASE.UPDATE = 1,
                                       SCALING.UPDATE.PRIOR=10,
                                       SCALING.UPDATE.DECAY=.5,#0.25,
                                       COV.BASE.UPDATE=0.2,
                                       COV.UPDATE.PRIOR=500,
                                       COV.UPDATE.DECAY=0.5,#0.25
                                       
                                       run=T,
                                       verbose=T,
                                       step.size.multiplier=1,
                                       derive.step.size.from.prior.mcmc=F,
                                       plot.first.sim=run)
{
    
    # Likelihood
    if (is.null(likelihood))
    {
        if (verbose)
            print("Creating Likelihood")
        likelihood = create.msa.likelihood(msa)
    }
    

    starting.parameters.to.use = get.initial.starting.parameters(msa=msa, version=version)
        
    start.value.generator = function(n){
        if (n==1)
            starting.parameters.to.use
        else
            t(sapply(1:n, function(i){starting.parameters.to.use}))
    }   
    
    # Pull chain state variables
    
    if (derive.step.size.from.prior.mcmc) #this chunk bases step sizes and initial cov mat on previous mcmc run
    {
        if (is.null(template.mcmc))
        {
            load(file.path(SYSTEMATIC.ROOT.DIR, 'test_runs/la.113c_revised.lik.v12_20K_2020-09-20.Rdata'))
            template.mcmc = mcmc
        }
        
        simset = extract.simset(template.mcmc, additional.burn=mcmc@n.iter/2)
        chain.state = template.mcmc@chain.states[[1]]
        initial.scaling.parameters = lapply(chain.state@log.scaling.parameters, function(x){
            exp(x) * step.size.multiplier
        })
        initial.cov.mat = diag(diag(cov(log(simset@parameters))))
    }
    else #this code calculates step sizes and initial cov mat from scratch
    {
        param.medians = suppressWarnings(get.medians(prior))
        init.sds = suppressWarnings(get.sds(prior) / param.medians) / 40
        init.sds[prior@is.improper] = 0.1/40
        
        init.sds[init.sds>1] = (param.medians)[init.sds>1] / 40
        init.sds[grepl('peak.*mult', names(init.sds))] = init.sds[grepl('peak.*mult', names(init.sds))] / 16
        init.sds = init.sds * 2
        
        init.sds[init.sds==0] = 1/40 #for z-scores
        
        initial.cov.mat = diag(init.sds^2)
        
        initial.scaling.parameters = 2.38^2/sapply(parameter.var.blocks, length) #the default
    }
    
    setup.mcmc.for.msa(msa=msa,
                       version=version,
                       likelihood=likelihood,
                       prior=prior,
                       parameter.var.blocks=parameter.var.blocks,
                       start.value.generator=start.value.generator,
                       chains=chains,
                       n.iter=n.iter,
                       thin=thin,
                       burn=burn,
                       max.sim.time=max.sim.time,
                       save.dir=save.dir,
                       cache.dir=cache.dir,
                       update.frequency=update.frequency,
                       cache.frequency = cache.frequency,
                       save.suffix=save.suffix,
                       target.acceptance.rate = target.acceptance.rate,
                       run=run,
                       verbose=verbose,
                       
                       initial.cov.mat = initial.cov.mat,
                       initial.scaling.parameters = initial.scaling.parameters,
                       
                       SCALING.UPDATE.DECAY = SCALING.UPDATE.DECAY,
                       SCALING.UPDATE.PRIOR = SCALING.UPDATE.PRIOR,
                       SCALING.BASE.UPDATE = SCALING.BASE.UPDATE,
                       
                       COV.UPDATE.DECAY = COV.UPDATE.DECAY,
                       COV.UPDATE.PRIOR = COV.UPDATE.PRIOR,
                       COV.BASE.UPDATE = COV.BASE.UPDATE,
                       
                       plot.first.sim=plot.first.sim
    )
}

get.initial.starting.parameters <- function(msa,
                                            version,
                                            verbose=T,
                                            allow.beta.values=F)
{
    # Start values:
    # The order of preference for values is:
    # 1) Start values for this version
    # 2) beta start values for this version
    # 3) A prior run for an earlier version
    # 4) Start values for a prior version
    # 5) beta start values for a prior version
    
    starting.parameters = NULL
    for (version.to.pull in c(version, get.prior.versions(version)))
    {
        prior.simset.filename = get.full.filename(location=msa)
        prior.run.file = file.path(SIMULATIONS.DIR,
                                   paste0('baseline', get.directory.suffix.for.version(version.to.pull)),
                                   prior.simset.filename)

        if (version != version.to.pull &&
            file.exists(prior.run.file))
        {
            load(prior.run.file)
            if (verbose)
                print(paste0("* Getting starting parameters from prior run for version '", version.to.pull))
            starting.parameters = simset@parameters[simset@n.sim,]
        }
        else
        {
            start.value.file = file.path(START.VALUES.DIR,
                                         version.to.pull,
                                         paste0(msa, '.Rdata'))
            if (file.exists(start.value.file))
            {
                if (verbose)
                    print(paste0("* Getting starting parameters from start_values file for version '", version.to.pull))
                load(start.value.file)
            }
            else if (allow.beta.values)
            {
                beta.start.value.file = file.path(START.VALUES.DIR,
                                                  paste0(version.to.pull, '_beta'),
                                                  paste0(msa, '.Rdata'))
                if (file.exists(beta.start.value.file))
                {
                    if (verbose)
                        print(paste0("* Getting starting parameters from BETA start_values file for version '", version.to.pull))
                    load(beta.start.value.file)
                }
            }
        }
        
        if (!is.null(starting.parameters))
            break
    }
    
    if (is.null(starting.parameters))
        stop(paste0("Initial values have not been set for ", msa.names(msa), " for version '",
                    version, "' and no prior versions have runs or initial values"))
    
    
    # If the starting parameters don't match what we need for the version
    #  start at the median values
    prior = get.parameters.prior.for.version(version=version)
    starting.parameters.to.use = suppressWarnings(get.medians(prior))
    matching.names = intersect(names(starting.parameters), names(starting.parameters.to.use))
    starting.parameters.to.use[matching.names] = starting.parameters[matching.names]
    
    if (any(is.na(starting.parameters.to.use)))
        stop("NA produced in starting.parameters.to.use")
    
    starting.parameters.to.use
}

create.start.value.generator.for.msa <- function(msa, 
                                                 version,
                                                 initial.dir='systematic_initial')
{
    files = list.files(file.path(SYSTEMATIC.ROOT.DIR, initial.dir))
    full.files = list.files(file.path(SYSTEMATIC.ROOT.DIR, initial.dir), full.names = T)
    
    mask = grepl(msa, files)
    if (!any(mask))
        stop(paste0("No initial mcmc runs have been done for '", msa.names(msa), "' MSA (", msa, ')'))
    load(full.files[mask][sum(mask)])
    
    simset = extract.simset(mcmc, additional.burn=mcmc@n.iter/2)
    prior = get.parameters.prior.for.version(version=version)
    
    sampling.dist = create.starting.sampling.distribution(simset, prior = prior)
    save(sampling.dist, file=file.path(SYSTEMATIC.ROOT.DIR, 'starting_value_generators',
                                       paste0(msa, '.Rdata')))
}

##-----------------------##
##-- RUN PARALLEL MCMC --##
##-----------------------##

setup.parallel.mcmc.for.msa <- function(msa,
                                        version='collapsed_1.0',
                                        likelihood=NULL,
                                        prior=parameters.prior,
                                        parameter.var.blocks = get.parameter.sampling.blocks.for.version(version),
                                        start.value.generator=NULL,
                                        chains=4,
                                        n.iter=100000,
                                        thin=100,
                                        burn=0,
                                        max.sim.time=20,
                                        save.dir=file.path(SYSTEMATIC.ROOT.DIR,
                                                           paste0('systematic_parallel', 
                                                                  get.directory.suffix.for.version(version))),
                                        cache.dir=file.path(SYSTEMATIC.ROOT.DIR, 
                                                            paste0('systematic_caches',
                                                                   get.directory.suffix.for.version(version))),
                                        initial.dir=file.path(SYSTEMATIC.ROOT.DIR,
                                                              paste0('systematic_initial', 
                                                                     get.directory.suffix.for.version(version))),
                                        update.frequency=200,
                                        cache.frequency=CACHE.FREQUENCY,
                                        save.suffix='',
                                        
                                        target.acceptance.rate=0.238,
                                        COV.UPDATE.PRIOR=500,
                                        SCALING.UPDATE.PRIOR=100,
                                        SCALING.UPDATE.DECAY=.5,
                                        
                                        run=F,
                                        verbose=T)
{
    # Likelihood
    if (is.null(likelihood))
    {
        if (verbose)
            print("Creating Likelihood")
        likelihood = create.msa.likelihood(msa)
    }
    
    # Pull Initial MCMC
    if (verbose)
        print("Loading the initial MCMC")
    files = list.files(initial.dir)
    full.files = list.files(initial.dir, full.names = T)
    
    mask = grepl(msa, files)
    
    if (!any(mask))
        stop(paste0("No initial mcmc runs have been done for '", msa.names(msa), "' MSA (", msa, ')'))
    load(full.files[mask][sum(mask)])
    
    # Set up start value generator
    if (verbose)
        print("Creating starting value generator")
    simset = extract.simset(mcmc, additional.burn=mcmc@n.iter/2)
    prior = get.parameters.prior.for.version(version=version)
    start.value.generator = create.starting.sampling.distribution(simset, 
                                                                  prior = prior,
                                                                  correlated.sd.inflation = .75, 
                                                                  uncorrelated.sd.inflation = .5)
    
    # Pull chain state variables
    chain.state = mcmc@chain.states[[1]]
    initial.scaling.parameters = lapply(chain.state@log.scaling.parameters, function(x){
        exp(x)/2
    })
    initial.cov.mat = 0.5 * chain.state@cov.mat + 0.5 * diag(diag(chain.state@cov.mat))
    
    # Pass to sub function
    setup.mcmc.for.msa(msa=msa,
                       version=version,
                       likelihood=likelihood,
                       prior=prior,
                       parameter.var.blocks=parameter.var.blocks,
                       start.value.generator=start.value.generator,
                       chains=chains,
                       n.iter=n.iter,
                       thin=thin,
                       burn=burn,
                       max.sim.time=max.sim.time,
                       save.dir=save.dir,
                       cache.dir=cache.dir,
                       update.frequency=update.frequency,
                       cache.frequency = cache.frequency,
                       save.suffix=save.suffix,
                       target.acceptance.rate = target.acceptance.rate,
                       
                       initial.cov.mat = initial.cov.mat,
                       initial.scaling.parameters = initial.scaling.parameters,
                       
                       COV.UPDATE.PRIOR=COV.UPDATE.PRIOR,
                       SCALING.UPDATE.PRIOR=SCALING.UPDATE.PRIOR,
                       SCALING.UPDATE.DECAY=SCALING.UPDATE.DECAY,
                       
                       run=run,
                       verbose=verbose
    )
    
}

setup.mcmc.for.msa <- function(msa,
                               version,
                               likelihood=NULL,
                               prior=parameters.prior,
                               parameter.var.blocks = get.parameter.sampling.blocks.for.version(version),
                               start.value.generator=NULL,
                               chains=4,
                               n.iter=50000,
                               thin=100,
                               burn=0,
                               max.sim.time=Inf,
                               save.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_parallel'),
                               cache.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_caches'),
                               update.frequency=200,
                               cache.frequency=CACHE.FREQUENCY,
                               save.suffix='',
                               
                               target.acceptance.rate=0.238,
                               N.ITER.BEFORE.COV = 0,
                               ADAPTIVE.SCALING='componentwise',
                               SCALING.BASE.UPDATE = 1,
                               SCALING.UPDATE.PRIOR=100,
                               SCALING.UPDATE.DECAY=.5,
                               COV.BASE.UPDATE=1,
                               COV.UPDATE.PRIOR=500,
                               COV.UPDATE.DECAY=1,
                               initial.cov.mat=NULL,
                               initial.scaling.parameters=NULL,
                               run=F,
                               plot.first.sim=run,
                               verbose=T)
{
    
    # Likelihood
    if (is.null(likelihood))
    {
        if (verbose)
            print("Creating Likelihood")
        likelihood = create.msa.likelihood(msa)
    }
    
    #-- Start Values --#
    
    if (is.null(start.value.generator))
    {
        load(file.path(SYSTEMATIC.ROOT.DIR, 'starting_value_generators',
                       paste0(msa, '.Rdata')))
        start.value.generator = sampling.dist
    }
    
    if (is(start.value.generator, 'Distribution'))
        start.values = generate.random.samples(start.value.generator, chains)
    else
        start.values = start.value.generator(chains)
    
    if (chains==1)
        first.start.values=start.values
    else
        first.start.values=start.values[1,]
    
    if (verbose)
        print("Creating run.simulation function")
    
    run.simulation = create.run.simulation.function(msa=msa,
                                                    version=version,
                                                    start.values=first.start.values,
                                                    max.sim.time=max.sim.time)
    
    
    if (plot.first.sim)
    {
        if (verbose)
            print("Running initial simulation to plot")
        init.sim = run.simulation(first.start.values)
#        print(plot.calibration.risk(init.sim) + ggtitle(paste0("Initial Sim: ", msa.names(msa))))
        print(simplot(init.sim, data.types=c('new','prevalence'), facet.by = 'risk') + ggtitle(paste0("Initial Sim: ", msa.names(msa))) +
            theme(plot.title=element_text(hjust=1)))
    }
    
    #-- MCMC Control --#
    if (verbose)
        print("Creating MCMC Control")
    
    if (is.null(initial.cov.mat))
    {
        param.medians = suppressWarnings(get.medians(prior))
        init.sds = suppressWarnings(get.sds(prior) / param.medians) / 40
        init.sds[prior@is.improper] = 0.1/40
        
        init.sds[init.sds>1] = (param.medians)[init.sds>1] / 40
        init.sds = init.sds * 2
        initial.cov.mat = diag(init.sds^2)
    }
    if (is.null(initial.scaling.parameters))
    {
        initial.scaling.parameters = 2.38^2/sapply(parameter.var.blocks, length)
    }
    else if (length(initial.scaling.parameters) != length(parameter.var.blocks)) #this code just maps the 
    {
        initial.scaling.parameters = lapply(parameter.var.blocks, function(block){
            superset.mask = sapply(initial.scaling.parameters, function(params){
                length(setdiff(names(params), block))==0
            })
            if (any(superset.mask))
            {
                index = (1:length(initial.scaling.parameters))[superset.mask][1]              
                initial.scaling.parameters[[index]][block]
            }
            else
            {
                rv = sapply(block, function(var.in.block){
                    mask = sapply(initial.scaling.parameters, function(params){
                        any(names(params)==var.in.block)
                    })
                    index = (1:length(initial.scaling.parameters))[mask][1]
                    initial.scaling.parameters[[index]][var.in.block]
                })
                names(rv) = block
                rv
            }
        })
        names(initial.scaling.parameters) = names(parameter.var.blocks)
    }
    transformations = sapply(prior@var.names, function(v){'log'})
    logit.transform.mask = grepl('max.proportion', names(transformations)) &
        !grepl('\\.or', names(transformations))
    transformations[logit.transform.mask] = 'logit'
    identity.transform.mask = grepl('.z$', prior@var.names)
    transformations[identity.transform.mask] = 'identity'
    
    ctrl = create.adaptive.blockwise.metropolis.control(var.names=prior@var.names,
                                                        simulation.function=run.simulation,
                                                        log.prior.distribution = get.density.function(prior),
                                                        log.likelihood = likelihood,
                                                        burn=burn, thin=thin,
                                                        var.blocks = parameter.var.blocks,
                                                        reset.adaptive.scaling.update.after = 0,
                                                        transformations = transformations,
                                                        
                                                        initial.covariance.mat = initial.cov.mat,
                                                        initial.scaling.parameters = initial.scaling.parameters,
                                                        
                                                        target.acceptance.probability=target.acceptance.rate,
                                                        
                                                        n.iter.before.use.adaptive.covariance = N.ITER.BEFORE.COV,
                                                        adaptive.covariance.base.update = COV.BASE.UPDATE,
                                                        adaptive.covariance.update.prior.iter = COV.UPDATE.PRIOR,
                                                        adaptive.covariance.update.decay = COV.UPDATE.DECAY,
                                                        adaptive.scaling = ADAPTIVE.SCALING,
                                                        adaptive.scaling.base.update = SCALING.BASE.UPDATE,
                                                        adaptive.scaling.update.prior.iter= SCALING.UPDATE.PRIOR,
                                                        adaptive.scaling.update.decay= SCALING.UPDATE.DECAY
    )
    
    #-- Set up the MCMC cache --#
    if (save.suffix != '')
        save.suffix = paste0('_', save.suffix)
    cache.dir = file.path(cache.dir, paste0(msa, '_', chains, 'x', n.iter/1000, 'K',
                                            save.suffix, '_', Sys.Date()))
    
    remove.mcmc.cache(cache.dir)
    
    if (verbose)
        print("Creating the cache")
    
    create.mcmc.cache(dir=cache.dir,
                      control=ctrl,
                      n.iter=n.iter,
                      starting.values = start.values,
                      cache.frequency = cache.frequency)
    
    
    metadata = list(n.chains=chains,
                    save.dir=save.dir,
                    location=msa)
    
    save(metadata, file=file.path(cache.dir, 'metadata.Rdata'))
    
    if (run)
    {
        if (verbose)
            print("Running MCMC")
        run.mcmc.for.msa.cache(cache.dir,
                               update.detail='high',
                               update.frequency=update.frequency)
    }
    else
    {
        if (verbose)
            print("Done")
        NULL
    }
}


create.run.simulation.function <- function(msa,
                                           start.values,
                                           version = 'collapsed_1.0',
                                           max.sim.time=Inf,
                                           catch.errors=T,
                                           fix.components=T)
{
    #-- Init Components --#
    settings = get.settings.for.version(version)  
  
    base.components = setup.initial.components(msa=msa, settings = settings)
    
    get.components.fn = get.components.function.for.version(version)
    
    init.components = get.components.fn(start.values, base.components)
    
    if (fix.components)
        init.components = fix.components.for.calibration(components = init.components)
    
    #-- Run Function --#
    
    if (catch.errors)
      run.simulation <- function(parameters, 
                                 start.year=1970, end.year=2020, keep.years=start.year:end.year)
      {
          tryCatch({
              components = get.components.fn(parameters, init.components)
              sim = run.jheem.from.components(components,
                                              start.year=start.year, end.year=end.year,
                                              keep.years=keep.years,
                                              max.run.time.seconds = max.sim.time)
              
              if (sum(sapply(sim, function(x){sum(is.na(x))}))>0)
                  stop("NA values in simulation")
              
              sim
          },
          error = function(e){
              cat("----------------------------------\n")
              cat("THERE WAS AN ERROR RUNNING THE SIMULATION:\n")
              cat(e$message, '\n')
              
              error.file = file.path(SYSTEMATIC.ROOT.DIR, 'errors', paste0('error_', msa, '_', Sys.Date()))
              
              save(e,
                   parameters,
                   components,
                   init.components,
                   file=error.file)
              
              cat("Saving the details to '", error.file, "'\n and allowing the mcmc to continue.\n", sep='')
              cat("----------------------------------\n")
              
              list(terminated=T)
          })
      }
    else
        run.simulation <- function(parameters, 
                                   start.year=1970, end.year=2020, keep.years=start.year:end.year)
        {
            components = get.components.fn(parameters, init.components)
            sim = run.jheem.from.components(components, max.run.time.seconds = max.sim.time,
                                            start.year=start.year, end.year=end.year,
                                            keep.years=keep.years)
            
            if (sum(sapply(sim, function(x){sum(is.na(x))}))>0)
              stop("NA values in simulation")
            
            sim
        }
        
    
    run.simulation
}

run.mcmc.for.msa.cache <- function(cache.dir,
                                   chains=NULL,
                                   update.detail='high',
                                   update.frequency=200,
                                   remove.cache.when.done=F)
{
    cache.dir = file.path(cache.dir)
    load(file.path(cache.dir, 'metadata.Rdata'))
    if (is.null(chains))
        chains = 1:metadata$n.chains
    
    print(paste0("Running MCMC (",
                 ifelse(length(chains)==1, 'chain ', 'chains '),
                 paste0(chains, collapse=','),
                 " of ", metadata$n.chains,
                 ") for ", msa.names(metadata$location)))
    
    mcmc = run.mcmc.from.cache(dir=cache.dir,
                               chains=chains,
                               update.frequency = update.frequency,
                               update.detail = update.detail)
    
    if (remove.cache.when.done)
    {
        if (metadata$n.chains==mcmc@n.chains)
            to.save=T
        else if (is.mcmc.cache.complete(cache.dir))
        {
            mcmc = assemble.mcmc.from.cache(cache.dir)
            to.save=T
        }
        else
            to.save=F
        
        
        #-- Save and return --#
        if (to.save)
        {
            filename = paste0(basename(cache.dir), '.Rdata')
            save(mcmc, file=file.path(metadata$save.dir, filename))
            file.remove(file.path(cache.dir, 'metadata.Rdata'))
            remove.mcmc.cache(cache.dir)
        }
    }
    
    mcmc
    
}
