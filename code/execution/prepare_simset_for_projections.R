
CURRENT.GAINS.END.BY.YEAR = Uniform.Distribution(2020,2025)
TOTAL.FUTURE.SLOPE.OR.DIST = Lognormal.Distribution(0, log(1.05))

prepare.simset.for.intervention <- function(simset,
                                            update.version=NULL,
                                            crunch.components=F,
                                            seed = 234321,
                                            max.run.from.year=Inf,
                                            verbose=F)
{
    if (is.null(update.version))
        version = get.simset.version(simset)
    else
        version = update.version
    
    
    #-- Sample the projection parameters --#
    
    if (verbose)
        cat("Adding new parameters...")
    
    projection.parameters.distribution = get.projection.prarameters.distribution.for.version(version)
    new.params = simulate.values.from.distribution.fixed.seed(distributions = projection.parameters.distribution,
                                                              n = simset@n.sim,
                                                              seed = seed)
    simset = add.parameters(simset, 
                            parameters=new.params$values,
                            parameter.names = new.params$names,
                            parameter.lower.bounds = new.params$lower.bounds,
                            parameter.upper.bounds = new.params$upper.bounds)
    
    
    if (verbose)
        cat("done.\n")
    
    update.components.fn = get.projection.update.components.function.for.version(version)
    
    #-- Set up for if we need to update version --#
    
    need.to.update.version = !is.null(update.version) && update.version != get.simset.version(simset)
    if (need.to.update.version)
    {
        init.components = create.initial.components(location = get.simset.location(simset),
                                                    version = update.version,
                                                    start.values = simset@parameters[1,],
                                                    fix.components = F)
        
        get.components.fn = get.components.function.for.version(update.version)
    }
    
    
    if (verbose)
        cat("Preparing to update simulations...\n")
    
    # Push through to the sims
    counter = 1
    simset = extend.simulations(simset, function(sim, parameters){
        
        if (is.sim.compressed(sim))
            stop("simset simulations are compressed - cannot prepare for projections")
        
        if (verbose)
            cat("preparing simulation ", counter, " of ", simset@n.sim, "...", sep='')
        
        if (need.to.update.version)
        {            
            if (verbose)
                cat("redoing components...")
            # redo components
            components = get.components.fn(parameters, init.components)
            
            if (verbose)
                cat("expanding prior results...")
            # expand sim
            sim = expand.jheem.results(sim, components$jheem)
        }
        else
        {
            components = get.sim.components(sim)
            components = unfix.jheem.components(components)
        }
        
        components = update.components.fn(parameters, components)
        
        if (crunch.components)
            components = crunch.all.jheem.components(components)
        
        counter <<- counter + 1
        if (verbose)
            cat('done.\n')
        
        #for backwards compatibility with G1 models
        if (!is(components, 'jheem.components') && is.list(components))
            class(components) = 'jheem.components'
        
        set.sim.components(sim, components)
    })
    
#    min.future.slope.after.or.change.to.year = min(sapply(simset@simulations, function(sim){
#        components = get.sim.components(sim)
#        min(c(get.future.background.after.years(components),
#              get.background.change.to.years(components)),
#            na.rm=T)
#    }), na.rm=T)
    
    parameters.for.run.from = grepl('year', new.params$names)
    if (any(parameters.for.run.from))
        min.future.slope.after.or.change.to.year = min(new.params$lower.bounds[parameters.for.run.from])
    else
        min.future.slope.after.or.change.to.year = Inf
    
    run.from.year = min(max.run.from.year,
                        min.future.slope.after.or.change.to.year,
                        as.numeric(max(ALL.DATA.MANAGERS$census.totals$years)))
    
    attr(simset, 'run.from.year') = run.from.year
    
    # Return
    simset
}

is.simset.prepared.for.projection <- function(simset)
{
    version = get.simset.version(simset)
    projection.parameters.distribution = get.projection.prarameters.distribution.for.version(version)
    
    length(setdiff(projection.parameters.distribution@var.names, simset@parameter.names))==0
}


OLD.prepare.simset.for.interventions <- function(simset,
                                             
                                             testing.gains.end.by.year.dist = CURRENT.GAINS.END.BY.YEAR,
                                             prep.gains.end.by.year.dist = CURRENT.GAINS.END.BY.YEAR,
                                             suppression.gains.end.by.year.dist = CURRENT.GAINS.END.BY.YEAR,
                                             linkage.gains.end.by.year.dist = CURRENT.GAINS.END.BY.YEAR,
                                             retention.gains.end.by.year.dist = CURRENT.GAINS.END.BY.YEAR,
                                             reengagement.gains.end.by.year.dist = CURRENT.GAINS.END.BY.YEAR,
                                             
                                             total.future.testing.slope.or.dist = TOTAL.FUTURE.SLOPE.OR.DIST,
                                             total.future.prep.slope.or.dist = TOTAL.FUTURE.SLOPE.OR.DIST,
                                             total.future.suppressed.slope.or.dist = TOTAL.FUTURE.SLOPE.OR.DIST,
                                             
                                             total.future.unsuppression.slope.or.dist = TOTAL.FUTURE.SLOPE.OR.DIST,
                                             total.future.linkage.slope.or.dist = TOTAL.FUTURE.SLOPE.OR.DIST,
                                             total.future.disengagement.slope.or.dist = TOTAL.FUTURE.SLOPE.OR.DIST,
                                             total.future.reengagement.slope.or.dist = TOTAL.FUTURE.SLOPE.OR.DIST,
                                             
                                             future.slope.after.year=2020,
                                             crunch.components=F,
                                             max.run.from.year=2018,
                                             redo.needle.exchange=T)
{
    #-- Flatten it out --#
    simset = flatten.simset(simset)
    
    #-- Future Slopes --#
    
    simset = add.parameters(simset,
                            future.slope.after.year,
                            parameter.names='future.slope.after.year',
                            parameter.lower.bounds = future.slope.after.year,
                            parameter.upper.bounds = future.slope.after.year)
    
    total.future.testing.slope.bounds = get.support.bounds(total.future.testing.slope.or.dist@support)
    simset = add.parameters(simset,
                            generate.random.samples(total.future.testing.slope.or.dist, simset@n.sim),
                            parameter.names = 'total.future.testing.slope.or',
                            parameter.lower.bounds = total.future.testing.slope.bounds[1],
                            parameter.upper.bounds = total.future.testing.slope.bounds[2],
                            parameter.transformations = 'log')

    total.future.prep.slope.bounds = get.support.bounds(total.future.prep.slope.or.dist@support)
    simset = add.parameters(simset,
                            generate.random.samples(total.future.prep.slope.or.dist, simset@n.sim),
                            parameter.names = 'total.future.prep.slope.or',
                            parameter.lower.bounds = total.future.prep.slope.bounds[1],
                            parameter.upper.bounds = total.future.prep.slope.bounds[2],
                            parameter.transformations = 'log')

    total.future.suppressed.slope.bounds = get.support.bounds(total.future.suppressed.slope.or.dist@support)
    simset = add.parameters(simset,
                            generate.random.samples(total.future.suppressed.slope.or.dist, simset@n.sim),
                            parameter.names = 'total.future.suppressed.slope.or',
                            parameter.lower.bounds = total.future.suppressed.slope.bounds[1],
                            parameter.upper.bounds = total.future.suppressed.slope.bounds[2],
                            parameter.transformations = 'log')

    if (!attr(simset@simulations[[1]], 'components')$settings$IS_CONTINUUM_COLLAPSED)
    {
        total.future.unsuppression.slope.bounds = get.support.bounds(total.future.unsuppression.slope.or.dist@support)
        simset = add.parameters(simset,
                                generate.random.samples(total.future.unsuppression.slope.or.dist, simset@n.sim),
                                parameter.names = 'total.future.unsuppression.slope.or',
                                parameter.lower.bounds = total.future.unsuppression.slope.bounds[1],
                                parameter.upper.bounds = total.future.unsuppression.slope.bounds[2],
                                parameter.transformations = 'log')
        
        total.future.linkage.slope.bounds = get.support.bounds(total.future.linkage.slope.or.dist@support)
        simset = add.parameters(simset,
                                generate.random.samples(total.future.linkage.slope.or.dist, simset@n.sim),
                                parameter.names = 'total.future.linkage.slope.or',
                                parameter.lower.bounds = total.future.linkage.slope.bounds[1],
                                parameter.upper.bounds = total.future.linkage.slope.bounds[2],
                                parameter.transformations = 'log')
        
        total.future.disengagement.slope.bounds = get.support.bounds(total.future.disengagement.slope.or.dist@support)
        simset = add.parameters(simset,
                                generate.random.samples(total.future.disengagement.slope.or.dist, simset@n.sim),
                                parameter.names = 'total.future.disengagement.slope.or',
                                parameter.lower.bounds = total.future.disengagement.slope.bounds[1],
                                parameter.upper.bounds = total.future.disengagement.slope.bounds[2],
                                parameter.transformations = 'log')
        
        total.future.reengagement.slope.bounds = get.support.bounds(total.future.reengagement.slope.or.dist@support)
        simset = add.parameters(simset,
                                generate.random.samples(total.future.reengagement.slope.or.dist, simset@n.sim),
                                parameter.names = 'total.future.reengagement.slope.or',
                                parameter.lower.bounds = total.future.reengagement.slope.bounds[1],
                                parameter.upper.bounds = total.future.reengagement.slope.bounds[2],
                                parameter.transformations = 'log')
    }
    #-- Gains End By Year --#
    
    testing.gains.end.by.bounds = get.support.bounds(testing.gains.end.by.year.dist@support)
    simset = add.parameters(simset, 
                            parameters = generate.random.samples(testing.gains.end.by.year.dist, simset@n.sim),
                            parameter.names = 'testing.gains.end.by.year',
                            parameter.lower.bounds = testing.gains.end.by.bounds[1],
                            parameter.upper.bounds = testing.gains.end.by.bounds[2])
    
    prep.gains.end.by.bounds = get.support.bounds(prep.gains.end.by.year.dist@support)
    simset = add.parameters(simset, 
                            parameters = generate.random.samples(prep.gains.end.by.year.dist, simset@n.sim),
                            parameter.names = 'prep.gains.end.by.year',
                            parameter.lower.bounds = prep.gains.end.by.bounds[1],
                            parameter.upper.bounds = prep.gains.end.by.bounds[2])
    
    suppression.gains.end.by.bounds = get.support.bounds(suppression.gains.end.by.year.dist@support)
    simset = add.parameters(simset, 
                            parameters = generate.random.samples(suppression.gains.end.by.year.dist, simset@n.sim),
                            parameter.names = 'suppression.gains.end.by.year',
                            parameter.lower.bounds = suppression.gains.end.by.bounds[1],
                            parameter.upper.bounds = suppression.gains.end.by.bounds[2])
    
    if (!attr(simset@simulations[[1]], 'components')$settings$IS_CONTINUUM_COLLAPSED)
    {
        linkage.gains.end.by.bounds = get.support.bounds(linkage.gains.end.by.year.dist@support)
        simset = add.parameters(simset, 
                                parameters = generate.random.samples(linkage.gains.end.by.year.dist, simset@n.sim),
                                parameter.names = 'linkage.gains.end.by.year',
                                parameter.lower.bounds = linkage.gains.end.by.bounds[1],
                                parameter.upper.bounds = linkage.gains.end.by.bounds[2])
        
        
        retention.gains.end.by.bounds = get.support.bounds(retention.gains.end.by.year.dist@support)
        simset = add.parameters(simset, 
                                parameters = generate.random.samples(retention.gains.end.by.year.dist, simset@n.sim),
                                parameter.names = 'retention.gains.end.by.year',
                                parameter.lower.bounds = retention.gains.end.by.bounds[1],
                                parameter.upper.bounds = retention.gains.end.by.bounds[2])
        
        
        reengagement.gains.end.by.bounds = get.support.bounds(reengagement.gains.end.by.year.dist@support)
        simset = add.parameters(simset, 
                                parameters = generate.random.samples(reengagement.gains.end.by.year.dist, simset@n.sim),
                                parameter.names = 'reengagement.gains.end.by.year',
                                parameter.lower.bounds = reengagement.gains.end.by.bounds[1],
                                parameter.upper.bounds = reengagement.gains.end.by.bounds[2])
        
        expanded.run.from.year = min(linkage.gains.end.by.bounds[1],
                                     retention.gains.end.by.bounds[1],
                                     reengagement.gains.end.by.bounds[1])
    }
    else
        expanded.run.from.year = Inf

    #-- Re-render the simset (components for each sim) --#
    
    simset = extend.simulations(simset, function(sim, parameters){
        components = attr(sim, 'components')
        
        components = unfix.jheem.components(components)
        
        if (redo.needle.exchange)
        {
            components = setup.needle.exchange.remission.effect(components, 
                                                                needle.exchange.remission.rate.ratio = BASE_PARAMETER_VALUES['needle.exchange.remission.rate.ratio'])
            components = setup.needle.exchange.susceptibility(components,
                                                              needle.exchange.rr = BASE_PARAMETER_VALUES['needle.exchange.rr'])
        }
        
        get.components.fn = get.components.function.for.version(components$settings$VERSION)
        components = get.components.fn(parameters, components)
        
#        if (fix.components)
#            components = fix.jheem.components(components)
#        else
#            components = crunch.all.jheem.components(components)

        if (crunch.components)
            components = crunch.all.jheem.components(components)

        attr(sim, 'components') = components
        sim
    })
    
    attr(simset, 'run.from.year') = min(max(simset@simulations[[1]]$years),
                                        future.slope.after.year,
                                        testing.gains.end.by.bounds[1],
                                        prep.gains.end.by.bounds[1],
                                        suppression.gains.end.by.bounds[1],
                                        expanded.run.from.year,
                                        max.run.from.year)

    simset
}




