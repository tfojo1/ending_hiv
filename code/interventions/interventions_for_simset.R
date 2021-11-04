
CURRENT.GAINS.END.BY.YEAR = Uniform.Distribution(2020,2025)
TOTAL.FUTURE.SLOPE.OR.DIST = Lognormal.Distribution(0, log(1.05))

prepare.simset.for.interventions <- function(simset,
                                             
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
        
        get.components.fn = get.components.function.for.version(VERSION.MANAGER, components$settings$VERSION)
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


run.simset.intervention <- function(simset,
                                    intervention,
                                    run.from.year=attr(simset, 'run.from.year'),
                                    run.to.year=2030,
                                    keep.years=(run.from.year-1):run.to.year,
                                    save.intervention=T,
                                    verbose=F,
                                    update.progress=if (verbose) function(n){print(paste0("Running simulation ", n, " of ", simset@n.sim))} else NULL,
                                    seed = 12343)
{
    # If we need to add parameters from distributions, do so now
    if (length(intervention$parameter.distributions)>0)
    {
        if (verbose)
            print("Adding new parameters")
        
        if (!is.null(seed))
            to.reset.seed = round(runif(1, min=0, max=.Machine$integer.max))
        
        new.params = NULL
        new.param.names = character()
        for (i in 1:length(intervention$parameter.distributions))
        {
            dist = intervention$parameter.distributions[[i]]
            if (!is.null(seed))
            {
                dist.names = paste0(dist@var.names, collapse=',')
                dist.seed = as.integer( (sum(as.integer(charToRaw(dist.names))) + seed) %% .Machine$integer.max )
                set.seed(dist.seed)
            }
            
            new.params = cbind(new.params, generate.random.samples(dist, simset@n.sim))
            new.param.names = c(new.param.names, dist@var.names)   
        }
        
        #reset the seed
        if (!is.null(seed))
            set.seed(to.reset.seed)
        
        bounds = get.support.bounds(dist@support)
        
        simset = add.parameters(simset, 
                                parameters=new.params,
                                parameter.names = new.param.names,
                                parameter.lower.bounds = bounds[1,],
                                parameter.upper.bounds = bounds[2,])
    }
    
    if (verbose)
        print(paste0("Running intervention ", get.intervention.name(intervention), " on ", simset@n.sim, " simulations"))
    
    need.to.resolve.intervention = !intervention.is.resolved(intervention)
    simset@simulations = lapply(1:simset@n.sim, function(i){
        if (!is.null(update.progress))
            update.progress(i)
        
        if (need.to.resolve.intervention)
            int = resolve.intervention(intervention, simset@parameters[i,])
        else
            int = intervention
        
        run.sim.intervention(simset@simulations[[i]], int,
                             run.from.year=run.from.year, run.to.year = run.to.year,
                             keep.years = keep.years)
    })
    
 #   simset = extend.simulations(simset, function(sim, parameters){
 #       components = setup.components.for.intervention(attr(sim, 'components'), intervention)
 #       run.jheem.from.components(components, start.year=run.from.year, end.year=run.to.year,
 #                                 prior.results = sim, keep.components = T, keep.years=keep.years)
 #   })
    if (verbose)
        print("Done")
    
    if (save.intervention)
        attr(simset, 'intervention') = intervention
    
    simset
}

run.sim.intervention <- function(sim,
                                 intervention,
                                 run.from.year,
                                 run.to.year,
                                 keep.years=run.from.year:run.to.year)
{
    if (!is(intervention, 'list'))
        intervention = list(intervention)
    
    components = attr(sim, 'components')

    for (int in intervention)
        components = setup.components.for.intervention(components, int, overwrite.prior.intervention=F)
    
    sim = run.jheem.from.components(components, start.year=run.from.year, end.year=run.to.year,
                              prior.results = sim, keep.components = T, keep.years=keep.years,
                              pare.components = T)
    
    sim
}

run.multiple.simset.interventions <- function(simset,
                                              target.populations=DEFAULT.TARGET.POPULATIONS,
                                              testing.frequency=NA,
                                              suppressed.proportion=NA,
                                              prep.coverage=NA,
                                              intervention.ramped.up.year = 2021,
                                              run.from.year=attr(simset, 'run.from.year'),
                                              run.to.year=2030,
                                              keep.years=run.from.year:run.to.year,
                                              dir='../code/results/baltimore_v1',
                                              overwrite.prior=T)
{
    orig.simset = simset

    for (t.pop in target.populations)
    {
        intervention = create.one.intervention(t.pop,
                                               testing.frequency = testing.frequency,
                                               suppressed.proportion = suppressed.proportion,
                                               prep.coverage = prep.coverage,
                                               intervention.ramped.up.year = intervention.ramped.up.year)

        filename = file.path(dir,
                             paste0(get.intervention.filename(intervention),
                                    ".Rdata"))

        if (!overwrite.prior && file.exists(filename))
        {
            print(paste0("Skipping intervention ", intervention$name, " as it has been done previously"))
        }
        else
        {
            print(paste0("Running intervention *",
                         intervention$name, "* on ",
                         orig.simset@n.sim, " simulations"))

            simset = run.simset.intervention(orig.simset, intervention, run.from.year = run.from.year, run.to.year = run.to.year, keep.years=keep.years)
            print(paste0("  Done. Saving to ", filename))
            save(simset, file=filename)
        }
    }
}
