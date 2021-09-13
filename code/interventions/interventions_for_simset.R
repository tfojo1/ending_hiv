
CURRENT.GAINS.END.BY.YEAR = Uniform.Distribution(2020,2025)
TOTAL.FUTURE.SLOPE.OR.DIST = Lognormal.Distribution(0, log(1.05))

prepare.simset.for.interventions <- function(simset,
                                             testing.gains.end.by.year.dist = CURRENT.GAINS.END.BY.YEAR,
                                             prep.gains.end.by.year.dist = CURRENT.GAINS.END.BY.YEAR,
                                             suppression.gains.end.by.year.dist = CURRENT.GAINS.END.BY.YEAR,
                                             total.future.testing.slope.or.dist = TOTAL.FUTURE.SLOPE.OR.DIST,
                                             total.future.prep.slope.or.dist = TOTAL.FUTURE.SLOPE.OR.DIST,
                                             total.future.suppressed.slope.or.dist = TOTAL.FUTURE.SLOPE.OR.DIST,
                                             future.slope.after.year=2020,
                                             crunch.components=F,
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
        components = get.components.for.calibrated.parameters(parameters, components)
        
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
                                        suppression.gains.end.by.bounds[1])

    simset
}


run.simset.intervention <- function(simset,
                                    intervention,
                                    run.from.year=attr(simset, 'run.from.year'),
                                    run.to.year=2030,
                                    keep.years=(run.from.year-1):run.to.year,
                                    save.intervention=T,
                                    verbose=F,
                                    update.progress=if (verbose) function(n){print(paste0("Running simulation ", n, " of ", simset@n.sim))} else NULL)
{
    if (verbose)
        print(paste0("Running intervention ", get.intervention.name(intervention), " on ", simset@n.sim, " simulations"))
    simset@simulations = lapply(1:simset@n.sim, function(i){
        if (!is.null(update.progress))
            update.progress(i)
        run.sim.intervention(simset@simulations[[i]], intervention,
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
    
    run.jheem.from.components(components, start.year=run.from.year, end.year=run.to.year,
                              prior.results = sim, keep.components = T, keep.years=keep.years,
                              pare.components = T)
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
