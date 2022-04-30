
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




