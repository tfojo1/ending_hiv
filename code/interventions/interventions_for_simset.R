
CURRENT.GAINS.END.BY.YEAR = Uniform.Distribution(2018,2023)
TOTAL.FUTURE.SUPPRESSED.SLOPE.OR.DIST = Lognormal.Distribution(0, log(1.05))

prepare.simset.for.interventions <- function(simset,
                                             current.gains.end.by.year.dist = CURRENT.GAINS.END.BY.YEAR,
                                             total.future.suppressed.slope.or.dist = TOTAL.FUTURE.SUPPRESSED.SLOPE.OR.DIST)
{
    total.future.suppressed.slope.bounds = get.support.bounds(total.future.suppressed.slope.or.dist@support)
    simset = add.parameters(simset,
                            generate.random.samples(total.future.suppressed.slope.or.dist, simset@n.sim),
                            parameter.names = 'total.future.suppressed.slope.or',
                            parameter.lower.bounds = total.future.suppressed.slope.bounds[1],
                            parameter.upper.bounds = total.future.suppressed.slope.bounds[2],
                            parameter.transformations = 'log')

    current.gains.end.by.bounds = get.support.bounds(current.gains.end.by.year.dist@support)
    simset = add.parameters(simset, 
                            parameters = generate.random.samples(current.gains.end.by.year.dist, simset@n.sim),
                            parameter.names = 'current.gains.end.by.year',
                            parameter.lower.bounds = current.gains.end.by.bounds[1],
                            parameter.upper.bounds = current.gains.end.by.bounds[2])

    simset = extend.simulations(simset, function(sim, parameters){
        components = attr(sim, 'components')
        
        components = unfix.jheem.components(components)
        
        components = get.components.for.calibrated.parameters(parameters, components)
        components = fix.jheem.components(components)

        attr(sim, 'components') = components
        sim
    })
    
    attr(simset, 'run.from.year') = min(max(simset@simulations[[1]]$years),
                                        current.gains.end.by.bounds[1])

    simset
}


run.simset.intervention <- function(simset,
                                    intervention,
                                    run.from.year=attr(simset, 'run.from.year'),
                                    run.to.year=2030,
                                    keep.years=run.from.year:run.to.year,
                                    save.intervention=T)
{
    simset = extend.simulations(simset, function(sim, parameters){
        components = setup.components.for.intervention(attr(sim, 'components'), intervention)
        run.jheem.from.components(components, start.year=run.from.year, end.year=run.to.year,
                                  prior.results = sim, keep.components = F, keep.years=keep.years)
    })
    
    if (save.intervention)
        attr(simset, 'intervention') = interventions
    
    simset
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
