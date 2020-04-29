source('../code/interventions/parse_interventions.R')
source('../code/source_code.R')
load('../code/results/baltimore_v3/base.simset.Rdata')

#set up the initial
if (1==2)
{
    load('../code/mcmc_runs/balt6_prev.inflated_cv.035.Rdata')
    simset = extract.simset(mcmc, additional.burn=298)

    #let's fix the missing components
    #get init.components from the mcmc file
    simset = extend.simulations(simset, function(sim, parameters){
        attr(sim, 'components') =
            components = set.background.change.to.years(attr(sim, 'components'),
                                                        testing.change.to.year=round(parameters['current.gains.end.by.year']),
                                                        suppression.change.to.year=round(parameters['current.gains.end.by.year']),
                                                        prep.change.to.year=round(parameters['current.gains.end.by.year']))
        sim
    })
    bk = simset

    simset = prepare.simset.for.interventions(simset)
    save(simset, file='../code/results/baltimore_v1/base.simset.Rdata')



    #for v2
    current.gains.end.by.year = Uniform.Distribution(2018.500001,2023.499999)
    mcmc = assemble.mcmc.from.cache('../code/mcmc_runs/balt7_prev.inflated_cv.045.063_dx.wt.sqrt(10)_2020-03-03/',T)
    simset = extract.simset(mcmc, additional.burn=130)

    simset@parameters[,'current.gains.end.by.year'] = generate.random.samples(current.gains.end.by.year, simset@n.sim)
    simset = extend.simulations(simset, function(sim, parameters){
            components = attr(sim, 'components')

            if (any(names(parameters)=='total.future.suppressed.slope.or'))
                total.future.suppressed.slope.or = parameters['total.future.suppressed.slope.or']
            else
                total.future.suppressed.slope.or = components$background.suppression.inputs$total.future.suppressed.slope.or
            components = set.background.suppression(components=components,
                                                    data.managers=ALL.DATA.MANAGERS,
                                                    msa=components$background.suppression.inputs$msa,
                                                    max.smoothed.suppressed.proportion=components$background.suppression.inputs$max.smoothed.suppressed.proportion,
                                                    smoothing.years=min(components$background.suppression.inputs$smoothing.years):2023,
                                                    zero.suppression.year=components$background.suppression.inputs$zero.suppression.year,
                                                    initial.suppression.ramp.up.years=components$background.suppression.inputs$initial.suppression.ramp.up.years,
                                                    total.suppressed.or=parameters['total.suppressed.or'],
                                                    total.future.suppressed.slope.or=total.future.suppressed.slope.or,
                                                    anchor.year=components$background.suppression.inputs$anchor.year)

            components = set.background.hiv.testing.proportions(components,
                                                                ALL.DATA.MANAGERS,
                                                                msa=components$background.testing.inputs$msa,
                                                                max.smoothed.testing.proportion=components$background.testing.inputs$max.smoothed.testing.proportion,
                                                                smoothing.years=min(components$background.testing.inputs$smoothing.years):2023,
                                                                age1.testing.log.or.intercept=components$background.testing.inputs$age1.testing.log.or.intercept,
                                                                age1.testing.log.or.slope=components$background.testing.inputs$age1.testing.log.or.slope,
                                                                total.proportion.tested.or=parameters['total.proportion.tested.or'],
                                                                msm.proportion.tested.or=parameters['msm.proportion.tested.or'],
                                                                idu.proportion.tested.or=parameters['idu.proportion.tested.or'],
                                                                total.testing.slope.or=parameters['total.testing.slope.or'],
                                                                anchor.year=components$background.testing.inputs$anchor.year)

            components = set.background.prep.coverage(components,
                                                      ALL.DATA.MANAGERS,
                                                      components$prep.persistence,
                                                      smoothing.years=min(components$background.prep.inputs$smoothing.years):2023)


            attr(sim, 'components') = components
        sim
    })
    save(simset, file='../code/results/baltimore_v3/base.simset.Rdata')

}

#the base case
if (1==2)
{
    simset = run.simset.intervention(simset, NULL, keep.years = 1970:2030)

#    save(simset, file='../code/results/baltimore_v3/base.simset.Rdata')
    save(simset, file='../code/results/baltimore_v3/no.intervention.Rdata')

 #   save(simset, file='../code/results/baltimore_v1/no.intervention.Rdata')

    get.simset.incidence.reduction(simset)
}

#other interventions
if (1==2)
{
    run.multiple.simset.interventions(simset, dir = '../code/results/baltimore_v3', suppressed.proportion = 0.8, prep.coverage = 0.25, testing.frequency = 1, overwrite.prior = F)
    run.multiple.simset.interventions(simset, dir = '../code/results/baltimore_v3', suppressed.proportion = 0.8, prep.coverage = 0.5, testing.frequency = 1, overwrite.prior = F)

    run.multiple.simset.interventions(simset, dir = '../code/results/baltimore_v3', suppressed.proportion = 0.8, prep.coverage = NA, overwrite.prior = F)
    run.multiple.simset.interventions(simset, dir = '../code/results/baltimore_v3', suppressed.proportion = 0.8, prep.coverage = 0.25, overwrite.prior = F)

    run.multiple.simset.interventions(simset, dir = '../code/results/baltimore_v3', suppressed.proportion = 0.9, prep.coverage = 0.25, testing.frequency = 1, overwrite.prior = F)
    run.multiple.simset.interventions(simset, dir = '../code/results/baltimore_v3', suppressed.proportion = 0.9, prep.coverage = 0.5, testing.frequency = 1, overwrite.prior = F)

    run.multiple.simset.interventions(simset, dir = '../code/results/baltimore_v3', suppressed.proportion = 0.9, prep.coverage = 0.75, testing.frequency = 1, overwrite.prior = F)

    run.multiple.simset.interventions(simset, target.populations = DEFAULT.TARGET.POPULATIONS[6], dir = '../code/results/baltimore_v3',
                                      suppressed.proportion = 0.9, prep.coverage = 0.25, testing.frequency = 1, overwrite.prior = F)



    run.multiple.simset.interventions(simset, target.populations = DEFAULT.TARGET.POPULATIONS[2],
                                      suppressed.proportion = 0.9, prep.coverage = 0.75, testing.frequency = 1, overwrite.prior = F)
    run.multiple.simset.interventions(simset, target.populations = DEFAULT.TARGET.POPULATIONS[5],
                                      suppressed.proportion = 0.9, prep.coverage = 0.75, testing.frequency = 1, overwrite.prior = F)
    run.multiple.simset.interventions(simset, target.populations = DEFAULT.TARGET.POPULATIONS[6],
                                      suppressed.proportion = 0.9, prep.coverage = 0.75, testing.frequency = 1, overwrite.prior = F)
    run.multiple.simset.interventions(simset, target.populations = DEFAULT.TARGET.POPULATIONS[7],
                                      suppressed.proportion = 0.9, prep.coverage = 0.75, testing.frequency = 1, overwrite.prior = F)

}
