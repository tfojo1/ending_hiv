
source('code/source_code.R')
source('code/calibration/calibrated_parameters_expanded_1.R')
source('code/plots.R')


load('code/for Melissa/balt_starting_params.Rdata')

run.simulation.fn = create.run.simulation.function(msa=BALTIMORE.MSA,
                                                start.values = starting.parameters,
                                                settings=SETTINGS.EXPANDED.CONTINUUM,
                                                catch.errors = F, fix.components=F)

test.run.simulation <- function(...)
{
    pp = unlist(list(...))
    parameters = starting.parameters
    parameters[names(pp)] = pp
    
    run.simulation.fn(parameters)
}

if (1==2)
{
    #what parameters should we check?
    param.names = names(starting.parameters)
    param.names[grepl('link', param.names) |
                    grepl('adher', param.names) |
                    grepl('lost', param.names)]
    
    
    sim1 = test.run.simulation()
    plot.calibration(sim1)

    ## Checking linkage ##
    
    # Risk groups: Only MSM works; nothing happens for other risk groups
    sim2 = test.run.simulation(heterosexual.proportion.linked.or=2)
    plot.calibration(list(sim1,sim2), data.types='linkage')
    
    # Race and age: higher linkage by 2020, but weird slope to get there. Changing the slope.or doesn't matter
    sim2 = test.run.simulation(black.proportion.linked.or=2)
    plot.calibration(list(sim1,sim2), data.types='linkage', facet.by = 'race')
    sim2 = test.run.simulation(black.proportion.linked.or=1.1, black.proportion.linked.slope.or=.5)
    plot.calibration(list(sim1,sim2), data.types='linkage', facet.by = 'race')
    # (Same patterns for hispanic; and age groups)
    
    
    # Slopes - linkage 
    # Risk groups: Look okay except MSM-IDU doesn't do anything
    sim2 = test.run.simulation(heterosexual.proportion.linked.slope.or=1.2)
    plot.calibration(list(sim1,sim2), data.types='linkage')
    
    # Race and age: don't do anything
    sim2 = test.run.simulation(black.proportion.linked.slope.or=2)
    plot.calibration(list(sim1,sim2), data.types='linkage', facet.by = 'race')
    
    
    
    
    ## Checking adherence ##
    
    # Risk groups: Only MSM works; nothing happens for other risk groups
    sim2 = test.run.simulation(heterosexual.proportion.adherent.or=2)
    plot.calibration(list(sim1,sim2), data.types='suppression')
    
    # Race and age: lower suppression by 2020 (wrong), and weird slope to get there. Changing the slope.or doesn't matter
    sim2 = test.run.simulation(black.proportion.adherent.or=2)
    plot.calibration(list(sim1,sim2), data.types='suppression', facet.by = 'race')
    sim2 = test.run.simulation(black.proportion.adherent.or=10, black.proportion.adherent.slope.or=2)
    plot.calibration(list(sim1,sim2), data.types=c('suppression','engagement'), facet.by = 'race')
    # (Same patterns for hispanic; and age groups)
    
    
    # Slopes - adherence 
    # Risk groups: slope looks like the right pattern but 2020 value is lower (wrong). MSM-IDU doesn't do anything
    sim2 = test.run.simulation(msm.proportion.adherent.slope.or=2)
    plot.calibration(list(sim1,sim2), data.types='suppression')
    
    # Race and age: don't do anything
    sim2 = test.run.simulation(black.proportion.adherent.slope.or=2)
    plot.calibration(list(sim1,sim2), data.types='suppression', facet.by = 'race')
    
    
    # Doesn't do anything (maybe *barely* changes MSM with very extreme values)
    sim2 = test.run.simulation(suppressed.vs.nonsuppressed.proportion.adherent.or=200)
    plot.calibration(list(sim1,sim2), data.types='suppression')
    
    
    
    
    ## Checking lost ##
    
    # Risk groups: Only MSM works; nothing happens for other risk groups
    sim2 = test.run.simulation(heterosexual.proportion.lost.or=2)
    plot.calibration(list(sim1,sim2), data.types='suppression')
    
    # Race and age: higher suppression by 2020 (wrong). Changing the slope.or doesn't matter
    sim2 = test.run.simulation(black.proportion.lost.or=2)
    plot.calibration(list(sim1,sim2), data.types='engagement', facet.by = 'race')
    sim2 = test.run.simulation(black.proportion.lost.or=1.1, black.proportion.lost.slope.or=1.1)
    plot.calibration(list(sim1,sim2), data.types='engagement', facet.by = 'race')
    # (Same patterns for hispanic; and age groups)

    
    # Slopes - lost 
    # Risk groups: slope looks like the right pattern but 2020 value is higher (wrong). MSM-IDU doesn't do anything
    sim2 = test.run.simulation(heterosexual.proportion.lost.slope.or=2)
    plot.calibration(list(sim1,sim2), data.types='suppression')
    
    # Race and age: don't do anything
    sim2 = test.run.simulation(black.proportion.lost.slope.or=2)
    plot.calibration(list(sim1,sim2), data.types='engagement', facet.by = 'race')
    
    
    # Only affects MSM
    sim2 = test.run.simulation(suppressed.vs.nonsuppressed.proportion.lost.or=2)
    plot.calibration(list(sim1,sim2), data.types='suppression')
    
    sim2 = test.run.simulation(already.lost.vs.nonsuppressed.proportion.lost.or=2)
    plot.calibration(list(sim1,sim2), data.types='suppression')
    
    
    
    plot.calibration(sim1, data.types=c('linkage','suppression'), facet.by=NULL)
    plot.calibration(sim1, facet.by='risk', split.by='sex')
    
}


#this is the code we wrote to generate the starting parameters
if (1==2)
{
    load('mcmc_runs/visualization_simsets/12580/1.0_12580_baseline.Rdata')
    params.old = simset@parameters[simset@n.sim,]
    prior = get.parameters.prior.for.version(VERSION.MANAGER, version=SETTINGS.EXPANDED.CONTINUUM$VERSION)
    
    starting.parameters = get.medians(prior)
    common.params.to.both = intersect(names(params.old), names(starting.parameters))
    starting.parameters[common.params.to.both] = params.old[common.params.to.both]
    
    save(starting.parameters, file='code/for Melissa/balt_starting_params.Rdata')
}
