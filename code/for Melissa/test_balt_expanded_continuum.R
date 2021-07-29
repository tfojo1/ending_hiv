
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
    plot.calibration(sim1, data.types=c('linkage','suppression'), facet.by=NULL)
    plot.calibration(sim1, facet.by='risk', split.by='sex')
    
    sim2 = test.run.simulation(msm.proportion.linked.or=2)
    plot.calibration(list(sim1,sim2), data.types='linkage')
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
