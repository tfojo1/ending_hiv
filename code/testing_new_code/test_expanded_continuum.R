

source('code/source_code.R')
source('code/plots.R')
source('code/calibration/calibrated_parameters_expanded_1.R')

SETTINGS = SETTINGS.EXPANDED.CONTINUUM

msa = '12580'

#test of run from parameters
if (1==2)
{
    pp = get.medians(parameters.prior)
#    pp['global.trate'] = .14
    load('mcmc_runs/quick_simsets/1.0_12580_quick.Rdata')
    common.param.names = intersect(names(pp), simset@parameter.names)
    pp[common.param.names] = simset@parameters[simset@n.sim, common.param.names]
    
    base.components = setup.initial.components(msa=msa)
    base.components = setup.trates(base.components, 
                                   r0 = 1,
                                   r1 = 1,
                                   r2 = 1,
                                   r.peak = 1
    )
    
    components = get.components.for.calibrated.parameters(parameters=pp, components=base.components)
    
    sim = run.jheem.from.components(components)
    
    plot.calibration(sim, data.types='new')
    
    plot.calibration(sim, data.types='linkage', facet.by=NULL)
    plot.calibration(sim, data.types='engagement', facet.by=NULL)
    plot.calibration(sim, data.types='suppression', facet.by=NULL)
    
    pp2 = pp
    pp2['msm.proportion.linked.or'] = 1/100
    
    components2 = get.components.for.calibrated.parameters(parameters=pp2, components=base.components)
    sim2 = run.jheem.from.components(components2)
    
    
    plot.calibration(list(sim, sim2), data.types='new')
}


#This was the initial test 
if (1==2)
{
base.components = setup.initial.components(msa=msa)
base.components = setup.trates(base.components, 
                               r0 = 1,
                               r1 = 1,
                               r2 = 1,
                               r.peak = 1
                               )
components = crunch.all.jheem.components(base.components)

adj.components = setup.global.trates(components,
                                 global.sexual.trate=0.14,
                                 global.idu.trate=0.14)

sim = run.jheem.from.components(adj.components)

plot.calibration(sim, data.types='new')
plot.calibration(sim)

#init.components = get.components.for.calibrated.parameters(start.values, base.components)
#init.components = fix.components.for.calibration(components = init.components)
}