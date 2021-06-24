

source('code/source_code.R')
source('code/plots.R')

SETTINGS = SETTINGS.EXPANDED.CONTINUUM

msa = '12580'
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