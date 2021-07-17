

source('code/source_code.R')

load('mcmc_runs/start_values/12580.Rdata')



run.simulation = create.run.simulation.function('12580', starting.parameters)

sim = run.simulation(starting.parameters)

source('code/plots.R')
plot.calibration(sim)

# Avoid the try-catch in the run-simulation function
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
