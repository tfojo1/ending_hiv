

msa = '35620'
version = 'expanded_1.0'

source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

pp = get.initial.starting.parameters(msa, version)
run.simulation = create.run.simulation.function(msa, start.values = pp, version=version)
likelihood = create.msa.likelihood(msa, version)
liks = attr(likelihood, 'components')

sim1 = run.simulation(pp)

simplot(sim1)
simplot(sim1, data.types='testing')

likelihood(sim1)
sapply(liks, function(lik){lik(sim1)})

liks[['testing']](sim1, debug=T)

lik.testing = create.msa.likelihood(msa, version, debug.component = 'testing')
