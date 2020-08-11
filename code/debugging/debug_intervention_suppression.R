
load('mcmc_runs/test_simsets/35620.Rdata')
sim = simset@simulations[[1]]

intervention = INTERVENTION.SET[[13]]

components = setup.components.for.intervention(attr(sim, 'components'), intervention)
