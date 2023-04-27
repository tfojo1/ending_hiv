load('mcmc_runs/quick_simsets/ex1.0_12580_full.Rdata')

sim = simset@simulations[[1]]
no.int = MELISSA.CROI.INTERVENTIONS.2022[[length(MELISSA.CROI.INTERVENTIONS.2022)]]
whole.pop.int = MELISSA.CROI.INTERVENTIONS.2022[[6]]

sim.no.int = run.sim.intervention(sim,no.int, run.from.year = 2018, run.to.year=2035)
sim.whole.pop.int = run.sim.intervention(sim,run.from.year = 2018, whole.pop.int, run.to.year = 2035)

simplot(sim.no.int,sim.whole.pop.int, data.types = "prep")
