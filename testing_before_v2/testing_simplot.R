

# The code for pulling up and saving the test sim
if (1==2)
{
  load('P:JHEEM/simulations/ehe/1.0_12580_full.Rdata')
  sim = simset@simulations[[1]]
  sim2 = simset@simulations[[1000]]
  save(sim, sim2, file='simulations/test_sim.Rdata')
}

source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

load('simulations/test_sim.Rdata')
simplot(sim)
simplot(sim, sim2)
