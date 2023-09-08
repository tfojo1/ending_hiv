

# The code for pulling up and saving the test sim
if (!==2)
{
  load('P:JHEEM/simulations/ehe/1.0_12580_full.Rdata')
  sim = simset@simulations[[1]]
  save(sim, file='simulations/test_sim.Rdata')
}

source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

load('simulations/test_sim.Rdata')
simplot(sim)
