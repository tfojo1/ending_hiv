#setwd('../../../Ending HIV/Ending_HIV/')

source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

# Get our starting points
#mcmc = assemble.mcmc.from.cache('Q:Ending_HIV/mcmc_runs/systematic_caches/38900_1x20K_manual_2022-02-20',T)
#sim1 = mcmc@simulations[[1]]
#sim2 = mcmc@simulations[[length(mcmc@simulations)]]
#msa = attr(sim1, 'location')
#pp1 = mcmc@samples[1,1,]
#pp2 = mcmc@samples[1,mcmc@n.iter,]
load('mcmc_runs/temp_debugging.Rdata')
run.simulation = create.run.simulation.function(msa, pp2)



likelihood = OLD.create.msa.likelihood.v1.for.annals(msa)

N.SIM = 100

DEFAULT.JHEEM.ATOL = DEFAULT.JHEEM.RTOL = 1e-03
start.e3 = Sys.time()
sims.tol.e3 = lapply(1:N.SIM, function(i){run.simulation(pp2)})
end.e3 = Sys.time()


DEFAULT.JHEEM.ATOL = DEFAULT.JHEEM.RTOL = 1e-04
start.e4 = Sys.time()
sims.tol.e4 = lapply(1:N.SIM, function(i){run.simulation(pp2)})
end.e4 = Sys.time()

DEFAULT.JHEEM.ATOL = DEFAULT.JHEEM.RTOL = 1e-05
start.e5 = Sys.time()
sims.tol.e5 = lapply(1:N.SIM, function(i){run.simulation(pp2)})
end.e5 = Sys.time()

DEFAULT.JHEEM.ATOL = DEFAULT.JHEEM.RTOL = 1e-06
start.e6 = Sys.time()
sims.tol.e6 = lapply(1:N.SIM, function(i){run.simulation(pp2)})
end.e6 = Sys.time()


DEFAULT.JHEEM.ATOL = DEFAULT.JHEEM.RTOL = 1e-07
start.e7 = Sys.time()
sims.tol.e7 = lapply(1:N.SIM, function(i){run.simulation(pp2)})
end.e7 = Sys.time()


DEFAULT.JHEEM.ATOL = DEFAULT.JHEEM.RTOL = 1e-08
start.e8 = Sys.time()
sims.tol.e8 = lapply(1:N.SIM, function(i){run.simulation(pp2)})
end.e8 = Sys.time()


liks.e3 = sapply(sims.tol.e3, likelihood)
liks.e4 = sapply(sims.tol.e4, likelihood)
liks.e5 = sapply(sims.tol.e5, likelihood)
liks.e6 = sapply(sims.tol.e6, likelihood)
liks.e7 = sapply(sims.tol.e7, likelihood)
liks.e8 = sapply(sims.tol.e8, likelihood)

sd(liks.e3);sd(liks.e4);sd(liks.e5);sd(liks.e6); sd(liks.e7); sd(liks.e8)

as.numeric(end.e3-start.e3) / as.numeric(end.e6 - start.e6)
as.numeric(end.e4-start.e4) / as.numeric(end.e6 - start.e6)
as.numeric(end.e5-start.e5) / as.numeric(end.e6 - start.e6)
as.numeric(end.e7-start.e7) / as.numeric(end.e6 - start.e6)
as.numeric(end.e8-start.e8) / as.numeric(end.e6 - start.e6)
