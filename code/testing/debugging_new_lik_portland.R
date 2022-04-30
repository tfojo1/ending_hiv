

simplot(sim1, sim2, facet.by='race')
simplot(sim1, sim2, facet.by='risk', split.by='race')

new.lik = liks[['new']]

pp3 = pp2
pp3['other.msm.trate.2'] = 1.3
sim3 = run.simulation(pp3)


simplot(sim3, sim2, facet.by='race')
simplot(sim3, sim2, facet.by='risk')
simplot(sim3, sim2, facet.by='age')

new.lik(sim3) - new.lik(sim2)
# sim3 *should* be better, but is not


#-- Checking on the likelihood --#
real.lik = OLD.create.msa.likelihood.v1.for.annals(msa)
real.lik(sim3) - real.lik(sim2)
real.liks = attr(real.lik, 'components')
real.new.lik = real.liks[['new']]

real.new.lik(sim3) - real.new.lik(sim2)


save(sim1, sim2, sim3, pp2, pp3, file='mcmc_runs/portland_debug.Rdata')
