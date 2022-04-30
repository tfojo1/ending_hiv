
source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

load('Q:Ending_HIV/mcmc_runs/systematic_initial_expanded/12580_infl.nobs.wt2_2022-03-07.Rdata')
sim1 = mcmc@simulations[[1]]
sim2 = mcmc@simulations[[length(mcmc@simulations)]]


simplot(sim1, sim2, data.types=c('diagnosed','testing'))
simplot(sim1, sim2, data.types=c('testing'), facet.by='race')

dx.lik = create.msa.likelihood('12580', version='expanded_1.0', debug.component = 'diagnosed')

lik = create.msa.likelihood('12580', version='expanded_1.0', debug.component = 'testing')
lik(sim2) - lik(sim1)

pp2 = mcmc@samples[1,mcmc@n.iter,]

run.simulation = create.run.simulation.function('12580', pp2, version='expanded_1.0')

pp3 = pp2
pp3[grepl('test', names(pp2))]

pp3['msm.proportion.tested.or'] = .6
sim3 = run.simulation(pp3)

simplot(sim1, sim2, sim3, data.types=c('testing'), facet.by='risk')
simplot(sim1, sim2, sim3, data.types=c('testing'), facet.by='race')
simplot(sim1, sim2, sim3, data.types=c('diagnosed','testing'))

t1 = create.msa.likelihood('12580', version='expanded_1.0', debug.component = 'testing',
                           TESTING.STATE.TO.MSA.ERROR.SD.MULTIPLIER = 1)
t2 = create.msa.likelihood('12580', version='expanded_1.0', debug.component = 'testing',
                           TESTING.STATE.TO.MSA.ERROR.SD.MULTIPLIER = 1/2)
t3 = create.msa.likelihood('12580', version='expanded_1.0', debug.component = 'testing',
                           TESTING.STATE.TO.MSA.ERROR.SD.MULTIPLIER = 1/2)

# call this one with additional error
t3 = create.msa.likelihood('12580', version='expanded_1.0', debug.component = 'testing',
                           TESTING.STATE.TO.MSA.ERROR.SD.MULTIPLIER = 1/2)
t1(sim3)-t1(sim2)
t2(sim3)-t2(sim2)
t3(sim3)-t3(sim2)


dx1 = create.msa.likelihood('12580', version='expanded_1.0', debug.component = 'diagnosed', 
                            DX.STATE.TO.MSA.ERROR.SD.MULTIPLIER = 1)
dx2 = create.msa.likelihood('12580', version='expanded_1.0', debug.component = 'diagnosed', 
                            DX.STATE.TO.MSA.ERROR.SD.MULTIPLIER = 1/(2))

dx1(sim3) - dx1(sim2)
dx2(sim3) - dx2(sim2)

simplot(sim1, sim2, sim3, data.types='diagnosed')
simplot(sim1, sim2, sim3, data.types='diagnosed', facet.by='race')


dx1(sim1, debug=T)
