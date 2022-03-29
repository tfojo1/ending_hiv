


source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

#load up the prior values somehow
pp = pp2

msm.trates = names(pp)[grepl('msm.trate',names(pp))]
msm.trates.1 = names(pp)[grepl('msm.trate.1',names(pp))]
msm.trates.2 = names(pp)[grepl('msm.trate.2',names(pp))]
idu.trates = names(pp)[grepl('idu.trate',names(pp))]
idu.trates.1 = names(pp)[grepl('idu.trate.1',names(pp))]
idu.trates.2 = names(pp)[grepl('idu.trate.2',names(pp))]
het.trates = names(pp)[grepl('heterosexual.trate',names(pp))]
het.trates.1 = names(pp)[grepl('heterosexual.trate.1',names(pp))]
het.trates.2 = names(pp)[grepl('heterosexual.trate.2',names(pp))]
msm.idu.multipliers = names(pp)[grepl('msm.vs',names(pp))]
testing = names(pp)[grepl('proportion.tested', names(pp))]
suppression = names(pp)[grepl('suppressed', names(pp))]







#-- MSA specific --#

msa = BALTIMORE.MSA
version = 'expanded_1.0'
run.simulation = create.run.simulation.function(msa, start.values=pp, version=version)



#lik = create.msa.likelihood(msa)

sim1 = run.simulation(pp)
sim2 = run.simulation(pp2)
simplot(sim1,sim2, facet.by='risk')
simplot(sim1,sim2, data.types=c('testing','diagnosed'))

pp2 = pp
pp2['msm.proportion.tested.or'] = .6
pp2['msm.proportion.tested.slope.or'] = .88

pp2[msm.trates] = pp[msm.trates] * 1
pp2[msm.trates.1] = pp[msm.trates.1] * 0.9
pp2[msm.trates.2] = pp[msm.trates.2] * 0.9

sim2 = run.simulation(pp2)
simplot(sim2, facet.by='risk')
simplot(sim2, data.types=c('testing','diagnosed'))
simplot(sim2, data.types=c('testing'), facet.by='risk')

starting.parameters = pp2; msa.names(msa)
save(starting.parameters, file=file.path(SYSTEMATIC.ROOT.DIR, 'start_values', version, paste0(msa, '.Rdata')));  save(starting.parameters, file=file.path('mcmc_runs/start_values', version, paste0(msa, '.Rdata')))



