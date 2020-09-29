
source('code/source_code.R')
source('code/targets/target_msas.R')
source('code/systematic_calibration/systematic_calibration.R')
load('mcmc_runs/test_runs/la.113c_revised.lik.v12_20K_2020-09-20.Rdata')
pp = mcmc@samples[1,mcmc@n.iter,]

msm.trates = names(pp)[grepl('msm.trate',names(pp))]
msm.trates.1 = names(pp)[grepl('msm.trate.1',names(pp))]
msm.trates.2 = names(pp)[grepl('msm.trate.2',names(pp))]
idu.trates = names(pp)[grepl('idu.trate',names(pp))]
idu.trates.2 = names(pp)[grepl('idu.trate.2',names(pp))]
het.trates = names(pp)[grepl('heterosexual.trate',names(pp))]
het.trates.2 = names(pp)[grepl('heterosexual.trate.2',names(pp))]
msm.idu.multipliers = names(pp)[grepl('msm.vs',names(pp))]
testing = names(pp)[grepl('proportion.tested', names(pp))]
suppression = names(pp)[grepl('suppressed', names(pp))]

#-- MSA specific --#

msa = PHILADELPHIA.MSA
pp2 = pp; pp2[testing] = 1; pp2[suppression] = 1
run.simulation = create.run.simulation.function(msa, start.values=pp)

#lik = create.msa.likelihood(msa)

sim1 = run.simulation(pp)

plot.calibration.risk(sim1)
plot.calibration.race.risk(sim1)


pp2 = pp; pp2[testing] = 1; pp2[suppression] = 1
pp2[msm.trates] = pp[msm.trates] * 0.92
#pp2[msm.trates.2] = pp[msm.trates.2] * 0.8
pp2[idu.trates] = pp[idu.trates] * 0.75
#pp2[idu.trates.2] = pp[idu.trates.2] * 0.5
pp2[msm.idu.multipliers] = pp[msm.idu.multipliers] * 0.4
pp2[het.trates] = pp[het.trates] * 1.7
pp2[het.trates.2] = pp[het.trates.2] * 0.8


sim2 = run.simulation(pp2)
plot.calibration.risk(list(sim1,sim2))
plot.calibration.total(list(sim1,sim2))
plot.calibration.race.risk(sim2)

starting.parameters = pp2
save(starting.parameters, file=file.path('mcmc_runs/start_values', paste0(msa, '.Rdata'))); msa.names(msa)


#to redo with testing parameters set to 1
for (msa in c(NYC.MSA, LA.MSA, MIAMI.MSA, ATLANTA.MSA, HOUSTON.MSA, DALLAS.MSA, CHICAGO.MSA, DC.MSA))
{
    load(file=file.path('mcmc_runs/start_values', paste0(msa, '.Rdata'))); msa.names(msa)
    starting.parameters[testing] = 1; starting.parameters[suppression] = 1
    save(starting.parameters, file=file.path('mcmc_runs/start_values', paste0(msa, '.Rdata'))); msa.names(msa)
}