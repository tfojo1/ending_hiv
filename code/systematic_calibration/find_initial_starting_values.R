


source('code/source_code.R')
source('code/targets/target_msas.R')
source('code/systematic_calibration/systematic_calibration.R')
load('mcmc_runs/systematic_initial/12060_1x20K_2020-10-01.Rdata')
load('mcmc_runs/systematic_initial/19100_1x20K_2020-10-01.Rdata')
#load('mcmc_runs/test_runs/la.113c_revised.lik.v12_20K_2020-09-20.Rdata')
pp = mcmc@samples[1,mcmc@n.iter,]

load('mcmc_runs/start_values/31080.Rdata')
load('mcmc_runs/start_values/35620.Rdata')
load('mcmc_runs/start_values/12060.Rdata')
load('mcmc_runs/start_values/26420.Rdata')
load('mcmc_runs/start_values/19820.Rdata')
load('mcmc_runs/start_values/29280.Rdata')
load('mcmc_runs/start_values/32820.Rdata')
load('mcmc_runs/start_values/41860.Rdata')
load('mcmc_runs/start_values/12420.Rdata')
load('mcmc_runs/start_values/41470.Rdata')
load('mcmc_runs/start_values/18140.Rdata')
pp = starting.parameters

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

set.pp.to.default <- function(pp)
{
    pp[testing] = pp[suppression] = 1
    pp['proportion.msm.of.male.mult']
    pp['testing.ramp.up.vs.current.rr'] = 0.5
    
    pp['oe.female.pairings.with.msm'] = 0.0895
    pp['age.mixing.sd.mult'] = 1
    pp['black.black.sexual.oe'] = 3.76
    pp['hispanic.hispanic.sexual.oe'] = 2.19
    pp['other.other.sexual.oe'] = 1.55

    pp['acute.transmissibility.rr'] = 12
    pp['diagnosed.transmission.rr'] = mean(c(1-.68, 1/3.5))
        
    pp    
}

#-- MSA specific --#

msa = CLEVELAND.MSA
pp2 = set.pp.to.default(pp)
run.simulation = create.run.simulation.function(msa, start.values=pp)

#lik = create.msa.likelihood(msa)

sim1 = run.simulation(pp)
sim2 = run.simulation(pp2)
plot.calibration.risk(list(sim1,sim2))
#plot.calibration.risk(list(sim1))#,sim2))
#plot.calibration.race.risk(sim1)


pp2 = set.pp.to.default(pp)
pp2[msm.trates] = pp[msm.trates] *  0.97 * 1.05
pp2[msm.trates.1] = pp[msm.trates.1] * 1.06 * 1.05
pp2[msm.trates.2] = pp[msm.trates.2] * 0.8 * 1.05
pp2[idu.trates] = pp[idu.trates] * 0.6
pp2[idu.trates.1] = pp[idu.trates.1] * .6
pp2[idu.trates.2] = pp[idu.trates.2] * 1.4
pp2[msm.idu.multipliers] = pp[msm.idu.multipliers] * .5
pp2['msm.vs.heterosexual.male.idu.susceptibility.rr.2'] = pp['msm.vs.heterosexual.male.idu.susceptibility.rr.2'] * .3
pp2[het.trates] = pp[het.trates] * 1.08
pp2[het.trates.1] = pp[het.trates.1] * 0.7
pp2[het.trates.2] = pp[het.trates.2] * 0.7


sim2 = run.simulation(pp2); plot.calibration.risk(list(sim2))

plot.calibration.risk(list(sim2), years=2000:2020)
plot.calibration.total(list(sim1,sim2))
plot.calibration.race.risk(sim2)

starting.parameters = pp2; msa.names(msa)
save(starting.parameters, file=file.path('mcmc_runs/start_values', paste0(msa, '.Rdata')))




#to redo parameters
msa = CHICAGO.MSA
load(file.path('mcmc_runs/start_values', paste0(msa, '.Rdata'))); msa.names(msa)
pp = starting.parameters
pp2 = pp; pp2[testing] = 1; pp2[suppression] = 1; pp2['msm.proportion.tested.or'] = 0.5
run.simulation = create.run.simulation.function(msa, start.values=pp)
sim1 = run.simulation(pp)

#to redo with testing parameters set to 1
for (msa in c(NYC.MSA, LA.MSA, MIAMI.MSA, ATLANTA.MSA, HOUSTON.MSA, DALLAS.MSA, CHICAGO.MSA, DC.MSA))
{
    load(file=file.path('mcmc_runs/start_values', paste0(msa, '.Rdata'))); msa.names(msa)
    starting.parameters[testing] = 1; starting.parameters[suppression] = 1
    save(starting.parameters, file=file.path('mcmc_runs/start_values', paste0(msa, '.Rdata'))); msa.names(msa)
}