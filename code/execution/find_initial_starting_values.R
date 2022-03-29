


source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')


#load('mcmc_runs/systematic_initial/12060_1x20K_2020-10-01.Rdata')
#load('mcmc_runs/systematic_initial/19100_1x20K_2020-10-01.Rdata')
#load('mcmc_runs/test_runs/la.113c_revised.lik.v12_20K_2020-09-20.Rdata')
#pp = mcmc@samples[1,mcmc@n.iter,]

prior = get.parameters.prior.for.version(VERSION.MANAGER, 'collapsed_1.0')
prep.pp = function(pp)
{
    prior.medians = suppressWarnings(get.medians(prior))
    matching = intersect(names(pp), prior@var.names)
    prior.medians[matching] = pp[matching]
    prior.medians
}

load('mcmc_runs/start_values/collapsed_1.0/31080.Rdata'); pp = prep.pp(starting.parameters)
load('mcmc_runs/start_values/collapsed_1.0/35620.Rdata'); pp = prep.pp(starting.parameters)
load('mcmc_runs/start_values/collapsed_1.0/12060.Rdata'); pp = prep.pp(starting.parameters)
load('mcmc_runs/start_values/collapsed_1.0/26420.Rdata'); pp = prep.pp(starting.parameters)
load('mcmc_runs/start_values/collapsed_1.0/19820.Rdata'); pp = prep.pp(starting.parameters)
load('mcmc_runs/start_values/collapsed_1.0/29280.Rdata'); pp = prep.pp(starting.parameters)
load('mcmc_runs/start_values/collapsed_1.0/32820.Rdata'); pp = prep.pp(starting.parameters)
load('mcmc_runs/start_values/collapsed_1.0/41860.Rdata'); pp = prep.pp(starting.parameters)
load('mcmc_runs/start_values/collapsed_1.0/12420.Rdata'); pp = prep.pp(starting.parameters)
load('mcmc_runs/start_values/collapsed_1.0/41470.Rdata'); pp = prep.pp(starting.parameters)
load('mcmc_runs/start_values/collapsed_1.0/18140.Rdata'); pp = prep.pp(starting.parameters)
load('mcmc_runs/start_values/collapsed_1.0/12580.Rdata'); pp = prep.pp(starting.parameters)
load('mcmc_runs/start_values/collapsed_1.0/33100.Rdata'); pp = prep.pp(starting.parameters)
load('mcmc_runs/start_values/collapsed_1.0/19740.Rdata'); pp = prep.pp(starting.parameters)


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
    pp['proportion.msm.of.male.mult'] = 1
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

msa = DENVER.MSA
run.simulation = create.run.simulation.function(msa, start.values=pp)

pp2 = set.pp.to.default(pp)

#lik = create.msa.likelihood(msa)

sim1 = run.simulation(pp)
sim2 = run.simulation(pp2)
simplot(sim1,sim2, facet.by='risk')
#plot.calibration.risk(list(sim1))#,sim2))
#plot.calibration.race.risk(sim1)

sim2 = run.simulation(pp2)
simplot(sim2, facet.by='risk')

pp2 = set.pp.to.default(pp)
pp2[msm.trates] = pp[msm.trates] *  .9
pp2[msm.trates.1] = pp[msm.trates.1] * 1.5
pp2[msm.trates.2] = pp[msm.trates.2] * 1

pp2[idu.trates] = pp[idu.trates] * .75
pp2[idu.trates.1] = pp[idu.trates.1] * 1.4
pp2[idu.trates.2] = pp[idu.trates.2] * 1

pp2[het.trates] = pp[het.trates] * 1.6
pp2[het.trates.1] = pp[het.trates.1] * .3
pp2[het.trates.2] = pp[het.trates.2] * 1

pp2[msm.idu.multipliers] = pp[msm.idu.multipliers] * 1
pp2['msm.vs.heterosexual.male.idu.susceptibility.rr.2'] = pp['msm.vs.heterosexual.male.idu.susceptibility.rr.2'] * .5

pp2['msm.incident.idu.multiplier.0'] = pp['msm.incident.idu.multiplier.0'] * 4
pp2['msm.incident.idu.multiplier.2'] = pp['msm.incident.idu.multiplier.2'] * 4

sim2 = run.simulation(pp2)
simplot

simplot(sim2, data.types='aids.diagnoses', years= 1990:2010)


simplot(list(sim2), facet.by='risk', years=2000:2020)
simplot(list(sim1,sim2))
simplot(sim2, facet.by='race')
simplot(sim2, facet.by='risk')

starting.parameters = pp2; msa.names(msa)
# save to both local and systematic directories
save(starting.parameters, file=file.path(SYSTEMATIC_ROOT_DIR, 'start_values/collapsed_1.0', paste0(msa, '.Rdata')));  save(starting.parameters, file=file.path('mcmc_runs/start_values/collapsed_1.0', paste0(msa, '.Rdata')))




D#to redo parameters
msa = CHICAGO.MSA
load(file.path('mcmc_runs/start_values/collapsed_1.0', paste0(msa, '.Rdata'))); msa.names(msa)
pp = prep.pp(starting.parameters)
pp2 = pp; pp2[testing] = 1; pp2[suppression] = 1; pp2['msm.proportion.tested.or'] = 0.5
run.simulation = create.run.simulation.function(msa, start.values=pp)
sim1 = run.simulation(pp)

#to redo with testing parameters set to 1
for (msa in c(NYC.MSA, LA.MSA, MIAMI.MSA, ATLANTA.MSA, HOUSTON.MSA, DALLAS.MSA, CHICAGO.MSA, DC.MSA))
{
    load(file=file.path('mcmc_runs/start_values/collapsed_1.0/', paste0(msa, '.Rdata'))); msa.names(msa)
    starting.parameters[testing] = 1; starting.parameters[suppression] = 1
    save(starting.parameters, file=file.path('mcmc_runs/start_values/collapsed_1.0', paste0(msa, '.Rdata'))); msa.names(msa)
}



# Why is likelihood -Inf
likelihood = create.msa.likelihood(msa)
likelihood(sim2)

lik.comps = attr(likelihood, 'components')
sapply(lik.comps, function(ll){ll(sim2)})

