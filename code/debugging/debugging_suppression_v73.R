
#Load up the mcmc
mcmc = assemble.mcmc.from.cache('mcmc_runs/la.73_2020-07-21/',T)
msa = attr(mcmc@simulations[[1]], 'location'); msa.names(msa)

#Set up the run sim function
base.components = setup.initial.components(msa=msa)
init.components = get.components.for.calibrated.parameters(mcmc@samples[1,1,], base.components)
init.components = fix.components.for.calibration(components = init.components)
run.simulation <- function(parameters)
{
    components = get.components.for.calibrated.parameters(parameters, init.components)
    run.jheem.from.components(components, max.run.time.seconds = Inf)
}

#Get sims for specific parameters
pp1 = mcmc@samples[1,500,]
sim1 = run.simulation(pp1)

pp2 = pp1
pp2['age1.suppressed.or'] = 1
pp2['heterosexual.suppressed.slope.or'] = .9
pp2['heterosexual.suppressed.or'] = .8
pp2['idu.suppressed.slope.or'] = .9
pp2['idu.suppressed.or'] = .6
sim2 = run.simulation(pp2)


pp2['age1.suppressed.slope.or'] = 2

pp2['msm.suppressed.slope.or'] = .9
pp2['msm.suppressed.or'] = .6


#Plot it
plot.calibration.total(list(sim1, sim2), data.types='suppression')
plot.calibration.race(list(sim1, sim2), data.types='suppression')
plot.calibration.risk(list(sim1, sim2), data.types='suppression')
plot.calibration.age(list(sim1, sim2), data.types='suppression')
plot.calibration.sex(list(sim1, sim2), data.types='suppression')

#Check Likelihoods (construct below)
exp(-full.likelihood(sim1, verbose-T) + full.likelihood(sim2, verbose=T))

exp(supp.lik(sim2)-supp.lik(sim1))
exp(supp.lik.check(sim2)-supp.lik.check(sim1))

exp(supp.lik.total(sim2)-supp.lik.total(sim1))
exp(supp.lik.age(sim2)-supp.lik.age(sim1))
exp(supp.lik.race(sim2)-supp.lik.race(sim1))
exp(supp.lik.sex(sim2)-supp.lik.sex(sim1))
exp(supp.lik.risk(sim2)-supp.lik.risk(sim1))
exp(supp.lik.race.risk(sim2)-supp.lik.race.risk(sim1))
exp(supp.lik.total.race.risk(sim2)-supp.lik.total.race.risk(sim1))
exp(supp.lik.total.age.race.risk(sim2)-supp.lik.total.age.race.risk(sim1))
exp(supp.lik.all(sim2)-supp.lik.all(sim1))

#Likelihood and components
full.likelihood = create.msa.likelihood(msa)
supp.lik = attr(full.likelihood, 'components')[['supp']]


#Split up suppression likelihood
POPULATION.TOTALS=get.census.totals(ALL.DATA.MANAGERS$census.totals, msa)

new.correlated.year.chunks=list(2008:2014, 2015:2018)
prevalence.correlated.year.chunks=list(2007:2013, 2014:2017)
mortality.correlated.year.chunks=list(2009:2013, 2014:2016)

year.to.year.chunk.correlation=0.65
year.to.year.off.correlation=0.25

EVERYTHING.WEIGHT = 1
NEW.WEIGHT = 4
PREV.WEIGHT = 4
MORT.WEIGHT = 1/4
CUM.MORT.WEIGHT = 1/4
IDU.WEIGHT = 64
AIDS.DX.WEIGHT = 1
TOTAL.DX.WEIGHT=1/16/8
STRATIFIED.DX.WEIGHT=1/128/8
SUPPRESSION.WEIGHT=1/16/8
prev.to.new.cv.ratio=1
FOCUS.WEIGHT=1

# The demographic groups to which we want to give added weight
to.focus = function(description){
    (grepl(' male',description) & (grepl('13-24', description) | grepl('25-34',description))) |
        (grepl('black, msm$',description) | grepl('hispanic, msm$', description))
}


SD.INFLATION.NEW.NUM = 1/sqrt(NEW.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
SD.INFLATION.NEW = function(description){SD.INFLATION.NEW.NUM /
        (sqrt(FOCUS.WEIGHT)^as.numeric(to.focus(description)))}
NEW.SD = function(years, num){sqrt(0.5 * (0.065*num)^2 + 0.5 * (num^0.33)^2)}


SUPPRESSED.SD = function(...){.01}
SUPPRESSION.SD.INFLATION = 1/sqrt(SUPPRESSION.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
SUPPRESSED.STATE.SD.INFLATION = 3

suppression.years = 2010:2018


supp.lik.total = create.suppressed.likelihood(location=msa,
                                              years=suppression.years,
                                              surv=msa.surveillance,
                                              numerator.year.to.year.chunk.correlation=0.5,
                                              numerator.chunk.years=list(suppression.years),
                                              numerator.sd = SUPPRESSED.SD,
                                              sd.inflation=SUPPRESSION.SD.INFLATION,
                                              numerator.sd.inflation.if.backup=SUPPRESSED.STATE.SD.INFLATION,
                                              inflate.sd.by.n.obs.per.year = F,
                                              by.total=T,
                                              by.age=F,
                                              by.sex=F,
                                              by.race=F,
                                              by.risk=F)

supp.lik.age = create.suppressed.likelihood(location=msa,
                                              years=suppression.years,
                                              surv=msa.surveillance,
                                              numerator.year.to.year.chunk.correlation=0.5,
                                              numerator.chunk.years=list(suppression.years),
                                              numerator.sd = SUPPRESSED.SD,
                                              sd.inflation=SUPPRESSION.SD.INFLATION,
                                              numerator.sd.inflation.if.backup=SUPPRESSED.STATE.SD.INFLATION,
                                            inflate.sd.by.n.obs.per.year = F,
                                              by.total=F,
                                              by.age=T,
                                              by.sex=F,
                                              by.race=F,
                                              by.risk=F)

supp.lik.race = create.suppressed.likelihood(location=msa,
                                              years=suppression.years,
                                              surv=msa.surveillance,
                                              numerator.year.to.year.chunk.correlation=0.5,
                                              numerator.chunk.years=list(suppression.years),
                                              numerator.sd = SUPPRESSED.SD,
                                              sd.inflation=SUPPRESSION.SD.INFLATION,
                                              numerator.sd.inflation.if.backup=SUPPRESSED.STATE.SD.INFLATION,
                                             inflate.sd.by.n.obs.per.year = F,
                                              by.total=F,
                                              by.age=F,
                                              by.sex=F,
                                              by.race=T,
                                              by.risk=F)

supp.lik.sex = create.suppressed.likelihood(location=msa,
                                              years=suppression.years,
                                              surv=msa.surveillance,
                                              numerator.year.to.year.chunk.correlation=0.5,
                                              numerator.chunk.years=list(suppression.years),
                                              numerator.sd = SUPPRESSED.SD,
                                              sd.inflation=SUPPRESSION.SD.INFLATION,
                                              numerator.sd.inflation.if.backup=SUPPRESSED.STATE.SD.INFLATION,
                                            inflate.sd.by.n.obs.per.year = F,
                                              by.total=F,
                                              by.age=F,
                                              by.sex=T,
                                              by.race=F,
                                              by.risk=F)

supp.lik.risk = create.suppressed.likelihood(location=msa,
                                              years=suppression.years,
                                              surv=msa.surveillance,
                                              numerator.year.to.year.chunk.correlation=0.5,
                                              numerator.chunk.years=list(suppression.years),
                                              numerator.sd = SUPPRESSED.SD,
                                              sd.inflation=SUPPRESSION.SD.INFLATION,
                                              numerator.sd.inflation.if.backup=SUPPRESSED.STATE.SD.INFLATION,
                                             inflate.sd.by.n.obs.per.year = F,
                                              by.total=F,
                                              by.age=F,
                                              by.sex=F,
                                              by.race=F,
                                              by.risk=T)

supp.lik.race.risk = create.suppressed.likelihood(location=msa,
                                             years=suppression.years,
                                             surv=msa.surveillance,
                                             numerator.year.to.year.chunk.correlation=0.5,
                                             numerator.chunk.years=list(suppression.years),
                                             numerator.sd = SUPPRESSED.SD,
                                             sd.inflation=SUPPRESSION.SD.INFLATION,
                                             numerator.sd.inflation.if.backup=SUPPRESSED.STATE.SD.INFLATION,
                                             inflate.sd.by.n.obs.per.year = F,
                                             by.total=F,
                                             by.age=F,
                                             by.sex=F,
                                             by.race=T,
                                             by.risk=T)

supp.lik.total.race.risk = create.suppressed.likelihood(location=msa,
                                                  years=suppression.years,
                                                  surv=msa.surveillance,
                                                  numerator.year.to.year.chunk.correlation=0.5,
                                                  numerator.chunk.years=list(suppression.years),
                                                  numerator.sd = SUPPRESSED.SD,
                                                  sd.inflation=SUPPRESSION.SD.INFLATION,
                                                  numerator.sd.inflation.if.backup=SUPPRESSED.STATE.SD.INFLATION,
                                                  inflate.sd.by.n.obs.per.year = F,
                                                  by.total=T,
                                                  by.age=F,
                                                  by.sex=F,
                                                  by.race=T,
                                                  by.risk=T)

supp.lik.total.age.race.risk = create.suppressed.likelihood(location=msa,
                                                        years=suppression.years,
                                                        surv=msa.surveillance,
                                                        numerator.year.to.year.chunk.correlation=0.5,
                                                        numerator.chunk.years=list(suppression.years),
                                                        numerator.sd = SUPPRESSED.SD,
                                                        sd.inflation=SUPPRESSION.SD.INFLATION,
                                                        numerator.sd.inflation.if.backup=SUPPRESSED.STATE.SD.INFLATION,
                                                        inflate.sd.by.n.obs.per.year = F,
                                                        by.total=T,
                                                        by.age=T,
                                                        by.sex=F,
                                                        by.race=T,
                                                        by.risk=T)


supp.lik.all = create.suppressed.likelihood(location=msa,
                                            years=suppression.years,
                                            surv=msa.surveillance,
                                            numerator.year.to.year.chunk.correlation=0.5,
                                            numerator.chunk.years=list(suppression.years),
                                            numerator.sd = SUPPRESSED.SD,
                                            sd.inflation=SUPPRESSION.SD.INFLATION,
                                            numerator.sd.inflation.if.backup=SUPPRESSED.STATE.SD.INFLATION,
                                            inflate.sd.by.n.obs.per.year = F,
                                            by.total=T,
                                            by.age=T,
                                            by.sex=T,
                                            by.race=T,
                                            by.risk=T)

supp.lik.check = supp.lik.all = create.suppressed.likelihood(location=msa,
                                                             years=suppression.years,
                                                             surv=msa.surveillance,
                                                             numerator.year.to.year.chunk.correlation=0.5,
                                                             numerator.chunk.years=list(suppression.years),
                                                             numerator.sd = SUPPRESSED.SD,
                                                             sd.inflation=SUPPRESSION.SD.INFLATION,
                                                             numerator.sd.inflation.if.backup=SUPPRESSED.STATE.SD.INFLATION,
                                                             inflate.sd.by.n.obs.per.year = F)
