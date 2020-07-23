
if (1==2) #the constructors for these likelihoods are below
{
    verbose.likelihood(sim1);verbose.likelihood(sim2)
    
    exp(new.lik.sex.age(sim2) - new.lik.sex.age(sim1))
    exp(new.lik.race.risk(sim2) - new.lik.race.risk(sim1))
    exp(new.lik.sex.risk(sim2) - new.lik.sex.risk(sim1))
    exp(new.lik.sex.race(sim2) - new.lik.sex.race(sim1))
    
    exp(new.lik(sim2) - new.lik(sim1))
    
    plot.calibration.sex.age(list(sim1,sim2))
    plot.calibration.risk.race(list(sim1,sim2))
    plot.calibration.sex.race(list(sim1,sim2))
}


load('mcmc_runs/balt.69_revised2_wt.6.6.33_20000_2020-07-14.Rdata')

pp1 = mcmc@samples[1,800,]
sim1 = run.simulation(pp1)

# this gets black msm higher
pp2 = pp1
pp2['black.msm.trate.1'] = 2.5
sim2 = run.simulation(pp2)

# this gets the decreasing trend for other msm
pp2 = pp1
pp2['other.msm.trate.1'] = 1
pp2['other.msm.trate.2'] = 0.8
sim2 = run.simulation(pp2)

plot.calibration.risk(list(sim1, sim2))
plot.calibration.race.risk(list(sim1, sim2))
plot.calibration.sex.age(list(sim1, sim2))
plot.calibration.risk.race(list(sim1,sim2))

verbose.likelihood = create.msa.likelihood('12580', verbose=T)

verbose.likelihood(sim1); verbose.likelihood(sim2)

#-- The prior --#
parameters.prior.1 = parameters.prior

calculate.density(parameters.prior.1, rbind(pp1,pp2))
calculate.density(parameters.prior.1, pp2) / calculate.density(parameters.prior.1, pp1)


#-- Split the new likelihood for each stratification --#
msa='12580'
POPULATION.TOTALS=get.census.totals(ALL.DATA.MANAGERS$census.totals, msa)

new.correlated.year.chunks=list(2008:2014, 2015:2018)
prevalence.correlated.year.chunks=list(2007:2013, 2014:2017)
mortality.correlated.year.chunks=list(2009:2013, 2014:2016)

year.to.year.chunk.correlation=0.65
year.to.year.off.correlation=0.25

EVERYTHING.WEIGHT = 1
NEW.WEIGHT = 6
PREV.WEIGHT = 6
MORT.WEIGHT = 1/3
CUM.MORT.WEIGHT = 1/4
IDU.WEIGHT = 64
AIDS.DX.WEIGHT = 1
TOTAL.DX.WEIGHT=1/16/8
STRATIFIED.DX.WEIGHT=1/128/8
SUPPRESSION.WEIGHT=1/16/8
prev.to.new.cv.ratio=1
FOCUS.WEIGHT=4

# The demographic groups to which we want to give added weight
to.focus = function(description){
    (grepl(' male',description) & (grepl('13-24', description) | grepl('25-34',description))) |
        (grepl('black, msm$',description) | grepl('hispanic, msm$', description))
}


SD.INFLATION.NEW.NUM = 1/sqrt(NEW.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
SD.INFLATION.NEW = function(description){SD.INFLATION.NEW.NUM /
        (sqrt(FOCUS.WEIGHT)^as.numeric(to.focus(description)))}
NEW.SD = function(years, num){sqrt(0.5 * (0.065*num)^2 + 0.5 * (num^0.33)^2)}

new.lik = create.likelihood.function(data.type='new',
                                     years = sort(unlist(new.correlated.year.chunks)),
                                     surv=msa.surveillance,
                                     location=msa,
                                     by.total = T,
                                     by.sex.age=T,
                                     by.sex.race=T,
                                     by.sex.risk=T,
                                     by.race.risk=T,
                                     population=POPULATION.TOTALS,
                                     denominator.dimensions='year',
                                     msm.cv=0,
                                     idu.cv=0,
                                     sd.inflation=SD.INFLATION.NEW,
                                     year.to.year.correlation = 0,
                                     numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                     numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                     numerator.chunk.years=new.correlated.year.chunks,
                                     numerator.sd = NEW.SD)
#to check that we set all our parameters the same
new.lik(sim)

new.lik.sex.age = create.likelihood.function(data.type='new',
                                             years = sort(unlist(new.correlated.year.chunks)),
                                             surv=msa.surveillance,
                                             location=msa,
                                             by.sex.age=T,
                                             population=POPULATION.TOTALS,
                                             denominator.dimensions='year',
                                             msm.cv=0,
                                             idu.cv=0,
                                             sd.inflation=SD.INFLATION.NEW,
                                             year.to.year.correlation = 0,
                                             numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                             numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                             numerator.chunk.years=new.correlated.year.chunks,
                                             numerator.sd = NEW.SD)
plot.calibration.sex.age(list(sim1, sim2))
new.lik.sex.age(sim1);new.lik.sex.age(sim2)


new.lik.race.risk = create.likelihood.function(data.type='new',
                                             years = sort(unlist(new.correlated.year.chunks)),
                                             surv=msa.surveillance,
                                             location=msa,
                                             by.race.risk=T,
                                             population=POPULATION.TOTALS,
                                             denominator.dimensions='year',
                                             msm.cv=0,
                                             idu.cv=0,
                                             sd.inflation=SD.INFLATION.NEW,
                                             year.to.year.correlation = 0,
                                             numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                             numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                             numerator.chunk.years=new.correlated.year.chunks,
                                             numerator.sd = NEW.SD)
new.lik.race.risk(sim1);new.lik.race.risk(sim2)
plot.calibration.risk.race(list(sim1,sim2))

new.lik.sex.risk = create.likelihood.function(data.type='new',
                                               years = sort(unlist(new.correlated.year.chunks)),
                                               surv=msa.surveillance,
                                               location=msa,
                                               by.sex.risk=T,
                                               population=POPULATION.TOTALS,
                                               denominator.dimensions='year',
                                               msm.cv=0,
                                               idu.cv=0,
                                               sd.inflation=SD.INFLATION.NEW,
                                               year.to.year.correlation = 0,
                                               numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                               numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                               numerator.chunk.years=new.correlated.year.chunks,
                                               numerator.sd = NEW.SD)
new.lik.sex.risk(sim1);new.lik.sex.risk(sim2)
plot.calibration.sex.risk(list(sim1, sim2))

new.lik.sex.race = create.likelihood.function(data.type='new',
                                              years = sort(unlist(new.correlated.year.chunks)),
                                              surv=msa.surveillance,
                                              location=msa,
                                              by.sex.race=T,
                                              population=POPULATION.TOTALS,
                                              denominator.dimensions='year',
                                              msm.cv=0,
                                              idu.cv=0,
                                              sd.inflation=SD.INFLATION.NEW,
                                              year.to.year.correlation = 0,
                                              numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                              numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                              numerator.chunk.years=new.correlated.year.chunks,
                                              numerator.sd = NEW.SD)
new.lik.sex.race(sim1);new.lik.sex.race(sim2)
plot.calibration.sex.race(list(sim1,sim2))
