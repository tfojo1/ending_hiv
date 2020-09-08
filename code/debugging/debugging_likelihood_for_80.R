

plot.calibration.race.risk(list(sim1, sim2))
plot.calibration.sex.age(list(sim1, sim2))
plot.calibration.sex.race(list(sim1, sim2))
plot.calibration.sex.risk(list(sim1,sim2))


plot.calibration.risk.race(list(sim1,sim2), risk='IDU')


plot.calibration(list(sim1, sim2), facet.by=list('risk','race'), split.by=NULL, data.types='new')


-lik(sim1, verbose=T)+lik(sim2, verbose=T)

-new.lik(sim1)+new.lik(sim2)
-new.lik.sex.age(sim1)+new.lik.sex.age(sim2)
-new.lik.sex.race(sim1)+new.lik.sex.race(sim2)
-new.lik.sex.risk(sim1)+new.lik.sex.risk(sim2)
-new.lik.race.risk(sim1)+new.lik.race.risk(sim2)

-prev.lik(sim1)+prev.lik(sim2)
-prev.lik.sex.age(sim1)+prev.lik.sex.age(sim2)
-prev.lik.sex.race(sim1)+prev.lik.sex.race(sim2)
-prev.lik.sex.risk(sim1)+prev.lik.sex.risk(sim2)
-prev.lik.race.risk(sim1)+prev.lik.race.risk(sim2)

-supp.lik(sim1) + supp.lik(sim2)

plot.calibration.sex.race(list(sim1,sim2))
plot.calibration.sex.risk(list(sim1,sim2), risk=c('msm','idu','msm_idu','heterosexual'), ncol=4)
plot.calibration.race.risk(list(sim1,sim2), risk=c('msm','idu','msm_idu','heterosexual'), ncol=4)
plot.calibration.sex.age(list(sim1,sim2))


plot.calibration.total(list(sim1,sim2), data.types='suppression')
plot.calibration.race(list(sim1,sim2), data.types='suppression')
plot.calibration.risk(list(sim1,sim2), data.types='suppression')
plot.calibration.age(list(sim1,sim2), data.types='suppression')
plot.calibration.sex(list(sim1,sim2), data.types='suppression')

#changing for supp
pp2 = pp1
pp2['age1.suppressed.or']=.8
pp2['hispanic.suppressed.or']=0.6
pp2['hispanic.suppressed.slope.or']=.9

sim2 = run.simulation(pp2)
plot.calibration.race(list(sim1,sim2), data.types='suppression')

-supp.lik(sim1) + supp.lik(sim2)

#-- The Likelihood --#

supp.lik = attr(lik, 'components')[['supp']]

EVERYTHING.WEIGHT=1/8

NEW.WEIGHT = 1
PREV.WEIGHT = 1
MORT.WEIGHT = 1
CUM.MORT.WEIGHT = 1
IDU.WEIGHT = 64*8
AIDS.DX.WEIGHT = 4
TOTAL.DX.WEIGHT=1/8
STRATIFIED.DX.WEIGHT=1/128/4
SUPPRESSION.WEIGHT=1/8

prev.to.new.cv.ratio=
    FOCUS.WEIGHT=1

new.correlated.year.chunks=list(2008:2014, 2015:2018)
prevalence.correlated.year.chunks=list(2007:2013, 2014:2017)
mortality.correlated.year.chunks=list(2009:2013, 2014:2016)
idu.years=2014:2016
diagnosed.years = 2010:2018
suppression.years = 2010:2018

year.to.year.chunk.correlation=0.65
year.to.year.off.correlation=0.25

to.focus = function(description){
    (grepl(' male',description) & (grepl('13-24', description) | grepl('25-34',description))) |
        (grepl('black, msm$',description) | grepl('hispanic, msm$', description))
}

POPULATION.TOTALS=get.census.totals(ALL.DATA.MANAGERS$census.totals, msa)
#-- Elements for New Diagnoses --#
SD.INFLATION.NEW.NUM = 1/sqrt(NEW.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
SD.INFLATION.NEW = function(description){SD.INFLATION.NEW.NUM /
        (sqrt(FOCUS.WEIGHT)^as.numeric(to.focus(description)))}

NEW.SD = function(years, num){sqrt(0.5 * (0.065*num)^2 + 0.5 * (num^0.33)^2)}


#-- Elements for Prevalence --#
PREV.INFLATION = prev.to.new.cv.ratio * get.cv.weights(location=msa, weight.to='new')['prevalence']

SD.INFLATION.PREV.NUM = 1/sqrt(PREV.WEIGHT)/sqrt(EVERYTHING.WEIGHT)*PREV.INFLATION
SD.INFLATION.PREV = function(description){SD.INFLATION.PREV.NUM /
        (sqrt(FOCUS.WEIGHT)^as.numeric(to.focus(description)))}

PREV.SD = function(years, num){sqrt(0.5 * (0.09*num)^2 + 0.5 * (num^0.69)^2)}


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
new.lik.sex.age = create.likelihood.function(data.type='new',
                                     years = sort(unlist(new.correlated.year.chunks)),
                                     surv=msa.surveillance,
                                     location=msa,
                                     by.total = F,
                                     by.sex.age=T,
                                     by.sex.race=F,
                                     by.sex.risk=F,
                                     by.race.risk=F,
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
new.lik.sex.race = create.likelihood.function(data.type='new',
                                     years = sort(unlist(new.correlated.year.chunks)),
                                     surv=msa.surveillance,
                                     location=msa,
                                     by.total = F,
                                     by.sex.age=F,
                                     by.sex.race=T,
                                     by.sex.risk=F,
                                     by.race.risk=F,
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
new.lik.sex.risk = create.likelihood.function(data.type='new',
                                     years = sort(unlist(new.correlated.year.chunks)),
                                     surv=msa.surveillance,
                                     location=msa,
                                     by.total = F,
                                     by.sex.age=F,
                                     by.sex.race=F,
                                     by.sex.risk=T,
                                     by.race.risk=F,
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
new.lik.race.risk = create.likelihood.function(data.type='new',
                                     years = sort(unlist(new.correlated.year.chunks)),
                                     surv=msa.surveillance,
                                     location=msa,
                                     by.total = F,
                                     by.sex.age=F,
                                     by.sex.race=F,
                                     by.sex.risk=F,
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

prev.lik = create.likelihood.function(data.type='prevalence',
                                      years = sort(unlist(prevalence.correlated.year.chunks)),
                                      surv=msa.surveillance,
                                      location=msa,
                                      by.total = T,
                                      #by.sex.age=T,
                                      #by.sex.race=T,
                                      #by.sex.risk=T,
                                      #by.race.risk=T,
                                      by.sex=T,
                                      by.risk=T,
                                      by.race=T,
                                      by.age=T,
                                      population=POPULATION.TOTALS,
                                      denominator.dimensions='year',
                                      msm.cv=0,
                                      idu.cv=0,
                                      sd.inflation=SD.INFLATION.PREV,
                                      year.to.year.correlation = 0,
                                      numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                      numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                      numerator.chunk.years=prevalence.correlated.year.chunks,
                                      numerator.sd = PREV.SD)

prev.lik = create.likelihood.function(data.type='prevalence',
                                      years = sort(unlist(prevalence.correlated.year.chunks)),
                                      surv=msa.surveillance,
                                      location=msa,
                                      by.total = T,
                                      by.sex.age=T,
                                      by.sex.race=T,
                                      by.sex.risk=T,
                                      by.race.risk=T,
                                      #by.sex=T,
                                      #by.risk=T,
                                      #by.race=T,
                                      #by.age=T,
                                      population=POPULATION.TOTALS,
                                      denominator.dimensions='year',
                                      msm.cv=0,
                                      idu.cv=0,
                                      sd.inflation=SD.INFLATION.PREV,
                                      year.to.year.correlation = 0,
                                      numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                      numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                      numerator.chunk.years=prevalence.correlated.year.chunks,
                                      numerator.sd = PREV.SD)

prev.lik.sex.age = create.likelihood.function(data.type='prevalence',
                                              years = sort(unlist(prevalence.correlated.year.chunks)),
                                              surv=msa.surveillance,
                                              location=msa,
                                              by.total = F,
                                              by.sex.age=T,
                                              by.sex.race=F,
                                              by.sex.risk=F,
                                              by.race.risk=F,
                                              #by.sex=T,
                                              #by.risk=T,
                                              #by.race=T,
                                              #by.age=T,
                                              population=POPULATION.TOTALS,
                                              denominator.dimensions='year',
                                              msm.cv=0,
                                              idu.cv=0,
                                              sd.inflation=SD.INFLATION.PREV,
                                              year.to.year.correlation = 0,
                                              numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                              numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                              numerator.chunk.years=prevalence.correlated.year.chunks,
                                              numerator.sd = PREV.SD)

prev.lik.race.risk = create.likelihood.function(data.type='prevalence',
                                                years = sort(unlist(prevalence.correlated.year.chunks)),
                                                surv=msa.surveillance,
                                                location=msa,
                                                by.total = F,
                                                by.sex.age=F,
                                                by.sex.race=F,
                                                by.sex.risk=F,
                                                by.race.risk=T,
                                                #by.sex=T,
                                                #by.risk=T,
                                                #by.race=T,
                                                #by.age=T,
                                                population=POPULATION.TOTALS,
                                                denominator.dimensions='year',
                                                msm.cv=0,
                                                idu.cv=0,
                                                sd.inflation=SD.INFLATION.PREV,
                                                year.to.year.correlation = 0,
                                                numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                                numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                                numerator.chunk.years=prevalence.correlated.year.chunks,
                                                numerator.sd = PREV.SD)

prev.lik.sex.race = create.likelihood.function(data.type='prevalence',
                                               years = sort(unlist(prevalence.correlated.year.chunks)),
                                               surv=msa.surveillance,
                                               location=msa,
                                               by.total = F,
                                               by.sex.age=F,
                                               by.sex.race=T,
                                               by.sex.risk=F,
                                               by.race.risk=F,
                                               #by.sex=T,
                                               #by.risk=T,
                                               #by.race=T,
                                               #by.age=T,
                                               population=POPULATION.TOTALS,
                                               denominator.dimensions='year',
                                               msm.cv=0,
                                               idu.cv=0,
                                               sd.inflation=SD.INFLATION.PREV,
                                               year.to.year.correlation = 0,
                                               numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                               numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                               numerator.chunk.years=prevalence.correlated.year.chunks,
                                               numerator.sd = PREV.SD)

prev.lik.sex.risk = create.likelihood.function(data.type='prevalence',
                                               years = sort(unlist(prevalence.correlated.year.chunks)),
                                               surv=msa.surveillance,
                                               location=msa,
                                               by.total = F,
                                               by.sex.age=F,
                                               by.sex.race=F,
                                               by.sex.risk=T,
                                               by.race.risk=F,
                                               #by.sex=T,
                                               #by.risk=T,
                                               #by.race=T,
                                               #by.age=T,
                                               population=POPULATION.TOTALS,
                                               denominator.dimensions='year',
                                               msm.cv=0,
                                               idu.cv=0,
                                               sd.inflation=SD.INFLATION.PREV,
                                               year.to.year.correlation = 0,
                                               numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                               numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                               numerator.chunk.years=prevalence.correlated.year.chunks,
                                               numerator.sd = PREV.SD)

prev.lik.total = create.likelihood.function(data.type='prevalence',
                                            years = sort(unlist(prevalence.correlated.year.chunks)),
                                            surv=msa.surveillance,
                                            location=msa,
                                            by.total = T,
                                            by.sex.age=F,
                                            by.sex.race=F,
                                            by.sex.risk=F,
                                            by.race.risk=F,
                                            #by.sex=T,
                                            #by.risk=T,
                                            #by.race=T,
                                            #by.age=T,
                                            population=POPULATION.TOTALS,
                                            denominator.dimensions='year',
                                            msm.cv=0,
                                            idu.cv=0,
                                            sd.inflation=SD.INFLATION.PREV,
                                            year.to.year.correlation = 0,
                                            numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                            numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                            numerator.chunk.years=prevalence.correlated.year.chunks,
                                            numerator.sd = PREV.SD)

prev.lik.sex.race.sex.risk = create.likelihood.function(data.type='prevalence',
                                                        years = sort(unlist(prevalence.correlated.year.chunks)),
                                                        surv=msa.surveillance,
                                                        location=msa,
                                                        by.total = F,
                                                        by.sex.age=F,
                                                        by.sex.race=T,
                                                        by.sex.risk=T,
                                                        by.race.risk=F,
                                                        #by.sex=T,
                                                        #by.risk=T,
                                                        #by.race=T,
                                                        #by.age=T,
                                                        population=POPULATION.TOTALS,
                                                        denominator.dimensions='year',
                                                        msm.cv=0,
                                                        idu.cv=0,
                                                        sd.inflation=SD.INFLATION.PREV,
                                                        year.to.year.correlation = 0,
                                                        numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                                        numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                                        numerator.chunk.years=prevalence.correlated.year.chunks,
                                                        numerator.sd = PREV.SD)

prev.lik.sex.age.race.risk = create.likelihood.function(data.type='prevalence',
                                                        years = sort(unlist(prevalence.correlated.year.chunks)),
                                                        surv=msa.surveillance,
                                                        location=msa,
                                                        by.total = F,
                                                        by.sex.age=T,
                                                        by.sex.race=F,
                                                        by.sex.risk=F,
                                                        by.race.risk=T,
                                                        #by.sex=T,
                                                        #by.risk=T,
                                                        #by.race=T,
                                                        #by.age=T,
                                                        population=POPULATION.TOTALS,
                                                        denominator.dimensions='year',
                                                        msm.cv=0,
                                                        idu.cv=0,
                                                        sd.inflation=SD.INFLATION.PREV,
                                                        year.to.year.correlation = 0,
                                                        numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                                        numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                                        numerator.chunk.years=prevalence.correlated.year.chunks,
                                                        numerator.sd = PREV.SD)
