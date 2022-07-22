#113 - like 108 but with peak multiplier 
library(distributions)

MSM.BASE.TRATE.MEAN = 1
HET.BASE.TRATE.MEAN = 1
IDU.BASE.TRATE.MEAN = 1
BASE.TRATE.CV = 20

TRATE.RR.1.2.SPAN = 2#1.5
TRATE.RR.0.1.SPAN = 4#2#1.5
TRATE.RR.0.PEAK.SPAN = 8#3



EXPANDED.CONTINUUM.PARAMETERS.PRIOR = join.distributions(
    
    global.trate = Loguniform.Distribution(0,Inf),
    
    #-- MSM Transmission --#
    black.msm.transmission = create.transmission.prior.distribution(r1.log.mean=log(MSM.BASE.TRATE.MEAN),
                                                                    r1.log.sd=log(BASE.TRATE.CV),
                                                                    rr.2.to.1.log.sd=0.5*log(TRATE.RR.1.2.SPAN),
                                                                    rr.0.to.1.log.sd=0.5*log(TRATE.RR.0.1.SPAN),
                                                                    race='black',
                                                                    route='msm'),
    
    hispanic.msm.transmission = create.transmission.prior.distribution(r1.log.mean=log(MSM.BASE.TRATE.MEAN),
                                                                       r1.log.sd=log(BASE.TRATE.CV),
                                                                       rr.2.to.1.log.sd=0.5*log(TRATE.RR.1.2.SPAN),
                                                                       rr.0.to.1.log.sd=0.5*log(TRATE.RR.0.1.SPAN),
                                                                       race='hispanic',
                                                                       route='msm'),
    
    other.msm.transmission = create.transmission.prior.distribution(r1.log.mean=log(MSM.BASE.TRATE.MEAN),
                                                                    r1.log.sd=log(BASE.TRATE.CV),
                                                                    rr.2.to.1.log.sd=0.5*log(TRATE.RR.1.2.SPAN),
                                                                    rr.0.to.1.log.sd=0.5*log(TRATE.RR.0.1.SPAN),
                                                                    race='other',
                                                                    route='msm'),
    
    msm.peak.trate.multiplier =  Lognormal.Distribution(log(3.1), 0.5*log(TRATE.RR.0.PEAK.SPAN)),
    
    
    #-- Heterosexual Transmission --#
    black.heterosexual.transmission = create.transmission.prior.distribution(r1.log.mean=log(HET.BASE.TRATE.MEAN),
                                                                             r1.log.sd=log(BASE.TRATE.CV),
                                                                             rr.2.to.1.log.sd=0.5*log(TRATE.RR.1.2.SPAN),
                                                                             rr.0.to.1.log.sd=0.5*log(TRATE.RR.0.1.SPAN),
                                                                             race='black',
                                                                             route='heterosexual'),
    
    hispanic.heterosexual.transmission = create.transmission.prior.distribution(r1.log.mean=log(HET.BASE.TRATE.MEAN),
                                                                                r1.log.sd=log(BASE.TRATE.CV),
                                                                                rr.2.to.1.log.sd=0.5*log(TRATE.RR.1.2.SPAN),
                                                                                rr.0.to.1.log.sd=0.5*log(TRATE.RR.0.1.SPAN),
                                                                                race='hispanic',
                                                                                route='heterosexual'),
    
    other.heterosexual.transmission = create.transmission.prior.distribution(r1.log.mean=log(HET.BASE.TRATE.MEAN),
                                                                             r1.log.sd=log(BASE.TRATE.CV),
                                                                             rr.2.to.1.log.sd=0.5*log(TRATE.RR.1.2.SPAN),
                                                                             rr.0.to.1.log.sd=0.5*log(TRATE.RR.0.1.SPAN),
                                                                             race='other',
                                                                             route='heterosexual'),
    
    heterosexual.peak.trate.multiplier =  Lognormal.Distribution(log(2.2), 0.5*log(TRATE.RR.0.PEAK.SPAN)),
    
    
    #-- IDU Transmission --#
    black.idu.transmission = create.transmission.prior.distribution(r1.log.mean=log(IDU.BASE.TRATE.MEAN),
                                                                    r1.log.sd=log(BASE.TRATE.CV),
                                                                    rr.2.to.1.log.sd=0.5*log(TRATE.RR.1.2.SPAN),
                                                                    rr.0.to.1.log.sd=0.5*log(TRATE.RR.0.1.SPAN),
                                                                    race='black',
                                                                    route='idu'),
    
    hispanic.idu.transmission = create.transmission.prior.distribution(r1.log.mean=log(IDU.BASE.TRATE.MEAN),
                                                                       r1.log.sd=log(BASE.TRATE.CV),
                                                                       rr.2.to.1.log.sd=0.5*log(TRATE.RR.1.2.SPAN),
                                                                       rr.0.to.1.log.sd=0.5*log(TRATE.RR.0.1.SPAN),
                                                                       race='hispanic',
                                                                       route='idu'),
    
    other.idu.transmission = create.transmission.prior.distribution(r1.log.mean=log(IDU.BASE.TRATE.MEAN),
                                                                    r1.log.sd=log(BASE.TRATE.CV),
                                                                    rr.2.to.1.log.sd=0.5*log(TRATE.RR.1.2.SPAN),
                                                                    rr.0.to.1.log.sd=0.5*log(TRATE.RR.0.1.SPAN),
                                                                    race='other',
                                                                    route='idu'),
    
    idu.peak.trate.multiplier =  Lognormal.Distribution(log(4.7), 0.5*log(TRATE.RR.0.PEAK.SPAN)),
    
    #-- MSM-IDU Transmission --#
    
    # take the OR of borrowing needles from table 2 of 
    # https://pubmed.ncbi.nlm.nih.gov/9489050/
    # as an RR
    msm.vs.heterosexual.male.idu.susceptibility.rr.peak = Lognormal.Distribution(log(3.3), 0.5*log(2)),
    msm.vs.heterosexual.male.idu.susceptibility.rr.0 = Lognormal.Distribution(log(3.3), 0.5*log(2)),
    msm.vs.heterosexual.male.idu.susceptibility.rr.1 = Lognormal.Distribution(log(3.3), 0.5*log(2)),
    msm.vs.heterosexual.male.idu.susceptibility.rr.2 = Lognormal.Distribution(log(3.3), 0.5*log(2)),
    
    #-- Age Susceptibility --#
    
    age1.susceptibility.rr.mult = Lognormal.Distribution(0, 0.25*log(2)),
    age2.susceptibility.rr.mult = Lognormal.Distribution(0, 0.25*log(2)),
    age4.susceptibility.rr.mult = Lognormal.Distribution(0, 0.25*log(2)),
    age5.susceptibility.rr.mult = Lognormal.Distribution(0, 0.25*log(2)),
    
    
    age1.msm.susceptibility.rr.mult.1 = Lognormal.Distribution(0, 0.25*log(2)),
    age2.msm.susceptibility.rr.mult.1 = Lognormal.Distribution(0, 0.25*log(2)),
    age4.msm.susceptibility.rr.mult.12 = Lognormal.Distribution(0, 0.25*log(2)),
    age5.msm.susceptibility.rr.mult.12 = Lognormal.Distribution(0, 0.25*log(2)),
    
    age1.msm.susceptibility.rr.mult.2 = Lognormal.Distribution(0, 0.25*log(2)),
    age2.msm.susceptibility.rr.mult.2 = Lognormal.Distribution(0, 0.25*log(2)),
    
    #-- Aging --#
    
    #Age1 rates from
    #https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-supplemental-report-vol-24-5.pdf
    #table 10b
    
    #aging.2 rates from 2018 data in table 16b of
    # https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2018-updated-vol-31.pdf
    
    #aging.1 rates from 2010 data in table 15a of
    # https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2011-vol-23.pdf
    
    #aging.0 rates from 2000 data in table 10 of
    # https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2002-vol-14.pdf
    
    msm.age1.aging.base = Lognormal.Distribution(log(12209/2/22537), 0.25*log(2)),
    msm.age2.aging.0 = Lognormal.Distribution(log(get.aging.rate.mid.of.20(56552,151476)), 0.5*log(2)),
    msm.age2.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(49965,69515)), 0.5*log(2)),
    #msm.age2.aging.2 = Lognormal.Distribution(log(get.aging.rate.last.of.10(11562,16831), 0.5*log(2)),
    msm.age3.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(91431,140082)), 0.5*log(2)),
    #msm.age3.aging.2 = Lognormal.Distribution(log(get.aging.rate.last.of.10(23488,28704)), 0.5*log(2)),
    #msm.age4.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(173559,139643)), 0.5*log(2)),
    #msm.age4.aging.2 = Lognormal.Distribution(log(get.aging.rate.last.of.10(34401,38437)), 0.5*log(2)),
    
    heterosexual.age1.aging.base = Lognormal.Distribution(log((427+1861)/2/(814+3752)), 0.25*log(2)),
    heterosexual.age2.aging.0 = Lognormal.Distribution(log(get.aging.rate.mid.of.20(56552,151476)), 0.5*log(2)),
    heterosexual.age2.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(49965,69515)), 0.5*log(2)),
    #heterosexual.age2.aging.2 = Lognormal.Distribution(log(get.aging.rate.last.of.10(11562,16831), 0.5*log(2)),
    heterosexual.age3.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(91431,140082)), 0.5*log(2)),
    #heterosexual.age3.aging.2 = Lognormal.Distribution(log(get.aging.rate.last.of.10(23488,28704)), 0.5*log(2)),
    #heterosexual.age4.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(173559,139643)), 0.5*log(2)),
    #heterosexual.age4.aging.2 = Lognormal.Distribution(log(get.aging.rate.last.of.10(34401,38437)), 0.5*log(2)),
    
    idu.age1.aging.base = Lognormal.Distribution(log((180+221)/2/(218+411)), 0.25*log(2)),
    idu.age2.aging.0 = Lognormal.Distribution(log(get.aging.rate.mid.of.20(56552,151476)), 0.5*log(2)),
    idu.age2.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(49965,69515)), 0.5*log(2)),
    #idu.age2.aging.2 = Lognormal.Distribution(log(get.aging.rate.last.of.10(11562,16831), 0.5*log(2)),
    idu.age3.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(91431,140082)), 0.5*log(2)),
    #idu.age3.aging.2 = Lognormal.Distribution(log(get.aging.rate.last.of.10(23488,28704)), 0.5*log(2)),
    #idu.age4.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(173559,139643)), 0.5*log(2)),
    #idu.age4.aging.2 = Lognormal.Distribution(log(get.aging.rate.last.of.10(34401,38437)), 0.5*log(2)),
    
    
    
    #-- Other Sex-Specific Transmission Parameters --#
    
    #products of 
    # 1) ratio of female.to.male vs male.to.female - from Maunank's paper
    # 2) ratio of condomless vaginal sex (male vs female)
    male.vs.female.heterosexual.rr = Lognormal.Distribution(log(3.75/4.75 * 87.4/92), 0.5*log(2)),
    
    #idu by sex from table 9 and 10 from
    #  https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-24.pdf
    # RR of prob of daily use (heroin) * prob of needle sharing
    female.vs.heterosexual.male.idu.susceptibility.rr = Lognormal.Distribution(log(.777/.755*.626/.585), 0.5*log(2)),
    
    #-- HIV Testing --#
    heterosexual.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    msm.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    idu.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    msm.idu.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    
    black.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    hispanic.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    
    age1.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    age2.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    age4.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    age5.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    
    heterosexual.proportion.tested.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    msm.proportion.tested.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    idu.proportion.tested.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    msm.idu.proportion.tested.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    testing.ramp.up.vs.current.rr = Lognormal.Distribution(log(0.5), 0.25*log(2), upper = 1),
    
    
    #-- LINKAGE --#
    heterosexual.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    msm.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    idu.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    msm.idu.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    
    black.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    hispanic.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    
    age1.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    age2.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    age4.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    age5.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    
    heterosexual.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    msm.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    idu.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    msm.idu.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    black.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    hispanic.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    age1.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    age2.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    age4.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    age5.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    
    #-- STARTING ART --#
    heterosexual.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    msm.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    idu.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    msm.idu.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    age1.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    age2.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    age4.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    age5.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    full.art.year = Uniform.Distribution(2015, 2018),
    
    #-- ADHERENCE --#
    heterosexual.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    msm.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    idu.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    msm.idu.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    age1.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    age2.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    age4.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    age5.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    heterosexual.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    msm.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    idu.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    msm.idu.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    black.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    hispanic.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    age1.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    age2.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    age4.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    age5.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    # (plus a term for which direction suppression/loss of suppression)
    recently.suppressed.vs.failing.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    durably.suppressed.vs.failing.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    naive.vs.failing.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    #-- DISENGAGEMENT/REENGAGEMENT --#
    heterosexual.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    msm.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    idu.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    msm.idu.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    age1.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    age2.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    age4.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    age5.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    heterosexual.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    msm.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    idu.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    msm.idu.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    black.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    hispanic.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    age1.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    age2.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    age4.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    age5.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    # (plus a term for from suppressed or from unsuppressed) 
    recently.suppressed.vs.failing.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(4)),
    durably.suppressed.vs.failing.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(4)),
    naive.vs.failing.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(4)),
    already.lost.vs.failing.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(4)),
    
    #-- PrEP --#
    
    msm.prep.intercept.or = Lognormal.Distribution(0, log(8)),
    non.msm.prep.intercept.or = Lognormal.Distribution(0, log(4)),
    
    # NB: These slope ORs are NOT exponentiated logistic slopes
    # They are an OR that is applied to the linear (ie probability scale) slope
    msm.prep.slope.or = Lognormal.Distribution(0, log(8)),
    non.msm.prep.slope.or = Compound.Symmetry.Multivariate.Normal.Distribution(rho=0.8,
                                                                               n=2,
                                                                               mu=0,
                                                                               sds = log(4),
                                                                               transformations = 'log',
                                                                               lower=0,
                                                                               var.names=c('idu.prep.slope.or',
                                                                                           'heterosexual.prep.slope.or')),
    
    # These ORs are applied to both intercept and slope
    black.prep.or = Lognormal.Distribution(0, log(1.25)/2),
    hispanic.prep.or = Lognormal.Distribution(0, log(1.25)/2),
    
    age1.prep.or = Lognormal.Distribution(0, log(2)),
    age2.prep.or = Lognormal.Distribution(0, log(2)),
    age4.prep.or = Lognormal.Distribution(0, log(2)),
    age5.prep.or = Lognormal.Distribution(0, log(2)),
    
    prep.efficacy.z = Normal.Distribution(0, 1),
    prep.persistence = Normal.Distribution(0.56, 0.0587, lower=0, upper=1),
    #from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6378757/
    # with Wald CI inflated 10x
    
    
    #-- Proportion MSM --#
    proportion.msm.of.male.mult = Lognormal.Distribution(0, 0.125*log(2)),
    
    #-- IDU Transitions --#
    
    black.incident.idu.multiplier.0 = Lognormal.Distribution(0, .5*log(2)),
    hispanic.incident.idu.multiplier.0 = Lognormal.Distribution(0, .5*log(2)),
    other.incident.idu.multiplier.0 = Lognormal.Distribution(0, .5*log(2)),
    
    black.incident.idu.multiplier.2 = Lognormal.Distribution(0, .5*log(2)),
    hispanic.incident.idu.multiplier.2 = Lognormal.Distribution(0, .5*log(2)),
    other.incident.idu.multiplier.2 = Lognormal.Distribution(0, .5*log(2)),
    
    msm.incident.idu.multiplier.0 = Lognormal.Distribution(0, .5*log(2)),
    msm.incident.idu.multiplier.2 = Lognormal.Distribution(0, .5*log(2)),
    
    idu.remission.multiplier = Lognormal.Distribution(0, .5*log(2)),
    idu.relapse.multiplier = Lognormal.Distribution(0, .5*log(2)),
    #idu.mortality = Lognormal.Distribution(log(0.0166), 0.1322), 
    #citation=25409098,
    #non-AIDS mortality in hiv-negative from table 3'
    
    #-- HIV-Specific Mortality --#
    hiv.mortality.0 = Lognormal.Distribution(log(9.5/6.1 * 23/1000), log(2)/2),
    hiv.mortality.2 = Lognormal.Distribution(log(23/1000), log(2)),
    peak.hiv.mortality = Lognormal.Distribution(log(41/6.1 * 23/1000), log(2)/2),
    #http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.688.1831&rep=rep1&type=pdf
    
    #-- Sexual Mixing by Age --#
    age.mixing.sd.mult = Lognormal.Distribution(0, 0.25*log(2)),
    
    #-- Other Sexual Mixing --#
    
    oe.female.pairings.with.msm = Lognormal.Distribution(log(0.0895), 0.5*log(2), upper = 1), #Pathela 2006 - see below
    fraction.heterosexual.male.pairings.with.male = Lognormal.Distribution(log(.004), 0.5*log(2), upper=1),
    oe.never.idu.pairings.with.idu = Lognormal.Distribution(log(0.2), 0.5*log(2), upper=1), #see calculations below
    
    black.black.sexual.oe = Lognormal.Distribution(log(3.76), 0.25*log(2), lower=1), #see below
    hispanic.hispanic.sexual.oe = Lognormal.Distribution(log(2.19), 0.25*log(2), lower=1),
    other.other.sexual.oe = Lognormal.Distribution(log(1.55), 0.25*log(2), lower=1),
    
    #-- Acute HIV and the Effect of Diagnosis --#
    acute.transmissibility.rr = Lognormal.Distribution(log(12), 0.25*log(2)), #do I have a more evidence based range?
    diagnosed.transmission.rr = Lognormal.Distribution(log(mean(c(1-.68, 1/3.5))), 0.25*log(2), upper=1), #avg of Marks 2006 and Marks 2005
    
    #-- Uncertainty About the Future --#
    msm.fraction.trate.change.after.t2 = Lognormal.Distribution(meanlog=log(0.1), sdlog=log(2), lower=0, upper=0.5),
    heterosexual.fraction.trate.change.after.t2 = Lognormal.Distribution(meanlog=log(0.1), sdlog=log(2), lower=0, upper=0.5),
    idu.fraction.trate.change.after.t2 = Lognormal.Distribution(meanlog=log(0.1), sdlog=log(2), lower=0, upper=0.5)
)
VERSION.MANAGER = register.parameters.prior(VERSION.MANAGER,
                                            prior=EXPANDED.CONTINUUM.PARAMETERS.PRIOR,
                                            version='expanded_1.0')

EXPANDED.CONTINUUM.PARAMETER.VAR.BLOCKS.1 = list(
    
    peak.msm.transmission = 'msm.peak.trate.multiplier',
    
    black.msm.transmission = c('black.msm.trate.0',
                               'black.msm.trate.1',
                               'black.msm.trate.2'),
    
    hispanic.msm.transmission = c('hispanic.msm.trate.0',
                                  'hispanic.msm.trate.1',
                                  'hispanic.msm.trate.2'),
    
    other.msm.transmission = c('other.msm.trate.0',
                               'other.msm.trate.1',
                               'other.msm.trate.2',
                               'msm.fraction.trate.change.after.t2'),
    
    msm.aging = c('msm.age1.aging.base',
                  'msm.age2.aging.0',
                  'msm.age2.aging.1',
                  'msm.age3.aging.1'),
    
    sexual.mixing = c('black.black.sexual.oe',
                      'hispanic.hispanic.sexual.oe',
                      'other.other.sexual.oe'),
    
    msm.age1.susceptibility = c('age1.msm.susceptibility.rr.mult.1',
                                'age1.msm.susceptibility.rr.mult.2'),
    
    msm.age2.susceptibility = c('age2.msm.susceptibility.rr.mult.1',
                                'age2.msm.susceptibility.rr.mult.2'),
    
    old.msm.age.susceptibility = c('age4.msm.susceptibility.rr.mult.12',
                                   'age5.msm.susceptibility.rr.mult.12'),
    
    sexual.pairing = c('oe.female.pairings.with.msm',
                       'fraction.heterosexual.male.pairings.with.male',
                       'oe.never.idu.pairings.with.idu'),
    
    proportion.msm.of.male = 'proportion.msm.of.male.mult',
    
    age.mixing = 'age.mixing.sd.mult',
    
    peak.heterosexual.transmission = 'heterosexual.peak.trate.multiplier',
    
    black.heterosexual.transmission = c('black.heterosexual.trate.0',
                                        'black.heterosexual.trate.1',
                                        'black.heterosexual.trate.2'),
    
    hispanic.heterosexual.transmission = c('hispanic.heterosexual.trate.0',
                                           'hispanic.heterosexual.trate.1',
                                           'hispanic.heterosexual.trate.2'),
    
    other.heterosexual.transmission = c('other.heterosexual.trate.0',
                                        'other.heterosexual.trate.1',
                                        'other.heterosexual.trate.2',
                                        'heterosexual.fraction.trate.change.after.t2'),
    
    male.vs.female.heterosexual.rr = 'male.vs.female.heterosexual.rr',
    
    heterosexual.aging = c('heterosexual.age1.aging.base',
                           'heterosexual.age2.aging.0',
                           'heterosexual.age2.aging.1',
                           'heterosexual.age3.aging.1'),
    
    peak.idu.transmission = 'idu.peak.trate.multiplier',
    
    black.idu.transmission = c('black.idu.trate.0',
                               'black.idu.trate.1',
                               'black.idu.trate.2'),
    
    hispanic.idu.transmission = c('hispanic.idu.trate.0',
                                  'hispanic.idu.trate.1',
                                  'hispanic.idu.trate.2'),
    
    other.idu.transmission = c('other.idu.trate.0',
                               'other.idu.trate.1',
                               'other.idu.trate.2',
                               'idu.fraction.trate.change.after.t2'),
    
    female.vs.heterosexual.male.idu.susceptibility = 'female.vs.heterosexual.male.idu.susceptibility.rr',
    
    msm.idu.transmission = c('msm.vs.heterosexual.male.idu.susceptibility.rr.peak',
                             'msm.vs.heterosexual.male.idu.susceptibility.rr.0',
                             'msm.vs.heterosexual.male.idu.susceptibility.rr.1',
                             'msm.vs.heterosexual.male.idu.susceptibility.rr.2'),
    
    idu.aging = c('idu.age1.aging.base',
                  'idu.age2.aging.0',
                  'idu.age2.aging.1',
                  'idu.age3.aging.1'),
    
    young.age.susceptibility = c('age1.susceptibility.rr.mult',
                                 'age2.susceptibility.rr.mult'),
    
    age4.susceptibility = 'age4.susceptibility.rr.mult',
    
    age5.susceptibility = 'age5.susceptibility.rr.mult',
    
    idu.transitions.0 = c('black.incident.idu.multiplier.0',
                          'hispanic.incident.idu.multiplier.0',
                          'other.incident.idu.multiplier.0',
                          'msm.incident.idu.multiplier.0'),
    
    idu.transitions.2 = c('black.incident.idu.multiplier.2',
                          'hispanic.incident.idu.multiplier.2',
                          'other.incident.idu.multiplier.2',
                          'msm.incident.idu.multiplier.2'),
    
    other.idu.transitions = c('idu.remission.multiplier',
                              'idu.relapse.multiplier'),
    
    diagnosed.transmission = c('diagnosed.transmission.rr',
                               'global.trate'),
    
    acute.transmissibility = c('acute.transmissibility.rr',
                               'global.trate'),
    
    msm.heterosexual.testing = c('msm.proportion.tested.or',
                                 'msm.proportion.tested.slope.or',
                                 'heterosexual.proportion.tested.or',
                                 'heterosexual.proportion.tested.slope.or'),
    
    idu.testing = c('idu.proportion.tested.or',
                    'idu.proportion.tested.slope.or'),
    
    msm.idu.testing = c('msm.idu.proportion.tested.or',
                        'msm.idu.proportion.tested.slope.or'),
    
    testing.by.race = c('black.proportion.tested.or',
                        'hispanic.proportion.tested.or'),
    
    young.age.testing = c('age1.proportion.tested.or',
                          'age2.proportion.tested.or'),
    
    old.age.testing = c('age4.proportion.tested.or',
                        'age5.proportion.tested.or'),
    
    testing.ramp.up = 'testing.ramp.up.vs.current.rr',
    
    msm.suppression = c('msm.proportion.linked.or',
                        'msm.proportion.linked.slope.or',
                        'msm.idu.proportion.linked.or',
                        'msm.idu.proportion.linked.slope.or'),
    
    idu.heterosexual.suppression = c('idu.proportion.linked.or',
                                     'idu.proportion.linked.slope.or',
                                     'heterosexual.proportion.linked.or',
                                     'heterosexual.proportion.linked.slope.or'),
    
    linked.by.race = c('black.proportion.linked.or',
                       'black.proportion.linked.slope.or',
                       'hispanic.proportion.linked.or',
                       'hispanic.proportion.linked.slope.or'),
    
    young.linked = c('age1.proportion.linked.or',
                     'age1.proportion.linked.slope.or',
                     'age2.proportion.linked.or',
                     'age2.proportion.linked.slope.or'),
    
    old.linked = c('age4.proportion.linked.or',
                   'age4.proportion.linked.slope.or',
                   'age5.proportion.linked.or',
                   'age5.proportion.linked.slope.or'),
    
    start.art.by.risk = c('heterosexual.start.art.or',
                          'msm.start.art.or',
                          'idu.start.art.or',
                          'msm.idu.start.art.or'), 
    
    start.art.by.race = c('black.start.art.or',
                          'hispanic.start.art.or',
                          'full.art.year'),  
    
    start.art.by.age = c('age1.start.art.or',
                         'age2.start.art.or',
                         'age4.start.art.or',
                         'age5.start.art.or'), 
    
    msm.adherence = c('msm.proportion.adherent.or',
                      'msm.proportion.adherent.slope.or',
                      'msm.idu.proportion.adherent.or',
                      'msm.idu.proportion.adherent.slope.or'),
    
    idu.heterosexual.adherence = c('idu.proportion.adherent.or',
                                   'idu.proportion.adherent.slope.or',
                                   'heterosexual.proportion.adherent.or',
                                   'heterosexual.proportion.adherent.slope.or'),
    
    
    adherence.by.race = c('black.proportion.adherent.or',
                          'black.proportion.adherent.slope.or',
                          'hispanic.proportion.adherent.or',
                          'hispanic.proportion.adherent.slope.or'),
    
    young.adherence = c('age1.proportion.adherent.or',
                        'age1.proportion.adherent.slope.or',
                        'age2.proportion.adherent.or',
                        'age2.proportion.adherent.slope.or'),
    
    old.adherence = c('age4.proportion.adherent.or',
                      'age4.proportion.adherent.slope.or',
                      'age5.proportion.adherent.or',
                      'age5.proportion.adherent.slope.or'),
    
    
    msm.loss = c('msm.proportion.lost.or',
                 'msm.proportion.lost.slope.or',
                 'msm.idu.proportion.lost.or',
                 'msm.idu.proportion.lost.slope.or'),
    
    idu.heterosexual.loss = c('idu.proportion.lost.or',
                              'idu.proportion.lost.slope.or',
                              'heterosexual.proportion.lost.or',
                              'heterosexual.proportion.lost.slope.or'),
    
    loss.by.race = c('black.proportion.lost.or',
                     'black.proportion.lost.slope.or',
                     'hispanic.proportion.lost.or',
                     'hispanic.proportion.lost.slope.or'),
    
    young.loss = c('age1.proportion.lost.or',
                   'age1.proportion.lost.slope.or',
                   'age2.proportion.lost.or',
                   'age2.proportion.lost.slope.or'),
    
    old.loss = c('age4.proportion.lost.or',
                 'age4.proportion.lost.slope.or',
                 'age5.proportion.lost.or',
                 'age5.proportion.lost.slope.or'),
    
    suppressed.vs.nonsuppressed.adherence = c('recently.suppressed.vs.failing.proportion.adherent.or',
                                              'durably.suppressed.vs.failing.proportion.adherent.or',
                                              'naive.vs.failing.proportion.adherent.or'),
    
    suppressed.vs.nonsuppressed.retention = c('recently.suppressed.vs.failing.proportion.lost.or',
                                              'durably.suppressed.vs.failing.proportion.lost.or',
                                              'naive.vs.failing.proportion.lost.or',
                                              'already.lost.vs.failing.proportion.lost.or'),
    
    msm.prep = c('msm.prep.intercept.or',
                 'msm.prep.slope.or',
                 'prep.persistence'),
    
    non.msm.prep = c('non.msm.prep.intercept.or',
                     'idu.prep.slope.or',
                     'heterosexual.prep.slope.or'),
    
    prep.by.race = c('black.prep.or',
                     'hispanic.prep.or',
                     'prep.efficacy.z'),
    
    prep.by.age = c('age1.prep.or',
                    'age2.prep.or',
                    'age4.prep.or',
                    'age5.prep.or'),
    
    hiv.mortality = c('peak.hiv.mortality',
                      'hiv.mortality.0',
                      'hiv.mortality.2')
)
VERSION.MANAGER = register.parameter.sampling.blocks(VERSION.MANAGER,
                                                     blocks=EXPANDED.CONTINUUM.PARAMETER.VAR.BLOCKS.1,
                                                     version='expanded_1.0')

if (1==2)
{
    length(EXPANDED.CONTINUUM.PARAMETER.VAR.BLOCKS.1)
    setdiff(unique(unlist(EXPANDED.CONTINUUM.PARAMETER.VAR.BLOCKS.1)), EXPANDED.CONTINUUM.PARAMETERS.PRIOR@var.names)    
    setdiff(EXPANDED.CONTINUUM.PARAMETERS.PRIOR@var.names, unique(unlist(EXPANDED.CONTINUUM.PARAMETER.VAR.BLOCKS.1)))    
}



##---------------------------------##
##-- The Get Components Function --##
##---------------------------------##
##
EXPANDED.CONTINUUM.GET.COMPONENTS.FUNCTION = join.get.components.functions(
    BASE.GET.COMPONENTS.FUNCTION,
function(parameters, components,
         data.managers = ALL.DATA.MANAGERS)
{
    exclude.from.check = c('recently.suppressed.vs.failing.proportion.adherent.or', 
                           'durably.suppressed.vs.failing.proportion.adherent.or', 
                           'naive.vs.failing.proportion.adherent.or',
                           'recently.suppressed.vs.failing.proportion.lost.or', 
                           'durably.suppressed.vs.failing.proportion.lost.or', 
                           'naive.vs.failing.proportion.lost.or',
                           'already.lost.vs.failing.proportion.lost.or')
    
    # Linkage
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='linkage',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.linked.or',
                                                                slope.parameter.suffix = 'proportion.linked.slope.or',
                                                                idu.applies.to.in.remission = T)
    # Failing to Recently Suppressed
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='failing.to.suppressed',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.adherent.or',
                                                                slope.parameter.suffix = 'proportion.adherent.slope.or',
                                                                idu.applies.to.in.remission = T,
                                                                exclude.from.check = exclude.from.check)

    # Recently Suppressed to Failing
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='recently.suppressed.to.failing',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.adherent.or',
                                                                slope.parameter.suffix = 'proportion.adherent.slope.or',
                                                                idu.applies.to.in.remission = T,
                                                                transformation = 'reciprocal',
                                                                sex.risk.intercept.multiplier = parameters['recently.suppressed.vs.failing.proportion.adherent.or'],
                                                                exclude.from.check = exclude.from.check)
    
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='durably.suppressed.to.failing',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.adherent.or',
                                                                slope.parameter.suffix = 'proportion.adherent.slope.or',
                                                                idu.applies.to.in.remission = T,
                                                                transformation = 'reciprocal',
                                                                sex.risk.intercept.multiplier = parameters['durably.suppressed.vs.failing.proportion.adherent.or'],
                                                                exclude.from.check = exclude.from.check)
    

    # Naive - Recently suppressed
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='naive.to.suppressed',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.adherent.or',
                                                                slope.parameter.suffix = 'proportion.adherent.slope.or',
                                                                idu.applies.to.in.remission = T,
                                                                transformation = 'identity',
                                                                sex.risk.intercept.multiplier = parameters['naive.vs.failing.proportion.adherent.or'],
                                                                exclude.from.check = exclude.from.check)
    
    # Recently Suppressed to Disengaged
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='recently.suppressed.to.disengaged',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.lost.or',
                                                                slope.parameter.suffix = 'proportion.lost.slope.or',
                                                                idu.applies.to.in.remission = T,
                                                                transformation = 'identity',
                                                                sex.risk.intercept.multiplier = parameters['recently.suppressed.vs.failing.proportion.lost.or'],
                                                                exclude.from.check = exclude.from.check)
    
    # Durably Suppressed to Disengaged
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='durably.suppressed.to.disengaged',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.lost.or',
                                                                slope.parameter.suffix = 'proportion.lost.slope.or',
                                                                idu.applies.to.in.remission = T,
                                                                transformation = 'identity',
                                                                sex.risk.intercept.multiplier = parameters['durably.suppressed.vs.failing.proportion.lost.or'],
                                                                exclude.from.check = exclude.from.check)
    
    # Failing to Disengaged
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='failing.to.disengaged',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.lost.or',
                                                                slope.parameter.suffix = 'proportion.lost.slope.or',
                                                                idu.applies.to.in.remission = T,
                                                                transformation = 'identity',
                                                                sex.risk.intercept.multiplier = 1,
                                                                exclude.from.check = exclude.from.check)
    
    # Naive to Disengaged
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='naive.to.disengaged',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.lost.or',
                                                                slope.parameter.suffix = 'proportion.lost.slope.or',
                                                                idu.applies.to.in.remission = T,
                                                                transformation = 'identity',
                                                                sex.risk.intercept.multiplier = parameters['naive.vs.failing.proportion.lost.or'],
                                                                exclude.from.check = exclude.from.check)

    # Reengagement
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='reengagement',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.lost.or',
                                                                slope.parameter.suffix = 'proportion.lost.slope.or',
                                                                idu.applies.to.in.remission = T,
                                                                transformation = 'reciprocal',
                                                                sex.risk.intercept.multiplier = parameters['already.lost.vs.failing.proportion.lost.or'],
                                                                exclude.from.check = exclude.from.check)
 
    # Time to start art
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='start.art',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'start.art.or',
                                                                slope.parameter.suffix = NULL, #no slope
                                                                idu.applies.to.in.remission = T)

    #-- Smoothing Years --#
    
    if (any(grepl('gains\\.end\\.by\\.year', names(parameters))))
        components = set.background.change.to.years(components,
                                                    
                                                    linkage = parameters['continuum.gains.end.by.year'],
                                                    
                                                    naive.to.suppressed = parameters['continuum.gains.end.by.year'],
                                                    naive.to.disengaged = parameters['continuum.gains.end.by.year'],
                                                    
                                                    failing.to.disengaged = parameters['continuum.gains.end.by.year'],
                                                    failing.to.suppressed = parameters['continuum.gains.end.by.year'],
                                                    
                                                    suppressed.to.disengaged = parameters['continuum.gains.end.by.year'],
                                                    suppressed.to.failing = parameters['continuum.gains.end.by.year'],
                                                    
                                                    reengagement = parameters['continuum.gains.end.by.year']
        )
    
    if (any(grepl('total\\.future.*slope\\.or', names(parameters))))
        components = set.future.background.slopes(components,
                                                  
                                                  linkage = parameters['total.future.linkage.slope.or'],
                                                  
                                                  # to suppressed
                                                  naive.to.recently.suppressed = parameters['total.future.suppressed.slope.or'],
                                                  failing.to.recently.suppressed = parameters['total.future.suppressed.slope.or'],
                                                  
                                                  # to failing
                                                  recently.suppressed.to.failing = parameters['total.future.unsuppression.slope.or'],
                                                  durably.suppressed.to.failing = parameters['total.future.unsuppression.slope.or'],
                                                  naive.to.failing = parameters['total.future.unsuppression.slope.or'],
                                                  
                                                  # to disengaged
                                                  failing.to.disengaged = parameters['total.future.disengagement.slope.or'],
                                                  naive.to.disengaged = parameters['total.future.disengagement.slope.or'],
                                                  recently.suppressed.to.disengaged = parameters['total.future.disengagement.slope.or'],
                                                  durably.suppressed.to.disengaged = parameters['total.future.disengagement.slope.or'],
                                                  
                                                  # reengagement
                                                  reengagement = parameters['total.future.reengagement.slope.or'],
                                                  
                                                  after.year = parameters['future.slope.after.year'])
    
    #-- Return --#
    components
})

VERSION.MANAGER = register.get.components.function(VERSION.MANAGER, 
                                                   fn=EXPANDED.CONTINUUM.GET.COMPONENTS.FUNCTION,
                                                   version = 'expanded_1.0')


