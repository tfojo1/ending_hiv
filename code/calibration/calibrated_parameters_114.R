#113 - like 108 but with peak multiplier
library(distributions)

MSM.BASE.TRATE.MEAN = 1
HET.BASE.TRATE.MEAN = 1
IDU.BASE.TRATE.MEAN = 1
BASE.TRATE.CV = 20

TRATE.RR.1.2.SPAN = 4#2#1.5
TRATE.RR.0.1.SPAN = 4#2#1.5
TRATE.RR.0.PEAK.SPAN = 8#3



parameters.prior = join.distributions(

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
    female.vs.heterosexual.male.idu.susceptibility.rr = Lognormal.Distribution(log(.777/.755*.364/.309), 0.5*log(2)),
    
    #-- HIV Testing --#
    heterosexual.proportion.tested.or = Lognormal.Distribution(0, 0.5*log(2)),
    msm.proportion.tested.or = Lognormal.Distribution(0, 0.5*log(2)),
    idu.proportion.tested.or = Lognormal.Distribution(0, 0.5*log(2)),
    msm.idu.proportion.tested.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.proportion.tested.or = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.proportion.tested.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    age1.proportion.tested.or = Lognormal.Distribution(0, 0.5*log(2)),
    age2.proportion.tested.or = Lognormal.Distribution(0, 0.5*log(2)),
    age4.proportion.tested.or = Lognormal.Distribution(0, 0.5*log(2)),
    age5.proportion.tested.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    heterosexual.proportion.tested.slope.or = Lognormal.Distribution(0, log(1.25)),
    msm.proportion.tested.slope.or = Lognormal.Distribution(0, log(1.25)),
    idu.proportion.tested.slope.or = Lognormal.Distribution(0, log(1.25)),
    msm.idu.proportion.tested.slope.or = Lognormal.Distribution(0, log(1.25)),
    
    testing.ramp.up.vs.current.rr = Lognormal.Distribution(log(0.5), 0.25*log(2), upper = 1),
    
    
    #-- Suppression --#
    heterosexual.suppressed.or = Lognormal.Distribution(0, log(2)),
    msm.suppressed.or = Lognormal.Distribution(0, log(2)),
    idu.suppressed.or = Lognormal.Distribution(0, log(2)),
    msm.idu.suppressed.or = Lognormal.Distribution(0, log(2)),
    
    black.suppressed.or = Lognormal.Distribution(0, log(2)),
    hispanic.suppressed.or = Lognormal.Distribution(0, log(2)),
    
    age1.suppressed.or = Lognormal.Distribution(0, log(2)),
    age2.suppressed.or = Lognormal.Distribution(0, log(2)),
    age4.suppressed.or = Lognormal.Distribution(0, log(2)),
    age5.suppressed.or = Lognormal.Distribution(0, log(2)),
    
    heterosexual.suppressed.slope.or = Lognormal.Distribution(0, log(2)/5),
    msm.suppressed.slope.or = Lognormal.Distribution(0, log(2)/5),
    idu.suppressed.slope.or = Lognormal.Distribution(0, log(2)/5),
    msm.idu.suppressed.slope.or = Lognormal.Distribution(0, log(2)/5),
    
    black.suppressed.slope.or = Lognormal.Distribution(0, log(2)/5),
    hispanic.suppressed.slope.or = Lognormal.Distribution(0, log(2)/5),
    
    age1.suppressed.slope.or = Lognormal.Distribution(0, log(2)/5),
    age2.suppressed.slope.or = Lognormal.Distribution(0, log(2)/5),
    age4.suppressed.slope.or = Lognormal.Distribution(0, log(2)/5),
    age5.suppressed.slope.or = Lognormal.Distribution(0, log(2)/5),
    
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
    hiv.mortality.0 = Lognormal.Distribution(log(9.5/6.1 * 23/1000), log(2)/4),
    hiv.mortality.2 = Lognormal.Distribution(log(23/1000), log(2)/4),
    peak.hiv.mortality = Lognormal.Distribution(log(41/6.1 * 23/1000), log(2)/4),
    #http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.688.1831&rep=rep1&type=pdf
    
    #-- Sexual Mixing by Age --#
    age.mixing.sd.mult = Lognormal.Distribution(0, 0.25*log(2)),
    
    #-- Other Sexual Mixing --#
    
    oe.female.pairings.with.msm = Lognormal.Distribution(log(0.0895), 0.5*log(2), upper = 1), #Pathela 2006 - see below
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

PARAMETER.VAR.BLOCKS.1 = list(
    
    peak.msm.transmission = 'msm.peak.trate.multiplier',

    black.msm.transmission.0 = c('black.msm.trate.0'),
    
    hispanic.msm.transmission.0 = c('hispanic.msm.trate.0'),
    
    other.msm.transmission.0 = c('other.msm.trate.0'),
    
    msm.transmission.1 = c('black.msm.trate.1',
                           'hispanic.msm.trate.1',
                           'other.msm.trate.1'),
    
    msm.transmission.2 = c('black.msm.trate.2',
                           'hispanic.msm.trate.2',
                           'other.msm.trate.2'),
    
    msm.aging.a = c('msm.age1.aging.base',
                    'msm.age2.aging.0'),
    
    msm.aging.b = c('msm.age2.aging.1',
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
                                        'other.heterosexual.trate.2'),
    
    male.vs.female = c('male.vs.female.heterosexual.rr',
                       'female.vs.heterosexual.male.idu.susceptibility.rr'),
    
    heterosexual.aging.a = c('heterosexual.age1.aging.base',
                             'heterosexual.age2.aging.0'),
    
    heterosexual.aging.b = c('heterosexual.age2.aging.1',
                             'heterosexual.age3.aging.1'),
    
    peak.idu.transmission = 'idu.peak.trate.multiplier',
    
    black.idu.transmission.0 = c('black.idu.trate.0',
                                 'black.incident.idu.multiplier.0'),
    
    hispanic.idu.transmission.0 = c('hispanic.idu.trate.0',
                                    'hispanic.incident.idu.multiplier.0'),
    
    other.idu.transmission.0 = c('other.idu.trate.0',
                                 'other.incident.idu.multiplier.0'),

    idu.transmission.1 = c('black.idu.trate.1',
                           'hispanic.idu.trate.1',
                           'other.idu.trate.1'),
    
    black.idu.transmission.2 = c('black.idu.trate.2',
                                 'black.incident.idu.multiplier.2'),
    
    hispanic.idu.transmission.2 = c('hispanic.idu.trate.2',
                                    'hispanic.incident.idu.multiplier.2'),
    
    other.idu.transmission.2 = c('other.idu.trate.2',
                                 'other.incident.idu.multiplier.2'),
    
    early.msm.idu.transmission = c('msm.vs.heterosexual.male.idu.susceptibility.rr.peak',
                                   'msm.vs.heterosexual.male.idu.susceptibility.rr.0',
                                   'msm.incident.idu.multiplier.0'),
    
    late.msm.idu.transmission = c('msm.vs.heterosexual.male.idu.susceptibility.rr.1',
                                  'msm.vs.heterosexual.male.idu.susceptibility.rr.2',
                                  'msm.incident.idu.multiplier.2'),
    
    idu.aging.a = c('idu.age1.aging.base',
                    'idu.age2.aging.0'),
    
    idu.aging.b = c('idu.age2.aging.1',
                    'idu.age3.aging.1'),
    
    young.age.susceptibility = c('age1.susceptibility.rr.mult',
                                 'age2.susceptibility.rr.mult'),
    
    old.age.susceptibility = c('age4.susceptibility.rr.mult',
                               'age5.susceptibility.rr.mult'),
    
    idu.remission.relapse = c('idu.remission.multiplier',
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
                    'idu.proportion.tested.slope.or',
                    'msm.idu.proportion.tested.or',
                    'msm.idu.proportion.tested.slope.or'),
    
    testing.by.race = c('black.proportion.tested.or',
                        'hispanic.proportion.tested.or'),
    
    testing.by.age = c('age1.proportion.tested.or',
                       'age2.proportion.tested.or',
                       'age4.proportion.tested.or',
                       'age5.proportion.tested.or'),
    
    testing.ramp.up = 'testing.ramp.up.vs.current.rr',

    msm.suppression = c('msm.suppressed.or',
                        'msm.suppressed.slope.or'),
    
    heterosexual.suppression = c('heterosexual.suppressed.or',
                                 'heterosexual.suppressed.slope.or'),
    
    idu.suppression = c('idu.suppressed.or',
                        'idu.suppressed.slope.or'),
    
    msm.idu.suppression = c('msm.idu.suppressed.or',
                            'msm.idu.suppressed.slope.or'),
    
    black.suppression = c('black.suppressed.or',
                          'black.suppressed.slope.or'),
    
    hispanic.suppression = c('hispanic.suppressed.or',
                             'hispanic.suppressed.slope.or'),
    
    age1.suppression = c('age1.suppressed.or',
                          'age1.suppressed.slope.or'),
    
    age2.suppression = c('age2.suppressed.or',
                         'age2.suppressed.slope.or'),
    
    age4.suppression = c('age4.suppressed.or',
                         'age4.suppressed.slope.or'),
    
    age5.suppression = c('age5.suppressed.or',
                         'age5.suppressed.slope.or'),
    
    hiv.mortality = c('peak.hiv.mortality',
                      'hiv.mortality.0',
                      'hiv.mortality.2'),

    future.change = c('msm.fraction.trate.change.after.t2',
                      'heterosexual.fraction.trate.change.after.t2',
                      'idu.fraction.trate.change.after.t2')
)

if (1==2)
{
    length(PARAMETER.VAR.BLOCKS.1)
    setdiff(unique(unlist(PARAMETER.VAR.BLOCKS.1)), parameters.prior@var.names)    
    setdiff(parameters.prior@var.names, unique(unlist(PARAMETER.VAR.BLOCKS.1)))    
}

#sexual frequency by age from
#https://link.springer.com/article/10.1007/s10508-017-0953-1
# from table 2
#
#age 1 multiplied by .6
# to balance out 13-17 vs 18-29
# From Abma 2017 (see setup for msa function)
# sum(c(.076,.108,.13,.26,.42,.55,.68,.75*5))/14 / sum(c(.55,.68,.75*5)) * 7

SEXUAL.SUSCEPTIBLITY.RR.BY.AGE = c(0.6*78.50/mean(c(77.84,63.22)),
                                   mean(c(78.50,77.84))/mean(c(77.84,63.22)),
                                   1,
                                   mean(c(63.22,38.29))/mean(c(77.84,63.22)),
                                   mean(c(38.29,25.03,10.88))/mean(c(77.84,63.22)))

#idu by age from table 9 and 10 from
#  https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-24.pdf
# RR of prob of daily use (heroin) * prob of needle sharing

IDU.SUSCEPTIBLITY.RR.BY.AGE = c(.762/.788*.720/.672,
                                .797/.788*.705/.672,
                                1,
                                .745/.788*.596/.672,
                                .740/.788*.498/.672)

#aging.1 rates from 2010 data in table 15a of
# https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2011-vol-23.pdf
AGE4.AGING.1 = get.aging.rate.last.of.10(173559,139643)

FRACTION.HET.MALE.PAIRINGS.WITH.MALE = 0.004

get.components.for.calibrated.parameters <- function(parameters, components,
                                                     data.managers = ALL.DATA.MANAGERS)
{
    #-- Setup Global Transmission --#

    components = setup.global.trates(components,
                                     global.sexual.trate = parameters['global.trate'],
                                     global.idu.trate = parameters['global.trate'])

    #-- Setup for Transmission --#
    
    # MSM
    msm.age.rr.peak = msm.age.rr0 = c(parameters['age1.susceptibility.rr.mult'],
                                      parameters['age2.susceptibility.rr.mult'],
                                      1,
                                      parameters['age4.susceptibility.rr.mult'],
                                      parameters['age5.susceptibility.rr.mult']) *
        SEXUAL.SUSCEPTIBLITY.RR.BY.AGE
    
    msm.age.rr1 = c(parameters['age1.msm.susceptibility.rr.mult.1'],
                    parameters['age2.msm.susceptibility.rr.mult.1'],
                    1,
                    parameters['age4.msm.susceptibility.rr.mult.12'],
                    parameters['age5.msm.susceptibility.rr.mult.12']) *
        SEXUAL.SUSCEPTIBLITY.RR.BY.AGE
    
    msm.age.rr2 = c(parameters['age1.msm.susceptibility.rr.mult.2'],
                    parameters['age2.msm.susceptibility.rr.mult.2'],
                    1,
                    parameters['age4.msm.susceptibility.rr.mult.12'],
                    parameters['age5.msm.susceptibility.rr.mult.12']) *
        SEXUAL.SUSCEPTIBLITY.RR.BY.AGE
    
    # Het male
    heterosexual.male.age.rr.peak = heterosexual.male.age.rr0 =
        heterosexual.male.age.rr1 = heterosexual.male.age.rr2 = 
        c(parameters['age1.susceptibility.rr.mult'],
          parameters['age2.susceptibility.rr.mult'],
          1,
          parameters['age4.susceptibility.rr.mult'],
          parameters['age5.susceptibility.rr.mult']) * 
        parameters['male.vs.female.heterosexual.rr'] *
        SEXUAL.SUSCEPTIBLITY.RR.BY.AGE
        
    # Het female
    heterosexual.female.age.rr.peak = heterosexual.female.age.rr0 =
        heterosexual.female.age.rr1 = heterosexual.female.age.rr2 = 
        c(parameters['age1.susceptibility.rr.mult'],
          parameters['age2.susceptibility.rr.mult'],
          1,
          parameters['age4.susceptibility.rr.mult'],
          parameters['age5.susceptibility.rr.mult'])  *
        SEXUAL.SUSCEPTIBLITY.RR.BY.AGE
    
    
    # IDU - heterosexual male
    idu.male.age.rr.peak = idu.male.age.rr0 =
        idu.male.age.rr1 = idu.male.age.rr2 =
        c(parameters['age1.susceptibility.rr.mult'],
          parameters['age2.susceptibility.rr.mult'],
          1,
          parameters['age4.susceptibility.rr.mult'],
          parameters['age5.susceptibility.rr.mult']) *
        IDU.SUSCEPTIBLITY.RR.BY.AGE
    
    # IDU - MSM
    idu.msm.age.rr.peak = idu.male.age.rr.peak * parameters['msm.vs.heterosexual.male.idu.susceptibility.rr.peak']
    idu.msm.age.rr0 = idu.male.age.rr0 * parameters['msm.vs.heterosexual.male.idu.susceptibility.rr.0']
    idu.msm.age.rr1 = idu.male.age.rr1 * parameters['msm.vs.heterosexual.male.idu.susceptibility.rr.1']
    idu.msm.age.rr2 = idu.male.age.rr2 * parameters['msm.vs.heterosexual.male.idu.susceptibility.rr.2']
    
    # IDU - female
    idu.female.age.rr.peak = idu.male.age.rr.peak * parameters['female.vs.heterosexual.male.idu.susceptibility.rr']
    idu.female.age.rr0 = idu.male.age.rr0 * parameters['female.vs.heterosexual.male.idu.susceptibility.rr']
    idu.female.age.rr1 = idu.male.age.rr1 * parameters['female.vs.heterosexual.male.idu.susceptibility.rr']
    idu.female.age.rr2 = idu.male.age.rr2 * parameters['female.vs.heterosexual.male.idu.susceptibility.rr']
    
    #-- MSM Transmission --#
    for (age.index in 1:5)
    {
        for (race in c('black','hispanic','other'))
        {
            components = setup.trates(components, routes='msm', races=race,
                                      age.indices = age.index,
                                      r0 = parameters[paste0(race, '.msm.trate.0')] * msm.age.rr0[age.index],
                                      r1 = parameters[paste0(race, '.msm.trate.1')] * msm.age.rr1[age.index],
                                      r2 = parameters[paste0(race, '.msm.trate.2')] * msm.age.rr2[age.index],
                                      r.peak = parameters[paste0(race, '.msm.trate.0')] * parameters['msm.peak.trate.multiplier'] *
                                          msm.age.rr.peak[age.index],
                                      fraction.change.after.end = parameters['msm.fraction.trate.change.after.t2'])
        }
    }
  
    #-- Heterosexual Male Transmission --#
    for (age.index in 1:5)
    {
        for (race in c('black','hispanic','other'))
        {
            components = setup.trates(components, routes='heterosexual.male', races=race,
                                      age.indices = age.index,
                                      r0 = parameters[paste0(race, '.heterosexual.trate.0')] * heterosexual.male.age.rr0[age.index],
                                      r1 = parameters[paste0(race, '.heterosexual.trate.1')] * heterosexual.male.age.rr1[age.index],
                                      r2 = parameters[paste0(race, '.heterosexual.trate.2')] * heterosexual.male.age.rr2[age.index],
                                      r.peak = parameters[paste0(race, '.heterosexual.trate.0')] * parameters['heterosexual.peak.trate.multiplier'] *
                                          heterosexual.male.age.rr.peak[age.index],
                                      fraction.change.after.end = parameters['heterosexual.fraction.trate.change.after.t2'])
        }
    }
    
    #-- Heterosexual Female Transmission --#
    for (age.index in 1:5)
    {
        for (race in c('black','hispanic','other'))
        {
            components = setup.trates(components, routes='heterosexual.female', races=race,
                                      age.indices = age.index,
                                      r0 = parameters[paste0(race, '.heterosexual.trate.0')] * heterosexual.female.age.rr0[age.index],
                                      r1 = parameters[paste0(race, '.heterosexual.trate.1')] * heterosexual.female.age.rr1[age.index],
                                      r2 = parameters[paste0(race, '.heterosexual.trate.2')] * heterosexual.female.age.rr2[age.index],
                                      r.peak = parameters[paste0(race, '.heterosexual.trate.0')] * parameters['heterosexual.peak.trate.multiplier'] *
                                          heterosexual.female.age.rr.peak[age.index],
                                      fraction.change.after.end = parameters['heterosexual.fraction.trate.change.after.t2'])
        }
    }
    
    #-- IDU Transmission: MSM+IDU --#
    for (age.index in 1:5)
    {
        for (race in c('black','hispanic','other'))
        {
            components = setup.trates(components, routes='idu.msm', races=race,
                                      age.indices = age.index,
                                      r0 = parameters[paste0(race, '.idu.trate.0')] * idu.msm.age.rr0[age.index],
                                      r1 = parameters[paste0(race, '.idu.trate.1')] * idu.msm.age.rr1[age.index],
                                      r2 = parameters[paste0(race, '.idu.trate.2')] * idu.msm.age.rr2[age.index],
                                      r.peak = parameters[paste0(race, '.idu.trate.0')] * parameters['idu.peak.trate.multiplier'] *
                                          idu.msm.age.rr.peak[age.index],
                                      fraction.change.after.end = parameters['idu.fraction.trate.change.after.t2'])
        }
    }
    
    #-- IDU Transmission: heterosexual male --#
    for (age.index in 1:5)
    {
        for (race in c('black','hispanic','other'))
        {
            components = setup.trates(components, routes='idu.male', races=race,
                                      age.indices = age.index,
                                      r0 = parameters[paste0(race, '.idu.trate.0')] * idu.male.age.rr0[age.index],
                                      r1 = parameters[paste0(race, '.idu.trate.1')] * idu.male.age.rr1[age.index],
                                      r2 = parameters[paste0(race, '.idu.trate.2')] * idu.male.age.rr2[age.index],
                                      r.peak = parameters[paste0(race, '.idu.trate.0')] * parameters['idu.peak.trate.multiplier'] *
                                          idu.male.age.rr.peak[age.index],
                                      fraction.change.after.end = parameters['idu.fraction.trate.change.after.t2'])
        }
    }
    
    #-- IDU Transmission: female --#
    for (age.index in 1:5)
    {
        for (race in c('black','hispanic','other'))
        {
            components = setup.trates(components, routes='idu.female', races=race,
                                      age.indices = age.index,
                                      r0 = parameters[paste0(race, '.idu.trate.0')] * idu.female.age.rr0[age.index],
                                      r1 = parameters[paste0(race, '.idu.trate.1')] * idu.female.age.rr1[age.index],
                                      r2 = parameters[paste0(race, '.idu.trate.2')] * idu.female.age.rr2[age.index],
                                      r.peak = parameters[paste0(race, '.idu.trate.0')] * parameters['idu.peak.trate.multiplier'] *
                                          idu.female.age.rr.peak[age.index],
                                      fraction.change.after.end = parameters['idu.fraction.trate.change.after.t2'])
        }
    }
    
    #-- Aging --#

    for (route in c('msm','heterosexual.male','heterosexual.female','idu'))
    {
        if (route=='heterosexual.male' || route=='heterosexual.female')
            param.route = 'heterosexual'
        else
            param.route = route
        
        components = set.aging.rates(components, route=param.route,
                                     age.indices = 1,
                                     r.pre.spike = parameters[paste0(param.route, '.age1.aging.base')],
                                     r.spike = parameters[paste0(param.route, '.age1.aging.base')],
                                     r0 = parameters[paste0(param.route, '.age1.aging.base')],
                                     r1 = parameters[paste0(param.route, '.age1.aging.base')],
                                     r2 = parameters[paste0(param.route, '.age1.aging.base')],
                                     r3 = parameters[paste0(param.route, '.age1.aging.base')])

        components = set.aging.rates(components, route=param.route,
                                     age.indices = 2,
                                     r0 = parameters[paste0(param.route, '.age2.aging.0')],
                                     r1 = parameters[paste0(param.route, '.age2.aging.1')])

        components = set.aging.rates(components, route=param.route,
                                     age.indices = 3,
                                     r1 = parameters[paste0(param.route, '.age3.aging.1')])

        components = set.aging.rates(components, route=param.route,
                                     age.indices = 4,
                                     r1 = AGE4.AGING.1)

    }

    #-- HIV Testing --#
    
    components = set.background.hiv.testing.ors(components,
                                                msm.or.intercept=parameters['msm.proportion.tested.or'],
                                                heterosexual.or.intercept=parameters['heterosexual.proportion.tested.or'],
                                                idu.or.intercept=parameters['idu.proportion.tested.or'],
                                                msm.idu.or.intercept=parameters['msm.idu.proportion.tested.or'],
                                                black.or.intercept=parameters['black.proportion.tested.or'],
                                                hispanic.or.intercept=parameters['hispanic.proportion.tested.or'],
                                                age1.or.intercept=parameters['age1.proportion.tested.or'],
                                                age2.or.intercept=parameters['age2.proportion.tested.or'],
                                                age4.or.intercept=parameters['age4.proportion.tested.or'],
                                                age5.or.intercept=parameters['age5.proportion.tested.or'],
                                                
                                                msm.or.slope=parameters['msm.proportion.tested.slope.or'],
                                                heterosexual.or.slope=parameters['heterosexual.proportion.tested.slope.or'],
                                                idu.or.slope=parameters['idu.proportion.tested.slope.or'],
                                                msm.idu.or.slope=parameters['msm.idu.proportion.tested.slope.or'])
    
    components = set.background.hiv.testing.ramp.up(components,
                                                    testing.ramp.up.vs.current.rr=parameters['testing.ramp.up.vs.current.rr'])
    

    #-- Proportion MSM --#

    components = set.proportions.msm.of.male(components,
                                             black.proportion = components$base.proportions.msm.of.male['black'] *
                                                 parameters['proportion.msm.of.male.mult'],
                                             hispanic.proportion = components$base.proportions.msm.of.male['hispanic'] *
                                                 parameters['proportion.msm.of.male.mult'],
                                             other.proportion = components$base.proportions.msm.of.male['other'] *
                                                 parameters['proportion.msm.of.male.mult'])

    #-- IDU Transitions --#
    incident.idu.0 = incident.idu.2 = components$idu.transition.elements$incident.idu

    incident.idu.0[,'black',] = incident.idu.0[,'black',] * parameters['black.incident.idu.multiplier.0']
    incident.idu.0[,'hispanic',] = incident.idu.0[,'hispanic',] * parameters['hispanic.incident.idu.multiplier.0']
    incident.idu.0[,'other',] = incident.idu.0[,'other',] * parameters['other.incident.idu.multiplier.0']
    incident.idu.0[,,'msm'] = incident.idu.0[,,'msm'] * parameters['msm.incident.idu.multiplier.0']

    incident.idu.2[,'black',] = incident.idu.2[,'black',] * parameters['black.incident.idu.multiplier.2']
    incident.idu.2[,'hispanic',] = incident.idu.2[,'hispanic',] * parameters['hispanic.incident.idu.multiplier.2']
    incident.idu.2[,'other',] = incident.idu.2[,'other',] * parameters['other.incident.idu.multiplier.2']
    incident.idu.2[,,'msm'] = incident.idu.2[,,'msm'] * parameters['msm.incident.idu.multiplier.2']

    idu.remission = components$idu.transition.elements$idu.remission * parameters['idu.remission.multiplier']
    idu.relapse = components$idu.transition.elements$idu.relapse  * parameters['idu.relapse.multiplier']


    components = set.idu.transitions(components,
                                     indices=1,
                                     incident.idu = incident.idu.0,
                                     idu.remission = idu.remission,
                                     idu.relapse = idu.relapse,
                                     overwrite.elements = F)

    components = set.idu.transitions(components,
                                     indices=2,
                                     incident.idu = incident.idu.2,
                                     idu.remission = idu.remission,
                                     idu.relapse = idu.relapse,
                                     overwrite.elements = F)
  
    components = setup.idu.transition.times(components, 
                                            years=NA,
                                            end.year=NA,
                                            fraction.change.after.last.year=parameters['idu.fraction.trate.change.after.t2'])
    
    #components = setup.idu.mortality(components, parameters['idu.mortality'])

    #-- HIV-Specific Mortality --#

    components = setup.hiv.mortality.rates(components,
                                           r.peak = parameters['peak.hiv.mortality'],
                                           r0 = parameters['hiv.mortality.0'],
                                           r1 = parameters['hiv.mortality.2'],
                                           r2 = parameters['hiv.mortality.2'],
                                           fraction.change.after.end = 0.025)

    #-- Sexual Mixing by Age --#
    heterosexual.male.age.model = components$sexual.transmission$base.heterosexual.male.age.model
    heterosexual.male.age.model['sd.intercept'] = parameters['age.mixing.sd.mult'] *
        heterosexual.male.age.model['sd.intercept']
    heterosexual.male.age.model['sd.slope'] = parameters['age.mixing.sd.mult'] *
        heterosexual.male.age.model['sd.slope']

    female.age.model = components$sexual.transmission$base.female.age.model
    female.age.model['sd.intercept'] = parameters['age.mixing.sd.mult'] *
        female.age.model['sd.intercept']
    female.age.model['sd.slope'] = parameters['age.mixing.sd.mult'] *
        female.age.model['sd.slope']

    msm.age.model = components$sexual.transmission$base.msm.age.model
    msm.age.model['sd.intercept'] = parameters['age.mixing.sd.mult'] *
        msm.age.model['sd.intercept']
    msm.age.model['sd.slope'] = parameters['age.mixing.sd.mult'] *
        msm.age.model['sd.slope']

    components = setup.sex.by.age(components,
                                  heterosexual.male.age.model = heterosexual.male.age.model,
                                  female.age.model = female.age.model,
                                  msm.age.model = msm.age.model)

    #-- Other Sexual Mixing --#

    components = setup.sex.by.sex(components,
                                  fraction.msm.pairings.with.female=components$sexual.transmission$fraction.msm.pairings.with.female,
                                  oe.female.pairings.with.msm=parameters['oe.female.pairings.with.msm'],
                                  fraction.heterosexual.male.pairings.with.male=FRACTION.HET.MALE.PAIRINGS.WITH.MALE)

    components = setup.sex.by.idu(components,
                                  never.idu.sexual.oe=components$never.idu.sexual.oe,
                                  never.with.idu.sexual.oe=parameters['oe.never.idu.pairings.with.idu'],
                                  idu.sexual.oe=components$idu.sexual.oe)

    components = setup.sex.by.race(components,
                                   black.black.oe = parameters['black.black.sexual.oe'],
                                   hispanic.hispanic.oe = parameters['hispanic.hispanic.sexual.oe'],
                                   other.other.oe = parameters['other.other.sexual.oe'])

    #-- Acute HIV and the Effect of Diagnosis --#

    components = setup.transmissibility(components,
                                        acute.transmissibility.rr = parameters['acute.transmissibility.rr'],
                                        diagnosed.needle.sharing.rr = parameters['diagnosed.transmission.rr'],
                                        diagnosed.het.male.condomless.rr = parameters['diagnosed.transmission.rr'],
                                        diagnosed.female.condomless.rr = parameters['diagnosed.transmission.rr'],
                                        diagnosed.msm.condomless.rr = parameters['diagnosed.transmission.rr'],
                                        black.sexual.transmissibility.rr = components$sexual.transmissibility.rr.by.race['black'],
                                        hispanic.sexual.transmissibility.rr = components$sexual.transmissibility.rr.by.race['hispanic'],
                                        black.idu.transmissibility.rr = components$idu.transmissibility.rr.by.race['black'],
                                        hispanic.idu.transmissibility.rr = components$idu.transmissibility.rr.by.race['hispanic']
    )


    #-- Suppression --#

    if (any(names(parameters)=='total.future.suppressed.slope.or'))
        total.future.suppressed.slope.or = parameters['total.future.suppressed.slope.or']
    else
        total.future.suppressed.slope.or = components$background.suppression.inputs$total.future.suppressed.slope.or

    components = set.background.suppression.ors(components,
                                                msm.or.intercept=parameters['msm.suppressed.or'],
                                                heterosexual.or.intercept=parameters['heterosexual.suppressed.or'],
                                                idu.or.intercept=parameters['idu.suppressed.or'],
                                                msm.idu.or.intercept=parameters['msm.idu.suppressed.or'],
                                                black.or.intercept=parameters['black.suppressed.or'],
                                                hispanic.or.intercept=parameters['hispanic.suppressed.or'],
                                                age1.or.intercept=parameters['age1.suppressed.or'],
                                                age2.or.intercept=parameters['age2.suppressed.or'],
                                                age4.or.intercept=parameters['age4.suppressed.or'],
                                                age5.or.intercept=parameters['age5.suppressed.or'],
                                                
                                                msm.or.slope=parameters['msm.suppressed.slope.or'],
                                                heterosexual.or.slope=parameters['heterosexual.suppressed.slope.or'],
                                                idu.or.slope=parameters['idu.suppressed.slope.or'],
                                                msm.idu.or.slope=parameters['msm.idu.suppressed.slope.or'],
                                                black.or.slope=parameters['black.suppressed.slope.or'],
                                                hispanic.or.slope=parameters['hispanic.suppressed.slope.or'],
                                                age1.or.slope=parameters['age1.suppressed.slope.or'],
                                                age2.or.slope=parameters['age2.suppressed.slope.or'],
                                                age4.or.slope=parameters['age4.suppressed.slope.or'],
                                                age5.or.slope=parameters['age5.suppressed.slope.or'])
    
    #-- Smoothing Years --#

    if (any(names(parameters)=='current.gains.end.by.year'))
        components = set.background.change.to.years(components,
                                                    testing.change.to.year=parameters['current.gains.end.by.year'],
                                                    suppression.change.to.year=parameters['current.gains.end.by.year'],
                                                    prep.change.to.year=parameters['current.gains.end.by.year'])

    #-- Return --#
    components
}





##-- CALCULATIONS OF PARAMETERS --##
if (1==2)
{
    ## oe female with msm
    # from pathela 2006, table 1
    # among men who identify as msm or bisexual, what fraction report sex with women?
    # among MSM and MSMV, what fraction are MSMW
    oe = (13700+1800+8500)/(13700+1800+8500 + 175300+65600+3200)
    
    ##Fraction het male with male
    # The number of straight males who report sex with MEN and women /
    # that plus the number of straight males who report sex with women only
    
    ## fraction het male with male - NO LONGER USING THIS
    # from pathela 2006, table 1
    # among men who identify as msm or bisexual, what fraction report sex with women?
    # (count sex with men and women as 0.5)
    #*This calculation takes into account average number of partners
    # so it is basically the fraction of partnerships
    # we take >=3 partners to be 4 partners
    # (and assume that bisexual behavior has the same number of partners as sex with men)
    frac = .5*13700/(13700+1692300)
    
    male.male.partners = 1*.956 + 2*.023 + 4*.021
    male.female.partners = 1*.775 + 2*.08 + 4 * .145
    frac = (206+.5*18)*male.male.partners/((2511+18*.5)*male.female.partners+(206+18*.5)*male.male.partners)
    
    (175300+.5*13700)*male.male.partners/((1692300+13700*.5)*male.female.partners+(175300+13700*.5)*male.male.partners)
    
    #could do this by race, but there is not much difference = 6.9% yo 8.3%
    
    ## oe.never.idu.pairings.with.idu

    frac.last.partner.idu = 0.052 #From Jenness 2010
    prevalence.idu.in.sample = (.269*410+.234*436) / (410+436)#From Jenness 2009
    frac.never.idu.partner = 1-frac.last.partner.idu
    prev.never.idu = 1 - prevalence.idu.in.sample
    oe.never.idu = frac.never.idu.partner / prev.never.idu; oe.never.idu
    oe.never.with.idu = frac.last.partner.idu / prevalence.idu.in.sample; oe.never.with.idu

    ## young.msm.susceptibility
    ## from Glick 2012
    ## The "Partnership formation..." section

    msm.new.partnerships.young.v.old = 78.1 / 41.7 #use the anal partnerships number
    het.new.partnerships.young.v.old = c(55.9,34.1) / c(20.8,9.6)

    msm.new.partnerships.young.v.old
    mean(het.new.partnerships.young.v.old)

    #OLDER:
    #msm.new.partnerships.in.past.year = .86
    #het.new.partnerships.in.past.year = (.56+.34)/2
    #rr = msm.new.partnerships.in.past.year / het.new.partnerships.in.past.year


    #age5 susceptibility
    #based on https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3132270/#!po=56.2500
    # (frequency of sex)
    young.freq = c(6.18,4.68)
    old.freq = c(3.13,1.74)
    mean(old.freq/young.freq)

    #Race-mixing
    sex.by.race.oes = c(ALL.DATA.MANAGERS$pairing$msm.sex.by.race.oe, ALL.DATA.MANAGERS$pairing$het.sex.by.race.oe)
    mean.oe = sex.by.race.oes[[1]]
    for (i in 1:length(sex.by.race.oes))
        mean.oe = mean.oe + sex.by.race.oes[[i]]
    mean.oe = mean.oe / length(sex.by.race.oes)
    mean.oe


    #Trate spike ratios


    prevalence.msa.1992 = c(idu=204000, msm=314400, het=47300)#c(idu=.14, msm=.183, het=0.023) #from Holmberg 1996
    prevalence.msa.2010 = c(idu=73123+46711,msm=375146,het=58341+130622)
    prevalence.msa.2010 = c(idu=60739+34930,msm=327677,het=46095+91513)

    new.national.1985 = c(idu=33000,msm=77000,het=9365) #From Moore
    new.national.2010 = c(idu=3307, msm=25895, het=12032) #from HIV atlas

    new.national.1985 * prevalence.msa.2010 / new.national.2010 / prevalence.msa.1992


    incidence.1992 = c(idu=19000, msm=9800, het=9300)#c(idu=1.5/100, msm=0.7/100, het=0.5/100) #from Holmberg 1996
    prevalence.2010 = c(idu=141600, msm=528700, het=267300)

    incidence.2010 = c(idu=1972+1097,msm=23559,het=3477+6651)
    prevalence.2010 = c(idu=64649+40453,msm=332267,het=50028+112614)

    incidence.2016 = c(idu=1900,msm=26400,het=9100)
    prevalence.2016 = c(idu=131000, msm=648500, het=298700)

    inc.max = c(idu=35000, msm=85000, het=10000)

    i.p.ratio.1992 = incidence.1992/prevalence.1992
    i.p.ratio.2010 = incidence.2010/prevalence.2010
    i.p.ratio.2016 = incidence.2016/prevalence.2016

    i.p.ratio.ratio = i.p.ratio.1992/i.p.ratio.2010


    #Race based RRs

    inc.by.race = c(black=18400, hispanic=9200, white=10900)
    prev.by.race = c(black=340290, hispanic=171228, white=262009)
    ip.ratio = inc.by.race/prev.by.race
    ip.ratio/ip.ratio['white']


    #T-rate ratios

    #From HIV atlas, take 2010 and 2015 data for each state, and
    # for each risk group of <msm, idu, heterosexual>
    # calculate the ratio 2015/2010. Throw away any ratios with a count in either 2010 or 2015 <= 10
    # Excluding the outliers, the ratio ranges from 0.27 to 1.76
    # So we'll take our

    df = read.csv('../code/calibration/state_rf_level_new_2010_2015.csv', stringsAsFactors = F)
    dim.names = list(year=as.character(sort(unique(df$Year))),
                     state=sort(unique(df$Geography)),
                     risk=unique(df$Transmission.Category))
    df$Cases = gsub(',', '', df$Cases)
    arr = array(0.0, dim=sapply(dim.names, length), dimnames=dim.names)
    for (i in 1:dim(df)[1])
    {
        #    print(paste0(df$year[i], ", ", df$Geography[i], ", ", df$Transmission.Category[i]))
        arr[as.character(df$Year[i]), df$Geography[i], df$Transmission.Category[i]] = as.numeric(df$Cases[i])
    }

    #OLDER
    ratio = arr['2015',,] / arr['2010',,]

    min.num = arr['2010',,] > 10 & arr['2015',,] > 10
    ratio = ratio[!is.na(ratio) & min.num & ratio<max(ratio,na.rm=T)]

    qplot(as.numeric(ratio))
    range(ratio, na.rm=T)

    log.ratio = log(ratio)

    qplot(log.ratio)

    qqnorm(ratio)
    qqnorm(log.ratio)
    mean(log.ratio)
    sd(log.ratio)

    RISKS = c('msm','idu','heterosexual')
    ratio = msa.surveillance$new.risk['2015',,RISKS] / msa.surveillance$new.risk['2010',,RISKS]
    min.num = msa.surveillance$new.risk['2015',,RISKS] > 10 & msa.surveillance$new.risk['2010',,RISKS] > 10
    ratio = ratio[!is.na(ratio) & min.num]

    qplot(as.numeric(ratio))
    range(ratio, na.rm=T)
    qqnorm(ratio)

    log.ratio = as.numeric(log(ratio))
    qplot(log.ratio)
    qqnorm(log.ratio)
    mean(log.ratio)
    sd(log.ratio)

    exp(sd(log.ratio)/3)^2 #assume only 1/3 the effect is due to trate changes

    # HIV MORTALITY BY RACE

    aids.fraction = 0.5
    est = c(aids.black=1.34, nonaids.black=1.41, aids.hispanic=1.18, nonaids.hispanic=1.18)
    ci.lower = c(aids.black=1.23, nonaids.black=1.26, aids.hispanic=1.05, nonaids.hispanic=1.03)
    ci.upper = c(aids.black=1.47, nonaids.black=1.57, aids.hispanic=1.32, nonaids.hispanic=1.36)
    ci.span = ci.upper-ci.lower

    #check log-normality
    mean.ci = (ci.lower+ci.upper)/2
    geometric.mean.ci = exp((log(ci.lower)+log(ci.upper))/2)
    cbind(est, mean.ci, geometric.mean.ci)

    log.est = log(est)
    log.ci.span = log(ci.upper) - log(ci.lower)
    log.sd = log.ci.span / 2 / qnorm(.975)

    #averages
    log.est.race = c(black=log.est['aids.black'] * aids.fraction + log.est['nonaids.black'] * (1-aids.fraction),
                     hispanic=log.est['aids.hispanic'] * aids.fraction + log.est['nonaids.hispanic'] * (1-aids.fraction))
    names(log.est.race) = c('black','hispanic')
    log.var.race = c(black = aids.fraction * (log.sd['aids.black']^2 + log.est['aids.black']^2) +
                         (1-aids.fraction) * (log.sd['nonaids.black']^2 + log.est['nonaids.black']^2) -
                         log.est.race['black']^2,
                     hispanic = aids.fraction * (log.sd['aids.hispanic']^2 + log.est['aids.hispanic']^2) +
                         (1-aids.fraction) * (log.sd['nonaids.hispanic']^2 + log.est['nonaids.hispanic']^2) -
                         log.est.race['hispanic']^2
                     )
    log.sd.race = sqrt(log.var.race)

    #check - back to scale
    cbind(pooled.est = exp(log.est.race),
          pooled.ci.lower = exp(log.est.race + log.sd.race * qnorm(.025)),
          pooled.ci.upper = exp(log.est.race + log.sd.race * qnorm(.975)))

    cbind(est, ci.lower, ci.upper)
}
