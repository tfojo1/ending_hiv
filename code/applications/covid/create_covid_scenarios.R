
library(lubridate)

#-- Dates --#
START.DATE = as.Date('2020-03-01')
EARLY.START.NORMALIZE.DATE = as.Date('2021-03-08')
EARLY.NORMAL.DATE = as.Date('2021-07-04')
LATE.START.NORMALIZE.DATE = EARLY.START.NORMALIZE.DATE %m+% months(6)
LATE.NORMAL.DATE = EARLY.NORMAL.DATE %m+% months(6)

#-- Distributions --#
SEXUAL.TRANSMISSION.DIST = Uniform.Distribution(0,.5)
SUPPRESSION.DIST = Uniform.Distribution(0,0.4)
TESTING.DIST = Uniform.Distribution(0,0.5)
PREP.DIST = Uniform.Distribution(0,0.3)

MOBILITY.WEIGHT.DIST = Beta.Distribution(2,2)


#-- The Scenarios --#
DELAYED.CARE.MOBILITY = create.covid.scenario(mobility.weight.distribution = MOBILITY.WEIGHT.DIST,
                                     sexual.transmission.reduction.distribution = SEXUAL.TRANSMISSION.DIST,
                                     suppression.reduction.distribution = SUPPRESSION.DIST,
                                     testing.reduction.distribution = TESTING.DIST,
                                     prep.reduction.distribution = PREP.DIST,
                                     
                                     index.to.mobility=T,

                                     start.pandemic.effects.time=START.DATE,
                                     
                                     start.sexual.transmission.normalize.time = EARLY.START.NORMALIZE.DATE,
                                     sexual.transmission.normal.time = EARLY.NORMAL.DATE,
                                     
                                     start.suppression.normalize.time = LATE.START.NORMALIZE.DATE,
                                     suppression.normal.time = LATE.NORMAL.DATE)
INTERVENTION.MANAGER.1.0 = register.intervention(DELAYED.CARE.MOBILITY,
                                                 code='covid.delayed.mobility',
                                                 name='Prolonged Barriers to Care')

RAPID.RESUMPTION.MOBILITY = create.covid.scenario(mobility.weight.distribution = MOBILITY.WEIGHT.DIST,
                                                  sexual.transmission.reduction.distribution = SEXUAL.TRANSMISSION.DIST,
                                                  suppression.reduction.distribution = SUPPRESSION.DIST,
                                                  testing.reduction.distribution = TESTING.DIST,
                                                  prep.reduction.distribution = PREP.DIST,
                                                  
                                                  index.to.mobility=T,
                                                  
                                                  start.pandemic.effects.time=START.DATE,
                                                  
                                                  start.sexual.transmission.normalize.time = EARLY.START.NORMALIZE.DATE,
                                                  sexual.transmission.normal.time = EARLY.NORMAL.DATE,
                                                  
                                                  start.suppression.normalize.time = EARLY.START.NORMALIZE.DATE,
                                                  suppression.normal.time = EARLY.NORMAL.DATE)
INTERVENTION.MANAGER.1.0 = register.intervention(RAPID.RESUMPTION.MOBILITY,
                                                 code='covid.rapid.resumption.mobility',
                                                 name='Rapid Resumption of Care')
