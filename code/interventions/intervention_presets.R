
#source('code/setup/default_jheem_settings.R')

##------------------------##
##-- TARGET POPULATIONS --##
##------------------------##

TARGET.POPULATION.MANAGER.1.0 = create.target.population.manager()

YOUNG.BLACK.HISPANIC.MSM = create.target.population(ages=1:2, races=c('black','hispanic'), sexes = 'msm')
TARGET.POPULATION.MANAGER.1.0 = add.target.population(YOUNG.BLACK.HISPANIC.MSM, code='ybhmsm', name='Black and Hispanic MSM and MSM-IDU <35 years old')

ALL.MSM = create.target.population(sexes='msm')
TARGET.POPULATION.MANAGER.1.0 = add.target.population(ALL.MSM, code='msm', name='All MSM and MSM-IDU')

ALL.ACTIVE.IDU = create.target.population(risks='active_IDU')
TARGET.POPULATION.MANAGER.1.0 = add.target.population(ALL.ACTIVE.IDU, code='aidu', name='All active IDU')

ALL.ACTIVE.AND.PRIOR.IDU = create.target.population(risks=c('active_IDU','IDU_in_remission'))
TARGET.POPULATION.MANAGER.1.0 = add.target.population(ALL.ACTIVE.AND.PRIOR.IDU, code='idu', name='All active and prior IDU')

ALL.MSM.AND.IDU = union.target.populations(ALL.MSM, ALL.ACTIVE.AND.PRIOR.IDU)
TARGET.POPULATION.MANAGER.1.0 = add.target.population(ALL.MSM.AND.IDU, code='allmi', name='All MSM and all active and prior IDU')

ALL.MSM.AND.IDU.MINUS.YOUNG.BH = diff.target.populations(ALL.MSM.AND.IDU, YOUNG.BLACK.HISPANIC.MSM)
TARGET.POPULATION.MANAGER.1.0 = add.target.population(ALL.MSM.AND.IDU.MINUS.YOUNG.BH, code='allminoybhm', name='All MSM and all active and prior IDU except Black and Hispanic MSM and MSM-IDU <35 years old')

ALL.MSM.AND.ACTIVE.IDU = union.target.populations(ALL.MSM, ALL.ACTIVE.IDU)
TARGET.POPULATION.MANAGER.1.0 = add.target.population(ALL.MSM.AND.ACTIVE.IDU, code='allmai', name='All MSM and all active IDU')

ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH = diff.target.populations(ALL.MSM.AND.ACTIVE.IDU, YOUNG.BLACK.HISPANIC.MSM)
TARGET.POPULATION.MANAGER.1.0 = add.target.population(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH, code='allmainoybhm', name='All MSM and all active IDU except Black and Hispanic MSM and MSM-IDU <35 years old')

ALL.HETEROSEXUAL.NON.IDU = create.target.population(sexes=c('heterosexual_male','female'), risks='never_IDU')
TARGET.POPULATION.MANAGER.1.0 = add.target.population(ALL.HETEROSEXUAL.NON.IDU, code='het', name='All Heterosexual non-IDU')

ALL.HETEROSEXUAL.NON.ACTIVE.IDU = create.target.population(sexes=c('heterosexual_male','female'), risks=c('IDU_in_remission', 'never_IDU'))
TARGET.POPULATION.MANAGER.1.0 = add.target.population(ALL.HETEROSEXUAL.NON.ACTIVE.IDU, code='hetpri', name='All Heterosexual non-active IDU')


##------------------------##
##-- INTERVENTION UNITS --##
##------------------------##

TESTING.YEARLY.20.21 = create.intervention.unit('testing', 2021, 1, 2022)

PREP.10.21.22 = create.intervention.unit('prep', 2021, 0.1, 2022)
PREP.25.21.22 = create.intervention.unit('prep', 2021, 0.25, 2022)
PREP.50.21.22 = create.intervention.unit('prep', 2021, 0.5, 2022)

SUPPRESSION.80.21.22 = create.intervention.unit('suppression', 2021, 0.8, 2022)
SUPPRESSION.90.21.22 = create.intervention.unit('suppression', 2021, 0.9, 2022)


##-------------------##
##-- INTERVENTIONS --##
##-------------------##

INTERVENTION.MANAGER.1.0 = create.intervention.manager()

NO.INTERVENTION = create.null.intervention()
INTERVENTION.MANAGER.1.0 = register.intervention(NO.INTERVENTION, code='noint', name='No Intervention')

#-- Just young Black and Hispanic MSM --#
YBHM.1.25.80 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, 
                                   TESTING.YEARLY.20.21,
                                   PREP.25.21.22,
                                   SUPPRESSION.80.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.1.25.80, code='ybhm.1.25.80', 
                                                 name='Young Black and Hispanic MSM tested yearly, 25% on PrEP, 80% suppressed')

YBHM.1.25.90 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, 
                                   TESTING.YEARLY.20.21,
                                   PREP.25.21.22,
                                   SUPPRESSION.90.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.1.25.90, code='ybhm.1.25.90', 
                                                 name='Young Black and Hispanic MSM tested yearly, 25% on PrEP, 90% suppressed')

YBHM.1.50.80 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, 
                                   TESTING.YEARLY.20.21,
                                   PREP.50.21.22,
                                   SUPPRESSION.80.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.1.50.80, code='ybhm.1.50.80', 
                                                 name='Young Black and Hispanic MSM tested yearly, 50% on PrEP, 80% suppressed')

YBHM.1.50.90 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, 
                                   TESTING.YEARLY.20.21,
                                   PREP.50.21.22,
                                   SUPPRESSION.90.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.1.50.90, code='ybhm.1.50.90', 
                                                 name='Young Black and Hispanic MSM tested yearly, 50% on PrEP, 90% suppressed')

#-- Plus all other MSM and all IDU --#
MSM.IDU.1.25.80.YBH.HIGH = join.interventions(YBHM.1.50.90,
                                              create.intervention(ALL.MSM.AND.IDU,
                                                                  SUPPRESSION.80.21.22),
                                              create.intervention(ALL.MSM.AND.ACTIVE.IDU,
                                                                  TESTING.YEARLY.20.21,
                                                                  PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.1.25.80.YBH.HIGH,
                                                 code='mi.1.25.80.ybh.high',
                                                 name='Young Black and Hispanic MSM 50% on PrEP, 90% suppressed; all other MSM and all IDU 25% on PrEP, 90% suppressed; all MSM and all IDU tested yearly')

MSM.IDU.1.25.90.YBH.HIGH = join.interventions(YBHM.1.50.90,
                                              create.intervention(ALL.MSM.AND.IDU,
                                                                  SUPPRESSION.90.21.22),
                                              create.intervention(ALL.MSM.AND.ACTIVE.IDU,
                                                                  TESTING.YEARLY.20.21,
                                                                  PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.1.25.90.YBH.HIGH,
                                                 code='mi.1.25.90.ybh.high',
                                                 name='Young Black and Hispanic MSM 50% on PrEP; all other MSM and all IDU 25% on PrEP; all MSM and all IDU tested yearly, 90% suppressed')

MSM.IDU.1.50.80.YBH.HIGH = join.interventions(YBHM.1.50.90,
                                              create.intervention(ALL.MSM.AND.IDU,
                                                                  SUPPRESSION.80.21.22),
                                              create.intervention(ALL.MSM.AND.ACTIVE.IDU,
                                                                  TESTING.YEARLY.20.21,
                                                                  PREP.50.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.1.50.80.YBH.HIGH,
                                                 code='mi.1.50.80.ybh.high',
                                                 name='Young Black and Hispanic MSM 90% suppressed; all other MSM and all IDU 90% suppressed; all MSM and all IDU tested yearly, 50% on PrEP')

MSM.IDU.1.50.90.YBH.HIGH = join.interventions(YBHM.1.50.90,
                                              create.intervention(ALL.MSM.AND.IDU,
                                                                  SUPPRESSION.90.21.22),
                                              create.intervention(ALL.MSM.AND.ACTIVE.IDU,
                                                                  TESTING.YEARLY.20.21,
                                                                  PREP.50.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.1.50.90.YBH.HIGH,
                                                 code='mi.1.50.90.ybh.high',
                                                 name='All MSM and all IDU tested yearly, 50% on PrEP, 90% suppressed')

#-- Whole Population --#

HET.1.10.80.MI.HIGH = join.interventions(MSM.IDU.1.50.90.YBH.HIGH,
                                              create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                                  SUPPRESSION.80.21.22),
                                              create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                                  TESTING.YEARLY.20.21,
                                                                  PREP.10.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.10.80.MI.HIGH,
                                                 code='het.1.10.80.mi.high',
                                                 name='All MSM and all IDU 50% on PrEP, 90% suppressed; heterosexuals 10% on PrEP, 80% suppressed; Everyone tested yearly')


HET.1.10.90.MI.HIGH = join.interventions(MSM.IDU.1.50.90.YBH.HIGH,
                                         create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                             SUPPRESSION.90.21.22),
                                         create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                             TESTING.YEARLY.20.21,
                                                             PREP.10.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.10.90.MI.HIGH,
                                                 code='het.1.10.90.mi.high',
                                                 name='All MSM and all IDU 50% on PrEP; heterosexuals 10% on PrEP; All demographic subgroups tested yearly and 90% suppressed')


HET.1.25.80.MI.HIGH = join.interventions(MSM.IDU.1.50.90.YBH.HIGH,
                                         create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                             SUPPRESSION.80.21.22),
                                         create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                             TESTING.YEARLY.20.21,
                                                             PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.25.80.MI.HIGH,
                                                 code='het.1.25.80.mi.high',
                                                 name='All MSM and all IDU 50% on PrEP, 90% suppressed; heterosexuals 25% on PrEP, 80% suppressed; All demographic subgroups tested yearly')


HET.1.25.90.MI.HIGH = join.interventions(MSM.IDU.1.50.90.YBH.HIGH,
                                         create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                             SUPPRESSION.90.21.22),
                                         create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                             TESTING.YEARLY.20.21,
                                                             PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.25.90.MI.HIGH,
                                                 code='het.1.25.90.mi.high',
                                                 name='All MSM and all IDU 50% on PrEP; heterosexuals 25% on PrEP; All demographic subgroups tested yearly and 90% suppressed')
