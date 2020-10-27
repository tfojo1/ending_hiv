
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
TARGET.POPULATION.MANAGER.1.0 = add.target.population(ALL.MSM.AND.IDU.MINUS.YOUNG.BH, code='allminoybhm', name='All MSM 35+ years old or Other race and all active and prior IDU ')

ALL.MSM.AND.ACTIVE.IDU = union.target.populations(ALL.MSM, ALL.ACTIVE.IDU)
TARGET.POPULATION.MANAGER.1.0 = add.target.population(ALL.MSM.AND.ACTIVE.IDU, code='allmai', name='All MSM and all active IDU')

ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH = diff.target.populations(ALL.MSM.AND.ACTIVE.IDU, YOUNG.BLACK.HISPANIC.MSM)
TARGET.POPULATION.MANAGER.1.0 = add.target.population(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH, code='allmainoybhm', name='All MSM 35+ years old or Other race and all active IDU ')

ALL.HETEROSEXUAL.NON.IDU = create.target.population(sexes=c('heterosexual_male','female'), risks='never_IDU')
TARGET.POPULATION.MANAGER.1.0 = add.target.population(ALL.HETEROSEXUAL.NON.IDU, code='het', name='All Heterosexual non-IDU')

ALL.HETEROSEXUAL.NON.ACTIVE.IDU = create.target.population(sexes=c('heterosexual_male','female'), risks=c('IDU_in_remission', 'never_IDU'))
TARGET.POPULATION.MANAGER.1.0 = add.target.population(ALL.HETEROSEXUAL.NON.ACTIVE.IDU, code='hetpri', name='All Heterosexual non-active IDU')


##------------------------##
##-- INTERVENTION UNITS --##
##------------------------##

TESTING.YEARLY.20.21 = create.intervention.unit('testing', 2021, 1, 2022)
TESTING.Q2Y.20.21 = create.intervention.unit('testing', 2021, 0.5, 2022)

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

#---------------------------------------#
#-- Just young Black and Hispanic MSM --#
#---------------------------------------#

#-- Single-aspect Interventions --#

# Testing
YBHM.TQ2 = create.intervention(YOUNG.BLACK.HISPANIC.MSM,
                               TESTING.Q2Y.20.21)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.TQ2, code='ybhm.tq2',
                                                 name='Young Black and Hispanic MSM tested every 2 years')

YBHM.TQ1 = create.intervention(YOUNG.BLACK.HISPANIC.MSM,
                               TESTING.YEARLY.20.21)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.TQ1, code='ybhm.tq1',
                                                 name='Young Black and Hispanic MSM tested yearly')

# PrEP
YBHM.P25 = create.intervention(YOUNG.BLACK.HISPANIC.MSM,
                               PREP.25.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.P25, code='ybhm.p25',
                                                 name='Young Black and Hispanic MSM 25% on PrEP')

YBHM.P50 = create.intervention(YOUNG.BLACK.HISPANIC.MSM,
                               PREP.50.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.P50, code='ybhm.p50',
                                                 name='Young Black and Hispanic MSM 50% on PrEP')

# Suppression
YBHM.S80 = create.intervention(YOUNG.BLACK.HISPANIC.MSM,
                               SUPPRESSION.80.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.S80, code='ybhm.s80',
                                                 name='Young Black and Hispanic MSM 80% suppressed')

YBHM.S90 = create.intervention(YOUNG.BLACK.HISPANIC.MSM,
                               SUPPRESSION.90.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.S90, code='ybhm.s90',
                                                 name='Young Black and Hispanic MSM 90% suppressed')


#-- Multi-aspect Interventions --#

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

#------------------------------------#
#-- Plus all other MSM and all IDU --#
#------------------------------------#

#-- Single-aspect Interventions --#

# Testing
MSM.IDU.TQ2.YBH.TQ1.X = join.interventions(YBHM.TQ1,
                                            create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                                   TESTING.Q2Y.20.21))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.TQ2.YBH.TQ1.X, code='mi.tq2.ybh.tq1.x',
                                                 name='Young Black and Hispanic MSM tested yearly; All other MSM and all IDU tested every 2 years')

MSM.IDU.TQ1.YBH.TQ1.X = join.interventions(YBHM.TQ1,
                                         create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                             TESTING.YEARLY.20.21))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.TQ1.YBH.TQ1.X, code='mi.tq1.ybh.tq1.x',
                                                 name='Young Black and Hispanic MSM tested yearly; All other MSM and all IDU tested every 2 years')


# PrEP
MSM.IDU.P25.YBH.P50.X = join.interventions(YBHM.P50,
                                        create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                                   PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.P25.YBH.P50.X, code='mi.p25.ybh.p50.x',
                                                 name='Young Black and Hispanic MSM 50% on PrEP; All other MSM and all IDU 25% on Prep')

MSM.IDU.P50.YBH.P50.X = join.interventions(YBHM.P50,
                                        create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                            PREP.50.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.P50.YBH.P50.X, code='mi.p50.ybh.p50.x',
                                                 name='All MSM and all IDU 50% on Prep')


# Suppression
MSM.IDU.S80.YBH.S90.X = join.interventions(YBHM.S90,
                                           create.intervention(ALL.MSM.AND.IDU.MINUS.YOUNG.BH,
                                                               SUPPRESSION.80.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.S80.YBH.S90.X, code='mi.s80.ybhm.s90.x',
                                                 name='Young Black and Hispanic MSM 90% suppressed; All other MSM and all IDU 80% suppressed')

MSM.IDU.S90.YBH.S90.X = join.interventions(YBHM.S90,
                                           create.intervention(ALL.MSM.AND.IDU.MINUS.YOUNG.BH,
                                                               SUPPRESSION.90.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.S90.YBH.S90.X, code='mi.s90.ybhm.s90.x',
                                                 name='All MSM and all IDU 90% suppressed')


#-- Multi-Aspect Interventions --#
#--   with non-overlapping target populations --#
MSM.IDU.1.25.80.YBH.HIGH.X = join.interventions(YBHM.1.50.90,
                                              create.intervention(ALL.MSM.AND.IDU.MINUS.YOUNG.BH,
                                                                  SUPPRESSION.80.21.22),
                                              create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                                  TESTING.YEARLY.20.21,
                                                                  PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.1.25.80.YBH.HIGH.X,
                                                 code='mi.1.25.80.ybh.high.x',
                                                 name='Young Black and Hispanic MSM 50% on PrEP, 90% suppressed; all other MSM and all IDU 25% on PrEP, 90% suppressed; all MSM and all IDU tested yearly')

MSM.IDU.1.25.90.YBH.HIGH.X = join.interventions(YBHM.1.50.90,
                                              create.intervention(ALL.MSM.AND.IDU.MINUS.YOUNG.BH,
                                                                  SUPPRESSION.90.21.22),
                                              create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                                  TESTING.YEARLY.20.21,
                                                                  PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.1.25.90.YBH.HIGH.X,
                                                 code='mi.1.25.90.ybh.high.x',
                                                 name='Young Black and Hispanic MSM 50% on PrEP; all other MSM and all IDU 25% on PrEP; all MSM and all IDU tested yearly, 90% suppressed')

MSM.IDU.1.50.80.YBH.HIGH.X = join.interventions(YBHM.1.50.90,
                                              create.intervention(ALL.MSM.AND.IDU.MINUS.YOUNG.BH,
                                                                  SUPPRESSION.80.21.22),
                                              create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                                  TESTING.YEARLY.20.21,
                                                                  PREP.50.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.1.50.80.YBH.HIGH.X,
                                                 code='mi.1.50.80.ybh.high.x',
                                                 name='Young Black and Hispanic MSM 90% suppressed; all other MSM and all IDU 90% suppressed; all MSM and all IDU tested yearly, 50% on PrEP')

MSM.IDU.1.50.90.YBH.HIGH.X = join.interventions(YBHM.1.50.90,
                                              create.intervention(ALL.MSM.AND.IDU.MINUS.YOUNG.BH,
                                                                  SUPPRESSION.90.21.22),
                                              create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                                  TESTING.YEARLY.20.21,
                                                                  PREP.50.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.1.50.90.YBH.HIGH.X,
                                                 code='mi.1.50.90.ybh.high.x',
                                                 name='All MSM and all IDU tested yearly, 50% on PrEP, 90% suppressed')


#-- Multi-Aspect Interventions --#
#--  With potentially overlapping target populations --#
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

#----------------------#
#-- Whole Population --#
#----------------------#

# Testing
HET.TQ2.MI.TQ1.X = join.interventions(MSM.IDU.TQ1.YBH.TQ1.X,
                                           create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                               TESTING.Q2Y.20.21))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.TQ2.MI.TQ1.X, code='het.tq2.mi.tq1.x',
                                                 name='All MSM and all IDU tested yearly; heterosexuals tested every 2 years')

HET.TQ1.MI.TQ1.X = join.interventions(MSM.IDU.TQ1.YBH.TQ1.X,
                                      create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                          TESTING.YEARLY.20.21))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.TQ1.MI.TQ1.X, code='het.tq1.mi.tq1.x',
                                                 name='Whole Population tested yearly')



# PrEP
HET.P10.MI.P50.X = join.interventions(MSM.IDU.P50.YBH.P50.X,
                                           create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                               PREP.10.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.P10.MI.P50.X, code='het.p10.mi.p50.x',
                                                 name='All MSM and all IDU 50% on PrEP; heterosexuals 10% on Prep')

HET.P25.MI.P50.X = join.interventions(MSM.IDU.P50.YBH.P50.X,
                                      create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                          PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.P25.MI.P50.X, code='het.p25.mi.p50.x',
                                                 name='All MSM and all IDU 50% on PrEP; heterosexuals 25% on Prep')


# Suppression
HET.S80.MI.S90.X = join.interventions(MSM.IDU.S90.YBH.S90.X,
                                           create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                               SUPPRESSION.80.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.S80.MI.S90.X, code='het.s80.mi.s90.x',
                                                 name='All MSM and all IDU 90% suppressed; heterosexuals 80% suppressed')

HET.S90.MI.S90.X = join.interventions(MSM.IDU.S90.YBH.S90.X,
                                      create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                          SUPPRESSION.90.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.S90.MI.S90.X, code='het.s90.mi.s90.x',
                                                 name='All PWH 90% suppressed')


#-- Multi-Aspect Interventions --#
#--   with non-overlapping target populations --#
HET.1.10.80.MI.HIGH.X = join.interventions(MSM.IDU.1.50.90.YBH.HIGH.X,
                                              create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                                  SUPPRESSION.80.21.22),
                                              create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                                  TESTING.YEARLY.20.21,
                                                                  PREP.10.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.10.80.MI.HIGH.X,
                                                 code='het.1.10.80.mi.high.x',
                                                 name='All MSM and all IDU 50% on PrEP, 90% suppressed; heterosexuals 10% on PrEP, 80% suppressed; Everyone tested yearly')


HET.1.10.90.MI.HIGH.X = join.interventions(MSM.IDU.1.50.90.YBH.HIGH.X,
                                         create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                             SUPPRESSION.90.21.22),
                                         create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                             TESTING.YEARLY.20.21,
                                                             PREP.10.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.10.90.MI.HIGH.X,
                                                 code='het.1.10.90.mi.high.x',
                                                 name='All MSM and all IDU 50% on PrEP; heterosexuals 10% on PrEP; All demographic subgroups tested yearly and 90% suppressed')


HET.1.25.80.MI.HIGH.X = join.interventions(MSM.IDU.1.50.90.YBH.HIGH.X,
                                         create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                             SUPPRESSION.80.21.22),
                                         create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                             TESTING.YEARLY.20.21,
                                                             PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.25.80.MI.HIGH.X,
                                                 code='het.1.25.80.mi.high.x',
                                                 name='All MSM and all IDU 50% on PrEP, 90% suppressed; heterosexuals 25% on PrEP, 80% suppressed; All demographic subgroups tested yearly')


HET.1.25.90.MI.HIGH.X = join.interventions(MSM.IDU.1.50.90.YBH.HIGH.X,
                                         create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                             SUPPRESSION.90.21.22),
                                         create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                             TESTING.YEARLY.20.21,
                                                             PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.25.90.MI.HIGH.X,
                                                 code='het.1.25.90.mi.high.x',
                                                 name='All MSM and all IDU 50% on PrEP; heterosexuals 25% on PrEP; All demographic subgroups tested yearly and 90% suppressed')



#-- Multi-Aspect Interventions --#
#--  With potentially overlapping target populations --#
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
