


create.standard.interventions <- function(start.year=2021,
                                          end.year=2022,
                                          suffix='',
                                          INTERVENTION.MANAGER=INTERVENTION.MANAGER.1.0)
{
    if (suffix != '' && substr(suffix, 1,1)!='.')
        suffix = paste0(".", suffix)
    
    ##------------------------##
    ##-- INTERVENTION UNITS --##
    ##------------------------##
    
    TESTING.YEARLY = create.intervention.unit('testing', start.year, 1, end.year)
    TESTING.Q2Y = create.intervention.unit('testing', start.year, 0.5, end.year)
    TESTING.Q6M = create.intervention.unit('testing', start.year, 2, end.year)
    
    PREP.10 = create.intervention.unit('prep', start.year, 0.1, end.year)
    PREP.25 = create.intervention.unit('prep', start.year, 0.25, end.year)
    PREP.50 = create.intervention.unit('prep', start.year, 0.5, end.year)
    
    SUPPRESSION.80 = create.intervention.unit('suppression', start.year, 0.8, end.year)
    SUPPRESSION.90 = create.intervention.unit('suppression', start.year, 0.9, end.year)
    

#---------------------------------------#
#-- Just young Black and Hispanic MSM --#
#---------------------------------------#

#-- Single-aspect Interventions --#

# Testing
YBHM.TQ2 = create.intervention(YOUNG.BLACK.HISPANIC.MSM,
                               TESTING.Q2Y)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.TQ2, code=paste0('ybhm.tq2', suffix),
                                                 name='Young Black and Hispanic MSM tested every 2 years')

YBHM.TQ1 = create.intervention(YOUNG.BLACK.HISPANIC.MSM,
                               TESTING.YEARLY.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.TQ1, code=paste0('ybhm.tq1', suffix),
                                                 name='Young Black and Hispanic MSM tested yearly')

YBHM.TQ6M = create.intervention(YOUNG.BLACK.HISPANIC.MSM,
                                TESTING.Q6M.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.TQ6M, code=paste0('ybhm.tq6m', suffix),
                                                 name='Young Black and Hispanic MSM tested every 6 months')

# PrEP
YBHM.P25 = create.intervention(YOUNG.BLACK.HISPANIC.MSM,
                               PREP.25.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.P25, code=paste0('ybhm.p25', suffix),
                                                 name='Young Black and Hispanic MSM 25% on PrEP')

YBHM.P50 = create.intervention(YOUNG.BLACK.HISPANIC.MSM,
                               PREP.50.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.P50, code=paste0('ybhm.p50', suffix),
                                                 name='Young Black and Hispanic MSM 50% on PrEP')

# Suppression
YBHM.S80 = create.intervention(YOUNG.BLACK.HISPANIC.MSM,
                               SUPPRESSION.80.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.S80, code=paste0('ybhm.s80', suffix),
                                                 name='Young Black and Hispanic MSM 80% suppressed')

YBHM.S90 = create.intervention(YOUNG.BLACK.HISPANIC.MSM,
                               SUPPRESSION.90.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.S90, code=paste0('ybhm.s90', suffix),
                                                 name='Young Black and Hispanic MSM 90% suppressed')


#-- Multi-aspect Interventions --#

#testing yearly
YBHM.1.25.80 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, 
                                   TESTING.YEARLY.21.22,
                                   PREP.25.21.22,
                                   SUPPRESSION.80.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.1.25.80, code=paste0('ybhm.1.25.80', suffix), 
                                                 name='Young Black and Hispanic MSM tested yearly, 25% on PrEP, 80% suppressed')

YBHM.1.25.90 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, 
                                   TESTING.YEARLY.21.22,
                                   PREP.25.21.22,
                                   SUPPRESSION.90.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.1.25.90, code=paste0('ybhm.1.25.90', suffix), 
                                                 name='Young Black and Hispanic MSM tested yearly, 25% on PrEP, 90% suppressed')

YBHM.1.50.80 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, 
                                   TESTING.YEARLY.21.22,
                                   PREP.50.21.22,
                                   SUPPRESSION.80.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.1.50.80, code='ybhm.1.50.80', 
                                                 name='Young Black and Hispanic MSM tested yearly, 50% on PrEP, 80% suppressed')

YBHM.1.50.90 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, 
                                   TESTING.YEARLY.21.22,
                                   PREP.50.21.22,
                                   SUPPRESSION.90.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.1.50.90, code='ybhm.1.50.90', 
                                                 name='Young Black and Hispanic MSM tested yearly, 50% on PrEP, 90% suppressed')

# testing q6mo
YBHM.6M.25.80 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, 
                                    TESTING.Q6M.21.22,
                                    PREP.25.21.22,
                                    SUPPRESSION.80.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.6M.25.80, code='ybhm.6m.25.80', 
                                                 name='Young Black and Hispanic MSM tested every 6 months, 25% on PrEP, 80% suppressed')

YBHM.6M.25.90 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, 
                                    TESTING.Q6M.21.22,
                                    PREP.25.21.22,
                                    SUPPRESSION.90.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.6M.25.90, code='ybhm.6m.25.90', 
                                                 name='Young Black and Hispanic MSM tested every 6 months, 25% on PrEP, 90% suppressed')

YBHM.6M.50.80 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, 
                                    TESTING.Q6M.21.22,
                                    PREP.50.21.22,
                                    SUPPRESSION.80.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.6M.50.80, code='ybhm.6m.50.80', 
                                                 name='Young Black and Hispanic MSM tested every 6 months, 50% on PrEP, 80% suppressed')

YBHM.6M.50.90 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, 
                                    TESTING.Q6M.21.22,
                                    PREP.50.21.22,
                                    SUPPRESSION.90.21.22)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHM.6M.50.90, code='ybhm.6m.50.90', 
                                                 name='Young Black and Hispanic MSM tested every 6 months, 50% on PrEP, 90% suppressed')

#------------------------------------#
#-- Plus all other MSM and all IDU --#
#------------------------------------#

#-- Single-aspect Interventions --#

# Testing
MSM.IDU.TQ2.YBH.TQ1.X = join.interventions(YBHM.TQ1,
                                           create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                               TESTING.Q2Y.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.TQ2.YBH.TQ1.X, code='mi.tq2.ybh.tq1.x',
                                                 name='Young Black and Hispanic MSM tested yearly; All other MSM and all IDU tested every 2 years')

MSM.IDU.TQ1.YBH.TQ1.X = join.interventions(YBHM.TQ1,
                                           create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                               TESTING.YEARLY.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.TQ1.YBH.TQ1.X, code='mi.tq1.ybh.tq1.x',
                                                 name='All MSM and all IDU tested yearly')


MSM.IDU.TQ1.YBH.TQ6M = join.interventions(YBHM.TQ6M,
                                          create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                              TESTING.YEARLY.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.TQ1.YBH.TQ6M, code='mi.tq1.ybh.tq6m',
                                                 name='Young Black and Hispanic MSM tested every 6 months; All other MSM and all IDU tested yearly')

MSM.IDU.TQ6M.YBH.TQ6M = join.interventions(YBHM.TQ6M,
                                           create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                               TESTING.Q6M.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.TQ6M.YBH.TQ6M, code='mi.tq6m.ybh.tq6m',
                                                 name='All MSM and all IDU tested every 6 months')



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

# based on yearly testing
MSM.IDU.1.25.80.YBH.HIGH.X = join.interventions(YBHM.1.50.90,
                                                create.intervention(ALL.MSM.AND.IDU.MINUS.YOUNG.BH,
                                                                    SUPPRESSION.80.21.22),
                                                create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                                    TESTING.YEARLY.21.22,
                                                                    PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.1.25.80.YBH.HIGH.X,
                                                 code='mi.1.25.80.ybh.high.x',
                                                 name='Young Black and Hispanic MSM 50% on PrEP, 90% suppressed; all other MSM and all IDU 25% on PrEP, 90% suppressed; all MSM and all IDU tested yearly')

MSM.IDU.1.25.90.YBH.HIGH.X = join.interventions(YBHM.1.50.90,
                                                create.intervention(ALL.MSM.AND.IDU.MINUS.YOUNG.BH,
                                                                    SUPPRESSION.90.21.22),
                                                create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                                    TESTING.YEARLY.21.22,
                                                                    PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.1.25.90.YBH.HIGH.X,
                                                 code='mi.1.25.90.ybh.high.x',
                                                 name='Young Black and Hispanic MSM 50% on PrEP; all other MSM and all IDU 25% on PrEP; all MSM and all IDU tested yearly, 90% suppressed')

MSM.IDU.1.50.80.YBH.HIGH.X = join.interventions(YBHM.1.50.90,
                                                create.intervention(ALL.MSM.AND.IDU.MINUS.YOUNG.BH,
                                                                    SUPPRESSION.80.21.22),
                                                create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                                    TESTING.YEARLY.21.22,
                                                                    PREP.50.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.1.50.80.YBH.HIGH.X,
                                                 code='mi.1.50.80.ybh.high.x',
                                                 name='Young Black and Hispanic MSM 90% suppressed; all other MSM and all IDU 90% suppressed; all MSM and all IDU tested yearly, 50% on PrEP')

MSM.IDU.1.50.90.YBH.HIGH.X = join.interventions(YBHM.1.50.90,
                                                create.intervention(ALL.MSM.AND.IDU.MINUS.YOUNG.BH,
                                                                    SUPPRESSION.90.21.22),
                                                create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                                    TESTING.YEARLY.21.22,
                                                                    PREP.50.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.1.50.90.YBH.HIGH.X,
                                                 code='mi.1.50.90.ybh.high.x',
                                                 name='All MSM and all IDU tested yearly, 50% on PrEP, 90% suppressed')


# based on q6mo testing
MSM.IDU.6M.25.80.YBH.HIGH6 = join.interventions(YBHM.6M.50.90,
                                                create.intervention(ALL.MSM.AND.IDU.MINUS.YOUNG.BH,
                                                                    SUPPRESSION.80.21.22),
                                                create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                                    TESTING.Q6M.21.22,
                                                                    PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.6M.25.80.YBH.HIGH6,
                                                 code='mi.6m.25.80.ybh.high6',
                                                 name='Young Black and Hispanic MSM 50% on PrEP, 90% suppressed; all other MSM and all IDU 25% on PrEP, 90% suppressed; all MSM and all IDU tested every 6 months')

MSM.IDU.6M.25.90.YBH.HIGH6 = join.interventions(YBHM.6M.50.90,
                                                create.intervention(ALL.MSM.AND.IDU.MINUS.YOUNG.BH,
                                                                    SUPPRESSION.90.21.22),
                                                create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                                    TESTING.Q6M.21.22,
                                                                    PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.6M.25.90.YBH.HIGH6,
                                                 code='mi.6m.25.90.ybh.high6',
                                                 name='Young Black and Hispanic MSM 50% on PrEP; all other MSM and all IDU 25% on PrEP; all MSM and all IDU tested every 6 months, 90% suppressed')

MSM.IDU.6M.50.80.YBH.HIGH6 = join.interventions(YBHM.6M.50.90,
                                                create.intervention(ALL.MSM.AND.IDU.MINUS.YOUNG.BH,
                                                                    SUPPRESSION.80.21.22),
                                                create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                                    TESTING.Q6M.21.22,
                                                                    PREP.50.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.6M.50.80.YBH.HIGH6,
                                                 code='mi.6m.50.80.ybh.high6',
                                                 name='Young Black and Hispanic MSM 90% suppressed; all other MSM and all IDU 90% suppressed; all MSM and all IDU tested every 6 months, 50% on PrEP')

MSM.IDU.6M.50.90.YBH.HIGH6 = join.interventions(YBHM.6M.50.90,
                                                create.intervention(ALL.MSM.AND.IDU.MINUS.YOUNG.BH,
                                                                    SUPPRESSION.90.21.22),
                                                create.intervention(ALL.MSM.AND.ACTIVE.IDU.MINUS.YOUNG.BH,
                                                                    TESTING.Q6M.21.22,
                                                                    PREP.50.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.6M.50.90.YBH.HIGH6,
                                                 code='mi.6m.50.90.ybh.high6',
                                                 name='All MSM and all IDU tested every 6 months, 50% on PrEP, 90% suppressed')


#----------------------#
#-- Whole Population --#
#----------------------#

# Testing
HET.TQ2.MI.TQ1.X = join.interventions(MSM.IDU.TQ1.YBH.TQ1.X,
                                      create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                          TESTING.Q2Y.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.TQ2.MI.TQ1.X, code='het.tq2.mi.tq1.x',
                                                 name='All MSM and all IDU tested yearly; heterosexuals tested every 2 years')

HET.TQ1.MI.TQ1.X = join.interventions(MSM.IDU.TQ1.YBH.TQ1.X,
                                      create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                          TESTING.YEARLY.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.TQ1.MI.TQ1.X, code='het.tq1.mi.tq1.x',
                                                 name='Whole Population tested yearly')


HET.TQ2.MI.TQ6M = join.interventions(MSM.IDU.TQ6M.YBH.TQ6M,
                                     create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                         TESTING.Q2Y.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.TQ2.MI.TQ6M, code='het.tq2.mi.tq6m',
                                                 name='All MSM and all IDU tested every 6 months; heterosexuals tested every 2 years')

HET.TQ1.MI.TQ6M = join.interventions(MSM.IDU.TQ6M.YBH.TQ6M,
                                     create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                         TESTING.YEARLY.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.TQ1.MI.TQ6M, code='het.tq1.mi.tq6m',
                                                 name='All MSM and all IDU tested every 6 months; heterosexuals tested yearly')


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

#based on yearly testing
HET.1.10.80.MI.HIGH.X = join.interventions(MSM.IDU.1.50.90.YBH.HIGH.X,
                                           create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                               SUPPRESSION.80.21.22),
                                           create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                               TESTING.YEARLY.21.22,
                                                               PREP.10.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.10.80.MI.HIGH.X,
                                                 code='het.1.10.80.mi.high.x',
                                                 name='All MSM and all IDU 50% on PrEP, 90% suppressed; heterosexuals 10% on PrEP, 80% suppressed; Everyone tested yearly')


HET.1.10.90.MI.HIGH.X = join.interventions(MSM.IDU.1.50.90.YBH.HIGH.X,
                                           create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                               SUPPRESSION.90.21.22),
                                           create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                               TESTING.YEARLY.21.22,
                                                               PREP.10.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.10.90.MI.HIGH.X,
                                                 code='het.1.10.90.mi.high.x',
                                                 name='All MSM and all IDU 50% on PrEP; heterosexuals 10% on PrEP; All demographic subgroups tested yearly and 90% suppressed')


HET.1.25.80.MI.HIGH.X = join.interventions(MSM.IDU.1.50.90.YBH.HIGH.X,
                                           create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                               SUPPRESSION.80.21.22),
                                           create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                               TESTING.YEARLY.21.22,
                                                               PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.25.80.MI.HIGH.X,
                                                 code='het.1.25.80.mi.high.x',
                                                 name='All MSM and all IDU 50% on PrEP, 90% suppressed; heterosexuals 25% on PrEP, 80% suppressed; All demographic subgroups tested yearly')


HET.1.25.90.MI.HIGH.X = join.interventions(MSM.IDU.1.50.90.YBH.HIGH.X,
                                           create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                               SUPPRESSION.90.21.22),
                                           create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                               TESTING.YEARLY.21.22,
                                                               PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.25.90.MI.HIGH.X,
                                                 code='het.1.25.90.mi.high.x',
                                                 name='All MSM and all IDU 50% on PrEP; heterosexuals 25% on PrEP; All demographic subgroups tested yearly and 90% suppressed')


#based on q6mo testing
HET.1.10.80.MI.HIGH6 = join.interventions(MSM.IDU.6M.50.90.YBH.HIGH6,
                                          create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                              SUPPRESSION.80.21.22),
                                          create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                              TESTING.YEARLY.21.22,
                                                              PREP.10.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.10.80.MI.HIGH6,
                                                 code='het.1.10.80.mi.high6',
                                                 name='All MSM and all IDU 50% on PrEP, 90% suppressed, tested every 6 months; heterosexuals 10% on PrEP, 80% suppressed tested yearly')


HET.1.10.90.MI.HIGH6 = join.interventions(MSM.IDU.6M.50.90.YBH.HIGH6,
                                          create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                              SUPPRESSION.90.21.22),
                                          create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                              TESTING.YEARLY.21.22,
                                                              PREP.10.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.10.90.MI.HIGH6,
                                                 code='het.1.10.90.mi.high6',
                                                 name='All MSM and all IDU 50% on PrEP and tested every 6 months; heterosexuals 10% on PrEP and tested yearly; All demographic subgroups 90% suppressed')


HET.1.25.80.MI.HIGH6 = join.interventions(MSM.IDU.6M.50.90.YBH.HIGH6,
                                          create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                              SUPPRESSION.80.21.22),
                                          create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                              TESTING.YEARLY.21.22,
                                                              PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.25.80.MI.HIGH6,
                                                 code='het.1.25.80.mi.high6',
                                                 name='All MSM and all IDU 50% on PrEP, 90% suppressed, and tested every 6 months; heterosexuals 25% on PrEP, 80% suppressed, and tested yearly')


HET.1.25.90.MI.HIGH6 = join.interventions(MSM.IDU.6M.50.90.YBH.HIGH6,
                                          create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                              SUPPRESSION.90.21.22),
                                          create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                              TESTING.YEARLY.21.22,
                                                              PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.25.90.MI.HIGH6,
                                                 code='het.1.25.90.mi.high6',
                                                 name='All MSM and all IDU 50% on PrEP and tested every 6 months; heterosexuals 25% on PrEP and tested yearly; All demographic subgroups 90% suppressed')





##-----------------------------------------------------------##
##-- LEGACY INTERVENTIONS - to keep backward compatibility --##
##-----------------------------------------------------------##
##

#-- Multi-Aspect Interventions --#
#--  With potentially overlapping target populations --#
MSM.IDU.1.25.80.YBH.HIGH = join.interventions(YBHM.1.50.90,
                                              create.intervention(ALL.MSM.AND.IDU,
                                                                  SUPPRESSION.80.21.22),
                                              create.intervention(ALL.MSM.AND.ACTIVE.IDU,
                                                                  TESTING.YEARLY.21.22,
                                                                  PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.1.25.80.YBH.HIGH,
                                                 code='mi.1.25.80.ybh.high',
                                                 name='Young Black and Hispanic MSM 50% on PrEP, 90% suppressed; all other MSM and all IDU 25% on PrEP, 90% suppressed; all MSM and all IDU tested yearly')

MSM.IDU.1.25.90.YBH.HIGH = join.interventions(YBHM.1.50.90,
                                              create.intervention(ALL.MSM.AND.IDU,
                                                                  SUPPRESSION.90.21.22),
                                              create.intervention(ALL.MSM.AND.ACTIVE.IDU,
                                                                  TESTING.YEARLY.21.22,
                                                                  PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.1.25.90.YBH.HIGH,
                                                 code='mi.1.25.90.ybh.high',
                                                 name='Young Black and Hispanic MSM 50% on PrEP; all other MSM and all IDU 25% on PrEP; all MSM and all IDU tested yearly, 90% suppressed')

MSM.IDU.1.50.80.YBH.HIGH = join.interventions(YBHM.1.50.90,
                                              create.intervention(ALL.MSM.AND.IDU,
                                                                  SUPPRESSION.80.21.22),
                                              create.intervention(ALL.MSM.AND.ACTIVE.IDU,
                                                                  TESTING.YEARLY.21.22,
                                                                  PREP.50.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.1.50.80.YBH.HIGH,
                                                 code='mi.1.50.80.ybh.high',
                                                 name='Young Black and Hispanic MSM 90% suppressed; all other MSM and all IDU 90% suppressed; all MSM and all IDU tested yearly, 50% on PrEP')

MSM.IDU.1.50.90.YBH.HIGH = join.interventions(YBHM.1.50.90,
                                              create.intervention(ALL.MSM.AND.IDU,
                                                                  SUPPRESSION.90.21.22),
                                              create.intervention(ALL.MSM.AND.ACTIVE.IDU,
                                                                  TESTING.YEARLY.21.22,
                                                                  PREP.50.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.IDU.1.50.90.YBH.HIGH,
                                                 code='mi.1.50.90.ybh.high',
                                                 name='All MSM and all IDU tested yearly, 50% on PrEP, 90% suppressed')




#-- Multi-Aspect Interventions --#
#--  With potentially overlapping target populations --#
HET.1.10.80.MI.HIGH = join.interventions(MSM.IDU.1.50.90.YBH.HIGH,
                                         create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                             SUPPRESSION.80.21.22),
                                         create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                             TESTING.YEARLY.21.22,
                                                             PREP.10.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.10.80.MI.HIGH,
                                                 code='het.1.10.80.mi.high',
                                                 name='All MSM and all IDU 50% on PrEP, 90% suppressed; heterosexuals 10% on PrEP, 80% suppressed; Everyone tested yearly')


HET.1.10.90.MI.HIGH = join.interventions(MSM.IDU.1.50.90.YBH.HIGH,
                                         create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                             SUPPRESSION.90.21.22),
                                         create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                             TESTING.YEARLY.21.22,
                                                             PREP.10.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.10.90.MI.HIGH,
                                                 code='het.1.10.90.mi.high',
                                                 name='All MSM and all IDU 50% on PrEP; heterosexuals 10% on PrEP; All demographic subgroups tested yearly and 90% suppressed')


HET.1.25.80.MI.HIGH = join.interventions(MSM.IDU.1.50.90.YBH.HIGH,
                                         create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                             SUPPRESSION.80.21.22),
                                         create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                             TESTING.YEARLY.21.22,
                                                             PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.25.80.MI.HIGH,
                                                 code='het.1.25.80.mi.high',
                                                 name='All MSM and all IDU 50% on PrEP, 90% suppressed; heterosexuals 25% on PrEP, 80% suppressed; All demographic subgroups tested yearly')


HET.1.25.90.MI.HIGH = join.interventions(MSM.IDU.1.50.90.YBH.HIGH,
                                         create.intervention(ALL.HETEROSEXUAL.NON.IDU,
                                                             SUPPRESSION.90.21.22),
                                         create.intervention(ALL.HETEROSEXUAL.NON.ACTIVE.IDU,
                                                             TESTING.YEARLY.21.22,
                                                             PREP.25.21.22))
INTERVENTION.MANAGER.1.0 = register.intervention(HET.1.25.90.MI.HIGH,
                                                 code='het.1.25.90.mi.high',
                                                 name='All MSM and all IDU 50% on PrEP; heterosexuals 25% on PrEP; All demographic subgroups tested yearly and 90% suppressed')


}