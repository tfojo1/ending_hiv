source('code/source_code.R')

# Interventions: 
    # (1) PrEP only - 2 levels (10%, 25%)
    # (2) Linkage only - 2 levels (90%, 95%) - will probably remove
    # (2) Retention only - 2 levels (90%, 95%)
    # (3) Suppression only - 2 levels (90%, 95%)
    # (4) Combined without PrEP - 2 levels (low/high) - includes linkage for now
    # (5) Combined with PrEP - 2 levels (low/high) - includes linkage for now

# Populations: 
    # Young Black/Hispanic MSM
    # All MSM, All PWID 
    # Full population (All MSM, All PWID, All heterosexual)

# Intervention units
    # scale is one of 'proportion', 'time', 'rate'
    # type needs to match the names in expanded_continuum_jheem_settings.R in the calls to "register.transition.element'

P10 = create.intervention.unit(type = "prep", rates = .1, scale='proportion', start.year = 2023, years = 2027)
P25 = create.intervention.unit(type = "prep", rates = .25, scale='proportion', start.year = 2023, years = 2027)

# Will probably remove linkage 
LINKAGE.90 = create.intervention.unit(type = "linkage", rates = .9, scale='proportion', start.year = 2023, years = 2027)
LINKAGE.95 = create.intervention.unit(type = "linkage", rates = .95, scale='proportion', start.year = 2023, years = 2027)

RETENTION.RECENT.SUPP.90 = create.intervention.unit(type = "recently.suppressed.to.disengaged", scale='proportion', 
                                                    rates = .1, start.year = 2023, years = 2027)
RETENTION.DURABLE.SUPP.90 = create.intervention.unit(type = "durably.suppressed.to.disengaged", scale='proportion', 
                                                     rates = .1, start.year = 2023, years = 2027)
RETENTION.UNSUPP.90 = create.intervention.unit(type = "failing.to.disengaged", scale='proportion', 
                                               rates = .1, start.year = 2023, years = 2027)
RETENTION.NAIVE.90 = create.intervention.unit(type = "naive.to.disengaged", scale='proportion', 
                                              rates = .1, start.year = 2023, years = 2027)

RETENTION.RECENT.SUPP.95 = create.intervention.unit(type = "recently.suppressed.to.disengaged", scale='proportion', 
                                                    rates = .05, start.year = 2023, years = 2027)
RETENTION.DURABLE.SUPP.95 = create.intervention.unit(type = "durably.suppressed.to.disengaged", scale='proportion', 
                                                     rates = .05, start.year = 2023, years = 2027)
RETENTION.UNSUPP.95 = create.intervention.unit(type = "failing.to.disengaged", scale='proportion', 
                                               rates = .05, start.year = 2023, years = 2027)
RETENTION.NAIVE.95 = create.intervention.unit(type = "naive.to.disengaged", scale='proportion', 
                                              rates = .05, start.year = 2023, years = 2027)


ANNUAL.SUPP.90 = create.intervention.unit(type = "failing.to.suppressed", scale='proportion', 
                                          rates = .9, start.year = 2023, years = 2027)
ANNUAL.SUPP.95 = create.intervention.unit(type = "failing.to.suppressed", scale='proportion', 
                                          rates = .95, start.year = 2023, years = 2027)




#### Interventions ####
#### PrEP ####
YBHMSM.P10 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, P10)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHMSM.P10, code = "ybhmsm.p10",
                                                 name = 'Young Black and Hispanic MSM 10% PrEP')

YBHMSM.P25 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, P25)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHMSM.P25, code = "ybhmsm.p25",
                                                 name = 'Young Black and Hispanic MSM 25% PrEP')

MSMIDU.P10 = create.intervention(ALL.MSM.AND.IDU, P10)
INTERVENTION.MANAGER.1.0 = register.intervention(MSMIDU.P10, code = "msmidu.p10",
                                                 name = 'All MSM and IDU 10% PrEP')

MSMIDU.P25 = create.intervention(ALL.MSM.AND.IDU, P25)
INTERVENTION.MANAGER.1.0 = register.intervention(MSMIDU.P25, code = "msmidu.p25",
                                                 name = 'All MSM and IDU 25% PrEP')

WHOLEPOP.P10 = create.intervention(WHOLE.POPULATION, P10)
INTERVENTION.MANAGER.1.0 = register.intervention(WHOLEPOP.P10, code = "wholepop.p10",
                                                 name = 'All MSM, IDU, and heterosexual 10% PrEP')

WHOLEPOP.P25 = create.intervention(WHOLE.POPULATION, P25)
INTERVENTION.MANAGER.1.0 = register.intervention(WHOLEPOP.P25, code = "wholepop.p25",
                                                 name = 'All MSM, IDU, and heterosexual 25% PrEP')


#### Linkage ####
YBHMSM.L90 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, LINKAGE.90)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHMSM.L90, code = "ybhmsm.l90",
                                                 name = 'Young Black and Hispanic MSM 90% linkage')

YBHMSM.L95 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, LINKAGE.95)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHMSM.L95, code = "ybhmsm.l95",
                                                 name = 'Young Black and Hispanic MSM 95% linkage')

MSMIDU.L90 = create.intervention(ALL.MSM.AND.IDU, LINKAGE.90)
INTERVENTION.MANAGER.1.0 = register.intervention(MSMIDU.L90, code = "msmidu.l90",
                                                 name = 'All MSM and IDU 90% linkage')

MSMIDU.L95 = create.intervention(ALL.MSM.AND.IDU, LINKAGE.95)
INTERVENTION.MANAGER.1.0 = register.intervention(MSMIDU.L95, code = "msmidu.l95",
                                                 name = 'All MSM and IDU 95% linkage')

WHOLEPOP.L90 = create.intervention(WHOLE.POPULATION, LINKAGE.90)
INTERVENTION.MANAGER.1.0 = register.intervention(WHOLEPOP.L90, code = "wholepop.l90",
                                                 name = 'All MSM, IDU, and heterosexual 90% linkage')

WHOLEPOP.L95 = create.intervention(WHOLE.POPULATION, LINKAGE.95)
INTERVENTION.MANAGER.1.0 = register.intervention(WHOLEPOP.L95, code = "wholepop.l95",
                                                 name = 'All MSM, IDU, and heterosexual 95% linkage')

#### Retention (combining recently suppressed/durably suppressed/failing/naive retention) ####
YBHMSM.RRS90 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, RETENTION.RECENT.SUPP.90)
YBHMSM.RDS90 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, RETENTION.DURABLE.SUPP.90)
YBHMSM.RU90 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, RETENTION.UNSUPP.90)
YBHMSM.RN90 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, RETENTION.NAIVE.90)
YBHMSM.R90 = join.interventions(YBHMSM.RRS90, YBHMSM.RDS90, YBHMSM.RU90, YBHMSM.RN90)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHMSM.R90, code = "ybhmsm.r90",
                                                 name = 'Young Black and Hispanic MSM 90% retention')

YBHMSM.RRS95 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, RETENTION.RECENT.SUPP.95)
YBHMSM.RDS95 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, RETENTION.DURABLE.SUPP.95)
YBHMSM.RU95 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, RETENTION.UNSUPP.95)
YBHMSM.RN95 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, RETENTION.NAIVE.95)
YBHMSM.R95 = join.interventions(YBHMSM.RRS95, YBHMSM.RDS95, YBHMSM.RU95, YBHMSM.RN95)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHMSM.R95, code = "ybhmsm.r95",
                                                 name = 'Young Black and Hispanic MSM 95% retention')

MSMIDU.RRS90 = create.intervention(ALL.MSM.AND.IDU, RETENTION.RECENT.SUPP.90)
MSMIDU.RDS90 = create.intervention(ALL.MSM.AND.IDU, RETENTION.DURABLE.SUPP.90)
MSMIDU.RU90 = create.intervention(ALL.MSM.AND.IDU, RETENTION.UNSUPP.90)
MSMIDU.RN90 = create.intervention(ALL.MSM.AND.IDU, RETENTION.NAIVE.90)
MSMIDU.R90 = join.interventions(MSMIDU.RRS90, MSMIDU.RDS90, MSMIDU.RU90, MSMIDU.RN90)
INTERVENTION.MANAGER.1.0 = register.intervention(MSMIDU.R90, code = "msmidu.r90",
                                                 name = 'All MSM and IDU 90% retention')

MSMIDU.RRS95 = create.intervention(ALL.MSM.AND.IDU, RETENTION.RECENT.SUPP.95)
MSMIDU.RDS95 = create.intervention(ALL.MSM.AND.IDU, RETENTION.DURABLE.SUPP.95)
MSMIDU.RU95 = create.intervention(ALL.MSM.AND.IDU, RETENTION.UNSUPP.95)
MSMIDU.RN95 = create.intervention(ALL.MSM.AND.IDU, RETENTION.NAIVE.95)
MSMIDU.R95 = join.interventions(MSMIDU.RRS95, MSMIDU.RDS95, MSMIDU.RU95, MSMIDU.RN95)
INTERVENTION.MANAGER.1.0 = register.intervention(MSMIDU.R95, code = "msmidu.r95",
                                                 name = 'All MSM and IDU 95% retention')

WHOLEPOP.RRS90 = create.intervention(WHOLE.POPULATION, RETENTION.RECENT.SUPP.90)
WHOLEPOP.RDS90 = create.intervention(WHOLE.POPULATION, RETENTION.DURABLE.SUPP.90)
WHOLEPOP.RU90 = create.intervention(WHOLE.POPULATION, RETENTION.UNSUPP.90)
WHOLEPOP.RN90 = create.intervention(WHOLE.POPULATION, RETENTION.NAIVE.90)
WHOLEPOP.R90 = join.interventions(WHOLEPOP.RRS90, WHOLEPOP.RDS90, WHOLEPOP.RU90, WHOLEPOP.RN90)
INTERVENTION.MANAGER.1.0 = register.intervention(WHOLEPOP.R90, code = "wholepop.r90",
                                                 name = 'All MSM, IDU, and heterosexual 90% retention')

WHOLEPOP.RRS95 = create.intervention(WHOLE.POPULATION, RETENTION.RECENT.SUPP.95)
WHOLEPOP.RDS95 = create.intervention(WHOLE.POPULATION, RETENTION.DURABLE.SUPP.95)
WHOLEPOP.RU95 = create.intervention(WHOLE.POPULATION, RETENTION.UNSUPP.95)
WHOLEPOP.RN95 = create.intervention(WHOLE.POPULATION, RETENTION.NAIVE.95)
WHOLEPOP.R95 = join.interventions(WHOLEPOP.RRS95, WHOLEPOP.RDS95, WHOLEPOP.RU95, WHOLEPOP.RN95)
INTERVENTION.MANAGER.1.0 = register.intervention(WHOLEPOP.R95, code = "wholepop.r95",
                                                 name = 'All MSM, IDU, and heterosexual 95% retention')


#### Gain of suppression (failing only) ####
YBHMSM.AS90 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, ANNUAL.SUPP.90)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHMSM.AS90, code = "ybhmsm.as90",
                                                 name = 'Young Black and Hispanic MSM 90% annual suppression')

YBHMSM.AS95 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, ANNUAL.SUPP.95)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHMSM.AS95, code = "ybhmsm.as95",
                                                 name = 'Young Black and Hispanic MSM 95% annual suppression')

MSMIDU.AS90 = create.intervention(ALL.MSM.AND.IDU, ANNUAL.SUPP.90)
INTERVENTION.MANAGER.1.0 = register.intervention(MSMIDU.AS90, code = "msmidu.as90",
                                                 name = 'All MSM and IDU 90% annual suppression')

MSMIDU.AS95 = create.intervention(ALL.MSM.AND.IDU, ANNUAL.SUPP.95)
INTERVENTION.MANAGER.1.0 = register.intervention(MSMIDU.AS95, code = "msmidu.as95",
                                                 name = 'All MSM and IDU 95% annual suppression')

WHOLEPOP.AS90 = create.intervention(WHOLE.POPULATION, ANNUAL.SUPP.90)
INTERVENTION.MANAGER.1.0 = register.intervention(WHOLEPOP.AS90, code = "wholepop.as90",
                                                 name = 'All MSM, IDU, and heterosexual 90% annual suppression')

WHOLEPOP.AS95 = create.intervention(WHOLE.POPULATION, ANNUAL.SUPP.95)
INTERVENTION.MANAGER.1.0 = register.intervention(WHOLEPOP.AS95, code = "wholepop.as95",
                                                 name = 'All MSM, IDU, and heterosexual 95% annual suppression')


#### Combined (without PrEP) - INCLUDES LINKAGE #### 
YBHMSM.C.LOW = join.interventions(YBHMSM.L90, YBHMSM.R90, YBHMSM.AS90)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHMSM.C.LOW, code = "ybhmsm.c.low",
                                                 name = 'Young Black and Hispanic MSM combined linkage/retention/suppression, low')

YBHMSM.C.HIGH = join.interventions(YBHMSM.L95, YBHMSM.R95, YBHMSM.AS95)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHMSM.C.HIGH, code = "ybhmsm.c.high",
                                                 name = 'Young Black and Hispanic MSM combined linkage/retention/suppression, high')

MSMIDU.C.LOW = join.interventions(MSMIDU.L90, MSMIDU.R90, MSMIDU.AS90)
INTERVENTION.MANAGER.1.0 = register.intervention(MSMIDU.C.LOW, code = "msmidu.c.low",
                                                 name = 'All MSM and IDU combined linkage/retention/suppression, low')

MSMIDU.C.HIGH = join.interventions(MSMIDU.L95, MSMIDU.R95, MSMIDU.AS95)
INTERVENTION.MANAGER.1.0 = register.intervention(MSMIDU.C.HIGH, code = "msmidu.c.high",
                                                 name = 'All MSM and IDU combined linkage/retention/suppression, high')

WHOLEPOP.C.LOW = join.interventions(WHOLEPOP.L90, WHOLEPOP.R90, WHOLEPOP.AS90)
INTERVENTION.MANAGER.1.0 = register.intervention(WHOLEPOP.C.LOW, code = "wholepop.c.low",
                                                 name = 'All MSM, IDU, and heterosexual combined linkage/retention/suppression, low')

WHOLEPOP.C.HIGH = join.interventions(WHOLEPOP.L95, WHOLEPOP.R95, WHOLEPOP.AS95)
INTERVENTION.MANAGER.1.0 = register.intervention(WHOLEPOP.C.HIGH, code = "wholepop.c.high",
                                                 name = 'All MSM, IDU, and heterosexual combined linkage/retention/suppression, high')



#### Combined (with PrEP) - INCLUDES LINKAGE #### 
YBHMSM.C.P.LOW = join.interventions(YBHMSM.P10, YBHMSM.L90, YBHMSM.R90, YBHMSM.AS90)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHMSM.C.P.LOW, code = "ybhmsm.c.p.low",
                                                 name = 'Young Black and Hispanic MSM combined PrEP/linkage/retention/suppression, low')

YBHMSM.C.P.HIGH = join.interventions(YBHMSM.P25, YBHMSM.L95, YBHMSM.R95, YBHMSM.AS95)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHMSM.C.P.HIGH, code = "ybhmsm.c.p.high",
                                                 name = 'Young Black and Hispanic MSM combined PrEP/linkage/retention/suppression, high')

MSMIDU.C.P.LOW = join.interventions(MSMIDU.P10, MSMIDU.L90, MSMIDU.R90, MSMIDU.AS90)
INTERVENTION.MANAGER.1.0 = register.intervention(MSMIDU.C.P.LOW, code = "msmidu.c.p.low",
                                                 name = 'All MSM and IDU combined PrEP/linkage/retention/suppression, low')

MSMIDU.C.P.HIGH = join.interventions(MSMIDU.P25, MSMIDU.L95, MSMIDU.R95, MSMIDU.AS95)
INTERVENTION.MANAGER.1.0 = register.intervention(MSMIDU.C.P.HIGH, code = "msmidu.c.p.high",
                                                 name = 'All MSM and IDU combined PrEP/linkage/retention/suppression, high')

WHOLEPOP.C.P.LOW = join.interventions(WHOLEPOP.P10, WHOLEPOP.L90, WHOLEPOP.R90, WHOLEPOP.AS90)
INTERVENTION.MANAGER.1.0 = register.intervention(WHOLEPOP.C.P.LOW, code = "wholepop.c.p.low",
                                                 name = 'All MSM, IDU, and heterosexual combined PrEP/linkage/retention/suppression, low')

WHOLEPOP.C.P.HIGH = join.interventions(WHOLEPOP.P25, WHOLEPOP.L95, WHOLEPOP.R95, WHOLEPOP.AS95)
INTERVENTION.MANAGER.1.0 = register.intervention(WHOLEPOP.C.P.HIGH, code = "wholepop.c.p.high",
                                                 name = 'All MSM, IDU, and heterosexual combined PrEP/linkage/retention/suppression, high')



EXPANDED.CONTINUUM.INTERVENTIONS = list(YBHMSM.P10, YBHMSM.P25, MSMIDU.P10, MSMIDU.P25, WHOLEPOP.P10, WHOLEPOP.P25,
                                        YBHMSM.L90, YBHMSM.L95, MSMIDU.L90, MSMIDU.L95, WHOLEPOP.L90, WHOLEPOP.L95,
                                        YBHMSM.R90, YBHMSM.R95, MSMIDU.R90, MSMIDU.R95, WHOLEPOP.R90, WHOLEPOP.R95,
                                        YBHMSM.AS90, YBHMSM.AS95, MSMIDU.AS90, MSMIDU.AS95, WHOLEPOP.AS90, WHOLEPOP.AS95,
                                        YBHMSM.C.LOW, YBHMSM.C.HIGH, MSMIDU.C.LOW, MSMIDU.C.HIGH, WHOLEPOP.C.LOW, WHOLEPOP.C.HIGH,
                                        YBHMSM.C.P.LOW, YBHMSM.C.P.HIGH, MSMIDU.C.P.LOW, MSMIDU.C.P.HIGH, WHOLEPOP.C.P.LOW, WHOLEPOP.C.P.HIGH,
                                        NO.INTERVENTION)
