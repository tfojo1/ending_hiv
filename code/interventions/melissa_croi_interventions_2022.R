
source('code/source_code.R')
source('code/interventions/intervention_presets.R')
source('code/interventions/interventions.R')
source('code/interventions/intervention_units.R')

# Intervention units
LINKAGE.90 = create.intervention.unit(type = "linkage", rates = .9, start.year = 2023, years = 2027)
LINKAGE.95 = create.intervention.unit(type = "linkage", rates = .95, start.year = 2023, years = 2027)

RETENTION.SUPP.80 = create.intervention.unit(type = "retention.suppressed", rates = .8, start.year = 2023, years = 2027)
RETENTION.UNSUPP.80 = create.intervention.unit(type = "retention.unsuppressed", rates = .8, start.year = 2023, years = 2027)
RETENTION.SUPP.90 = create.intervention.unit(type = "retention.suppressed", rates = .9, start.year = 2023, years = 2027)
RETENTION.UNSUPP.90 = create.intervention.unit(type = "retention.unsuppressed", rates = .9, start.year = 2023, years = 2027)

ADHERENCE.SUPP.90 = create.intervention.unit(type = "art.adherence.suppressed", rates = .9, start.year = 2023, years = 2027)
ADHERENCE.UNSUPP.90 = create.intervention.unit(type = "art.adherence.unsuppressed", rates = .9, start.year = 2023, years = 2027)
ADHERENCE.SUPP.95 = create.intervention.unit(type = "art.adherence.suppressed", rates = .95, start.year = 2023, years = 2027)
ADHERENCE.UNSUPP.95 = create.intervention.unit(type = "art.adherence.unsuppressed", rates = .95, start.year = 2023, years = 2027)



# Interventions
# Linkage
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


# Retention (combining suppressed/unsuppressed retention)
YBHMSM.RS80 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, RETENTION.SUPP.80)
YBHMSM.RU80 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, RETENTION.UNSUPP.80)
YBHMSM.R80 = join.interventions(YBHMSM.RS80, YBHMSM.RU80)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHMSM.R80, code = "ybhmsm.r80",
                                                 name = 'Young Black and Hispanic MSM 80% retention')

YBHMSM.RS90 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, RETENTION.SUPP.90)
YBHMSM.RU90 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, RETENTION.UNSUPP.90)
YBHMSM.R90 = join.interventions(YBHMSM.RS90, YBHMSM.RU90)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHMSM.R90, code = "ybhmsm.r90",
                                                 name = 'Young Black and Hispanic MSM 90% retention')

MSMIDU.RS80 = create.intervention(ALL.MSM.AND.IDU, RETENTION.SUPP.80)
MSMIDU.RU80 = create.intervention(ALL.MSM.AND.IDU, RETENTION.UNSUPP.80)
MSMIDU.R80 = join.interventions(MSMIDU.RS80, MSMIDU.RU80)
INTERVENTION.MANAGER.1.0 = register.intervention(MSMIDU.R80, code = "msmidu.r80",
                                                 name = 'All MSM and IDU 80% retention')

MSMIDU.RS90 = create.intervention(ALL.MSM.AND.IDU, RETENTION.SUPP.90)
MSMIDU.RU90 = create.intervention(ALL.MSM.AND.IDU, RETENTION.UNSUPP.90)
MSMIDU.R90 = join.interventions(MSMIDU.RS90, MSMIDU.RU90)
INTERVENTION.MANAGER.1.0 = register.intervention(MSMIDU.R90, code = "msmidu.r90",
                                                 name = 'All MSM and IDU 90% retention')

WHOLEPOP.RS80 = create.intervention(WHOLE.POPULATION, RETENTION.SUPP.80)
WHOLEPOP.RU80 = create.intervention(WHOLE.POPULATION, RETENTION.UNSUPP.80)
WHOLEPOP.R80 = join.interventions(WHOLEPOP.RS80, WHOLEPOP.RU80)
INTERVENTION.MANAGER.1.0 = register.intervention(WHOLEPOP.R80, code = "wholepop.r80",
                                                 name = 'All MSM, IDU, and heterosexual 80% retention')

WHOLEPOP.RS90 = create.intervention(WHOLE.POPULATION, RETENTION.SUPP.90)
WHOLEPOP.RU90 = create.intervention(WHOLE.POPULATION, RETENTION.UNSUPP.90)
WHOLEPOP.R90 = join.interventions(WHOLEPOP.RS90, WHOLEPOP.RU90)
INTERVENTION.MANAGER.1.0 = register.intervention(WHOLEPOP.R90, code = "wholepop.r90",
                                                 name = 'All MSM, IDU, and heterosexual 90% retention')


# Adherence (combining suppressed/unsuppressed retention)
YBHMSM.AS90 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, ADHERENCE.SUPP.90)
YBHMSM.AU90 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, ADHERENCE.UNSUPP.90)
YBHMSM.A90 = join.interventions(YBHMSM.AS90, YBHMSM.AU90)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHMSM.A90, code = "ybhmsm.a90",
                                                 name = 'Young Black and Hispanic MSM 90% adherence')

YBHMSM.AS95 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, ADHERENCE.SUPP.95)
YBHMSM.AU95 = create.intervention(YOUNG.BLACK.HISPANIC.MSM, ADHERENCE.UNSUPP.95)
YBHMSM.A95 = join.interventions(YBHMSM.AS95, YBHMSM.AU95)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHMSM.A95, code = "ybhmsm.a95",
                                                 name = 'Young Black and Hispanic MSM 95% adherence')

MSMIDU.AS90 = create.intervention(ALL.MSM.AND.IDU, ADHERENCE.SUPP.90)
MSMIDU.AU90 = create.intervention(ALL.MSM.AND.IDU, ADHERENCE.UNSUPP.90)
MSMIDU.A90 = join.interventions(MSMIDU.AS90, MSMIDU.AU90)
INTERVENTION.MANAGER.1.0 = register.intervention(MSMIDU.A90, code = "msmidu.a90",
                                                 name = 'All MSM and IDU 90% adherence')

MSMIDU.AS95 = create.intervention(ALL.MSM.AND.IDU, ADHERENCE.SUPP.95)
MSMIDU.AU95 = create.intervention(ALL.MSM.AND.IDU, ADHERENCE.UNSUPP.95)
MSMIDU.A95 = join.interventions(MSMIDU.AS95, MSMIDU.AU95)
INTERVENTION.MANAGER.1.0 = register.intervention(MSMIDU.A95, code = "msmidu.a95",
                                                 name = 'All MSM and IDU 95% adherence')

WHOLEPOP.AS90 = create.intervention(WHOLE.POPULATION, ADHERENCE.SUPP.90)
WHOLEPOP.AU90 = create.intervention(WHOLE.POPULATION, ADHERENCE.UNSUPP.90)
WHOLEPOP.A90 = join.interventions(WHOLEPOP.AS90, WHOLEPOP.AU90)
INTERVENTION.MANAGER.1.0 = register.intervention(WHOLEPOP.A90, code = "wholepop.a90",
                                                 name = 'All MSM, IDU, and heterosexual 90% adherence')

WHOLEPOP.AS95 = create.intervention(WHOLE.POPULATION, ADHERENCE.SUPP.95)
WHOLEPOP.AU95 = create.intervention(WHOLE.POPULATION, ADHERENCE.UNSUPP.95)
WHOLEPOP.A95 = join.interventions(WHOLEPOP.AS95, WHOLEPOP.AU95)
INTERVENTION.MANAGER.1.0 = register.intervention(WHOLEPOP.A95, code = "wholepop.a95",
                                                 name = 'All MSM, IDU, and heterosexual 95% adherence')

# Combined
YBHMSM.C.LOW = join.interventions(YBHMSM.L90, YBHMSM.R80, YBHMSM.A90)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHMSM.C.LOW, code = "ybhmsm.c.low",
                                                 name = 'Young Black and Hispanic MSM combined linkage/retention/adherence, low')

YBHMSM.C.HIGH = join.interventions(YBHMSM.L95, YBHMSM.R90, YBHMSM.A95)
INTERVENTION.MANAGER.1.0 = register.intervention(YBHMSM.C.HIGH, code = "ybhmsm.c.high",
                                                 name = 'Young Black and Hispanic MSM combined linkage/retention/adherence, high')

MSMIDU.C.LOW = join.interventions(MSMIDU.L90, MSMIDU.R80, MSMIDU.A90)
INTERVENTION.MANAGER.1.0 = register.intervention(MSMIDU.C.LOW, code = "msmidu.c.low",
                                                 name = 'All MSM and IDU combined linkage/retention/adherence, low')

MSMIDU.C.HIGH = join.interventions(MSMIDU.L95, MSMIDU.R90, MSMIDU.A95)
INTERVENTION.MANAGER.1.0 = register.intervention(MSMIDU.C.HIGH, code = "msmidu.c.high",
                                                 name = 'All MSM and IDU combined linkage/retention/adherence, high')

WHOLEPOP.C.LOW = join.interventions(WHOLEPOP.L90, WHOLEPOP.R80, WHOLEPOP.A90)
INTERVENTION.MANAGER.1.0 = register.intervention(WHOLEPOP.C.LOW, code = "wholepop.c.low",
                                                 name = 'All MSM, IDU, and heterosexual combined linkage/retention/adherence, low')

WHOLEPOP.C.HIGH = join.interventions(WHOLEPOP.L95, WHOLEPOP.R90, WHOLEPOP.A95)
INTERVENTION.MANAGER.1.0 = register.intervention(WHOLEPOP.C.HIGH, code = "wholepop.c.high",
                                                 name = 'All MSM, IDU, and heterosexual combined linkage/retention/adherence, high')

MELISSA.CROI.INTERVENTIONS.2022 = list(YBHMSM.L90, YBHMSM.L95, MSMIDU.L90, MSMIDU.L95, WHOLEPOP.L90, WHOLEPOP.L95,
                                       YBHMSM.R80, YBHMSM.R90, MSMIDU.R80, MSMIDU.R90, WHOLEPOP.R80, WHOLEPOP.R90,
                                       YBHMSM.A90, YBHMSM.A95, MSMIDU.A90, MSMIDU.A95, WHOLEPOP.A90, WHOLEPOP.A95,
                                       YBHMSM.C.LOW, YBHMSM.C.HIGH, MSMIDU.C.LOW, MSMIDU.C.HIGH, WHOLEPOP.C.LOW, WHOLEPOP.C.HIGH,
                                       NO.INTERVENTION)



