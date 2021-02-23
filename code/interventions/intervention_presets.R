
#source('code/setup/default_jheem_settings.R')
#source('code/interventions/create_standard_intervention_presets.R')

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



##-----------------------------##
##-- SOME INTERVENTION UNITS --##
##-----------------------------##

TESTING.YEARLY.21.22 = create.intervention.unit('testing', 2021, 1, 2022)
TESTING.Q2Y.21.22 = create.intervention.unit('testing', 2021, 0.5, 2022)
TESTING.Q6M.21.22 = create.intervention.unit('testing', 2021, 2, 2022)

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

INTERVENTION.MANAGER.1.0 = register.standard.interventions(start.year=2021,
                                                           end.year=2022,
                                                           suffix='',
                                                           INTERVENTION.MANAGER=INTERVENTION.MANAGER.1.0)

INTERVENTION.MANAGER.1.0 = register.standard.interventions(start.year=2021,
                                                           end.year=2024,
                                                           suffix='3y',
                                                           INTERVENTION.MANAGER=INTERVENTION.MANAGER.1.0)