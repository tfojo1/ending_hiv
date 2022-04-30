
source('code/interventions/intervention_presets.R')

#### New analysis (PrEP vs. suppression) - 11/19/20 ####
YOUNG.MSM = create.target.population(ages=1:2, sexes = 'msm')
TARGET.POPULATION.MANAGER.1.0 = add.target.population(YOUNG.MSM, code='ymsm', name='MSM and MSM-IDU <35 years old')

GROUP1 = YOUNG.MSM
GROUP2 = ALL.MSM

# Intervention units
P10 = create.intervention.unit(type = "prep", rates = .1, start.year = 2021, years = 2022)
P25 = create.intervention.unit(type = "prep", rates = .25, start.year = 2021, years = 2022)
P50 = create.intervention.unit(type = "prep", rates = .5, start.year = 2021, years = 2022)
S80 = create.intervention.unit(type = "suppression", rates = .8, start.year = 2021, years = 2022)
S85 = create.intervention.unit(type = "suppression", rates = .85, start.year = 2021, years = 2022)
S90 = create.intervention.unit(type = "suppression", rates = .9, start.year = 2021, years = 2022)

# Interventions
# Young MSM
YMSM.P10 = create.intervention(GROUP1, P10)
INTERVENTION.MANAGER.1.0 = register.intervention(YMSM.P10, code = "ymsm.p10", 
                                                 name = 'Young MSM 10% on PrEP')
YMSM.P25 = create.intervention(GROUP1, P25)
INTERVENTION.MANAGER.1.0 = register.intervention(YMSM.P25, code = "ymsm.p25", 
                                                 name = 'Young MSM 25% on PrEP')
YMSM.P50 = create.intervention(GROUP1, P50)
INTERVENTION.MANAGER.1.0 = register.intervention(YMSM.P50, code = "ymsm.p50", 
                                                 name = 'Young MSM 50% on PrEP')

YMSM.S80 = create.intervention(GROUP1, S80)
INTERVENTION.MANAGER.1.0 = register.intervention(YMSM.S80, code = "ymsm.s80", 
                                                 name = 'Young MSM 80% suppressed')
YMSM.S85 = create.intervention(GROUP1, S85)
INTERVENTION.MANAGER.1.0 = register.intervention(YMSM.S85, code = "ymsm.s85", 
                                                 name = 'Young MSM 85% suppressed')
YMSM.S90 = create.intervention(GROUP1, S90)
INTERVENTION.MANAGER.1.0 = register.intervention(YMSM.S90, code = "ymsm.s90", 
                                                 name = 'Young MSM 90% suppressed')

# All MSM
MSM.P10 = create.intervention(GROUP2, P10)
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.P10, code = "msm.p10", 
                                                 name = 'All MSM 10% on PrEP')
MSM.P25 = create.intervention(GROUP2, P25)
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.P25, code = "msm.p25", 
                                                 name = 'All MSM 25% on PrEP')
MSM.P50 = create.intervention(GROUP2, P50)
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.P50, code = "msm.p50", 
                                                 name = 'All MSM 50% on PrEP')

MSM.S80 = create.intervention(GROUP2, S80)
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.S80, code = "msm.s80", 
                                                 name = 'All MSM 80% suppressed')
MSM.S85 = create.intervention(GROUP2, S85)
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.S85, code = "msm.s85", 
                                                 name = 'All MSM 85% suppressed')
MSM.S90 = create.intervention(GROUP2, S90)
INTERVENTION.MANAGER.1.0 = register.intervention(MSM.S90, code = "msm.s90", 
                                                 name = 'All MSM 90% suppressed')


# Combined 
YMSM.P25.MSM.S90 = join.interventions(YMSM.P25, MSM.S90)
INTERVENTION.MANAGER.1.0 = register.intervention(YMSM.P25.MSM.S90, code = "ymsm.p25.msm.s90", 
                                                 name = 'Young MSM 25% on PrEP; All MSM 90% suppressed')

YMSM.P25.MSM.S80 = join.interventions(YMSM.P25, MSM.S80)
INTERVENTION.MANAGER.1.0 = register.intervention(YMSM.P25.MSM.S80, code = "ymsm.p25.msm.s80", 
                                                 name = 'Young MSM 25% on PrEP; All MSM 80% suppressed')

YMSM.P25.MSM.S85 = join.interventions(YMSM.P25, MSM.S85)
INTERVENTION.MANAGER.1.0 = register.intervention(YMSM.P25.MSM.S85, code = "ymsm.p25.msm.s85", 
                                                 name = 'Young MSM 25% on PrEP; All MSM 85% suppressed')

YMSM.P50.MSM.S90 = join.interventions(YMSM.P50, MSM.S90)
INTERVENTION.MANAGER.1.0 = register.intervention(YMSM.P50.MSM.S90, code = "ymsm.p50.msm.s90", 
                                                 name = 'Young MSM 50% on PrEP; All MSM 90% suppressed')


MELISSA.INTERVENTIONS.2 = list(YMSM.P10, YMSM.P25, YMSM.P50, YMSM.S80, YMSM.S85, YMSM.S90, 
                               MSM.P10, MSM.P25, MSM.P50, MSM.S80, MSM.S85, MSM.S90,
                               YMSM.P25.MSM.S80, YMSM.P25.MSM.S85, YMSM.P50.MSM.S90, NO.INTERVENTION)


# MELISSA.INTERVENTIONS.3 = list(YMSM.P25.MSM.S80, YMSM.P25.MSM.S85, YMSM.P50.MSM.S90, NO.INTERVENTION)

#### Original analysis - 11/9/20 ####
GROUP1.SUPP = YOUNG.BLACK.HISPANIC.MSM
GROUP1.PT = YOUNG.BLACK.HISPANIC.MSM
GROUP2.SUPP = ALL.MSM.AND.IDU
GROUP2.PT = ALL.MSM.AND.ACTIVE.IDU
GROUP3.SUPP = ALL.HETEROSEXUAL.NON.IDU
GROUP3.PT = ALL.HETEROSEXUAL.NON.ACTIVE.IDU

S90 = create.intervention.unit(type = "suppression", rates = .9, start.year = 2021, years = 2022)
T1 = create.intervention.unit(type = "testing", rates = 1, start.year = 2021, years = 2022)
P25 = create.intervention.unit(type = "prep", rates = .25, start.year = 2021, years = 2022)
P50 = create.intervention.unit(type = "prep", rates = .5, start.year = 2021, years = 2022)

INT.G1T = create.intervention(GROUP1.PT, T1)
INT.G2T = create.intervention(GROUP2.PT, T1)
INT.G3T = create.intervention(GROUP3.PT, T1)

INT.G1S = create.intervention(GROUP1.SUPP, S90)
INT.G2S = create.intervention(GROUP2.SUPP, S90)
INT.G3S = create.intervention(GROUP3.SUPP, S90)

INT.G1P = create.intervention(GROUP1.PT, P50)
INT.G2P = create.intervention(GROUP2.PT, P50)
INT.G3P = create.intervention(GROUP3.PT, P25)


## First set of 9: All Group 3 = Testing
#1
INT.TTT = join.interventions(INT.G1T,INT.G2T,INT.G3T)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.TTT, code = "m.ttt", name = "Groups 1, 2, and 3 all testing annually")

#2
INT.TST = join.interventions(INT.G1T,INT.G2S,INT.G3T)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.TST, code = "m.tst", name = "Groups 1 and 3 testing annually; Group 2 suppressed at 90%")

#3
INT.TPT = join.interventions(INT.G1T,INT.G2P,INT.G3T)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.TPT, code = "m.tpt", name = "Groups 1 and 3 testing annually; Group 2 PrEP coverage at 50%")

#4
INT.STT = join.interventions(INT.G1S,INT.G2T,INT.G3T)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.STT, code = "m.stt", name = "Group 1 suppressed at 90%; Groups 2 and 3 testing annually")

#5
INT.SST = join.interventions(INT.G1S,INT.G2S,INT.G3T)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.SST, code = "m.sst", name = "Groups 1 and 2 suppressed at 90%; Group 3 testing annually")

#6
INT.SPT = join.interventions(INT.G1S,INT.G2P,INT.G3T)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.SPT, code = "m.spt", name = "Group 1 suppressed at 90%; Group 2 PrEP coverage at 50%; Group 3 testing annually")

#7
INT.PTT = join.interventions(INT.G1P,INT.G2T,INT.G3T)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.PTT, code = "m.ptt", name = "Group 1 PrEP coverage at 50%; Groups 2 and 3 testing annually")

#8
INT.PST = join.interventions(INT.G1P,INT.G2S,INT.G3T)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.PST, code = "m.pst", name = "Group 1 PrEP coverage at 50%; Group 2 suppressed at 90%; Group 3 testing annually")

#9
INT.PPT = join.interventions(INT.G1P,INT.G2P,INT.G3T)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.PPT, code = "m.ppt", name = "Groups 1 and 2 PrEP coverage at 50%; Group 3 testing annually")




## Second set of 9: All Group 3 = Suppression
#10
INT.TTS = join.interventions(INT.G1T,INT.G2T,INT.G3S)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.TTS, code = "m.tts", name = "Groups 1 and 2 testing annually; Group 3 suppressed at 90%")

#11
INT.TSS = join.interventions(INT.G1T,INT.G2S,INT.G3S)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.TSS, code = "m.tss", name = "Group 1 testing annually; Groups 2 and 3 suppressed at 90%")

#12
INT.TPS = join.interventions(INT.G1T,INT.G2P,INT.G3S)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.TPS, code = "m.tps", name = "Group 1 testing annually; Group 2 PrEP coverage at 50%; Group 3 suppressed at 90%")

#13
INT.STS = join.interventions(INT.G1S,INT.G2T,INT.G3S)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.STS, code = "m.sts", name = "Groups 1 and 3 suppressed at 90%; Group 2 testing annually")

#14
INT.SSS = join.interventions(INT.G1S,INT.G2S,INT.G3S)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.SSS, code = "m.sss", name = "Groups 1, 2, and 3 all suppressed at 90%")

#15
INT.SPS = join.interventions(INT.G1S,INT.G2P,INT.G3S)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.SPS, code = "m.sps", name = "Groups 1 and 3 suppressed at 90%; Group 2 PrEP coverage at 50%")

#16
INT.PTS = join.interventions(INT.G1P,INT.G2T,INT.G3S)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.PTS, code = "m.pts", name = "Group 1 PrEP coverage at 50%; Group 2 testing annually; Group 3 suppressed at 90%")

#17
INT.PSS = join.interventions(INT.G1P,INT.G2S,INT.G3S)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.PSS, code = "m.pss", name = "Group 1 PrEP coverage at 50%; Groups 2 and 3 suppressed at 90%")

#18
INT.PPS = join.interventions(INT.G1P,INT.G2P,INT.G3S)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.PPS, code = "m.pps", name = "Groups 1 and 2 PrEP coverage at 50%; Group 3 suppressed at 90%")




## Third set of 9: All Group 3 = PrEP (at 25% not 50%)
#19
INT.TTP = join.interventions(INT.G1T,INT.G2T,INT.G3P)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.TTP, code = "m.ttp", name = "Groups 1 and 2 testing annually; Group 3 PrEP coverage at 25%")

#20
INT.TSP = join.interventions(INT.G1T,INT.G2S,INT.G3P)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.TSP, code = "m.tsp", name = "Group 1 testing annually; Group 2 suppressed at 90%; Group 3 PrEP coverage at 25%")

#21
INT.TPP = join.interventions(INT.G1T,INT.G2P,INT.G3P)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.TPP, code = "m.tpp", name = "Group 1 testing annually; Group 2 PrEP coverage at 50%; Group 3 PrEP coverage at 25%")

#22
INT.STP = join.interventions(INT.G1S,INT.G2T,INT.G3P)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.STP, code = "m.stp", name = "Group 1 suppressed at 90%; Group 2 testing annually; Group 3 PrEP coverage at 25%")

#23
INT.SSP = join.interventions(INT.G1S,INT.G2S,INT.G3P)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.SSP, code = "m.ssp", name = "Groups 1 and 2 suppressed at 90%; Group 3 PrEP coverage at 25%")

#24
INT.SPP = join.interventions(INT.G1S,INT.G2P,INT.G3P)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.SPP, code = "m.spp", name = "Group 1 suppressed at 90%; Group 2 PrEP coverage at 50%; Group 3 PrEP coverage at 25%")

#25
INT.PTP = join.interventions(INT.G1P,INT.G2T,INT.G3P)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.PTP, code = "m.ptp", name = "Group 1 PrEP coverage at 50%; Group 2 testing annually; Group 3 PrEP coverage at 25%")

#26
INT.PSP = join.interventions(INT.G1P,INT.G2S,INT.G3P)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.PSP, code = "m.psp", name = "Group 1 PrEP coverage at 50%; Group 2 suppressed at 90%; Group 3 PrEP coverage at 25%")

#27
INT.PPP = join.interventions(INT.G1P,INT.G2P,INT.G3P)
INTERVENTION.MANAGER.1.0 = register.intervention(INT.PPP, code = "m.ppp", name = "Groups 1 and 2 PrEP coverage at 50%; Group 3 PrEP coverage at 25%")




MELISSA.INTERVENTIONS = list(INT.TTT, INT.TST, INT.TPT, INT.STT, INT.SST, INT.SPT, INT.PTT, INT.PST, INT.PPT, 
                             INT.TTS, INT.TSS, INT.TPS, INT.STS, INT.SSS, INT.SPS, INT.PTS, INT.PSS, INT.PPS, 
                             INT.TTP, INT.TSP, INT.TPP, INT.STP, INT.SSP, INT.SPP, INT.PTP, INT.PSP, INT.PPP)



