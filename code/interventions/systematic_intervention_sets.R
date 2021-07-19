

##------------------------------##
##-- DEFINE INTERVENTION SETS --##
##------------------------------##

MAIN.INTERVENTION.CODES = c(
    # Marginal
    'all.marginal.t125.p05.s10',
    
    # Single-modality
    'mi.t2x_het.t1x',
    'all.p25',
    'all.s90',
    
    # YBHM Only
    'ybhm.t1x.p10.s80',
    'ybhm.t2x.p10.s80',
    'ybhm.t2x.p25.s80',
    'ybhm.t2x.p25.s90',
    
    # MI Only
    'mi.t1x.p10.s80',
    'mi.t2x.p10.s80',
    'mi.t2x.p25.s80',
    'mi.t2x.p25.s90',
    
    # All
    'mi.t1x.p10.s80_het.t05x.p10.s80',
    'mi.t2x.p10.s80_het.t1x.p10.s80',
    'mi.t2x.p25.s80_het.t1x.p25.s80',
    'mi.t2x.p25.s90_het.t1x.p25.s90'
)

SINGLE.MODALITY.INTERVENTION.CODES = c(
    
    #Testing
    'ybhm.t1x',
    'ybhm.t2x',
    'mi.t1x',
    'mi.t2x',
    'mi.t1x_het.t05x',
    'mi.t2x_het.t1x',
    
    #PrEP
    'ybhm.p10',
    'ybhm.p25',
    'mi.p10',
    'mi.p25',
    'all.p10',
    'all.p25',
    
    #Suppression
    'ybhm.s80',
    'ybhm.s90',
    'mi.s80',
    'mi.s90',
    'all.s80',
    'all.s90'
)

IDU.INTERVENTION.CODES = c(
    'idu.n10',
    'idu.n25',
    'idu.moud10',
    'idu.moud25',
    'non.idu.p10_idu.n10.moud10',
    'non.idu.p25_idu.n25.moud25'
)



MAIN.INTERVENTIONS.23.27 = c(list(NO.INTERVENTION),
                               lapply(paste0(MAIN.INTERVENTION.CODES, '_23.27'), intervention.from.code))

MAIN.INTERVENTIONS.23.25 = c(list(NO.INTERVENTION),
                             lapply(paste0(MAIN.INTERVENTION.CODES, '_23.25'), intervention.from.code))

SINGLE.MODALITY.INTERVENTIONS.23.27 = c(list(NO.INTERVENTION),
                             lapply(paste0(SINGLE.MODALITY.INTERVENTION.CODES, '_23.27'), intervention.from.code))


IDU.INTERVENTIONS.23.27 = c(list(NO.INTERVENTION),
                             lapply(paste0(IDU.INTERVENTION.CODES, '_23.27'), intervention.from.code))

IDU.INTERVENTIONS.PLUS.23.27 = c(IDU.INTERVENTIONS.23.27,
                                 lapply(paste0(c('all.p10','all.p25'), '_23.27'), intervention.from.code))


ALL.INTERVENTION.CODES = unique(c(
    'noint',
    
    paste0(MAIN.INTERVENTION.CODES, "_23.27"),
    paste0(MAIN.INTERVENTION.CODES, "_23.25"),
    
    paste0(SINGLE.MODALITY.INTERVENTION.CODES, "_23.27"),
    
    paste0(IDU.INTERVENTION.CODES, "_23.27")
))

ALL.INTERVENTIONS = lapply(ALL.INTERVENTION.CODES, intervention.from.code)



#OLDER VERSIONS 
if (1==2)
{
    
    A1.INTERVENTION.CODES = c('het.tq1.mi.tq6m',
                              'het.s90.mi.s90.x',
                              'het.p25.mi.p50.x',
                              'ybhm.6m.25.80',
                              'ybhm.6m.25.90',
                              'ybhm.6m.50.80',
                              'ybhm.6m.50.90',
                              'mi.6m.25.80.ybh.high6',
                              'mi.6m.25.90.ybh.high6',
                              'mi.6m.50.80.ybh.high6',
                              'mi.6m.50.90.ybh.high6',
                              'het.1.10.80.mi.high6',
                              'het.1.10.90.mi.high6',
                              'het.1.25.80.mi.high6',
                              'het.1.25.90.mi.high6')
    
    A2.INTERVENTION.CODES = c('ybhm.tq1',
                              'ybhm.tq6m',
                              'ybhm.s80',
                              'ybhm.s90',
                              'ybhm.p25',
                              'ybhm.p50',
                              'mi.tq1.ybh.tq1.x',
                              'mi.tq6m.ybh.tq6m',
                              'mi.s80.ybhm.s80.x',
                              'mi.s90.ybhm.s90.x',
                              'mi.p25.ybh.p25.x',
                              'mi.p50.ybh.p50.x',
                              'het.tq2.mi.tq1.x',
                              'het.tq1.mi.tq6m',
                              'het.s80.mi.s80.x',
                              'het.s90.mi.s90.x',
                              'het.p10.mi.p25.x',
                              'het.p25.mi.p50.x')
    
    
    A1.INTERVENTION.SET.1Y = c(list(NO.INTERVENTION),
                               lapply(A1.INTERVENTION.CODES, intervention.from.code))
    
    A2.INTERVENTION.SET.1Y = c(list(NO.INTERVENTION),
                               lapply(A2.INTERVENTION.CODES, intervention.from.code))
    
    ALL.INTERVENTIONS.1Y = union.intervention.lists(A1.INTERVENTION.SET.1Y,
                                                    A2.INTERVENTION.SET.1Y)
    
    
    
    A1.INTERVENTION.SET.3Y = c(list(NO.INTERVENTION),
                               lapply(paste0(A1.INTERVENTION.CODES, '.3y'), intervention.from.code))
    
    A2.INTERVENTION.SET.3Y = c(list(NO.INTERVENTION),
                               lapply(paste0(A2.INTERVENTION.CODES, '.3y'), intervention.from.code))
    
    ALL.INTERVENTIONS.3Y = union.intervention.lists(A1.INTERVENTION.SET.3Y,
                                                    A2.INTERVENTION.SET.3Y)
    
    
    A1.INTERVENTION.SET = A1.INTERVENTION.SET.3Y
    A2.INTERVENTION.SET = A2.INTERVENTION.SET.3Y
    ALL.INTERVENTIONS = ALL.INTERVENTIONS.3Y
    
    
    # For legacy interventions
    OLD.A2.INTERVENTION.CODES = c('ybhm.tq1',
                                  'ybhm.tq6m',
                                  'ybhm.s80',
                                  'ybhm.s90',
                                  'ybhm.p25',
                                  'ybhm.p50',
                                  'mi.tq1.ybh.tq6m',
                                  'mi.tq6m.ybh.tq6m',
                                  'mi.s80.ybhm.s90.x',
                                  'mi.s90.ybhm.s90.x',
                                  'mi.p25.ybh.p50.x',
                                  'mi.p50.ybh.p50.x',
                                  'het.tq2.mi.tq6m',
                                  'het.tq1.mi.tq6m',
                                  'het.s80.mi.s90.x',
                                  'het.s90.mi.s90.x',
                                  'het.p10.mi.p50.x',
                                  'het.p25.mi.p50.x')
    
    OLD.A2.TO.REMOVE = setdiff(OLD.A2.INTERVENTION.CODES, A2.INTERVENTION.CODES)
    NEW.A2.TO.ADD = setdiff(A2.INTERVENTION.CODES, OLD.A2.INTERVENTION.CODES)
    
    
    OLD.A2.TO.REMOVE.SET.3Y = lapply(paste0(OLD.A2.TO.REMOVE, '.3y'), intervention.from.code)
    NEW.A2.TO.ADD.SET.3Y = lapply(paste0(NEW.A2.TO.ADD, '.3y'), intervention.from.code)
}