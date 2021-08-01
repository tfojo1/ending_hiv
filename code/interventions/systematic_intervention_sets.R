

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

APPROX959595.CODES = c(
    'all.t05x.p25.s90',
    'all.p25.s90'
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

APPROX959595.INTERVENTIONS.23.25 = lapply(paste0(APPROX959595.CODES, '_23.25'), intervention.from.code)

WEB.TOOL.INTERVENTION.CODES = unique(c(
    'noint',
    
    paste0(MAIN.INTERVENTION.CODES, "_23.27"),
    paste0(MAIN.INTERVENTION.CODES, "_23.25"),
    
    paste0(SINGLE.MODALITY.INTERVENTION.CODES, "_23.27"),
    
    paste0(IDU.INTERVENTION.CODES, "_23.27")
))

WEB.TOOL.INTERVENTIONS = lapply(WEB.TOOL.INTERVENTION.CODES, intervention.from.code)

ALL.INTERVENTION.CODES = unique(c(
    WEB.TOOL.INTERVENTION.CODES,    
    paste0(APPROX959595.CODES, '_23.25')
))

ALL.INTERVENTIONS = lapply(ALL.INTERVENTION.CODES, intervention.from.code)

# A check here
if (any(sapply(ALL.INTERVENTIONS, is.null)))
    stop("Error in specifying ALL.INTERVENTIONS")
