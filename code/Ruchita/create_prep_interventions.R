

# This file defines the interventions we will use for PrEP 
#  and registers them to an 'intervention manager'
# Registering them lets the manager know they exist and
#  allows them to be retrieved by name. This is going to
#  help us when we push it to the website

create.prep.interventions <- function(start.year,
                                      implemented.year,
                                      suffix='',
                                      INTERVENTION.MANAGER=INTERVENTION.MANAGER.1.0,
                                      injectable.prep.rr.dist=INJ.PREP.RR.DIST,
                                      oral.prep.rr.dist=ORAL.PREP.RR.DIST)
{
    if (suffix != '' && substr(suffix, 1,1)!='_')
        suffix = paste0("_", suffix)

    #-- DEFINE THE INTERVENTION UNITS --#
    
    PREP.10 = create.intervention.unit(type = "prep", start.year = start.year, 
                                       rates = .1, years = implemented.year)
    PREP.25 = create.intervention.unit(type = "prep", start.year = start.year, 
                                       rates = .25, years = implemented.year)
    PREP.50 = create.intervention.unit(type = "prep", start.year = start.year, 
                             rates = .5, years = implemented.year)
    
    INJECTABLE.PREP = create.intervention.unit(type = "rr.prep", start.year = start.year, 
                                               rates = .34, years = implemented.year, 
                                               apply.function = "multiplier", allow.less.than.otherwise = T)
    
    INJECTABLE.PREP.VARIABLE = create.intervention.unit(type = "rr.prep", start.year = start.year, 
                                                        rates = 'inj.rr', years = implemented.year, 
                                                        apply.function = "multiplier", allow.less.than.otherwise = T)
    
    # Ruchita - use these two intervention blocks to build variable interventions
    ORAL.PREP.VARIABLE = create.intervention.unit(type='rr.prep', start.year=2014,
                                                  rates = 'oral.rr', years=2014.00001,
                                                  apply.function='multiplier', allow.less.than.otherwise = T)
    INJECTABLE.ORAL.PREP.VARIABLE = create.intervention.unit(type='rr.prep', start.year = 2014, 
                                                             rates=c('oral.rr','oral.rr','inj.rr'), 
                                                             years=c(2014.00001, start.year, implemented.year),
                                                             apply.function='multiplier', allow.less.than.otherwise = T)
    
    
    #-- COMBINE TO MAKE SPECIFIC INTERVENTIONS --#
  
    
    MSM.P10 = create.intervention(ALL.MSM, PREP.10)
    INTERVENTION.MANAGER = register.intervention(MSM.P10, code=paste0('msm.p10.oral', suffix),
                                                 name='Immediate 10% of MSM on oral PrEP',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    
    MSM.IP10 = create.intervention(ALL.MSM, PREP.10, INJECTABLE.PREP) 
    INTERVENTION.MANAGER = register.intervention(MSM.IP10, code=paste0('msm.p10.inj', suffix),
                                                 name='Immediate 10% of MSM on long-acting PrEP',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    MSM.P25 = create.intervention(ALL.MSM, PREP.25)
    INTERVENTION.MANAGER = register.intervention(MSM.P25, code=paste0('msm.p25.oral', suffix),
                                                 name='Immediate 25% of MSM on oral PrEP',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    MSM.IP25 = create.intervention(ALL.MSM, PREP.25, INJECTABLE.PREP)
    INTERVENTION.MANAGER = register.intervention(MSM.IP25, code=paste0('msm.p25.inj', suffix),
                                                 name='Immediate 25% of MSM on long-acting PrEP',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    MSM.P50 = create.intervention(ALL.MSM, PREP.50)
    INTERVENTION.MANAGER = register.intervention(MSM.P50, code=paste0('msm.p50.oral', suffix),
                                                 name='Immediate 50% of MSM on oral PrEP',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    MSM.IP50 = create.intervention(ALL.MSM, PREP.50, INJECTABLE.PREP)
    INTERVENTION.MANAGER = register.intervention(MSM.IP50, code=paste0('msm.p50.inj', suffix),
                                                 name='Immediate 50% of MSM on long-acting PrEP',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    
    
    MSM.IP10v = create.intervention(ALL.MSM, PREP.10, INJECTABLE.PREP.VARIABLE,injectable.prep.rr.dist)
    INTERVENTION.MANAGER = register.intervention(MSM.IP10v, code=paste0('msm.p10.inj.variable', suffix),
                                                 name='Immediate 10% of MSM on long-acting PrEP variable',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    MSM.IP25v = create.intervention(ALL.MSM, PREP.25, INJECTABLE.PREP.VARIABLE,injectable.prep.rr.dist)
    INTERVENTION.MANAGER = register.intervention(MSM.IP25v, code=paste0('msm.p25.inj.variable', suffix),
                                                 name='Immediate 25% of MSM on long-acting PrEP var',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    
    MSM.IP50v = create.intervention(ALL.MSM, PREP.50, INJECTABLE.PREP.VARIABLE,injectable.prep.rr.dist)
    INTERVENTION.MANAGER = register.intervention(MSM.IP50v, code=paste0('msm.p50.inj.variable', suffix),
                                                 name='Immediate 50% of MSM on long-acting PrEP variable',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    
    MSM.P10v = create.intervention(ALL.MSM, PREP.10, ORAL.PREP.VARIABLE,oral.prep.rr.dist)
    INTERVENTION.MANAGER = register.intervention(MSM.P10v, code=paste0('msm.p10.oral.variable', suffix),
                                                 name='Immediate 10% of MSM on oral PrEP variable',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    MSM.P25v = create.intervention(ALL.MSM, PREP.25, ORAL.PREP.VARIABLE,oral.prep.rr.dist)
    INTERVENTION.MANAGER = register.intervention(MSM.P25v, code=paste0('msm.p25.oral.variable', suffix),
                                                 name='Immediate 25% of MSM on oral PrEP variable',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    MSM.P50v = create.intervention(ALL.MSM, PREP.50, ORAL.PREP.VARIABLE,oral.prep.rr.dist)
    INTERVENTION.MANAGER = register.intervention(MSM.P50v, code=paste0('msm.p50.oral.variable', suffix),
                                                 name='Immediate 50% of MSM on oral PrEP variable',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    MSM.OIP10v = create.intervention(ALL.MSM, PREP.10,  INJECTABLE.ORAL.PREP.VARIABLE,oral.prep.rr.dist,injectable.prep.rr.dist)
    INTERVENTION.MANAGER = register.intervention(MSM.OIP10v, code=paste0('msm.p10.oralinj.variable', suffix),
                                                 name='Immediate 10% of MSM on oral/injectable PrEP variable',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    MSM.OIP25v = create.intervention(ALL.MSM, PREP.25,  INJECTABLE.ORAL.PREP.VARIABLE,oral.prep.rr.dist,injectable.prep.rr.dist)
    INTERVENTION.MANAGER = register.intervention(MSM.OIP25v, code=paste0('msm.p25.oralinj.variable', suffix),
                                                 name='Immediate 25% of MSM on oral/injectable PrEP variable',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    MSM.OIP50v = create.intervention(ALL.MSM, PREP.50,  INJECTABLE.ORAL.PREP.VARIABLE,oral.prep.rr.dist,injectable.prep.rr.dist)
    INTERVENTION.MANAGER = register.intervention(MSM.OIP50v, code=paste0('msm.p50.oralinj.variable', suffix),
                                                 name='Immediate 50% of MSM on oral/injectable PrEP variable',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
  
 
    #-- RETURN THE INTERVENTION MANAGER --#
    INTERVENTION.MANAGER
}


##-- DEFINE THE DISTRIBUTIONS TO USE FOR INJECTABLE and ORAL PREP EFFICACY --##

# INJECTABLE PREP, from:
# 
inj.ci.lower = log(.18)
inj.ci.upper = log(.62)

inj.log.sd = (inj.ci.upper - inj.ci.lower) / 2 / 1.96
inj.log.mean = (inj.ci.lower + inj.ci.upper)/2

INJ.PREP.RR.DIST = Lognormal.Distribution(meanlog = inj.log.mean, sdlog = inj.log.sd, var.name = 'inj.rr')


# ORAL PREP, from:
# https://pubmed.ncbi.nlm.nih.gov/26364263/
# (PROUD Study in MSM)

oral.efficacy.est = 0.86
oral.efficacy.lower = 0.64
oral.efficacy.upper = 0.96

#these are the ci relative to the estimate - ie log(bound/estimate) = log(bound) - log(estimate)
oral.ci.lower = log(1-oral.efficacy.upper) - log(1-oral.efficacy.est)
oral.ci.upper = log(1-oral.efficacy.lower) - log(1-oral.efficacy.est)

oral.log.sd = (oral.ci.upper - oral.ci.lower) / 2 / 1.96

ORAL.PREP.RR.DIST = Lognormal.Distribution(meanlog = 0, sdlog = oral.log.sd, var.name = 'oral.rr')
#^This is an RR relative to the already existing estimate 
#need to figure this out 

##-- ACTUALLY CREATE INTERVENTIONS FOR A 2023-2027 time frame --## 

#Note: breaks if intervention was already made

INTERVENTION.MANAGER.1.0 = create.prep.interventions(start.year=2023,
                                                     implemented.year=2027,
                                                     suffix='23_27',
                                                     INTERVENTION.MANAGER=INTERVENTION.MANAGER.1.0)

INTERVENTION.MANAGER.1.0 = create.prep.interventions(start.year=2015,
                                                     implemented.year=2015.0001,
                                                     suffix='15_15',
                                                     INTERVENTION.MANAGER=INTERVENTION.MANAGER.1.0)


