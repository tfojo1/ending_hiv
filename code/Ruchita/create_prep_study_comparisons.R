
source('code/source_code.R')

persistence.to.coverage.fraction <- function(persistence)
{
  if (persistence==1)
    1
  else
  {
    lambda = -log(persistence)
    (1/lambda - exp(-lambda)/lambda)
  }
}

##-- DEFINE DISTRIBUTIONS --##

#ORAL PREP, from:
# https://pubmed.ncbi.nlm.nih.gov/26364263/
# (PROUD Study in MSM)

oral.efficacy.est = 0.86
oral.efficacy.lower = 0.64
oral.efficacy.upper = 0.96

#these are the ci relative to the estimate - ie log(bound/estimate) = log(bound) - log(estimate)
oral.ci.lower = log(1-oral.efficacy.upper)# - log(1-oral.efficacy.est)
oral.ci.upper = log(1-oral.efficacy.lower)# - log(1-oral.efficacy.est)

oral.log.sd = (oral.ci.upper - oral.ci.lower) / 2 / 1.96

ORAL.PREP.MSM.RR.DIST = Lognormal.Distribution(meanlog = (oral.ci.upper+oral.ci.lower)/2, 
                                               sdlog = oral.log.sd, var.name = 'oral.prep.rr')

# INJECTABLE PREP, from:
# 
inj.ci.lower = log(.18)
inj.ci.upper = log(.62)

inj.log.sd = (inj.ci.upper - inj.ci.lower) / 2 / 1.96
inj.log.mean = (inj.ci.lower + inj.ci.upper)/2

INJ.PREP.HR.DIST = Lognormal.Distribution(meanlog = inj.log.mean, sdlog = inj.log.sd, var.name = 'inj.vs.oral.hr')


# @Ruchita - find a better evidence-based mean and sd https://onlinelibrary.wiley.com/doi/full/10.1002/jia2.25252

oral.persistence.mean = .56
oral.prep.persistence.ci.upper = oral.persistence.mean + 1.96*sqrt((oral.persistence.mean*(1-oral.persistence.mean))/7148)
oral.prep.persistence.ci.lower = oral.persistence.mean-1.96*sqrt((oral.persistence.mean*(1-oral.persistence.mean))/7148) 

#oral.persistence.sd = (oral.prep.persistence.ci.upper - oral.prep.persistence.ci.lower) /3.92
oral.persistence.sd = .21 / 2 / 1.96
ORAL.PREP.PERSISTENCE.DIST = Normal.Distribution(mean=oral.persistence.mean,
                                                 sd=oral.persistence.sd,
                                                 lower=0,
                                                 upper=1,
                                                 var.name='oral.prep.persistence')

INJ.VS.ORAL.DISCONTINUATION.RR.DIST = Uniform.Distribution(min=.25,
                                                           max=1,
                                                           var.name='inj.vs.oral.discontinuation.rr')

#take this out of function 

oral.prep.rr.dist = ORAL.PREP.MSM.RR.DIST #oral.prep.rr
inj.vs.oral.hr.dist = INJ.PREP.HR.DIST #inj.vs.oral.hr
oral.prep.persistence.dist = ORAL.PREP.PERSISTENCE.DIST #oral.prep.persistence
inj.vs.oral.discontinuation.rr.dist = INJ.VS.ORAL.DISCONTINUATION.RR.DIST #inj.vs.oral.discontinuation.rr
INTERVENTION.MANAGER = INTERVENTION.MANAGER.1.0

  if (suffix != '' && substr(suffix, 1,1)!='_')
    suffix = paste0("_", suffix)
  
  # COVERAGE INTERVENTION UNITS
  
  
  #Assume immediate implementation
  PREP.BASELINE.MARSHALL = create.intervention.unit(type='prep', start.year=2015,
                                           rates=0,
                                           years=2015.001,
                                           apply.function='absolute',
                                           max.rate = 1)
  
  PREP.35.MARSHALL = create.intervention.unit(type='prep', start.year=2015,
                                              rates=.35,
                                              years=2015.001,
                                              apply.function='absolute',
                                              max.rate = 1)
  
  PREP.BASELINE.MALONEY = create.intervention.unit(type='prep', start.year=2018,
                                                    rates=0,
                                                    years=2018.001,
                                                    apply.function='absolute',
                                                    max.rate = 1)
  
  PREP.15.MALONEY = create.intervention.unit(type='prep', start.year=2018,
                                              rates=.15,
                                              years=2018.001,
                                              apply.function='absolute',
                                              max.rate = 1)
  
 
  #Injectable and Oral Variables 
  
  INJECTABLE.PREP.VARIABLE.MARSHALL = create.intervention.unit(type = "rr.prep", start.year = 2018, 
                                                               rates = 'inj.vs.oral.hr', years = 2018.001, 
                                                               apply.function = "multiplier", allow.less.than.otherwise = T)
  
  INJECTABLE.PREP.VARIABLE.MALONEY = create.intervention.unit(type = "rr.prep", start.year = 2018, 
                                                      rates = 'inj.vs.oral.hr', years = 2018.001, 
                                                      apply.function = "multiplier", allow.less.than.otherwise = T)
  
  
  ORAL.PREP.VARIABLE = create.intervention.unit(type='rr.prep', start.year=2014,
                                                rates = 'oral.prep.rr', years=2014.001,
                                                apply.function='multiplier', allow.less.than.otherwise = T)
  
  
  # PUT THEM TOGETHER INTO INTERVENTIONS
  
  BASELINE.MARSHALL = create.intervention(ALL.MSM,PREP.BASELINE.MARSHALL)
  INTERVENTION.MANAGER = register.intervention(BASELINE.MARSHALL, code=paste0('baseline.marshall'),
                                               name='baseline marshall',
                                               manager = INTERVENTION.MANAGER,
                                               allow.intervention.multiple.names = T)
  
  BASELINE.MALONEY = create.intervention(ALL.MSM,PREP.BASELINE.MALONEY)
  INTERVENTION.MANAGER = register.intervention(BASELINE.MALONEY, code=paste0('baseline.maloney'),
                                               name='baseline maloney',
                                               manager = INTERVENTION.MANAGER,
                                               allow.intervention.multiple.names = T)
  
  ORAL.MARSHALL = create.intervention(ALL.MSM,PREP.35.MARSHALL,ORAL.PREP.VARIABLE, oral.prep.rr.dist, inj.vs.oral.hr.dist)
  INTERVENTION.MANAGER = register.intervention(ORAL.MARSHALL, code=paste0('oral.marshall'),
                                               name='35% all oral marshall',
                                               manager = INTERVENTION.MANAGER,
                                               allow.intervention.multiple.names = T)
  
  ORAL.MALONEY = create.intervention(ALL.MSM,PREP.15.MALONEY,ORAL.PREP.VARIABLE, oral.prep.rr.dist, inj.vs.oral.hr.dist)
  INTERVENTION.MANAGER = register.intervention(ORAL.MALONEY, code=paste0('oral.maloney'),
                                               name='15% all oral maloney',
                                               manager = INTERVENTION.MANAGER,
                                               allow.intervention.multiple.names = T)
  
  INJ.MARSHALL = create.intervention(ALL.MSM,PREP.35.MARSHALL,INJECTABLE.PREP.VARIABLE.MARSHALL, oral.prep.rr.dist, inj.vs.oral.hr.dist)
  INTERVENTION.MANAGER = register.intervention(INJ.MARSHALL, code=paste0('inj.marshall'),
                                               name='35% all injectable marshall',
                                               manager = INTERVENTION.MANAGER,
                                               allow.intervention.multiple.names = T)
  
  INJ.MALONEY = create.intervention(ALL.MSM,PREP.15.MALONEY,INJECTABLE.PREP.VARIABLE.MALONEY, oral.prep.rr.dist, inj.vs.oral.hr.dist)
  INTERVENTION.MANAGER = register.intervention(INJ.MALONEY, code=paste0('inj.marshall'),
                                               name='15% all injectable maloney',
                                               manager = INTERVENTION.MANAGER,
                                               allow.intervention.multiple.names = T)
  




#@ruchita - I want you to fill in this list
COMPARISON.INTERVENTIONS.TO.RUN = list(
INJ.MALONEY, INJ.MARSHALL, ORAL.MALONEY, ORAL.MARSHALL, BASELINE.MARSHALL, BASELINE.MALONEY

)
