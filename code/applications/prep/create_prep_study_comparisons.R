
oral.prep.rr.dist = DEFAULT.ORAL.PREP.MSM.RR.DIST #oral.prep.rr
inj.vs.oral.hr.dist = DEFAULT.INJ.PREP.HR.DIST #inj.vs.oral.hr
oral.prep.persistence.dist = DEFAULT.ORAL.PREP.PERSISTENCE.DIST #oral.prep.persistence
inj.vs.oral.discontinuation.rr.dist = DEFAULT.INJ.VS.ORAL.DISCONTINUATION.RR.DIST #inj.vs.oral.discontinuation.rr
INTERVENTION.MANAGER = INTERVENTION.MANAGER.1.0

  
  # COVERAGE INTERVENTION UNITS
  
  
  #Assume immediate implementation
  PREP.BASELINE.MARSHALL = create.intervention.unit(type='prep', start.year=2015,
                                           rates=0,
                                           years=2015.001,
                                           apply.function='absolute',
                                           max.rate = 1,
                                           scale = 'proportion')
  
  PREP.35.MARSHALL = create.intervention.unit(type='prep', start.year=2015,
                                              rates=.35,
                                              years=2015.001,
                                              apply.function='absolute',
                                              max.rate = 1,
                                              scale = 'proportion')
  
  PREP.BASELINE.MALONEY = create.intervention.unit(type='prep', start.year=2018,
                                                    rates=0,
                                                    years=2018.001,
                                                    apply.function='absolute',
                                                    max.rate = 1,
                                                   scale = 'proportion')
  
  PREP.15.MALONEY = create.intervention.unit(type='prep', start.year=2018,
                                              rates=.15,
                                              years=2018.001,
                                              apply.function='absolute',
                                              max.rate = 1,
                                             scale = 'proportion')
  
 
  #Injectable and Oral Variables 
  
  INJECTABLE.PREP.VARIABLE.MARSHALL = create.intervention.unit(type = "rr.prep", start.year = 2018, 
                                                               rates = 'inj.vs.oral.hr', years = 2018.001, 
                                                               apply.function = "multiplier", allow.less.than.otherwise = T,
                                                               scale = 'proportion')
  
  INJECTABLE.PREP.VARIABLE.MALONEY = create.intervention.unit(type = "rr.prep", start.year = 2018, 
                                                      rates = 'inj.vs.oral.hr', years = 2018.001, 
                                                      apply.function = "multiplier", allow.less.than.otherwise = T,
                                                      scale = 'proportion')
  
  
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
  INTERVENTION.MANAGER = register.intervention(INJ.MALONEY, code=paste0('inj.maloney'),
                                               name='15% all injectable maloney',
                                               manager = INTERVENTION.MANAGER,
                                               allow.intervention.multiple.names = T)
  




#@ruchita - I want you to fill in this list
COMPARISON.INTERVENTIONS.TO.RUN = list(
INJ.MALONEY, INJ.MARSHALL, ORAL.MALONEY, ORAL.MARSHALL, BASELINE.MARSHALL, BASELINE.MALONEY
)

INTERVENTION.MANAGER.1.0 = INTERVENTION.MANAGER
