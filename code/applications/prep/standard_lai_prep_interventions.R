
##-- MAKE THE INTERVENTIONS AND REGISTER THEM --##

create.standard.prep.interventions <- function(start.year=2023,
                                               implemented.year=2027,
                                               suffix='',
                                               oral.prep.rr.dist = DEFAULT.ORAL.PREP.MSM.RR.DIST, #oral.prep.rr
                                               inj.vs.oral.hr.dist = DEFAULT.INJ.PREP.HR.DIST, #inj.vs.oral.hr
                                               oral.prep.persistence.dist = DEFAULT.ORAL.PREP.PERSISTENCE.DIST, #oral.prep.persistence
                                               inj.vs.oral.discontinuation.rr.dist = DEFAULT.INJ.VS.ORAL.DISCONTINUATION.RR.DIST, #inj.vs.oral.discontinuation.rr
                                               INTERVENTION.MANAGER = INTERVENTION.MANAGER.1.0)
{
    if (suffix != '' && substr(suffix, 1,1)!='_')
        suffix = paste0("_", suffix)

    BASELINE.TO.INJ.COVERAGE = create.intervention.unit(type='prep',
                                                        start.year=start.year,
                                                        rates=expression(discontinuation.to.coverage.fraction(inj.vs.oral.discontinuation.rr * (1-oral.prep.persistence)) /
                                                                           persistence.to.coverage.fraction(oral.prep.persistence)),
                                                        years=implemented.year,
                                                        apply.function='multiplier',
                                                        allow.less.than.otherwise = T)
    
    
    BASELINE.TO.50.50.COVERAGE = create.intervention.unit(type='prep',
                                                          start.year=start.year,
                                                          rates=expression((0.5 * discontinuation.to.coverage.fraction(inj.vs.oral.discontinuation.rr * (1-oral.prep.persistence)) +
                                                                              0.5 * persistence.to.coverage.fraction(oral.prep.persistence)) /
                                                                             persistence.to.coverage.fraction(oral.prep.persistence)
                                                          ),
                                                          years=implemented.year,
                                                          apply.function='multiplier',
                                                          allow.less.than.otherwise = T)
    
    
    ADDITIONAL.PLUS.10.ORAL = create.intervention.unit(type='prep', start.year=start.year,
                                              rates=expression(0.10 * persistence.to.coverage.fraction(oral.prep.persistence)),
                                               years=implemented.year,
                                                 apply.function='additive',
                                                 max.rate = 1)
    
    ADDITIONAL.PLUS.10.INJ = create.intervention.unit(type='lai.prep', start.year=start.year,
                                                rates=expression(0.10 * persistence.to.coverage.fraction(1-(1-oral.prep.persistence)*inj.vs.oral.discontinuation.rr)),
                                                years=implemented.year,
                                                apply.function='additive',
                                                max.rate = 1)
    
    ADDITIONAL.PLUS.25.ORAL = create.intervention.unit(type='prep', start.year=start.year,
                                                       rates=expression(0.25 * persistence.to.coverage.fraction(oral.prep.persistence)),
                                                       years=implemented.year,
                                                       apply.function='additive',
                                                       max.rate = 1)
    
    ADDITIONAL.PLUS.25.INJ = create.intervention.unit(type='lai.prep', start.year=start.year,
                                                      rates=expression(0.25 * persistence.to.coverage.fraction(1-(1-oral.prep.persistence)*inj.vs.oral.discontinuation.rr)),
                                                      years=implemented.year,
                                                      apply.function='additive',
                                                      max.rate = 1)
    
    
    ADDITIONAL.PLUS.35.ORAL = create.intervention.unit(type='prep', start.year=start.year,
                                                       rates=expression(0.35 * persistence.to.coverage.fraction(oral.prep.persistence)),
                                                       years=implemented.year,
                                                       apply.function='additive',
                                                       max.rate = 1)
    
    ADDITIONAL.PLUS.35.INJ = create.intervention.unit(type='lai.prep', start.year=start.year,
                                                      rates=expression(0.35 * persistence.to.coverage.fraction(1-(1-oral.prep.persistence)*inj.vs.oral.discontinuation.rr)),
                                                      years=implemented.year,
                                                      apply.function='additive',
                                                      max.rate = 1)
    
    ADDITIONAL.PLUS.5.ORAL.5.INJ = create.intervention.unit(type='lai.prep', start.year=start.year,
                                                            rates=expression(0.10 * (
                                                              0.5 * persistence.to.coverage.fraction(oral.prep.persistence) +
                                                                0.5 * persistence.to.coverage.fraction(1-(1-oral.prep.persistence)*inj.vs.oral.discontinuation.rr
                                                                ))),
                                                            years=implemented.year,
                                                            max.rate = 1)
    
    

    
    #efficacy
    #double check
    
    BASELINE.TO.ORAL.EFFICACY = create.intervention.unit(type='rr.prep', start.year=2000,
                                                         rates='oral.prep.rr',
                                                         years=2000.001,
                                                         allow.less.than.otherwise = T)
    BASELINE.TO.INJ.EFFICACY = create.intervention.unit(type='rr.lai.prep', 
                                                            start.year=start.year,
                                                            rates=expression(oral.prep.rr * inj.vs.oral.hr),
                                                            years=implemented.year,
                                                            allow.less.than.otherwise = T)

    
    BASELINE.TO.50.50.EFFICACY = create.intervention.unit(type='rr.prep', start.year=2000,
                                                              rates=c('oral.prep.rr',
                                                                      'oral.prep.rr',
                                                                      expression(0.5 * oral.prep.rr * persistence.to.coverage.fraction(oral.prep.persistence) +
                                                                                     0.5 * oral.prep.rr * inj.vs.oral.hr * persistence.to.coverage.fraction(1 - (1-oral.prep.persistence) * inj.vs.oral.discontinuation.rr))),
                                                              years=c(2000.001, start.year, implemented.year),
                                                              allow.less.than.otherwise = T)
    
    ADDITIONAL.TO.50.50.EFFICACY = create.intervention.unit(type='rr.lai.prep', start.year=2000,
                                                              rates=expression(0.5 * oral.prep.rr * persistence.to.coverage.fraction(oral.prep.persistence) +
                                                                                     0.5 * oral.prep.rr * inj.vs.oral.hr * persistence.to.coverage.fraction(1 - (1-oral.prep.persistence) * inj.vs.oral.discontinuation.rr)),
                                                              years=2000.001,
                                                              allow.less.than.otherwise = T)
    
    ADDITIONAL.TO.INJ.EFFICACY = create.intervention.unit(type='rr.lai.prep', start.year=2000,
                                                            rates=expression(oral.prep.rr * inj.vs.oral.hr * persistence.to.coverage.fraction(1 - (1-oral.prep.persistence) * inj.vs.oral.discontinuation.rr)),
                                                            years=2000.001,
                                                            allow.less.than.otherwise = T)
    
    ADDITIONAL.TO.ORAL.EFFICACY = create.intervention.unit(type='rr.prep', start.year=2000,
                                                            rates=expression(oral.prep.rr * persistence.to.coverage.fraction(oral.prep.persistence)),
                                                            years=2000.001,
                                                            allow.less.than.otherwise = T)
    
    


    # Make the interventions
    
    #BASELINE
    baseline.oral = create.intervention(ALL.MSM,
                                        BASELINE.TO.ORAL.EFFICACY,oral.prep.rr.dist,inj.vs.oral.hr.dist, oral.prep.persistence.dist, inj.vs.oral.discontinuation.rr.dist)
    INTERVENTION.MANAGER = register.intervention(baseline.oral, code=paste0('msm.baseline.oral', suffix),
                                                 name='Baseline Oral PrEP on MSM',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    #SCENARIO 1
    baseline.to.lai = create.intervention(ALL.MSM,
                                          BASELINE.TO.INJ.COVERAGE,
                                          BASELINE.TO.INJ.EFFICACY,oral.prep.rr.dist,inj.vs.oral.hr.dist, oral.prep.persistence.dist, inj.vs.oral.discontinuation.rr.dist)
    INTERVENTION.MANAGER = register.intervention(baseline.to.lai, code=paste0('msm.baseline.lai', suffix),
                                                 name='Baseline LAI PrEP on MSM',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    #SCENARIO 2
    oral.baseline.plus.10.oral = create.intervention(ALL.MSM,
                                                    BASELINE.TO.ORAL.EFFICACY,
                                                    ADDITIONAL.PLUS.10.ORAL,
                                                    ADDITIONAL.TO.ORAL.EFFICACY,oral.prep.rr.dist,inj.vs.oral.hr.dist, oral.prep.persistence.dist, inj.vs.oral.discontinuation.rr.dist)
    INTERVENTION.MANAGER = register.intervention(oral.baseline.plus.10.oral, code=paste0('msm.baseline.oral.plus.10.oral', suffix),
                                                 name='Baseline Oral PrEP with 10% Oral uptake on MSM',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    #SCENARIO 3
    oral.baseline.plus.10.lai = create.intervention(ALL.MSM,
                                                    BASELINE.TO.ORAL.EFFICACY,
                                                    ADDITIONAL.PLUS.10.INJ,
                                                    ADDITIONAL.TO.INJ.EFFICACY,oral.prep.rr.dist,inj.vs.oral.hr.dist, oral.prep.persistence.dist, inj.vs.oral.discontinuation.rr.dist)
    INTERVENTION.MANAGER = register.intervention(oral.baseline.plus.10.lai, code=paste0('msm.baseline.oral.plus.10.lai', suffix),
                                                 name='Baseline Oral PrEP with 10% LAI uptake on MSM',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    #SCENARIO 4
    lai.baseline.plus.10.lai = create.intervention(ALL.MSM,
                                                   BASELINE.TO.INJ.COVERAGE,
                                                   BASELINE.TO.INJ.EFFICACY,
                                                   ADDITIONAL.PLUS.10.INJ,
                                                   ADDITIONAL.TO.INJ.EFFICACY,oral.prep.rr.dist,inj.vs.oral.hr.dist, oral.prep.persistence.dist, inj.vs.oral.discontinuation.rr.dist)
    INTERVENTION.MANAGER = register.intervention(lai.baseline.plus.10.lai, code=paste0('msm.baseline.lai.plus.10.lai', suffix),
                                                 name='Baseline LAI PrEP with 10% LAI uptake on MSM',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    #SCENARIO 5
    oral.baseline.plus.25.oral = create.intervention(ALL.MSM,
                                                     BASELINE.TO.ORAL.EFFICACY,
                                                     ADDITIONAL.PLUS.25.ORAL,
                                                     ADDITIONAL.TO.ORAL.EFFICACY,oral.prep.rr.dist,inj.vs.oral.hr.dist, oral.prep.persistence.dist, inj.vs.oral.discontinuation.rr.dist)
    INTERVENTION.MANAGER = register.intervention(oral.baseline.plus.25.oral, code=paste0('msm.baseline.oral.plus.25.oral', suffix),
                                                 name='Baseline Oral PrEP with 25% Oral uptake on MSM',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    #SCENARIO 6
    oral.baseline.plus.25.lai = create.intervention(ALL.MSM,
                                                    BASELINE.TO.ORAL.EFFICACY,
                                                    ADDITIONAL.PLUS.25.INJ,
                                                    ADDITIONAL.TO.INJ.EFFICACY,oral.prep.rr.dist,inj.vs.oral.hr.dist, oral.prep.persistence.dist, inj.vs.oral.discontinuation.rr.dist)
    INTERVENTION.MANAGER = register.intervention(oral.baseline.plus.25.lai, code=paste0('msm.baseline.oral.plus.25.lai', suffix),
                                                 name='Baseline Oral PrEP with 25% LAI uptake on MSM',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    #SCENARIO 7
    lai.baseline.plus.25.lai = create.intervention(ALL.MSM,
                                                   BASELINE.TO.INJ.COVERAGE,
                                                   BASELINE.TO.INJ.EFFICACY,
                                                   ADDITIONAL.PLUS.25.INJ,
                                                   ADDITIONAL.TO.INJ.EFFICACY,oral.prep.rr.dist,inj.vs.oral.hr.dist, oral.prep.persistence.dist, inj.vs.oral.discontinuation.rr.dist)
    INTERVENTION.MANAGER = register.intervention(lai.baseline.plus.25.lai, code=paste0('msm.baseline.lai.plus.25.lai', suffix),
                                                 name='Baseline LAI PrEP with 25% LAI uptake on MSM',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
   
    
    #Additional
    

    half.half.baseline.plus.5.oral.5.lai = create.intervention(ALL.MSM,
                                                               BASELINE.TO.50.50.COVERAGE,
                                                               BASELINE.TO.50.50.EFFICACY,
                                                               ADDITIONAL.PLUS.5.ORAL.5.INJ,
                                                               ADDITIONAL.TO.50.50.EFFICACY,oral.prep.rr.dist,inj.vs.oral.hr.dist, oral.prep.persistence.dist, inj.vs.oral.discontinuation.rr.dist)
    INTERVENTION.MANAGER = register.intervention(half.half.baseline.plus.5.oral.5.lai, code=paste0('msm.50.50.baseline.plus.combined', suffix),
                                                 name='50/50 Baseline PrEP with 5% LAI/5% Oral uptake on MSM',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
}





INTERVENTION.MANAGER.1.0 = create.standard.prep.interventions(start.year=2023,
                                                              implemented.year=2027,
                                                              suffix='23_27',
                                                              oral.prep.rr.dist = DEFAULT.ORAL.PREP.MSM.RR.DIST, #oral.prep.rr
                                                              inj.vs.oral.hr.dist = DEFAULT.INJ.PREP.HR.DIST, #inj.vs.oral.hr
                                                              oral.prep.persistence.dist = DEFAULT.ORAL.PREP.PERSISTENCE.DIST, #oral.prep.persistence
                                                              inj.vs.oral.discontinuation.rr.dist = DEFAULT.INJ.VS.ORAL.DISCONTINUATION.RR.DIST, #inj.vs.oral.discontinuation.rr
                                                              INTERVENTION.MANAGER = INTERVENTION.MANAGER.1.0)