

create.web.interventions <- function(TARGET.POP,
                                     baseline.mix, #vector of proportion oral, proportion convert to lai, both must sum to 1
                                     additional.mix ,#vector of lai, oral, mix
                                     uptake,#additional PrEP uptake of .1,.25,.35
                                     start.year,
                                     implemented.year){
  
  ##-- HELPER FUNCTIONS --##
  
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
  
  discontinuation.to.coverage.fraction <- function(discontinuation.rate)
  {
    persistence.to.coverage.fraction(1-discontinuation.rate)
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
  
  
  #Baseline Coverage
  
  if(baseline.mix[1] < 1) {
    BASELINE = create.intervention.unit(type='prep',
                                         start.year=start.year,
                                         rates=expression((baseline.mix[1] * discontinuation.to.coverage.fraction(inj.vs.oral.discontinuation.rr * (1-oral.prep.persistence)) +
                                                             baseline.mix[2] * persistence.to.coverage.fraction(oral.prep.persistence)) /
                                                            persistence.to.coverage.fraction(oral.prep.persistence)),
                                         years=implemented.year,
                                         apply.function='multiplier',
                                         allow.less.than.otherwise = T)
    
  }
  
  
  if(baseline.mix[1] == 1){
    BASELINE.EFFICACY = create.intervention.unit(type='rr.prep', start.year=2000,
                                                 rates='oral.prep.rr',
                                                 years=2000.001,
                                                 allow.less.than.otherwise = T)
  }
  
  else if(baseline.mix[1] == 0){
    BASELINE.EFFICACY = create.intervention.unit(type='rr.lai.prep', 
                                                 start.year=start.year,
                                                 rates=expression(oral.prep.rr * inj.vs.oral.hr),
                                                 years=implemented.year,
                                                 allow.less.than.otherwise = T)
  }
  else {
    BASELINE.EFFICACY = create.intervention.unit(type='rr.prep', start.year=2000,
                                                 rates=c('oral.prep.rr',
                                                         'oral.prep.rr',
                                                         expression(baseline.mix[1] * oral.prep.rr * persistence.to.coverage.fraction(oral.prep.persistence) +
                                                                      baseline.mix[2] * oral.prep.rr * inj.vs.oral.hr * persistence.to.coverage.fraction(1 - (1-oral.prep.persistence) * inj.vs.oral.discontinuation.rr))),
                                                 years=c(2000.001, start.year, implemented.year),
                                                 allow.less.than.otherwise = T)
    
  }
  

  
  
  #Additional Uptake/Efficacy 
  
  if(additional.mix == "mix"){
    ADDITIONAL = create.intervention.unit(type='lai.prep', start.year=start.year,
                                          rates=expression(uptake * (
                                            0.5 * persistence.to.coverage.fraction(oral.prep.persistence) +
                                              0.5 * persistence.to.coverage.fraction(1-(1-oral.prep.persistence)*inj.vs.oral.discontinuation.rr
                                              ))),
                                          years=implemented.year,
                                          max.rate = 1)
    EFFICACY = create.intervention.unit(type='rr.lai.prep', start.year=2000,
                                                            rates=expression(0.5 * oral.prep.rr * persistence.to.coverage.fraction(oral.prep.persistence) +
                                                                               0.5 * oral.prep.rr * inj.vs.oral.hr * persistence.to.coverage.fraction(1 - (1-oral.prep.persistence) * inj.vs.oral.discontinuation.rr)),
                                                            years=2000.001,
                                                            allow.less.than.otherwise = T)
  
    
  }
  else if (additional.mix == "oral"){
    
    ADDITIONAL = create.intervention.unit(type='prep', start.year=start.year,
                             rates=expression(uptake * persistence.to.coverage.fraction(oral.prep.persistence)),
                             years=implemented.year,
                             apply.function='additive',
                             max.rate = 1)
    
    EFFICACY = create.intervention.unit(type='rr.prep', start.year=2000,
                                                           rates=expression(oral.prep.rr * persistence.to.coverage.fraction(oral.prep.persistence)),
                                                           years=2000.001,
                                                           allow.less.than.otherwise = T)
    
  }
  
  else{
    
    ADDITIONAL = create.intervention.unit(type='lai.prep', start.year=start.year,
                             rates=expression(uptake * persistence.to.coverage.fraction(1-(1-oral.prep.persistence)*inj.vs.oral.discontinuation.rr)),
                             years=implemented.year,
                             apply.function='additive',
                             max.rate = 1)
     
    EFFICACY = create.intervention.unit(type='rr.lai.prep', start.year=2000,
                                                          rates=expression(oral.prep.rr * inj.vs.oral.hr * persistence.to.coverage.fraction(1 - (1-oral.prep.persistence) * inj.vs.oral.discontinuation.rr)),
                                                          years=2000.001,
                                                          allow.less.than.otherwise = T)
    
  }
  
  
  
  #Create intervention
  
  create.intervention(TARGET.POP,
                      BASELINE.EFFICACY,
                      ADDITIONAL.PLUS.10.INJ,
                      ADDITIONAL.TO.INJ.EFFICACY,oral.prep.rr.dist,inj.vs.oral.hr.dist, oral.prep.persistence.dist, inj.vs.oral.discontinuation.rr.dist)
  
  
  if(baseline.mix[1] <1){
    intervention = create.intervention(TARGET.POP,
                                       BASELINE,
                                       BASELINE.EFFICACY,
                                       ADDITIONAL,
                                       ADDITIONAL.EFFICACY,oral.prep.rr.dist,inj.vs.oral.hr.dist, oral.prep.persistence.dist, inj.vs.oral.discontinuation.rr.dist)
  }
  
  else{
    intervention = create.intervention(TARGET.POP,
                                       BASELINE.EFFICACY,
                                       ADDITIONAL,
                                       ADDITIONAL.EFFICACY,oral.prep.rr.dist,inj.vs.oral.hr.dist, oral.prep.persistence.dist, inj.vs.oral.discontinuation.rr.dist)
  }
 
  
return(intervention)
  
}