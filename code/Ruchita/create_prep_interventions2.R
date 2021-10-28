
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


# @Ruchita - find a better evidence-based mean and sd
ORAL.PREP.PERSISTENCE.DIST = Normal.Distribution(mean=0.6,
                                                 sd=.05,
                                                 lower=0,
                                                 upper=1,
                                                 var.name='oral.prep.persistence')

INJ.VS.ORAL.DISCONTINUATION.RR.DIST = Uniform.Distribution(min=.25,
                                                           max=1,
                                                           var.name='inj.vs.oral.discontinuation.rr')

##-- MAKE THE INTERVENTIONS AND REGISTER THEM --##

create.prep.interventions.v2 <- function(start.year=2023,
                                         implemented.year=2027,
                                         suffix='',
                                         oral.prep.rr.dist = ORAL.PREP.MSM.RR.DIST, #oral.prep.rr
                                         inj.vs.oral.hr.dist = INJ.PREP.HR.DIST, #inj.vs.oral.hr
                                         oral.prep.persistence.dist = ORAL.PREP.PERSISTENCE.DIST, #oral.prep.persistence
                                         inj.vs.oral.discontinuation.rr.dist = INJ.VS.ORAL.DISCONTINUATION.RR.DIST #inj.vs.oral.discontinuation.rr
                                         )
{
    
    # UPTAKE INTERVENTION UNITS
    PREP.PLUS.10.ORAL = create.intervention.unit(type='prep', start.year=start.year,
                                                 rates=expression(0.10 * persistence.to.coverage.fraction(oral.prep.persistence)),
                                                 years=implemented.year,
                                                 apply.function='additive',
                                                 max.rate = 1)
    
    PREP.PLUS.10.INJ = create.intervention.unit(type='prep', start.year=start.year,
                                                 rates=expression(0.10 * persistence.to.coverage.fraction(1-(1-oral.prep.persistence)*inj.vs.oral.discontinuation.rr)),
                                                 years=implemented.year,
                                                 apply.function='additive',
                                                 max.rate = 1)
    
    
    PREP.PLUS.10.COMBINED = create.intervention.unit(type='prep', start.year=start.year,
                                                rates=expression(0.10 * (
                                                    0.5 * persistence.to.coverage.fraction(oral.prep.persistence) +
                                                    0.5 * persistence.to.coverage.fraction(1-(1-oral.prep.persistence)*inj.vs.oral.discontinuation.rr
                                                        ))),
                                                years=implemented.year,
                                                apply.function='additive',
                                                max.rate = 1)
    
    # EFFICACY INTERVENTION UNITS
    
    ORAL.PREP.EFFICACY = create.intervention.unit(type='rr.prep', start.year=2000,
                                                  rates='oral.prep.rr',
                                                  years=2000.001,
                                                  allow.less.than.otherwise = T)
    
    
    INJ.PREP.EFFICACY = create.intervention.unit(type='rr.prep', start.year=2000,
                                                  rates=c('oral.prep.rr',
                                                          'oral.prep.rr',
                                                          expression(oral.prep.rr * inj.vs.oral.hr)),
                                                  years=c(2000.001, start.year, implemented.year),
                                                  allow.less.than.otherwise = T)
    

    COMBINED.50.50.PREP.EFFICACY = create.intervention.unit(type='rr.prep', start.year=2000,
                                                 rates=c('oral.prep.rr',
                                                         'oral.prep.rr',
                                                         expression(0.5 * oral.prep.rr * persistence.to.coverage.fraction(oral.prep.persistence) +
                                                                            0.5 * oral.prep.rr * inj.vs.oral.hr * persistence.to.coverage.fraction(1 - (1-oral.prep.persistence) * inj.vs.oral.discontinuation.rr))),
                                                 years=c(2000.001, start.year, implemented.year),
                                                 allow.less.than.otherwise = T)
    
    
    
    # PUT THEM TOGETHER INTO INTERVENTIONS
    
    intervention.eg = create.intervention(ALL.MSM,
                                          PREP.PLUS.10.COMBINED,
                                          COMBINED.50.50.PREP.EFFICACY,
                                          oral.prep.rr.dist,
                                          inj.vs.oral.hr.dist,
                                          oral.prep.persistence.dist,
                                          inj.vs.oral.discontinuation.rr.dist
    )
}