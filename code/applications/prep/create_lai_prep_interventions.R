


#'@param baseline.proportion.oral
#'@param additional.proportion.oral
create.lai.prep.interventions <- function(target.population,
                                          
                                          baseline.proportion.oral,
                                          additional.proportion.oral,
                                          additional.uptake,#additional PrEP uptake of .1,.25,.35
                                          
                                          start.year,
                                          implemented.year,
                                          
                                          oral.prep.rr.dist = DEFAULT.ORAL.PREP.MSM.RR.DIST,
                                          inj.vs.oral.hr.dist = DEFAULT.INJ.PREP.HR.DIST, 
                                          oral.prep.persistence.dist = DEFAULT.ORAL.PREP.PERSISTENCE.DIST, 
                                          inj.vs.oral.discontinuation.rr.dist = DEFAULT.INJ.VS.ORAL.DISCONTINUATION.RR.DIST
)
{
    #-- Check Distributions --#
    
    if (oral.prep.rr.dist@n.var==1)
        oral.prep.rr.dist@var.names = 'oral.prep.rr'
    else if (!any(oral.prep.rr.dist@var.names=='oral.prep.rr'))
        stop("oral.prep.rr.dist must be either a univariate distribution or have a parameter named 'oral.prep.rr'")
    
    if (inj.vs.oral.hr.dist@n.var==1)
        inj.vs.oral.hr.dist@var.names = 'inj.vs.oral.hr'
    else if (!any(inj.vs.oral.hr.dist@var.names=='inj.vs.oral.hr'))
        stop("inj.vs.oral.hr.dist must be either a univariate distribution or have a parameter named 'inj.vs.oral.hr'")
    
    if (oral.prep.persistence.dist@n.var==1)
        oral.prep.persistence.dist@var.names = 'oral.prep.persistence'
    else if (!any(oral.prep.persistence.dist@var.names=='oral.prep.persistence'))
        stop("oral.prep.persistence.dist must be either a univariate distribution or have a parameter named 'oral.prep.persistence'")
    
    if (inj.vs.oral.discontinuation.rr.dist@n.var==1)
        inj.vs.oral.discontinuation.rr.dist@var.names = 'inj.vs.oral.discontinuation.rr'
    else if (!any(inj.vs.oral.discontinuation.rr.dist@var.names=='inj.vs.oral.discontinuation.rr'))
        stop("inj.vs.oral.discontinuation.rr.dist must be either a univariate distribution or have a parameter named 'inj.vs.oral.discontinuation.rr'")
    
    #-- Baseline Coverage --
    baseline.mix = c(baseline.proportion.oral, 1-baseline.proportion.oral)
    
    if(baseline.mix[1] < 1) {
        rates.expr = parse(text=paste0(
            "(", baseline.mix[1]," * discontinuation.to.coverage.fraction(inj.vs.oral.discontinuation.rr * (1-oral.prep.persistence)) + ",
            baseline.mix[2], " * persistence.to.coverage.fraction(oral.prep.persistence)) / ",
            " persistence.to.coverage.fraction(oral.prep.persistence)"
            ))
        
        BASELINE = create.intervention.unit(type='prep',
                                            start.year=start.year,
                                            rates.expr,
                                        #    rates=expression((baseline.mix[1] * discontinuation.to.coverage.fraction(inj.vs.oral.discontinuation.rr * (1-oral.prep.persistence)) +
                                        #                          baseline.mix[2] * persistence.to.coverage.fraction(oral.prep.persistence)) /
                                        #                         persistence.to.coverage.fraction(oral.prep.persistence)),
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
        rate.3.expr = parse(text=paste0(
            baseline.mix[1], " * oral.prep.rr * persistence.to.coverage.fraction(oral.prep.persistence) + ",
            baseline.mix[2], " * oral.prep.rr * inj.vs.oral.hr * persistence.to.coverage.fraction(1 - (1-oral.prep.persistence) * inj.vs.oral.discontinuation.rr)"
            ))
        BASELINE.EFFICACY = create.intervention.unit(type='rr.prep', start.year=2000,
                                                     rates=c('oral.prep.rr',
                                                             'oral.prep.rr',
                                                             rate.3.expr),
                                                       #      expression(baseline.mix[1] * oral.prep.rr * persistence.to.coverage.fraction(oral.prep.persistence) +
                                                       #                     baseline.mix[2] * oral.prep.rr * inj.vs.oral.hr * persistence.to.coverage.fraction(1 - (1-oral.prep.persistence) * inj.vs.oral.discontinuation.rr))),
                                                     years=c(2000.001, start.year, implemented.year),
                                                     allow.less.than.otherwise = T)
        
    }
    
    
    
    
    #Additional Uptake/Efficacy 
    
    additional.mix = c(additional.proportion.oral, 1-additional.proportion.oral)
    
    rates.expr = parse(text=paste0(
        "(", additional.uptake, " * (",
            additional.mix[1], " * persistence.to.coverage.fraction(oral.prep.persistence) + ",
            additional.mix[2], " * persistence.to.coverage.fraction(1-(1-oral.prep.persistence)*inj.vs.oral.discontinuation.rr
                )))"
    ))
    ADDITIONAL = create.intervention.unit(type='lai.prep', start.year=start.year,
                                          rates = rates.expr,
                                          #rates=expression(additional.uptake * (
                                           #   additional.mix[1] * persistence.to.coverage.fraction(oral.prep.persistence) +
                                            #      additional.mix[2] * persistence.to.coverage.fraction(1-(1-oral.prep.persistence)*inj.vs.oral.discontinuation.rr
                                             #     ))),
                                          years=implemented.year,
                                          max.rate = 1)
    
    rates.expr = parse(text=paste0(
        additional.mix[1], "* oral.prep.rr * persistence.to.coverage.fraction(oral.prep.persistence) + ",
                       additional.mix[2], " * oral.prep.rr * inj.vs.oral.hr * persistence.to.coverage.fraction(1 - (1-oral.prep.persistence) * inj.vs.oral.discontinuation.rr)"
    ))
    ADDITIONAL.EFFICACY = create.intervention.unit(type='rr.lai.prep', start.year=2000,
                                                   rates=rates.expr,
#                                        rates=expression(additional.mix[1] * oral.prep.rr * persistence.to.coverage.fraction(oral.prep.persistence) +
 #                                                            additional.mix[2] * oral.prep.rr * inj.vs.oral.hr * persistence.to.coverage.fraction(1 - (1-oral.prep.persistence) * inj.vs.oral.discontinuation.rr)),
                                        years=2000.001,
                                        allow.less.than.otherwise = T)
        
   
    
    
    #Create intervention
    
    if(baseline.mix[1] <1){
        intervention = create.intervention(target.population,
                                           BASELINE,
                                           BASELINE.EFFICACY,
                                           ADDITIONAL,
                                           ADDITIONAL.EFFICACY,
                                           
                                           oral.prep.rr.dist,
                                           inj.vs.oral.hr.dist,
                                           oral.prep.persistence.dist,
                                           inj.vs.oral.discontinuation.rr.dist)
    }
    
    else{
        intervention = create.intervention(target.population,
                                           BASELINE.EFFICACY,
                                           ADDITIONAL,
                                           ADDITIONAL.EFFICACY,
                                           
                                           oral.prep.rr.dist,
                                           inj.vs.oral.hr.dist, 
                                           oral.prep.persistence.dist, 
                                           inj.vs.oral.discontinuation.rr.dist)
    }
    
    
    intervention
    
}