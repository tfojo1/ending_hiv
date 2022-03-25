##---------------------------##
##-- CREATE THE LIKELIHOOD --##
##---------------------------##

create.msa.likelihood <- function(msa,
                                  include.engagement=F,
                                  include.linkage=F,
                                  include.retention = F,
                                  include.cum.aids.mort = T,
                                  
                                  EVERYTHING.WEIGHT=1/2,
                                  
                                  NEW.WEIGHT = 1/4,#1/2,
                                  PREV.WEIGHT = 1,#2,
                                  MORT.WEIGHT = 1,
                                  CUM.MORT.WEIGHT = 1,
                                  IDU.WEIGHT = 1,
                                  AIDS.DX.WEIGHT = 1,
                                  TOTAL.DX.WEIGHT = 1/4,
                                  STRATIFIED.DX.WEIGHT=1/128/4,
                                  use.stratified.dx = F,
                                  SUPPRESSION.WEIGHT = 1/16,
                                  ENGAGEMENT.WEIGHT = 1/16,
                                  LINKAGE.WEIGHT = 1/16,
                                  PREP.WEIGHT = 1,
                                  TOTAL.TESTING.WEIGHT = 1/32,
                                  STRATIFIED.TESTING.WEIGHT = 1/32,#1/16
                                  TOTAL.TESTING.LOG.SD.EVER.TO.12MO = log(2),
                                  TOTAL.TESTING.RHO = 0.5,
                                  TOTAL.TESTING.ERROR.MULT=2,
                                  PROB.TESTING.DECREASING = 0.05,
                                  TESTING.DECREASE.THRESHOLD = -0.1,
                                  
                                  SUPPRESSION.SD = 0.01,
                                  ENGAGEMENT.SD = 0.01,
                                  LINKAGE.SD = 0.01,
                                  
                                  prep.exp = 0.5,
                                  prep.indicated.cv = 0.25,#0.5,
                                  prep.indicated.rho = 0.9,
                                  
                                  use.prev.to.new.cv.ratio=T,#F,
                                  FOCUS.WEIGHT=1,#4,
                                  IDU.LOG.SD = log(2),
                                  
                                  new.correlated.year.chunks=list(2008:2014, 2015:2018),
                                  prevalence.correlated.year.chunks=list(2007:2013, 2014:2017),
                                  mortality.correlated.year.chunks=list(2009:2013, 2014:2016),
                                  idu.years=2014:2016,
                                  diagnosed.years = 2010:2020,
                                  suppression.years = 2010:2020,
                                  engagement.years = 2010:2020,
                                  linkage.years = 2010:2020,
                                  prep.years = 2012:2018,
                                  
                                  year.to.year.chunk.correlation=0.65,
                                  year.to.year.off.correlation=0.25,
                                  
                                  measurement.error.cv.vs.sqrt.weight = 1,
                                  measurement.error.sd.mult=1,
                                  
                                  new.cv = 0.065,
                                  prevalence.cv = 0.09,#0.034,#0.09
                                  new.exp = 0.33,
                                  prevalence.exp = 0.69,
                                  
                                  verbose=F)
{ 
    parameters = as.list(environment())
    
    #-- SETTINGS --#
    
    # The demographic groups to which we want to give added weight
    to.focus = function(description){
        (grepl(' male',description) & (grepl('13-24', description) | grepl('25-34',description))) |
            (grepl('black, msm$',description) | grepl('hispanic, msm$', description))
    }
    
    #-- Elements for New Diagnoses --#
    SD.INFLATION.NEW.NUM = 1/sqrt(NEW.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
    SD.INFLATION.NEW = function(description){SD.INFLATION.NEW.NUM /
            (sqrt(FOCUS.WEIGHT)^as.numeric(to.focus(description)))}
    
    #  NEW.SD = function(years, num){sqrt(measurement.error.cv.vs.sqrt.weight * (0.047*num)^2 +#from 2016 mult.exp
    NEW.SD = function(years, num){sqrt(measurement.error.cv.vs.sqrt.weight * (new.cv*num)^2 +
                                           (1-measurement.error.cv.vs.sqrt.weight) * (num^new.exp)^2) *
            measurement.error.sd.mult}
    
    
    #-- Elements for Prevalence --#
    if (use.prev.to.new.cv.ratio)
        PREV.INFLATION = get.cv.weights(location=msa, weight.to='new')['prevalence']
    else
        PREV.INFLATION = 1
    
    SD.INFLATION.PREV.NUM = 1/sqrt(PREV.WEIGHT)/sqrt(EVERYTHING.WEIGHT)*PREV.INFLATION
    SD.INFLATION.PREV = function(description){SD.INFLATION.PREV.NUM /
            (sqrt(FOCUS.WEIGHT)^as.numeric(to.focus(description)))}
    
    PREV.SD = function(years, num){sqrt(measurement.error.cv.vs.sqrt.weight * (prevalence.cv*num)^2 + 
                                            (1-measurement.error.cv.vs.sqrt.weight) * (num^prevalence.exp)^2) *
            measurement.error.sd.mult}
    
    
    #-- Elements for Mortality --#
    SD.INFLATION.MORT = 1/sqrt(MORT.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
    MORT.SD = PREV.SD
    
    missing.mort.fraction = mean(c(.02, 458 / (1926 + 12219)))
    #2% in NDI alone = https://academic.oup.com/aje/article/174/1/90/126134
    #458 / (1926 + 12219) #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2773949/
    #458 in NDI alone
    
    MORT.BIAS = function(years, num){num - num/(1-missing.mort.fraction)}
    MORT.BIAS.SD = function(years, num){missing.mort.fraction * num / 2}
    
    #-- Elements for Awareness of Diagnosis --#
    TOTAL.SD.INFLATION.DX = 1/sqrt(TOTAL.DX.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
    STRATIFIED.SD.INFLATION.DX = 1/sqrt(STRATIFIED.DX.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
    DIAGNOSED.OBS.ERROR.SD=function(...){0.005}
    PROBABILITY.DIAGNOSIS.DECREASING = NA

    #-- Elements for Suppression --#
    SUPPRESSION.OBS.ERROR.SD = function(...){SUPPRESSION.SD}
    SUPPRESSION.SD.INFLATION = 1/sqrt(SUPPRESSION.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
    PROBABILITY.SUPPRESSION.DECREASING = 0.05

    #-- Elements for Engagement --#
    ENGAGEMENT.OBS.ERROR.SD = function(...){ENGAGEMENT.SD}
    ENGAGEMENT.SD.INFLATION = 1/sqrt(ENGAGEMENT.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
    PROBABILITY.ENGAGEMENT.DECREASING = 0.05
    
    
    #-- Elements for Linkage --#
    LINKAGE.OBS.ERROR.SD = function(...){LINKAGE.SD}
    LINKAGE.SD.INFLATION = 1/sqrt(LINKAGE.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
    PROBABILITY.LINKAGE.DECREASING = 0.05
    
    
    #-- Elements for PrEP --#
    PREP.SD = function(years, num){num^prep.exp}
    PREP.SD.INFLATION = 1/sqrt(PREP.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
    
    #-- Elements for Testing --#
    #none
    
    #-- Elements for IDU --#
    IDU.LOG.SD = IDU.LOG.SD / sqrt(EVERYTHING.WEIGHT) / sqrt(IDU.WEIGHT)
    
    #-- Elements for Historical AIDS Diagnoses --#
    SD.INFLATION.AIDS.DIAGNOSES = 1/sqrt(EVERYTHING.WEIGHT) / sqrt(AIDS.DX.WEIGHT)
    AIDS.SD = NEW.SD#function(...){0}#NEW.SD
    AIDS.TO.HIV.RATIO.LOG.SD = 0.5 * log(1.2)
    AIDS.RHO = 0
    
    #-- Elements for Historical Cumulative Mortality --#
    SD.INFLATION.CUM.MORT = 1/sqrt(EVERYTHING.WEIGHT) / sqrt(CUM.MORT.WEIGHT)
    #    CUM.MORT.SD = function(years,num){sqrt(5^2 + 0.5 * (0.065*num)^2 + 0.5 * (num^0.33)^2)}
    CUM.MORT.SD = function(years, num){2 * sqrt(5^2 + 0.5 * (0.09*num)^2 + 0.5 * (num^0.69)^2)}
    
    cum.aids.mort.data = get.surveillance.data(msa.surveillance, msa, data.type='cumulative.aids.mortality', throw.error.if.missing.data = F,
                                               sex=T,race=T,risk=T)
    if (include.cum.aids.mort && 
        (is.null(cum.aids.mort.data) || all(is.na(cum.aids.mort.data))))
    {
        print(paste0("NOT USING CUMULATIVE AIDS MORTALITY IN LIKELIHOOD (No data for ",
                     msa.names(msa), ")"))
        include.cum.aids.mort = F
    }
    
    #-- Pull Other Values to make likelihood --#
    POPULATION.TOTALS=get.census.totals(ALL.DATA.MANAGERS$census.totals, msa)
    
    #-- The Cache for Nested Errors --#
    nested.error.cache = make.nested.errors.cache(msa.surveillance, state.surveillance, county.surveillance)
    
    #Create the individual components of the likelihood
    if (verbose)
        print("Creating 'new' likelihood")
    new.lik = create.likelihood.function(data.type='new',
                                         years = sort(unlist(new.correlated.year.chunks)),
                                         surv=msa.surveillance,
                                         location=msa,
                                         by.total = T,
                                         by.sex.age=T,
                                         by.sex.race=T,
                                         by.sex.risk=T,
                                         by.race.risk=T,
                                         #         by.sex=T,
                                         #         by.risk=T,
                                         #         by.race=T,
                                         #         by.age=T,
                                         population=POPULATION.TOTALS,
                                         denominator.dimensions='year',
                                         msm.cv=0,
                                         idu.cv=0,
                                         sd.inflation=SD.INFLATION.NEW,
                                         year.to.year.correlation = 0,
                                         numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                         numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                         numerator.chunk.years=new.correlated.year.chunks,
                                         numerator.sd = NEW.SD)
    
    if (verbose)
        print("Creating 'prevalence' likelihood")
    prev.lik = create.likelihood.function(data.type='prevalence',
                                          years = sort(unlist(prevalence.correlated.year.chunks)),
                                          surv=msa.surveillance,
                                          location=msa,
                                          by.total = T,
                                          by.sex.age=T,
                                          by.sex.race=T,
                                          by.sex.risk=T,
                                          by.race.risk=T,
                                          #by.sex=T,
                                          #by.risk=T,
                                          #by.race=T,
                                          #by.age=T,
                                          population=POPULATION.TOTALS,
                                          denominator.dimensions='year',
                                          msm.cv=0,
                                          idu.cv=0,
                                          sd.inflation=SD.INFLATION.PREV,
                                          year.to.year.correlation = 0,
                                          numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                          numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                          numerator.chunk.years=prevalence.correlated.year.chunks,
                                          numerator.sd = PREV.SD)
    
    if (verbose)
        print("Creating 'mortality' likelihood")
    mort.lik = create.likelihood.function(data.type='mortality',
                                          years=sort(unlist(mortality.correlated.year.chunks)),
                                          surv=msa.surveillance,
                                          location=msa,
                                          by.sex=T,
                                          #   by.total=T,
                                          population=POPULATION.TOTALS,
                                          denominator.dimensions='year',
                                          msm.cv=0,
                                          idu.cv=0,
                                          sd.inflation = SD.INFLATION.MORT,
                                          year.to.year.correlation = 0,
                                          
                                          numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                          numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                          numerator.chunk.years=mortality.correlated.year.chunks,
                                          numerator.sd = MORT.SD,
                                          bias.fn = MORT.BIAS,
                                          bias.sd = MORT.BIAS.SD
    )
    
    if (verbose)
        print("Creating 'suppression' likelihood")
    suppressed.lik = create.nested.likelihood(data.type='suppression',
                                              years=suppression.years,
                                              msa=msa,
                                              
                                              msa.surveillance = msa.surveillance,
                                              state.surveillance = state.surveillance,
                                              county.surveillance = county.surveillance,
                                              
                                              observation.error.fn = SUPPRESSION.OBS.ERROR.SD,
                                              sd.inflation = SUPPRESSION.SD.INFLATION,
                                              
                                              probability.decreasing.slope=PROBABILITY.SUPPRESSION.DECREASING,
                                              
                                              cached.errors = nested.error.cache
                                              )
    
    if (include.engagement)
    {
        if (verbose)
            print("Creating 'engagement' likelihood")
        engaged.lik = create.nested.likelihood(data.type='engagement',
                                                  years=engagement.years,
                                                  msa=msa,
                                                  
                                                  msa.surveillance = msa.surveillance,
                                                  state.surveillance = state.surveillance,
                                                  county.surveillance = county.surveillance,
                                                  
                                                  observation.error.fn = ENGAGEMENT.OBS.ERROR.SD,
                                                  sd.inflation = ENGAGEMENT.SD.INFLATION,
                                                  
                                                  probability.decreasing.slope=PROBABILITY.ENGAGEMENT.DECREASING,
                                               
                                               cached.errors = nested.error.cache
        )
    }
    else
        engaged.lik = function(sim, log=T){if (log) 0 else 1}
    
    if (include.linkage)
    {
        if (verbose)
            print("Creating 'linkage' likelihood")
        linkage.lik = create.nested.likelihood(data.type='linkage',
                                                  years=linkage.years,
                                                  msa=msa,
                                                  
                                                  msa.surveillance = msa.surveillance,
                                                  state.surveillance = state.surveillance,
                                                  county.surveillance = county.surveillance,
                                                  
                                                  observation.error.fn = LINKAGE.OBS.ERROR.SD,
                                                  sd.inflation = LINKAGE.SD.INFLATION,
                                                  
                                                  probability.decreasing.slope=PROBABILITY.LINKAGE.DECREASING,
                                               
                                               cached.errors = nested.error.cache
        )
    }
    else
        linkage.lik = function(sim, log=T){if (log) 0 else 1}
    
    if (include.retention)
    {
        if (verbose)
            print("Creating 'retention' likelihood")
        
        retention.lik = create.retention.likelihood()
    }
    else
        retention.lik = function(sim, log=T){if (log) 0 else 1}
    
    
    if (verbose)
        print("Creating 'PrEP' likelihood")
    prep.lik = create.prep.likelihood(location=msa,
                                      years=prep.years,
                                      surv=msa.surveillance,
                                      population=POPULATION.TOTALS,
                                      numerator.year.to.year.chunk.correlation=0.5,
                                      numerator.year.to.year.off.correlation=0,
                                      numerator.chunk.years=list(prep.years),
                                      numerator.sd = PREP.SD,
                                      sd.inflation = PREP.SD.INFLATION,
                                      inflate.sd.by.n.obs.per.year=F,
                                      upweight.by.n.obs.per.year=T,
                                      by.total=T,
                                      by.age=T,
                                      by.sex=T,
                                      p.indicated.cv=prep.indicated.cv,
                                      p.indicated.rho=prep.indicated.rho)
    
    if (verbose)
        print("Creating 'diagnosed' likelihood")
#    dx.lik = create.knowledge.of.status.likelihood(location=msa,
 #                                                  surv=msa.surveillance,
  #                                                 state.surv=state.surveillance,
   #                                                census.totals=ALL.DATA.MANAGERS$census.totals,
    #                                               knowledge.of.status.regressions=NULL,
     #                                              years=diagnosed.years,
      #                                             total.sd.inflation=TOTAL.SD.INFLATION.DX,
       #                                            stratified.ors.sd.inflation=STRATIFIED.SD.INFLATION.DX,
        #                                           total.rho=0.5,
         #                                          total.numerator.sd = DX.SD,
          #                                         total.sd.multiplier.if.state = DX.SD.MULTIPLIER.IF.STATE,
           #                                        use.stratified.ors = use.stratified.dx)
    dx.lik = create.nested.likelihood(data.type='diagnosed',
                                              years=diagnosed.years,
                                              msa=msa,
                                              
                                              msa.surveillance = msa.surveillance,
                                              state.surveillance = state.surveillance,
                                              county.surveillance = county.surveillance,
                                              
                                              observation.error.fn = DIAGNOSED.OBS.ERROR.SD,
                                              sd.inflation = TOTAL.SD.INFLATION.DX,
                                              
                                              probability.decreasing.slope=PROBABILITY.DIAGNOSIS.DECREASING,
                                      
                                      cached.errors = nested.error.cache
    )
    
    if (verbose)
        print("Creating 'testing' likelihood")
    testing.lik = create.testing.likelihood(location=msa,
                                            continuum.manager=ALL.DATA.MANAGERS$continuum,
                                            census.totals=ALL.DATA.MANAGERS$census.totals,
                                            sd.inflation.total = 1/sqrt(TOTAL.TESTING.WEIGHT)/sqrt(EVERYTHING.WEIGHT),
                                            sd.inflation.stratified = 1/sqrt(STRATIFIED.TESTING.WEIGHT)/sqrt(EVERYTHING.WEIGHT),
                                            log.sd.ever.to.12mo = TOTAL.TESTING.LOG.SD.EVER.TO.12MO,
                                            rho.total = TOTAL.TESTING.RHO,
                                            sd.inflation.if.location.missing = 3,
                                            probability.decreasing.slope = PROB.TESTING.DECREASING,
                                            decreasing.slope.threshold = TESTING.DECREASE.THRESHOLD,
                                            sd.error.total.mult = TOTAL.TESTING.ERROR.MULT)
    
    if (verbose)
        print("Creating 'IDU' likelihood")
    idu.lik = create.idu.likelihood(idu.manager=ALL.DATA.MANAGERS$idu,
                                    census=ALL.DATA.MANAGERS$census.full,
                                    location=msa,
                                    years=idu.years,
                                    log.sd = IDU.LOG.SD)
    
    if (include.cum.aids.mort)
    {
        if (verbose)
            print("Creating 'cumulative aids mortality' likelihood")
        cum.mort.lik = create.cumulative.mortality.likelihood(surv = msa.surveillance,
                                                              location = msa,
                                                              numerator.sd = CUM.MORT.SD,
                                                              sd.inflation = SD.INFLATION.CUM.MORT)
    }
    else
        cum.mort.lik = function(sim, log=T){if (log) 0 else 1}
    
    if (verbose)
        print("Creating 'AIDS diagnoses' likelihood")
    aids.lik = create.aids.diagnoses.likelihood(surv=msa.surveillance,
                                                location=msa,
                                                numerator.sd=AIDS.SD,
                                                sd.inflation=SD.INFLATION.AIDS.DIAGNOSES,
                                                hiv.to.aids.diagnoses.ratio.log.sd=AIDS.TO.HIV.RATIO.LOG.SD,
                                                rho=AIDS.RHO)
    
    
    
    #Join them together
    
    if (verbose)
        print("Joining them together")
    full.likelihood = create.joint.likelihood.function(new=new.lik,
                                                       prev=prev.lik, 
                                                       mort=mort.lik,
                                                       dx=dx.lik,
                                                       linkage=linkage.lik,
                                                       engagement=engaged.lik,
                                                       retention=retention.lik,
                                                       supp=suppressed.lik,
                                                       prep=prep.lik,
                                                       testing=testing.lik,
                                                       idu=idu.lik,
                                                       cum.mort=cum.mort.lik, 
                                                       aids=aids.lik)
    
    attr(full.likelihood, 'parameters') = parameters
    
    if (1==2)
    {
    attr(full.likelihood, 'elements') = list(EVERYTHING.WEIGHT=EVERYTHING.WEIGHT,
                                             PREV.SD=PREV.SD,
                                             NEW.SD=NEW.SD,
                                             MORT.SD=MORT.SD,
                                             MORT.BIAS=MORT.BIAS,
                                             MORT.BIAS.SD=MORT.BIAS.SD,
                                             DX.SD=DX.SD,
                                             SD.INFLATION.NEW.NUM=SD.INFLATION.NEW.NUM,
                                             SD.INFLATION.PREV.NUM=SD.INFLATION.PREV.NUM,
                                             SD.INFLATION.NEW=SD.INFLATION.NEW,
                                             SD.INFLATION.PREV=SD.INFLATION.PREV,
                                             SD.INFLATION.MORT=SD.INFLATION.MORT,
                                             TOTAL.SD.INFLATION.DX=TOTAL.SD.INFLATION.DX,
                                             STRATIFIED.SD.INFLATION.DX=STRATIFIED.SD.INFLATION.DX,
                                             SD.INFLATION.CUM.MORT=SD.INFLATION.CUM.MORT,
                                             CUM.MORT.SD=CUM.MORT.SD,
                                             FOCUS.WEIGHT=FOCUS.WEIGHT,
                                             IDU.LOG.SD=IDU.LOG.SD,
                                             TOTAL.DX.WEIGHT=TOTAL.DX.WEIGHT,
                                             STRATIFIED.DX.WEIGHT=STRATIFIED.DX.WEIGHT,
                                             SD.INFLATION.AIDS.DIAGNOSES = SD.INFLATION.AIDS.DIAGNOSES,
                                             AIDS.SD = AIDS.SD,
                                             AIDS.TO.HIV.RATIO.LOG.SD = AIDS.TO.HIV.RATIO.LOG.SD)
    }
    
    attr(full.likelihood, 'components') = list(new=new.lik,
                                               prev=prev.lik, 
                                               mort=mort.lik,
                                               dx=dx.lik,
                                               linkage=linkage.lik,
                                               engagement=engaged.lik,
                                               retention=retention.lik,
                                               supp=suppressed.lik,
                                               prep=prep.lik,
                                               testing=testing.lik,
                                               idu=idu.lik,
                                               cum.mort=cum.mort.lik, 
                                               aids=aids.lik)
    
    if (verbose)
        print("Done. Returning")
    full.likelihood
}




##----------------------------------------##
##--  OLDER VERSIONS OF THE LIKELIHOOD  --##
##
##  (saved here to be able to look back)  ##
##----------------------------------------##


OLD.create.msa.likelihood.v1.for.annals <- function(msa,
                                  
                                  EVERYTHING.WEIGHT=1/2,
                                  
                                  NEW.WEIGHT = 1/4,#1/2,
                                  PREV.WEIGHT = 1,#2,
                                  MORT.WEIGHT = 1,
                                  CUM.MORT.WEIGHT = 1,
                                  IDU.WEIGHT = 1,
                                  AIDS.DX.WEIGHT = 1,
                                  TOTAL.DX.WEIGHT = 1/4,
                                  STRATIFIED.DX.WEIGHT=1/128/4,
                                  use.stratified.dx = F,
                                  SUPPRESSION.WEIGHT = 1/16,
                                  PREP.WEIGHT = 1,
                                  TOTAL.TESTING.WEIGHT = 1/32,
                                  STRATIFIED.TESTING.WEIGHT = 1/32,#1/16
                                  TOTAL.TESTING.LOG.SD.EVER.TO.12MO = log(2),
                                  TOTAL.TESTING.RHO = 0.5,
                                  TOTAL.TESTING.ERROR.MULT=2,
                                  PROB.TESTING.DECREASING = 0.05,
                                  TESTING.DECREASE.THRESHOLD = -0.1,
                                  
                                  SUPPRESSION.SD = 0.01,
                                  prep.exp = 0.5,
                                  prep.indicated.cv = 0.25,#0.5,
                                  prep.indicated.rho = 0.9,
                                  
                                  use.prev.to.new.cv.ratio=T,#F,
                                  FOCUS.WEIGHT=1,#4,
                                  IDU.LOG.SD = log(2),
                                  
                                  new.correlated.year.chunks=list(2008:2014, 2015:2018),
                                  prevalence.correlated.year.chunks=list(2007:2013, 2014:2017),
                                  mortality.correlated.year.chunks=list(2009:2013, 2014:2016),
                                  idu.years=2014:2016,
                                  diagnosed.years = 2010:2018,
                                  suppression.years = 2010:2018,
                                  prep.years = 2012:2018,
                                  
                                  year.to.year.chunk.correlation=0.65,
                                  year.to.year.off.correlation=0.25,
                                  
                                  measurement.error.cv.vs.sqrt.weight = 1,
                                  measurement.error.sd.mult=1,
                                  
                                  new.cv = 0.065,
                                  prevalence.cv = 0.09,#0.034,#0.09
                                  new.exp = 0.33,
                                  prevalence.exp = 0.69)
{ 
    parameters = as.list(environment())
    
    #-- SETTINGS --#
    
    # The demographic groups to which we want to give added weight
    to.focus = function(description){
        (grepl(' male',description) & (grepl('13-24', description) | grepl('25-34',description))) |
            (grepl('black, msm$',description) | grepl('hispanic, msm$', description))
    }
    
    #-- Elements for New Diagnoses --#
    SD.INFLATION.NEW.NUM = 1/sqrt(NEW.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
    SD.INFLATION.NEW = function(description){SD.INFLATION.NEW.NUM /
            (sqrt(FOCUS.WEIGHT)^as.numeric(to.focus(description)))}
    
    #  NEW.SD = function(years, num){sqrt(measurement.error.cv.vs.sqrt.weight * (0.047*num)^2 +#from 2016 mult.exp
    NEW.SD = function(years, num){sqrt(measurement.error.cv.vs.sqrt.weight * (new.cv*num)^2 +
                                           (1-measurement.error.cv.vs.sqrt.weight) * (num^new.exp)^2) *
            measurement.error.sd.mult}
    
    
    #-- Elements for Prevalence --#
    if (use.prev.to.new.cv.ratio)
        PREV.INFLATION = get.cv.weights(location=msa, weight.to='new')['prevalence']
    else
        PREV.INFLATION = 1
    
    SD.INFLATION.PREV.NUM = 1/sqrt(PREV.WEIGHT)/sqrt(EVERYTHING.WEIGHT)*PREV.INFLATION
    SD.INFLATION.PREV = function(description){SD.INFLATION.PREV.NUM /
            (sqrt(FOCUS.WEIGHT)^as.numeric(to.focus(description)))}
    
    #PREV.SD = function(years, num){sqrt(measurement.error.cv.vs.sqrt.weight * (0.09*num)^2 + 
    #                                       (1-measurement.error.cv.vs.sqrt.weight) * (num^0.69)^2) *
    #                              measurement.error.sd.mult}
    
    #from 2015 estimates vs atlas
    #    PREV.SD = function(years, num){sqrt(measurement.error.cv.vs.sqrt.weight * (0.083*num)^2 + 
    #                                           (1-measurement.error.cv.vs.sqrt.weight) * (num^0.51)^2) *
    #            measurement.error.sd.mult}
    
    #from 2009 intra-msa estimates
    #    PREV.SD = function(years, num){sqrt(measurement.error.cv.vs.sqrt.weight * (0.058*num)^2 + 
    #                                            (1-measurement.error.cv.vs.sqrt.weight) * (num^0.51)^2) *
    #            measurement.error.sd.mult}
    
    #from 2009-2014 intra-msa estimates
    #    PREV.SD = function(years, num){sqrt(measurement.error.cv.vs.sqrt.weight * (0.034*num)^2 + 
    #                                           (1-measurement.error.cv.vs.sqrt.weight) * (num^0.47)^2) *
    #          measurement.error.sd.mult}
    
    PREV.SD = function(years, num){sqrt(measurement.error.cv.vs.sqrt.weight * (prevalence.cv*num)^2 + 
                                            (1-measurement.error.cv.vs.sqrt.weight) * (num^prevalence.exp)^2) *
            measurement.error.sd.mult}
    
    
    #-- Elements for Mortality --#
    SD.INFLATION.MORT = 1/sqrt(MORT.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
    MORT.SD = PREV.SD
    
    missing.mort.fraction = mean(c(.02, 458 / (1926 + 12219)))
    #2% in NDI alone = https://academic.oup.com/aje/article/174/1/90/126134
    #458 / (1926 + 12219) #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2773949/
    #458 in NDI alone
    
    MORT.BIAS = function(years, num){num - num/(1-missing.mort.fraction)}
    MORT.BIAS.SD = function(years, num){missing.mort.fraction * num / 2}
    
    #-- Elements for Awareness of Diagnosis --#
    TOTAL.SD.INFLATION.DX = 1/sqrt(TOTAL.DX.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
    STRATIFIED.SD.INFLATION.DX = 1/sqrt(STRATIFIED.DX.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
    DX.SD=function(...){0.005}
    DX.SD.MULTIPLIER.IF.STATE = 3
    
    #-- Elements for Suppression --#
    SUPPRESSED.SD = function(...){SUPPRESSION.SD}
    SUPPRESSION.SD.INFLATION = 1/sqrt(SUPPRESSION.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
    SUPPRESSED.STATE.SD.INFLATION = 3
    PROBABILITY.SUPPRESSION.DECREASING = 0.05
    CONSIDER.DECREASING.ON.MARGINALS = F
    
    #-- Elements for PrEP --#
    PREP.SD = function(years, num){num^prep.exp}
    PREP.SD.INFLATION = 1/sqrt(PREP.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
    
    #-- Elements for Testing --#
    #none
    
    #-- Elements for IDU --#
    IDU.LOG.SD = IDU.LOG.SD / sqrt(EVERYTHING.WEIGHT) / sqrt(IDU.WEIGHT)
    
    #-- Elements for Historical AIDS Diagnoses --#
    SD.INFLATION.AIDS.DIAGNOSES = 1/sqrt(EVERYTHING.WEIGHT) / sqrt(AIDS.DX.WEIGHT)
    AIDS.SD = NEW.SD#function(...){0}#NEW.SD
    AIDS.TO.HIV.RATIO.LOG.SD = 0.5 * log(1.2)
    AIDS.RHO = 0
    
    #-- Elements for Historical Cumulative Diagnoses --#
    SD.INFLATION.CUM.MORT = 1/sqrt(EVERYTHING.WEIGHT) / sqrt(CUM.MORT.WEIGHT)
    #    CUM.MORT.SD = function(years,num){sqrt(5^2 + 0.5 * (0.065*num)^2 + 0.5 * (num^0.33)^2)}
    CUM.MORT.SD = function(years, num){2 * sqrt(5^2 + 0.5 * (0.09*num)^2 + 0.5 * (num^0.69)^2)}
    
    
    #-- Pull Other Values to make likelihood --#
    POPULATION.TOTALS=get.census.totals(ALL.DATA.MANAGERS$census.totals, msa)
    
    #Create the individual components of the likelihood
    new.lik = create.likelihood.function(data.type='new',
                                         years = sort(unlist(new.correlated.year.chunks)),
                                         surv=msa.surveillance,
                                         location=msa,
                                         by.total = T,
                                         by.sex.age=T,
                                         by.sex.race=T,
                                         by.sex.risk=T,
                                         by.race.risk=T,
                                         #         by.sex=T,
                                         #         by.risk=T,
                                         #         by.race=T,
                                         #         by.age=T,
                                         population=POPULATION.TOTALS,
                                         denominator.dimensions='year',
                                         msm.cv=0,
                                         idu.cv=0,
                                         sd.inflation=SD.INFLATION.NEW,
                                         year.to.year.correlation = 0,
                                         numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                         numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                         numerator.chunk.years=new.correlated.year.chunks,
                                         numerator.sd = NEW.SD)
    
    prev.lik = create.likelihood.function(data.type='prevalence',
                                          years = sort(unlist(prevalence.correlated.year.chunks)),
                                          surv=msa.surveillance,
                                          location=msa,
                                          by.total = T,
                                          by.sex.age=T,
                                          by.sex.race=T,
                                          by.sex.risk=T,
                                          by.race.risk=T,
                                          #by.sex=T,
                                          #by.risk=T,
                                          #by.race=T,
                                          #by.age=T,
                                          population=POPULATION.TOTALS,
                                          denominator.dimensions='year',
                                          msm.cv=0,
                                          idu.cv=0,
                                          sd.inflation=SD.INFLATION.PREV,
                                          year.to.year.correlation = 0,
                                          numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                          numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                          numerator.chunk.years=prevalence.correlated.year.chunks,
                                          numerator.sd = PREV.SD)
    
    mort.lik = create.likelihood.function(data.type='mortality',
                                          years=sort(unlist(mortality.correlated.year.chunks)),
                                          surv=msa.surveillance,
                                          location=msa,
                                          by.sex=T,
                                          #   by.total=T,
                                          population=POPULATION.TOTALS,
                                          denominator.dimensions='year',
                                          msm.cv=0,
                                          idu.cv=0,
                                          sd.inflation = SD.INFLATION.MORT,
                                          year.to.year.correlation = 0,
                                          
                                          numerator.year.to.year.chunk.correlation=year.to.year.chunk.correlation,
                                          numerator.year.to.year.off.correlation=year.to.year.off.correlation,
                                          numerator.chunk.years=mortality.correlated.year.chunks,
                                          numerator.sd = MORT.SD,
                                          bias.fn = MORT.BIAS,
                                          bias.sd = MORT.BIAS.SD
    )
    
    suppressed.lik = create.suppressed.likelihood(location=msa,
                                                  years=suppression.years,
                                                  surv=msa.surveillance,
                                                  numerator.year.to.year.chunk.correlation=0.5,
                                                  numerator.chunk.years=list(suppression.years),
                                                  numerator.sd = SUPPRESSED.SD,
                                                  sd.inflation=SUPPRESSION.SD.INFLATION,
                                                  inflate.sd.by.n.obs.per.year = F,
                                                  numerator.sd.inflation.if.backup=SUPPRESSED.STATE.SD.INFLATION,
                                                  probability.decreasing.slope=PROBABILITY.SUPPRESSION.DECREASING,
                                                  consider.decreasing.on.marginals=CONSIDER.DECREASING.ON.MARGINALS)
    
    
    prep.lik = create.prep.likelihood(location=msa,
                                      years=prep.years,
                                      surv=msa.surveillance,
                                      population=POPULATION.TOTALS,
                                      numerator.year.to.year.chunk.correlation=0.5,
                                      numerator.year.to.year.off.correlation=0,
                                      numerator.chunk.years=list(prep.years),
                                      numerator.sd = PREP.SD,
                                      sd.inflation = PREP.SD.INFLATION,
                                      inflate.sd.by.n.obs.per.year=F,
                                      upweight.by.n.obs.per.year=T,
                                      by.total=T,
                                      by.age=T,
                                      by.sex=T,
                                      p.indicated.cv=prep.indicated.cv,
                                      p.indicated.rho=prep.indicated.rho)
    
    dx.lik = create.knowledge.of.status.likelihood(location=msa,
                                                   surv=msa.surveillance,
                                                   state.surv=state.surveillance,
                                                   census.totals=ALL.DATA.MANAGERS$census.totals,
                                                   knowledge.of.status.regressions=NULL,
                                                   years=diagnosed.years,
                                                   total.sd.inflation=TOTAL.SD.INFLATION.DX,
                                                   stratified.ors.sd.inflation=STRATIFIED.SD.INFLATION.DX,
                                                   total.rho=0.5,
                                                   total.numerator.sd = DX.SD,
                                                   total.sd.multiplier.if.state = DX.SD.MULTIPLIER.IF.STATE,
                                                   use.stratified.ors = use.stratified.dx)
    
    testing.lik = create.testing.likelihood(location=msa,
                                            continuum.manager=ALL.DATA.MANAGERS$continuum,
                                            census.totals=ALL.DATA.MANAGERS$census.totals,
                                            sd.inflation.total = 1/sqrt(TOTAL.TESTING.WEIGHT)/sqrt(EVERYTHING.WEIGHT),
                                            sd.inflation.stratified = 1/sqrt(STRATIFIED.TESTING.WEIGHT)/sqrt(EVERYTHING.WEIGHT),
                                            log.sd.ever.to.12mo = TOTAL.TESTING.LOG.SD.EVER.TO.12MO,
                                            rho.total = TOTAL.TESTING.RHO,
                                            sd.inflation.if.location.missing = 3,
                                            probability.decreasing.slope = PROB.TESTING.DECREASING,
                                            decreasing.slope.threshold = TESTING.DECREASE.THRESHOLD,
                                            sd.error.total.mult = TOTAL.TESTING.ERROR.MULT)
    
    idu.lik = create.idu.likelihood(idu.manager=ALL.DATA.MANAGERS$idu,
                                    census=ALL.DATA.MANAGERS$census.full,
                                    location=msa,
                                    years=idu.years,
                                    log.sd = IDU.LOG.SD)
    
    cum.mort.lik = create.cumulative.mortality.likelihood(surv = msa.surveillance,
                                                          location = msa,
                                                          numerator.sd = CUM.MORT.SD,
                                                          sd.inflation = SD.INFLATION.CUM.MORT)
    
    aids.lik = create.aids.diagnoses.likelihood(surv=msa.surveillance,
                                                location=msa,
                                                numerator.sd=AIDS.SD,
                                                sd.inflation=SD.INFLATION.AIDS.DIAGNOSES,
                                                hiv.to.aids.diagnoses.ratio.log.sd=AIDS.TO.HIV.RATIO.LOG.SD,
                                                rho=AIDS.RHO)
    
    
    
    #Join them together
    
    full.likelihood = create.joint.likelihood.function(new=new.lik,
                                                       prev=prev.lik, 
                                                       mort=mort.lik,
                                                       dx=dx.lik,
                                                       supp=suppressed.lik,
                                                       prep=prep.lik,
                                                       testing=testing.lik,
                                                       idu=idu.lik,
                                                       cum.mort=cum.mort.lik, 
                                                       aids=aids.lik)
    
    attr(full.likelihood, 'parameters') = parameters
    
    attr(full.likelihood, 'elements') = list(EVERYTHING.WEIGHT=EVERYTHING.WEIGHT,
                                             PREV.SD=PREV.SD,
                                             NEW.SD=NEW.SD,
                                             MORT.SD=MORT.SD,
                                             MORT.BIAS=MORT.BIAS,
                                             MORT.BIAS.SD=MORT.BIAS.SD,
                                             DX.SD=DX.SD,
                                             SD.INFLATION.NEW.NUM=SD.INFLATION.NEW.NUM,
                                             SD.INFLATION.PREV.NUM=SD.INFLATION.PREV.NUM,
                                             SD.INFLATION.NEW=SD.INFLATION.NEW,
                                             SD.INFLATION.PREV=SD.INFLATION.PREV,
                                             SD.INFLATION.MORT=SD.INFLATION.MORT,
                                             TOTAL.SD.INFLATION.DX=TOTAL.SD.INFLATION.DX,
                                             STRATIFIED.SD.INFLATION.DX=STRATIFIED.SD.INFLATION.DX,
                                             SD.INFLATION.CUM.MORT=SD.INFLATION.CUM.MORT,
                                             CUM.MORT.SD=CUM.MORT.SD,
                                             FOCUS.WEIGHT=FOCUS.WEIGHT,
                                             IDU.LOG.SD=IDU.LOG.SD,
                                             TOTAL.DX.WEIGHT=TOTAL.DX.WEIGHT,
                                             STRATIFIED.DX.WEIGHT=STRATIFIED.DX.WEIGHT,
                                             SD.INFLATION.AIDS.DIAGNOSES = SD.INFLATION.AIDS.DIAGNOSES,
                                             AIDS.SD = AIDS.SD,
                                             AIDS.TO.HIV.RATIO.LOG.SD = AIDS.TO.HIV.RATIO.LOG.SD)
    
    attr(full.likelihood, 'components') = list(new=new.lik,
                                               prev=prev.lik, 
                                               mort=mort.lik,
                                               dx=dx.lik,
                                               supp=suppressed.lik,
                                               prep=prep.lik,
                                               testing=testing.lik,
                                               idu=idu.lik,
                                               cum.mort=cum.mort.lik, 
                                               aids=aids.lik)
    
    full.likelihood
}
