create.msa.likelihood <- function(msa,
                                  EVERYTHING.WEIGHT=1/8,
                                  
                                  NEW.WEIGHT = 1,
                                  PREV.WEIGHT = 1,
                                  MORT.WEIGHT = 1,
                                  CUM.MORT.WEIGHT = 1,
                                  IDU.WEIGHT = 64*8,
                                  AIDS.DX.WEIGHT = 4,
                                  TOTAL.DX.WEIGHT=1/8,
                                  STRATIFIED.DX.WEIGHT=1/128/4,
                                  SUPPRESSION.WEIGHT=1/8,
                                  
                                  prev.to.new.cv.ratio=1,
                                  FOCUS.WEIGHT=1,#4,
                                  
                                  new.correlated.year.chunks=list(2008:2014, 2015:2018),
                                  prevalence.correlated.year.chunks=list(2007:2013, 2014:2017),
                                  mortality.correlated.year.chunks=list(2009:2013, 2014:2016),
                                  idu.years=2014:2016,
                                  diagnosed.years = 2010:2018,
                                  suppression.years = 2010:2018,
                                  
                                  year.to.year.chunk.correlation=0.65,
                                  year.to.year.off.correlation=0.25)
{
    
    
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
    
    NEW.SD = function(years, num){sqrt(0.5 * (0.065*num)^2 + 0.5 * (num^0.33)^2)}
    
    
    #-- Elements for Prevalence --#
    PREV.INFLATION = prev.to.new.cv.ratio * get.cv.weights(location=msa, weight.to='new')['prevalence']
    
    SD.INFLATION.PREV.NUM = 1/sqrt(PREV.WEIGHT)/sqrt(EVERYTHING.WEIGHT)*PREV.INFLATION
    SD.INFLATION.PREV = function(description){SD.INFLATION.PREV.NUM /
            (sqrt(FOCUS.WEIGHT)^as.numeric(to.focus(description)))}
    
    PREV.SD = function(years, num){sqrt(0.5 * (0.09*num)^2 + 0.5 * (num^0.69)^2)}
    
    
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
    SUPPRESSED.SD = function(...){.005}
    SUPPRESSION.SD.INFLATION = 1/sqrt(SUPPRESSION.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
    SUPPRESSED.STATE.SD.INFLATION = 3
    PROBABILITY.SUPPRESSION.DECREASING = 0.05
    
    #-- Elements for IDU --#
    IDU.LOG.SD = log(2) / sqrt(EVERYTHING.WEIGHT) / sqrt(IDU.WEIGHT)
    
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
                                                  probability.decreasing.slope=PROBABILITY.SUPPRESSION.DECREASING)
    
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
                                                   total.sd.multiplier.if.state = DX.SD.MULTIPLIER.IF.STATE)
    
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
                                                       idu=idu.lik,
                                                       cum.mort=cum.mort.lik, 
                                                       aids=aids.lik)


    attr(full.likelihood, 'parameters') = list(EVERYTHING.WEIGHT=EVERYTHING.WEIGHT,
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
                                               idu=idu.lik,
                                               cum.mort=cum.mort.lik, 
                                               aids=aids.lik)

    full.likelihood
}
