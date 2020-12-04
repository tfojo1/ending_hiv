

##---------------------------##
##-- CREATE THE LIKELIHOOD --##
##---------------------------##

create.msa.likelihood <- function(msa,
                                  EVERYTHING.WEIGHT=1/2,#1/8,
                                  
                                  NEW.WEIGHT = 1,#1/2,
                                  PREV.WEIGHT = 4,#1/8,
                                  MORT.WEIGHT = 1,
                                  CUM.MORT.WEIGHT = 1,
                                  IDU.WEIGHT = 1,
                                  AIDS.DX.WEIGHT = 1,
                                  TOTAL.DX.WEIGHT = 1/4,#1,
                                  STRATIFIED.DX.WEIGHT=1/128/4,
                                  use.stratified.dx = F,
                                  SUPPRESSION.WEIGHT = 1/16,#1/4,
                                  PREP.WEIGHT = 1,
                                  TOTAL.TESTING.WEIGHT = 1/32,#1/8,
                                  STRATIFIED.TESTING.WEIGHT = 1/16,#1/4,
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


##-----------------##
##-- RUN INITIAL --##
##-----------------##

setup.initial.mcmc.for.msa <- function(msa,
                                       likelihood=NULL,
                                       prior=parameters.prior,
                                       parameter.var.blocks = PARAMETER.VAR.BLOCKS.1,
                                       template.mcmc=NULL,
                                       chains=1,
                                       n.iter=20000,
                                       thin=20,
                                       burn=0,
                                       max.sim.time=20,
                                       save.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_initial'),
                                       cache.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_caches'),
                                       update.frequency=200,
                                       cache.frequency=500,
                                       save.suffix='',
                                       
                                       target.acceptance.rate=0.1,
                                       SCALING.BASE.UPDATE = 1,
                                       SCALING.UPDATE.PRIOR=10,
                                       SCALING.UPDATE.DECAY=.5,#0.25,
                                       COV.BASE.UPDATE=0.2,
                                       COV.UPDATE.PRIOR=500,
                                       COV.UPDATE.DECAY=0.5,#0.25
                                       
                                       run=T,
                                       verbose=T,
                                       step.size.multiplier=1,
                                       derive.step.size.from.prior.mcmc=F)
{
    
    # Likelihood
    if (is.null(likelihood))
    {
        if (verbose)
            print("Creating Likelihood")
        likelihood = create.msa.likelihood(msa)
    }
    
    # Start values
    start.value.file = file.path('mcmc_runs/start_values', paste0(msa, '.Rdata'))
    if (!file.exists(start.value.file))
        stop(paste0("Initial values have not been set for ", msa.names(msa)))
    
    load(start.value.file)    
    start.value.generator = function(n){
        if (n==1)
            starting.parameters
        else
            t(sapply(1:n, function(i){starting.parameters}))
    }   
    
    # Pull chain state variables
    
    if (derive.step.size.from.prior.mcmc) #this chunk bases step sizes and initial cov mat on previous mcmc run
    {
        if (is.null(template.mcmc))
        {
            load('mcmc_runs/test_runs/la.113c_revised.lik.v12_20K_2020-09-20.Rdata')
            template.mcmc = mcmc
        }
        
        simset = extract.simset(template.mcmc, additional.burn=mcmc@n.iter/2)
        chain.state = template.mcmc@chain.states[[1]]
        initial.scaling.parameters = lapply(chain.state@log.scaling.parameters, function(x){
            exp(x) * step.size.multiplier
        })
        initial.cov.mat = diag(diag(cov(log(simset@parameters))))
    }
    else #this code calculates step sizes and initial cov mat from scratch
    {
        param.medians = suppressWarnings(get.medians(prior))
        init.sds = suppressWarnings(get.sds(prior) / param.medians) / 40
        init.sds[prior@is.improper] = 0.1/40
        
        init.sds[init.sds>1] = (param.medians)[init.sds>1] / 40
        init.sds[grepl('peak.*mult', names(init.sds))] = init.sds[grepl('peak.*mult', names(init.sds))] / 16
        init.sds = init.sds * 2
        
        initial.cov.mat = diag(init.sds^2)
        
        initial.scaling.parameters = 2.38^2/sapply(parameter.var.blocks, length) #the default
    }
    
    setup.mcmc.for.msa(msa=msa,
                       likelihood=likelihood,
                       prior=prior,
                       parameter.var.blocks=parameter.var.blocks,
                       start.value.generator=start.value.generator,
                       chains=chains,
                       n.iter=n.iter,
                       thin=thin,
                       burn=burn,
                       max.sim.time=max.sim.time,
                       save.dir=save.dir,
                       cache.dir=cache.dir,
                       update.frequency=update.frequency,
                       cache.frequency = cache.frequency,
                       save.suffix=save.suffix,
                       target.acceptance.rate = target.acceptance.rate,
                       run=run,
                       verbose=verbose,
                       
                       initial.cov.mat = initial.cov.mat,
                       initial.scaling.parameters = initial.scaling.parameters,
                       
                       SCALING.UPDATE.DECAY = SCALING.UPDATE.DECAY,
                       SCALING.UPDATE.PRIOR = SCALING.UPDATE.PRIOR,
                       SCALING.BASE.UPDATE = SCALING.BASE.UPDATE,
                       
                       COV.UPDATE.DECAY = COV.UPDATE.DECAY,
                       COV.UPDATE.PRIOR = COV.UPDATE.PRIOR,
                       COV.BASE.UPDATE = COV.BASE.UPDATE,
                       
                       plot.first.sim=run
    )
}

create.start.value.generator.for.msa <- function(msa)
{
    files = list.files(file.path(SYSTEMATIC.ROOT.DIR, 'systematic_initial'))
    full.files = list.files(file.path(SYSTEMATIC.ROOT.DIR, 'systematic_initial'), full.names = T)
    
    mask = grepl(msa, files)
    if (!any(mask))
        stop(paste0("No initial mcmc runs have been done for '", msa.names(msa), "' MSA (", msa, ')'))
    load(full.files[mask][sum(mask)])
    
    simset = extract.simset(mcmc, additional.burn=mcmc@n.iter/2)
    sampling.dist = create.starting.sampling.distribution(simset)
    save(sampling.dist, file=file.path(SYSTEMATIC.ROOT.DIR, 'starting_value_generators',
                                       paste0(msa, '.Rdata')))
}

##-----------------------##
##-- RUN PARALLEL MCMC --##
##-----------------------##

setup.parallel.mcmc.for.msa <- function(msa,
                                        likelihood=NULL,
                                        prior=parameters.prior,
                                        parameter.var.blocks = PARAMETER.VAR.BLOCKS.1,
                                        start.value.generator=NULL,
                                        chains=4,
                                        n.iter=100000,
                                        thin=100,
                                        burn=0,
                                        max.sim.time=20,
                                        save.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_parallel'),
                                        cache.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_caches'),
                                        update.frequency=200,
                                        cache.frequency=1000,
                                        save.suffix='',
                                        
                                        target.acceptance.rate=0.238,
                                        COV.UPDATE.PRIOR=500,
                                        SCALING.UPDATE.PRIOR=100,
                                        SCALING.UPDATE.DECAY=.5,
                                        
                                        run=F,
                                        verbose=T)
{
    # Likelihood
    if (is.null(likelihood))
    {
        if (verbose)
            print("Creating Likelihood")
        likelihood = create.msa.likelihood(msa)
    }
    
    # Pull Initial MCMC
    if (verbose)
        print("Loading the initial MCMC")
    files = list.files(file.path(SYSTEMATIC.ROOT.DIR, 'systematic_initial'))
    full.files = list.files(file.path(SYSTEMATIC.ROOT.DIR, 'systematic_initial'), full.names = T)
    
    mask = grepl(msa, files)
    if (!any(mask))
        stop(paste0("No initial mcmc runs have been done for '", msa.names(msa), "' MSA (", msa, ')'))
    load(full.files[mask][sum(mask)])
    
    # Set up start value generator
    if (verbose)
        print("Creating starting value generator")
    simset = extract.simset(mcmc, additional.burn=mcmc@n.iter/2)
    start.value.generator = create.starting.sampling.distribution(simset, correlated.sd.inflation = .75, uncorrelated.sd.inflation = .5)
    
    # Pull chain state variables
    chain.state = mcmc@chain.states[[1]]
    initial.scaling.parameters = lapply(chain.state@log.scaling.parameters, function(x){
        exp(x)/2
    })
    initial.cov.mat = 0.5 * chain.state@cov.mat + 0.5 * diag(diag(chain.state@cov.mat))
    
    # Pass to sub function
    setup.mcmc.for.msa(msa=msa,
                       likelihood=likelihood,
                       prior=prior,
                       parameter.var.blocks=parameter.var.blocks,
                       start.value.generator=start.value.generator,
                       chains=chains,
                       n.iter=n.iter,
                       thin=thin,
                       burn=burn,
                       max.sim.time=max.sim.time,
                       save.dir=save.dir,
                       cache.dir=cache.dir,
                       update.frequency=update.frequency,
                       cache.frequency = cache.frequency,
                       save.suffix=save.suffix,
                       target.acceptance.rate = target.acceptance.rate,
                       
                       initial.cov.mat = initial.cov.mat,
                       initial.scaling.parameters = initial.scaling.parameters,
                       
                       COV.UPDATE.PRIOR=COV.UPDATE.PRIOR,
                       SCALING.UPDATE.PRIOR=SCALING.UPDATE.PRIOR,
                       SCALING.UPDATE.DECAY=SCALING.UPDATE.DECAY,
                       
                       run=run,
                       verbose=verbose
    )
    
}

setup.mcmc.for.msa <- function(msa,
                               likelihood=NULL,
                               prior=parameters.prior,
                               parameter.var.blocks = PARAMETER.VAR.BLOCKS.1,
                               start.value.generator=NULL,
                               chains=4,
                               n.iter=50000,
                               thin=100,
                               burn=0,
                               max.sim.time=Inf,
                               save.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_parallel'),
                               cache.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_caches'),
                               update.frequency=200,
                               cache.frequency=2000,
                               save.suffix='',
                               
                               target.acceptance.rate=0.238,
                               N.ITER.BEFORE.COV = 0,
                               ADAPTIVE.SCALING='componentwise',
                               SCALING.BASE.UPDATE = 1,
                               SCALING.UPDATE.PRIOR=100,
                               SCALING.UPDATE.DECAY=.5,
                               COV.BASE.UPDATE=1,
                               COV.UPDATE.PRIOR=500,
                               COV.UPDATE.DECAY=1,
                               initial.cov.mat=NULL,
                               initial.scaling.parameters=NULL,
                               run=F,
                               plot.first.sim=run,
                               verbose=T)
{
    
    # Likelihood
    if (is.null(likelihood))
    {
        if (verbose)
            print("Creating Likelihood")
        likelihood = create.msa.likelihood(msa)
    }
    
    #-- Start Values --#
    
    if (is.null(start.value.generator))
    {
        load(file.path(SYSTEMATIC.ROOT.DIR, 'starting_value_generators',
                       paste0(msa, '.Rdata')))
        start.value.generator = sampling.dist
    }
    
    if (is(start.value.generator, 'Distribution'))
        start.values = generate.random.samples(start.value.generator, chains)
    else
        start.values = start.value.generator(chains)
    
    if (chains==1)
        first.start.values=start.values
    else
        first.start.values=start.values[1,]
    
    if (verbose)
        print("Creating run.simulation function")
    
    run.simulation = create.run.simulation.function(msa=msa,
                                                    start.values=first.start.values,
                                                    max.sim.time=max.sim.time)
    
    
    if (plot.first.sim)
    {
        if (verbose)
            print("Running initial simulation to plot")
        init.sim = run.simulation(first.start.values)
#        print(plot.calibration.risk(init.sim) + ggtitle(paste0("Initial Sim: ", msa.names(msa))))
        print(plot.calibration.sex(init.sim, data.types='prep') + ggtitle(paste0("Initial Sim: ", msa.names(msa))))
    }
    
    #-- MCMC Control --#
    if (verbose)
        print("Creating MCMC Control")
    
    if (is.null(initial.cov.mat))
    {
        param.medians = suppressWarnings(get.medians(prior))
        init.sds = suppressWarnings(get.sds(prior) / param.medians) / 40
        init.sds[prior@is.improper] = 0.1/40
        
        init.sds[init.sds>1] = (param.medians)[init.sds>1] / 40
        init.sds = init.sds * 2
        initial.cov.mat = diag(init.sds^2)
    }
    if (is.null(initial.scaling.parameters))
    {
        initial.scaling.parameters = 2.38^2/sapply(parameter.var.blocks, length)
    }
    else if (length(initial.scaling.parameters) != length(parameter.var.blocks)) #this code just maps the 
    {
        initial.scaling.parameters = lapply(parameter.var.blocks, function(block){
            superset.mask = sapply(initial.scaling.parameters, function(params){
                length(setdiff(names(params), block))==0
            })
            if (any(superset.mask))
            {
                index = (1:length(initial.scaling.parameters))[superset.mask][1]              
                initial.scaling.parameters[[index]][block]
            }
            else
            {
                rv = sapply(block, function(var.in.block){
                    mask = sapply(initial.scaling.parameters, function(params){
                        any(names(params)==var.in.block)
                    })
                    index = (1:length(initial.scaling.parameters))[mask][1]
                    initial.scaling.parameters[[index]][var.in.block]
                })
                names(rv) = block
                rv
            }
        })
        names(initial.scaling.parameters) = names(parameter.var.blocks)
    }
    transformations = sapply(prior@var.names, function(v){'log'})
    logit.transform.mask = grepl('max.proportion', names(transformations)) &
        !grepl('\\.or', names(transformations))
    transformations[logit.transform.mask] = 'logit'
    
    ctrl = create.adaptive.blockwise.metropolis.control(var.names=prior@var.names,
                                                        simulation.function=run.simulation,
                                                        log.prior.distribution = get.density.function(prior),
                                                        log.likelihood = likelihood,
                                                        burn=burn, thin=thin,
                                                        var.blocks = parameter.var.blocks,
                                                        reset.adaptive.scaling.update.after = 0,
                                                        transformations = transformations,
                                                        
                                                        initial.covariance.mat = initial.cov.mat,
                                                        initial.scaling.parameters = initial.scaling.parameters,
                                                        
                                                        target.acceptance.probability=target.acceptance.rate,
                                                        
                                                        n.iter.before.use.adaptive.covariance = N.ITER.BEFORE.COV,
                                                        adaptive.covariance.base.update = COV.BASE.UPDATE,
                                                        adaptive.covariance.update.prior.iter = COV.UPDATE.PRIOR,
                                                        adaptive.covariance.update.decay = COV.UPDATE.DECAY,
                                                        adaptive.scaling = ADAPTIVE.SCALING,
                                                        adaptive.scaling.base.update = SCALING.BASE.UPDATE,
                                                        adaptive.scaling.update.prior.iter= SCALING.UPDATE.PRIOR,
                                                        adaptive.scaling.update.decay= SCALING.UPDATE.DECAY
    )
    
    #-- Set up the MCMC cache --#
    if (save.suffix != '')
        save.suffix = paste0('_', save.suffix)
    cache.dir = file.path(cache.dir, paste0(msa, '_', chains, 'x', n.iter/1000, 'K',
                                            save.suffix, '_', Sys.Date()))
    
    remove.mcmc.cache(cache.dir)
    
    if (verbose)
        print("Creating the cache")
    
    create.mcmc.cache(dir=cache.dir,
                      control=ctrl,
                      n.iter=n.iter,
                      starting.values = start.values,
                      cache.frequency = cache.frequency)
    
    
    metadata = list(n.chains=chains,
                    save.dir=save.dir,
                    location=msa)
    
    save(metadata, file=file.path(cache.dir, 'metadata.Rdata'))
    
    if (run)
    {
        if (verbose)
            print("Running MCMC")
        run.mcmc.for.msa.cache(cache.dir,
                               update.detail='high',
                               update.frequency=update.frequency)
    }
    else
    {
        if (verbose)
            print("Done")
        NULL
    }
}


create.run.simulation.function <- function(msa,
                                           start.values,
                                           max.sim.time=Inf)
{
    #-- Init Components --#
    base.components = setup.initial.components(msa=msa)
    
    init.components = get.components.for.calibrated.parameters(start.values, base.components)
    init.components = fix.components.for.calibration(components = init.components)
    
    #-- Run Function --#
    run.simulation <- function(parameters)
    {
        tryCatch({
            components = get.components.for.calibrated.parameters(parameters, init.components)
            sim = run.jheem.from.components(components, max.run.time.seconds = max.sim.time)
            
            if (sum(sapply(sim, function(x){sum(is.na(x))}))>0)
                stop("NA values in simulation")
            
            sim
        },
        error = function(e){
            cat("----------------------------------\n")
            cat("THERE WAS AN ERROR RUNNING THE SIMULATION:\n")
            cat(e$message, '\n')
            
            error.file = file.path('mcmc_runs/errors/', paste0('error_', msa, '_', Sys.Date()))
            
            save(e,
                 parameters,
                 components,
                 init.components,
                 file=error.file)
            
            cat("Saving the details to '", error.file, "'\n and allowing the mcmc to continue.\n", sep='')
            cat("----------------------------------\n")
            
            list(terminated=T)
        })
    }
    
    run.simulation
}

run.mcmc.for.msa.cache <- function(cache.dir,
                                   chains=NULL,
                                   update.detail='high',
                                   update.frequency=200,
                                   remove.cache.when.done=F)
{
    cache.dir = file.path(cache.dir)
    load(file.path(cache.dir, 'metadata.Rdata'))
    if (is.null(chains))
        chains = 1:metadata$n.chains
    
    print(paste0("Running MCMC (",
                 ifelse(length(chains)==1, 'chain ', 'chains '),
                 paste0(chains, collapse=','),
                 " of ", metadata$n.chains,
                 ") for ", msa.names(metadata$location)))
    
    mcmc = run.mcmc.from.cache(dir=cache.dir,
                               chains=chains,
                               update.frequency = update.frequency,
                               update.detail = update.detail)
    
    if (remove.cache.when.done)
    {
        if (metadata$n.chains==mcmc@n.chains)
            to.save=T
        else if (is.mcmc.cache.complete(cache.dir))
        {
            mcmc = assemble.mcmc.from.cache(cache.dir)
            to.save=T
        }
        else
            to.save=F
        
        
        #-- Save and return --#
        if (to.save)
        {
            filename = paste0(basename(cache.dir), '.Rdata')
            save(mcmc, file=file.path(metadata$save.dir, filename))
            file.remove(file.path(cache.dir, 'metadata.Rdata'))
            remove.mcmc.cache(cache.dir)
        }
    }
    
    mcmc
    
}
