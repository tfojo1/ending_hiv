
source('code/systematic_calibration/systematic_settings.R')
source('code/systematic_calibration/starting_value_generator.R')


##-----------------##
##-- RUN INITIAL --##
##-----------------##

run.initial.mcmc.for.msa <- function(msa,
                                     likelihood=create.msa.likelihood(msa),
                                     prior=parameters.prior,
                                     parameter.var.blocks = PARAMETER.VAR.BLOCKS.1,
                                     start.value.generator=NULL,
                                     chains=1,
                                     n.iter=20000,
                                     thin=20,
                                     burn=0,
                                     max.sim.time=Inf,
                                     save.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_initial'),
                                     cache.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_caches'),
                                     update.frequency=200,
                                     cache.frequency=500,
                                     save.suffix='',
                                     resume.cache=NULL,
                                     target.acceptance.rate=0.1)
{
    if (is.null(start.value.generator))
    {
        load(file.path(SYSTEMATIC.ROOT.DIR, 'starting_value_generators/47900.Rdata'))
        start.value.generator = sampling.dist
    }
    
    run.mcmc.for.msa(msa=msa,
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
                     resume.cache=resume.cache,
                     target.acceptance.rate = target.acceptance.rate
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

run.parallel.mcmc.for.msa <- function(msa,
                                      likelihood=create.msa.likelihood(msa),
                                      prior=parameters.prior,
                                      parameter.var.blocks = PARAMETER.VAR.BLOCKS.1,
                                      start.value.generator=NULL,
                                      chains=4,
                                      n.iter=60000,
                                      thin=80,
                                      burn=0,
                                      max.sim.time=Inf,
                                      save.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_parallel'),
                                      cache.dir=file.path(SYSTEMATIC.ROOT.DIR, 'systematic_caches'),
                                      update.frequency=200,
                                      cache.frequency=800,
                                      save.suffix='',
                                      resume.cache=NULL,
                                      
                                      target.acceptance.rate=0.238,
                                      COV.UPDATE.PRIOR=500,
                                      SCALING.UPDATE.PRIOR=100,
                                      SCALING.UPDATE.DECAY=.5)
{
    # Pull Initial MCMC
    files = list.files(file.path(SYSTEMATIC.ROOT.DIR, 'systematic_initial'))
    full.files = list.files(file.path(SYSTEMATIC.ROOT.DIR, 'systematic_initial'), full.names = T)
    
    mask = grepl(msa, files)
    if (!any(mask))
        stop(paste0("No initial mcmc runs have been done for '", msa.names(msa), "' MSA (", msa, ')'))
    load(full.files[mask][sum(mask)])
    
    # Set up start value generator
    simset = extract.simset(mcmc, additional.burn=mcmc@n.iter/2)
    start.value.generator = create.starting.sampling.distribution(simset, correlated.sd.inflation = .75, uncorrelated.sd.inflation = .5)

    # Pull chain state variables
    chain.state = mcmc@chain.states[[1]]
    initial.scaling.parameters = lapply(chain.state@log.scaling.parameters, function(x){
        exp(x)/2
    })
    initial.cov.mat = 0.5 * chain.state@cov.mat + 0.5 * diag(diag(chain.state@cov.mat))
    
    # Pass to sub function
    run.mcmc.for.msa(msa=msa,
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
                     resume.cache=resume.cache,
                     target.acceptance.rate = target.acceptance.rate,
                     
                     initial.cov.mat = initial.cov.mat,
                     initial.scaling.parameters = initial.scaling.parameters,
                     
                     COV.UPDATE.PRIOR=COV.UPDATE.PRIOR,
                     SCALING.UPDATE.PRIOR=SCALING.UPDATE.PRIOR,
                     SCALING.UPDATE.DECAY=SCALING.UPDATE.DECAY
    )
    
}

run.mcmc.for.msa <- function(msa,
                             likelihood=create.msa.likelihood(msa),
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
                             resume.cache=NULL,
                             
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
                             initial.scaling.parameters=NULL)
{
    #-- Run Function --#
    run.simulation <- function(parameters)
    {
        components = get.components.for.calibrated.parameters(parameters, init.components)
        sim = run.jheem.from.components(components, max.run.time.seconds = max.sim.time)
        sim
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


    #-- Init Components --#
    base.components = setup.initial.components(msa=msa)
    if (chains==1)
        first.start.values=start.values
    else
        first.start.values=start.values[1,]

    init.components = get.components.for.calibrated.parameters(first.start.values, base.components)
    init.components = fix.components.for.calibration(components = init.components)
#    init.components = fix.components.for.calibration(components = base.components)


    #-- MCMC Control --#
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
    transformations = sapply(prior@var.names, function(v){'log'})

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

    #-- Run the MCMC --#
    if (save.suffix != '')
        save.suffix = paste0('_', save.suffix)
    cache.dir = file.path(cache.dir, paste0(msa, '_', chains, 'x', n.iter/1000, 'K',
                                            save.suffix, '_', Sys.Date()))

    if (!is.null(resume.cache))
    {
        mcmc = run.mcmc.from.cache(dir = resume.cache,
                                   update.frequency = update.frequency,
                                   update.detail = 'high')
    }
    else
    {
        remove.mcmc.cache(cache.dir)

        print(paste0("Running MCMC for ", msa.names(msa)))
        mcmc = run.mcmc(control=ctrl,
                        n.iter=n.iter,
                        starting.values = start.values,
                        update.frequency = update.frequency,
                        update.detail = 'high',
                        cache.frequency = cache.frequency,
                        cache.dir = cache.dir
        )
    }

    #-- Save and return --#
    filename = paste0(msa, '_', chains, 'x', n.iter/1000, 'K',
                      save.suffix, "_", Sys.Date(), ".Rdata")
    save(mcmc, file=file.path(save.dir, filename))
    mcmc
}


create.msa.likelihood <- function(msa,
                                  EVERYTHING.MULT=sqrt(6),
                                  prev.to.new.cv.ratio=1,
                                  FOCUS.WEIGHT=4,
                                  TOTAL.DX.WEIGHT=1/16,
                                  STRATIFIED.DX.WEIGHT=1/128,
                                  SUPPRESSION.WEIGHT=1/16,
                                  verbose=F,
                                  
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
    TOTAL.WEIGHT = 16

    # The demographic groups to which we want to give added weight
    to.focus = function(description){
        (grepl(' male',description) & (grepl('13-24', description) | grepl('25-34',description))) |
            (grepl('black, msm$',description) | grepl('hispanic, msm$', description))
    }
    
    #-- Elements for New Diagnoses --#
    SD.INFLATION.NEW.NUM = sqrt(1)*EVERYTHING.MULT# * 1.6
    SD.INFLATION.NEW = function(description){SD.INFLATION.NEW.NUM /
            (sqrt(FOCUS.WEIGHT)^as.numeric(to.focus(description)))}
    
    NEW.SD = function(years, num){sqrt(0.5 * (0.065*num)^2 + 0.5 * (num^0.33)^2)}
    
    
    #-- Elements for Prevalence --#
    PREV.INFLATION = prev.to.new.cv.ratio * get.cv.weights(location=msa, weight.to='new')['prevalence']
    
    SD.INFLATION.PREV.NUM = sqrt(1)*EVERYTHING.MULT*PREV.INFLATION#sqrt(6) * 8.4
    SD.INFLATION.PREV = function(description){SD.INFLATION.PREV.NUM /
            (sqrt(FOCUS.WEIGHT)^as.numeric(to.focus(description)))}
    
    PREV.SD = function(years, num){sqrt(0.5 * (0.09*num)^2 + 0.5 * (num^0.69)^2)}
    
    
    #-- Elements for Mortality --#
    SD.INFLATION.MORT = sqrt(1)*EVERYTHING.MULT
    MORT.SD = PREV.SD
    
    missing.mort.fraction = mean(c(.02, 458 / (1926 + 12219)))
    #2% in NDI alone = https://academic.oup.com/aje/article/174/1/90/126134
    #458 / (1926 + 12219) #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2773949/
    #458 in NDI alone
    
    MORT.BIAS = function(years, num){num - num/(1-missing.mort.fraction)}
    MORT.BIAS.SD = function(years, num){missing.mort.fraction * num / 2}
    
    #-- Elements for Awareness of Diagnosis --#
    TOTAL.SD.INFLATION.DX = sqrt(1)/sqrt(TOTAL.DX.WEIGHT)*EVERYTHING.MULT
    STRATIFIED.SD.INFLATION.DX = sqrt(1)/sqrt(STRATIFIED.DX.WEIGHT)*EVERYTHING.MULT
    DX.SD=function(...){0}
    
    
    #-- Elements for Suppression --#
    SUPPRESSED.SD = function(...){.01}
    SUPPRESSION.SD.INFLATION = 1/sqrt(SUPPRESSION.WEIGHT)
    
    #-- Elements for IDU --#
    IDU.LOG.SD = log(2)/8
    
    
    #-- Elements for Historical AIDS Diagnoses --#
    SD.INFLATION.AIDS.DIAGNOSES = EVERYTHING.MULT
    AIDS.SD = function(...){0}#NEW.SD
    AIDS.TO.HIV.RATIO.LOG.SD = 0.5 * log(1.1)
    
    #-- Elements for Historical Cumulative Diagnoses --#
    SD.INFLATION.CUM.MORT = EVERYTHING.MULT * 2
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
                                                  sd.inflation=SUPPRESSION.SD.INFLATION)
    
    dx.lik = create.knowledge.of.status.likelihood(location=msa,
                                                   surv=msa.surveillance,
                                                   state.surv=state.surveillance,
                                                   census.totals=ALL.DATA.MANAGERS$census.totals,
                                                   knowledge.of.status.regressions=NULL,
                                                   years=diagnosed.years,
                                                   total.sd.inflation=TOTAL.SD.INFLATION.DX,
                                                   stratified.ors.sd.inflation=STRATIFIED.SD.INFLATION.DX,
                                                   total.rho=0.5)
    
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
                                                hiv.to.aids.diagnoses.ratio.log.sd=AIDS.TO.HIV.RATIO.LOG.SD)
    
    
    
    #Join them together
    full.likelihood = create.joint.likelihood.function(new=new.lik,
                                                       prev=prev.lik, 
                                                       mort=mort.lik,
                                                       dx=dx.lik,
                                                       supp=suppressed.lik,
                                                       idu=idu.lik,
                                                       cum.mort=cum.mort.lik, 
                                                       aids=aids.lik,
                                                       verbose=verbose)


    attr(full.likelihood, 'parameters') = list(EVERYTHING.MULT=EVERYTHING.MULT,
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

    full.likelihood
}
