N.ITER.BEFORE.COV = 0
ADAPTIVE.SCALING='componentwise'
SCALING.BASE.UPDATE = 1
SCALING.UPDATE.PRIOR=100
SCALING.UPDATE.DECAY=.5
COV.BASE.UPDATE=1
COV.UPDATE.PRIOR=500
COV.UPDATE.DECAY=1



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
                             save.dir='../results/mcmc_runs',
                             cache.dir='../results/mcmc_caches',
                             update.frequency=200,
                             cache.frequency=2000,
                             save.suffix='',
                             resume.cache=NULL)
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
        load(file.path('code/systematic_calibration/starting_value_generators',
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
    param.medians = suppressWarnings(get.medians(prior))
    init.sds = suppressWarnings(get.sds(prior) / param.medians) / 40
    init.sds[prior@is.improper] = 0.1/40
        
    init.sds[init.sds>1] = (param.medians)[init.sds>1] / 40
    init.sds = init.sds * 2
    transformations = sapply(prior@var.names, function(v){'log'})

    ctrl = create.adaptive.blockwise.metropolis.control(var.names=prior@var.names,
                                                        simulation.function=run.simulation,
                                                        log.prior.distribution = get.density.function(prior),
                                                        log.likelihood = likelihood,
                                                        initial.covariance.mat = diag(init.sds^2),
                                                        burn=burn, thin=thin,
                                                        var.blocks = parameter.var.blocks,
                                                        reset.adaptive.scaling.update.after = 0,
                                                        transformations = transformations,

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
    cache.dir = file.path(cache.dir, paste0(msa, save.suffix, '_', Sys.Date()))

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
    filename = paste0(msa, save.suffix, "_", Sys.Date(), ".Rdata")
    save(mcmc, file=filename)
    mcmc
}


create.msa.likelihood <- function(msa,
                                  EVERYTHING.MULT=sqrt(6),
                                  prev.to.new.cv.ratio=1,
                                  FOCUS.WEIGHT=4,
                                  DX.WEIGHT=36,
                                  verbose=F)
{
    TOTAL.WEIGHT = 16
    PREV.INFLATION = prev.to.new.cv.ratio * get.cv.weights(location=msa, weight.to='new')['prevalence']

    SD.INFLATION.PREV.NUM = sqrt(1)*EVERYTHING.MULT*PREV.INFLATION#sqrt(6) * 8.4
    SD.INFLATION.NEW.NUM = sqrt(1)*EVERYTHING.MULT# * 1.6

    to.focus = function(description){
        (grepl(' male',description) & (grepl('13-24', description) | grepl('25-34',description))) |
            (grepl('black, msm$',description) | grepl('hispanic, msm$', description))
    }
    SD.INFLATION.PREV = function(description){SD.INFLATION.PREV.NUM /
            (sqrt(FOCUS.WEIGHT)^as.numeric(to.focus(description)))}
    SD.INFLATION.NEW = function(description){SD.INFLATION.NEW.NUM /
            (sqrt(FOCUS.WEIGHT)^as.numeric(to.focus(description)))}

    SD.INFLATION.MORT = sqrt(1)*EVERYTHING.MULT
    SD.INFLATION.DX = sqrt(1)/sqrt(DX.WEIGHT)*EVERYTHING.MULT
    PREV.SD = function(years, num){sqrt(0.5 * (0.09*num)^2 + 0.5 * (num^0.69)^2)}
    NEW.SD = function(years, num){sqrt(0.5 * (0.065*num)^2 + 0.5 * (num^0.33)^2)}
    MORT.SD = PREV.SD
    
    
    missing.mort.fraction = mean(c(.02, 458 / (1926 + 12219)))
    #2% in NDI alone = https://academic.oup.com/aje/article/174/1/90/126134
    #458 / (1926 + 12219) #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2773949/
    #458 in NDI alone
    
    MORT.BIAS = function(years, num){num - num/(1-missing.mort.fraction)}
    MORT.BIAS.SD = function(years, num){missing.mort.fraction * num / 2}
    
    
    DX.SD=function(...){0}
    USE.TOTAl.PREV = T
    SD.INFLATION.TOTAL.PREV = SD.INFLATION.PREV.NUM/sqrt(TOTAL.WEIGHT)*PREV.INFLATION*EVERYTHING.MULT
    SD.INFLATION.TOTAL.NEW = SD.INFLATION.NEW.NUM/sqrt(TOTAL.WEIGHT)*EVERYTHING.MULT
    TOTAL.PREV.SD = PREV.SD
    TOTAL.NEW.SD = NEW.SD #function(years, num){NEW.SD(years, num) * 2^((2010-years)/(2010-1993))}
    IDU.LOG.SD = log(2)/8
    
    SD.INFLATION.AIDS.DIAGNOSES = EVERYTHING.MULT
    AIDS.SD = function(...){0}#NEW.SD
    AIDS.TO.HIV.RATIO.LOG.SD = 0.5 * log(1.1)
    
    SD.INFLATION.CUM.MORT = EVERYTHING.MULT * 2
#    CUM.MORT.SD = function(years,num){sqrt(5^2 + 0.5 * (0.065*num)^2 + 0.5 * (num^0.33)^2)}
    CUM.MORT.SD = function(years, num){2 * sqrt(5^2 + 0.5 * (0.09*num)^2 + 0.5 * (num^0.69)^2)}
    
    full.likelihood = create.full.likelihood(location = msa,
                                             total.new.years = 2008:2010,
                                             prevalence.numerator.sd = PREV.SD,
                                             new.numerator.sd = NEW.SD,
                                             mortality.numerator.sd = MORT.SD,
                                             mortality.bias.fn = MORT.BIAS,
                                             mortality.bias.sd = MORT.BIAS.SD,
                                             diagnosed.numerator.sd = DX.SD,
                                             new.sd.inflation = SD.INFLATION.NEW,
                                             prevalence.sd.inflation = SD.INFLATION.PREV,
                                             mortality.sd.inflation = SD.INFLATION.MORT,
                                             diagnosed.sd.inflation = SD.INFLATION.DX,
                                             total.new.numerator.sd = TOTAL.NEW.SD,
                                             total.prevalence.numerator.sd = TOTAL.PREV.SD,
                                             cumulative.mortality.numerator.sd = CUM.MORT.SD,
                                             total.new.sd.inflation = SD.INFLATION.TOTAL.NEW,
                                             total.prevalence.sd.inflation = SD.INFLATION.TOTAL.PREV,
                                             cumulative.mortality.sd.inflation = SD.INFLATION.CUM.MORT,
                                             use.total.prevalence = USE.TOTAl.PREV,
                                             idu.log.sd = IDU.LOG.SD,
                                             
                                             aids.diagnoses.numerator.sd = AIDS.SD,
                                             aids.diagnoses.sd.inflation = SD.INFLATION.AIDS.DIAGNOSES,
                                             aids.diagnoses.to.hiv.ratio.log.sd = AIDS.TO.HIV.RATIO.LOG.SD,
                                             
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
                                               SD.INFLATION.DX=SD.INFLATION.DX,
                                               SD.INFLATION.CUM.MORT=SD.INFLATION.CUM.MORT,
                                               CUM.MORT.SD=CUM.MORT.SD,
                                               TOTAL.NEW.SD = TOTAL.NEW.SD,
                                               TOTAL.PREV.SD = TOTAL.PREV.SD,
                                               SD.INFLATION.TOTAL.NEW = SD.INFLATION.TOTAL.NEW,
                                               SD.INFLATION.TOTAL.PREV = SD.INFLATION.TOTAL.PREV,
                                               USE.TOTAl.PREV=USE.TOTAl.PREV,
                                               TOTAL.WEIGHT=TOTAL.WEIGHT,
                                               FOCUS.WEIGHT=FOCUS.WEIGHT,
                                               IDU.LOG.SD=IDU.LOG.SD,
                                               DX.WEIGHT=DX.WEIGHT,
                                               SD.INFLATION.AIDS.DIAGNOSES = SD.INFLATION.AIDS.DIAGNOSES,
                                               AIDS.SD = AIDS.SD,
                                               AIDS.TO.HIV.RATIO.LOG.SD = AIDS.TO.HIV.RATIO.LOG.SD)

    full.likelihood
}
