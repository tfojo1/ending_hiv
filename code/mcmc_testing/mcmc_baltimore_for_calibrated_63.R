if (1==2)
{
    setwd('../../../Ending HIV/jheem/')
}

source('../code/source_code.R')
load('../code/cached_surveillance/msa.surveillance.estimated.correct.to.county.Rdata')

source('../code/calibration/calibrated_parameters_63_helpers.R')
source('../code/calibration/calibrated_parameters_63.R')

# MCMC Parameters
BURN = 0
KEEP = 2000
ITER.AFTER.BURN = 20000
THIN = 25
CACHE.FREQUENCY = 500
FILE.PREFIX = 'balt63_half.mult.half.exp_all.x3_prev.x2.6_idu.2010.wt.4'
DESCRIPTION = "V63: half mult error sd, half exp error sd; all x3 + prev x2.6"
RESUME.PRIOR.CACHE = NULL#'../code/mcmc_runs/balt63_half.mult.half.exp_all.x3_prev.x2.6_idu.2010.wt.4_2020-04-20/'
if (!is.null(RESUME.PRIOR.CACHE))
    print("THIS IS RESUMPTION OF A PRIOR CACHE!!!")

EVERYTHING.MULT = 3
TOTAL.WEIGHT = 8
PREV.INFLATION = 2.6#5.2

SD.INFLATION.PREV = sqrt(1)*EVERYTHING.MULT*PREV.INFLATION#sqrt(6) * 8.4
SD.INFLATION.NEW = sqrt(1)*EVERYTHING.MULT# * 1.6
SD.INFLATION.MORT = sqrt(1)*EVERYTHING.MULT
SD.INFLATION.DX = sqrt(1)/sqrt(16)*EVERYTHING.MULT
PREV.SD = function(years, num){sqrt(0.5 * (0.09*num)^2 + 0.5 * (num^0.69)^2)}
NEW.SD = function(years, num){sqrt(0.5 * (0.065*num)^2 + 0.5 * (num^0.33)^2)}
MORT.SD = NEW.SD
DX.SD=function(...){0}
USE.TOTAl.PREV = T
SD.INFLATION.TOTAL.PREV = SD.INFLATION.PREV/sqrt(TOTAL.WEIGHT)*PREV.INFLATION*EVERYTHING.MULT
SD.INFLATION.TOTAL.NEW = SD.INFLATION.NEW/sqrt(TOTAL.WEIGHT)*EVERYTHING.MULT
TOTAL.PREV.SD = PREV.SD
TOTAL.NEW.SD = function(years, num){NEW.SD(years, num) * 2^((2010-years)/(2010-1993))}
IDU.LOG.SD = log(2)/8

SHOW.PLOTS = F
UPDATE.FREQUENCY = 200
MAX.SIM.TIME = 10

N.ITER.BEFORE.COV = 0
ADAPTIVE.SCALING='componentwise'
SCALING.BASE.UPDATE = 1
SCALING.UPDATE.PRIOR=100
SCALING.UPDATE.DECAY=.5
COV.BASE.UPDATE=1
COV.UPDATE.PRIOR=500
COV.UPDATE.DECAY=1


PARAMETER.VAR.BLOCKS = PARAMETER.VAR.BLOCKS.1

if (1==2)
{
    BURN = 0
    ITER.AFTER.BURN = 100
    CACHE.FREQUENCY = 100
    THIN=1
    FILE.PREFIX = 'test'
}

SAVE.TO.FILE = paste0('../code/mcmc_runs/', FILE.PREFIX, "_", (BURN+ITER.AFTER.BURN), "_", Sys.Date(), ".Rdata")
CACHE.DIR = paste0('../code/mcmc_runs/', FILE.PREFIX, "_", Sys.Date())

set.seed(409832)


# Set up the likelihood

cat('Building likelihood...')
full.likelihood = create.full.likelihood(verbose=F,
                                         prevalence.numerator.sd = PREV.SD,
                                         new.numerator.sd = NEW.SD,
                                         mortality.numerator.sd = MORT.SD,
                                         diagnosed.numerator.sd = DX.SD,
                                         new.sd.inflation = SD.INFLATION.NEW,
                                         prevalence.sd.inflation = SD.INFLATION.PREV,
                                         mortality.sd.inflation = SD.INFLATION.MORT,
                                         diagnosed.sd.inflation = SD.INFLATION.DX,
                                         total.new.numerator.sd = TOTAL.NEW.SD,
                                         total.prevalence.numerator.sd = TOTAL.PREV.SD,
                                         total.new.sd.inflation = SD.INFLATION.TOTAL.NEW,
                                         total.prevalence.sd.inflation = SD.INFLATION.TOTAL.PREV,
                                         use.total.prevalence = USE.TOTAl.PREV,
                                         idu.log.sd = IDU.LOG.SD)
cat('Done\n')


likelihood.parameters = list(PREV.SD=PREV.SD,
                             NEW.SD=NEW.SD,
                             MORT.SD=MORT.SD,
                             DX.SD=DX.SD,
                             SD.INFLATION.NEW=SD.INFLATION.NEW,
                             SD.INFLATION.PREV=SD.INFLATION.PREV,
                             SD.INFLATION.MORT=SD.INFLATION.MORT,
                             SD.INFLATION.DX=SD.INFLATION.DX,
                             TOTAL.NEW.SD = TOTAL.NEW.SD,
                             TOTAL.PREV.SD = TOTAL.PREV.SD,
                             SD.INFLATION.TOTAL.NEW = SD.INFLATION.TOTAL.NEW,
                             SD.INFLATION.TOTAL.PREV = SD.INFLATION.TOTAL.PREV,
                             USE.TOTAl.PREV=USE.TOTAl.PREV,
                             TOTAL.WEIGHT=TOTAL.WEIGHT,
                             IDU.LOG.SD=IDU.LOG.SD,

                             N.ITER.BEFORE.COV = N.ITER.BEFORE.COV,
                             ADAPTIVE.SCALING=ADAPTIVE.SCALING,
                             SCALING.BASE.UPDATE = SCALING.BASE.UPDATE,
                             SCALING.UPDATE.PRIOR=SCALING.UPDATE.PRIOR,
                             SCALING.UPDATE.DECAY=SCALING.UPDATE.DECAY,
                             COV.BASE.UPDATE=COV.BASE.UPDATE,
                             COV.UPDATE.PRIOR=COV.UPDATE.PRIOR,
                             COV.UPDATE.DECAY=COV.UPDATE.DECAY
                             )

# Set up the initial components
base.components = setup.initial.components()

init.parameters = c(
    global.trate = 0.1,

    #-- Acute HIV and the Effect of Diagnosis --#
    acute.transmissibility.rr = 12,#16.8,
    diagnosed.transmission.rr = .3,#0.31,

    age1.msm.suppressed.or.slope = 1,
    age2.msm.suppressed.or.slope = 1,

    age1.msm.testing.or = 1,
    age2.msm.testing.or = 1,

    #-- MSM Transmission --#
    black.msm.trate.peak = 3.61,
    black.msm.trate.0 = 1.91,
    black.msm.trate.1 = 1.97,
    black.msm.trate.2 = 1.89,

    hispanic.msm.trate.peak = 3.71,
    hispanic.msm.trate.0 = 1.33,
    hispanic.msm.trate.1 = 1.34,
    hispanic.msm.trate.2 = 1.43,

    other.msm.trate.peak = 1.87,
    other.msm.trate.0 = .719,
    other.msm.trate.1 = .716,
    other.msm.trate.2 = .723,

    #-- Heterosexual Transmission --#
    black.heterosexual.trate.peak = 2.20,
    black.heterosexual.trate.0 = 1.28,
    black.heterosexual.trate.1 = 1.11,
    black.heterosexual.trate.2 = 1.07,

    hispanic.heterosexual.trate.peak = 1.18,
    hispanic.heterosexual.trate.0 = .392,
    hispanic.heterosexual.trate.1 = .421,
    hispanic.heterosexual.trate.2 = .413,

    other.heterosexual.trate.peak = .382,
    other.heterosexual.trate.0 = 0.172,
    other.heterosexual.trate.1 = 0.154,
    other.heterosexual.trate.2 = 0.165,

    #-- IDU Transmission --#
    black.idu.trate.peak = 36.85,
    black.idu.trate.0 = 5.73,
    black.idu.trate.1 = 4.34,
    black.idu.trate.2 = 3.89,

    hispanic.idu.trate.peak = 1.70,
    hispanic.idu.trate.0 = .228,
    hispanic.idu.trate.1 = .196,
    hispanic.idu.trate.2 = .217,

    other.idu.trate.peak = 2.47,
    other.idu.trate.0 = .922,
    other.idu.trate.1 = 1.06,
    other.idu.trate.2 = .797,

    #-- Other Sexual Transmission Parameters --#
    male.vs.female.heterosexual.rr = 0.595,

    age1.msm.susceptibility.rr1 = 1.59,
    age1.msm.susceptibility.rr2 = 1.08,
    age2.msm.susceptibility.rr1 = 1.36,
    age2.msm.susceptibility.rr2 = 1.15,

    age1.susceptibility.rr = 0.767,
    age2.susceptibility.rr = 1.265,
    age4.susceptibility.rr = 0.645,
    age5.susceptibility.rr = 0.248,

    #-- Aging --#
    msm.age1.aging.base = .286,
    msm.age2.aging.0 = .199,
    msm.age3.aging.1 = .246,
    msm.age4.aging.1 = .069,

    heterosexual.age1.aging.base = .135,
    heterosexual.age2.aging.0 = .187,
    heterosexual.age3.aging.1 = .110,
    heterosexual.age4.aging.1 = .041,

    idu.age1.aging.base = .163,
    idu.age2.aging.0 = .228,
    idu.age3.aging.1 = .155,
    idu.age4.aging.1 = .050,

    #-- HIV Testing --#
    heterosexual.proportion.tested.or = .25,#0.251,
    msm.proportion.tested.or =.25*1.5,
    idu.proportion.tested.or = .25*1.1,

    total.testing.slope.or = 1,

    testing.ramp.up.vs.current.rr = 0.401,

    #-- Proportion MSM --#
    proportion.msm.of.male.mult = 1.03,

    #-- IDU Transitions --#
    black.incident.idu.multiplier.0 = 3.33,
    hispanic.incident.idu.multiplier.0 = 1.18,
    other.incident.idu.multiplier.0 = 1.33,

    black.incident.idu.multiplier.2 = 0.76,
    hispanic.incident.idu.multiplier.2 = 0.84,
    other.incident.idu.multiplier.2 = 2.89,

    idu.remission.multiplier = 1.3,

    idu.relapse.multiplier = 1.35,

    #-- HIV-Specific Mortality --#
    hiv.mortality.0= .051,
    hiv.mortality.2 = .0172,
    peak.hiv.mortality = 6.62*0.051,

    #-- Sexual Mixing by Age --#
    age.mixing.sd.mult = 1.18,

    #-- Other Sexual Mixing --#
    oe.female.pairings.with.msm = .116,
    oe.never.idu.pairings.with.idu = .246,

    black.black.sexual.oe = 3.02,
    hispanic.hispanic.sexual.oe = 2.34,
    other.other.sexual.oe = 1.62,

    #-- Suppression --#
    total.suppressed.or = 1.05,

    #-- Uncertainty About the Future --#
    #    current.gains.end.by.year = 2020,
    msm.fraction.trate.change.after.t2 = 0.067,
    heterosexual.fraction.trate.change.after.t2 = 0.124,
    idu.fraction.trate.change.after.t2 = 0.0167
)
init.components = get.components.for.calibrated.parameters(init.parameters, base.components)
init.components = fix.components.for.calibration(components = init.components)

# Run the MCMC

run.simulation <- function(parameters)
{
#    print(length(parameters))
#    print(paste0(names(parameters), '=', parameters, collapse=", "))
    components = get.components.for.calibrated.parameters(parameters, init.components)
    sim = run.jheem.from.components(components, max.run.time.seconds = MAX.SIM.TIME)
    if (sim$terminated)
        print("simulation terminated for time")

#    print(full.likelihood(sim) + calculate.density(parameters.prior, parameters, log=T))
    if (SHOW.PLOTS)
    {
        tryCatch({
            the.plot = suppressWarnings(
                plot.calibration.risk(sim) +
                    ggtitle(paste0('log.lik = ', round(full.likelihood(sim),1),
                                   ' [ ', round(new.likelihood(sim),1),
                                   ' / ', round(prev.likelihood(sim),1),
                                   ' / ', round(mort.likelihood(sim),1),
                                   ' / ', round(dx.likelihood(sim), 1),
                                   ' ], log.prior = ', round(calculate.density(parameters.prior, parameters, log=T),1)
                                   ))
                )
            print(the.plot)
        }, error=function(e){})
    }

    sim
}

if (1==2)
{
    run.simulation <- function(parameters){parameters}
    full.likelihood <- function(sim){dnorm(rnorm(1), log=T)}
}

param.medians = suppressWarnings(get.medians(parameters.prior))
init.sds = suppressWarnings(get.sds(parameters.prior) / param.medians) / 40
init.sds['global.trate'] = 0.1/40
init.sds[init.sds>1] = (param.medians)[init.sds>1] / 40

init.sds = init.sds * 2
#init.sds[PARAMETER.VAR.BLOCKS[['black.susceptibility']]] = 0.25 * init.sds[PARAMETER.VAR.BLOCKS[['black.susceptibility']]]
#init.sds[PARAMETER.VAR.BLOCKS[['proportion.msm.and.mixing']]] = 2 * init.sds[PARAMETER.VAR.BLOCKS[['proportion.msm.and.mixing']]]
#init.sds[PARAMETER.VAR.BLOCKS[['testing']]] = 4 * init.sds[PARAMETER.VAR.BLOCKS[['testing']]]

transformations = sapply(parameters.prior@var.names, function(v){'log'})
#transformations['current.gains.end.by.year'] = 'identity'

ctrl = create.adaptive.blockwise.metropolis.control(var.names=parameters.prior@var.names,
                                             simulation.function=run.simulation,
                                             log.prior.distribution = get.density.function(parameters.prior),
                                             log.likelihood = full.likelihood,
                                             initial.covariance.mat = diag(init.sds^2),
                                             burn=BURN, thin=THIN,
                                             var.blocks = PARAMETER.VAR.BLOCKS,
                                             reset.adaptive.scaling.update.after = 0,#BURN * .6,
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

if (is.null(RESUME.PRIOR.CACHE))
{
    remove.mcmc.cache(CACHE.DIR)
    print(FILE.PREFIX)
    print(DESCRIPTION)
print(paste0("PREPARING TO RUN MCMC WITH ",
             format((ITER.AFTER.BURN+BURN), big.mark=','),
             " ITERATIONS: ", Sys.time()))
start.time=Sys.time()
mcmc = run.mcmc(control=ctrl,
                n.iter=ITER.AFTER.BURN+BURN,
                starting.values = init.parameters,
                update.frequency = UPDATE.FREQUENCY,
                update.detail = 'high',
                cache.frequency = CACHE.FREQUENCY,
                cache.dir = CACHE.DIR
                )
end.time=Sys.time()
run.time = end.time-start.time
print(run.time)
}

if (!is.null(RESUME.PRIOR.CACHE))
{
    CACHE.DIR = RESUME.PRIOR.CACHE
    DESCRIPTION = 'resumed prior'
    SAVE.TO.FILE = file.path('../code/mcmc_runs', paste0(RESUME.PRIOR.CACHE, '.Rdata'))

    print(RESUME.PRIOR.CACHE)
    print(DESCRIPTION)

    print(paste0("PREPARING TO RESUME MCMC"))
    start.time=Sys.time()
    mcmc = run.mcmc.from.cache(dir = CACHE.DIR,
                                 update.frequency = UPDATE.FREQUENCY,
                                 update.detail = 'high')
    end.time=Sys.time()
    run.time = end.time-start.time
    print(run.time)

}

save(mcmc, DESCRIPTION, likelihood.parameters, file=SAVE.TO.FILE)


if (1==2)
{
    simset = extract.simset(mcmc)
    plot.calibration.risk(simset)
}
