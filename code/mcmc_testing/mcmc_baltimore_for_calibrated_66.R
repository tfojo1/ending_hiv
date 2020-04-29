if (1==2)
{
    setwd('../../../Ending HIV/jheem/')
}

source('code/source_code.R')
load('code/cached_surveillance/msa.surveillance.estimated.correct.to.county.Rdata')
source('code/systematic_calibration/systematic_calibration.R')

source('code/calibration/calibrated_parameters_63_helpers.R')
source('code/calibration/calibrated_parameters_66.R')

DC.MSA = cbsa.for.msa.name('Washington,DC')
BALTIMORE.MSA = '12580'
msa = BALTIMORE.MSA

# MCMC Parameters
BURN = 0
KEEP = 2000
ITER.AFTER.BURN = 20000
THIN = 25
CACHE.FREQUENCY = 500
UPDATE.FREQUENCY = 200
MAX.SIM.TIME=Inf

FILE.PREFIX = 'balt.66_focus.wt.9_all.xsqrt(6)_prev.by.cv'
DESCRIPTION = "V63: half mult error sd, half exp error sd; all x3 + prev x2.6"
RESUME.PRIOR.CACHE = NULL#'../code/mcmc_runs/balt63_half.mult.half.exp_all.x3_prev.x2.6_idu.2010.wt.4_2020-04-20/'
if (!is.null(RESUME.PRIOR.CACHE))
    print("THIS IS RESUMPTION OF A PRIOR CACHE!!!")



PARAMETER.VAR.BLOCKS = PARAMETER.VAR.BLOCKS.1

if (1==2)
{
    BURN = 0
    ITER.AFTER.BURN = 100
    CACHE.FREQUENCY = 100
    THIN=1
    FILE.PREFIX = 'test'
}

SAVE.TO.FILE = paste0('mcmc_runs/', FILE.PREFIX, "_", (BURN+ITER.AFTER.BURN), "_", Sys.Date(), ".Rdata")
CACHE.DIR = paste0('mcmc_runs/', FILE.PREFIX, "_", Sys.Date())

set.seed(409832)


# Set up the likelihood

cat('Building likelihood...')
full.likelihood = create.msa.likelihood(msa)
cat('Done\n')


# Set up the initial components
base.components = setup.initial.components(msa=msa)

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

    sim
}

if (1==2)
{
    run.simulation <- function(parameters){parameters}
    full.likelihood <- function(sim){dnorm(rnorm(1), log=T)}
}

param.medians = suppressWarnings(get.medians(parameters.prior))
init.sds = suppressWarnings(get.sds(parameters.prior) / param.medians) / 40
init.sds[parameters.prior@is.improper] = 0.1/40

init.sds[init.sds>1] = (param.medians)[init.sds>1] / 40
init.sds = init.sds * 2
transformations = sapply(parameters.prior@var.names, function(v){'log'})


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
    SAVE.TO.FILE = file.path('mcmc_runs', paste0(RESUME.PRIOR.CACHE, '.Rdata'))

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
