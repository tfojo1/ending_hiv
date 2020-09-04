if (1==2)
{
    setwd('../../../Ending HIV/Ending_HIV/')
}

source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')

source('code/calibration/calibrated_parameters_87_helpers.R')
source('code/calibration/calibrated_parameters_94b.R')

DC.MSA = cbsa.for.msa.name('Washington,DC')
BALTIMORE.MSA = '12580'
NYC.MSA = '35620'
msa ='31080' #LA

print(paste0("Calibrating for ", msa.names(msa)))

# MCMC Parameters
BURN = 0
KEEP = 2000
ITER.AFTER.BURN = 20000
THIN = 25
CACHE.FREQUENCY = 500
UPDATE.FREQUENCY = 200
MAX.SIM.TIME=Inf

FILE.PREFIX = 'la.94b_supp.wt.1.decreasing.stratified_new.047cv.only_prev.058cv.only_prev.wt.25.no.cv.scaling_dx.wt.1'
DESCRIPTION = "V94b"
RESUME.PRIOR.CACHE = NULL
if (!is.null(RESUME.PRIOR.CACHE))
    print("THIS IS RESUMPTION OF A PRIOR CACHE!!!")

TARGET.ACCEPTANCE.RATE = .238


PARAMETER.VAR.BLOCKS = PARAMETER.VAR.BLOCKS.1

if (1==2)
{
    BURN = 0
    ITER.AFTER.BURN = 100
    CACHE.FREQUENCY = 100
    THIN=1
    FILE.PREFIX = 'test'
}


SAVE.TO.FILE = paste0('mcmc_runs/test_runs/', FILE.PREFIX, "_", (BURN+ITER.AFTER.BURN)/1000, "K_", Sys.Date(), ".Rdata")
CACHE.DIR = paste0('mcmc_runs/test_caches/', FILE.PREFIX, "_", (BURN+ITER.AFTER.BURN)/1000, "K_", Sys.Date())

set.seed(409832)


# Set up the likelihood

cat('Building likelihood...')
full.likelihood = create.msa.likelihood(msa)
cat('Done\n')


# Set up the initial components
base.components = setup.initial.components(msa=msa)

init.parameters = c(
    global.trate = 0.133,
    
    #-- MSM Transmission --#
    black.msm.trate.peak = 12.205,
    black.msm.trate.0 = 0.811,
    black.msm.trate.1 = 2.319,
    black.msm.trate.2 = 2.192,
    
    hispanic.msm.trate.peak = 1.344,
    hispanic.msm.trate.0 = 1.071,
    hispanic.msm.trate.1 = 1.389,
    hispanic.msm.trate.2 = 0.998,
    
    other.msm.trate.peak = 0.601,
    other.msm.trate.0 = 1.098,
    other.msm.trate.1 = 0.815,
    other.msm.trate.2 = 0.896,
    
    #-- Heterosexual Transmission --#
    black.heterosexual.trate.peak = 1.874,
    black.heterosexual.trate.0 = 0.916,
    black.heterosexual.trate.1 = 1.474,
    black.heterosexual.trate.2 = 1.155,
    
    hispanic.heterosexual.trate.peak = 0.412,
    hispanic.heterosexual.trate.0 = 0.548,
    hispanic.heterosexual.trate.1 = 0.252,
    hispanic.heterosexual.trate.2 = 0.210,
    
    other.heterosexual.trate.peak = 0.186,
    other.heterosexual.trate.0 = 0.105,
    other.heterosexual.trate.1 = 0.149,
    other.heterosexual.trate.2 = 0.172,
    
    #-- IDU Transmission --#
    black.idu.trate.peak = 13.751,
    black.idu.trate.0 = 2.894,
    black.idu.trate.1 = 2.889,
    black.idu.trate.2 = 4.967,
    
    hispanic.idu.trate.peak = 3.29,
    hispanic.idu.trate.0 = 1.245,
    hispanic.idu.trate.1 = 1.471,
    hispanic.idu.trate.2 = 1.794,
    
    other.idu.trate.peak = 2.337,
    other.idu.trate.0 = 1.253,
    other.idu.trate.1 = 1.044,
    other.idu.trate.2 = 2.000,
    
    #-- MSM-IDU Transmission --#
    black.msm.idu.trate.peak = 127.472,
    black.msm.idu.trate.0 = 38.514,
    black.msm.idu.trate.1 = 39.879,
    black.msm.idu.trate.2 = 19.588,
    
    hispanic.msm.idu.trate.peak = 31.977,
    hispanic.msm.idu.trate.0 = 13.518,
    hispanic.msm.idu.trate.1 = 13.596,
    hispanic.msm.idu.trate.2 = 6.845,
    
    other.msm.idu.trate.peak = 184.615,
    other.msm.idu.trate.0 = 8.756,
    other.msm.idu.trate.1 = 11.895,
    other.msm.idu.trate.2 = 11.486,
    
    #-- Other Sexual Transmission Parameters --#
    age1.msm.susceptibility.rr1 = 1.283,
    age2.msm.susceptibility.rr1 = 0.941,
    age3.msm.susceptibility.rr1 = 0.703,
    
    age1.msm.susceptibility.rr2 = 1.730,
    age2.msm.susceptibility.rr2 = 1.236,
    age3.msm.susceptibility.rr2 = 0.589,
    
    age1.sexual.susceptibility.rr = 0.892,
    age2.sexual.susceptibility.rr = 1.189,
    age4.sexual.susceptibility.rr = 0.428,
    age5.sexual.susceptibility.rr = 0.157,
    
    age1.idu.susceptibility.rr = 0.900,
    age2.idu.susceptibility.rr = 0.973,
    age4.idu.susceptibility.rr = 0.666,
    age5.idu.susceptibility.rr = 0.597,
    
    
    #-- Aging --#
    msm.age1.aging.base = 0.401,
    msm.age2.aging.0 = 0.195,
    msm.age2.aging.1 = 0.097,
    msm.age3.aging.1 = 0.119,
    msm.age4.aging.1 = 0.061,

    heterosexual.age1.aging.base = 0.166,
    heterosexual.age2.aging.0 = 0.200,
    heterosexual.age2.aging.1 = 0.099,
    heterosexual.age3.aging.1 = 0.138,
    heterosexual.age4.aging.1 = 0.051,

    idu.age1.aging.base = 0.198,
    idu.age2.aging.0 = 0.069,
    idu.age2.aging.1 = 0.216,
    idu.age3.aging.1 = 0.121,
    idu.age4.aging.1 = 0.056,

    
    #-- Other Sexual Transmission Parameters --#
    male.vs.female.heterosexual.rr = 0.392,
    
    female.vs.heterosexual.male.idu.susceptibility.rr = 1.063,
    
    
    #-- HIV Testing --#
    heterosexual.proportion.tested.or = 0.683,
    msm.proportion.tested.or = 0.316,
    idu.proportion.tested.or = 1.624,
    msm.idu.proportion.tested.or = 1.412,
    
    black.proportion.tested.or = 0.992,
    hispanic.proportion.tested.or = 1.193,
    
    age1.proportion.tested.or = 0.998,
    age2.proportion.tested.or = 0.642,
    age4.proportion.tested.or = 1.446,
    age5.proportion.tested.or = 0.793,
    
    total.proportion.tested.slope.or = 0.944,
    
    testing.ramp.up.vs.current.rr = 0.784,
    
    #-- Suppression --#
    heterosexual.suppressed.or = 2.099,
    msm.suppressed.or = 1.538,
    idu.suppressed.or = 1.372,
    msm.idu.suppressed.or = 0.653,
    
    black.suppressed.or = 0.927,
    hispanic.suppressed.or = 1.303,
    
    age1.suppressed.or = 0.643,
    age2.suppressed.or = 1.065,
    age4.suppressed.or = 0.961,
    age5.suppressed.or = 1.027,
    
    
    heterosexual.suppressed.slope.or = 0.908,
    msm.suppressed.slope.or = 0.921,
    idu.suppressed.slope.or = 0.935,
    msm.idu.suppressed.slope.or = 1.221,
    
    black.suppressed.slope.or = 1.008,
    hispanic.suppressed.slope.or = 0.947,
    
    age1.suppressed.slope.or = 1.171,
    age2.suppressed.slope.or = 1.001,
    age4.suppressed.slope.or = 0.989,
    age5.suppressed.slope.or = 0.980,
    
    
    #-- Proportion MSM --#
    proportion.msm.of.male.mult = 1.287,
    
    #-- IDU Transitions --#
    black.incident.idu.multiplier.0 = 3.951,
    hispanic.incident.idu.multiplier.0 = 1.385,
    other.incident.idu.multiplier.0 = 1.298,
    
    black.incident.idu.multiplier.2 = 1.351,
    hispanic.incident.idu.multiplier.2 = 1.918,
    other.incident.idu.multiplier.2 = 3.679,
    
    idu.remission.multiplier = 2.34,
    
    idu.relapse.multiplier = 2.177,
    
    idu.mortality = 0.017,
    
    #-- HIV-Specific Mortality --#
    hiv.mortality.0 = 0.034,
    hiv.mortality.2 = 0.011,
    peak.hiv.mortality = 0.159,
    
    #-- Sexual Mixing by Age --#
    age.mixing.sd.mult = 0.812,
    
    #-- Other Sexual Mixing --#
    oe.female.pairings.with.msm = 0.224,
    fraction.heterosexual.male.pairings.with.male=0.001,
    oe.never.idu.pairings.with.idu = 0.092,
    
    black.black.sexual.oe = 2.025,
    hispanic.hispanic.sexual.oe = 2.144,
    other.other.sexual.oe = 1.006,
    
    #-- Acute HIV and the Effect of Diagnosis --#
    acute.transmissibility.rr = 10.873,
    diagnosed.transmission.rr = 0.274,
    
    #-- Uncertainty About the Future --#
    #    current.gains.end.by.year = 2020,
    msm.fraction.trate.change.after.t2 = 0.018,
    heterosexual.fraction.trate.change.after.t2 = 0.020,
    idu.fraction.trate.change.after.t2 = 0.036
    )
init.components = get.components.for.calibrated.parameters(init.parameters, base.components)
init.components = fix.components.for.calibration(components = init.components)

# Run the MCMC

run.simulation <- function(parameters)
{
#    print(length(parameters))
#    print(paste0(names(parameters), '=', parameters, collapse=", "))
 #   print(Sys.time())
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
                                             transformations = transformations
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
    SAVE.TO.FILE = paste0(substr(RESUME.PRIOR.CACHE, 1, nchar(RESUME.PRIOR.CACHE)-1), '.Rdata')

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

save(mcmc, DESCRIPTION, file=SAVE.TO.FILE)


if (1==2)
{
    simset = extract.simset(mcmc)
    plot.calibration.risk(simset)
}
