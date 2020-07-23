if (1==2)
{
    setwd('../../../Ending HIV/Ending_HIV/')
}

source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')

source('code/calibration/calibrated_parameters_68_helpers.R')
source('code/calibration/calibrated_parameters_72.R')

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

FILE.PREFIX = 'la.72'
DESCRIPTION = "V72"
RESUME.PRIOR.CACHE = NULL#'mcmc_runs/balt.70_wt.6.6.33_2020-07-16/'
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
    global.trate = 0.137,
    
    #-- MSM Transmission --#
    black.msm.trate.peak = 12.655,
    black.msm.trate.0 = 0.852,
    black.msm.trate.1 = 2.905,
    black.msm.trate.2 = 2.846,
    
    hispanic.msm.trate.peak = 1.754,
    hispanic.msm.trate.0 = 0.962,
    hispanic.msm.trate.1 = 1.307,
    hispanic.msm.trate.2 = 1.216,
    
    other.msm.trate.peak = 0.483,
    other.msm.trate.0 = 1.106,
    other.msm.trate.1 = 1.152,
    other.msm.trate.2 = 1.040,
    
    #-- Heterosexual Transmission --#
    black.heterosexual.trate.peak = 0.935,
    black.heterosexual.trate.0 = 0.505,
    black.heterosexual.trate.1 = 2.196,
    black.heterosexual.trate.2 = 1.252,
    
    hispanic.heterosexual.trate.peak = 0.300,
    hispanic.heterosexual.trate.0 = 0.627,
    hispanic.heterosexual.trate.1 = 0.421,
    hispanic.heterosexual.trate.2 = 0.240,
    
    other.heterosexual.trate.peak = 0.178,
    other.heterosexual.trate.0 = 0.150,
    other.heterosexual.trate.1 = 0.185,
    other.heterosexual.trate.2 = 0.252,
    
    #-- IDU Transmission --#
    black.idu.trate.peak = 9.112,
    black.idu.trate.0 = 5.395,
    black.idu.trate.1 = 4.111,
    black.idu.trate.2 = 5.040,
    
    hispanic.idu.trate.peak = 2.836,
    hispanic.idu.trate.0 = 0.361,
    hispanic.idu.trate.1 = 2.619,
    hispanic.idu.trate.2 = 0.167,
    
    other.idu.trate.peak = 2.441,
    other.idu.trate.0 = 1.783,
    other.idu.trate.1 = 2.506,
    other.idu.trate.2 = 2.127,
    
    #-- Other Sexual Transmission Parameters --#
    age1.msm.susceptibility.rr1 = 1.549,
    age2.msm.susceptibility.rr1 = 0.797,
    age3.msm.susceptibility.rr1 = 0.653,
    
    age1.msm.susceptibility.rr2 = 1.579,
    age2.msm.susceptibility.rr2 = 1.090,
    age3.msm.susceptibility.rr2 = 0.629,
    
    age1.susceptibility.rr = 1.000,
    age2.susceptibility.rr = 1.189,
    age4.susceptibility.rr = 0.736,
    age5.susceptibility.rr = 0.211,
    
    
    #-- Aging --#
    msm.age1.aging.base = 0.613,
    msm.age2.aging.0 = 0.233,
    msm.age3.aging.1 = 0.108,
    msm.age3.aging.2 = 0.1,
    msm.age4.aging.1 = 0.053,
    
    heterosexual.age1.aging.base = 0.166,
    heterosexual.age2.aging.0 = 0.258,
    heterosexual.age3.aging.1 = 0.102,
    heterosexual.age3.aging.2 = 0.1,
    heterosexual.age4.aging.1 = 0.040,
    
    idu.age1.aging.base = 0.158,
    idu.age2.aging.0 = 0.166,
    idu.age3.aging.1 = 0.116,
    idu.age3.aging.2 = 0.1,
    idu.age4.aging.1 = 0.048,
    
    
    #-- Other Sexual Transmission Parameters --#
    male.vs.female.heterosexual.rr = 0.416,
    
    #-- HIV Testing --#
    heterosexual.proportion.tested.or = 0.782,
    msm.proportion.tested.or = 0.273,
    idu.proportion.tested.or = 0.840,
    
    black.proportion.tested.or = 1.014,
    hispanic.proportion.tested.or = 0.961,
    
    age1.proportion.tested.or = 0.835,
    age2.proportion.tested.or = 0.557,
    age4.proportion.tested.or = 1.165,
    age5.proportion.tested.or = 0.748,
    
    total.proportion.tested.slope.or = 0.914,
    
    testing.ramp.up.vs.current.rr = 0.667,
    
    #-- Suppression --#
    heterosexual.suppressed.or = 1.722,
    msm.suppressed.or = 1.002,
    idu.suppressed.or = 1.144,
    
    black.suppressed.or = 0.673,
    hispanic.suppressed.or = 0.675,
    
    age1.suppressed.or = 0.569,
    age2.suppressed.or = 1.429,
    age4.suppressed.or = 0.835,
    age5.suppressed.or = 1.372,
    
    
    heterosexual.suppressed.slope.or = 1,
    msm.suppressed.slope.or = 1,
    idu.suppressed.slope.or = 1,
    
    black.suppressed.slope.or = 1,
    hispanic.suppressed.slope.or = 1,
    
    age1.suppressed.slope.or = 1,
    age2.suppressed.slope.or = 1,
    age4.suppressed.slope.or = 1,
    age5.suppressed.slope.or = 1,
    
    
    #-- Proportion MSM --#
    proportion.msm.of.male.mult = 1.046,
    
    #-- IDU Transitions --#
    black.incident.idu.multiplier.0 = 3.241,
    hispanic.incident.idu.multiplier.0 = 1.454,
    other.incident.idu.multiplier.0 = 1.308,
    
    black.incident.idu.multiplier.2 = 1.350,
    hispanic.incident.idu.multiplier.2 = 1.925,
    other.incident.idu.multiplier.2 = 3.446,
    
    idu.remission.multiplier = 2.134,
    
    idu.relapse.multiplier = 1.923,
    
    idu.mortality = 0.012,
    
    #-- HIV-Specific Mortality --#
    hiv.mortality.0 = 0.022,
    hiv.mortality.2 = 0.011,
    peak.hiv.mortality = 0.210,
    
    #-- Sexual Mixing by Age --#
    age.mixing.sd.mult = 1.182,
    
    #-- Other Sexual Mixing --#
    oe.female.pairings.with.msm = 0.180,
    fraction.heterosexual.male.pairings.with.male=0.001,
    oe.never.idu.pairings.with.idu = 0.180,
    
    black.black.sexual.oe = 3.092,
    hispanic.hispanic.sexual.oe = 2.078,
    other.other.sexual.oe = 1.781,
    
    #-- Acute HIV and the Effect of Diagnosis --#
    acute.transmissibility.rr = 11.563,
    diagnosed.transmission.rr = 0.308,
    
    #-- Uncertainty About the Future --#
    #    current.gains.end.by.year = 2020,
    msm.fraction.trate.change.after.t2 = 0.089,
    heterosexual.fraction.trate.change.after.t2 = 0.060,
    idu.fraction.trate.change.after.t2 = 0.154
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
