if (1==2)
{
    setwd('../../../Ending HIV/Ending_HIV/')
}

source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')

source('code/calibration/calibrated_parameters_87_helpers.R')
source('code/calibration/calibrated_parameters_100b.R')

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

FILE.PREFIX = 'la.100b_revised.lik.9'
DESCRIPTION = "V100b; prev cv 0.034 with CV only with ratio with wt 1; new cv 0.065 with cv only; dx wt 1 with no strat; testing likelihood with total wt 1, strat wt 0.125; supp wt 0.25; focus wt 1"
RESUME.PRIOR.CACHE = NULL#'mcmc_runs/test_caches/la.95_supp.wt.1.decreasing.stratified_new.047cv.only_prev.058cv.only_prev.wt.25.no.cv.scaling_dx.wt.1_idu.wt.32_20K_2020-09-03/'
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
    global.trate = 0.135,
    
    #-- MSM Transmission --#
    black.msm.trate.peak = 10.421,
    black.msm.trate.0 = 0.834,
    black.msm.trate.1 = 2.373,
    black.msm.trate.2 = 1.804,
    
    hispanic.msm.trate.peak = 1.553,
    hispanic.msm.trate.0 = 1.121,
    hispanic.msm.trate.1 = 1.426,
    hispanic.msm.trate.2 = 1.145,
    
    other.msm.trate.peak = 0.571,
    other.msm.trate.0 = 1.177,
    other.msm.trate.1 = 0.831,
    other.msm.trate.2 = 0.831,
    
    #-- Heterosexual Transmission --#
    black.heterosexual.trate.peak = 2.403,
    black.heterosexual.trate.0 = 0.904,
    black.heterosexual.trate.1 = 1.496,
    black.heterosexual.trate.2 = 1.575,
    
    hispanic.heterosexual.trate.peak = 0.372,
    hispanic.heterosexual.trate.0 = 0.517,
    hispanic.heterosexual.trate.1 = 0.155,
    hispanic.heterosexual.trate.2 = 0.247,
    
    other.heterosexual.trate.peak = 0.217,
    other.heterosexual.trate.0 = 0.128,
    other.heterosexual.trate.1 = 0.131,
    other.heterosexual.trate.2 = 0.169,
    
    #-- IDU Transmission --#
    black.idu.trate.peak = 13.139,
    black.idu.trate.0 = 3.715,
    black.idu.trate.1 = 4.144,
    black.idu.trate.2 = 4.579,
    
    hispanic.idu.trate.peak = 2.579,
    hispanic.idu.trate.0 = 0.940,
    hispanic.idu.trate.1 = 1.447,
    hispanic.idu.trate.2 = 2.191,
    
    other.idu.trate.peak = 2.414,
    other.idu.trate.0 = 0.907,
    other.idu.trate.1 = 1.340,
    other.idu.trate.2 = 2.141,
    
    
    #-- Other Sexual Transmission Parameters --#
    age1.msm.susceptibility.rr1 = 1.280,
    age2.msm.susceptibility.rr1 = 1.088,
    age3.msm.susceptibility.rr1 = 0.716,
    
    age1.msm.susceptibility.rr2 = 1.71,
    age2.msm.susceptibility.rr2 = 1.217,
    age3.msm.susceptibility.rr2 = 0.592,
    
    age1.susceptibility.rr.mult = 1.328,
    age2.susceptibility.rr.mult = 1.039,
    age4.susceptibility.rr.mult = 0.908,
    age5.susceptibility.rr.mult = 0.448,
    
    
    #-- Aging --#
    msm.age1.aging.base = 0.401,
    msm.age2.aging.0 = 0.099,
    msm.age2.aging.1 = 0.118,
    msm.age3.aging.1 = 0.054,
    msm.age4.aging.1 = 0.054,

    heterosexual.age1.aging.base = 0.182,
    heterosexual.age2.aging.0 = 0.210,
    heterosexual.age2.aging.1 = 0.094,
    heterosexual.age3.aging.1 = 0.141,
    heterosexual.age4.aging.1 = 0.049,

    idu.age1.aging.base = 0.188,
    idu.age2.aging.0 = 0.096,
    idu.age2.aging.1 = 0.244,
    idu.age3.aging.1 = 0.124,
    idu.age4.aging.1 = 0.046,

    
    #-- Other Sexual Transmission Parameters --#
    male.vs.female.heterosexual.rr = 0.335,
    
    female.vs.heterosexual.male.idu.susceptibility.rr = 0.968,
    
    msm.vs.heterosexual.male.idu.susceptibility.rr.peak = 8.178,
    msm.vs.heterosexual.male.idu.susceptibility.rr.0 = 6.736,
    msm.vs.heterosexual.male.idu.susceptibility.rr.1 = 6.426,
    msm.vs.heterosexual.male.idu.susceptibility.rr.2 = 3.309,
    
    #-- HIV Testing --#
    heterosexual.proportion.tested.or = 0.666,
    msm.proportion.tested.or = 0.486,
    idu.proportion.tested.or = 2.383,
    msm.idu.proportion.tested.or = 1.289,
    
    black.proportion.tested.or = 1.206,
    hispanic.proportion.tested.or = 1.230,
    
    age1.proportion.tested.or = 0.798,
    age2.proportion.tested.or = 0.538,
    age4.proportion.tested.or = 1.413,
    age5.proportion.tested.or = 0.818,
    
    heterosexual.proportion.tested.slope.or = 1,
    msm.proportion.tested.slope.or = 1,
    idu.proportion.tested.slope.or = 1,
    msm.idu.proportion.tested.slope.or = 1,
    
    testing.ramp.up.vs.current.rr = 0.736,
    
    #-- Suppression --#
    heterosexual.suppressed.or = 1.946,
    msm.suppressed.or = 1.412,
    idu.suppressed.or = 1.350,
    msm.idu.suppressed.or = 0.706,
    
    black.suppressed.or = 0.978,
    hispanic.suppressed.or = 1.158,
    
    age1.suppressed.or = 0.825,
    age2.suppressed.or = 1.079,
    age4.suppressed.or = 1.009,
    age5.suppressed.or = 1.101,
    
    
    heterosexual.suppressed.slope.or = 0.887,
    msm.suppressed.slope.or = 0.898,
    idu.suppressed.slope.or = 0.938,
    msm.idu.suppressed.slope.or = 1.212,
    
    black.suppressed.slope.or = 1.023,
    hispanic.suppressed.slope.or = 1.014,
    
    age1.suppressed.slope.or = 1.125,
    age2.suppressed.slope.or = 0.988,
    age4.suppressed.slope.or = 0.997,
    age5.suppressed.slope.or = 0.985,
    
    
    #-- Proportion MSM --#
    proportion.msm.of.male.mult = 1.307,
    
    #-- IDU Transitions --#
    black.incident.idu.multiplier.0 = 3.901,
    hispanic.incident.idu.multiplier.0 = 1.204,
    other.incident.idu.multiplier.0 = 1.046,
    
    black.incident.idu.multiplier.2 = 0.746,
    hispanic.incident.idu.multiplier.2 = 2.039,
    other.incident.idu.multiplier.2 = 1.603,
    
    black.incident.msm.idu.multiplier.0 = 4.041,
    hispanic.incident.msm.idu.multiplier.0 = 1.208,
    other.incident.msm.idu.multiplier.0 = 1.710,
    
    black.incident.msm.idu.multiplier.2 = 0.358,
    hispanic.incident.msm.idu.multiplier.2 = 0.946,
    other.incident.msm.idu.multiplier.2 = 2.285,
    
    idu.remission.multiplier = 1.375,
    
    idu.relapse.multiplier = 1.386,
    
    idu.mortality = 0.013,
    
    #-- HIV-Specific Mortality --#
    hiv.mortality.0 = 0.034,
    hiv.mortality.2 = 0.011,
    peak.hiv.mortality = 0.156,
    
    #-- Sexual Mixing by Age --#
    age.mixing.sd.mult = 0.885,
    
    #-- Other Sexual Mixing --#
    oe.female.pairings.with.msm = 0.258,
    fraction.heterosexual.male.pairings.with.male=0.002,
    oe.never.idu.pairings.with.idu = 0.085,
    
    black.black.sexual.oe = 2.278,
    hispanic.hispanic.sexual.oe = 2.128,
    other.other.sexual.oe = 1.772,
    
    #-- Acute HIV and the Effect of Diagnosis --#
    acute.transmissibility.rr = 10.503,
    diagnosed.transmission.rr = 0.340,
    
    #-- Uncertainty About the Future --#
    #    current.gains.end.by.year = 2020,
    msm.fraction.trate.change.after.t2 = 0.019,
    heterosexual.fraction.trate.change.after.t2 = 0.037,
    idu.fraction.trate.change.after.t2 = 0.021
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
