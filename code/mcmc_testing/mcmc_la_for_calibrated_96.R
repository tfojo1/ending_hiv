if (1==2)
{
    setwd('../../../Ending HIV/Ending_HIV/')
}

source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')

source('code/calibration/calibrated_parameters_87_helpers.R')
source('code/calibration/calibrated_parameters_96.R')

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

FILE.PREFIX = 'la.96_supp.wt.125.decreasing.stratified_new.047cv.only_prev.058cv.only_prev.wt.25.no.cv.scaling_dx.wt.1_idu.wt.2'
DESCRIPTION = "V96"
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
    global.trate = 0.131,
    
    #-- MSM Transmission --#
    black.msm.trate.peak = 11.944,
    black.msm.trate.0 = 0.767,
    black.msm.trate.1 = 2.392,
    black.msm.trate.2 = 2.189,
    
    hispanic.msm.trate.peak = 1.353,
    hispanic.msm.trate.0 = 1.097,
    hispanic.msm.trate.1 = 1.335,
    hispanic.msm.trate.2 = 0.978,
    
    other.msm.trate.peak = 0.571,
    other.msm.trate.0 = 1.150,
    other.msm.trate.1 = 0.823,
    other.msm.trate.2 = 0.857,
    
    #-- Heterosexual Transmission --#
    black.heterosexual.trate.peak = 2.403,
    black.heterosexual.trate.0 = 0.883,
    black.heterosexual.trate.1 = 1.431,
    black.heterosexual.trate.2 = 1.673,
    
    hispanic.heterosexual.trate.peak = 0.420,
    hispanic.heterosexual.trate.0 = 0.532,
    hispanic.heterosexual.trate.1 = 0.246,
    hispanic.heterosexual.trate.2 = 0.223,
    
    other.heterosexual.trate.peak = 0.191,
    other.heterosexual.trate.0 = 0.044,
    other.heterosexual.trate.1 = 0.214,
    other.heterosexual.trate.2 = 0.166,
    
    #-- IDU Transmission --#
    black.idu.trate.peak = 12.027,
    black.idu.trate.0 = 2.933,
    black.idu.trate.1 = 4.000,
    black.idu.trate.2 = 4.81,
    
    hispanic.idu.trate.peak = 3.239,
    hispanic.idu.trate.0 = 0.947,
    hispanic.idu.trate.1 = 1.800,
    hispanic.idu.trate.2 = 2.200,
    
    other.idu.trate.peak = 2.476,
    other.idu.trate.0 = 0.503,
    other.idu.trate.1 = 1.435,
    other.idu.trate.2 = 2.328,
    
    #-- MSM-IDU Transmission --#
    black.msm.idu.trate.peak = 11.148,
    black.msm.idu.trate.0 = 8.633,
    black.msm.idu.trate.1 = 15.000,
    black.msm.idu.trate.2 = 11.424,
    
    hispanic.msm.idu.trate.peak = 43.929,
    hispanic.msm.idu.trate.0 = 15.302,
    hispanic.msm.idu.trate.1 = 6.000,
    hispanic.msm.idu.trate.2 = 3.717,
    
    other.msm.idu.trate.peak = 43.010,
    other.msm.idu.trate.0 = 18.149,
    other.msm.idu.trate.1 = 5.000,
    other.msm.idu.trate.2 = 4.000,
    
    #-- Other Sexual Transmission Parameters --#
    age1.msm.susceptibility.rr1 = 1.295,
    age2.msm.susceptibility.rr1 = 1.058,
    age3.msm.susceptibility.rr1 = 0.702,
    
    age1.msm.susceptibility.rr2 = 1.744,
    age2.msm.susceptibility.rr2 = 1.229,
    age3.msm.susceptibility.rr2 = 0.578,
    
    age1.susceptibility.rr.mult = 0.883 / 0.667,
    age2.susceptibility.rr.mult = 1.153 / 1.108,
    age4.susceptibility.rr.mult = 0.652 / 0.720,
    age5.susceptibility.rr.mult = 0.156 / 0.350,
    
    
    #-- Aging --#
    msm.age1.aging.base = 0.402,
    msm.age2.aging.0 = 0.196,
    msm.age2.aging.1 = 0.097,
    msm.age3.aging.1 = 0.121,
    msm.age4.aging.1 = 0.054,

    heterosexual.age1.aging.base = 0.182,
    heterosexual.age2.aging.0 = 0.203,
    heterosexual.age2.aging.1 = 0.100,
    heterosexual.age3.aging.1 = 0.138,
    heterosexual.age4.aging.1 = 0.052,

    idu.age1.aging.base = 0.196,
    idu.age2.aging.0 = 0.069,
    idu.age2.aging.1 = 0.212,
    idu.age3.aging.1 = 0.120,
    idu.age4.aging.1 = 0.045,

    
    #-- Other Sexual Transmission Parameters --#
    male.vs.female.heterosexual.rr = 0.335,
    
    female.vs.heterosexual.male.idu.susceptibility.rr = 1.000,
    
    
    #-- HIV Testing --#
    heterosexual.proportion.tested.or = 1.096,
    msm.proportion.tested.or = 0.41,
    idu.proportion.tested.or = 1.431,
    msm.idu.proportion.tested.or = 1.410,
    
    black.proportion.tested.or = 1.204,
    hispanic.proportion.tested.or = 1.173,
    
    age1.proportion.tested.or = 0.907,
    age2.proportion.tested.or = 0.545,
    age4.proportion.tested.or = 1.426,
    age5.proportion.tested.or = 0.813,
    
    total.proportion.tested.slope.or = 0.936,
    
    testing.ramp.up.vs.current.rr = 0.749,
    
    #-- Suppression --#
    heterosexual.suppressed.or = 1.829,
    msm.suppressed.or = 1.415,
    idu.suppressed.or = 1.326,
    msm.idu.suppressed.or = 0.680,
    
    black.suppressed.or = 1.013,
    hispanic.suppressed.or = 1.428,
    
    age1.suppressed.or = 0.629,
    age2.suppressed.or = 1.072,
    age4.suppressed.or = 0.985,
    age5.suppressed.or = 1.074,
    
    
    heterosexual.suppressed.slope.or = 0.903,
    msm.suppressed.slope.or = 0.911,
    idu.suppressed.slope.or = 0.938,
    msm.idu.suppressed.slope.or = 1.173,
    
    black.suppressed.slope.or = 1.009,
    hispanic.suppressed.slope.or = 0.966,
    
    age1.suppressed.slope.or = 1.174,
    age2.suppressed.slope.or = 1.005,
    age4.suppressed.slope.or = 1.000,
    age5.suppressed.slope.or = 0.981,
    
    
    #-- Proportion MSM --#
    proportion.msm.of.male.mult = 1.246,
    
    #-- IDU Transitions --#
    black.incident.idu.multiplier.0 = 3.979,
    hispanic.incident.idu.multiplier.0 = 1.199,
    other.incident.idu.multiplier.0 = 1.285,
    
    black.incident.idu.multiplier.2 = 1.389,
    hispanic.incident.idu.multiplier.2 = 1.743,
    other.incident.idu.multiplier.2 = 3.625,
    
    msm.incident.idu.multiplier.0 = 1.341,
    msm.incident.idu.multiplier.2 = 1.028,
    
    idu.remission.multiplier = 2.241,
    
    idu.relapse.multiplier = 2.267,
    
    idu.mortality = 0.017,
    
    #-- HIV-Specific Mortality --#
    hiv.mortality.0 = 0.033,
    hiv.mortality.2 = 0.011,
    peak.hiv.mortality = 0.158,
    
    #-- Sexual Mixing by Age --#
    age.mixing.sd.mult = 0.835,
    
    #-- Other Sexual Mixing --#
    oe.female.pairings.with.msm = 0.225,
    fraction.heterosexual.male.pairings.with.male=0.002,
    oe.never.idu.pairings.with.idu = 0.090,
    
    black.black.sexual.oe = 2.023,
    hispanic.hispanic.sexual.oe = 2.148,
    other.other.sexual.oe = 1.554,
    
    #-- Acute HIV and the Effect of Diagnosis --#
    acute.transmissibility.rr = 10.920,
    diagnosed.transmission.rr = 0.351,
    
    #-- Uncertainty About the Future --#
    #    current.gains.end.by.year = 2020,
    msm.fraction.trate.change.after.t2 = 0.019,
    heterosexual.fraction.trate.change.after.t2 = 0.023,
    idu.fraction.trate.change.after.t2 = 0.027
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
