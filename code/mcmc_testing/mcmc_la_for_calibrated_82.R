if (1==2)
{
    setwd('../../../Ending HIV/Ending_HIV/')
}

source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')

source('code/targets/target_msas.R')

source('code/calibration/calibrated_parameters_77_helpers.R')
source('code/calibration/calibrated_parameters_82.R')

msa = LA.MSA

print(paste0("Calibrating for ", msa.names(msa)))

# MCMC Parameters
BURN = 0
KEEP = 2000
ITER.AFTER.BURN = 20000
THIN = 25
CACHE.FREQUENCY = 500
UPDATE.FREQUENCY = 200
MAX.SIM.TIME=Inf

FILE.PREFIX = 'la.82_supp.wt.1'
DESCRIPTION = "V82"
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
    global.trate = 0.143,
    
    #-- MSM Transmission --#
    black.msm.trate.peak = 9.689,
    black.msm.trate.0 = 1.318,
    black.msm.trate.1 = 2.978,
    black.msm.trate.2 = 2.468,
    
    hispanic.msm.trate.peak = 1.114,
    hispanic.msm.trate.0 = 1.164,
    hispanic.msm.trate.1 = 1.298,
    hispanic.msm.trate.2 = 1.260,
    
    other.msm.trate.peak = 1.372,
    other.msm.trate.0 = 1.064,
    other.msm.trate.1 = 1.111,
    other.msm.trate.2 = 1.067,
    
    #-- Heterosexual Transmission --#
    black.heterosexual.trate.peak = 0.605,
    black.heterosexual.trate.0 = 1.605,
    black.heterosexual.trate.1 = 2.273,
    black.heterosexual.trate.2 = 1.164,
    
    hispanic.heterosexual.trate.peak = 0.419,
    hispanic.heterosexual.trate.0 = 0.513,
    hispanic.heterosexual.trate.1 = 0.424,
    hispanic.heterosexual.trate.2 = 0.404,
    
    other.heterosexual.trate.peak = 0.212,
    other.heterosexual.trate.0 = 0.115,
    other.heterosexual.trate.1 = 0.163,
    other.heterosexual.trate.2 = 0.234,
    
    #-- IDU Transmission --#
    black.idu.trate.peak = 16.559,
    black.idu.trate.0 = 5.549,
    black.idu.trate.1 = 6.611,
    black.idu.trate.2 = 6.252,
    
    hispanic.idu.trate.peak = 4.026,
    hispanic.idu.trate.0 = 0.419,
    hispanic.idu.trate.1 = 2.468,
    hispanic.idu.trate.2 = 1.601,
    
    other.idu.trate.peak = 2.399,
    other.idu.trate.0 = 2.063,
    other.idu.trate.1 = 2.529,
    other.idu.trate.2 = 2.222,
    
    #-- Other Sexual Transmission Parameters --#
    age1.msm.susceptibility.rr1 = 1.102,
    age2.msm.susceptibility.rr1 = 1.222,
    age3.msm.susceptibility.rr1 = 0.605,
    
    age1.msm.susceptibility.rr2 = 1.312,
    age2.msm.susceptibility.rr2 = 1.579,
    age3.msm.susceptibility.rr2 = 0.605,
    
    age1.msm.susceptibility.rr0 = 0.898,
    age2.msm.susceptibility.rr0 = 1.294,
    age4.msm.susceptibility.rr = 0.681,
    age5.msm.susceptibility.rr = 0.145,
    
    age1.heterosexual.male.susceptibility.rr = 0.898*.41,
    age2.heterosexual.male.susceptibility.rr = 1.294*.41,
    age4.heterosexual.male.susceptibility.rr = 0.681*.41,
    age5.heterosexual.male.susceptibility.rr = 0.145*.41,
    
    age1.heterosexual.female.susceptibility.rr = 0.898,
    age2.heterosexual.female.susceptibility.rr = 1.294,
    age4.heterosexual.female.susceptibility.rr = 0.681,
    age5.heterosexual.female.susceptibility.rr = 0.145,
    
    age1.idu.susceptibility.rr = 0.898,
    age2.idu.susceptibility.rr = 1.294,
    age4.idu.susceptibility.rr = 0.681,
    age5.idu.susceptibility.rr = 0.145,
    
    #-- Aging --#
    msm.age1.aging.base = 0.405,
    msm.age2.aging.0 = 0.195,
    msm.age2.aging.1 = 0.144,
    msm.age3.aging.1 = 0.110,
    msm.age3.aging.2 = 0.201,
    msm.age4.aging.1 = 0.051,
    msm.age4.aging.2 = 0.184,
    
    heterosexual.age1.aging.base = 0.206,
    heterosexual.age2.aging.0 = 0.255,
    heterosexual.age2.aging.1 = 0.119,
    heterosexual.age3.aging.1 = 0.120,
    heterosexual.age3.aging.2 = 0.102,
    heterosexual.age4.aging.1 = 0.041,
    heterosexual.age4.aging.2 = 0.100,
    
    idu.age1.aging.base = 0.162,
    idu.age2.aging.0 = 0.123,
    idu.age2.aging.1 = 0.104,
    idu.age3.aging.1 = 0.107,
    idu.age3.aging.2 = 0.068,
    idu.age4.aging.1 = 0.050,
    idu.age4.aging.2 = 0.085,
    
    
    #-- Other Sex-Specific Transmission Parameters --#
    female.vs.heterosexual.male.idu.susceptibility.rr = 1,
    msm.vs.heterosexual.male.idu.susceptibility.rr = 1,
    
    #-- HIV Testing --#
    heterosexual.proportion.tested.or = 0.819,
    msm.proportion.tested.or = 0.271,
    idu.proportion.tested.or = 0.778,
    
    black.proportion.tested.or = 0.960,
    hispanic.proportion.tested.or = 0.836,
    
    age1.proportion.tested.or = 0.846,
    age2.proportion.tested.or = 0.655,
    age4.proportion.tested.or = 1.217,
    age5.proportion.tested.or = 0.717,
    
    total.proportion.tested.slope.or = 0.917,
    
    testing.ramp.up.vs.current.rr = 0.557,
    
    #-- Suppression --#
    heterosexual.suppressed.or = 1.772,
    msm.suppressed.or = 1.055,
    idu.suppressed.or = 1.137,
    
    black.suppressed.or = 0.568,
    hispanic.suppressed.or = 0.772,
    
    age1.suppressed.or = 0.540,
    age2.suppressed.or = 1.125,
    age4.suppressed.or = 1.038,
    age5.suppressed.or = 0.876,
    
    
    heterosexual.suppressed.slope.or = 1.028,
    msm.suppressed.slope.or = 1.000,
    idu.suppressed.slope.or = 0.997,
    
    black.suppressed.slope.or = 0.918,
    hispanic.suppressed.slope.or = 0.932,
    
    age1.suppressed.slope.or = 0.941,
    age2.suppressed.slope.or = 0.999,
    age4.suppressed.slope.or = 0.997,
    age5.suppressed.slope.or = 1.004,
    
    
    #-- Proportion MSM --#
    proportion.msm.of.male.mult = 1.154,
    
    #-- IDU Transitions --#
    black.incident.idu.multiplier.0 = 3.992,
    hispanic.incident.idu.multiplier.0 = 1.391,
    other.incident.idu.multiplier.0 = 1.297,
    
    black.incident.idu.multiplier.2 = 1.281,
    hispanic.incident.idu.multiplier.2 = 1.865,
    other.incident.idu.multiplier.2 = 3.461,
    
    idu.remission.multiplier = 2.302,
    
    idu.relapse.multiplier = 1.892,
    
    idu.mortality = 0.011,
    
    #-- HIV-Specific Mortality --#
    hiv.mortality.0 = 0.038,
    hiv.mortality.2 = 0.010,
    peak.hiv.mortality = 0.202,
    
    #-- Sexual Mixing by Age --#
    age.mixing.sd.mult = 1.319,
    
    #-- Other Sexual Mixing --#
    oe.female.pairings.with.msm = 0.176,
    fraction.heterosexual.male.pairings.with.male=0.001,
    oe.never.idu.pairings.with.idu = 0.192,
    
    black.black.sexual.oe = 2.907,
    hispanic.hispanic.sexual.oe = 1.971,
    other.other.sexual.oe = 1.607,
    
    #-- Acute HIV and the Effect of Diagnosis --#
    acute.transmissibility.rr = 10.580,
    diagnosed.transmission.rr = 0.310,
    
    #-- Uncertainty About the Future --#
    #    current.gains.end.by.year = 2020,
    msm.fraction.trate.change.after.t2 = 0.095,
    heterosexual.fraction.trate.change.after.t2 = 0.081,
    idu.fraction.trate.change.after.t2 = 0.143
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
