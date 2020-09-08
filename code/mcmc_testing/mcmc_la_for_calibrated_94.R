if (1==2)
{
    setwd('../../../Ending HIV/Ending_HIV/')
}

source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')

source('code/calibration/calibrated_parameters_87_helpers.R')
source('code/calibration/calibrated_parameters_94.R')

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

FILE.PREFIX = 'la.94_supp.wt.1.decreasing.stratified_new.047cv.only_prev.058cv.only_prev.wt.25.no.cv.scaling_dx.wt.1'
DESCRIPTION = "V94"
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
    global.trate = 0.129,
    
    #-- MSM Transmission --#
    black.msm.trate.peak = 12.444,
    black.msm.trate.0 = 0.701,
    black.msm.trate.1 = 1.931,
    black.msm.trate.2 = 2.218,
    
    hispanic.msm.trate.peak = 1.578,
    hispanic.msm.trate.0 = 1.004,
    hispanic.msm.trate.1 = 1.378,
    hispanic.msm.trate.2 = 1.234,
    
    other.msm.trate.peak = 0.626,
    other.msm.trate.0 = 1.154,
    other.msm.trate.1 = 0.826,
    other.msm.trate.2 = 0.969,
    
    #-- Heterosexual Transmission --#
    black.heterosexual.trate.peak = 2.079,
    black.heterosexual.trate.0 = 0.930,
    black.heterosexual.trate.1 = 1.856,
    black.heterosexual.trate.2 = 1.141,
    
    hispanic.heterosexual.trate.peak = 0.366,
    hispanic.heterosexual.trate.0 = 0.593,
    hispanic.heterosexual.trate.1 = 0.322,
    hispanic.heterosexual.trate.2 = 0.220,
    
    other.heterosexual.trate.peak = 0.203,
    other.heterosexual.trate.0 = 0.127,
    other.heterosexual.trate.1 = 0.188,
    other.heterosexual.trate.2 = 0.253,
    
    #-- IDU Transmission --#
    black.idu.trate.peak = 9.032,
    black.idu.trate.0 = 4.821,
    black.idu.trate.1 = 5.219,
    black.idu.trate.2 = 5.571,
    
    hispanic.idu.trate.peak = 2.575,
    hispanic.idu.trate.0 = 0.360,
    hispanic.idu.trate.1 = 1.652,
    hispanic.idu.trate.2 = 1.268,
    
    other.idu.trate.peak = 1.376,
    other.idu.trate.0 = 1.605,
    other.idu.trate.1 = 1.022,
    other.idu.trate.2 = 1.908,
    
    #-- MSM-IDU Transmission --#
    black.msm.idu.trate.peak = 9.032 * 6.5,
    black.msm.idu.trate.0 = 4.821 * 6.2,
    black.msm.idu.trate.1 = 5.219 * 8.2,
    black.msm.idu.trate.2 = 5.571 * 6.4,
    
    hispanic.msm.idu.trate.peak = 2.575 * 6.5,
    hispanic.msm.idu.trate.0 = 0.360 * 6.2,
    hispanic.msm.idu.trate.1 = 1.652 * 8.2,
    hispanic.msm.idu.trate.2 = 1.268 * 6.4,
    
    other.msm.idu.trate.peak = 1.376 * 6.5,
    other.msm.idu.trate.0 = 1.605 * 6.2,
    other.msm.idu.trate.1 = 1.022 * 8.2,
    other.msm.idu.trate.2 = 1.908 * 6.4,
    
    #-- Other Sexual Transmission Parameters --#
    age1.msm.susceptibility.rr1 = 1.194,
    age2.msm.susceptibility.rr1 = 1.156,
    age3.msm.susceptibility.rr1 = 0.699,
    
    age1.msm.susceptibility.rr2 = 1.591,
    age2.msm.susceptibility.rr2 = 1.229,
    age3.msm.susceptibility.rr2 = 0.610,
    
    age1.sexual.susceptibility.rr = 0.900,
    age2.sexual.susceptibility.rr = 1.177,
    age4.sexual.susceptibility.rr = 0.441,
    age5.sexual.susceptibility.rr = 0.157,
    
    age1.idu.susceptibility.rr = 1,
    age2.idu.susceptibility.rr = 1,
    age4.idu.susceptibility.rr = 1,
    age5.idu.susceptibility.rr = 1,
    
    
    #-- Aging --#
    msm.age1.aging.base = 0.399,
    msm.age2.aging.0 = 0.168,
    msm.age2.aging.1 = 0.098,
    msm.age3.aging.1 = 0.121,
    msm.age4.aging.1 = 0.059,

    heterosexual.age1.aging.base = 0.289,
    heterosexual.age2.aging.0 = 0.259,
    heterosexual.age2.aging.1 = 0.099,
    heterosexual.age3.aging.1 = 0.103,
    heterosexual.age4.aging.1 = 0.050,

    idu.age1.aging.base = 0.168,
    idu.age2.aging.0 = 0.175,
    idu.age2.aging.1 = 0.114,
    idu.age3.aging.1 = 0.122,
    idu.age4.aging.1 = 0.059,

    
    #-- Other Sexual Transmission Parameters --#
    male.vs.female.heterosexual.rr = 0.407,
    
    female.vs.heterosexual.male.idu.susceptibility.rr = 0.99,
    
    
    #-- HIV Testing --#
    heterosexual.proportion.tested.or = 0.782,
    msm.proportion.tested.or = 0.285,
    idu.proportion.tested.or = 2.408,
    msm.idu.proportion.tested.or = 2.408,
    
    black.proportion.tested.or = 0.910,
    hispanic.proportion.tested.or = 1.272,
    
    age1.proportion.tested.or = 1.102,
    age2.proportion.tested.or = 0.664,
    age4.proportion.tested.or = 1.387,
    age5.proportion.tested.or = 0.786,
    
    total.proportion.tested.slope.or = 0.936,
    
    testing.ramp.up.vs.current.rr = 0.654,
    
    #-- Suppression --#
    heterosexual.suppressed.or = 2.197,
    msm.suppressed.or = 1.530,
    idu.suppressed.or = 1.398,
    msm.idu.suppressed.or = 1.398,
    
    black.suppressed.or = 0.709,
    hispanic.suppressed.or = 1.338,
    
    age1.suppressed.or = 0.597,
    age2.suppressed.or = 1.076,
    age4.suppressed.or = 0.957,
    age5.suppressed.or = 1.072,
    
    
    heterosexual.suppressed.slope.or = 0.909,
    msm.suppressed.slope.or = 0.920,
    idu.suppressed.slope.or = 0.991,
    msm.idu.suppressed.slope.or = 0.991,
    
    black.suppressed.slope.or = 1.039,
    hispanic.suppressed.slope.or = 0.930,
    
    age1.suppressed.slope.or = 1.177,
    age2.suppressed.slope.or = 1.003,
    age4.suppressed.slope.or = 1.003,
    age5.suppressed.slope.or = 0.976,
    
    
    #-- Proportion MSM --#
    proportion.msm.of.male.mult = 1.155,
    
    #-- IDU Transitions --#
    black.incident.idu.multiplier.0 = 3.550,
    hispanic.incident.idu.multiplier.0 = 1.383,
    other.incident.idu.multiplier.0 = 1.307,
    
    black.incident.idu.multiplier.2 = 1.355,
    hispanic.incident.idu.multiplier.2 = 2.045,
    other.incident.idu.multiplier.2 = 3.491,
    
    idu.remission.multiplier = 2.689,
    
    idu.relapse.multiplier = 1.901,
    
    idu.mortality = 0.012,
    
    #-- HIV-Specific Mortality --#
    hiv.mortality.0 = 0.027,
    hiv.mortality.2 = 0.009,
    peak.hiv.mortality = 0.170,
    
    #-- Sexual Mixing by Age --#
    age.mixing.sd.mult = 0.742,
    
    #-- Other Sexual Mixing --#
    oe.female.pairings.with.msm = 0.203,
    fraction.heterosexual.male.pairings.with.male=0.001,
    oe.never.idu.pairings.with.idu = 0.185,
    
    black.black.sexual.oe = 2.902,
    hispanic.hispanic.sexual.oe = 2.138,
    other.other.sexual.oe = 1.268,
    
    #-- Acute HIV and the Effect of Diagnosis --#
    acute.transmissibility.rr = 10.852,
    diagnosed.transmission.rr = 0.254,
    
    #-- Uncertainty About the Future --#
    #    current.gains.end.by.year = 2020,
    msm.fraction.trate.change.after.t2 = 0.019,
    heterosexual.fraction.trate.change.after.t2 = 0.061,
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
