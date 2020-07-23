if (1==2)
{
    setwd('../../../Ending HIV/Ending_HIV/')
}

source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')

source('code/calibration/calibrated_parameters_68_helpers.R')
source('code/calibration/calibrated_parameters_69.R')

DC.MSA = cbsa.for.msa.name('Washington,DC')
BALTIMORE.MSA = '12580'
NYC.MSA = '35620'
msa = BALTIMORE.MSA

print(paste0("Calibrating for ", msa.names(msa)))

# MCMC Parameters
BURN = 0
KEEP = 2000
ITER.AFTER.BURN = 20000
THIN = 25
CACHE.FREQUENCY = 500
UPDATE.FREQUENCY = 200
MAX.SIM.TIME=Inf

FILE.PREFIX = 'balt.69_revised2_wt.6.6.33'
DESCRIPTION = "V69"
RESUME.PRIOR.CACHE = NULL#'mcmc_runs/nyc.68.8_aids.dx_cum.mort.1x_t1=08_2020-05-11/'
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
    global.trate = 0.103,
    
    #-- Acute HIV and the Effect of Diagnosis --#
    acute.transmissibility.rr = 12,#16.8,
    diagnosed.transmission.rr = .3,#0.31,
    
    
    #-- MSM Transmission --#
    black.msm.trate.peak = 3.23,
    black.msm.trate.0 = 2.08,
    black.msm.trate.1 = 2.16,
    black.msm.trate.2 = 2.68,
    
    hispanic.msm.trate.peak = 1.99,
    hispanic.msm.trate.0 = 1.00,
    hispanic.msm.trate.1 = 1.34,
    hispanic.msm.trate.2 = 1.18,
    
    other.msm.trate.peak = 1.99,
    other.msm.trate.0 = .807,
    other.msm.trate.1 = .931,
    other.msm.trate.2 = 1.06,
    
    #-- Heterosexual Transmission --#
    black.heterosexual.trate.peak = 1.68,
    black.heterosexual.trate.0 = 1.83,
    black.heterosexual.trate.1 = .887,
    black.heterosexual.trate.2 = 1.11,
    
    hispanic.heterosexual.trate.peak = .560,
    hispanic.heterosexual.trate.0 = .432,
    hispanic.heterosexual.trate.1 = .427,
    hispanic.heterosexual.trate.2 = .356,
    
    other.heterosexual.trate.peak = .340,
    other.heterosexual.trate.0 = 0.116,
    other.heterosexual.trate.1 = 0.133,
    other.heterosexual.trate.2 = 0.112,
    
    #-- IDU Transmission --#
    black.idu.trate.peak = 85.8,
    black.idu.trate.0 = 8.15,
    black.idu.trate.1 = 3.23,
    black.idu.trate.2 = 2.63,
    
    hispanic.idu.trate.peak = 1.80,
    hispanic.idu.trate.0 = .183,
    hispanic.idu.trate.1 = .215,
    hispanic.idu.trate.2 = .219,
    
    other.idu.trate.peak = 1.69,
    other.idu.trate.0 = 1.32,
    other.idu.trate.1 = 1.34,
    other.idu.trate.2 = 1.50,
    
    #-- Other Sexual Transmission Parameters --#
    male.vs.female.heterosexual.rr = .779,
    
    age1.msm.susceptibility.rr1 = 1.92,
    age1.msm.susceptibility.rr2 = 1.26,
    age2.msm.susceptibility.rr1 = 1.28,
    age2.msm.susceptibility.rr2 = 1.05,
    
    age1.susceptibility.rr = 0.852,
    age2.susceptibility.rr = 1.02,
    age4.susceptibility.rr = .542,
    age5.susceptibility.rr = .311,
    
    #-- Aging --#
    msm.age1.aging.base = .231,
    msm.age2.aging.0 = .373,
    msm.age3.aging.1 = .264,
    msm.age4.aging.1 = .0336,
    
    heterosexual.age1.aging.base = .115,
    heterosexual.age2.aging.0 = .172,
    heterosexual.age3.aging.1 = .113,
    heterosexual.age4.aging.1 = .036,
    
    idu.age1.aging.base = .233,
    idu.age2.aging.0 = .427,
    idu.age3.aging.1 = .102,
    idu.age4.aging.1 = .055,
    
    #-- HIV Testing --#
    heterosexual.proportion.tested.or = 1,
    msm.proportion.tested.or = 1,
    idu.proportion.tested.or = 1,
    
    black.proportion.tested.or = 1,
    hispanic.proportion.tested.or = 1,
    
    age1.proportion.tested.or = 1,
    age2.proportion.tested.or = 1,
    age4.proportion.tested.or = 1,
    age5.proportion.tested.or = 1,
    
    total.proportion.tested.slope.or = 1,
    
    testing.ramp.up.vs.current.rr = 0.5,
    
    #-- Suppression --#
    heterosexual.suppressed.or = 1,
    msm.suppressed.or = 1,
    idu.suppressed.or = 1,
    
    black.suppressed.or = 1,
    hispanic.suppressed.or = 1,
    
    age1.suppressed.or = 1,
    age2.suppressed.or = 1,
    age4.suppressed.or = 1,
    age5.suppressed.or = 1,
    
    total.suppressed.slope.or = 1,
    
    #-- Proportion MSM --#
    proportion.msm.of.male.mult = .904,
    
    #-- IDU Transitions --#
    black.incident.idu.multiplier.0 = 4.33,
    hispanic.incident.idu.multiplier.0 = 1.34,
    other.incident.idu.multiplier.0 = 1.51,
    
    black.incident.idu.multiplier.2 = 0.42,
    hispanic.incident.idu.multiplier.2 = 0.94,
    other.incident.idu.multiplier.2 = 3.52,
    
    idu.remission.multiplier = 1.71,
    
    idu.relapse.multiplier = 1.69,
    
    idu.mortality = .0166,
    
    #-- HIV-Specific Mortality --#
    hiv.mortality.0 = .105,
    hiv.mortality.2 = .0167,
    peak.hiv.mortality = .101,
    
    #-- Sexual Mixing by Age --#
    age.mixing.sd.mult = 1.37,
    
    #-- Other Sexual Mixing --#
    oe.female.pairings.with.msm = .053,
    oe.never.idu.pairings.with.idu = .265,
    fraction.heterosexual.male.pairings.with.male=.004,
    
    black.black.sexual.oe = 2.91,
    hispanic.hispanic.sexual.oe = 1.93,
    other.other.sexual.oe = 1.57,
    
    #-- Uncertainty About the Future --#
    #    current.gains.end.by.year = 2020,
    msm.fraction.trate.change.after.t2 = 0.096,
    heterosexual.fraction.trate.change.after.t2 = 0.097,
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

save(mcmc, DESCRIPTION, file=SAVE.TO.FILE)


if (1==2)
{
    simset = extract.simset(mcmc)
    plot.calibration.risk(simset)
}
