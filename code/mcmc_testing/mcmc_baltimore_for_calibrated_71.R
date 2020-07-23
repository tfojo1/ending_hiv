if (1==2)
{
    setwd('../../../Ending HIV/Ending_HIV/')
}

source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')

source('code/calibration/calibrated_parameters_68_helpers.R')
source('code/calibration/calibrated_parameters_71.R')

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

FILE.PREFIX = 'balt.71'
DESCRIPTION = "V71"
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
    global.trate = 0.108,
    
    #-- MSM Transmission --#
    black.msm.trate.peak = 4.03,
    black.msm.trate.0 = 1.96,
    black.msm.trate.1 = 2.5,#2.31,
    black.msm.trate.2 = 1.88,
    
    hispanic.msm.trate.peak = 1.90,
    hispanic.msm.trate.0 = 1.21,
    hispanic.msm.trate.1 = 0.845,
    hispanic.msm.trate.2 = 1.38,
    
    other.msm.trate.peak = 1.49,
    other.msm.trate.0 = .870,
    other.msm.trate.1 = .9,#.705,
    other.msm.trate.2 = .8,#1.09,
    
    #-- Heterosexual Transmission --#
    black.heterosexual.trate.peak = 1.29,
    black.heterosexual.trate.0 = 1.71,
    black.heterosexual.trate.1 = 1.32,
    black.heterosexual.trate.2 = 1.11,
    
    hispanic.heterosexual.trate.peak = .594,
    hispanic.heterosexual.trate.0 = .387,
    hispanic.heterosexual.trate.1 = .410,
    hispanic.heterosexual.trate.2 = .521,
    
    other.heterosexual.trate.peak = .296,
    other.heterosexual.trate.0 = 0.167,
    other.heterosexual.trate.1 = 0.156,
    other.heterosexual.trate.2 = 0.135,
    
    #-- IDU Transmission --#
    black.idu.trate.peak = 21.75,
    black.idu.trate.0 = 5.25,
    black.idu.trate.1 = 2.59,
    black.idu.trate.2 = 2.08,
    
    hispanic.idu.trate.peak = 1.22,
    hispanic.idu.trate.0 = .173,
    hispanic.idu.trate.1 = .204,
    hispanic.idu.trate.2 = .187,
    
    other.idu.trate.peak = 2.07,
    other.idu.trate.0 = 0.823,
    other.idu.trate.1 = 1.63,
    other.idu.trate.2 = 1.90,
    
    #-- Other Sexual Transmission Parameters --#
    male.vs.female.heterosexual.rr = .779,
    
    age1.msm.susceptibility.rr1 = 1.8,#1.64,
    age2.msm.susceptibility.rr1 = 1,#1.33,
    age3.msm.susceptibility.rr1 = 1,
    
    age1.msm.susceptibility.rr2 = 1.51,
    age2.msm.susceptibility.rr2 = 1.6,#1.37,
    age3.msm.susceptibility.rr2 = .5,
    
    age1.susceptibility.rr = 0.819,
    age2.susceptibility.rr = 1.08,
    age4.susceptibility.rr = .641,
    age5.susceptibility.rr = .418,
    
    #-- Aging --#
    msm.age1.aging.base = .282,
    msm.age2.aging.0 = .250,
    msm.age3.aging.1 = .425,
    msm.age4.aging.1 = .0753,
    
    heterosexual.age1.aging.base = .216,
    heterosexual.age2.aging.0 = .201,
    heterosexual.age3.aging.1 = .121,
    heterosexual.age4.aging.1 = .0313,
    
    idu.age1.aging.base = .304,
    idu.age2.aging.0 = .263,
    idu.age3.aging.1 = .116,
    idu.age4.aging.1 = .0586,
    
    #-- HIV Testing --#
    heterosexual.proportion.tested.or = 1.02,
    msm.proportion.tested.or = 0.327,
    idu.proportion.tested.or = 0.490,
    
    black.proportion.tested.or = 0.609,
    hispanic.proportion.tested.or = 0.968,
    
    age1.proportion.tested.or = 0.697,
    age2.proportion.tested.or = 0.538,
    age4.proportion.tested.or = 0.590,
    age5.proportion.tested.or = 0.940,
    
    total.proportion.tested.slope.or = 0.901,
    
    testing.ramp.up.vs.current.rr = 0.369,
    
    #-- Suppression --#
    heterosexual.suppressed.or = 0.869,
    msm.suppressed.or = 1.097,
    idu.suppressed.or = 0.999,
    
    black.suppressed.or = 1.97,
    hispanic.suppressed.or = 0.558,
    
    age1.suppressed.or = 0.913,
    age2.suppressed.or = 0.554,
    age4.suppressed.or = 0.965,
    age5.suppressed.or = 0.718,
    
    
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
    proportion.msm.of.male.mult = .933,
    
    #-- IDU Transitions --#
    black.incident.idu.multiplier.0 = 4.29,
    hispanic.incident.idu.multiplier.0 = 0.79,
    other.incident.idu.multiplier.0 = 1.25,
    
    black.incident.idu.multiplier.2 = 0.476,
    hispanic.incident.idu.multiplier.2 = 1.55,
    other.incident.idu.multiplier.2 = 3.35,
    
    idu.remission.multiplier = 1.51,
    
    idu.relapse.multiplier = 1.68,
    
    idu.mortality = .0164,
    
    #-- HIV-Specific Mortality --#
    hiv.mortality.0 = .043,
    hiv.mortality.2 = .0212,
    peak.hiv.mortality = .235,
    
    #-- Sexual Mixing by Age --#
    age.mixing.sd.mult = 0.672,
    
    #-- Other Sexual Mixing --#
    oe.female.pairings.with.msm = .096,
    fraction.heterosexual.male.pairings.with.male=.0035,
    oe.never.idu.pairings.with.idu = .256,
    
    black.black.sexual.oe = 3.011,
    hispanic.hispanic.sexual.oe = 2.19,
    other.other.sexual.oe = 1.73,
    
    #-- Acute HIV and the Effect of Diagnosis --#
    acute.transmissibility.rr = 14.38,
    diagnosed.transmission.rr = 0.300,
    
    #-- Uncertainty About the Future --#
    #    current.gains.end.by.year = 2020,
    msm.fraction.trate.change.after.t2 = 0.0187,
    heterosexual.fraction.trate.change.after.t2 = 0.0314,
    idu.fraction.trate.change.after.t2 = 0.117
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
