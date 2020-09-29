if (1==2)
{
    setwd('../../../Ending HIV/Ending_HIV/')
}

source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')

source('code/calibration/calibrated_parameters_87_helpers.R')
source('code/calibration/calibrated_parameters_108b.R')

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

FILE.PREFIX = 'la.108b_revised.lik.v5'
DESCRIPTION = "V108b; prev cv 0.034 with CV only with ratio wt 2; new cv 0.065 with cv only; dx wt 0.5 with no strat; testing likelihood with wt 1; supp wt 0.25; focus wt 1; aids dx wt 1"
RESUME.PRIOR.CACHE = NULL#'mcmc_runs/test_caches/la.108_revised.lik.v2_20K_2020-09-15/'
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


# Set up the initial components
base.components = setup.initial.components(msa=msa)

init.parameters = c(
    global.trate = 0.131,
    
    #-- MSM Transmission --#
    black.msm.trate.peak = 8.325,
    black.msm.trate.0 = 1.212,
    black.msm.trate.1 = 1.737,
    black.msm.trate.2 = 1.995,
    
    hispanic.msm.trate.peak = 1.060,
    hispanic.msm.trate.0 = 1.155,
    hispanic.msm.trate.1 = 0.719,
    hispanic.msm.trate.2 = 0.818,
    
    other.msm.trate.peak = 1.378,
    other.msm.trate.0 = 1.125,
    other.msm.trate.1 = 0.787 * 0.703,
    other.msm.trate.2 = 0.848 * 0.618,
    
    #-- Heterosexual Transmission --#
    black.heterosexual.trate.peak = 1.858,
    black.heterosexual.trate.0 = 0.891,
    black.heterosexual.trate.1 = 1.275,
    black.heterosexual.trate.2 = 1.527,
    
    hispanic.heterosexual.trate.peak = 0.331,
    hispanic.heterosexual.trate.0 = 0.375,
    hispanic.heterosexual.trate.1 = 0.204,
    hispanic.heterosexual.trate.2 = 0.254,
    
    other.heterosexual.trate.peak = 0.217,
    other.heterosexual.trate.0 = 0.140,
    other.heterosexual.trate.1 = 0.071,
    other.heterosexual.trate.2 = 0.206,
    
    #-- IDU Transmission --#
    black.idu.trate.peak = 6.638,
    black.idu.trate.0 = 2.623,
    black.idu.trate.1 = 4.347,
    black.idu.trate.2 = 4.014,
    
    hispanic.idu.trate.peak = 2.523,
    hispanic.idu.trate.0 = 0.958,
    hispanic.idu.trate.1 = 1.451,
    hispanic.idu.trate.2 = 1.657,
    
    other.idu.trate.peak = 2.150,
    other.idu.trate.0 = 1.212,
    other.idu.trate.1 = 1.838,
    other.idu.trate.2 = 1.402,
    
    
    #-- MSM-IDU Transmission --#
    msm.vs.heterosexual.male.idu.susceptibility.rr.peak = 2.484,
    msm.vs.heterosexual.male.idu.susceptibility.rr.0 = 6.688,
    msm.vs.heterosexual.male.idu.susceptibility.rr.1 = 3.200,
    msm.vs.heterosexual.male.idu.susceptibility.rr.2 = 3.463,

    
    #-- Other Sexual Transmission Parameters --#
    
    age1.msm.susceptibility.rr.mult.1 = 1.341 / 0.668 / 0.703,
    age2.msm.susceptibility.rr.mult.1 = 1.176 / 1.108 / 0.703,
    age4.msm.susceptibility.rr.mult.12 = 1.133 / 0.703,
    age5.msm.susceptibility.rr.mult.12 = 0.551 / 0.703,
    
    age1.msm.susceptibility.rr.mult.2 = 1.168 / 0.668 / 0.618,
    age2.msm.susceptibility.rr.mult.2 = 1.233 / 1.108 / 0.618,
    
    age1.susceptibility.rr.mult = 1.416,
    age2.susceptibility.rr.mult = 0.918,
    age4.susceptibility.rr.mult = 1.133,
    age5.susceptibility.rr.mult = 0.551,
    
    #-- Aging --#
    msm.age1.aging.base = 0.417,
    msm.age2.aging.0 = 0.158,
    msm.age2.aging.1 = 0.121,
    msm.age3.aging.1 = 0.074,

    heterosexual.age1.aging.base = 0.197,
    heterosexual.age2.aging.0 = 0.217,
    heterosexual.age2.aging.1 = 0.174,
    heterosexual.age3.aging.1 = 0.075,

    idu.age1.aging.base = 0.188,
    idu.age2.aging.0 = 0.196,
    idu.age2.aging.1 = 0.240,
    idu.age3.aging.1 = 0.127,

    
    #-- Other Sexual Transmission Parameters --#
    male.vs.female.heterosexual.rr = 0.484,
    
    female.vs.heterosexual.male.idu.susceptibility.rr = 0.886,
    
    
    #-- HIV Testing --#
    heterosexual.proportion.tested.or = 1,
    msm.proportion.tested.or = 1,
    idu.proportion.tested.or = 1,
    msm.idu.proportion.tested.or = 1,
    
    black.proportion.tested.or = 1,
    hispanic.proportion.tested.or = 1,
    
    age1.proportion.tested.or = 1,
    age2.proportion.tested.or = 1,
    age4.proportion.tested.or = 1,
    age5.proportion.tested.or = 1,
    
    heterosexual.proportion.tested.slope.or = 1,
    msm.proportion.tested.slope.or = 1,
    idu.proportion.tested.slope.or = 1,
    msm.idu.proportion.tested.slope.or = 1,
    
    testing.ramp.up.vs.current.rr = 0.781,
    
    #-- Suppression --#
    heterosexual.suppressed.or = 0.535,
    msm.suppressed.or = 0.528,
    idu.suppressed.or = 0.440,
    msm.idu.suppressed.or = 1.359 / 3.950,
    
    black.suppressed.or = 1.099,
    hispanic.suppressed.or = 1.164,
    
    age1.suppressed.or = 3.404,
    age2.suppressed.or = 1.038,
    age4.suppressed.or = 1.263,
    age5.suppressed.or = 1.125,
    
    
    heterosexual.suppressed.slope.or = 0.894,
    msm.suppressed.slope.or = 0.905,
    idu.suppressed.slope.or = 0.860,
    msm.idu.suppressed.slope.or = 1.032 / 1.181,
    
    black.suppressed.slope.or = 1.017,
    hispanic.suppressed.slope.or = 1.004,
    
    age1.suppressed.slope.or = 1.126,
    age2.suppressed.slope.or = 0.975,
    age4.suppressed.slope.or = 1.023,
    age5.suppressed.slope.or = 0.977,
    

    #-- Proportion MSM --#
    proportion.msm.of.male.mult = 1.201,
    
    #-- IDU Transitions --#
    black.incident.idu.multiplier.0 = 3.974,
    hispanic.incident.idu.multiplier.0 = 1.180,
    other.incident.idu.multiplier.0 = 1.046,
    
    black.incident.idu.multiplier.2 = 0.725,
    hispanic.incident.idu.multiplier.2 = 2.001,
    other.incident.idu.multiplier.2 = 1.582,
    
    msm.incident.idu.multiplier.0 = 1,
    msm.incident.idu.multiplier.2 = 1,
    
    idu.remission.multiplier = 1.091,
    
    idu.relapse.multiplier = 1.351,
    
    #-- HIV-Specific Mortality --#
    hiv.mortality.0 = 0.034,
    hiv.mortality.2 = 0.014,
    peak.hiv.mortality = 0.156,
    
    #-- Sexual Mixing by Age --#
    age.mixing.sd.mult = 0.850,
    
    #-- Other Sexual Mixing --#
    oe.female.pairings.with.msm = 0.262,
    fraction.heterosexual.male.pairings.with.male=0.001,
    oe.never.idu.pairings.with.idu = 0.256,
    
    black.black.sexual.oe = 2.555,
    hispanic.hispanic.sexual.oe = 1.984,
    other.other.sexual.oe = 1.232,
    
    #-- Acute HIV and the Effect of Diagnosis --#
    acute.transmissibility.rr = 10.286,
    diagnosed.transmission.rr = 0.396,
    
    #-- Uncertainty About the Future --#
    #    current.gains.end.by.year = 2020,
    msm.fraction.trate.change.after.t2 = 0.040,
    heterosexual.fraction.trate.change.after.t2 = 0.062,
    idu.fraction.trate.change.after.t2 = 0.063
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

#a quick check

init.sim = run.simulation(init.parameters)
print(plot.calibration.risk(init.sim) + ggtitle(paste0('INITIAL SIMULATION: ', FILE.PREFIX)))


# Set up the likelihood

cat('Building likelihood...')
full.likelihood = create.msa.likelihood(msa)
cat('Done\n')



# set up step sizes
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
