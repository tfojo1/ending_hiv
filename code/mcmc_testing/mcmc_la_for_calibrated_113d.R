if (1==2)
{
    setwd('../../../Ending HIV/Ending_HIV/')
}

source('code/source_code.R')
source('code/systematic_calibration/systematic_calibration.R')

source('code/calibration/calibrated_parameters_113_helpers.R')
source('code/calibration/calibrated_parameters_113d.R')

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

FILE.PREFIX = 'la.113d_revised.lik.v12'
DESCRIPTION = "V113d; prev cv 0.034 with CV only with ratio wt 2; new cv 0.065 with cv only; dx wt 1 with no strat; testing likelihood with total wt inv32 strat wt 0.5; supp wt 0.25; focus wt 1; aids dx wt 1"
RESUME.PRIOR.CACHE = NULL#'mcmc_runs/test_caches/la.113d_revised.lik.v12_20K_2020-09-20/'
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
    black.msm.trate.0 = 1.212,
    black.msm.trate.1 = 1.737,
    black.msm.trate.2 = 1.995,
    
    hispanic.msm.trate.0 = 1.155,
    hispanic.msm.trate.1 = 0.719,
    hispanic.msm.trate.2 = 0.818,
    
    other.msm.trate.0 = 1.1013,
    other.msm.trate.1 = 0.555,
    other.msm.trate.2 = 0.763,
    
    msm.peak.trate.multiplier = 1.6,
    
    #-- Heterosexual Transmission --#
    black.heterosexual.trate.0 = 1.314,
    black.heterosexual.trate.1 = 0.797,
    black.heterosexual.trate.2 = 1.434,
    
    hispanic.heterosexual.trate.0 = 0.345,
    hispanic.heterosexual.trate.1 = 0.163,
    hispanic.heterosexual.trate.2 = 0.297,
    
    other.heterosexual.trate.0 = 0.070,
    other.heterosexual.trate.1 = 0.136,
    other.heterosexual.trate.2 = 0.150,
    
    heterosexual.peak.trate.multiplier = 1.25,
    
    #-- IDU Transmission --#
    black.idu.trate.peak = 7.882,
    black.idu.trate.0 = 3.715,
    black.idu.trate.1 = 2.648,
    black.idu.trate.2 = 2.668,
    
    hispanic.idu.trate.peak = 2.657,
    hispanic.idu.trate.0 = 1.267,
    hispanic.idu.trate.1 = 1.541,
    hispanic.idu.trate.2 = 1.581,
    
    other.idu.trate.peak = 1.771,
    other.idu.trate.0 = 1.097,
    other.idu.trate.1 = 1.497,
    other.idu.trate.2 = 1.431,
    
    idu.peak.trate.multiplier = 1.5,
    
    #-- MSM-IDU Transmission --#
    msm.vs.heterosexual.male.idu.susceptibility.rr.peak = 2.314,
    msm.vs.heterosexual.male.idu.susceptibility.rr.0 = 6.729,
    msm.vs.heterosexual.male.idu.susceptibility.rr.1 = 3.187,
    msm.vs.heterosexual.male.idu.susceptibility.rr.2 = 1.101,

    
    #-- Other Sexual Transmission Parameters --#
    
    age1.susceptibility.rr.mult = 1.402,
    age2.susceptibility.rr.mult = 0.936,
    age4.susceptibility.rr.mult = 1.129,
    age5.susceptibility.rr.mult = 0.796,
    
    age1.msm.susceptibility.rr.mult.1 = 2.407,
    age2.msm.susceptibility.rr.mult.1 = 1.684,
    age4.msm.susceptibility.rr.mult.12 = 1.152,
    age5.msm.susceptibility.rr.mult.12 = 0.5,
    
    age1.msm.susceptibility.rr.mult.2 = 3.028,
    age2.msm.susceptibility.rr.mult.2 = 1.229,
    
    
    #-- Aging --#
    msm.age1.aging.base = 0.314,
    msm.age2.aging.0 = 0.117,
    msm.age2.aging.1 = 0.121,
    msm.age3.aging.1 = 0.13,
    
    heterosexual.age1.aging.base = 0.331,
    heterosexual.age2.aging.0 = 0.099,
    heterosexual.age2.aging.1 = 0.174,
    heterosexual.age3.aging.1 = 0.13,
    
    idu.age1.aging.base = 0.286,
    idu.age2.aging.0 = 0.2014,
    idu.age2.aging.1 = 0.240,
    idu.age3.aging.1 = 0.13,
    
    
    #-- Other Sexual Transmission Parameters --#
    male.vs.female.heterosexual.rr = 0.494,
    
    female.vs.heterosexual.male.idu.susceptibility.rr = 0.960,
    
    
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
    
    testing.ramp.up.vs.current.rr = 0.788,
    
    #-- Suppression --#
    heterosexual.suppressed.or = 0.415,
    msm.suppressed.or = 0.413,
    idu.suppressed.or = 0.498,
    msm.idu.suppressed.or = 0.279,
    
    black.suppressed.or = 0.973,
    hispanic.suppressed.or = 1.386,
    
    age1.suppressed.or = 3.607,
    age2.suppressed.or = 1.131,
    age4.suppressed.or = 1.176,
    age5.suppressed.or = 1.102,
    
    
    heterosexual.suppressed.slope.or = 0.869,
    msm.suppressed.slope.or = 0.882,
    idu.suppressed.slope.or = 0.891,
    msm.idu.suppressed.slope.or = 0.837,
    
    black.suppressed.slope.or = 0.995,
    hispanic.suppressed.slope.or = 1.025,
    
    age1.suppressed.slope.or = 1.119,
    age2.suppressed.slope.or = 0.981,
    age4.suppressed.slope.or = 1.006,
    age5.suppressed.slope.or = 0.990,
    
    
    #-- Proportion MSM --#
    proportion.msm.of.male.mult = 1.431,
    
    #-- IDU Transitions --#
    black.incident.idu.multiplier.0 = 4.337,
    hispanic.incident.idu.multiplier.0 = 1.167,
    other.incident.idu.multiplier.0 = 1.524,
    
    black.incident.idu.multiplier.2 = 0.725,
    hispanic.incident.idu.multiplier.2 = 2.122,
    other.incident.idu.multiplier.2 = 1.565,
    
    msm.incident.idu.multiplier.0 = 0.995,
    msm.incident.idu.multiplier.2 = 1.445,
    
    idu.remission.multiplier = 1.163,
    
    idu.relapse.multiplier = 1.296,
    
    #-- HIV-Specific Mortality --#
    hiv.mortality.0 = 0.027,
    hiv.mortality.2 = 0.014,
    peak.hiv.mortality = 0.185,
    
    #-- Sexual Mixing by Age --#
    age.mixing.sd.mult = 0.957,
    
    #-- Other Sexual Mixing --#
    oe.female.pairings.with.msm = 0.283,
    fraction.heterosexual.male.pairings.with.male=0.001,
    oe.never.idu.pairings.with.idu = 0.255,
    
    black.black.sexual.oe = 2.355,
    hispanic.hispanic.sexual.oe = 1.888,
    other.other.sexual.oe = 1.240,
    
    #-- Acute HIV and the Effect of Diagnosis --#
    acute.transmissibility.rr = 11.904,
    diagnosed.transmission.rr = 0.451,
    
    #-- Uncertainty About the Future --#
    #    current.gains.end.by.year = 2020,
    msm.fraction.trate.change.after.t2 = 0.052,
    heterosexual.fraction.trate.change.after.t2 = 0.043,
    idu.fraction.trate.change.after.t2 = 0.097
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
init.sds[grepl('peak.*mult', names(init.sds))] = init.sds[grepl('peak.*mult', names(init.sds))] / 16
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

likelihood.parameters = attr(full.likelihood, 'parameters')
save(mcmc, DESCRIPTION, likelihood.parameters, file=SAVE.TO.FILE)


if (1==2)
{
    simset = extract.simset(mcmc)
    plot.calibration.risk(simset)
}
