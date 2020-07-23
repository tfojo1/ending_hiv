
#Load up the mcmc
mcmc = assemble.mcmc.from.cache('mcmc_runs/la.73_2020-07-21/',T)
msa = attr(mcmc@simulations[[1]], 'location'); msa.names(msa)

#Set up the run sim function
base.components = setup.initial.components(msa=msa)
init.components = get.components.for.calibrated.parameters(mcmc@samples[1,1,], base.components)
init.components = fix.components.for.calibration(components = init.components)
run.simulation <- function(parameters)
{
    components = get.components.for.calibrated.parameters(parameters, init.components)
    run.jheem.from.components(components, max.run.time.seconds = Inf)
}

#Get sims for specific parameters
iteration = 500
pp1 = mcmc@samples[1,iteration,]

#test against these
pp2 = c(
    global.trate = 0.138,
    
    #-- MSM Transmission --#
    black.msm.trate.peak = 12.894,
    black.msm.trate.0 = 0.765,
    black.msm.trate.1 = 2.976,
    black.msm.trate.2 = 2.835,
    
    hispanic.msm.trate.peak = 1.737,
    hispanic.msm.trate.0 = 0.934,
    hispanic.msm.trate.1 = 1.294,
    hispanic.msm.trate.2 = 1.207,
    
    other.msm.trate.peak = 0.561,
    other.msm.trate.0 = 1.200,
    other.msm.trate.1 = 1.124,
    other.msm.trate.2 = 1.033,
    
    #-- Heterosexual Transmission --#
    black.heterosexual.trate.peak = 0.617,
    black.heterosexual.trate.0 = 1.283,
    black.heterosexual.trate.1 = 2.170,
    black.heterosexual.trate.2 = 1.220,
    
    hispanic.heterosexual.trate.peak = 0.346,
    hispanic.heterosexual.trate.0 = 0.552,
    hispanic.heterosexual.trate.1 = 0.433,
    hispanic.heterosexual.trate.2 = 0.247,
    
    other.heterosexual.trate.peak = 0.210,
    other.heterosexual.trate.0 = 0.125,
    other.heterosexual.trate.1 = 0.189,
    other.heterosexual.trate.2 = 0.257,
    
    #-- IDU Transmission --#
    black.idu.trate.peak = 11.408,
    black.idu.trate.0 = 5.434,
    black.idu.trate.1 = 4.153,
    black.idu.trate.2 = 5.122,
    
    hispanic.idu.trate.peak = 2.920,
    hispanic.idu.trate.0 = 0.362,
    hispanic.idu.trate.1 = 2.570,
    hispanic.idu.trate.2 = 0.168,
    
    other.idu.trate.peak = 2.553,
    other.idu.trate.0 = 1.750,
    other.idu.trate.1 = 2.556,
    other.idu.trate.2 = 2.135,
    
    #-- Other Sexual Transmission Parameters --#
    age1.msm.susceptibility.rr1 = 1.200,
    age2.msm.susceptibility.rr1 = 1.200,
    age3.msm.susceptibility.rr1 = 0.646,
    
    age1.msm.susceptibility.rr2 = 1.577,
    age2.msm.susceptibility.rr2 = 1.200,
    age3.msm.susceptibility.rr2 = 0.628,
    
    age1.susceptibility.rr = 0.997,
    age2.susceptibility.rr = 1.185,
    age4.susceptibility.rr = 0.737,
    age5.susceptibility.rr = 0.150,
    
    
    #-- Aging --#
    msm.age1.aging.base = 0.400,
    msm.age2.aging.0 = 0.227,
    msm.age2.aging.1 = 0.150,
    msm.age3.aging.1 = 0.109,
    msm.age3.aging.2 = 0.200,
    msm.age4.aging.1 = 0.055,
    msm.age4.aging.2 = 0.150,
    
    heterosexual.age1.aging.base = 0.167,
    heterosexual.age2.aging.0 = 0.258,
    heterosexual.age2.aging.1 = 0.098,
    heterosexual.age3.aging.1 = 0.103,
    heterosexual.age3.aging.2 = 0.100,
    heterosexual.age4.aging.1 = 0.041,
    heterosexual.age4.aging.2 = 0.101,
    
    idu.age1.aging.base = 0.159,
    idu.age2.aging.0 = 0.167,
    idu.age2.aging.1 = 0.101,
    idu.age3.aging.1 = 0.123,
    idu.age3.aging.2 = 0.100,
    idu.age4.aging.1 = 0.050,
    idu.age4.aging.2 = 0.100,
    
    
    #-- Other Sexual Transmission Parameters --#
    male.vs.female.heterosexual.rr = 0.405,
    
    #-- HIV Testing --#
    heterosexual.proportion.tested.or = 0.794,
    msm.proportion.tested.or = 0.268,
    idu.proportion.tested.or = 0.843,
    
    black.proportion.tested.or = 1.002,
    hispanic.proportion.tested.or = 0.940,
    
    age1.proportion.tested.or = 0.825,
    age2.proportion.tested.or = 0.554,
    age4.proportion.tested.or = 1.153,
    age5.proportion.tested.or = 0.753,
    
    total.proportion.tested.slope.or = 0.912,
    
    testing.ramp.up.vs.current.rr = 0.611,
    
    #-- Suppression --#
    heterosexual.suppressed.or = 1.798,
    msm.suppressed.or = 1.044,
    idu.suppressed.or = 1.162,
    
    black.suppressed.or = 0.703,
    hispanic.suppressed.or = 0.747,
    
    age1.suppressed.or = 0.604,
    age2.suppressed.or = 1.437,
    age4.suppressed.or = 0.804,
    age5.suppressed.or = 1.437,
    
    
    heterosexual.suppressed.slope.or = 1.005,
    msm.suppressed.slope.or = 1.001,
    idu.suppressed.slope.or = 0.996,
    
    black.suppressed.slope.or = 0.975,
    hispanic.suppressed.slope.or = 0.937,
    
    age1.suppressed.slope.or = 1.009,
    age2.suppressed.slope.or = 1.000,
    age4.suppressed.slope.or = 0.984,
    age5.suppressed.slope.or = 1.004,
    
    
    #-- Proportion MSM --#
    proportion.msm.of.male.mult = 1.043,
    
    #-- IDU Transitions --#
    black.incident.idu.multiplier.0 = 3.241,
    hispanic.incident.idu.multiplier.0 = 1.436,
    other.incident.idu.multiplier.0 = 1.305,
    
    black.incident.idu.multiplier.2 = 1.332,
    hispanic.incident.idu.multiplier.2 = 1.942,
    other.incident.idu.multiplier.2 = 3.365,
    
    idu.remission.multiplier = 2.138,
    
    idu.relapse.multiplier = 1.927,
    
    idu.mortality = 0.012,
    
    #-- HIV-Specific Mortality --#
    hiv.mortality.0 = 0.021,
    hiv.mortality.2 = 0.011,
    peak.hiv.mortality = 0.208,
    
    #-- Sexual Mixing by Age --#
    age.mixing.sd.mult = 1.192,
    
    #-- Other Sexual Mixing --#
    oe.female.pairings.with.msm = 0.176,
    fraction.heterosexual.male.pairings.with.male=0.001,
    oe.never.idu.pairings.with.idu = 0.182,
    
    black.black.sexual.oe = 2.888,
    hispanic.hispanic.sexual.oe = 2.087,
    other.other.sexual.oe = 1.768,
    
    #-- Acute HIV and the Effect of Diagnosis --#
    acute.transmissibility.rr = 11.449,
    diagnosed.transmission.rr = 0.311,
    
    #-- Uncertainty About the Future --#
    #    current.gains.end.by.year = 2020,
    msm.fraction.trate.change.after.t2 = 0.087,
    heterosexual.fraction.trate.change.after.t2 = 0.061,
    idu.fraction.trate.change.after.t2 = 0.155
)

sim1 = run.simulation(pp1)
sim2 = run.simulation(pp2)

plot.calibration.total(list(sim1,sim2))
plot.calibration.sex.age(list(sim1, sim2))
plot.calibration.race(list(sim1,sim2))
plot.calibration.risk(list(sim1,sim2))
df = data.frame(x=1999:2002,y=c(2693,2616,2562,2516),group='CDC',ci.lower=as.numeric(NA),ci.upper=as.numeric(NA),subtype='CDC')
plot.calibration.total(list(sim1,sim2), data.types='new', years=1970:2020)+ 
    geom_text(x=c(1999), y=c(2999), label='X')+ 
    geom_text(x=c(2000), y=c(2418), label='X')+ 
    geom_text(x=c(2001), y=c(2010), label='X')+ 
    geom_text(x=c(2002), y=c(2355), label='X')

full.likelihood = create.msa.likelihood(msa)
aids.lik = attr(full.likelihood, 'components')[['aids']]

exp(-full.likelihood(sim1, verbose=T) + full.likelihood(sim2, verbose=T))

exp(-aids.lik(sim1, verbose=T)+aids.lik(sim2, verbose=T))
