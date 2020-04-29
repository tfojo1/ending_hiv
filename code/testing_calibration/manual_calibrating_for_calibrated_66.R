source('code/source_code.R')
BALTIMORE.MSA='12580'

source('code/calibration/calibrated_parameters_63_helpers.R')
source('code/calibration/calibrated_parameters_66.R')


base.components = setup.initial.components(fix=F)


# a test
#base.components = setup.idu.mortality(base.components, .025)
start.time = Sys.time()
params = c(
    global.trate = 0.1,

    #-- Acute HIV and the Effect of Diagnosis --#
    acute.transmissibility.rr = 12,#16.8,
    diagnosed.transmission.rr = .3,#0.31,

    age1.msm.suppressed.or.slope = 1,
    age2.msm.suppressed.or.slope = 1,

    age1.msm.testing.or = 1,
    age2.msm.testing.or = 1,

    #-- MSM Transmission --#
    black.msm.trate.peak = 3,61,
    black.msm.trate.0 = 1.91,
    black.msm.trate.1 = 1.97,
    black.msm.trate.2 = 1.89,

    hispanic.msm.trate.peak = 3.71,
    hispanic.msm.trate.0 = 1.33,
    hispanic.msm.trate.1 = 1.34,
    hispanic.msm.trate.2 = 1.43,

    other.msm.trate.peak = 1.87,
    other.msm.trate.0 = .719,
    other.msm.trate.1 = .716,
    other.msm.trate.2 = .723,

    #-- Heterosexual Transmission --#
    black.heterosexual.trate.peak = 2.20,
    black.heterosexual.trate.0 = 1.28,
    black.heterosexual.trate.1 = 1.11,
    black.heterosexual.trate.2 = 1.07,

    hispanic.heterosexual.trate.peak = 1.18,
    hispanic.heterosexual.trate.0 = .392,
    hispanic.heterosexual.trate.1 = .421,
    hispanic.heterosexual.trate.2 = .413,

    other.heterosexual.trate.peak = .382,
    other.heterosexual.trate.0 = 0.172,
    other.heterosexual.trate.1 = 0.154,
    other.heterosexual.trate.2 = 0.165,

    #-- IDU Transmission --#
    black.idu.trate.peak = 36.85,
    black.idu.trate.0 = 5.73,
    black.idu.trate.1 = 4.34,
    black.idu.trate.2 = 3.89,

    hispanic.idu.trate.peak = 1.70,
    hispanic.idu.trate.0 = .228,
    hispanic.idu.trate.1 = .196,
    hispanic.idu.trate.2 = .217,

    other.idu.trate.peak = 2.47,
    other.idu.trate.0 = .922,
    other.idu.trate.1 = 1.06,
    other.idu.trate.2 = .797,

    #-- Other Sexual Transmission Parameters --#
    male.vs.female.heterosexual.rr = 0.595,

    age1.msm.susceptibility.rr1 = 1.59,
    age1.msm.susceptibility.rr2 = 1.08,
    age2.msm.susceptibility.rr1 = 1.36,
    age2.msm.susceptibility.rr2 = 1.15,

    age1.susceptibility.rr = 0.767,
    age2.susceptibility.rr = 1.265,
    age4.susceptibility.rr = 0.645,
    age5.susceptibility.rr = 0.248,

    #-- Aging --#
    msm.age1.aging.base = .286,
    msm.age2.aging.0 = .199,
    msm.age3.aging.1 = .246,
    msm.age4.aging.1 = .069,

    heterosexual.age1.aging.base = .135,
    heterosexual.age2.aging.0 = .187,
    heterosexual.age3.aging.1 = .110,
    heterosexual.age4.aging.1 = .041,

    idu.age1.aging.base = .163,
    idu.age2.aging.0 = .228,
    idu.age3.aging.1 = .155,
    idu.age4.aging.1 = .050,

    #-- HIV Testing --#
    heterosexual.proportion.tested.or = .25,#0.251,
    msm.proportion.tested.or =.25*1.5,
    idu.proportion.tested.or = .25*1.1,

    total.testing.slope.or = 1,

    testing.ramp.up.vs.current.rr = 0.401,

    #-- Proportion MSM --#
    proportion.msm.of.male.mult = 1.03,

    #-- IDU Transitions --#
    black.incident.idu.multiplier.0 = 3.33,
    hispanic.incident.idu.multiplier.0 = 1.18,
    other.incident.idu.multiplier.0 = 1.33,

    black.incident.idu.multiplier.2 = 0.76,
    hispanic.incident.idu.multiplier.2 = 0.84,
    other.incident.idu.multiplier.2 = 2.89,

    idu.remission.multiplier = 1.3,

    idu.relapse.multiplier = 1.35,

    #-- HIV-Specific Mortality --#
    hiv.mortality.0= .051,
    hiv.mortality.2 = .0172,
    peak.hiv.mortality = 6.62*0.051,

    #-- Sexual Mixing by Age --#
    age.mixing.sd.mult = 1.18,

    #-- Other Sexual Mixing --#
    oe.female.pairings.with.msm = .116,
    oe.never.idu.pairings.with.idu = .246,

    black.black.sexual.oe = 3.02,
    hispanic.hispanic.sexual.oe = 2.34,
    other.other.sexual.oe = 1.62,

    #-- Suppression --#
    total.suppressed.or = 1.05,

    #-- Uncertainty About the Future --#
    #    current.gains.end.by.year = 2020,
    msm.fraction.trate.change.after.t2 = 0.067,
    heterosexual.fraction.trate.change.after.t2 = 0.124,
    idu.fraction.trate.change.after.t2 = 0.0167
)
components = get.components.for.calibrated.parameters(components=base.components,
                                                            parameters=params
); sim=run.jheem.from.components(components)
end.time=Sys.time()
run.time = as.numeric(difftime(end.time, start.time))

#print(plot.calibration(sim))

#-- PLOTS --#

#print(plot.calibration.sex.age(sim))
if (1==2)
{
#plot.calibration.total(sim)
plot.calibration.risk(sim)
plot.calibration.risk(sim, years=1970:2020)

plot.calibration.total(sim, years=2000:2020)
plot.calibration.race(sim)
plot.calibration.age(sim)

plot.calibration.total(sim, years=1970:2020)
plot.calibration.risk(sim, years=1970:2020)
plot.calibration.risk.race(sim)

#print(plot.calibration.sex.age(sim, years=2000:2020))
plot.calibration.sex.age(sim)
plot.calibration(sim, facet.by=c('age','sex'), split.by = NULL, data.type='prevalence', years=1970:2020)

plot.calibration.sex.risk(sim)
plot.calibration.race.risk(sim, years=1970:2020, risk='idu')

plot.calibration.idu.prevalence(sim)

plot.calibration(sim, facet.by=c('year','risk'), years = c('1990','2010'), x.variables='age', split.by=NULL, data.types='new')

prev.pop = extract.prevalence(sim, years=2010:2017, keep.dimensions=c('year','risk'), use.cdc.categorizations = T) *
    apply(BALTIMORE.POPULATION.CDC[as.character(2010:2017),,,,], c('year','risk'), sum); round(t(t(prev.pop)/rowSums(prev.pop)),2)

#how much of 2010 25-34 male incidence is het vs msm
prev.pop2=extract.prevalence(sim, years=2010, ages=2, sex='male', use.cdc.categorizations = T, keep.dimensions = 'risk', per.population=NA); round(prev.pop2/sum(prev.pop2), 2)

z=crunch.all.jheem.components(components)
age=1;cbind(z$aging.hiv.positive.years, sapply(z$aging.rates.hiv.positive, function(r){r[age,1,1,'msm','never_IDU',1,1,1]}))
}

if (1==2)
{
    plot.smoothed.proportions(attr(components, 'smoothed.testing.proportions'), race='black', facet.by='sex', split.by='age', risks='never_IDU') + geom_vline(xintercept=2020) + ylim(0,1)
}
