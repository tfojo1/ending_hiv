source('code/source_code.R')
BALTIMORE.MSA='12580'

source('code/calibration/calibrated_parameters_68_helpers.R')
source('code/calibration/calibrated_parameters_70.R')


base.components = setup.initial.components(fix=F)


# a test
#base.components = setup.idu.mortality(base.components, .025)
start.time = Sys.time()
params =  c(
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
    
    total.suppressed.slope.or = 1.049,
    
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
components = get.components.for.calibrated.parameters(components=base.components,
                                                            parameters=params
); sim=run.jheem.from.components(components)
end.time=Sys.time()
run.time = as.numeric(difftime(end.time, start.time))

#print(plot.calibration.sex.age(sim))

print(plot.calibration.risk.race(sim))

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

cbind(melt(get.surveillance.data(location.codes=attr(sim,'location'), data.type='cumulative.aids.mortality', sex=T, risk=T, race=T)),
      sim=as.numeric(extract.overall.hiv.mortality(sim, years=1970:2000, continuum='diagnosed', keep.dimensions = c('race','sex','risk'), use.cdc.categorizations = T, per.population=NA) / 
                         extract.population.subset(sim, years=1970:2000, keep.dimensions=character(), per.population=NA) * 
                         sum(get.census.totals(ALL.DATA.MANAGERS$census.totals, location='12580', years=1970:2000))))

cbind(sum(get.surveillance.data(location.codes=attr(sim,'location'), data.type='cumulative.aids.mortality', sex=T, risk=T, race=T)),
      sim=extract.overall.hiv.mortality(sim, years=1970:2000, continuum='diagnosed', keep.dimensions = character(), per.population = NA) / 
          extract.population.subset(sim, years=1970:2000, keep.dimensions=character(), per.population=NA) * 
          sum(get.census.totals(ALL.DATA.MANAGERS$census.totals, location='12580', years=1970:2000)))


#how much of 2010 25-34 male incidence is het vs msm
prev.pop2=extract.prevalence(sim, years=2010, ages=2, sex='male', use.cdc.categorizations = T, keep.dimensions = 'risk', per.population=NA); round(prev.pop2/sum(prev.pop2), 2)

z=crunch.all.jheem.components(components)
age=1;cbind(z$aging.hiv.positive.years, sapply(z$aging.rates.hiv.positive, function(r){r[age,1,1,'msm','never_IDU',1,1,1]}))
}

if (1==2)
{
    plot.smoothed.proportions(attr(components, 'smoothed.testing.proportions'), race='black', facet.by='sex', split.by='age', risks='never_IDU') + geom_vline(xintercept=2020) + ylim(0,1)
}
