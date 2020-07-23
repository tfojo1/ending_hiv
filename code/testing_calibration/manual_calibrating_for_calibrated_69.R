source('code/source_code.R')
BALTIMORE.MSA='12580'

source('code/calibration/calibrated_parameters_68_helpers.R')
source('code/calibration/calibrated_parameters_69.R')


base.components = setup.initial.components(fix=F)


# a test
#base.components = setup.idu.mortality(base.components, .025)
start.time = Sys.time()
params =  c(
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
components = get.components.for.calibrated.parameters(components=base.components,
                                                            parameters=params
); sim=run.jheem.from.components(components)
end.time=Sys.time()
run.time = as.numeric(difftime(end.time, start.time))

print(plot.calibration.total(sim, years=1970:2020))

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
