#source('../code/setup/setup_helpers.R')
#source('../code/setup/setup_jheem_from_components.R')
#source('../code/setup/smoothing_proportions.R')

##-- THE GENERAL IDEA --##
##
## Set up what we can beforehand
## Do all data pulls here in setup_jheem_components
## Then crunch numbers JIT in setup_jheem_from_components
## And produce a final JHEEM


##--------------------##
##-- INITIALIZATION --##
##--------------------##

initialize.jheem.components <- function(settings,
                                       fips,
                                       data.managers,
                                       population.years,
                                       verbose=F,
                                       model.hiv.transmission)
{
    components = list()
    components$fixed = F

    #-- Store settings and fips--#
    components$settings = settings
    components$fips = fips

    #-- By default, we will plan to model PrEP, IDU, and HIV transmission --#
    components$model.idu = T
    components$model.hiv.transmission = model.hiv.transmission
    components$model.prep = T

    components = setup.track.mortality(components)

    #-- Pull Populations --#
    components = setup.populations(components, data.managers, population.years)

    #-- Default settings --#
    if (verbose)
        print("Using default settings for birth proportions, fertility, and initial population")
    components = setup.birth.proportions(components)
    components = setup.initial.population(components)
    components = setup.fertility(components, data.managers)
    components = setup.aging(components)
    components = setup.idu.age1.aging(components)

    components = set.background.suppression.ors(components)
    components = set.background.hiv.testing.rate.ratios(components)
    components = set.background.hiv.testing.rate.slopes(components)
    components = set.background.hiv.testing.slope.time(components)

    components = setup.global.trates(components, global.sexual.trate = 1, global.idu.trate = 1)

    #-- Return --#
    components
}

##----------------------------------------------------##
##--              MAJOR CONTROL SETTINGS            --##
##-- (Whether to model HIV transmission, IDU, PrEP) --##
##----------------------------------------------------##

set.model.idu <- function(components,
                          model.idu=T)
{
    components$model.idu = model.idu
    components = clear.dependent.values(components, 'model.idu')

    components
}


set.model.prep <- function(components,
                           model.prep=T)
{
    components$model.prep = model.prep
    components = clear.dependent.values(components, 'model.prep')

    components
}

set.model.hiv.transmission <- function(components,
                                       model.hiv.transmission=F)
{
    components$model.hiv.transmission = model.hiv.transmission
    components = clear.dependent.values(components, 'model.hiv.transmission')

    components
}

setup.track.mortality <- function(components,
                                  track.hiv.specific.mortality=T,
                                  track.overall.hiv.positive.mortality=T,
                                  track.overall.hiv.negative.mortality=F)
{
    components$track.hiv.specific.mortality = track.hiv.specific.mortality
    components$track.overall.hiv.positive.mortality = track.overall.hiv.positive.mortality
    components$track.overall.hiv.negative.mortality = track.overall.hiv.negative.mortality

    components = clear.dependent.values(components, 'track.mortality')

    components
}

##----------------------##
##-- PULL POPULATIONS --##
##----------------------##

setup.populations <- function(components, data.managers, population.years)
{
    #-- Slice and dice races and ages --#
    county.populations = get.census.data(data.managers$census.collapsed, fips=components$fips,
                                         year=population.years, aggregate.years = T) / length(population.years)
    county.populations.collapsed = collapse.races(county.populations)

    population.collapsed = colSums(county.populations.collapsed)
    population = colSums(county.populations)

    population.full.age = get.census.data(data.managers$census.full, fips=components$fips,
                                          year=population.years,
                                          aggregate.counties = T, aggregate.years = T) / length(population.years)
    population.full.age.collapsed = collapse.races(population.full.age)

    components$populations = list(years=population.years,
                                  by.county.all.races=county.populations,
                                  by.county.collapsed.races=county.populations.collapsed,
                                  all.races = population,
                                  collapsed.races = population.collapsed,
                                  full.age = population.full.age,
                                  full.age.collapsed.races=population.full.age.collapsed)

    #-- Update Dependent Values --#

    if (!is.null(components$fertility))
        components = setup.fertility(components, data.managers=data.managers,
                                     use.birth.rates=components$use.birth.rates)

    #-- Return --#
    components = clear.dependent.values(components, c('fertility', 'populations'))
    components
}


##---------------------------------##
##-- PROPORTIONS for MSM and IDU --##
##---------------------------------##

set.proportions.msm.of.male <- function(components,
                                        black.proportion,
                                        hispanic.proportion,
                                        other.proportion,
                                        overwrite.base.proportions=F)
{
    #-- Set it --#
    components$proportions.msm.of.male = c(black=as.numeric(black.proportion),
                                           hispanic=as.numeric(hispanic.proportion),
                                           other = as.numeric(other.proportion))

    #-- Consistency checks --#
    if (any(components$proportions.msm.of.male>1) || any(components$proportions.msm.of.male<0))
        stop("MSM proportions must be between 0 and 1")

    if (any(components$proportions.msm.of.male>0.1))
        warning("Some MSM proportions have been set to be greater than 10% - is this intended?")

    if (overwrite.base.proportions)
        components$base.proportions.msm.of.male = components$proportions.msm.of.male

    #-- Return --#
    components = clear.dependent.values(components, 'proportions.msm.of.male')
    components
}

set.idu.proportions <- function(components,
                                data.managers,
                                active.idu.prevalence,
                                idu.ever.prevalence)
{
    #-- Check arguments --#
    if (any(active.idu.prevalence<0) || any(active.idu.prevalence>1))
        stop('All values of active.idu.prevalence must be between 0 and 1')

    if (any(idu.ever.prevalence<0) || any(idu.ever.prevalence>1))
        stop('All values of idu.ever.prevalence must be between 0 and 1')

    if (any(active.idu.prevalence > idu.ever.prevalence))
        stop('All values of active.idu.prevalence must be less than or equal to the corresponding values in idu.ever.prevalence')
    #going to need more arguments


    #-- Store --#
    components$active.idu.prevalence = active.idu.prevalence
    components$idu.ever.prevalence = idu.ever.prevalence


    #-- Return --#
    components = clear.dependent.values(components, c('active.idu.prevalence','idu.ever.prevalence'))
    components
}


##----------------------##
##-- FIX STRATA SIZES --##
##----------------------##

setup.fix.strata.sizes <- function(components,
                                   fix.strata.sizes,
                                   times)
{
    components$fix.strata.sizes = fix.strata.sizes
    components$fix.strata.size.times = times

    components = clear.dependent.values(components, 'fix.strata.sizes')

    components
}
##------------------------##
##-- INITIAL POPULATION --##
##------------------------##

setup.initial.population <- function(components,
                                     seed.rate.per.stratum=NULL,
                                     num.to.seed.per.race=1,
                                     seed.one.other.msm=F)
{
    components$seed.rate.per.stratum = seed.rate.per.stratum
    components$num.to.seed.per.race = num.to.seed.per.race
    components$seed.one.other.msm = seed.one.other.msm

    components = clear.dependent.values(components, c('seed.rate.per.stratum','num.to.seed.per.race'))

    components
}

get.minimum.prevalence <- function(surv=msa.surveillance.all.race,
                                   census,
                                   year,
                                   location)
{
    get.surveillance.data(surv, location.codes = location)
}

##---------------------------------------------##
##-- FERTILITY, BIRTHS, AGING, and MORTALITY --##
##---------------------------------------------##

#If use.birth.rates is TRUE, sets fertility rates as birth rates (ie, per total population)
# Otherwise, sets age-specific fertility rates for females
setup.fertility <- function(components,
                            data.managers,
                            use.birth.rates=T)
{
    if (is.null(components$fips))
        stop("components has not been initialized as JHEEM components")

    components$use.birth.rates = use.birth.rates

    if (use.birth.rates)
    {
        birth.rates = get.birth.rates(data.managers$fertility,
                                      fips=components$fips,
                                      census=data.managers$census.collapsed,
                                      aggregate.counties = T)
        birth.rates = array(birth.rates, dim=c(race=length(birth.rates)), dimnames=list(race=names(birth.rates)))
        pop.by.race = apply(components$populations$all.races, 'race', sum)
        pop.by.race = array(pop.by.race, dim=c(race=length(pop.by.race)), dimnames=list(race=names(pop.by.race)))
        birth.rates = collapse.races.for.rates(pop.by.race, rates=birth.rates)

        components$fertility = birth.rates
    }
    else
    {
        stop("Fertility rates (as opposed to birth rates) is not implemented at this time")
    }

    components = clear.dependent.values(components, 'fertility')

    components
}

setup.birth.proportions <- function(components,
                                    male.to.female.birth.ratio=105/100)
{
    if (male.to.female.birth.ratio < 0)
        stop('The male.to.female.birth.ratio must be non-negative')

    components$male.to.female.birth.ratio = male.to.female.birth.ratio

    components = clear.dependent.values(components, 'male.to.female.birth.ratio')

    components
}

setup.general.mortality <- function(components,
                                    data.managers,
                                    excess.idu.mortality=NULL)
{
    if (!is.null(excess.idu.mortality))
        components = setup.idu.mortality(components, excess.idu.mortality)

    components$raw.mortality.rates = get.mortality.rates(data.managers$mortality, states=state.for.county(components$fips), verbose=F)

    components = clear.dependent.values(components, c('raw.mortality.rates'))
    components
}

setup.idu.mortality <- function(components, excess.idu.mortality)
{
    if (is.null(dim(excess.idu.mortality)))
        excess.idu.mortality = as.numeric(excess.idu.mortality)
    components$excess.idu.mortality = excess.idu.mortality

    components = clear.dependent.values(components, c('excess.idu.mortality'))
    components
}



setup.hiv.mortality.years <- function(components,
                              routes=c('idu','msm','heterosexual'),
                              races=c('black','hispanic','other'),
                              age.indices=1:5,
                              t.pre.peak=NA,
                              t.peak.start=NA,
                              t.peak.end=NA,
                              t0.start=NA,
                              t0.end=NA,
                              t1=NA,
                              t2=NA,
                              t.end=NA)
{
    for (route in routes)
    {
        for (race in races)
        {
            for (age in paste0('age', age.indices))
            {
                if (!is.na(t.pre.peak))
                    components[[paste0(route, '.hiv.mortality.rates')]][[race]][[age]]$t.pre.peak = t.pre.peak
                if (!is.na(t.peak.start))
                    components[[paste0(route, '.hiv.mortality.rates')]][[race]][[age]]$t.peak.start = t.peak.start
                if (!is.na(t.peak.end))
                    components[[paste0(route, '.hiv.mortality.rates')]][[race]][[age]]$t.peak.end = t.peak.end
                if (!is.na(t0.start))
                    components[[paste0(route, '.hiv.mortality.rates')]][[race]][[age]]$t0.start = t0.start
                if (!is.na(t0.end))
                    components[[paste0(route, '.hiv.mortality.rates')]][[race]][[age]]$t0.end = t0.end
                if (!is.na(t1))
                    components[[paste0(route, '.hiv.mortality.rates')]][[race]][[age]]$t1 = t1
                if (!is.na(t2))
                    components[[paste0(route, '.hiv.mortality.rates')]][[race]][[age]]$t2 = t2
                if (!is.na(t.end))
                    components[[paste0(route, '.hiv.mortality.rates')]][[race]][[age]]$t.end = t.end
            }
        }
    }

    components = clear.dependent.values(components, c('untreated.hiv.mortality', 'untreated.aids.mortality'))
    components
}

setup.hiv.mortality.rates <- function(components,
                         routes=c('idu','msm','heterosexual'),
                         races=c('black','hispanic','other'),
                         age.indices = 1:5,
                         r.peak=NA,
                         r0=NA,
                         r1=NA,
                         r2=NA,
                         fraction.change.after.end=NA)
{
    for (route in routes)
    {
        for (race in races)
        {
            for (age in paste0('age', age.indices))
            {
                if (!is.na(r.peak))
                    components[[paste0(route, '.hiv.mortality.rates')]][[race]][[age]]$r.peak = r.peak
                if (!is.na(r.peak))
                    components[[paste0(route, '.hiv.mortality.rates')]][[race]][[age]]$r0 = r0
                if (!is.na(r.peak))
                    components[[paste0(route, '.hiv.mortality.rates')]][[race]][[age]]$r1 = r1
                if (!is.na(r.peak))
                    components[[paste0(route, '.hiv.mortality.rates')]][[race]][[age]]$r2 = r2
                if (!is.na(fraction.change.after.end))
                    components[[paste0(route, '.hiv.mortality.rates')]][[race]][[age]]$fraction.change.after.end = fraction.change.after.end
            }
        }
    }

    components = clear.dependent.values(components, c('untreated.hiv.mortality', 'untreated.aids.mortality'))
    components
}

#This comes from 2017 HIV surveillance report, table 1b
# https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2017-vol-29.pdf

DEFAULT.PREVALENCE.BY.AGE = array(c(241,51,1940,7277,6560,5559,4232,4543,4389,3255,1963,1072,860,
                                    184,44,1706,7144,6737,5292,4031,3991,4007,3041,2053,1102,888,
                                    180,32,1754,7486,7282,5513,4316,3872,3671,2946,1978,1001,857,
                                    140,25,1748,7358,7689,5517,4330,3478,3389,3056,1907,1015,882,
                                    130,25,1712,6942,8001,5727,4296,3319,3155,2926,1937,1103,869,
                                    99,25,1723,6416,7755,5678,4365,3032,3006,2729,1918,1108,885),
                                  dim=c(age=13,year=6),
                                  dimnames=list(age=c('<13yo',
                                                      '13-14yo','15-19yo','20-24yo',
                                                      '25-29yo','30-34yo',
                                                      '35-39yo','40-44yo',
                                                      '45-49yo','50-54yo',
                                                      '55-59yo','60-64yo','65+yo'),
                                                year=as.character(2012:2017)))



set.aging.times <- function(components,
                            routes=c('idu','msm','heterosexual'),
                            races=c('black','hispanic','other'),
                            age.indices=1:5,
                            t.pre.spike=NA,
                            t.spike=NA,
                            t0=NA,
                            t1=NA,
                            t2=NA,
                            t3=NA
)
{
    for (route in routes)
    {
        for (race in races)
        {
            for (age.index in age.indices)
            {
                if (!is.na(t.pre.spike))
                    components[[paste0('aging.', route)]][[race]][[paste0('age',age.index)]]$t.pre.spike = t.pre.spike
                if (!is.na(t.spike))
                    components[[paste0('aging.', route)]][[race]][[paste0('age',age.index)]]$t.spike = t.spike
                if (!is.na(t0))
                    components[[paste0('aging.', route)]][[race]][[paste0('age',age.index)]]$t0 = t0
                if (!is.na(t1))
                    components[[paste0('aging.', route)]][[race]][[paste0('age',age.index)]]$t1 = t1
                if (!is.na(t2))
                    components[[paste0('aging.', route)]][[race]][[paste0('age',age.index)]]$t2 = t2
                if (!is.na(t3))
                    components[[paste0('aging.', route)]][[race]][[paste0('age',age.index)]]$t3 = t3
            }
        }
    }

    components = clear.dependent.values(components, 'aging')
    components
}

set.aging.rates <- function(components,
                            routes=c('idu','msm','heterosexual'),
                            races=c('black','hispanic','other'),
                            age.indices=1:5,
                            r.pre.spike=NA,
                            r.spike=NA,
                            r0=NA,
                            r1=NA,
                            r2=NA,
                            r3=NA
)
{
    for (route in routes)
    {
        for (race in races)
        {
            for (age.index in age.indices)
            {
                if (!is.na(r.pre.spike))
                    components[[paste0('aging.', route)]][[race]][[paste0('age',age.index)]]$r.pre.spike = r.pre.spike
                if (!is.na(r.spike))
                    components[[paste0('aging.', route)]][[race]][[paste0('age',age.index)]]$r.spike = r.spike
                if (!is.na(r0))
                    components[[paste0('aging.', route)]][[race]][[paste0('age',age.index)]]$r0 = r0
                if (!is.na(r1))
                    components[[paste0('aging.', route)]][[race]][[paste0('age',age.index)]]$r1 = r1
                if (!is.na(r2))
                    components[[paste0('aging.', route)]][[race]][[paste0('age',age.index)]]$r2 = r2
                if (!is.na(r3))
                    components[[paste0('aging.', route)]][[race]][[paste0('age',age.index)]]$r3 = r3
            }
        }
    }

    components = clear.dependent.values(components, 'aging')
    components
}

setup.aging <- function(components,
                        prevalence.by.age=DEFAULT.PREVALENCE.BY.AGE,
                        proportion.prevalent.13.to.24.who.are.24 = sum(prevalence.by.age['20-24yo',]) / 5 /
                            sum(prevalence.by.age[c('13-14yo','15-19yo','20-24yo'),]),
                        proportion.prevalent.25.to.34.who.are.34 = 0.1,
                        proportion.prevalent.35.to.44.who.are.44 = 0.1,
                        proportion.prevalent.45.to.54.who.are.54 = 0.1
#                        proportion.prevalent.25.to.34.who.are.34 = sum(prevalence.by.age['30-34yo',]) / 5 /
#                            sum(prevalence.by.age[c('25-29yo','30-34yo'),]),
#                        proportion.prevalent.35.to.44.who.are.44 = sum(prevalence.by.age['40-44yo',]) / 5 /
#                            sum(prevalence.by.age[c('35-39yo','40-44yo'),]),
#                        proportion.prevalent.45.to.54.who.are.54 = sum(prevalence.by.age['50-54yo',]) / 5 /
#                            sum(prevalence.by.age[c('45-49yo','50-54yo'),])
)

{
    components = set.aging.rates(components, age.indices=1,
                                 r.pre.spike=proportion.prevalent.13.to.24.who.are.24,
                                 r0=proportion.prevalent.13.to.24.who.are.24,
                                 r1=proportion.prevalent.13.to.24.who.are.24,
                                 r2=proportion.prevalent.13.to.24.who.are.24,
                                 r3=proportion.prevalent.13.to.24.who.are.24)
    components = set.aging.rates(components, age.indices=2,
                                 r.pre.spike=proportion.prevalent.25.to.34.who.are.34,
                                 r0=proportion.prevalent.25.to.34.who.are.34,
                                 r1=proportion.prevalent.25.to.34.who.are.34,
                                 r2=proportion.prevalent.25.to.34.who.are.34,
                                 r3=proportion.prevalent.25.to.34.who.are.34)
    components = set.aging.rates(components, age.indices=3,
                                 r.pre.spike=proportion.prevalent.35.to.44.who.are.44,
                                 r0=proportion.prevalent.35.to.44.who.are.44,
                                 r1=proportion.prevalent.25.to.34.who.are.34,
                                 r2=proportion.prevalent.25.to.34.who.are.34,
                                 r3=proportion.prevalent.35.to.44.who.are.44)
    components = set.aging.rates(components, age.indices=4,
                                 r.pre.spike=proportion.prevalent.45.to.54.who.are.54,
                                 r0=proportion.prevalent.45.to.54.who.are.54,
                                 r1=proportion.prevalent.45.to.54.who.are.54,
                                 r2=proportion.prevalent.45.to.54.who.are.54,
                                 r3=proportion.prevalent.45.to.54.who.are.54)
    components = set.aging.rates(components, age.indices=5,
                                 r.pre.spike=0,
                                 r0=0,
                                 r1=0,
                                 r2=0,
                                 r3=0)
    components
}

# using NSDUH 2015-2018 data, 25% of prevalent ever IDU among 13-24 are 24
#  get.idu.13.24.frac.24()
setup.idu.age1.aging <- function(components,
                                 proportion.active.idu.13.24.who.are.24=0.25,
                                 proportion.prior.idu.13.24.who.are.24=0.25)
{
    components$proportion.active.idu.13.24.who.are.24 = proportion.active.idu.13.24.who.are.24
    components$proportion.prior.idu.13.24.who.are.24 = proportion.prior.idu.13.24.who.are.24

    components = clear.dependent.values(components, 'aging')

    components
}

setup.idu.availability.by.age <- function(components,
                                             age.index,
                                             availability.by.age)
{
    ages.for.index = components$jheem$age$lowers[age.index]:(min(max(as.numeric(dimnames(components$populations$full.age.collapsed.races)[['age']])),
                                                                 components$jheem$age$uppers[age.index]-1))
    availability = sapply(as.character(ages.for.index), function(i){1})
    availability[names(availability.by.age)] = availability.by.age
    components$idu.availability[[paste0('age', age.index)]] = availability

    components = clear.dependent.values(components, 'idu.contact.arrays')

    components
}


##---------------------##
##-- IDU TRANSITIONS --##
##---------------------##

setup.idu.transition.times <- function(components, years, end.year)
{
    components$years.for.idu.transitions = years
    components$end.year.idu.transition = end.year

    components$incident.idu = components$idu.remission = components$idu.relapse = lapply(years, function(year){NA})

    components = clear.dependent.values(components, c('incident.idu','idu.remission','idu.relapse'))
    components
}

set.idu.transitions <- function(components,
                                incident.idu,
                                idu.remission,
                                idu.relapse,
                                indices = 1:length(components$years.for.idu.transitions),
                                overwrite.elements=T)
{
    if (overwrite.elements)
        components$idu.transition.elements = list(incident.idu=incident.idu,
                                               idu.remission=idu.remission,
                                               idu.relapse=idu.relapse)

    for (index in indices)
    {
        components$incident.idu[[index]] = incident.idu
        components$idu.remission[[index]] = idu.remission
        components$idu.relapse[[index]] = idu.relapse
    }

    components = clear.dependent.values(components, c('incident.idu','idu.remission','idu.relapse'))
    components
}

##---------------------##
##-- HIV TRANSITIONS --##
##---------------------##

set.hiv.transitions <- function(components,
                                acute.hiv.duration,
                                prep.screening.frequency)
{
    components$acute.hiv.duration = as.numeric(acute.hiv.duration)
    components$prep.screening.frequency = as.numeric(prep.screening.frequency)

    components = clear.dependent.values(components, c('acute.hiv.duration','prep.screening.frequency'))

    components
}

set.aids.transitions <- function(components,
                                 aids.progression.rate,
                                 cd4.recovery.rate)
{
    components$aids.progression.rate = aids.progression.rate
    components$cd4.recovery.rate = cd4.recovery.rate

    components = clear.dependent.values(components, c('aids.progression.rate','cd4.recovery.rate'))

    components
}

DEFAULT.TESTING.Z.SCORES = c(testing.msm.z=0, testing.idu.z=0, testing.heterosexual.z=0,
                             testing.black.z=0, testing.hispanic.z=0,
                             testing.age1.z=0, testing.age2.z=0, testing.age4.z=0, testing.age5.z=0,
                             testing.female.z=0)

set.background.hiv.testing.slope.time <- function(components,
                                                  anchor.year=2010,
                                                  allow.decreasing.testing.rates=F)
{
    components$background.testing.slope.anchor.year = anchor.year
    components$allow.decreasing.testing.rates = allow.decreasing.testing.rates

    components = clear.dependent.values(components, 'background.testing.rate.ratios')

    components
}

set.background.hiv.testing.rate.slopes <- function(components,
                                                   msm.slope=0,
                                                   heterosexual.slope=0,
                                                   idu.slope=0)
{
    dim.names = list(sex=components$settings$SEXES, risk=components$settings$RISK_STRATA)
    components$background.testing.rate.slopes = array(heterosexual.slope, dim=sapply(dim.names, length), dimnames = dim.names)
    components$background.testing.rate.slopes['msm',] = msm.slope
    components$background.testing.rate.slopes[,'active_IDU'] = idu.slope

    components = clear.dependent.values(components, 'background.testing.rate.ratios')

    components
}

set.background.hiv.testing.rate.ratios <- function(components,
                                                   msm.ratio=1,
                                                   heterosexual.ratio=1,
                                                   idu.ratio=1)
{
    dim.names = list(sex=components$settings$SEXES, risk=components$settings$RISK_STRATA)
    components$background.testing.rate.ratios = array(heterosexual.ratio, dim=sapply(dim.names, length), dimnames = dim.names)
    components$background.testing.rate.ratios['msm',] = msm.ratio
    components$background.testing.rate.ratios[,'active_IDU'] = idu.ratio

    components = clear.dependent.values(components, 'background.testing.rate.ratios')

    components
}

set.testing.ramp.up.vs.current.rr <- function(components,
                                              msm.rr,
                                              heterosexual.rr,
                                              idu.rr)
{
    dim.names = list(sex=components$settings$SEXES, risk=components$settings$RISK_STRATA)
    components$testing.ramp.up.vs.current.rr = array(heterosexual.rr, dim=sapply(dim.names, length), dimnames = dim.names)
    components$testing.ramp.up.vs.current.rr['msm',] = msm.rr
    components$testing.ramp.up.vs.current.rr[,'active_IDU'] = idu.rr

    components = clear.dependent.values(components, 'background.testing.proportions')
    components
}

set.background.hiv.testing.proportions <- function(components,
                                                   data.managers,
                                                   msa,
                                                   max.smoothed.testing.proportion=0.9,
                                                   smoothing.years,
                                                   age1.testing.log.or.intercept=0,
                                                   age1.testing.log.or.slope=0,
                                                   total.proportion.tested.or=1,
                                                   msm.proportion.tested.or=1,
                                                   idu.proportion.tested.or=1,
                                                   total.testing.slope.or=1,
                                                   anchor.year=2012,
                                                   first.testing.year=1980,
                                                   testing.ramp.up.year=1990,
                                                   testing.ramp.up.vs.current.rr=0.5,
                                                   age1.msm.or.intercept=1,
                                                   age2.msm.or.intercept=1,
                                                   age1.msm.or.slope=1,
                                                   age2.msm.or.slope=1)
{
    if (is.null(components$proportions.msm.of.male))
        stop('MSM proportions must be set up in the components prior to pulling testing rates')

    if (is.null(components$active.idu.prevalence) || is.null(components$idu.ever.prevalence))
        stop('IDU proportions must be set up in the components prior to pulling testing rates')

    population = stratify.males.to.msm.by.race(components$populations$collapsed,
                                               components$proportions.msm.of.male)
    population = stratify.population.idu(population,
                                         active.idu.prevalence=components$active.idu.prevalence,
                                         idu.ever.prevalence=components$idu.ever.prevalence)

    components$background.testing.inputs = list(msa=msa,
                                                age1.testing.log.or.intercept=age1.testing.log.or.intercept,
                                                age1.testing.log.or.slope=age1.testing.log.or.slope,
                                                smoothing.years=smoothing.years,
                                                max.smoothed.testing.proportion=max.smoothed.testing.proportion,
                                                total.proportion.tested.or=total.proportion.tested.or,
                                                msm.proportion.tested.or=msm.proportion.tested.or,
                                                idu.proportion.tested.or=idu.proportion.tested.or,
                                                total.testing.slope.or=total.testing.slope.or,
                                                anchor.year=anchor.year,
                                                age1.msm.or.intercept=age1.msm.or.intercept,
                                                age2.msm.or.intercept=age2.msm.or.intercept,
                                                age1.msm.or.slope=age1.msm.or.slope,
                                                age2.msm.or.slope=age2.msm.or.slope)

    components = set.testing.ramp.up.vs.current.rr(components,
                                                   msm.rr = testing.ramp.up.vs.current.rr,
                                                   heterosexual.rr = testing.ramp.up.vs.current.rr,
                                                   idu.rr = testing.ramp.up.vs.current.rr)

    testing.proportions = smooth.proportions.by.strata(data.managers$continuum,
                                                       data.type='testing',
                                                       msa=msa,
                                                       max.proportion=max.smoothed.testing.proportion,
                                                       constrain.to.total = F,
                                                       desired.years=smoothing.years,
                                                       population=population,
                                                       age1.log.or.intercept=age1.testing.log.or.intercept,
                                                       age1.log.or.slope=age1.testing.log.or.slope,
                                                       extra.total.or=total.proportion.tested.or,
                                                       extra.msm.or=msm.proportion.tested.or,
                                                       extra.idu.or=idu.proportion.tested.or,
                                                       extra.total.slope.or=total.testing.slope.or,
                                                       anchor.year=anchor.year,
                                                       age1.msm.log.or.intercept = log(age1.msm.or.intercept),
                                                       age2.msm.log.or.intercept = log(age2.msm.or.intercept),
                                                       age1.msm.log.or.slope = log(age1.msm.or.slope),
                                                       age2.msm.log.or.slope = log(age2.msm.or.slope))

    attr(components, 'smoothed.testing.proportions') = testing.proportions

#    testing.rates = -log(1-testing.proportions)

#    attr(components, 'smoothed.testing.rates') = testing.rates

    if (is.null(components$jheem))
        components = do.setup.jheem.skeleton(components)

    components$background.testing.years = as.numeric(dimnames(testing.proportions)[['year']])
    components$background.testing.proportions = lapply(components$background.testing.years, function(year){
        expand.population.to.general(components$jheem, testing.proportions[as.character(year),,,,])
    })

    #add in the past - the ramp rr will be overwritten later
    components$background.testing.proportions = c(list(0 * components$background.testing.proportions[[1]],
                                                       testing.ramp.up.vs.current.rr * components$background.testing.proportions[[1]]),
                                                  components$background.testing.proportions)
    components$background.testing.years = c(first.testing.year, testing.ramp.up.year,
                                            components$background.testing.years)

    components = clear.dependent.values(components, 'background.testing.proportions')
    components
}

set.testing.ramp.up <- function(components,
                                testing.ramp.up.vs.current.rr)
{
    components$background.testing.inputs$testing.ramp.up.vs.current.rr = testing.ramp.up.vs.current.rr
    components$background.testing.proportions[[2]] = components$background.testing.proportions[[3]] *
        testing.ramp.up.vs.current.rr

    components = clear.dependent.values(components, 'background.testing.proportions')
    components
}

set.foreground.hiv.testing.rates <- function(components,
                                             testing.rates,
                                             years)
{
    if (is.null(testing.rates))
        components$foreground.testing.years = components$foreground.testing.rates = NULL
    else
    {
        if (class(testing.rates)!='list')
            testing.rates = list(testing.rates)

        components$foreground.testing.rates = lapply(testing.rates, function(rate){
            expand.population.to.general(components$jheem, rate)
        })
        names(components$foreground.testing.rates) = as.character(years)

        components$foreground.testing.years = years
    }

    components = clear.dependent.values(components, 'foreground.testing.proportions')
    components
}

##-----------------------------##
##-- TRANSMISSION PARAMETERS --##
##-----------------------------##

setup.trate.years <- function(components,
                              routes=c('idu','msm','heterosexual'),
                              races=c('black','hispanic','other'),
                              age.indices=1:5,
                              t.pre.peak=NA,
                              t.peak.start=NA,
                              t.peak.end=NA,
                              t0.start=NA,
                              t0.end=NA,
                              t1=NA,
                              t0.5=NA,
                              t2=NA,
                              t.end=NA)
{
    for (route in routes)
    {
        for (race in races)
        {
            for (age in paste0('age', age.indices))
            {
                if (!is.na(t.pre.peak))
                    components[[paste0(route, '.trates')]][[race]][[age]]$t.pre.peak = t.pre.peak
                if (!is.na(t.peak.start))
                    components[[paste0(route, '.trates')]][[race]][[age]]$t.peak.start = t.peak.start
                if (!is.na(t.peak.end))
                    components[[paste0(route, '.trates')]][[race]][[age]]$t.peak.end = t.peak.end
                if (!is.na(t0.start))
                    components[[paste0(route, '.trates')]][[race]][[age]]$t0.start = t0.start
                if (!is.na(t0.end))
                    components[[paste0(route, '.trates')]][[race]][[age]]$t0.end = t0.end
                if (!is.na(t1))
                    components[[paste0(route, '.trates')]][[race]][[age]]$t1 = t1
                if (!is.na(t0.5))
                    components[[paste0(route, '.trates')]][[race]][[age]]$t0.5 = t0.5
                if (!is.na(t2))
                    components[[paste0(route, '.trates')]][[race]][[age]]$t2 = t2
                if (!is.na(t.end))
                    components[[paste0(route, '.trates')]][[race]][[age]]$t.end = t.end
            }
        }
    }
    components = clear.dependent.values(components,
                                        c('idu.contact.arrays','sexual.contact.arrays')[c(any(routes=='idu'), any(routes=='msm' | routes=='heterosexual'))])
    components
}

setup.trates <- function(components,
                         routes=c('idu','msm','heterosexual'),
                         races=c('black','hispanic','other'),
                         age.indices = 1:5,
                         r.peak=NA,
                         r0=NA,
                         r1=NA,
                         r0.5=NA,
                         r2=NA,
                         fraction.change.after.end=NA)
{
    for (route in routes)
    {
        for (race in races)
        {
            for (age in paste0('age', age.indices))
            {
                if (!is.na(r.peak))
                    components[[paste0(route, '.trates')]][[race]][[age]]$r.peak = r.peak
                if (!is.na(r0))
                    components[[paste0(route, '.trates')]][[race]][[age]]$r0 = r0
                if (!is.na(r1))
                    components[[paste0(route, '.trates')]][[race]][[age]]$r1 = r1
                if (!is.na(r0.5))
                    components[[paste0(route, '.trates')]][[race]][[age]]$r0.5 = r0.5
                if (!is.na(r2))
                    components[[paste0(route, '.trates')]][[race]][[age]]$r2 = r2
                if (!is.na(fraction.change.after.end))
                    components[[paste0(route, '.trates')]][[race]][[age]]$fraction.change.after.end = fraction.change.after.end
            }
        }
    }

    components = clear.dependent.values(components,
                                        c('idu.contact.arrays','sexual.contact.arrays')[c(any(routes=='idu'), any(routes=='msm' | routes=='heterosexual'))])
    components
}


setup.susceptibility <- function(components,
                                 prep.rr.heterosexual,
                                 prep.rr.msm,
                                 prep.rr.idu,
                                 age1.sexual.susceptibility.rr=1,
                                 age2.sexual.susceptibility.rr=1,
                                 age3.sexual.susceptibility.rr=1,
                                 age4.sexual.susceptibility.rr=1,
                                 age5.sexual.susceptibility.rr=1,
                                 msm.age1.sexual.susceptibility.rr=1,
                                 msm.age2.sexual.susceptibility.rr=1,
                                 msm.age3.sexual.susceptibility.rr=1,
                                 msm.age4.sexual.susceptibility.rr=1,
                                 msm.age5.sexual.susceptibility.rr=1)
{
    components = setup.prep.susceptibility(components, prep.rr.heterosexual=prep.rr.heterosexual, prep.rr.msm=prep.rr.msm, prep.rr.idu=prep.rr.idu)

    components = setup.sexual.susceptibility(components,
                                             age1.sexual.susceptibility.rr=age1.sexual.susceptibility.rr,
                                             age2.sexual.susceptibility.rr=age2.sexual.susceptibility.rr,
                                             age3.sexual.susceptibility.rr=age3.sexual.susceptibility.rr,
                                             age4.sexual.susceptibility.rr=age4.sexual.susceptibility.rr,
                                             age5.sexual.susceptibility.rr=age5.sexual.susceptibility.rr,
                                             msm.age1.sexual.susceptibility.rr=msm.age1.sexual.susceptibility.rr,
                                             msm.age2.sexual.susceptibility.rr=msm.age2.sexual.susceptibility.rr,
                                             msm.age3.sexual.susceptibility.rr=msm.age3.sexual.susceptibility.rr,
                                             msm.age4.sexual.susceptibility.rr=msm.age4.sexual.susceptibility.rr,
                                             msm.age5.sexual.susceptibility.rr=msm.age5.sexual.susceptibility.rr)

    components
}

setup.global.trates <- function(components,
                                global.sexual.trate,
                                global.idu.trate)
{
    components$global.sexual.trate = global.sexual.trate
    components$global.idu.trate = global.idu.trate

    components = clear.dependent.values(components, c('global.sexual.transmission.rates','global.idu.transmission.rates'))

    components
}

setup.prep.susceptibility <- function(components,
                                      prep.rr.heterosexual,
                                      prep.rr.msm,
                                      prep.rr.idu)
{
    components$prep.rr.heterosexual = as.numeric(prep.rr.heterosexual)
    components$prep.rr.msm = as.numeric(prep.rr.msm)
    components$prep.rr.idu = as.numeric(prep.rr.idu)

    components = clear.dependent.values(components, c('prep.rr.heterosexual','prep.rr.msm','prep.rr.idu'))
    components
}

setup.sexual.susceptibility <- function(components,
                                        age1.sexual.susceptibility.rr=1,
                                        age2.sexual.susceptibility.rr=1,
                                        age3.sexual.susceptibility.rr=1,
                                        age4.sexual.susceptibility.rr=1,
                                        age5.sexual.susceptibility.rr=1,
                                        msm.age1.sexual.susceptibility.rr=1,
                                        msm.age2.sexual.susceptibility.rr=1,
                                        msm.age3.sexual.susceptibility.rr=1,
                                        msm.age4.sexual.susceptibility.rr=1,
                                        msm.age5.sexual.susceptibility.rr=1)
{

    components$sexual.susceptibility.rr.by.age = c(as.numeric(age1.sexual.susceptibility.rr),
                                                   as.numeric(age2.sexual.susceptibility.rr),
                                                   as.numeric(age3.sexual.susceptibility.rr),
                                                   as.numeric(age4.sexual.susceptibility.rr),
                                                   as.numeric(age5.sexual.susceptibility.rr))

    names(components$sexual.susceptibility.rr.by.age) = components$settings$AGES$labels

    components$msm.sexual.susceptibility.rr.by.age = c(as.numeric(msm.age1.sexual.susceptibility.rr),
                                                       as.numeric(msm.age2.sexual.susceptibility.rr),
                                                       as.numeric(msm.age3.sexual.susceptibility.rr),
                                                       as.numeric(msm.age4.sexual.susceptibility.rr),
                                                       as.numeric(msm.age5.sexual.susceptibility.rr))

    names(components$msm.sexual.susceptibility.rr.by.age) = components$settings$AGES$labels


    components = clear.dependent.values(components, c('susceptibility'))
    components
}


setup.transmissibility <- function(components,
                                   acute.transmissibility.rr=NA,
                                   msm.acute.transmissibility.rr=acute.transmissibility.rr,
                                   heterosexual.acute.transmissibility.rr=acute.transmissibility.rr,
                                   idu.acute.transmissibility.rr=acute.transmissibility.rr,
                                   diagnosed.needle.sharing.rr,
                                   diagnosed.het.male.condomless.rr,
                                   diagnosed.female.condomless.rr,
                                   diagnosed.msm.condomless.rr,
                                   black.sexual.transmissibility.rr=1,
                                   hispanic.sexual.transmissibility.rr=1,
                                   black.idu.transmissibility.rr=1,
                                   hispanic.idu.transmissibility.rr=1)
{
    dim.names = list(sex=components$settings$SEXES, risk=components$settings$RISK_STRATA)
    components$acute.transmissibility.rr = array(heterosexual.acute.transmissibility.rr, dim=sapply(dim.names, length), dimnames = dim.names)
    components$acute.transmissibility.rr['msm',] = msm.acute.transmissibility.rr
    components$acute.transmissibility.rr[,'active_IDU'] = idu.acute.transmissibility.rr

    components$diagnosed.needle.sharing.rr = as.numeric(diagnosed.needle.sharing.rr)

    components$diagnosed.het.male.condomless.rr = as.numeric(diagnosed.het.male.condomless.rr)
    components$diagnosed.female.condomless.rr = as.numeric(diagnosed.female.condomless.rr)
    components$diagnosed.msm.condomless.rr = as.numeric(diagnosed.msm.condomless.rr)

    components$idu.transmissibility.rr.by.race = c(black=as.numeric(black.idu.transmissibility.rr), hispanic=as.numeric(hispanic.idu.transmissibility.rr))
    components$sexual.transmissibility.rr.by.race = c(black=as.numeric(black.sexual.transmissibility.rr), hispanic=as.numeric(hispanic.sexual.transmissibility.rr))

    components = clear.dependent.values(components, c('acute.transmissibility.rr','diagnosed.needle.sharing.rr',
                                                      'diagnosed.het.male.condomless.rr','diagnosed.female.condomless.rr','diagnosed.msm.condomless.rr',
                                                      'idu.transmissibility.rr.by.race','sexual.transmissibility.rr.by.race'))
    components
}

setup.heterosexual.transmission <- function(components,
                                            male.to.female.sexual.transmission.ratio,
                                            female.to.male.sexual.transmission.ratio)
{
    components$male.to.female.sexual.transmission.ratio = male.to.female.sexual.transmission.ratio
    components$female.to.male.sexual.transmission.ratio = female.to.male.sexual.transmission.ratio

    components = clear.dependent.values(components, c('male.to.female.sexual.transmission',
                                                      'female.to.male.sexual.transmission'))
    components
}

setup.sex.by.age <- function(components,
                             heterosexual.male.age.model,
                             female.age.model,
                             msm.age.model)
{
    components$sexual.transmission$heterosexual.male.age.model = heterosexual.male.age.model
    components$sexual.transmission$female.age.model = female.age.model
    components$sexual.transmission$msm.age.model = msm.age.model

    components = clear.dependent.values(components, 'sexual.transmission')
    components
}

setup.sexual.availability.by.age <- function(components,
                                             age.index,
                                             availability.by.age)
{
    ages.for.index = components$jheem$age$lowers[age.index]:(min(max(as.numeric(dimnames(components$populations$full.age.collapsed.races)[['age']])),
                                                                 components$jheem$age$uppers[age.index]-1))
    availability = sapply(as.character(ages.for.index), function(i){1})
    availability[names(availability.by.age)] = availability.by.age
    components$sexual.availability[[paste0('age', age.index)]] = availability

    components = clear.dependent.values(components, 'sexual.transmission')

    components
}

setup.idu.by.age <- function(components,
                             age.model)
{
    components$idu.transmission$age.model = age.model
    components = clear.dependent.values(components, 'idu.transmission')
    components
}

setup.sex.by.race <- function(components,
                              black.black.oe,
                              hispanic.hispanic.oe,
                              other.other.oe)
{
    components$sexual.transmission$black.black.oe = black.black.oe
    components$sexual.transmission$hispanic.hispanic.oe = hispanic.hispanic.oe
    components$sexual.transmission$other.other.oe = other.other.oe

    components = clear.dependent.values(components, 'sexual.transmission')
    components
}

setup.idu.by.race <- function(components,
                              black.black.oe,
                              hispanic.hispanic.oe,
                              other.other.oe)
{
    components$idu.transmission$black.black.oe = black.black.oe
    components$idu.transmission$hispanic.hispanic.oe = hispanic.hispanic.oe
    components$idu.transmission$other.other.oe = other.other.oe

    components = clear.dependent.values(components, 'idu.transmission')
    components
}

setup.sex.by.sex <- function(components,
                             fraction.msm.pairings.with.female,
                             oe.female.pairings.with.msm)
{
    components$sexual.transmission$fraction.msm.pairings.with.female = fraction.msm.pairings.with.female
    components$sexual.transmission$oe.female.pairings.with.msm = oe.female.pairings.with.msm

    components = clear.dependent.values(components, 'sexual.transmission')
    components
}

setup.sex.by.idu <- function(components,
                             never.idu.sexual.oe=1,
                             never.with.idu.sexual.oe=1,
                             idu.sexual.oe=1)
{
    components$never.idu.sexual.oe = never.idu.sexual.oe
    components$never.with.idu.sexual.oe = never.with.idu.sexual.oe
    components$idu.sexual.oe = idu.sexual.oe

    components = clear.dependent.values(components, 'sexual.transmission')
    components
}

setup.idu.by.sex <- function(components,
                             sex.oes)
{
    components$idu.transmission$sex.oes = sex.oes

    components = clear.dependent.values(components, 'idu.transmission')
    components
}


##-- SUPPRESSION --##

DEFAULT.SUPPRESSION.Z.SCORES = c(suppressed.total.z=0, suppressed.black.z=0, suppressed.hispanic.z=0,
                                 suppressed.age1.z=0, suppressed.age2.z=0, suppressed.age4.z=0, suppressed.age5.z=0,
                                 suppressed.female.z=0, suppressed.msm.z=0, suppressed.idu.z=0)

set.background.suppression.ors <- function(components,
                                           msm.or=1,
                                           heterosexual.or=1,
                                           idu.or=1)
{
    dim.names = list(sex=components$settings$SEXES, risk=components$settings$RISK_STRATA)
    components$background.suppression.ors = array(heterosexual.or, dim=sapply(dim.names, length), dimnames = dim.names)
    components$background.suppression.ors['msm',] = msm.or
    components$background.suppression.ors[,'active_IDU'] = idu.or

    components = clear.dependent.values(components, 'background.suppression')
    components
}

set.background.suppression <- function(components,
                                       data.managers,
                                       msa,
                                       max.smoothed.suppressed.proportion=0.9,
                                       smoothing.years,
                                       zero.suppression.year=1996,
                                       initial.suppression.ramp.up.years=NA,#5,
                                       total.suppressed.or=1,
                                       total.future.suppressed.slope.or=1,
                                       anchor.year=2020,
                                       age1.msm.or.intercept=1,
                                       age2.msm.or.intercept=1,
                                       age1.msm.or.slope=1,
                                       age2.msm.or.slope=1)
{
    if (is.null(components$proportions.msm.of.male))
        stop('MSM proportions must be set up in the components prior to pulling suppression rates')

    if (is.null(components$active.idu.prevalence) || is.null(components$idu.ever.prevalence))
        stop('IDU proportions must be set up in the components prior to pulling suppression rates')

    population = stratify.males.to.msm.by.race(components$populations$collapsed,
                                               components$proportions.msm.of.male)
    population = stratify.population.idu(population,
                                         active.idu.prevalence=components$active.idu.prevalence,
                                         idu.ever.prevalence=components$idu.ever.prevalence)

    components$background.suppression.inputs = list(msa=msa,
                                                    max.smoothed.suppressed.proportion=max.smoothed.suppressed.proportion,
                                                    smoothing.years=smoothing.years,
                                                    zero.suppression.year=zero.suppression.year,
                                                    initial.suppression.ramp.up.years=initial.suppression.ramp.up.years,
                                                    total.suppressed.or=total.suppressed.or,
                                                    total.future.suppressed.slope.or=total.future.suppressed.slope.or,
                                                    anchor.year=anchor.year,
                                                    age1.msm.or.intercept=age1.msm.or.intercept,
                                                    age2.msm.or.intercept=age2.msm.or.intercept,
                                                    age1.msm.or.slope=age1.msm.or.slope,
                                                    age2.msm.or.slope=age2.msm.or.slope)

    suppressed.proportions = smooth.proportions.by.strata(data.managers$continuum,
                                                          data.type='suppression',
                                                          msa=msa,
                                                          max.proportion=max.smoothed.suppressed.proportion,
                                                          constrain.to.total = T,
                                                          desired.years=smoothing.years,
                                                          population=population,
                                                          anchor.year=anchor.year,
                                                          extra.total.or=total.suppressed.or,
                                                          extra.total.slope.after.anchor.or=total.future.suppressed.slope.or,
                                                          age1.msm.log.or.intercept = log(age1.msm.or.intercept),
                                                          age2.msm.log.or.intercept = log(age2.msm.or.intercept),
                                                          age1.msm.log.or.slope = log(age1.msm.or.slope),
                                                          age2.msm.log.or.slope = log(age2.msm.or.slope))

    attr(components, 'smoothed.suppressed.proportions') = suppressed.proportions

    components$background.suppression.years = as.numeric(dimnames(suppressed.proportions)[['year']])
    components$background.suppression = lapply(components$background.suppression.years, function(year){
        #        access(suppressed.proportions, year=as.character(year))
        expand.population.to.hiv.positive(components$jheem, suppressed.proportions[as.character(year),,,,])
    })

    if (!is.na(zero.suppression.year))
    {
        if (!is.na(initial.suppression.ramp.up.years))
        {
            if ((zero.suppression.year+initial.suppression.ramp.up.years)>components$background.suppression.years[1])
                stop(paste0("A zero-suppression year of ", zero.suppression.year,
                            " and ", initial.suppression.ramp.up.years, " years to ramp up initial suppression,",
                            " puts the initial ramped up year at ", zero.suppression.year + initial.suppression.ramp.up.years,
                            " which is AFTER the first year for which there is suppression data, ", components$background.suppression.years[1]))
            components$background.suppression.years = c(zero.suppression.year+initial.suppression.ramp.up.years, components$background.suppression.years)
            components$background.suppression = c(components$background.suppression[1], components$background.suppression)
        }

        components$background.suppression.years = c(zero.suppression.year, components$background.suppression.years)
        components$background.suppression = c(list(expand.population.to.hiv.positive(components$jheem, 0)),
                                              components$background.suppression)
    }

    components = clear.dependent.values(components, 'background.suppression')
    components
}
set.foreground.suppression <- function(components,
                                       suppressed.proportions,
                                       years)
{
    if (is.null(suppressed.proportions))
        components$foreground.suppression.years = components$foreground.suppresion = NULL
    else
    {
        if (class(suppressed.proportions)!='list')
            suppressed.proportions = list(suppressed.proportions)

        components$foreground.suppression = suppressed.proportions
        names(components$foreground.suppression) = as.character(years)

        components$foreground.suppression.years = years
    }

    components = clear.dependent.values(components, 'foreground.suppression')
    components
}

set.background.change.to.years <- function(components,
                                           testing.change.to.year,
                                           suppression.change.to.year,
                                           prep.change.to.year)
{
    components$background.change.to.years = list(testing=testing.change.to.year,
                                                 suppression=suppression.change.to.year,
                                                 prep=prep.change.to.year)

    components = clear.dependent.values(components, c('background.prep.coverage',
                                                      'background.testing.proportions',
                                                      'background.suppression'))

    components
}

##-- PREP --##

set.background.prep.coverage <- function(components,
                                       data.managers,
                                       prep.persistence,
                                       prep.coverage=NULL,
                                       smooth=T,
                                       max.smoothed.prep.coverage=0.8,
                                       smoothing.years)
{
    components$prep.persistence = prep.persistence

    components$background.prep.inputs = list(prep.coverage=prep.coverage,
                                             smooth=smooth,
                                             max.smoothed.prep.coverage=max.smoothed.prep.coverage,
                                             smoothing.years=smoothing.years)

    if (is.null(prep.coverage))
    {
        if (is.null(components$proportions.msm.of.male))
            stop('MSM proportions must be set up in the components prior to pulling PrEP rates')

        county.populations = stratify.males.to.msm.by.race(components$populations$by.county.collapsed.races,
                                                   components$proportions.msm.of.male)

        prep.years = sort(data.managers$prep$years)
        no.prep.year = min(prep.years)

        for (year in prep.years)
        {
            prep.rates = get.prep.rates(data.managers$prep,
                                        counties=components$fips, years=year,
                                        ages=components$settings$AGES$labels,
                                        races=components$settings$RACES,
                                        sexes=components$settings$SEXES)[1,,,,]


            prep.numerators = prep.rates * county.populations

            missing.counties = apply(is.na(prep.numerators), 'county', any)
            if (all(missing.counties))
                stop(paste0("No PrEP rates available for any county for year ", year))

            prep.numerators = prep.numerators[!missing.counties,,,]
            prep.denominators = county.populations[!missing.counties,,,]

            if (sum(!missing.counties)>1)
            {
                prep.numerators = colSums(prep.numerators)
                prep.denominators = colSums(county.populations)
            }

            one.prep.coverage = prep.numerators / prep.denominators * components$prep.persistence

            if (is.null(prep.coverage))
            {
                dim.names = c(list(year=as.character(c(no.prep.year, prep.years+1))), dimnames(one.prep.coverage))
                prep.coverage = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
            }

            prep.coverage[as.character(year+1),,,] = one.prep.coverage
        }
    }
    else
    {
        no.prep.year = min(as.numeric(dimnames(prep.coverage)[['year']]))-1
    }

    if (smooth)
    {
        smoothing.years = unique(sort(c(no.prep.year, smoothing.years)))
        prep.coverage = smooth.proportions(prep.coverage,
                                           max.proportion = max.smoothed.prep.coverage,
                                           desired.years = smoothing.years,
                                           min.slope=0,
                                           model.on.log.odds = F)

        for (year in smoothing.years[smoothing.years < no.prep.year])
            prep.coverage[as.characte(year),,,] = 0
#            access(prep.coverage, year=as.character(year)) = 0
    }

    components$background.prep.years = as.numeric(dimnames(prep.coverage)[['year']])
    components$background.prep.coverage = lapply(components$background.prep.years, function(year){
#        access(prep.coverage, year=as.character(year))
        expand.population.to.general(components$jheem, prep.coverage[as.character(year),,,])
    })

    components = clear.dependent.values(components, 'background.prep.coverage')
    components
}

set.foreground.prep.coverage <- function(components,
                                         prep.coverage,
                                         years)
{
    if (is.null(prep.coverage))
        components$foreground.prep.years = components$foreground.prep.coverage = NULL
    else
    {
        if (class(prep.coverage)!='list')
            prep.coverage = list(prep.coverage)

        components$foreground.prep.coverage = prep.coverage
        names(components$foreground.prep.coverage) = as.character(years)

        components$foreground.prep.years = years
    }

    components = clear.dependent.values(components, 'foreground.prep.coverage')
    components
}

##--------------------------------------##
##-- COMPARE TWO DIFFERENT COMPONENTS --##
##--------------------------------------##


components.equal <- function(c1, c2, print.details=T, print.prefix='')
{
    are.equal = T

    names.c1 = names(c1)
    names.c2 = names(c2)

    if (length(setdiff(names.c1, names.c2))>0)
    {
        if (print.details)
            print(paste0(print.prefix, "In c1 but not in c2: ",
                         paste0("'", setdiff(names.c1, names.c2), "'", collapse=', ')))
        are.equal=F
    }

    if (length(setdiff(names.c2, names.c1))>0)
    {
        if (print.details)
            print(paste0(print.prefix, "In c2 but not in c1: ",
                         paste0("'", setdiff(names.c2, names.c1), "'", collapse=', ')))
        are.equal=F
    }

    names.both = intersect(names.c1, names.c2)
    equal.mask = sapply(names.both, function(name){
        sub.equal = components.equal(c1[[name]], c2[[name]], print.details=F)

        if (!sub.equal && print.details)
        {
            print(paste0(print.prefix, "Not equal on ", name, ":"))
            components.equal(c1[[name]], c2[[name]], print.details=T, print.prefix=paste0('-', print.prefix))
        }

        sub.equal
    })

    are.equal && all(equal.mask)
}

