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
    
    components = setup.global.trates(components, global.sexual.trate = 1, global.idu.trate = 1)
    
    components = do.setup.jheem.skeleton(components)
    
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
                                  track.hiv.specific.mortality=F,
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
    
    if (any(components$proportions.msm.of.male>0.25))
        warning("Some MSM proportions have been set to be greater than 25% - is this intended?")
    
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
                            routes=c('idu','msm','heterosexual.male','heterosexual.female'),
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
                            routes=c('idu','msm','heterosexual.male','heterosexual.female'),
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

setup.idu.transition.times <- function(components, 
                                       years,
                                       end.year,
                                       fraction.change.after.last.year=0.05)
{
    if (all(!is.na(years)))
        components$years.for.idu.transitions = years
    
    if (!is.na(end.year))
        components$end.year.idu.transition = end.year
    
    #    components$incident.idu = components$idu.remission = components$idu.relapse = lapply(years, function(year){NA})
    
    if (!is.na(fraction.change.after.last.year))
        components$fraction.idu.transitions.change.after.last.year = fraction.change.after.last.year
    
    components = clear.dependent.values(components, c('incident.idu','idu.remission','idu.relapse'))
    components
}

set.idu.transitions <- function(components,
                                incident.idu,
                                idu.remission,
                                idu.relapse,
                                indices = 1:length(components$years.for.idu.transitions),
                                overwrite.elements=F)
{
    if (overwrite.elements)
        components$idu.transition.elements = list(incident.idu=incident.idu,
                                                  idu.remission=idu.remission,
                                                  idu.relapse=idu.relapse)
    
    for (index in indices)
    {
        components$incident.idu[[index]] = expand.population.to.general(components$jheem, incident.idu)[,,,,'active_IDU']
        components$idu.remission[[index]] = expand.population.to.general(components$jheem, idu.remission)[,,,,'active_IDU']
        components$idu.relapse[[index]] = expand.population.to.general(components$jheem, idu.relapse)[,,,,'active_IDU']
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

set.background.hiv.testing.ors <- function(components,
                                           msm.or.intercept=NA,
                                           heterosexual.or.intercept=NA,
                                           idu.or.intercept=NA,
                                           msm.idu.or.intercept=NA,
                                           black.or.intercept=NA,
                                           hispanic.or.intercept=NA,
                                           other.or.intercept=NA,
                                           age1.or.intercept=NA,
                                           age2.or.intercept=NA,
                                           age3.or.intercept=NA,
                                           age4.or.intercept=NA,
                                           age5.or.intercept=NA,
                                           
                                           total.or.slope=NA,
                                           
                                           msm.or.slope=NA,
                                           heterosexual.or.slope=NA,
                                           idu.or.slope=NA,
                                           msm.idu.or.slope=NA,
                                           black.or.slope=NA,
                                           hispanic.or.slope=NA,
                                           other.or.slope=NA,
                                           age1.or.slope=NA,
                                           age2.or.slope=NA,
                                           age3.or.slope=NA,
                                           age4.or.slope=NA,
                                           age5.or.slope=NA)
{
    if (is.null(components$background.testing))
        components$background.testing = list()
    
    if (is.null(components$background.testing$additional.intercept.ors))
        components$background.testing$additional.intercept.ors = numeric()
    
    if (is.null(components$background.testing$additional.slope.ors))
        components$background.testing$additional.slope.ors = numeric()
    
    if (!is.na(msm.or.intercept))
        components$background.testing$additional.intercept.ors['msm'] = msm.or.intercept
    if (!is.na(heterosexual.or.intercept))
        components$background.testing$additional.intercept.ors['heterosexual'] = heterosexual.or.intercept
    if (!is.na(idu.or.intercept))
        components$background.testing$additional.intercept.ors['idu'] = idu.or.intercept
    if (!is.na(msm.idu.or.intercept))
        components$background.testing$additional.intercept.ors['msm_idu'] = msm.idu.or.intercept
    
    if (!is.na(black.or.intercept))
        components$background.testing$additional.intercept.ors['black'] = black.or.intercept
    if (!is.na(hispanic.or.intercept))
        components$background.testing$additional.intercept.ors['hispanic'] = hispanic.or.intercept
    if (!is.na(other.or.intercept))
        components$background.testing$additional.intercept.ors['other'] = other.or.intercept
    
    if (!is.na(age1.or.intercept))
        components$background.testing$additional.intercept.ors['age1'] = age1.or.intercept
    if (!is.na(age2.or.intercept))
        components$background.testing$additional.intercept.ors['age2'] = age2.or.intercept
    if (!is.na(age3.or.intercept))
        components$background.testing$additional.intercept.ors['age3'] = age3.or.intercept
    if (!is.na(age4.or.intercept))
        components$background.testing$additional.intercept.ors['age4'] = age4.or.intercept
    if (!is.na(age5.or.intercept))
        components$background.testing$additional.intercept.ors['age5'] = age5.or.intercept
    
    
    if (!is.na(total.or.slope))
        components$background.testing$additional.slope.ors['all'] = total.or.slope
    
    if (!is.na(msm.or.slope))
        components$background.testing$additional.slope.ors['msm'] = msm.or.slope
    if (!is.na(heterosexual.or.slope))
        components$background.testing$additional.slope.ors['heterosexual'] = heterosexual.or.slope
    if (!is.na(idu.or.slope))
        components$background.testing$additional.slope.ors['idu'] = idu.or.slope
    if (!is.na(msm.idu.or.slope))
        components$background.testing$additional.slope.ors['msm_idu'] = msm.idu.or.slope
    
    if (!is.na(black.or.slope))
        components$background.testing$additional.slope.ors['black'] = black.or.slope
    if (!is.na(hispanic.or.slope))
        components$background.testing$additional.slope.ors['hispanic'] = hispanic.or.slope
    if (!is.na(other.or.slope))
        components$background.testing$additional.slope.ors['other'] = other.or.slope
    
    if (!is.na(age1.or.slope))
        components$background.testing$additional.slope.ors['age1'] = age1.or.slope
    if (!is.na(age2.or.slope))
        components$background.testing$additional.slope.ors['age2'] = age2.or.slope
    if (!is.na(age3.or.slope))
        components$background.testing$additional.slope.ors['age3'] = age3.or.slope
    if (!is.na(age4.or.slope))
        components$background.testing$additional.slope.ors['age4'] = age4.or.slope
    if (!is.na(age5.or.slope))
        components$background.testing$additional.slope.ors['age5'] = age5.or.slope
    
    components = clear.dependent.values(components, 'background.testing')
    components
}

set.background.hiv.testing.ramp.up <- function(components,
                                               testing.ramp.up.vs.current.rr=0.5,
                                               testing.ramp.up.yearly.increase=2)
{
    if (is.null(components$background.testing))
        components$background.testing = list()
    
    if (!is.na(testing.ramp.up.vs.current.rr))
        components$background.testing$ramp.up.vs.current.rr = testing.ramp.up.vs.current.rr
    
    if (!is.na(testing.ramp.up.yearly.increase))
        components$background.testing$ramp.up.yearly.increase = testing.ramp.up.yearly.increase
    
    components = clear.dependent.values(components, 'background.testing')
    components
}

setup.background.hiv.testing <- function(components,
                                         continuum.manager,
                                         location,
                                         years,
                                         first.testing.year=1981,
                                         testing.ramp.up.year=1993)
{
    if (is.null(components$background.testing))
        components$background.testing = list()
    
    if (is.null(components$background.testing))
        components$background.testing = list()
    
    components$background.testing$years = years
    components$background.testing$first.testing.year = first.testing.year
    components$background.testing$ramp.up.year = testing.ramp.up.year
    
    
    if (is.null(components$proportions.msm.of.male))
        stop('MSM proportions must be set up in the components prior to pulling testing rates')
    
    if (is.null(components$active.idu.prevalence) || is.null(components$idu.ever.prevalence))
        stop('IDU proportions must be set up in the components prior to pulling testing rates')
    
    population = stratify.males.to.msm.by.race(components$populations$collapsed,
                                               components$proportions.msm.of.male)
    population = stratify.population.idu(population,
                                         active.idu.prevalence=components$active.idu.prevalence,
                                         idu.ever.prevalence=components$idu.ever.prevalence)
    components$background.testing$model = get.testing.model(continuum.manager, 
                                                            location=location,
                                                            population=population)
    components$background.testing$model$mixed.linear=F
    
    components = clear.dependent.values(components, 'background.testing')
    components
}

set.foreground.rates <- function(components,
                                 type=c('testing','suppression','prep')[1],
                                 rates,
                                 years,
                                 start.years,
                                 end.years,
                                 apply.functions,
                                 foreground.min,
                                 foreground.max,
                                 allow.foreground.less=F,
                                 overwrite.previous=T)
{
    type.name = paste0('foreground.', type)
    if (is.null(rates))
    {
        if (overwrite.previous)
            components[[type.name]] = NULL
    }
    else
    {
        if (is.null(components[[type.name]]) || overwrite.previous)
        {
            components[[type.name]] = list()
            
            components[[type.name]]$rates = list()
            components[[type.name]]$years = list()
            components[[type.name]]$start.years = list()
            components[[type.name]]$end.years = list()
            components[[type.name]]$apply.functions = list()
            components[[type.name]]$allow.foreground.less = list()
            components[[type.name]]$foreground.min = list()
            components[[type.name]]$foreground.max = list()
        }
        
        if (class(rates)!='list')
            rates = list(rates)
        names(rates) = as.character(years)
        
        # Add the new settings
        components[[type.name]]$rates = c(components[[type.name]]$rates, list(rates))
        
        components[[type.name]]$years = c(components[[type.name]]$years, list(years))
        components[[type.name]]$start.years = c(components[[type.name]]$start.years, list(start.years))
        components[[type.name]]$end.years = c(components[[type.name]]$end.years, list(end.years))
        components[[type.name]]$apply.functions = c(components[[type.name]]$apply.functions, list(apply.functions))
        components[[type.name]]$allow.foreground.less = c(components[[type.name]]$allow.foreground.less, list(allow.foreground.less))
        components[[type.name]]$foreground.min = c(components[[type.name]]$foreground.min, list(foreground.min))
        components[[type.name]]$foreground.max = c(components[[type.name]]$foreground.max, list(foreground.max))
        
        # Clear dependencies
        components = clear.dependent.values(components, type.name)
    }
    components
}

##-----------------------------##
##-- TRANSMISSION PARAMETERS --##
##-----------------------------##

setup.trate.years <- function(components,
                              routes=c('idu.msm','idu.male','idu.female','msm','msm.idu','heterosexual,male','heterosexual.female'),
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
#                         routes=c('idu','msm','msm.idu','heterosexual.male','heterosexual.female'),
                         routes=c('idu.msm','idu.male','idu.female','msm','msm.idu','heterosexual.male','heterosexual.female'),
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
                                      prep.rr.idu,
                                      prep.persistence)
{
    components$prep.rr.heterosexual = as.numeric(prep.rr.heterosexual)
    components$prep.rr.msm = as.numeric(prep.rr.msm)
    components$prep.rr.idu = as.numeric(prep.rr.idu)
    components$prep.persistence = as.numeric(prep.persistence)
    
    components = clear.dependent.values(components, c('prep.rr.heterosexual','prep.rr.msm','prep.rr.idu'))
    components
}

setup.needle.exchange.remission.effect <- function(components,
                                                   needle.exchange.remission.rate.ratio)
{
    components$needle.exchange.remission.rate.ratio = needle.exchange.remission.rate.ratio
    
    components = clear.dependent.values(components, 'needle.exchange.remission.rate.ratio')
    components
}

setup.needle.exchange.susceptibility <- function(components,
                                                 needle.exchange.rr)
{
    components$needle.exchange.rr = needle.exchange.rr
    
    components = clear.dependent.values(components, 'needle.exchange.rr')
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

setup.sex.by.age <- function(components,
                             heterosexual.male.age.model,
                             female.age.model,
                             msm.age.model,
                             overwrite.base.models=F)
{
    components$sexual.transmission$heterosexual.male.age.model = heterosexual.male.age.model
    components$sexual.transmission$female.age.model = female.age.model
    components$sexual.transmission$msm.age.model = msm.age.model
    
    if (overwrite.base.models)
    {
        components$sexual.transmission$base.heterosexual.male.age.model = heterosexual.male.age.model
        components$sexual.transmission$base.female.age.model = female.age.model
        components$sexual.transmission$base.msm.age.model = msm.age.model
    }
    
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
                             oe.female.pairings.with.msm,
                             fraction.heterosexual.male.pairings.with.male)
{
    components$sexual.transmission$fraction.msm.pairings.with.female = fraction.msm.pairings.with.female
    components$sexual.transmission$fraction.heterosexual.male.pairings.with.male = fraction.heterosexual.male.pairings.with.male
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
                             sex.oes=NULL)
{
    if (!is.null(sex.oes))
        components$idu.transmission$sex.oes = sex.oes
    
    components = clear.dependent.values(components, 'idu.transmission')
    components
}


##-- SUPPRESSION --##


set.background.suppression.ors <- function(components,
                                           msm.or.intercept=NA,
                                           heterosexual.or.intercept=NA,
                                           idu.or.intercept=NA,
                                           msm.idu.or.intercept=NA,
                                           black.or.intercept=NA,
                                           hispanic.or.intercept=NA,
                                           other.or.intercept=NA,
                                           age1.or.intercept=NA,
                                           age2.or.intercept=NA,
                                           age3.or.intercept=NA,
                                           age4.or.intercept=NA,
                                           age5.or.intercept=NA,
                                           
                                           total.or.slope=NA,
                                           
                                           msm.or.slope=NA,
                                           heterosexual.or.slope=NA,
                                           idu.or.slope=NA,
                                           msm.idu.or.slope=NA,
                                           black.or.slope=NA,
                                           hispanic.or.slope=NA,
                                           other.or.slope=NA,
                                           age1.or.slope=NA,
                                           age2.or.slope=NA,
                                           age3.or.slope=NA,
                                           age4.or.slope=NA,
                                           age5.or.slope=NA)
{
    components = do.set.background.ors(components,
                                       component.name='background.suppression',
                                       
                                       msm.or.intercept=msm.or.intercept,
                                       heterosexual.or.intercept=heterosexual.or.intercept,
                                       idu.or.intercept=idu.or.intercept,
                                       msm.idu.or.intercept=msm.idu.or.intercept,
                                       black.or.intercept=black.or.intercept,
                                       hispanic.or.intercept=hispanic.or.intercept,
                                       other.or.intercept=other.or.intercept,
                                       age1.or.intercept=age1.or.intercept,
                                       age2.or.intercept=age2.or.intercept,
                                       age3.or.intercept=age3.or.intercept,
                                       age4.or.intercept=age4.or.intercept,
                                       age5.or.intercept=age5.or.intercept,
                                       
                                       total.or.slope=total.or.slope,
                                       
                                       msm.or.slope=msm.or.slope,
                                       heterosexual.or.slope=heterosexual.or.slope,
                                       idu.or.slope=idu.or.slope,
                                       msm.idu.or.slope=msm.idu.or.slope,
                                       black.or.slope=black.or.slope,
                                       hispanic.or.slope=hispanic.or.slope,
                                       other.or.slope=other.or.slope,
                                       age1.or.slope=age1.or.slope,
                                       age2.or.slope=age2.or.slope,
                                       age3.or.slope=age3.or.slope,
                                       age4.or.slope=age4.or.slope,
                                       age5.or.slope=age5.or.slope
                                       )
    
    components
}

do.set.background.ors <- function(components,
                                  component.name,
                                  msm.or.intercept=NA,
                                  heterosexual.or.intercept=NA,
                                  idu.or.intercept=NA,
                                  msm.idu.or.intercept=NA,
                                  black.or.intercept=NA,
                                  hispanic.or.intercept=NA,
                                  other.or.intercept=NA,
                                  age1.or.intercept=NA,
                                  age2.or.intercept=NA,
                                  age3.or.intercept=NA,
                                  age4.or.intercept=NA,
                                  age5.or.intercept=NA,
                                  
                                  total.or.slope=NA,
                                  
                                  msm.or.slope=NA,
                                  heterosexual.or.slope=NA,
                                  idu.or.slope=NA,
                                  msm.idu.or.slope=NA,
                                  black.or.slope=NA,
                                  hispanic.or.slope=NA,
                                  other.or.slope=NA,
                                  age1.or.slope=NA,
                                  age2.or.slope=NA,
                                  age3.or.slope=NA,
                                  age4.or.slope=NA,
                                  age5.or.slope=NA
                                  )
{
    if (is.null(components[[component.name]]))
        components[[component.name]] = list()
    
    if (is.null(components[[component.name]]$additional.intercept.ors))
        components[[component.name]]$additional.intercept.ors = numeric()
    
    if (is.null(components[[component.name]]$additional.slope.ors))
        components[[component.name]]$additional.slope.ors = numeric()
    
    if (!is.na(msm.or.intercept))
        components[[component.name]]$additional.intercept.ors['msm'] = msm.or.intercept
    if (!is.na(heterosexual.or.intercept))
        components[[component.name]]$additional.intercept.ors['heterosexual'] = heterosexual.or.intercept
    if (!is.na(idu.or.intercept))
        components[[component.name]]$additional.intercept.ors['idu'] = idu.or.intercept
    if (!is.na(msm.idu.or.intercept))
        components[[component.name]]$additional.intercept.ors['msm_idu'] = msm.idu.or.intercept
    
    if (!is.na(black.or.intercept))
        components[[component.name]]$additional.intercept.ors['black'] = black.or.intercept
    if (!is.na(hispanic.or.intercept))
        components[[component.name]]$additional.intercept.ors['hispanic'] = hispanic.or.intercept
    if (!is.na(other.or.intercept))
        components[[component.name]]$additional.intercept.ors['other'] = other.or.intercept
    
    if (!is.na(age1.or.intercept))
        components[[component.name]]$additional.intercept.ors['age1'] = age1.or.intercept
    if (!is.na(age2.or.intercept))
        components[[component.name]]$additional.intercept.ors['age2'] = age2.or.intercept
    if (!is.na(age3.or.intercept))
        components[[component.name]]$additional.intercept.ors['age3'] = age3.or.intercept
    if (!is.na(age4.or.intercept))
        components[[component.name]]$additional.intercept.ors['age4'] = age4.or.intercept
    if (!is.na(age5.or.intercept))
        components[[component.name]]$additional.intercept.ors['age5'] = age5.or.intercept
    
    
    if (!is.na(total.or.slope))
        components[[component.name]]$additional.slope.ors['all'] = total.or.slope
    
    
    if (!is.na(msm.or.slope))
        components[[component.name]]$additional.slope.ors['msm'] = msm.or.slope
    if (!is.na(heterosexual.or.slope))
        components[[component.name]]$additional.slope.ors['heterosexual'] = heterosexual.or.slope
    if (!is.na(idu.or.slope))
        components[[component.name]]$additional.slope.ors['idu'] = idu.or.slope
    if (!is.na(msm.idu.or.slope))
        components[[component.name]]$additional.slope.ors['msm_idu'] = msm.idu.or.slope
    
    if (!is.na(black.or.slope))
        components[[component.name]]$additional.slope.ors['black'] = black.or.slope
    if (!is.na(hispanic.or.slope))
        components[[component.name]]$additional.slope.ors['hispanic'] = hispanic.or.slope
    if (!is.na(other.or.slope))
        components[[component.name]]$additional.slope.ors['other'] = other.or.slope
    
    if (!is.na(age1.or.slope))
        components[[component.name]]$additional.slope.ors['age1'] = age1.or.slope
    if (!is.na(age2.or.slope))
        components[[component.name]]$additional.slope.ors['age2'] = age2.or.slope
    if (!is.na(age3.or.slope))
        components[[component.name]]$additional.slope.ors['age3'] = age3.or.slope
    if (!is.na(age4.or.slope))
        components[[component.name]]$additional.slope.ors['age4'] = age4.or.slope
    if (!is.na(age5.or.slope))
        components[[component.name]]$additional.slope.ors['age5'] = age5.or.slope
    
    
    components = clear.dependent.values(components, component.name)
    components
}

set.future.background.slopes <- function(components,
                                         future.supression.slope.or=NA,
                                         future.testing.slope.or=NA,
                                         future.prep.slope.or=NA,
                                         future.newly.suppressed.slope.or=NA,
                                         future.unsuppression.slope.or=NA,
                                         future.linkage.slope.or=NA,
                                         future.unsuppressed.to.disengaged.slope.or=NA,
                                         future.suppressed.to.disengaged.slope.or=NA,
                                         future.reengagement.slope.or=NA,
                                         after.year=2020)
{
    if (is.null(components$background.suppression))
        components$background.suppression = list()
    
    if (!is.na(future.supression.slope.or))
    {
        components$background.suppression$future.slope.or = future.supression.slope.or
        components$background.suppression$future.slope.after.year = after.year
        components = clear.dependent.values(components, 'background.suppression')
    }
    
    if (!is.na(future.testing.slope.or))
    {
        components$background.testing$future.slope.or = future.testing.slope.or
        components$background.testing$future.slope.after.year = after.year
        components = clear.dependent.values(components, 'background.testing')
    }
    
    if (!is.na(future.prep.slope.or))
    {
        components$background.prep$future.slope.or = future.prep.slope.or
        components$background.prep$future.slope.after.year = after.year
        components = clear.dependent.values(components, 'background.prep')
    }
    
    if (!is.na(future.newly.suppressed.slope.or))
    {
        components$background.newly.suppressed$future.slope.or = future.newly.suppressed.slope.or
        components$background.newly.suppressed$future.slope.after.year = after.year
        components = clear.dependent.values(components, 'background.newly.suppressed')
    }
    
    if (!is.na(future.unsuppression.slope.or))
    {
        components$background.unsuppression$future.slope.or = future.unsuppression.slope.or
        components$background.unsuppression$future.slope.after.year = after.year
        components = clear.dependent.values(components, 'background.unsuppression')
    }
    
    if (!is.na(future.linkage.slope.or))
    {
        components$background.linkage$future.slope.or = future.linkage.slope.or
        components$background.linkage$future.slope.after.year = after.year
        components = clear.dependent.values(components, 'background.linkage')
    }
    
    if (!is.na(future.unsuppressed.to.disengaged.slope.or))
    {
        components$background.unsuppressed.to.disengaged$future.slope.or = future.unsuppressed.to.disengaged.slope.or
        components$background.unsuppressed.to.disengaged$future.slope.after.year = after.year
        components = clear.dependent.values(components, 'background.unsuppressed.to.disengaged')
    }
    
    if (!is.na(future.suppressed.to.disengaged.slope.or))
    {
        components$background.suppressed.to.disengaged$future.slope.or = future.suppressed.to.disengaged.slope.or
        components$background.suppressed.to.disengaged$future.slope.after.year = after.year
        components = clear.dependent.values(components, 'background.suppressed.to.disengaged')
    }
    
    if (!is.na(future.reengagement.slope.or))
    {
        components$background.reengagement$future.slope.or = future.reengagement.slope.or
        components$background.reengagement$future.slope.after.year = after.year
        components = clear.dependent.values(components, 'background.reengagement')
    }
    
    components 
}

setup.background.suppression <- function(components,
                                         continuum.manager,
                                         location,
                                         years,
                                         zero.suppression.year=1996)
{
    if (is.null(components$background.suppression))
        components$background.suppression = list()
    
    components$background.suppression$years = years
    components$background.suppression$zero.suppression.year = zero.suppression.year
    
    components$background.suppression$model = get.suppression.model(continuum.manager, 
                                                                    location=location)
    
    components$background.suppression$model$mixed.linear = F
    
    components = clear.dependent.values(components, 'background.suppression')
    components
}


#-- NEWLY SUPPRESSED --#
setup.background.newly.suppressed <- function(components,
                                              continuum.manager,
                                              location,
                                              years,
                                              zero.suppression.year=1996)
{
    if (is.null(components$background.background.newly.suppressed))
        components$background.newly.suppressed = list()
    
    components$background.newly.suppressed$years = years
    components$background.newly.suppressed$zero.suppression.year = zero.suppression.year
    
    components$background.newly.suppressed$model = get.newly.suppressed.model(continuum.manager, 
                                                                              location=location)
    
    components$background.newly.suppressed$model$mixed.linear = F
    
    components = clear.dependent.values(components, 'background.newly.suppressed')
    components
}


set.background.newly.suppressed.ors <- function(components,
                                           msm.or.intercept=NA,
                                           heterosexual.or.intercept=NA,
                                           idu.or.intercept=NA,
                                           msm.idu.or.intercept=NA,
                                           black.or.intercept=NA,
                                           hispanic.or.intercept=NA,
                                           other.or.intercept=NA,
                                           age1.or.intercept=NA,
                                           age2.or.intercept=NA,
                                           age3.or.intercept=NA,
                                           age4.or.intercept=NA,
                                           age5.or.intercept=NA,
                                           
                                           total.or.slope=NA,
                                           
                                           msm.or.slope=NA,
                                           heterosexual.or.slope=NA,
                                           idu.or.slope=NA,
                                           msm.idu.or.slope=NA,
                                           black.or.slope=NA,
                                           hispanic.or.slope=NA,
                                           other.or.slope=NA,
                                           age1.or.slope=NA,
                                           age2.or.slope=NA,
                                           age3.or.slope=NA,
                                           age4.or.slope=NA,
                                           age5.or.slope=NA)
{
    components = do.set.background.ors(components,
                                       component.name='background.newly.suppressed',
                                       
                                       msm.or.intercept=msm.or.intercept,
                                       heterosexual.or.intercept=heterosexual.or.intercept,
                                       idu.or.intercept=idu.or.intercept,
                                       msm.idu.or.intercept=msm.idu.or.intercept,
                                       black.or.intercept=black.or.intercept,
                                       hispanic.or.intercept=hispanic.or.intercept,
                                       other.or.intercept=other.or.intercept,
                                       age1.or.intercept=age1.or.intercept,
                                       age2.or.intercept=age2.or.intercept,
                                       age3.or.intercept=age3.or.intercept,
                                       age4.or.intercept=age4.or.intercept,
                                       age5.or.intercept=age5.or.intercept,
                                       
                                       total.or.slope=total.or.slope,
                                       
                                       msm.or.slope=msm.or.slope,
                                       heterosexual.or.slope=heterosexual.or.slope,
                                       idu.or.slope=idu.or.slope,
                                       msm.idu.or.slope=msm.idu.or.slope,
                                       black.or.slope=black.or.slope,
                                       hispanic.or.slope=hispanic.or.slope,
                                       other.or.slope=other.or.slope,
                                       age1.or.slope=age1.or.slope,
                                       age2.or.slope=age2.or.slope,
                                       age3.or.slope=age3.or.slope,
                                       age4.or.slope=age4.or.slope,
                                       age5.or.slope=age5.or.slope
    )
    
    components = clear.dependent.values(components, 'background.newly.suppressed')
    components
}

setup.background.unsuppression <- function(components,
                                     continuum.manager,
                                     location,
                                     years,
                                     unsuppression.ramp.year=1996,
                                     unsuppression.ramp.multiplier=1,
                                     time.to.link.vs.disengage=1/4 #3 months
)
{
    if (is.null(components$background.unsuppression))
        components$background.unsuppression = list()
    
    components$background.unsuppression$years = years
    components$background.unsuppression$unsuppression.ramp.year = unsuppression.ramp.year
    components$background.unsuppression$unsuppression.ramp.multiplier = unsuppression.ramp.multiplier
    
    components$background.unsuppression$model = get.unsuppression.model(continuum.manager, 
                                                            location=location)
    
    components$background.unsuppression$model$mixed.linear = F
    
    components$time.to.link.vs.disengage = time.to.link.vs.disengage
    
    components = clear.dependent.values(components, 'background.unsuppression')
    components
}

set.background.unsuppression.ors <- function(components,
                                                msm.or.intercept=NA,
                                                heterosexual.or.intercept=NA,
                                                idu.or.intercept=NA,
                                                msm.idu.or.intercept=NA,
                                                black.or.intercept=NA,
                                                hispanic.or.intercept=NA,
                                                other.or.intercept=NA,
                                                age1.or.intercept=NA,
                                                age2.or.intercept=NA,
                                                age3.or.intercept=NA,
                                                age4.or.intercept=NA,
                                                age5.or.intercept=NA,
                                                
                                                total.or.slope=NA,
                                                
                                                msm.or.slope=NA,
                                                heterosexual.or.slope=NA,
                                                idu.or.slope=NA,
                                                msm.idu.or.slope=NA,
                                                black.or.slope=NA,
                                                hispanic.or.slope=NA,
                                                other.or.slope=NA,
                                                age1.or.slope=NA,
                                                age2.or.slope=NA,
                                                age3.or.slope=NA,
                                                age4.or.slope=NA,
                                                age5.or.slope=NA)
{
    components = do.set.background.ors(components,
                                       component.name='background.unsuppression',
                                       
                                       msm.or.intercept=msm.or.intercept,
                                       heterosexual.or.intercept=heterosexual.or.intercept,
                                       idu.or.intercept=idu.or.intercept,
                                       msm.idu.or.intercept=msm.idu.or.intercept,
                                       black.or.intercept=black.or.intercept,
                                       hispanic.or.intercept=hispanic.or.intercept,
                                       other.or.intercept=other.or.intercept,
                                       age1.or.intercept=age1.or.intercept,
                                       age2.or.intercept=age2.or.intercept,
                                       age3.or.intercept=age3.or.intercept,
                                       age4.or.intercept=age4.or.intercept,
                                       age5.or.intercept=age5.or.intercept,
                                       
                                       total.or.slope=total.or.slope,
                                       
                                       msm.or.slope=msm.or.slope,
                                       heterosexual.or.slope=heterosexual.or.slope,
                                       idu.or.slope=idu.or.slope,
                                       msm.idu.or.slope=msm.idu.or.slope,
                                       black.or.slope=black.or.slope,
                                       hispanic.or.slope=hispanic.or.slope,
                                       other.or.slope=other.or.slope,
                                       age1.or.slope=age1.or.slope,
                                       age2.or.slope=age2.or.slope,
                                       age3.or.slope=age3.or.slope,
                                       age4.or.slope=age4.or.slope,
                                       age5.or.slope=age5.or.slope
    )
    
    components = clear.dependent.values(components, 'background.unsuppression')
    components
}



#-- LINKAGE --#
setup.background.linkage <- function(components,
                                     continuum.manager,
                                     location,
                                     years,
                                     linkage.ramp.year=1996,
                                     linkage.ramp.multiplier=1,
                                     time.to.link.vs.disengage=1/4 #3 months
                                     )
{
    if (is.null(components$background.linkage))
        components$background.linkage = list()
    
    components$background.linkage$years = years
    components$background.linkage$linkage.ramp.year = linkage.ramp.year
    components$background.linkage$linkage.ramp.multiplier = linkage.ramp.multiplier
    
    components$background.linkage$model = get.linkage.model(continuum.manager, 
                                                            location=location)
    
    components$background.linkage$model$mixed.linear = F
    
    components$time.to.link.vs.disengage = time.to.link.vs.disengage
    
    components = clear.dependent.values(components, 'background.linkage')
    components
}


set.background.linkage.ors <- function(components,
                                                msm.or.intercept=NA,
                                                heterosexual.or.intercept=NA,
                                                idu.or.intercept=NA,
                                                msm.idu.or.intercept=NA,
                                                black.or.intercept=NA,
                                                hispanic.or.intercept=NA,
                                                other.or.intercept=NA,
                                                age1.or.intercept=NA,
                                                age2.or.intercept=NA,
                                                age3.or.intercept=NA,
                                                age4.or.intercept=NA,
                                                age5.or.intercept=NA,
                                                
                                                total.or.slope=NA,
                                                
                                                msm.or.slope=NA,
                                                heterosexual.or.slope=NA,
                                                idu.or.slope=NA,
                                                msm.idu.or.slope=NA,
                                                black.or.slope=NA,
                                                hispanic.or.slope=NA,
                                                other.or.slope=NA,
                                                age1.or.slope=NA,
                                                age2.or.slope=NA,
                                                age3.or.slope=NA,
                                                age4.or.slope=NA,
                                                age5.or.slope=NA)
{
    components = do.set.background.ors(components,
                                       component.name='background.linkage',
                                       
                                       msm.or.intercept=msm.or.intercept,
                                       heterosexual.or.intercept=heterosexual.or.intercept,
                                       idu.or.intercept=idu.or.intercept,
                                       msm.idu.or.intercept=msm.idu.or.intercept,
                                       black.or.intercept=black.or.intercept,
                                       hispanic.or.intercept=hispanic.or.intercept,
                                       other.or.intercept=other.or.intercept,
                                       age1.or.intercept=age1.or.intercept,
                                       age2.or.intercept=age2.or.intercept,
                                       age3.or.intercept=age3.or.intercept,
                                       age4.or.intercept=age4.or.intercept,
                                       age5.or.intercept=age5.or.intercept,
                                       
                                       total.or.slope=total.or.slope,
                                       
                                       msm.or.slope=msm.or.slope,
                                       heterosexual.or.slope=heterosexual.or.slope,
                                       idu.or.slope=idu.or.slope,
                                       msm.idu.or.slope=msm.idu.or.slope,
                                       black.or.slope=black.or.slope,
                                       hispanic.or.slope=hispanic.or.slope,
                                       other.or.slope=other.or.slope,
                                       age1.or.slope=age1.or.slope,
                                       age2.or.slope=age2.or.slope,
                                       age3.or.slope=age3.or.slope,
                                       age4.or.slope=age4.or.slope,
                                       age5.or.slope=age5.or.slope
    )
    
    components = clear.dependent.values(components, 'background.linkage')
    components
}

#-- UNSUPPRESSED TO DISENGAGED --#
setup.background.unsuppressed.to.disengaged <- function(components,
                                           continuum.manager,
                                           location,
                                           years,
                                           ramp.year=1996,
                                           ramp.multiplier=1)
{
    if (is.null(components$background.unsuppressed.to.disengaged))
        components$background.unsuppressed.to.disengaged = list()
    
    components$background.unsuppressed.to.disengaged$years = years
    components$background.unsuppressed.to.disengaged$unsuppressed.to.disengaged.ramp.year = ramp.year
    components$background.unsuppressed.to.disengaged$unsuppressed.to.disengaged.ramp.multiplier = ramp.multiplier
    
    components$background.unsuppressed.to.disengaged$model = get.unsuppressed.to.disengaged.model(continuum.manager, 
                                                                        location=location)
    
    components$background.unsuppressed.to.disengaged$model$mixed.linear = F
    
    components = clear.dependent.values(components, 'background.unsuppressed.to.disengaged')
    components
}


set.background.unsuppressed.to.disengaged.ors <- function(components,
                                             msm.or.intercept=NA,
                                             heterosexual.or.intercept=NA,
                                             idu.or.intercept=NA,
                                             msm.idu.or.intercept=NA,
                                             black.or.intercept=NA,
                                             hispanic.or.intercept=NA,
                                             other.or.intercept=NA,
                                             age1.or.intercept=NA,
                                             age2.or.intercept=NA,
                                             age3.or.intercept=NA,
                                             age4.or.intercept=NA,
                                             age5.or.intercept=NA,
                                             
                                             total.or.slope=NA,
                                             
                                             msm.or.slope=NA,
                                             heterosexual.or.slope=NA,
                                             idu.or.slope=NA,
                                             msm.idu.or.slope=NA,
                                             black.or.slope=NA,
                                             hispanic.or.slope=NA,
                                             other.or.slope=NA,
                                             age1.or.slope=NA,
                                             age2.or.slope=NA,
                                             age3.or.slope=NA,
                                             age4.or.slope=NA,
                                             age5.or.slope=NA)
{
    components = do.set.background.ors(components,
                                       component.name='background.unsuppressed.to.disengaged',
                                       
                                       msm.or.intercept=msm.or.intercept,
                                       heterosexual.or.intercept=heterosexual.or.intercept,
                                       idu.or.intercept=idu.or.intercept,
                                       msm.idu.or.intercept=msm.idu.or.intercept,
                                       black.or.intercept=black.or.intercept,
                                       hispanic.or.intercept=hispanic.or.intercept,
                                       other.or.intercept=other.or.intercept,
                                       age1.or.intercept=age1.or.intercept,
                                       age2.or.intercept=age2.or.intercept,
                                       age3.or.intercept=age3.or.intercept,
                                       age4.or.intercept=age4.or.intercept,
                                       age5.or.intercept=age5.or.intercept,
                                       
                                       total.or.slope=total.or.slope,
                                       
                                       msm.or.slope=msm.or.slope,
                                       heterosexual.or.slope=heterosexual.or.slope,
                                       idu.or.slope=idu.or.slope,
                                       msm.idu.or.slope=msm.idu.or.slope,
                                       black.or.slope=black.or.slope,
                                       hispanic.or.slope=hispanic.or.slope,
                                       other.or.slope=other.or.slope,
                                       age1.or.slope=age1.or.slope,
                                       age2.or.slope=age2.or.slope,
                                       age3.or.slope=age3.or.slope,
                                       age4.or.slope=age4.or.slope,
                                       age5.or.slope=age5.or.slope
    )
    
    components = clear.dependent.values(components, 'background.unsuppressed.to.disengaged')
    components
}

#-- SUPPRESSED TO DISENGAGED --#
setup.background.suppressed.to.disengaged <- function(components,
                                                        continuum.manager,
                                                        location,
                                                        years,
                                                        ramp.year=1996,
                                                        ramp.multiplier=1)
{
    if (is.null(components$background.suppressed.to.disengaged))
        components$background.suppressed.to.disengaged = list()
    
    components$background.suppressed.to.disengaged$years = years
    components$background.suppressed.to.disengaged$suppressed.to.disengaged.ramp.year = ramp.year
    components$background.suppressed.to.disengaged$suppressed.to.disengaged.ramp.multiplier = ramp.multiplier
    
    components$background.suppressed.to.disengaged$model = get.suppressed.to.disengaged.model(continuum.manager, 
                                                                                                  location=location)
    
    components$background.suppressed.to.disengaged$model$mixed.linear = F
    
    components = clear.dependent.values(components, 'background.suppressed.to.disengaged')
    components
}


set.background.suppressed.to.disengaged.ors <- function(components,
                                                          msm.or.intercept=NA,
                                                          heterosexual.or.intercept=NA,
                                                          idu.or.intercept=NA,
                                                          msm.idu.or.intercept=NA,
                                                          black.or.intercept=NA,
                                                          hispanic.or.intercept=NA,
                                                          other.or.intercept=NA,
                                                          age1.or.intercept=NA,
                                                          age2.or.intercept=NA,
                                                          age3.or.intercept=NA,
                                                          age4.or.intercept=NA,
                                                          age5.or.intercept=NA,
                                                          
                                                          total.or.slope=NA,
                                                          
                                                          msm.or.slope=NA,
                                                          heterosexual.or.slope=NA,
                                                          idu.or.slope=NA,
                                                          msm.idu.or.slope=NA,
                                                          black.or.slope=NA,
                                                          hispanic.or.slope=NA,
                                                          other.or.slope=NA,
                                                          age1.or.slope=NA,
                                                          age2.or.slope=NA,
                                                          age3.or.slope=NA,
                                                          age4.or.slope=NA,
                                                          age5.or.slope=NA)
{
    components = do.set.background.ors(components,
                                       component.name='background.suppressed.to.disengaged',
                                       
                                       msm.or.intercept=msm.or.intercept,
                                       heterosexual.or.intercept=heterosexual.or.intercept,
                                       idu.or.intercept=idu.or.intercept,
                                       msm.idu.or.intercept=msm.idu.or.intercept,
                                       black.or.intercept=black.or.intercept,
                                       hispanic.or.intercept=hispanic.or.intercept,
                                       other.or.intercept=other.or.intercept,
                                       age1.or.intercept=age1.or.intercept,
                                       age2.or.intercept=age2.or.intercept,
                                       age3.or.intercept=age3.or.intercept,
                                       age4.or.intercept=age4.or.intercept,
                                       age5.or.intercept=age5.or.intercept,
                                       
                                       total.or.slope=total.or.slope,
                                       
                                       msm.or.slope=msm.or.slope,
                                       heterosexual.or.slope=heterosexual.or.slope,
                                       idu.or.slope=idu.or.slope,
                                       msm.idu.or.slope=msm.idu.or.slope,
                                       black.or.slope=black.or.slope,
                                       hispanic.or.slope=hispanic.or.slope,
                                       other.or.slope=other.or.slope,
                                       age1.or.slope=age1.or.slope,
                                       age2.or.slope=age2.or.slope,
                                       age3.or.slope=age3.or.slope,
                                       age4.or.slope=age4.or.slope,
                                       age5.or.slope=age5.or.slope
    )
    
    components = clear.dependent.values(components, 'background.suppressed.to.disengaged')
    components
}


#-- REENGAGEMENT --#
setup.background.reengagement <- function(components,
                                                      continuum.manager,
                                                      location,
                                                      years,
                                                      ramp.year=1996,
                                                      ramp.multiplier=1)
{
    if (is.null(components$background.reengagement))
        components$background.reengagement = list()
    
    components$background.reengagement$years = years
    components$background.reengagement$reengagement.ramp.year = ramp.year
    components$background.reengagement$reengagement.ramp.multiplier = ramp.multiplier
    
    components$background.reengagement$model = get.reengagement.model(continuum.manager, 
                                                                                              location=location)
    
    components$background.reengagement$model$mixed.linear = F
    
    components = clear.dependent.values(components, 'background.reengagement')
    components
}


set.background.reengagement.ors <- function(components,
                                                        msm.or.intercept=NA,
                                                        heterosexual.or.intercept=NA,
                                                        idu.or.intercept=NA,
                                                        msm.idu.or.intercept=NA,
                                                        black.or.intercept=NA,
                                                        hispanic.or.intercept=NA,
                                                        other.or.intercept=NA,
                                                        age1.or.intercept=NA,
                                                        age2.or.intercept=NA,
                                                        age3.or.intercept=NA,
                                                        age4.or.intercept=NA,
                                                        age5.or.intercept=NA,
                                                        
                                                        total.or.slope=NA,
                                                        
                                                        msm.or.slope=NA,
                                                        heterosexual.or.slope=NA,
                                                        idu.or.slope=NA,
                                                        msm.idu.or.slope=NA,
                                                        black.or.slope=NA,
                                                        hispanic.or.slope=NA,
                                                        other.or.slope=NA,
                                                        age1.or.slope=NA,
                                                        age2.or.slope=NA,
                                                        age3.or.slope=NA,
                                                        age4.or.slope=NA,
                                                        age5.or.slope=NA)
{
    components = do.set.background.ors(components,
                                       component.name='background.reengagement',
                                       
                                       msm.or.intercept=msm.or.intercept,
                                       heterosexual.or.intercept=heterosexual.or.intercept,
                                       idu.or.intercept=idu.or.intercept,
                                       msm.idu.or.intercept=msm.idu.or.intercept,
                                       black.or.intercept=black.or.intercept,
                                       hispanic.or.intercept=hispanic.or.intercept,
                                       other.or.intercept=other.or.intercept,
                                       age1.or.intercept=age1.or.intercept,
                                       age2.or.intercept=age2.or.intercept,
                                       age3.or.intercept=age3.or.intercept,
                                       age4.or.intercept=age4.or.intercept,
                                       age5.or.intercept=age5.or.intercept,
                                       
                                       total.or.slope=total.or.slope,
                                       
                                       msm.or.slope=msm.or.slope,
                                       heterosexual.or.slope=heterosexual.or.slope,
                                       idu.or.slope=idu.or.slope,
                                       msm.idu.or.slope=msm.idu.or.slope,
                                       black.or.slope=black.or.slope,
                                       hispanic.or.slope=hispanic.or.slope,
                                       other.or.slope=other.or.slope,
                                       age1.or.slope=age1.or.slope,
                                       age2.or.slope=age2.or.slope,
                                       age3.or.slope=age3.or.slope,
                                       age4.or.slope=age4.or.slope,
                                       age5.or.slope=age5.or.slope
    )
    
    components = clear.dependent.values(components, 'background.reengagement')
    components
}

#-- CHANGE TO YEARS --#

set.background.change.to.years <- function(components,
                                           testing.change.to.year,
                                           suppression.change.to.year,
                                           prep.change.to.year,
                                           newly.suppressed.change.to.year=NA,
                                           unsuppression.change.to.year=NA,
                                           linkage.change.to.year=NA,
                                           unsuppressed.to.disengaged.change.to.year=NA,
                                           suppressed.to.disengaged.change.to.year=NA,
                                           reengagement.change.to.year=NA)
{
    components$background.change.to.years = list(testing=testing.change.to.year,
                                                 suppression=suppression.change.to.year,
                                                 prep=prep.change.to.year,
                                                 newly.suppressed=newly.suppressed.change.to.year,
                                                 unsuppression=unsuppression.change.to.year,
                                                 linkage=linkage.change.to.year,
                                                 unsuppressed.to.disengaged=unsuppressed.to.disengaged.change.to.year,
                                                 suppressed.to.disengaged.change.to.year=suppressed.to.disengaged.change.to.year,
                                                 reengagement.change.to.year=reengagement.change.to.year)
    
    components = clear.dependent.values(components, c('background.prep',
                                                      'background.testing',
                                                      'background.suppression',
                                                      'background.newly.suppressed',
                                                      'background.unsuppression',
                                                      'background.linkage',
                                                      'background.unsuppressed.to.disengaged',
                                                      'background.suppressed.to.disengaged',
                                                      'background.reengagement'))
    components
}

##-- PREP --##

set.background.prep.max.proportions <- function(components,
                                                msm.or=NA,
                                                idu.or=NA,
                                                heterosexual.or=NA,
                                                msm.idu.or=NA,
                                                
                                                black.or=NA,
                                                hispanic.or=NA,
                                                
                                                age1.or=NA,
                                                age2.or=NA,
                                                age4.or=NA,
                                                age5.or=NA,
                                                
                                                female.or=NA)
{
    if (is.null(components$background.prep))
        components$background.prep = list()
    
    if (is.null(components$background.prep$max.proportion.ors))
        components$background.prep$additional.intercept.ors = numeric()
    
    
    if (!is.na(msm.or))
        components$background.prep$max.proportion.ors['msm'] = msm.or
    if (!is.na(idu.or))
        components$background.prep$max.proportion.ors['idu'] = idu.or
    if (!is.na(heterosexual.or))
        components$background.prep$max.proportion.ors['heterosexual'] = heterosexual.or
    if (!is.na(msm.idu.or))
        components$background.prep$max.proportion.ors['msm_idu'] = msm.idu.or
    
    if (!is.na(black.or))
        components$background.prep$max.proportion.ors['black'] = black.or
    if (!is.na(hispanic.or))
        components$background.prep$max.proportion.ors['hispanic'] = hispanic.or
    
    if (!is.na(age1.or))
        components$background.prep$max.proportion.ors['age1'] = age1.or
    if (!is.na(age2.or))
        components$background.prep$max.proportion.ors['age2'] = age2.or
    if (!is.na(age4.or))
        components$background.prep$max.proportion.ors['age4'] = age4.or
    if (!is.na(age5.or))
        components$background.prep$max.proportion.ors['age5'] = age5.or
    
    if (!is.na(female.or))
        components$background.prep$max.proportion.ors['female'] = female.or
    
    
    components = clear.dependent.values(components, 'background.prep')
    components
}

set.background.prep.ors <- function(components,
                                    msm.or.intercept=NA,
                                    heterosexual.or.intercept=NA,
                                    idu.or.intercept=NA,
                                    msm.idu.or.intercept=NA,
                                    black.or.intercept=NA,
                                    hispanic.or.intercept=NA,
                                    other.or.intercept=NA,
                                    age1.or.intercept=NA,
                                    age2.or.intercept=NA,
                                    age3.or.intercept=NA,
                                    age4.or.intercept=NA,
                                    age5.or.intercept=NA,
                                    female.or.intercept=NA,
                                    
                                    total.or.slope=NA,
                                    
                                    msm.or.slope=NA,
                                    heterosexual.or.slope=NA,
                                    idu.or.slope=NA,
                                    msm.idu.or.slope=NA,
                                    black.or.slope=NA,
                                    hispanic.or.slope=NA,
                                    other.or.slope=NA,
                                    age1.or.slope=NA,
                                    age2.or.slope=NA,
                                    age3.or.slope=NA,
                                    age4.or.slope=NA,
                                    age5.or.slope=NA,
                                    female.or.slope=NA)
{
    if (is.null(components$background.prep))
        components$background.prep = list()
    
    if (is.null(components$background.prep$additional.intercept.ors))
        components$background.prep$additional.intercept.ors = numeric()
    
    if (is.null(components$background.prep$additional.slope.ors))
        components$background.prep$additional.slope.ors = numeric()
    
    if (!is.na(msm.or.intercept))
        components$background.prep$additional.intercept.ors['msm'] = msm.or.intercept
    if (!is.na(heterosexual.or.intercept))
        components$background.prep$additional.intercept.ors['heterosexual'] = heterosexual.or.intercept
    if (!is.na(idu.or.intercept))
        components$background.prep$additional.intercept.ors['idu'] = idu.or.intercept
    if (!is.na(msm.idu.or.intercept))
        components$background.prep$additional.intercept.ors['msm_idu'] = msm.idu.or.intercept
    
    if (!is.na(black.or.intercept))
        components$background.prep$additional.intercept.ors['black'] = black.or.intercept
    if (!is.na(hispanic.or.intercept))
        components$background.prep$additional.intercept.ors['hispanic'] = hispanic.or.intercept
    if (!is.na(other.or.intercept))
        components$background.prep$additional.intercept.ors['other'] = other.or.intercept
    
    if (!is.na(age1.or.intercept))
        components$background.prep$additional.intercept.ors['age1'] = age1.or.intercept
    if (!is.na(age2.or.intercept))
        components$background.prep$additional.intercept.ors['age2'] = age2.or.intercept
    if (!is.na(age3.or.intercept))
        components$background.prep$additional.intercept.ors['age3'] = age3.or.intercept
    if (!is.na(age4.or.intercept))
        components$background.prep$additional.intercept.ors['age4'] = age4.or.intercept
    if (!is.na(age5.or.intercept))
        components$background.prep$additional.intercept.ors['age5'] = age5.or.intercept
    
    if (!is.na(female.or.intercept))
        components$background.prep$additional.intercept.ors['female'] = female
    
    if (!is.na(total.or.slope))
        components$background.prep$additional.slope.ors['all'] = total.or.slope
    
    if (!is.na(msm.or.slope))
        components$background.prep$additional.slope.ors['msm'] = msm.or.slope
    if (!is.na(heterosexual.or.slope))
        components$background.prep$additional.slope.ors['heterosexual'] = heterosexual.or.slope
    if (!is.na(idu.or.slope))
        components$background.prep$additional.slope.ors['idu'] = idu.or.slope
    if (!is.na(msm.idu.or.slope))
        components$background.prep$additional.slope.ors['msm_idu'] = msm.idu.or.slope
    
    if (!is.na(black.or.slope))
        components$background.prep$additional.slope.ors['black'] = black.or.slope
    if (!is.na(hispanic.or.slope))
        components$background.prep$additional.slope.ors['hispanic'] = hispanic.or.slope
    if (!is.na(other.or.slope))
        components$background.prep$additional.slope.ors['other'] = other.or.slope
    
    if (!is.na(age1.or.slope))
        components$background.prep$additional.slope.ors['age1'] = age1.or.slope
    if (!is.na(age2.or.slope))
        components$background.prep$additional.slope.ors['age2'] = age2.or.slope
    if (!is.na(age3.or.slope))
        components$background.prep$additional.slope.ors['age3'] = age3.or.slope
    if (!is.na(age4.or.slope))
        components$background.prep$additional.slope.ors['age4'] = age4.or.slope
    if (!is.na(age5.or.slope))
        components$background.prep$additional.slope.ors['age5'] = age5.or.slope
    
    if (!is.na(female.or.slope))
        components$background.prep$additional.slope.ors['female'] = female
    
    
    components = clear.dependent.values(components, 'background.prep')
    components
}

setup.background.prep <- function(components,
                                  prep.manager,
                                  years,
                                  zero.prep.year=2010)
{
    if (is.null(components$background.prep))
        components$background.prep = list()
    
    if (any(zero.prep.year>=years))
        stop("'zero.prep.year' must be before all elements of 'years'")
    
    components$background.prep$years = years
    components$background.prep$zero.prep.year = zero.prep.year
    
    components$background.prep$model = get.prep.model(prep.manager)
    names(components$background.prep$model)[grepl('intercept', names(components$background.prep$model))] = 'intercept'
    names(components$background.prep$model)[grepl('slope', names(components$background.prep$model))] = 'slope'
    
    components = clear.dependent.values(components, 'background.prep')
    components
}

setup.background.needle.exchange <- function(components,
                                             proportions=0,
                                             years=2010)
{
    if (!is(proportions, 'list'))
        proportions = list(proportions)
    components$background.needle.exchange = list(
        years = years,
        proportions = lapply(proportions, function(p){
            expand.population.to.general(components$jheem, p)[,,,,'active_IDU'] #gives us [age, race, subpop, sex]
        })
    )
    
    components = clear.dependent.values(components, 'background.needle.exchange')
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

