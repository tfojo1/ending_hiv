#source('../code/setup/setup_helpers.R')
#source('../code/setup/logit_transformations.R')

##------------------------##
##-- RUN FOR COMPONENTS --##
##------------------------##

DEFAULT.JHEEM.ATOL = 1e-04
DEFAULT.JHEEM.RTOL = 1e-04
run.jheem.from.components <- function(components,
                                      start.year=1970, end.year=2020,
                                      max.run.time.seconds=Inf,
                                      prior.results=NULL,
                                      keep.components=T,
                                      pare.components=F,
                                      keep.years=start.year:end.year,
                                      atol=atol, rtol=rtol)
{
    #-- Pull JHEEM --#
    jheem = setup.jheem.from.components(components)
    
    #-- Run it -_#
    results = run.jheem(jheem,
                        prior.run.results = prior.results,
                        start.year = start.year, end.year = end.year,
                        verbose=F, print.warnings = F,
                        max.run.time.seconds=max.run.time.seconds,
                        keep.years=keep.years, 
                        atol = DEFAULT.JHEEM.ATOL, rtol=DEFAULT.JHEEM.RTOL)
    
    #-- Store some attributes --#
    attr(results, 'msm.proportions.by.race') = components$proportions.msm.of.male
    attr(results, 'smoothed.suppressed.proportions') = attr(components, 'smoothed.suppressed.proportions')
    attr(results, 'smoothed.testing.proportions') = attr(components, 'smoothed.testing.proportions')
    
    attr(results, 'location') = attr(components, 'location')
    
    if (keep.components)
    {
        components = crunch.intervention.rates(components)
        if (pare.components)
            components = pare.jheem.components(components)
        attr(results, 'components') = components
    }
    
    #-- Return it --#
    results
}

##------------------------##
##-- THE MAIN FUNCTIONs --##
##------------------------##

crunch.all.jheem.components <- function(components, verbose=F)
{
    if (verbose)
        print("CRUNCHING JHEEM COMPONENTS:")
    do.setup.crunch.or.fix(components,
                        setting = CRUNCH,
                        verbose=verbose)
}

fix.jheem.components <- function(components, verbose=F)
{
    if (verbose)
        print("FIXING JHEEM COMPONENTS:")
    components = do.setup.crunch.or.fix(components,
                                                 setting = FIX,
                                        verbose=verbose)
    components$fixed = T
    
    components
}

unfix.jheem.components <- function(components)
{
    components$fixed.jheem = NULL
    components$fixed = F
    
    components
}

unfix.simset.components <- function(simset)
{
    extend.simulations(simset, function(sim, parameters){
        attr(sim, 'components') = unfix.jheem.components(attr(sim, 'components'))
        sim
    })
}

setup.jheem.from.components <- function(components, verbose=F)
{
    do.setup.crunch.or.fix(components,
                           setting=PRODUCE.JHEEM,
                           verbose=verbose)
}
#just does the rates we might intervene on (for pre-caching for plots)
crunch.intervention.rates <- function(components)
{
    if (is.null(components$suppression.rates.and.times))
        components = do.calculate.suppression(components)
    if (is.null(components$testing.rates.and.times))
        components = do.calculate.rates(components, 'testing')
    if (is.null(components$prep.rates.and.times))
        components = do.calculate.prep.coverage(components)
    if (is.null(components$needle.exchange.rates.and.times))
        components = do.calculate.needle.exchange.coverage(components)
    
    components
}

# setting aggressive=T will save a lot of memory
# but it will render the components unable to run further simulations
# (basically, it keeps just the pre-computed rates for suppression, testing, prep, etc)
pare.jheem.components <- function(components,
                                  keep.rates.and.times=T,
                                  aggressive=F,
                                  keep.years=NULL)
{
    components = unfix.jheem.components(components)
    if (is.null(components$ALL.DEPENDENT.NAMES))
        components = enumerate.components.dependencies(components)
    
    # Decide what we're going to keep
    to.keep = character()
    if (keep.rates.and.times)
        to.keep = ALL.DEPENDENT.NAMES[grepl('rates.and.times', components$ALL.DEPENDENT.NAMES)]
    
    # Decide what we're going to remove
    if (aggressive)
        to.clear = setdiff(names(components), to.keep)
    else
        to.clear = setdiff(components$ALL.DEPENDENT.NAMES, to.keep)
    
    # remove
    for (elem in to.clear)
        components[[elem]] = NULL

    # trim years for what we keep
    if (!is.null(keep.years))
    {
        for (elem in to.keep)
            components[[elem]] = trim.rates.and.times(components[[elem]], keep.times=keep.years)
    }
    
    # package and return
    if (!aggressive)
        components = do.setup.jheem.skeleton(components)
    
    components
}

FIX = 1
CRUNCH = 2
PRODUCE.FROM.UNFIXED = 3
PRODUCE.FROM.FIXED = 4
PRODUCE.JHEEM = 5

do.setup.crunch.or.fix <- function(components,
                                   setting,
                                   verbose)
{
    if (setting==PRODUCE.JHEEM)
    {
        if (components$fixed)
            setting = PRODUCE.FROM.FIXED
        else
            setting = PRODUCE.FROM.UNFIXED
    }
    #FOUR POSSIBILITIES
    # produce from unfixed -> set up all null components, set all components to jheem
    # produce from fixed -> set up all null components, set all previously null components to jheem
    # fix -> set up no components, set all non-null components to jheem
    # crunch -> set up all null components, set nothing to jheem
    
    
    #-- SET-UP JHEEM --#
    if (setting == PRODUCE.FROM.FIXED)
        jheem = components$fixed.jheem
    else
    {
        if (is.null(components$jheem))
        {
            if (verbose)
                print('Setting up JHEEM skeleton')
            components = do.setup.jheem.skeleton(components)
        }
        jheem = components$jheem
    }
    
    #-- INITIAL POPULATION --#
    comp.was.null = is.null(components$initial.population)
    if (comp.was.null && setting != FIX)
    {
        if (verbose)
            print('Setting up Initial Population')
        components = do.setup.initial.population(components)
    }
    if (setting == PRODUCE.FROM.UNFIXED ||
        (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
        (setting == FIX && !comp.was.null))
    {
        jheem = set.initial.populations(jheem,
                                        expand.population.to.hiv.negative(jheem, components$initial.population$hiv.negative),
                                        expand.population.to.hiv.positive(jheem, components$initial.population$hiv.positive))
    }
    
    #-- BIRTHS --#
    comp.was.null = is.null(components$fertility.check)
    if (comp.was.null && setting != FIX)
    {
        if (is.null(components$fertility))
            stop("Fertility rates have not been set up in the components to make the JHEEM")
        components$ferility.check = 'check'
    }
    if (setting == PRODUCE.FROM.UNFIXED ||
        (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
        (setting == FIX && !comp.was.null))
        jheem = set.fertility(jheem, components$fertility)
    
    comp.was.null = is.null(components$birth.proportions)
    if (comp.was.null && setting != FIX)
    {
        if (verbose)
            print('Setting up Birth Proportions')
        components = do.setup.birth.proportions(components)
    }
    
    if (setting == PRODUCE.FROM.UNFIXED ||
        (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
        (setting == FIX && !comp.was.null))
        jheem = set.birth.proportions.no.maternal.transmission(jheem, components$birth.proportions)
    
    #-- AGING --#
    comp.was.null = is.null(components$aging.hiv.negative) || is.null(components$aging.rates.hiv.positive)
    if (comp.was.null && setting != FIX)
    {
        if (verbose)
            print('Setting up Aging')
        components = do.setup.aging(components)
    }
    
    if (setting == PRODUCE.FROM.UNFIXED ||
        (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
        (setting == FIX && !comp.was.null))
    {
        jheem = set.aging.hiv.negative(jheem, components$aging.hiv.negative)
        
        for (i in 1:length(components$aging.hiv.positive.years))
        {
            jheem = set.aging.hiv.positive(jheem,
                                           components$aging.rates.hiv.positive[[i]],
                                           time=components$aging.hiv.positive.years[i])
        }
    }
    
    #-- MORTALITY --#
    comp.was.null = is.null(components$general.mortality)
    if (comp.was.null && setting != FIX)
    {
        if (verbose)
            print('Setting up General Mortality')
        components = do.setup.general.mortality(components)
    }
    
    if (setting == PRODUCE.FROM.UNFIXED ||
        (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
        (setting == FIX && !comp.was.null))
        jheem = set.general.mortality(jheem, components$general.mortality)
    
    comp.was.null = is.null(components$hiv.mortality.rates)
    if (comp.was.null && setting != FIX)
    {
        if (verbose)
            print('Setting up HIV Mortality')
        components = do.setup.hiv.mortality(components) #this needs to go before transmissibility for suppression
    }
    if (setting == PRODUCE.FROM.UNFIXED ||
        (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
        (setting == FIX && !comp.was.null))
    {
        for (i in 1:length(components$hiv.mortality.rates))
            jheem = set.hiv.specific.mortality(jheem,
                                               components$hiv.mortality.rates[[i]],
                                               time = components$hiv.mortality.years[i])
    }
    
    #-- TRANSITIONS --#
    
    # IDU
    comp.was.null = is.null(components$idu.transitions)
    if (comp.was.null && setting != FIX)
    {
        if (verbose)
            print("Setting up IDU Transitions")
        components = do.setup.idu.transitions(components)
    }
    if (components$model.idu &&
        (setting == PRODUCE.FROM.UNFIXED ||
         (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
         (setting == FIX && !comp.was.null)))
    {
        for (i in 1:length(components$interpolated.idu.transition.years))
        {
            jheem = set.transition.array.hiv.negative(jheem, components$idu.transitions[[i]], time=components$interpolated.idu.transition.years[i])
            jheem = set.transition.array.hiv.positive(jheem, components$idu.transitions[[i]], time=components$interpolated.idu.transition.years[i])
        }
    }
    
    # CD4
    comp.was.null = is.null(components$cd4.transitions)
    if (comp.was.null && setting != FIX)
    {
        if (verbose)
            print("Setting up CD4 Transitions")
        components = do.setup.cd4.transitions(components)
    }
    if (setting == PRODUCE.FROM.UNFIXED ||
        (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
        (setting == FIX && !comp.was.null))
    {
        jheem = set.transition.array.hiv.positive(jheem, components$cd4.transitions)
    }
    
    # Continuum
    comp.was.null = is.null(components$continuum.transitions)
    if (comp.was.null && setting != FIX)
    {
        if (verbose)
            print("Setting up Continuum Transitions")
        components = do.setup.continuum.transitions(components)
    }
    if (setting == PRODUCE.FROM.UNFIXED ||
        (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
        (setting == FIX && !comp.was.null))
    {
        for (i in 1:length(components$continuum.transition.years))
            jheem = set.transition.array.hiv.positive(jheem,
                                                      components$continuum.transitions[[i]],
                                                      time=components$continuum.transition.years[i])
    }
    
    
    #-- TRANSMISSION --#
    
    # Global Rates
    comp.was.null = is.null(components$global.sexual.transmission.rates)
    if (comp.was.null && setting != FIX)
        components = do.setup.global.sexual.transmission(components)
    
    if (setting == PRODUCE.FROM.UNFIXED ||
        (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
        (setting == FIX && !comp.was.null))
    {
        if (!is.null(components$global.sexual.transmission.rates))
        {
            for (i in 1:length(components$global.sexual.transmission.rates))
                jheem = set.global.transmission.rate(jheem,
                                                     transmission.rate=components$global.sexual.transmission.rates[i],
                                                     time=components$global.sexual.transmission.years[i],
                                                     transmission.route.names = 'sexual')
        }
    }
    
    comp.was.null = is.null(components$global.idu.transmission.rates)
    if (comp.was.null && setting != FIX)
        components = do.setup.global.idu.transmission(components)
    
    if (setting == PRODUCE.FROM.UNFIXED ||
        (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
        (setting == FIX && !comp.was.null))
    {
        if (!is.null(components$global.idu.transmission.rates))
        {
            for (i in 1:length(components$global.idu.transmission.rates))
                jheem = set.global.transmission.rate(jheem,
                                                     transmission.rate=components$global.idu.transmission.rates[i],
                                                     time=components$global.idu.transmission.years[i],
                                                     transmission.route.names = 'idu')
        }
    }
    
    # Susceptibility
    comp.was.null = is.null(components[['sexual.susceptibility']]) ||
        (components$model.idu && is.null(components[['idu.susceptibility']]))
    if (comp.was.null && setting != FIX)
    {
        if (verbose)
            print("Setting up Susceptibility")
        components = do.setup.susceptibility(components) #this needs to go before new infection proportion for crunch prep coverage
    }
    if (setting == PRODUCE.FROM.UNFIXED ||
        (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
        (setting == FIX && !comp.was.null))
    {
        for (i in 1:length(components$sexual.susceptibility.years))
        {
            jheem = set.susceptibility(jheem,
                                       components$sexual.susceptibility[[i]],
                                       time=components$sexual.susceptibility.years[i],
                                       transmission.route.names = 'sexual')
        }
        
        if (components$model.idu)
        {
            for (i in 1:length(components$idu.susceptibility.years))
            {
                jheem = set.susceptibility(jheem,
                                           components$idu.susceptibility[[i]],
                                           time=components$idu.susceptibility.years[i],
                                           transmission.route.names = 'idu')
            }
        }
    }
    
    # Transmissibility
    comp.was.null = is.null(components$sexual.transmissibilities) ||
        (components$model.idu && is.null(components$idu.transmissibility))
    if (comp.was.null && setting != FIX)
    {
        if (verbose)
            print('Setting up Transmissibility')
        components = do.setup.transmissibility(components)
    }
    
    if (setting == PRODUCE.FROM.UNFIXED ||
        (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
        (setting == FIX && !comp.was.null))
    {
        for (i in 1:length(components$sexual.transmissibilities))
            jheem = set.transmissibility(jheem,
                                         components$sexual.transmissibilities[[i]],
                                         time=components$sexual.transmissibility.years[i],
                                         transmission.route.names = 'sexual')
        
        if (components$model.idu)
        {
            for (i in 1:length(components$idu.transmissibilities))
                jheem = set.transmissibility(jheem,
                                             components$idu.transmissibilities[[i]],
                                             time=components$idu.transmissibility.years[i],
                                             transmission.route.names = 'idu')
        }
    }
    
    # Sexual Contact
    comp.was.null = is.null(components$sexual.contact.arrays)
    if (comp.was.null && setting != FIX)
    {
        if (verbose)
            print("Setting up Sexual Contact")
        components = do.setup.sexual.contact(components)
    }
    if (setting == PRODUCE.FROM.UNFIXED ||
        (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
        (setting == FIX && !comp.was.null))
    {
        for (i in 1:length(components$sexual.contact.arrays))
        {
            year = components$sexual.contact.years[i]
            jheem = set.transmission.contact.array(jheem,
                                                   contact.array=components$sexual.contact.arrays[[i]],
                                                   transmission.route.names = 'sexual',
                                                   time=year)
        }
    }
    
    # IDU Contact
    if (components$model.idu)
    {
        comp.was.null = is.null(components$idu.contact.arrays)
        if (comp.was.null && setting != FIX)
        {
            if (verbose)
                print('Setting up IDU Contact')
            components = do.setup.idu.contact(components)
        }
        if (setting == PRODUCE.FROM.UNFIXED ||
            (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
            (setting == FIX && !comp.was.null))
        {
            for (i in 1:length(components$idu.contact.arrays))
            {
                year = components$idu.contact.years[i]
                jheem = set.transmission.contact.array(jheem,
                                                       contact.array=components$idu.contact.arrays[[i]],
                                                       transmission.route.names = 'idu',
                                                       time=year)
            }
        }
    }
    
    # New Infection Proportions
    if (components$model.hiv.transmission)
    {
        comp.was.null = is.null(components$new.infection.proportions)
        if (comp.was.null && setting != FIX)
        {
            if (verbose)
                print("Setting up New Infection Proportions")
            components = do.setup.new.infection.proportions(components)
        }
        
        if (setting == PRODUCE.FROM.UNFIXED ||
            (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
            (setting == FIX && !comp.was.null))
        {
            for (i in 1:length(components$new.infection.proportions))
            {
                jheem = set.new.infection.proportions(jheem,
                                                      proportions=components$new.infection.proportions[[i]],
                                                      time=components$new.infection.proportions.years[i])
            }
        }
    }
    
    # Fixed sizes
    comp.was.null = is.null(components$fix.strata.sizes.check)
    if (comp.was.null && setting != FIX)
        components$fix.strata.sizes.check = 'check'
    
    if (setting == PRODUCE.FROM.UNFIXED ||
        (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
        (setting == FIX && !comp.was.null))
    {
        if (!is.null(components$fix.strata.sizes))
        {
            jheem = set.fixed.size.strata(jheem, fix.age=T, fix.race=T, fix.sex=T)
            for (i in 1:length(components$fix.strata.sizes))
                jheem = set.keep.strata.sizes.constant(jheem,
                                                       components$fix.strata.sizes[i],
                                                       time=components$fix.strata.size.times[i])
        }
    }
    
    
    #-- SET TRACKING --#
    
    comp.was.null = is.null(components$track.mortality.check)
    if (comp.was.null && setting != FIX)
        components$track.mortality.check = 'check'
    
    if (setting == PRODUCE.FROM.UNFIXED ||
        (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
        (setting == FIX && !comp.was.null))
    {
        jheem = set.track.mortality(jheem,
                                    track.hiv.specific.mortality = components$track.hiv.specific.mortality,
                                    track.overall.hiv.positive.mortality = components$track.overall.hiv.positive.mortality,
                                    track.overall.hiv.negative.mortality = components$track.overall.hiv.negative.mortality,
                                    dimensions.hiv.specific.mortality = c('continuum'),
                                    dimensions.overall.hiv.positive.mortality = c('age','race','subpopulation','sex','risk','continuum'),
                                    dimensions.overall.hiv.negative.mortality = c('age','race','subpopulation','sex','risk')
        )
    }
    
    #-- RETURN --#
    if (setting == FIX)
        components$fixed.jheem = jheem
    
    if (verbose)
        print('All done! Returning')
    
    if (setting == PRODUCE.FROM.FIXED || setting == PRODUCE.FROM.UNFIXED)
        jheem
    else
        components
}




##----------------------------##
##-- CLEAR DEPENDENT VALUES --##
##----------------------------##


BASE.COMPONENTS.DEPENDENCIES = list(seed.rate.per.stratum='initial.population',
                    num.to.seed.per.race='initial.population',
                    male.to.female.birth.ratio='birth.proportions',
                    num.to.seed.per.race='initial.population',
                    proportions.msm.of.male=c('initial.population','birth.proportions','sexual.contact.arrays','idu.contact.arrays'),
                    active.idu.prevalence='initial.population',
                    idu.ever.prevalence='initial.population',
                    excess.idu.mortality='general.mortality',
                    raw.mortality.rates='general.mortality',
                    untreated.hiv.mortality='hiv.mortality.rates',
                    untreated.aids.mortality='hiv.mortality.rates',
                    incident.idu='idu.transitions',
                    foreground.idu.incidence='idu.transitions',
                    idu.remission='idu.transitions',
                    foreground.idu.remission='idu.transitions',
                    idu.relapse='idu.transitions',
                    foreground.idu.relapse='idu.transitions',
                    acute.hiv.duration='cd4.transitions',
                    aids.progression.rate='cd4.transitions',
                    cd4.recovery.rate='cd4.transitions',
                    prep.screening.frequency='continuum.transitions',
                    
                    # background/foreground
                    background.testing=c('continuum.transitions','testing.rates.and.times'),
                    foreground.testing=c('continuum.transitions','testing.rates.and.times'),
                    
                    background.suppression=c('suppression.rates.and.times','hiv.mortality.rates','sexual.transmissibilities','idu.transmissibilities'),
                    foreground.suppression=c('suppression.rates.and.times','hiv.mortality.rates','sexual.transmissibilities','idu.transmissibilities'),
                    
                    background.prep=c('prep.rates.and.times','idu.susceptibility','sexual.susceptibility','new.infection.proportions','new.infection.proportions.years'),
                    foreground.prep=c('prep.rates.and.times','idu.susceptibility','sexual.susceptibility','new.infection.proportions','new.infection.proportions.years'),
                    
                    background.needle.exchange = c('needle.exchange.rates.and.times','idu.susceptibility','idu.transitions'),
                    foreground.needle.exchange = c('needle.exchange.rates.and.times','idu.susceptibility','idu.transitions'),
                    
                    # Expanded continuum
                    background.linkage=c('linkage.rates.and.times','continuum.transitions'),
                    foreground.linkage=c('linkage.rates.and.times','continuum.transitions'),
                    
                    background.naive.to.suppressed=c('naive.to.suppressed.rates.and.times','continuum.transitions'),
                    foreground.naive.to.suppressed=c('naive.to.suppressed.rates.and.times','continuum.transitions'),
                    
                    background.naive.to.disengaged=c('naive.to.disengaged.rates.and.times','continuum.transitions'),
                    foreground.naive.to.disengaged=c('naive.to.disengaged.rates.and.times','continuum.transitions'),

                    background.start.art = c('start.art.rates.and.times', 'continuum.transitions'),
                    foreground.start.art = c('start.art.rates.and.times', 'continuum.transitions'),
                    time.to.suppression.on.art = 'continuum.transitions',
                    
                    background.failing.to.suppressed=c('failing.to.suppressed.rates.and.times','continuum.transitions'),
                    foreground.failing.to.suppressed=c('failing.to.suppressed.rates.and.times','continuum.transitions'),
                    
                    background.failing.to.disengaged=c('failing.to.disengaged.rates.and.times','continuum.transitions'),
                    foreground.failing.to.disengaged=c('failing.to.disengaged.rates.and.times','continuum.transitions'),
                    
                    background.suppressed.to.failing=c('suppressed.to.failing.rates.and.times','continuum.transitions'),
                    foreground.suppressed.to.failing=c('suppressed.to.failing.rates.and.times','continuum.transitions'),
                    
                    background.suppressed.to.disengaged=c('suppressed.to.disengaged.rates.and.times','continuum.transitions'),
                    foreground.suppressed.to.disengaged=c('suppressed.to.disengaged.rates.and.times','continuum.transitions'),
                    
                    background.reengagement=c('reengagement.rates.and.times','continuum.transitions'),
                    foreground.reengagement=c('reengagement.rates.and.times','continuum.transitions'),
                    
                    # Other
                    needle.exchange.rr = 'idu.susceptibility',
                    needle.exchange.remission.rate.ratio = 'idu.transitions',
                    prep.rr.heterosexual='sexual.susceptibility',
                    prep.rr.msm=c('sexual.susceptibility','new.infection.proportions'),
                    prep.rr.idu=c('idu.susceptibility','new.infection.proportions'),
                    foreground.rr.prep=c('sexual.susceptibility','idu.susceptibility','new.infection.proportions'),
                    acute.transmissibility.rr=c('sexual.transmissibilities','idu.transmissibilities'),
                    diagnosed.needle.sharing.rr='idu.transmissibilities',
                    diagnosed.het.male.condomless.rr='sexual.transmissibilities',
                    diagnosed.female.condomless.rr='sexual.transmissibilities',
                    diagnosed.msm.condomless.rr='sexual.transmissibilities',
                    idu.transmissibility.rr.by.race='idu.transmissibilities',
                    sexual.transmissibility.rr.by.race='sexual.transmissibilities',
                    male.to.male.sexual.transmission='sexual.contact.arrays',
                    male.to.female.sexual.transmission='sexual.contact.arrays',
                    female.to.male.sexual.transmission='sexual.contact.arrays',
                    sexual.transmission='sexual.contact.arrays',
                    foreground.sexual.transmission='sexual.contact.arrays',
                    idu.transmission='idu.contact.arrays',
                    foreground.idu.transmission='idu.contact.arrays',
                    susceptibility=c('sexual.susceptibility','idu.susceptibility'),
                    fertility='fertility.check',
                    global.sexual.transmission.rates='global.sexual.transmission.rates',
                    global.idu.transmission.rates='global.idu.transmission.rates',
                    fix.strata.sizes='fix.strata.sizes.check',
                    track.mortality='track.mortality.check',
                    aging=c('aging.rates.hiv.positive')
)

enumerate.components.dependencies <- function(components)
{
    # The base dependencies defined for all components
    components$DEPENDENCIES = BASE.COMPONENTS.DEPENDENCIES
    
    # Add in specific dependencies incurred by transition mapping
    
    transition.mapping = get.components.transition.mapping(components)
    for (transition.element.name in names(transition.mapping$elements.to.subgroups.and.dimensions))
    {
        # names of what is dependent on
        background.name = paste0('background.', transition.element.name)
        foreground.name = paste0('foreground.', transition.element.name)
        
        # names of components that are dependent
        rates.name = paste0(transition.element.name, ".rates.and.times")
        subgroups.and.dimensions = transition.mapping$elements.to.subgroups.and.dimensions[[transition.element.name]]
        dimension.transitions.names = unique(sapply(1:length(subgroups.and.dimensions$subgroup), function(i){
            get.transition.component.names(dimension = subgroups.and.dimensions$dimension[i],
                                           subgroup = subgroups.and.dimensions$subgroup[i])$name
        }))
        
        # add them
        dependencies.to.add = c(rates.name, dimension.transitions.names)
        
        components$DEPENDENCIES[[background.name]] = union(components$DEPENDENCIES[[background.name]],
                                                           dependencies.to.add)
        components$DEPENDENCIES[[foreground.name]] = union(components$DEPENDENCIES[[foreground.name]],
                                                           dependencies.to.add)
        components$DEPENDENCIES[[transition.element.name]] = union(components$DEPENDENCIES[[transition.element.name]],
                                                           dependencies.to.add)
    }
    
    # Add in dependencies that affect all values
    components$ALL.DEPENDENT.NAMES = unique(unlist(components$DEPENDENCIES)) 
    
    components$DEPENDENCIES = c(components$DEPENDENCIES,
                                list(model.idu=components$ALL.DEPENDENT.NAMES,
                                     model.hiv.transmission=components$ALL.DEPENDENT.NAMES,
                                     model.prep=components$ALL.DEPENDENT.NAMES,
                                     populations=components$ALL.DEPENDENT.NAMES))
    
    # Return
    components
}

clear.dependent.values <- function(components,
                                   dependent.on)
{
    if (is.null(components$DEPENDENCIES))
        components = enumerate.components.dependencies(components)
    
    #An internal check
    missing.from.all = sapply(unlist(components$DEPENDENCIES), function(dep){
        !any(components$ALL.DEPENDENT.NAMES==dep)
    })
    if (any(missing.from.all))
        stop(paste0("The following are missing from 'ALL.DEPENDENT.NAMES: ",
                    paste0("'", unique(unlist(components$DEPENDENCIES)[missing.from.all]), "'", collapse=', ')))
    
    missing.dependency = sapply(dependent.on, function(dep){
        !any(names(components$DEPENDENCIES)==dep) && !any(components$ALL.DEPENDENT.NAMES==dep)
    })
    
    if (any(missing.dependency))
        stop("The following are not valid parameters for DEPENDENCIES: ",
             paste0("'", dependent.on[missing.dependency], "'", collapse=', '))
    
    # Track (if we are tracking)
    if (!is.null(components$tracked.dependencies) && components$tracked.dependencies$track)
        components$tracked.dependencies$dependencies = union(components$tracked.dependencies$dependencies,
                                                             dependent.on)
    
    # Clear the dependendent items
    for (d in dependent.on)
    {
        #        if (any(ALL.DEPENDENT.NAMES==d))
        #            d.dep = d
        #        else
        d.dep = components$DEPENDENCIES[[d]]
        
        for (elem in d.dep)
        {
            if (components$fixed && !is.null(components[[elem]]))
                stop(paste0("The JHEEM components have been fixed. Cannot reset '", d, "'"))
            
            components[[elem]] = NULL
        }
    }
    
    components
}


#-- Tracking Dependencies --#
# (functions provided so that we can know what parameters/settings we are going to change
#  before we produce a fixed components object)
set.track.cleared.dependencies <- function(components,
                                           track)
{
    if (is.null(components$tracked.dependencies))
        components$tracked.dependencies = list()

    # indicate our tracking status
    components$tracked.dependencies$track = track
    
    # clear the tracked list
    components$tracked.dependencies$dependencies = character()
    
    # Return
    components
}

get.tracked.cleared.dependencies <- function(components)
{
    if (is.null(components$tracked.dependencies) || !components$tracked.dependencies$track)
        stop("The components object is not currently tracking cleared dependencies")
    
    components$tracked.dependencies$dependencies
}

##----------------------------------##
##--         JIT HELPERS          --##
##-- To Crunch Stuff Just-in-Time --##
##----------------------------------##


##-- THE JHEEM SKELETON --##
do.setup.jheem.skeleton <- function(components)
{
    settings = get.components.settings(components)
    
    if (components$model.idu)
        risk.strata = settings$RISK_STRATA
    else
        risk.strata = NULL
    
    if (components$model.hiv.transmission)
    {
        if (components$model.prep)
        {
            cd4.strata = settings$CD4_STRATA
            continuum = settings$CONTINUUM_OF_CARE
            first.diagnosed = settings$FIRST_DIAGNOSED_STATE
            undiagnosed = settings$UNDIAGNOSED_STATES
        }
        else
        {
            cd4.strata = settings$CD4_STRATA
            continuum = setdiff(settings$CONTINUUM_OF_CARE, settings$UNDIAGNOSED_FROM_PREP)
            first.diagnosed = settings$FIRST_DIAGNOSED_STATE
            undiagnosed = setdiff(settings$UNDIAGNOSED_STATES, settings$UNDIAGNOSED_FROM_PREP)
        }
        
        all.diagnosed = settings$DIAGNOSED_STATES
    }
    else
        cd4.strata = continuum = first.diagnosed = undiagnosed = all.diagnosed = NULL
    
    jheem = initialize.jheem(version = get.components.version(components),
                             age.cutoffs = settings$AGE_CUTOFFS,
                             race.strata = settings$RACES,
                             subpopulations = settings$SUBPOPULATIONS,
                             sex.strata = settings$SEXES,
                             risk.strata = risk.strata,
                             nonhiv.subsets = NULL,
                             continuum.of.care.states = continuum,
                             cd4.strata = cd4.strata,
                             hiv.subsets = NULL,
                             transmission.route.names = c('sexual','idu'),
                             first.diagnosed.hiv.continuum.states = first.diagnosed,
                             all.diagnosed.hiv.continuum.states = all.diagnosed,
                             new.diagnoses.keep.dimensions=c('age','race','subpopulation','sex','risk','cd4'))
    
    #by default, minimal tracking
    jheem = set.track.incidence.dimensions(jheem, dimensions=c('age','race','subpopulation','sex','risk'))
    
    components$jheem = jheem
    
    
    
    components
}

##-- INITIAL POPULATION --##
do.setup.initial.population <- function(components)
{
    settings = get.components.settings(components)
    
    #-- Check Required Parameters --#
    
    if (is.null(components$proportions.msm.of.male))
        stop("MSM proportions have not been set up in the components to make the JHEEM")
    #stop("MSM proportions must be set before setting initial population")
    
    if (components$model.idu && (is.null(components$active.idu.prevalence) || is.null(components$idu.ever.prevalence)))
        stop("The JHEEM is set to model IDU, but IDU proportions have not been set up in the components to make the JHEEM")
    #stop("If modeling IDU, IDU proportions must be set before setting initial population")
    
    if (components$model.hiv.transmission && is.null(components$seed.rate.per.stratum) && is.null(components$num.to.seed.per.race))
        stop("The parameters for seeding the initial population with HIV (seed.rate.per.stratum and num.to.seed.per.race) have not been set")
    
    #-- Stratify MSM --#
    population = stratify.males.to.msm.by.race(components$populations$collapsed.races,
                                               components$proportions.msm.of.male)
    
    #-- Stratify IDU --#
    
    if (components$model.idu)
    {
        population = stratify.population.idu(population,
                                             active.idu.prevalence=components$active.idu.prevalence,
                                             idu.ever.prevalence=components$idu.ever.prevalence)
        
    }
    
    
    #-- Seed HIV --#
    
    if (!components$model.hiv.transmission)
    {
        components$initial.population = list(hiv.negative=population, hiv.positive=0)
    }
    else if (components$seed.one.other.msm)
    {
        components$initial.population = get.seeded.initial.populations(components$jheem, population,
                                                                       cases.per=components$num.to.seed.per.race,
                                                                       seed.to.races = settings$RACES,
                                                                       #                                                                       seed.to.races = 'other',
                                                                       seed.to.sexes = 'msm')
    }
    else if (!is.null(components$seed.rate.per.stratum))
    {
        population = expand.population.to.general(components$jheem, population)
        to.seed = expand.population.to.general(components$jheem, components$seed.rate.per.stratum) * population
        
        hiv.positive = get.hiv.positive.population.skeleton(components$jheem)
        hiv.positive[,,,,,1,1,1] = to.seed
        #        access(hiv.positive, cd4=1, continuum=1, hiv.subset=1) = to.seed
        
        components$initial.population = list(hiv.negative=population-to.seed,
                                             hiv.positive=hiv.positive)
    }
    else
    {
        components$initial.population = get.seeded.initial.populations(components$jheem, population,
                                                                       cases.per=components$num.to.seed.per.race,
                                                                       seed.to.races=components$jheem$race)
    }
    
    #-- Return --#
    components
}


do.setup.birth.proportions <- function(components)
{
    #-- Check parameters --#
    
    #if (is.null(components$settings))
     #   stop("components has not been initialized as JHEEM components")
    
    if (is.null(components$proportions.msm.of.male))
        stop("MSM proportions have not been set up in the components to make the JHEEM")
    #stop("MSM proportions must be set before setting birth proportions")
    
    if (is.null(components$male.to.female.birth.ratio))
        stop("Parameters for birth proportions (male.to.female.birth.ratio) have not been set up in the components to make the JHEEM")
    
    
    #-- Set up the array --#
    
    proportion.male = components$male.to.female.birth.ratio / (1+components$male.to.female.birth.ratio)
    
    components$birth.proportions = get.birth.proportions.to.hiv.negative.skeleton(components$jheem)
    
    #    access(components$birth.proportions, sex.to='female', risk.to='never_IDU') = 1-proportion.male
    components$birth.proportions[,,,'female','never_IDU',] = 1-proportion.male
    
    for (race in components$jheem$race)
    {
        if (components$model.idu)
            risk.to = 'never_IDU'
        else
            risk.to = NULL
        
        #        access(components$birth.proportions, sex.to='heterosexual_male', risk.to=risk.to, race.from=race) = as.numeric(proportion.male * (1-components$proportions.msm.of.male[race]))
        components$birth.proportions[race,,,'heterosexual_male',risk.to,] = as.numeric(proportion.male * (1-components$proportions.msm.of.male[race]))
        #        access(components$birth.proportions, sex.to='msm', risk.to=risk.to, race.from=race) = as.numeric(proportion.male * components$proportions.msm.of.male[race])
        components$birth.proportions[race,,,'msm',risk.to,] = as.numeric(proportion.male * components$proportions.msm.of.male[race])
    }
    
    components
}

do.setup.general.mortality <- function(components)
{
    #-- Check parameters --#
    
    if (is.null(components$raw.mortality.rates))
        stop("Mortality rates have not been set up in the components to make the JHEEM")
    
    if (components$model.idu && is.null(components$excess.idu.mortality))
        stop("The JHEEM is set to model IDU, but excess IDU mortality has not been set up in the components to make the JHEEM")
    
    
    #-- Set up the array --#
    
    mortality.numerators = components$raw.mortality.rates * components$populations$by.county.all.races
    mortality.rates.all.counties = colSums(mortality.numerators) / components$populations$all.races
    mortality.rates = collapse.races.for.rates(components$populations$all.races, mortality.rates.all.counties,
                                               races=get.components.settings(components)$RACES)
    mortality.rates = stratify.males.by.orientation(mortality.rates, msm.multiplier = 1, heterosexual.multiplier = 1)
    
    components$general.mortality = expand.population.to.general(components$jheem, mortality.rates)
    
    if (components$model.idu)
    {
        #        access(components$general.mortality, risk='active_IDU') = access(components$general.mortality, risk='active_IDU') +
        #            access(expand.population.to.general(components$jheem, components$excess.idu.mortality), risk='active_IDU')
        components$general.mortality[,,,,'active_IDU'] = components$general.mortality[,,,,'active_IDU'] +
            expand.population.to.general(components$jheem, components$excess.idu.mortality)[,,,,'active_IDU']
    }
    
    #-- Return --#
    components
}


do.setup.hiv.mortality <- function(components)
{
    settings = get.components.settings(components)
    
    if (components$model.hiv.transmission)
    {
        mortality.rates = get.hiv.mortality.rate.arrays(components)
        
        components = do.calculate.suppression(components)
        suppression = calculate.suppression(components)
        
        mortality.and.suppression = merge.rates(rates1=mortality.rates$rates,
                                                times1=mortality.rates$times,
                                                rates2=suppression$rates,
                                                times2=suppression$times)
        
        components$hiv.mortality.rates = lapply(1:length(mortality.and.suppression$times), function(i){
            
            hiv.mortality = mortality.and.suppression$rates1[[i]]
            hiv.mortality[,,,,,settings$DIAGNOSED_STATES,,] = hiv.mortality[,,,,,settings$DIAGNOSED_STATES,,] *
                (1-mortality.and.suppression$rates2[[i]][,,,,,settings$DIAGNOSED_STATES,,])
            
            hiv.mortality
        })
        
        components$hiv.mortality.years = mortality.and.suppression$times
    }
    else
    {
        components$hiv.mortality.rates = 0
        components$hiv.mortality.years = -Inf
    }
    components
}

get.hiv.mortality.rate.arrays <- function(components)
{
    hiv.mortality.array.skeleton = get.hiv.positive.population.skeleton(components$jheem)
    hiv.mortality.mapping = get.hiv.positive.population.skeleton(components$jheem, value='')
    hiv.mortality.mapping[] = as.character(NA)
    
    rates.and.times.list = list()
    
    for (race in components$jheem$race)
    {
        for (age.index in 1:length(components$jheem$age$labels))
        {
            age = paste0('age', age.index)
            
            msm.rates = calculate.changing.trates.and.times(components$msm.hiv.mortality.rates[[race]][[age]])
            
            heterosexual.male.rates = heterosexual.female.rates =
                calculate.changing.trates.and.times(components$heterosexual.hiv.mortality.rates[[race]][[age]])
            
            idu.rates = calculate.changing.trates.and.times(components$idu.hiv.mortality.rates[[race]][[age]])
            
            msm.name = paste0(race, '.', age, '.msm')
            idu.name = paste0(race, '.', age, '.idu')
            het.male.name = paste0(race, '.', age, '.het.male')
            het.female.name = paste0(race, '.', age, '.female')
            
            to.add = list(msm.rates,
                          idu.rates,
                          heterosexual.male.rates,
                          heterosexual.female.rates)
            names(to.add) = c(msm.name, idu.name, het.male.name, het.female.name)
            rates.and.times.list = c(rates.and.times.list,
                                     to.add)
            
            hiv.mortality.mapping[age.index,race,,'msm',,,,] = msm.name
            hiv.mortality.mapping[age.index,race,,,c('active_IDU','IDU_in_remission'),,,] = idu.name
            hiv.mortality.mapping[age.index,race,,'heterosexual_male',,,,] = het.male.name
            hiv.mortality.mapping[age.index,race,,'female',,,,] = het.female.name
        }
    }
    
    hiv.mortality.arrays = array.merge.rates(skeleton.array = hiv.mortality.array.skeleton,
                                             rates.and.times.list = rates.and.times.list,
                                             array.indices.into.list = hiv.mortality.mapping)
    
    hiv.mortality.arrays
}

##-- TRANSITIONS --##


do.setup.idu.transitions <- function(components)
{
    if (components$model.idu)
    {
        if (is.null(components$incident.idu) || is.null(components$idu.remission) || is.null(components$idu.relapse))
            stop("IDU transition parameters have not been set in the components for the JHEEM")
        
        #setting up the background
        if (length(components$years.for.idu.transitions)==2)
        {
            background.idu.transition.years = components$years.for.idu.transitions[1]:components$end.year.idu.transition
            
            background.idu.incidence = calculate.change.ratios.logistic.array(r0.arr=components$incident.idu[[1]],
                                                                        r1.arr=components$incident.idu[[2]],
                                                                        times=background.idu.transition.years,
                                                                        t0=components$years.for.idu.transitions[1],
                                                                        t1=components$years.for.idu.transitions[2],
                                                                        fraction.of.asymptote.after.end=components$fraction.idu.transitions.change.after.last.year,
                                                                        fraction.of.asymptote.before.start=0.025)
            background.idu.remission = calculate.change.ratios.logistic.array(r0.arr=components$idu.remission[[1]],
                                                                         r1.arr=components$idu.remission[[2]],
                                                                         times=background.idu.transition.years,
                                                                         t0=components$years.for.idu.transitions[1],
                                                                         t1=components$years.for.idu.transitions[2],
                                                                         fraction.of.asymptote.after.end=components$fraction.idu.transitions.change.after.last.year,
                                                                         fraction.of.asymptote.before.start=0.025)
            background.idu.relapse = calculate.change.ratios.logistic.array(r0.arr=components$idu.relapse[[1]],
                                                                       r1.arr=components$idu.relapse[[2]],
                                                                       times=background.idu.transition.years,
                                                                       t0=components$years.for.idu.transitions[1],
                                                                       t1=components$years.for.idu.transitions[2],
                                                                       fraction.of.asymptote.after.end=components$fraction.idu.transitions.change.after.last.year,
                                                                       fraction.of.asymptote.before.start=0.025)
        }
        else
        {
            background.idu.incidence = components$incident.idu
            background.idu.remission = components$idu.remission
            background.idu.relapse = components$idu.relapse
           
            background.idu.transition.years = components$years.for.idu.transitions
        }
        
        # Add foreground
        idu.incidence = do.get.rates.from.background.and.foreground(background.rates = background.idu.incidence,
                                                                    background.times = background.idu.transition.years,
                                                                    background.data.type = 'rate',
                                                                    foreground = components$foreground.idu.incidence,
                                                                    max.background.time = Inf)
        
        
        idu.remission = do.get.rates.from.background.and.foreground(background.rates = background.idu.remission,
                                                                    background.times = background.idu.transition.years,
                                                                    background.data.type = 'rate',
                                                                    foreground = components$foreground.idu.remission,
                                                                    max.background.time = Inf)
        
        idu.relapse = do.get.rates.from.background.and.foreground(background.rates = background.idu.relapse,
                                                                  background.times = background.idu.transition.years,
                                                                  background.data.type = 'rate',
                                                                  foreground = components$foreground.idu.relapse,
                                                                  max.background.time = Inf)
        
 
        # I don't know why, but somehow this was getting set to an empty list
        # So just have to recalculate every time - it's cheap
#        if (is.null(components$needle.exchange.rates.and.times))
            components = do.calculate.needle.exchange.coverage(components)
        needle.exchange = calculate.needle.exchange.coverage(components)
        
        
        # Merge all the rates by time, with needle exchange        
    #    components$interpolated.idu.transition.years = sort(unique(union(needle.exchange$times,
     #                                                                    union(idu.relapse$times,
      #                                                                         union(idu.incidence$times, 
       #                                                                              idu.remission$times)
        #                                                                       ))))
        components$interpolated.idu.transition.years = union_sorted_vectors(list(needle.exchange$times,
                                                                                 idu.relapse$times,
                                                                                 idu.incidence$times, 
                                                                                 idu.remission$times))
        
        
        if (!setequal(components$interpolated.idu.transition.years, idu.incidence$times))
            idu.incidence$rates = interpolate.parameters(values=idu.incidence$rates,
                                                         value.times=idu.incidence$times,
                                                         desired.times=components$interpolated.idu.transition.years)
        
        if (!setequal(components$interpolated.idu.transition.years, idu.remission$times))
            idu.remission$rates = interpolate.parameters(values=idu.remission$rates,
                                                         value.times=idu.remission$times,
                                                         desired.times=components$interpolated.idu.transition.years)
        
        if (!setequal(components$interpolated.idu.transition.years, idu.relapse$times))
            idu.relapse$rates = interpolate.parameters(values=idu.relapse$rates,
                                                       value.times=idu.relapse$times,
                                                       desired.times=components$interpolated.idu.transition.years)
        
        if (!setequal(components$interpolated.idu.transition.years, needle.exchange$times))
            needle.exchange$rates = interpolate.parameters(values=needle.exchange$rates,
                                                       value.times=needle.exchange$times,
                                                       desired.times=components$interpolated.idu.transition.years)
                    

        # Put it together into transitions array
        idu.transitions = get.general.transition.array.skeleton(components$jheem, transition.dimension='risk')
        components$idu.transitions = lapply(1:length(components$interpolated.idu.transition.years), function(i){
            idu.transitions[,,,,'never_IDU','active_IDU'] = idu.incidence$rates[[i]]
            
            idu.transitions[,,,,'active_IDU','IDU_in_remission'] = idu.remission$rates[[i]] * 
                (1 + needle.exchange$rates[[i]] * (components$needle.exchange.remission.rate.ratio - 1) )
            
            idu.transitions[,,,,'IDU_in_remission', 'active_IDU'] = idu.relapse$rates[[i]] 
            
            idu.transitions
        })
    }
    else
    {
        components$years.for.idu.transitions = -Inf
        components$idu.transitions = list(0)
    }
    
    components
}

do.setup.cd4.transitions <- function(components)
{
    components$cd4.transitions = get.hiv.positive.transition.array.skeleton(components$jheem,
                                                                            transition.dimension='cd4')
    if (components$model.hiv.transmission)
    {
        components$cd4.transitions[,,,,,,'acute',,'chronic'] = 1 / components$acute.hiv.duration
    }
    
    components
}

do.setup.continuum.transitions <- function(components)
{
    
        return (do.setup.transitions(components,
                                     subgroup='hiv.positive',
                                     dimension='continuum'))
    
    #this (below) was the old way of doing it - keeping it here just in case for now
    
    print('doing it the old way')
    settings = get.components.settings(components)
    #-- HIV TRANSITIONS --#
    
    if (components$model.hiv.transmission)
    {
        base.transitions = get.hiv.positive.transition.array.skeleton(components$jheem,
                                                                      transition.dimension = 'continuum')
        
        if (components$model.prep)
        {
            base.transitions[,,,,,'undiagnosed_from_prep',,,settings$FIRST_DIAGNOSED_STATE] = 1 / components$prep.screening.frequency
            base.transitions[,,,,,'undiagnosed_from_prep',,,'undiagnosed'] = -log(1-components$prep.persistence)
        }
        
        #-- HIV Testing --#
        
        components = do.calculate.testing.rates(components) #this caches them
        testing.rates = calculate.testing.rates(components)
        
        
        if (get.components.version(components)=='collapsed_1.0')
        {
            components$continuum.transitions = lapply(1:length(testing.rates$rates), function(i){
                continuum.transitions.for.year = base.transitions
                continuum.transitions.for.year[,,,,,'undiagnosed',,,settings$FIRST_DIAGNOSED_STATE] = testing.rates$rates[[i]][,,,,,'undiagnosed',,]
                continuum.transitions.for.year
            })
            components$continuum.transition.years = testing.rates$times
        }
        else
        {
        ##-- CALCULATE THE RATES --##
            
            # unengaged -> engaged/naive
            # unengaged -> disengaged
            components = OLD.do.calculate.rates(components, 'linkage')
            linkage.proportions = calculate.rates(components, 'linkage')
            
            linkage.rates = list(rates=lapply(linkage.proportions$rates, function(p){
                p / components$time.to.link.vs.disengage
            }),
            times=linkage.proportions$times)
            
            non.linkage.rates = list(rates=lapply(linkage.proportions$rates, function(p){
                (1-p) / components$time.to.link.vs.disengage
            }),
            times=linkage.proportions$times)
            
            
            # engaged/naive -> engaged/failing
            # engaged/naive -> engaged/suppressed
            # engaged/naive -> disengaged
            
            components = do.calculate.start.art.proportions(components)
            start.art.proportions = calculate.start.art.proportions(components)
            
            components = OLD.do.calculate.rates(components, 'naive.to.suppressed')
            new.art.suppressed.proportions = calculate.rates(components, 'naive.to.suppressed')
            
            start.art.naive = merge.rates(rates1=start.art.proportions$rates,
                                          times1=start.art.proportions$times,
                                          rates2=new.art.suppressed.proportions$rates,
                                          times2=new.art.suppressed.proportions$times)
            
            time.to.suppression.on.art = calculate.time.to.suppression.on.art(components,
                                                                              years=start.art.naive$times)
            naive.to.suppressed.rates = list(
                rates=lapply(1:length(start.art.naive$rates1), function(i){
                    start.art.rate = -log(1-start.art.naive$rates1[[i]])
                    
                    start.art.naive$rates2[[i]] / #the proportion suppressed once starting art
                        (time.to.suppression.on.art[i] + 1/start.art.rate) # the time to suppression (or not)
                }),
                times=start.art.naive$times
            )
          
            naive.to.failing.rates = list(
                rates=lapply(1:length(start.art.naive$rates1), function(i){
                    start.art.rate = -log(1-start.art.naive$rates1[[i]])
                    
                    (1-start.art.naive$rates2[[i]]) / #the proportion NOT suppressed once starting art
                        (time.to.suppression.on.art[i] + 1/start.art.rate) # the time to suppression (or not)
                }),
                times=start.art.naive$times
            )
            
            components = OLD.do.calculate.rates(components, 'naive.to.disengaged')
            naive.to.disengaged.rates = calculate.rates(components, 'naive.to.disengaged')
            
            
            # engaged/failing -> engaged/suppressed
            # engaged/failing -> disengaged
            components = OLD.do.calculate.rates(components, 'failing.to.suppressed')
            failing.to.suppressed.rates = calculate.rates(components, 'failing.to.suppressed')
            
            components = OLD.do.calculate.rates(components, 'failing.to.disengaged')
            failing.to.disengaged.rates = calculate.rates(components, 'failing.to.disengaged')
            
            
            # engaged/suppressed -> engaged/failing
            # engaged/suppressed -> disengaged
            components = OLD.do.calculate.rates(components, 'suppressed.to.failing')
            suppressed.to.failing.rates = calculate.rates(components, 'suppressed.to.failing')
            
            components = OLD.do.calculate.rates(components, 'suppressed.to.disengaged')
            suppressed.to.disengaged.rates = calculate.rates(components, 'suppressed.to.disengaged')

            
            # disengaged -> engaged/failing
            components = OLD.do.calculate.rates(components, 'reengagement')
            reengagement.rates = calculate.rates(components, 'reengagement')
            
        ##-- INTERPOLATE THE RATES --##
            
            all.times = union_sorted_vectors(list(testing.rates$times,
                                                  linkage.rates$times,
                                                  non.linkage.rates$times,
                                                  naive.to.failing.rates$times,
                                                  naive.to.suppressed.rates$times,
                                                  naive.to.disengaged.rates$times,
                                                  failing.to.suppressed.rates$times,
                                                  failing.to.disengaged.rates$times,
                                                  suppressed.to.failing.rates$times,
                                                  suppressed.to.disengaged.rates$times,
                                                  reengagement.rates$times))
            
            
            #-- From Undiagnosed --#
            testing.rates$rates = interpolate.parameters(values = testing.rates$rates,
                                                         value.times = testing.rates$times,
                                                         desired.times = all.times)
            
            #-- From Diagnosed, Unengaged --#
            linkage.rates$rates = interpolate.parameters(values = linkage.rates$rates,
                                                         value.times = linkage.rates$times,
                                                         desired.times = all.times)
            
            non.linkage.rates$rates = interpolate.parameters(values = non.linkage.rates$rates,
                                                             value.times = non.linkage.rates$times,
                                                             desired.times = all.times)
            
            #-- From Naive --#
            naive.to.failing.rates$rates = interpolate.parameters(values = naive.to.failing.rates$rates,
                                                                  value.times = naive.to.failing.rates$times,
                                                                  desired.times = all.times)
            
            naive.to.suppressed.rates$rates = interpolate.parameters(values = naive.to.suppressed.rates$rates,
                                                                     value.times = naive.to.suppressed.rates$times,
                                                                     desired.times = all.times)
            
            naive.to.disengaged.rates$rates = interpolate.parameters(values = naive.to.disengaged.rates$rates,
                                                                     value.times = naive.to.disengaged.rates$times,
                                                                     desired.times = all.times)
            
            #-- From Failing --#
            failing.to.suppressed.rates$rates = interpolate.parameters(values = failing.to.suppressed.rates$rates,
                                                                       value.times = failing.to.suppressed.rates$times,
                                                                       desired.times = all.times)
            
            failing.to.disengaged.rates$rates = interpolate.parameters(values = failing.to.disengaged.rates$rates,
                                                                       value.times = failing.to.disengaged.rates$times,
                                                                       desired.times = all.times)
            
            
            #-- From Suppressed --#
            suppressed.to.failing.rates$rates = interpolate.parameters(values = suppressed.to.failing.rates$rates,
                                                                       value.times = suppressed.to.failing.rates$times,
                                                                       desired.times = all.times)
            
            suppressed.to.disengaged.rates$rates = interpolate.parameters(values = suppressed.to.disengaged.rates$rates,
                                                                          value.times = suppressed.to.disengaged.rates$times,
                                                                          desired.times = all.times)
            
            #-- From Disengaged --#
            reengagement.rates$rates = interpolate.parameters(values = reengagement.rates$rates,
                                                              value.times = reengagement.rates$times,
                                                              desired.times = all.times)
            
            #-- PLUG IT INTO TRANSITIONS --#
            
            components$continuum.transitions = lapply(1:length(all.times), function(i){
                continuum.transitions.for.year = base.transitions
                
                
         #- FROM UNDIAGNOSED --#       
                
                # undiagnosed --> diagnosed-unengaged
                continuum.transitions.for.year[,,,,,'undiagnosed',,,settings$FIRST_DIAGNOSED_STATE] =
                    testing.rates$rates[[i]][,,,,,'undiagnosed',,]
                
         #-- FROM DIAGNOSED, UNENGAGED --#
                # unengaged -> engaged/naive
                continuum.transitions.for.year[,,,,,settings$FIRST_DIAGNOSED_STATE,,,'engaged_unsuppressed_naive'] =
                    linkage.rates$rates[[i]][,,,,,settings$FIRST_DIAGNOSED_STATE,,]
                
                # unengaged -> disengaged
                continuum.transitions.for.year[,,,,,settings$FIRST_DIAGNOSED_STATE,,,'disengaged_naive'] =
                    non.linkage.rates$rates[[i]][,,,,,settings$FIRST_DIAGNOSED_STATE,,]
                
         #-- FROM ENGAGED, NAIVE --#
                # engaged/naive -> engaged/failing
                continuum.transitions.for.year[,,,,,'engaged_unsuppressed_naive',,,'engaged_unsuppressed_failing'] =
                    naive.to.failing.rates$rates[[i]][,,,,,'engaged_unsuppressed_naive',,]
                
                # engaged/naive -> engaged/suppressed
                continuum.transitions.for.year[,,,,,'engaged_unsuppressed_naive',,,'engaged_suppressed'] =
                    naive.to.suppressed.rates$rates[[i]][,,,,,'engaged_unsuppressed_naive',,]
                
                # engaged/naive -> disengaged
                continuum.transitions.for.year[,,,,,'engaged_unsuppressed_naive',,,'disengaged_naive'] =
                    naive.to.disengaged.rates$rates[[i]][,,,,,'engaged_unsuppressed_naive',,]
    
         #-- FROM ENGAGED, FAILING --#
                # engaged/failing -> engaged/suppressed
                continuum.transitions.for.year[,,,,,'engaged_unsuppressed_failing',,,'engaged_suppressed'] =
                    failing.to.suppressed.rates$rates[[i]][,,,,,'engaged_unsuppressed_failing',,]
    
                # engaged/failing -> disengaged
                continuum.transitions.for.year[,,,,,'engaged_unsuppressed_failing',,,'disengaged_failing'] =
                   failing.to.disengaged.rates$rates[[i]][,,,,,'engaged_unsuppressed_failing',,]
    
         #-- FROM ENGAGED, SUPPRESSED --#
                # engaged/suppressed -> engaged/failing
                continuum.transitions.for.year[,,,,,'engaged_suppressed',,,'engaged_unsuppressed_failing'] =
                    suppressed.to.failing.rates$rates[[i]][,,,,,'engaged_suppressed',,]
                
                # engaged/suppressed -> disengaged
                continuum.transitions.for.year[,,,,,'engaged_suppressed',,,'disengaged_failing'] =
                    suppressed.to.disengaged.rates$rates[[i]][,,,,,'engaged_suppressed',,]
                
         #-- FROM DISENGAGED --#       
                
                # disengaged -> engaged/unsuppressed
                continuum.transitions.for.year[,,,,,'disengaged_naive',,,'engaged_unsuppressed_naive'] =
                    reengagement.rates$rates[[i]][,,,,,'disengaged_naive',,]
                
                
                # disengaged -> engaged/unsuppressed
                continuum.transitions.for.year[,,,,,'disengaged_failing',,,'engaged_unsuppressed_failing'] =
                    reengagement.rates$rates[[i]][,,,,,'disengaged_failing',,]
                
                
        #-- CHECK and then RETURN --#
                if (any(is.na(continuum.transitions.for.year)))
                    stop("NA values produced in continuum transitions array")
                continuum.transitions.for.year
            })
            components$continuum.transition.years = all.times
        }
    }
    else
    {
        components$continuum.transitions = list(0)#list(get.transition.array.skeleton(components$jheem))
        components$continuum.transition.years=-Inf
    }
    
    components
}


##--------------------------------------------------##
##--------------------------------------------------##
##-- RATES and TIMES (for suppression, prep, etc) --##
##--------------------------------------------------##
##--------------------------------------------------##


trim.rates.and.times <- function(rates.and.times,
                                 keep.times)
{
    if (length(rates.and.times$times)>0)
    {
        mask = sapply(rates.and.times$times, function(time){
            any(time==keep.times)
        })
        rates.and.times$times = rates.and.times$times[mask]
        rates.and.times$rates = rates.and.times$rates[mask]
    }
    
    rates.and.times
}


#-- Suppression --#
calculate.suppression <- function(components)
{
    if (is.null(components$suppression.rates.and.times))
        components = do.calculate.suppression(components)
    components$suppression.rates.and.times
}

do.calculate.suppression <- function(components)
{
    settings = get.components.settings(components)
    
    if (get.components.version(components)=='collapsed_1.0')
    {
        #Pull background suppression proportions from logistic model
        if (is(components$background.suppression$model, 'model') &&
            !is.null(components$background.suppression$alphas))
        {
            background.values = project.from.model(model = components$background.suppression$model,
                                                   years = components$background.suppression$years,
                                                   alphas = components$background.suppression$alphas,
                                                   future.slope = components$background.suppression[['future.slope']],
                                                   future.slope.after.year = components$background.suppression$future.slope.after.year,
                                                   dim.names = settings$dimension.names.by.subgroup$hiv.positive)
        }
        else
        {
            if (is.null(components$background.suppression$future.slope.or))
            {
                if (is.null(components$background.suppression[['future.slope']]))
                    stop(paste0("Background has not been fully set up for 'suppression' (future.slope is missing)"))
                else
                    future.slope = components$background.suppression[['future.slope']]
            }
            else
                future.slope = log(components$background.suppression$future.slope.or)
   
            background.suppression = get.background.proportions(base.model = components$background.suppression$model,
                                                                years = components$background.suppression$years,
                                                                additional.intercepts = log(components$background.suppression$additional.intercept.ors),
                                                                additional.slopes = log(components$background.suppression$additional.slope.ors),
                                                                future.slope = future.slope,
                                                                future.slope.after.year = components$background.suppression$future.slope.after.year,
                                                                idu.applies.to.in.remission = T,
                                                                idu.applies.to.msm.idu=T,
                                                                msm.applies.to.msm.idu=T,
                                                                jheem=components$jheem)
        }
        
        #Add in zero suppression
        
        background.suppression.years = c(components$background.suppression$zero.suppression.year, 
                                         components$background.suppression$years)
        background.suppression = c(list(0 * background.suppression[[1]]), background.suppression)
        
        undiagnosed.states = setdiff(dimnames(background.suppression[[1]])[['continuum']], 'diagnosed')
        background.suppression = lapply(background.suppression, function(r){
            r[,,,,,undiagnosed.states,,] = 0
            r
        })
        
        #Overlay foreground suppression
        suppression = do.get.rates.from.background.and.foreground(background.rates = background.suppression,
                                                                  background.times = background.suppression.years,
                                                                  background.data.type = 'proportion',
                                                                  foreground = components$foreground.suppression,
                                                                  max.background.time = components$background.change.to.years$suppression)
        
        
        components$suppression.rates.and.times = suppression
        components
    }
    else
    {
        suppression.array = get.hiv.positive.population.skeleton(components$jheem, value=0)
        suppression.array[,,,,,settings$SUPPRESSED_STATES,,] = 1
        
        components$suppression.rates.and.times = list(
            rates = list(suppression.array),
            times = 2000
        )
        
        components
    }
}

#-- Testing --#
calculate.testing.rates <- function(components)
{
    if (is.null(components$testing.rates.and.times))
        components = do.calculate.testing.rates(components)
    components$testing.rates.and.times
}

do.calculate.testing.rates <- function(components)
{
    #Pull background testing proportions from logistic model
    background.testing.proportions = get.background.proportions(base.model = components$background.testing$model,
                                                                years = components$background.testing$years,
                                                                additional.intercepts = log(components$background.testing$additional.intercept.ors),
                                                                additional.slopes = log(components$background.testing$additional.slope.ors),
                                                                future.slope = log(components$background.testing$future.slope.or),
                                                                future.slope.after.year = components$background.testing$future.slope.after.year,
                                                                idu.applies.to.in.remission = F,
                                                                idu.applies.to.msm.idu=T,
                                                                msm.applies.to.msm.idu=T,
                                                                jheem=components$jheem)
    
    #Add in ramp up (in the past)
    ramp.years = setdiff(components$background.testing$first.testing.year:components$background.testing$ramp.up.year,
                         components$background.testing$first.testing.year)
    
    background.testing.proportions = c(list(0 * background.testing.proportions[[1]]),
                                       lapply(ramp.years, function(y){
                                           components$background.testing$ramp.up.vs.current.rr *
                                               (1/components$background.testing$ramp.up.yearly.increase)^(components$background.testing$ramp.up.year-y) *
                                               background.testing.proportions[[1]]
                                       }),
                                       background.testing.proportions)
    background.testing.years = c(components$background.testing$first.testing.year, ramp.years,
                                 components$background.testing$years)

    #Convert background to rates
    background.testing.rates = lapply(background.testing.proportions, function(p){
        -log(1-p)
    })
    
    #        if (!components$allow.decreasing.testing.rates)
    #        {
    #            for (i in 2:length(background.rates))
    #                background.rates[[i]] = pmax(background.rates[[i]], background.rates[[i-1]])
    #        }
    
    #Overlay foreground rates
    testing.rates = do.get.rates.from.background.and.foreground(background.rates = background.testing.rates,
                                                             background.times = background.testing.years,
                                                             background.data.type = 'rate',
                                                             foreground = components$foreground.testing,
                                                             max.background.time = components$background.change.to.years$testing)
    
    #Return
    components$testing.rates.and.times = testing.rates
    components
}


#-- PrEP --#

calculate.prep.coverage <- function(components, type='prep')
{
    if (is.null(components$prep.rates.and.times))
        components = do.calculate.prep.coverage(components)
    
    if (type=='prep')
        components$prep.rates.and.times
    else
        components$additional.prep[[type]]$rates.and.times
}

calculate.prep.rrs <- function(components, type='prep')
{
    if (type=='prep')
        components$prep.rrs.and.times
    else
        components$additional.prep[[type]]$rrs.and.times
}

do.calculate.prep.rrs <- function(components, type='prep')
{
    if (type=='prep')
    {
        rr.het = components$prep.rr.heterosexual
        rr.msm = components$prep.rr.msm
        rr.idu = components$prep.rr.idu
        foreground = components$foreground.rr.prep
    }
    else
    {
        rr.het = components$additional.prep[[type]]$rr.heterosexual
        rr.msm = components$additional.prep[[type]]$rr.msm
        rr.idu = components$additional.prep[[type]]$rr.idu
        foreground = components[[paste0('foreground.rr.', type)]]
        
        if (is.null(rr.het))
            rr.het = components$prep.rr.heterosexual
        if (is.null(rr.msm))
            rr.msm = components$prep.rr.msm
        if (is.null(rr.idu))
            rr.idu = components$prep.rr.idu
    }
    
    background.rrs = get.hiv.negative.population.skeleton(components$jheem, value=rr.het)
    background.rrs[,,,'msm',,] = rr.msm
    background.rrs[,,,,'active_IDU',] = rr.idu

    rv = do.get.rates.from.background.and.foreground(background.rates=list(background.rrs),
                                                     background.times=2020,
                                                     background.data.type = 'rate',
                                                     foreground = foreground,
                                                     max.background.time = Inf)
    
    rv$rates = lapply(rv$rates, function(rates){
        names(rates) = names(background.rrs)
        rates
    })
    
    rv
}


calculate.aggregate.prep.coverage.and.risk <- function(components)
{
    prep.types = get.prep.types(components)
    
    coverages = lapply(prep.types, calculate.prep.coverage, components=components)
    rrs = lapply(prep.types, calculate.prep.rrs, components=components)
    
    rv = list(
        coverage = coverages[[1]]$rates,
        risk = lapply(1:length(coverages[[1]]$rates), function(j){
            coverages[[1]]$rates[[j]] * rrs[[1]]$rates[[j]]
        }),
        times = coverages[[1]]$times
    )
    
    if (length(prep.types)>1)
    {
        for (i in 2:length(prep.types))
        {
            rv$coverage = lapply(1:length(rv$coverage), function(j){
                rv$coverage[[j]] + coverages[[i]]$rates[[j]]
            })
            
            rv$risk = lapply(1:length(rv$risk), function(j){
                rv$risk[[j]] + coverages[[i]]$rates[[j]] * rrs[[i]]$rates[[j]]
            })
        }
    }
    
    rv
}

do.calculate.prep.coverage <- function(components)
{
    #-- Baseline PrEP --#
    #Pull background prep proportions from logistic model
    if (!is.null(components$background.prep$max.proportion.ors))
    {
        logit.max.prep.proportion = log(components$background.prep$model$max.proportion) - log(1-components$background.prep$model$max.proportion)
        logit.max.prep.proportion = logit.max.prep.proportion + components$background.prep$model$max.p.lors
        logit.max.prep.proportion = add.additional.betas.to.array(logit.max.prep.proportion,
                                                                  additional.betas = log(components$background.prep$max.proportion.ors),
                                                                  idu.applies.to.in.remission = F,
                                                                  idu.applies.to.msm.idu=F,
                                                                  msm.applies.to.msm.idu=F)
        max.prep.proportion = 1 / (1+exp(-logit.max.prep.proportion))
    }
    else
        max.prep.proportion = components$background.prep$model$max.proportion
    
    background.prep = get.background.proportions(base.model = components$background.prep$model,
                                                 years = components$background.prep$years,
                                                 additional.intercepts = log(components$background.prep$additional.intercept.ors),
                                                 additional.slopes = log(components$background.prep$additional.slope.ors),
                                                 future.slope = log(components$background.prep$future.slope.or),
                                                 future.slope.after.year = components$background.prep$future.slope.after.year,
                                                 idu.applies.to.in.remission = F,
                                                 idu.applies.to.msm.idu=F,
                                                 msm.applies.to.msm.idu=F,
                                                 jheem=components$jheem,
                                                 max.proportion = max.prep.proportion,
                                                 expand.population='hiv.negative')
    #Add in zero suppression
    background.prep.years = c(components$background.prep$zero.prep.year, 
                              components$background.prep$years)
    background.prep = c(list(0 * background.prep[[1]]), background.prep)
    
    components$prep.rates.and.times = do.get.rates.from.background.and.foreground(background.rates = background.prep,
                                                                                  background.times = background.prep.years,
                                                                                  background.data.type = 'proportion',
                                                                                  foreground = components$foreground.prep,
                                                                                  max.background.time = components$background.change.to.years$prep)
    
    
    #-- Baseline PrEP RRs --#
    components$prep.rrs.and.times = do.calculate.prep.rrs(components)

#    all.times = sort(unique(c(components$prep.rates.and.times$times,
 #                             components$prep.rrs.and.times$times)))
  
    all.times = union_sorted_vectors(list(components$prep.rates.and.times$times,
                                          components$prep.rrs.and.times$times))
      
    #-- Other Types of PrEP --#
    if (length(get.prep.types(components))>1)
    {
        for (type in setdiff(get.prep.types(components), 'prep'))
        {
            components$additional.prep[[type]]$rates.and.times = 
                do.get.rates.from.background.and.foreground(background.rates = background.prep[1], #a zero array
                                                            background.times = background.prep.years[1],
                                                            background.data.type = 'proportion',
                                                            foreground = components[[paste0('foreground.',type)]],
                                                            max.background.time = components$background.change.to.years$prep)
            
            components$additional.prep[[type]]$rrs.and.times = do.calculate.prep.rrs(components, type=type)
            
            all.times = union_sorted_vectors(list(all.times,
                                                  components$additional.prep[[type]]$rates.and.times$times,
                                                  components$additional.prep[[type]]$rrs.and.times$times))
 #           all.times = sort(unique(c(all.times,
#                                      components$additional.prep[[type]]$rates.and.times$times,
  #                                    components$additional.prep[[type]]$rrs.and.times$times)))
        }
    }
    
    
    
    #-- Interpolate --#
    
    components$prep.rates.and.times$rates = interpolate.parameters(values=components$prep.rates.and.times$rates,
                                                                   value.times=components$prep.rates.and.times$times,
                                                                   desired.times=all.times,
                                                                   return.list=T)
    components$prep.rrs.and.times$rates = interpolate.parameters(values=components$prep.rrs.and.times$rates,
                                                                   value.times=components$prep.rrs.and.times$times,
                                                                   desired.times=all.times,
                                                                   return.list=T)
    components$prep.rates.and.times$times = components$prep.rrs.and.times$times = all.times
    
    
    if (length(get.prep.types(components))>1)
    {
        for (type in setdiff(get.prep.types(components), 'prep'))
        {
            components$additional.prep[[type]]$rates.and.times$rates = 
                interpolate.parameters(values=components$additional.prep[[type]]$rates.and.times$rates,
                                       value.times=components$additional.prep[[type]]$rates.and.times$times,
                                       desired.times=all.times,
                                       return.list=T)
            
            components$additional.prep[[type]]$rrs.and.times$rates = 
                interpolate.parameters(values=components$additional.prep[[type]]$rrs.and.times$rates,
                                       value.times=components$additional.prep[[type]]$rrs.and.times$times,
                                       desired.times=all.times,
                                       return.list=T)
            
            components$additional.prep[[type]]$rates.and.times$times =
                components$additional.prep[[type]]$rrs.and.times$times = all.times
        }
    }
    
    #-- Return --#
    components
}

#-- Needle Exchange --#
calculate.needle.exchange.coverage <- function(components)
{
    if (is.null(components$needle.exchange.rates.and.times))
        components = do.calculate.needle.exchange.coverage(components)
    components$needle.exchange.rates.and.times
}

do.calculate.needle.exchange.coverage <- function(components)
{
    components$needle.exchange.rates.and.times = do.get.rates.from.background.and.foreground(background.rates = components$background.needle.exchange$proportions,
                                                                                             background.times = components$background.needle.exchange$years,
                                                                                             background.data.type = 'proportion',
                                                                                             foreground = components$foreground.needle.exchange,
                                                                                             max.background.time = Inf)#components$background.change.to.years$needle.exchange)
    
    components
}

#-- FOR THE EXPANDED CONTINUUM (below) --#

calculate.linkage <- function(components)
{
    calculate.rates(components, type='linkage')
}

#-- The GENERAL calculate rates functions --#
calculate.rates <- function(components,
                            type)
{
    component.name = paste0(type, ".rates.and.times")
    if (is.null(components[[component.name]]))
        components = do.calculate.rates(components, type)
    components[[component.name]]
}

is.rate.calculated <- function(components,
                               type)
{
    component.name = paste0(type, ".rates.and.times")
    !is.null(components[[component.name]])
}

is.transition.element.set.up <- function(components,
                                         type)
{
    src.name = paste0('background.', type)
    !is.null(components[[src.name]]) || 
        !is.null(components[[type]])
}

do.calculate.rates <- function(components, type,
                               default.year=2010)
{
    special.cases = c( #that have their own specific function
                      'suppression',
                      'prep',
                      'rr.prep',
                      'needle.exchange.coverage'
    )   
    if (any(type==special.cases))
        stop(paste0("Calculating '", type, "' rates cannot be handled with a call to 'do.calculate.rates'. You must invoke the specific function"))
    
    tr.el = get.transition.element.by.name(get.components.transition.mapping(components),
                                           name=type, allow.missing=T)
    if (is.null(tr.el))
    {
        stop('missing transition element from transition mapping')
        background.model.type = ramp.type = return.type = 'rate'
        tr.el$required = T
    }
    else
    {
        background.model.type = tr.el$background.model.type
        ramp.type = tr.el$ramp.type
        return.type = tr.el$return.type
        required = tr.el$required
        default.value.if.missing = tr.el$default.value
        
        target.dim.names = tr.el$dim.names
    }
    
    #-- PART 1: Pull out the background --#
    
    dst.name = paste0(type, ".rates.and.times")
    src.name = paste0('background.', type)
    foreground.name = paste0('foreground.', type)
    sub.component = components[[src.name]]
    
    if (is.null(sub.component))
    {
        # Check if we have just a fixed value
        if (is.null(components[[type]]))
        {
            if (required)
                stop(paste0("No background values have been set for '", type, "' in components"))
            else
            {
                background.values = list(default.value.if.missing)
                background.years = default.year
            }
        }
        else
        {
            background.values = components[type]
            background.years = default.year
        }
        
        data.type = background.model.type
    }
    else if (length(sub.component)==1)
    {
        background.years = default.year
        background.values = sub.component
        
        data.type = background.model.type
    }
    else
    {
        if (is.null(sub.component$model))
            stop(paste0("Background has not been fully set up for '", type, "' (model is missing)"))
        
        if (is(sub.component$model, 'model'))
        {
            background.values = project.from.model(model = sub.component$model,
                                                   years = sub.component$years,
                                                   alphas = sub.component$alphas,
                                                   future.slope = sub.component[['future.slope']],
                                                   future.slope.after.year = sub.component$future.slope.after.year,
                                                   dim.names = tr.el$dim.names)
        }
        else
        {
            if (is.null(sub.component$additional.intercept.ors) || is.null(sub.component$additional.slope.ors))
                stop(paste0("Background has not been fully set up for '", type, "' (additional ors is missing)"))
            if (is.null(sub.component$future.slope.or))
                stop(paste0("Background has not been fully set up for '", type, "' (future.slope is missing)"))
            
            background.values = get.background.proportions(base.model = sub.component$model,
                                                           years = sub.component$years,
                                                           additional.intercepts = log(sub.component$additional.intercept.ors),
                                                           additional.slopes = log(sub.component$additional.slope.ors),
                                                           future.slope = log(sub.component$future.slope.or),
                                                           future.slope.after.year = sub.component$future.slope.after.year,
                                                           idu.applies.to.in.remission = type!='testing',
                                                           idu.applies.to.msm.idu=T,
                                                           msm.applies.to.msm.idu=T,
                                                           jheem=components$jheem)
            
            extra.dim = setdiff(names(dimnames(background.values[[1]])), target.dim.names)
            if (length(extra.dim)==0) # then hydrate up to our target dimensions
                background.values = lapply(background.values, function(v){
                    expand.population(v, target.dim.names = target.dim.names)
                })
            else if (length(extra.dim)==1) # need to strip out the extra dimension and then hydrate
                background.values = lapply(background.values, function(v){
                    expand.population(single.dim.access(v, dim=extra.dim, dim.value=1),
                                      target.dim.names = target.dim.names)
                })
            else # uh-oh
                stop(paste0("In calculating '", type, "' rates, too many surplus dimensions in legacy background: ",
                            paste0("'", extra.dim, "'", collapse=', ')))
        }
        
        data.type = background.model.type
        if (length(background.values[[1]])>0)
            background.values = lapply(background.values, function(values){
                expand.population(values, target.dim.names = target.dim.names)
            })
        
        #-- Add in ramp up (in the past) --#
        
        # For backwards compatibility with early versions
        if (!is.null(sub.component[['ramp.up.year']]))
        {
                   
            sub.component$ramp.years = c(sub.component$first.testing.year, 
                                         sub.component$first.testing.year+1,
                                         sub.component$ramp.up.year)
            sub.component$ramp.multipliers = c(0, 
                                               sub.component$ramp.up.vs.current.rr * 
                                                   (1/components$background.testing$ramp.up.yearly.increase)^(sub.component$ramp.up.year-sub.component$first.testing.year-1),
                                               sub.component$ramp.up.vs.current.rr)
        }
        
        
        if (length(sub.component$ramp.years) > 0)
        {
            background.values = convert.transition.element.type(background.values,
                                                                convert.from.type = data.type,
                                                                convert.to.type = ramp.type)
            data.type = ramp.type
            
            if (is.null(tr.el))
                stop("This state should be unreachable - no transition element set, but ramp required")
            
            ramp.multipliers.and.times = calculate.transition.ramp.multipliers(transition.element=tr.el,
                                                                               ramp.times=sub.component$ramp.years,
                                                                               ramp.multipliers=sub.component$ramp.multipliers,
                                                                               non.ramp.times=sub.component$years)

            background.years = c(ramp.multipliers.and.times$times, 
                                 sub.component$years)
            background.values = c(
                lapply(ramp.multipliers.and.times$multipliers, function(mult){
                    mult * background.values[[1]]
                }),
                background.values)
            
            o = order(background.years)
            background.years = background.years[o]
            background.values = background.values[o]
        }
    }
    

    #-- PART 2: Fold in the foreground --#

    rates.and.times = do.get.rates.from.background.and.foreground(background.rates = background.values,
                                                        background.times = background.years,
                                                        background.data.type = data.type,
                                                        foreground = components[[foreground.name]],
                                                        max.background.time = components$background.change.to.years[[type]])

    
    
    #-- PART 3: Transform if needed --#
    
    rates.and.times$rates = convert.transition.element.type(rates.and.times$rates,
                                                      convert.from.type = rates.and.times$data.type,
                                                      convert.to.type = return.type)
    rates.and.times$data.type = return.type
    
    #-- Return --#
    
    components[[dst.name]] = rates.and.times
    components
}

OLD.do.calculate.rates <- function(components, type)
{
    special.cases = c('testing', #that have their own specific function
                      'suppression',
                      'prep',
                      'rr.prep',
                      'needle.exchange.coverage',
                      'start.art'
    )   
    if (any(type==special.cases))
        stop(paste0("Calculating '", type, "' rates cannot be handled with a call to 'do.calculate.rates'. You must invoke the specific function"))
    
    
    dst.name = paste0(type, ".rates.and.times")
    src.name = paste0('background.', type)
    foreground.name = paste0('foreground.', type)
    sub.component = components[[src.name]]
    
        
    if (is.null(sub.component))
        stop(paste0("Background has not been set up for '", type, "'"))
    if (is.null(sub.component$model))
        stop(paste0("Background has not been fully set up for '", type, "' (model is missing)"))
    if (is.null(sub.component$additional.intercept.ors) || is.null(sub.component$additional.slope.ors))
        stop(paste0("Background has not been fully set up for '", type, "' (additional ors is missing)"))
    if (is.null(sub.component$future.slope.or))
        stop(paste0("Background has not been fully set up for '", type, "' (future.slope is missing)"))
    
    #Pull background proportions from logistic model
    background.proportions = get.background.proportions(base.model = sub.component$model,
                                                        years = sub.component$years,
                                                        additional.intercepts = log(sub.component$additional.intercept.ors),
                                                        additional.slopes = log(sub.component$additional.slope.ors),
                                                        future.slope = log(sub.component$future.slope.or),
                                                        future.slope.after.year = sub.component$future.slope.after.year,
                                                        idu.applies.to.in.remission = T,
                                                        idu.applies.to.msm.idu=T,
                                                        msm.applies.to.msm.idu=T,
                                                        jheem=components$jheem)
    
    #Add in ramp up (in the past)
    
    background.years = c(sub.component$ramp.years, 
                              sub.component$years)
    background.proportions = c(
        lapply(sub.component$ramp.multipliers, function(mult){
            mult * background.proportions[[1]]
        }),
        background.proportions)
    
    o = order(background.years)
    background.years = background.years[o]
    background.proportions = background.proportions[o]
    
    
    # Do we need to convert to rates?
    if (is.null(components[[foreground.name]]))
        convert.foreground.proportions.to.rates = sub.component$convert.proportions.to.rates 
            #By default, this has us interpolate on proportions
    else
    {
        convert.each.foreground.proportions.to.rates = 
            unlist(components[[foreground.name]]$convert.proportions.to.rates)
        
        convert.foreground.proportions.to.rates = all(convert.each.foreground.proportions.to.rates)
        if (!all(convert.each.foreground.proportions.to.rates==convert.foreground.proportions.to.rates))
            stop("Either all foreground elements must be converted from proportions to rates or none must (you cannot have some converted and others not)")
    }
    
    if (convert.foreground.proportions.to.rates && !sub.component$convert.proportions.to.rates)
        stop("Cannot convert foreground proportions to rates if we are not converting background proportions to rates")
    
    #Convert background to rates

    if (sub.component$convert.proportions.to.rates && !convert.foreground.proportions.to.rates)
    {
        background.rates = lapply(background.proportions, function(p){
            -log(1-p)
        })
        background.data.type = 'rate'
    }
    else
    {
        background.rates = background.proportions
        background.data.type = 'proportion'
    }
    
    #Overlay foreground rates
    rates = do.get.rates.from.background.and.foreground(background.rates = background.rates,
                                                        background.times = background.years,
                                                        background.data.type = background.data.type,
                                                        foreground = components[[foreground.name]],
                                                        max.background.time = components$background.change.to.years[[type]])
    
    if (sub.component$convert.proportions.to.rates && convert.foreground.proportions.to.rates)
        rates$rates = lapply(rates$rates, function(p){
            -log(1-p)
        })
        
    #Return
    components[[dst.name]] = rates
    components
}

calculate.start.art.proportions <- function(components,
                            type)
{
    component.name = "start.art.rates.and.times"
    if (is.null(components[[component.name]]))
        components = do.calculate.rates(components, type)
    components[[component.name]]
}


do.calculate.start.art.proportions <- function(components, type)
{
    sub.component = components$background.start.art
    foreground.name = 'foreground.start.art'
    
    #Pull background proportions from logistic model
    background.proportions = get.background.proportions(base.model = sub.component$model,
                                                        years = sub.component$years,
                                                        additional.intercepts = log(sub.component$additional.intercept.ors),
                                                        additional.slopes = log(sub.component$additional.slope.ors),
                                                        future.slope = log(sub.component$future.slope.or),
                                                        future.slope.after.year = sub.component$future.slope.after.year,
                                                        idu.applies.to.in.remission = T,
                                                        idu.applies.to.msm.idu=T,
                                                        msm.applies.to.msm.idu=T,
                                                        jheem=components$jheem)
    
    #Add in ramp up (in the past)
    background.years = c(sub.component$zero.art.year,
                         sub.component$ramp.years, 
                         sub.component$years,
                         sub.component$full.art.year)
    
    background.proportions = c(
        list(0 * background.proportions[[1]]),
        lapply(sub.component$ramp.multipliers, function(mult){
            mult * background.proportions[[1]]
        }),
        background.proportions,
        list(1 - 0 * background.proportions[[1]]))
    
    o = order(background.years)
    background.years = background.years[o]
    background.proportions = background.proportions[o]
    
    # Do we need to convert to rates?
    if (is.null(components[[foreground.name]]))
        convert.foreground.proportions.to.rates = sub.component$convert.proportions.to.rates
    else
    {
        convert.each.foreground.proportions.to.rates = 
            unlist(components[[foreground.name]]$convert.proportions.to.rates)
        
        convert.foreground.proportions.to.rates = all(convert.each.foreground.proportions.to.rates)
        if (!all(convert.each.foreground.proportions.to.rates==convert.foreground.proportions.to.rates))
            stop("Either all foreground elements must be converted from proportions to rates or none must (you cannot have some converted and others not)")
    }
    
    if (convert.foreground.proportions.to.rates && !sub.component$convert.proportions.to.rates)
        stop("Cannot convert foreground proportions to rates if we are not converting background proportions to rates")
    
    #Convert background to rates
    if (sub.component$convert.proportions.to.rates && !convert.foreground.proportions.to.rates)
        background.rates = lapply(background.proportions, function(p){
            -log(1-p)
        })
    else
        background.rates = background.proportions
    
    #Overlay foreground rates
    rates = do.get.rates.from.background.and.foreground(background.rates = background.rates,
                                                        background.times = background.years,
                                                        background.data.type = 'proportion',
                                                        foreground = components[[foreground.name]],
                                                        max.background.time = components$background.change.to.years[['start.art']])
    
    if (sub.component$convert.proportions.to.rates && convert.foreground.proportions.to.rates)
        rates$rates = lapply(rates$rates, function(p){
            -log(1-p)
        })
    
    #Return
    components$start.art.rates.and.times = rates
    components
}

#in years
calculate.time.to.suppression.on.art <- function(components,
                                                 years)
{
    interpolate.parameters(values=components$time.to.suppression.on.art$latency.to.suppression.in.years,
                           value.times=components$time.to.suppression.on.art$years.at.which.latency.applies,
                           desired.times = years,
                           return.list = F)
}

##-- GET BACKGROUND PROPORTIONS --##

get.background.proportions <- function(base.model,
                                       years,
                                       additional.intercepts,
                                       additional.slopes,
                                       future.slope=0,
                                       future.slope.after.year,
                                       idu.applies.to.in.remission=T,
                                       idu.applies.to.msm.idu=T,
                                       msm.applies.to.msm.idu=T,
                                       min.proportion=if (is.null(base.model$min.proportion)) 0 else base.model$min.proportion,
                                       max.proportion=base.model$max.proportion,
                                       transformation = function(x){min.proportion + (max.proportion-min.proportion) / (1+exp(-x))},
                                       jheem,
                                       expand.population=c('hiv.positive','hiv.negative','none')[1])
{
    is.logistic.tail.model = (!is.null(base.model$model.type) && base.model$model.type=='logistic.tail') ||
        (!is.null(base.model$mixed.linear) && base.model$mixed.linear)
    
    intercept = add.additional.betas.to.array(base.model$intercept, additional.intercepts,
                                              idu.applies.to.in.remission = idu.applies.to.in.remission,
                                              idu.applies.to.msm.idu = idu.applies.to.msm.idu,
                                              msm.applies.to.msm.idu = msm.applies.to.msm.idu)
    slope = add.additional.betas.to.array(base.model$slope, additional.slopes,
                                          idu.applies.to.in.remission = idu.applies.to.in.remission,
                                          idu.applies.to.msm.idu = idu.applies.to.msm.idu,
                                          msm.applies.to.msm.idu = msm.applies.to.msm.idu)

    if (is.logistic.tail.model)
    {
        intercept = transformation(intercept)
        pre.tx.slope = slope
        slope = transformation(pre.tx.slope)
        future.slope = transformation(pre.tx.slope + future.slope)
        
        if (base.model$use.logistic.tail)
            logistic.tail.model = make.logistic.tail.model(intercept,
                                                           slope=slope,
                                                           anchor.year=base.model$anchor.year,
                                                           additional.slope=future.slope-slope,
                                                           additional.slope.after.year=future.slope.after.year,
                                                           max.p=base.model$max.proportion.for.logistic.tail,
                                                           logistic.after.frac.of.max.p=base.model$logistic.after.frac.of.max.p)
    }
    else
        future.slope = slope + future.slope
    
    lapply(years, function(year){
        
        if (is.logistic.tail.model && base.model$use.logistic.tail)
        {
            p = calculate.logistic.tail.values(logistic.tail.model, year)
        }
        else
        {
            p = intercept + 
                slope * (min(year, future.slope.after.year) - base.model$anchor.year) +
                future.slope * max(0,year-future.slope.after.year)
            
            
            if (!is.null(base.model$mixed.linear) && base.model$mixed.linear)
                p[] = pmax(min.proportion, pmin(max.proportion, pmax(0, p)))
            else if (!is.null(transformation))
                p = transformation(p)
        }
        
        if (expand.population=='hiv.positive')
            expand.population.to.hiv.positive(jheem, p)
        else if (expand.population=='hiv.negative')
            expand.population.to.hiv.negative(jheem, p)
        else
            p
    })
}


add.additional.betas.to.array <- function(arr, additional.betas,
                                          idu.applies.to.in.remission=T,
                                          idu.applies.to.msm.idu=!any(grepl('msm_idu', names(additional.betas))),
                                          msm.applies.to.msm.idu=!any(grepl('msm_idu', names(additional.betas))))
{
    if (any(grepl('msm_idu', names(additional.betas))))
        idu.applies.to.msm.idu = msm.applies.to.msm.idu = F
    
    if (idu.applies.to.in.remission=='hi' || idu.applies.to.in.remission)
        idu.strata = c('active_IDU','IDU_in_remission')
    else
        idu.strata = 'active_IDU'
    non.idu.strata = setdiff(c('never_IDU','active_IDU','IDU_in_remission'), idu.strata)
    
    if (idu.applies.to.msm.idu)
        idu.sexes = c('heterosexual_male','female','msm')
    else
        idu.sexes = c('heterosexual_male','female')
    
    if (msm.applies.to.msm.idu)
        msm.risks = c('never_IDU','active_IDU','IDU_in_remission')
    else
        msm.risks = non.idu.strata
    
    for (name in names(additional.betas))
    {
        if (grepl('all', name, ignore.case = T))
            arr = arr + additional.betas[name]
        
        else if (grepl('age1', name, ignore.case = T))
            arr[1,,,] = arr[1,,,] + additional.betas[name]
        else if (grepl('age2', name, ignore.case = T))
            arr[2,,,] = arr[2,,,] + additional.betas[name]
        else if (grepl('age3', name, ignore.case = T))
            arr[3,,,] = arr[3,,,] + additional.betas[name]
        else if (grepl('age4', name, ignore.case = T))
            arr[4,,,] = arr[4,,,] + additional.betas[name]
        else if (grepl('age5', name, ignore.case = T))
            arr[5,,,] = arr[5,,,] + additional.betas[name]
        
        else if (grepl('black', name, ignore.case = T))
            arr[,'black',,] = arr[,'black',,] + additional.betas[name]
        else if (grepl('hispanic', name, ignore.case = T))
            arr[,'hispanic',,] = arr[,'hispanic',,] + additional.betas[name]
        else if (grepl('other', name, ignore.case = T))
            arr[,'other',,] = arr[,'other',,] + additional.betas[name]
        
        else if (grepl('msm_idu', name, ignore.case=T))
            arr[,,'msm',idu.strata] = arr[,,'msm',idu.strata] + additional.betas[name]
        else if (grepl('idu', name, ignore.case = T))
            arr[,,idu.sexes,idu.strata] = arr[,,idu.sexes,idu.strata] + additional.betas[name]
        else if (grepl('msm', name, ignore.case = T))
            arr[,,'msm',msm.risks] = arr[,,'msm',msm.risks] + additional.betas[name]
        else if (grepl('heterosexual', name, ignore.case = T))
            arr[,,c('heterosexual_male','female'),non.idu.strata] = 
                arr[,,c('heterosexual_male','female'),non.idu.strata] + additional.betas[name]
        
        else if (grepl('female', name, ignore.case = T))
            arr[,,'female',] = arr[,,'female',] + additional.betas[name]
    }
    
    arr
}

##-----------##
##-- AGING --##
##-----------##

do.setup.aging <- function(components)
{
    components$aging.hiv.negative = get.aging.skeleton.hiv.negative(components$jheem)
    components$aging.hiv.negative[1,,,,'active_IDU',] = components$proportion.active.idu.13.24.who.are.24
    components$aging.hiv.negative[1,,,,'IDU_in_remission',] = components$proportion.prior.idu.13.24.who.are.24
    
    min.time = Inf
    max.time = -Inf
    for (age.index in 1:(length(components$jheem$age$labels)))
    {
        for (race in components$jheem$race)
        {
            for (route in c('msm','heterosexual.male','heterosexual.female','idu'))
            {
                if (is.null(components[[paste0('aging.', route)]]))
                {
                    elements = components[[paste0(route, '.aging.rates')]][[race]][[paste0('age',age.index)]]
                    times=elements[c('t.pre.trough','t.trough','t.peak','t.post.peak',
                                     't.pre.second.change','t.second.change','t.post.second.change')]
                    rates=elements[c('pre.trough.rate','trough.rate','peak.rate','post.peak.rate',
                                     'pre.second.change.rate','second.change.rate','post.second.change.rate')]
                }
                else
                {
                    elements = components[[paste0('aging.', route)]][[race]][[paste0('age',age.index)]]
                    times=elements[c('t.pre.spike','t.spike',paste0('t',0:3))]
                    rates=elements[c('r.pre.spike','r.spike',paste0('r',0:3))]
                }
                
                min.time = min(min.time, as.numeric(times[!is.null(times) & !is.null(rates)]))
                max.time = max(max.time, as.numeric(times[!is.null(times) & !is.null(rates)]))
            }
        }
    }
    
    aging = get.aging.skeleton.hiv.positive(components$jheem)
    
    components$aging.hiv.positive.years = min.time:max.time
    components$aging.rates.hiv.positive = lapply(components$aging.hiv.positive.years, function(year){
        for (age.index in 1:(length(components$jheem$age$labels)-1))
        {
            for (race in components$jheem$race)
            {
                for (route in c('msm','heterosexual.male', 'heterosexual.female','idu'))
                {
                    if (is.null(components[[paste0('aging.', route)]]))
                    {
                        elements = components[[paste0(route, '.aging.rates')]][[race]][[paste0('age',age.index)]]
                        times=elements[c('t.pre.trough','t.trough','t.peak','t.post.peak',
                                         't.pre.second.change','t.second.change','t.post.second.change')]
                        rates=elements[c('pre.trough.rate','trough.rate','peak.rate','post.peak.rate',
                                         'pre.second.change.rate','second.change.rate','post.second.change.rate')]
                    }
                    else
                    {
                        elements = components[[paste0('aging.', route)]][[race]][[paste0('age',age.index)]]
                        times=elements[c('t.pre.spike','t.spike',paste0('t',0:3))]
                        rates=elements[c('r.pre.spike','r.spike',paste0('r',0:3))]
                    }
                    
                    aging.for.subgroup = do.get.aging.rate(year=year, times=times, rate=rates)
                    
                    if (route=='msm')
                        aging[age.index, race, , 'msm', ,,,] = aging.for.subgroup
                    else if (route=='heterosexual.male')
                        aging[age.index, race, , 'heterosexual_male', ,,,] = aging.for.subgroup
                    else if (route=='heterosexual.female')
                        aging[age.index, race, , 'female', ,,,] = aging.for.subgroup
                    else if (route=='idu')
                        aging[age.index, race, , , c('active_IDU','IDU_in_remission'),,,] = aging.for.subgroup
                }
            }
        }
        aging
    })
    
    components
}

do.get.aging.rate <- function(year, times, rates,
                              fraction.before.start=0.025,
                              fraction.between=0.025,
                              fraction.after.end=0.025)
{
    if (is(rates, 'list'))
    {
        rates[sapply(rates, is.null)] = NA
        rates = as.numeric(rates)
    }
    if (is(times, 'list'))
    {
        times[sapply(times, is.null)] = NA
        times = as.numeric(times)
    }
    
    mask = !is.na(rates) & !is.na(times) & rates >= 0
    rates = rates[mask]
    times = times[mask]
    
    n = length(rates)
    if (year <= times[1])
        rates[1]
    else if (year >= times[n])
        rates[n]
    else if (n==1)
        rates
    else if (n==2)
        calculate.change.ratios.logistic(r0=rates[1], r1=rates[2],
                                         t0=times[1], t1=times[2],
                                         times=year,
                                         fraction.of.asymptote.before.start = fraction.before.start,
                                         fraction.of.asymptote.after.end = fraction.after.end)
    else
    {
        index.mask = year <= times[-1]
        if (any(index.mask))
            index = (2:n)[index.mask][1]
        else
            index = n
        
        fraction.before = fraction.after = fraction.between
        if (index==1)
            fraction.before = fraction.before.start
        if (index==(n-1))
            fraction.after = fraction.after.end
        
        calculate.change.ratios.logistic(r0=rates[index-1], r1=rates[index],
                                         t0=times[index-1], t1=times[index],
                                         times=year,
                                         fraction.of.asymptote.before.start = fraction.before,
                                         fraction.of.asymptote.after.end = fraction.after)
    }
}

get.aging.rate <- function(year,
                           t.pre.trough, t.trough, t.mid, t.peak, t.post.peak,
                           base.rate, trough.rate, peak.rate,
                           t2, rate2)
{
    if (year < t.trough)
        calculate.change.ratios.logistic(r0=base.rate, r1=trough.rate,
                                         t0=t.pre.trough, t1=t.trough,
                                         times=year,
                                         fraction.of.asymptote.before.start = 0.05,
                                         fraction.of.asymptote.after.end = 0.025)
    #    else if (year < t.mid)
    #        calculate.change.ratios.logistic(r0=trough.rate, r1=base.rate,
    #                                         t0=t.trough, t1=t.mid,
    #                                         times=year,
    #                                         fraction.of.asymptote.before.start = 0.025,
    #                                         fraction.of.asymptote.after.end = 0.025)
    #    else if (year < t.peak && base.rate==trough.rate)
    #        calculate.change.ratios.logistic(r0=trough.rate, r1=peak.rate,
    #                                         #r0=base.rate, r1=peak.rate,
    #                                         t0=t.mid, t1=t.peak,
    #                                         #                                         t0=t.mid, t1=t.peak,
    #                                         times=year,
    #                                         fraction.of.asymptote.before.start = 0.025,
    #                                         fraction.of.asymptote.after.end = 0.025)
    else if (year < t.peak)
        calculate.change.ratios.logistic(r0=trough.rate, r1=peak.rate,
                                         #r0=base.rate, r1=peak.rate,
                                         t0=t.trough, t1=t.peak,
                                         #                                         t0=t.mid, t1=t.peak,
                                         times=year,
                                         fraction.of.asymptote.before.start = 0.025,
                                         fraction.of.asymptote.after.end = 0.025)
    else if (!is.null(rate2) && !is.null(t2) && year > t.post.peak)
        calculate.change.ratios.logistic(r0=base.rate, r1=rate2,
                                         t0=t.post.peak, t1=t2,
                                         times=year,
                                         fraction.of.asymptote.before.start = 0.025,
                                         fraction.of.asymptote.after.end = 0.05)
    else
        calculate.change.ratios.logistic(r0=peak.rate, r1=base.rate,
                                         t0=t.peak, t1=t.post.peak,
                                         times=year,
                                         fraction.of.asymptote.before.start = 0.025,
                                         fraction.of.asymptote.after.end = 0.05)
}

##------------------##
##-- TRANSMISSION --##
##------------------##

#global rates are a legacy from an earlier implementation. Here just set to 1
do.setup.global.sexual.transmission <- function(components)
{
    components$global.sexual.transmission.rates = components$global.sexual.trate
    components$global.sexual.transmission.years = -Inf
    
    components
}

do.setup.global.idu.transmission <- function(components)
{
    components$global.idu.transmission.rates = components$global.idu.trate
    components$global.idu.transmission.years = -Inf
    
    components
}

calculate.changing.trates.and.times <- function(params)
{
#tryCatch({
    do.calculate.changing.trates.and.times(r.peak = params$r.peak,
                                           r0 = params$r0,
                                           r1 = params$r1,
                                           r0.5 = params$r0.5,
                                           r2 = params$r2,
                                           t.pre.peak = params$t.pre.peak,
                                           t.peak.start = params$t.peak.start,
                                           t.peak.end = params$t.peak.end,
                                           t0.start = params$t0.start,
                                           t0.end = params$t0.end,
                                           t1 = params$t1,
                                           t0.5 = params$t0.5,
                                           t2 = params$t2,
                                           t.end = params$t.end,
                                           fraction.change.after.end = params$fraction.change.after.end)
#},
#error = function(e){
#    browser()
#})4
}

do.calculate.changing.trates.and.times <- function(r.peak, r0, r1, r2, r0.5,
                                                   t.pre.peak, t.peak.start, t.peak.end, t0.start, t0.end, t1, t2, t0.5, t.end,
                                                   fraction.change.after.end)
{
    peak.years = c(t.pre.peak, t.peak.start, t.peak.end, t0.start)
    peak.rates = c(r0, r.peak, r.peak, r0)
    
    ratio.years = t0.end:t.end
    if (is.null(r0.5))
        ratio.rates = calculate.change.ratios.two.logistic(r0 = r0, r1 = r1, r2 = r2,
                                                           t0 = t0.end, t1 = t1, t2 = t2,
                                                           times = ratio.years,
                                                           fraction.of.asymptote.after.end = fraction.change.after.end)
    else
    {
        ratio.rates.a = calculate.change.ratios.logistic(r0 = r0, r1 = r0.5,
                                                         t0 = t0.end, t1 = t0.5,
                                                         times = t0.end:t0.5,
                                                         fraction.of.asymptote.after.end = 0.025)
        ratio.rates.b = calculate.change.ratios.two.logistic(r0 = r0.5, r1 = r1, r2 = r2,
                                                             t0 = t0.5, t1 = t1, t2 = t2,
                                                             times = t0.5:t.end,
                                                             fraction.of.asymptote.after.end = fraction.change.after.end,
                                                             fraction.of.asymptote.before.start = 0.025)
        
        ratio.rates = c(ratio.rates.a[-length(ratio.rates.a)], ratio.rates.b)
    }
    
    if (t0.start==t0.end)
    {
        ratio.rates = ratio.rates[-1]
        ratio.years = ratio.years[-1]
    }
    
    list(rates=c(peak.rates, ratio.rates),
         times=c(peak.years, ratio.years))
}

do.setup.susceptibility <- function(components)
{
    base.sexual.susceptibility = base.idu.susceptibility = get.hiv.negative.population.skeleton(components$jheem, 1)
    
    for (age in names(components$sexual.susceptibility.rr.by.age))
        base.sexual.susceptibility[age,,,,,] = base.sexual.susceptibility[age,,,,,] *
            components$sexual.susceptibility.rr.by.age[age]
    
    for (age in names(components$msm.sexual.susceptibility.rr.by.age))
        base.sexual.susceptibility[age,,,'msm',,] = base.sexual.susceptibility[age,,,'msm',,] *
            components$msm.sexual.susceptibility.rr.by.age[age]
    
    if (components$model.prep)
    {
        if (is.null(components$prep.rates.and.times))
            components = do.calculate.prep.coverage(components)
        
        prep.rates.and.times = calculate.aggregate.prep.coverage.and.risk(components)
        
        components$sexual.susceptibility = lapply(1:length(prep.rates.and.times$times), function(i){
            prep.coverage = prep.rates.and.times$coverage[[i]]
            prep.risk = prep.rates.and.times$risk[[i]]
            
            if (is.null(dim(prep.coverage)))
                prep.coverage = as.numeric(prep.coverage)
            prep.coverage = expand.population.to.hiv.negative(components$jheem, prep.coverage)
            
            non.prep.risk = (1-prep.coverage)
#            prep.risk = prep.coverage * prep.rrs['heterosexual']
 #           prep.risk[,,,'msm',,] = prep.coverage[,,,'msm',,] * prep.rrs['msm']
  #          prep.risk[,,,,'active_IDU',] = prep.coverage[,,,,'active_IDU',] * prep.rrs['idu']
            
            susceptibility = non.prep.risk + prep.risk
            susceptibility * base.sexual.susceptibility
        })
        components$sexual.susceptibility.years = prep.rates.and.times$times
        
        if (components$model.idu)
        {
            if (is.null(components$needle.exchange.rates.and.times))
                components = do.calculate.needle.exchange.coverage(components)
            
            needle.exchange.rates.and.times = calculate.needle.exchange.coverage(components)
            
            rates.and.times = merge.rates(rates1 = prep.rates.and.times$coverage,
                                          times1 = prep.rates.and.times$times,
                                          rates2 = needle.exchange.rates.and.times$rates,
                                          times2 = needle.exchange.rates.and.times$times)
            
            all.prep.coverage = rates.and.times$rates1
            all.needle.exchange.rates = rates.and.times$rates2
            all.prep.risks = interpolate.parameters(values=prep.rates.and.times$risk, 
                                              value.times=prep.rates.and.times$times,
                                              desired.times = rates.and.times$times,
                                              return.list = T)
            
            components$idu.susceptibility = lapply(1:length(rates.and.times$times), function(i){
                
                prep.coverage =all.prep.coverage[[i]]
                needle.exchange.coverage = all.needle.exchange.rates[[i]]
                
                prep.rrs = all.prep.risks[[i]] / prep.coverage
                prep.rrs[prep.coverage==0] = 0
                
                if (is.null(dim(prep.coverage)))
                    prep.coverage = as.numeric(prep.coverage)
                prep.coverage = expand.population.to.hiv.negative(components$jheem, prep.coverage)
                needle.exchange.coverage = expand.population.to.hiv.negative(components$jheem, needle.exchange.coverage)
                
                # Set up the proportions for the 2x2 combos of PrEP & needle exchange
                #we presume here prep use and needle exchange are independent (ie, using one does not impact your chances of using the other)
                # (if we ever change this, would need to change new infection proportions as well)
                
                no.prep.no.exchange.p = (1-prep.coverage) * (1-needle.exchange.coverage)
                no.prep.yes.exchange.p = (1-prep.coverage) * needle.exchange.coverage
                yes.prep.no.exchange.p = prep.coverage * (1-needle.exchange.coverage)
                yes.prep.yes.exchange.p = prep.coverage * needle.exchange.coverage
                
                # Multiply by RRs to get risk
                no.prep.no.exchange.risk = no.prep.no.exchange.p
                
                no.prep.yes.exchange.risk = no.prep.yes.exchange.p
                no.prep.yes.exchange.risk[,,,,'active_IDU',] = no.prep.yes.exchange.p[,,,,'active_IDU',] *
                    components$needle.exchange.rr
                
                yes.prep.no.exchange.risk = yes.prep.no.exchange.p
                yes.prep.no.exchange.risk[,,,,'active_IDU',] = yes.prep.no.exchange.p[,,,,'active_IDU',] *
                    prep.rrs[,,,,'active_IDU',]
                
                yes.prep.yes.exchange.risk = yes.prep.yes.exchange.p
                yes.prep.yes.exchange.risk[,,,,'active_IDU',] = yes.prep.yes.exchange.p[,,,,'active_IDU',] *
                    prep.rrs[,,,,'active_IDU',] * components$needle.exchange.rr

                # Add together
                susceptibility = no.prep.no.exchange.risk + no.prep.yes.exchange.risk +
                    yes.prep.no.exchange.risk + yes.prep.yes.exchange.risk
                
                susceptibility * base.idu.susceptibility
                
            })
            
            
            components$idu.susceptibility.years = rates.and.times$times
        }
    }
    else
    {
        components$sexual.susceptibility = list(base.sexual.susceptibility)
        components$idu.susceptibility = list(base.idu.susceptibility)
        components$idu.susceptibility.years = components$sexual.susceptibility.years = -Inf
    }
    
    components
}

do.setup.transmissibility <- function(components)
{
    settings = get.components.settings(components)
    
    if (components$model.hiv.transmission)
    {
        #-- Set up general skeleton arrays (not year-specific) --#
        acute.transmissibility.rr = expand.population.to.hiv.positive(components$jheem,
                                                                      components$acute.transmissibility.rr)
        transmissibility = get.transmissibility.array.skeleton(components$jheem, value=1)
        transmissibility[,,,,,,settings$ACUTE_STATES,] =
            transmissibility[,,,,,,settings$ACUTE_STATES,] * acute.transmissibility.rr[,,,,,,settings$ACUTE_STATES,]
        sexual.transmissibility = idu.transmissibility = transmissibility
        
        idu.transmissibility[,,,,,settings$DIAGNOSED_STATES,,] =
            idu.transmissibility[,,,,,settings$DIAGNOSED_STATES,,] * components$diagnosed.needle.sharing.rr
        
        sexual.transmissibility[,,,'heterosexual_male',,settings$DIAGNOSED_STATES,,] =
            sexual.transmissibility[,,,'heterosexual_male',,settings$DIAGNOSED_STATES,,] * components$diagnosed.het.male.condomless.rr
        sexual.transmissibility[,,,'female',,settings$DIAGNOSED_STATES,,] =
            sexual.transmissibility[,,,'female',,settings$DIAGNOSED_STATES,,] * components$diagnosed.female.condomless.rr
        sexual.transmissibility[,,,'msm',,settings$DIAGNOSED_STATES,,] =
            sexual.transmissibility[,,,'msm',,settings$DIAGNOSED_STATES,,] * components$diagnosed.msm.condomless.rr
        
        if (components$model.idu)
        {
            for (race in names(components$idu.transmissibility.rr.by.race))
                idu.transmissibility[,race,,,,,,] = idu.transmissibility[,race,,,,,,] * components$idu.transmissibility.rr.by.race[race]
        }
        for (race in names(components$sexual.transmissibility.rr.by.race))
            sexual.transmissibility[,race,,,,,,] = sexual.transmissibility[,race,,,,,,] * components$sexual.transmissibility.rr.by.race[race]
        
        #-- Pull year-specific suppression --#
        
        
        suppression = calculate.suppression(components)  
        
        
        #-- Put them together --#
        components$sexual.transmissibilities = lapply(suppression$rates, function(suppressed.proportions){
            
            sexual.transmissibility.for.year = sexual.transmissibility
            
            expanded.unsuppressed = 1 - suppressed.proportions[,,,,,settings$DIAGNOSED_STATES,,]
            
            sexual.transmissibility.for.year[,,,,,settings$DIAGNOSED_STATES,,] =
                sexual.transmissibility.for.year[,,,,,settings$DIAGNOSED_STATES,,] * expanded.unsuppressed
            
            sexual.transmissibility.for.year
        })
        components$sexual.transmissibility.years = suppression$times
        
        if (components$model.idu)
        {
            components$idu.transmissibilities = lapply(suppression$rates, function(suppressed.proportions){
                
                idu.transmissibility.for.year = idu.transmissibility
                
                expanded.unsuppressed = 1 - suppressed.proportions[,,,,,settings$DIAGNOSED_STATES,,]
                
                idu.transmissibility.for.year[,,,,,settings$DIAGNOSED_STATES,,] =
                    idu.transmissibility.for.year[,,,,,settings$DIAGNOSED_STATES,,] * expanded.unsuppressed
                
                idu.transmissibility.for.year
            })
            components$idu.transmissibility.years = suppression$times
        }
    }
    else
    {
        components$sexual.transmissibilities = components$idu.transmissibilities = list(0)
        components$sexual.transmissibility.years = components$idu.transmissibility.years = -Inf
    }
    
    components
}

do.setup.sexual.contact <- function(components)
{
    settings = get.components.settings(components)
    
    if (components$model.hiv.transmission)
    {
        #-- Sex by age --#
        sexual.transmission.by.age = get.contact.array.skeleton(components$jheem, age=T, sex.to=T)
        
        # Set up full census ages
        ages = min(settings$AGE_CUTOFFS):max(as.numeric(dimnames(components$populations$full.age.collapsed.races)[['age']]))
        age.counts = apply(components$populations$full.age.collapsed.races, 'age', sum)[as.character(ages)]
        age1.raw.counts = age.counts[1:components$jheem$age$spans[1]]
        
        # reduce per sexual availability
        sexual.availability = unlist(components$sexual.availability, use.names=F)
        names(sexual.availability) = unlist(sapply(components$sexual.availability, function(x){names(x)}))
        age.counts[names(sexual.availability)] = age.counts[names(sexual.availability)] * sexual.availability
        age.1.available.counts = age.counts[1:components$jheem$age$spans[1]]
        
        het.male.sex.by.age = get.age.proportions.from.age.model(components$sexual.transmission$heterosexual.male.age.model,
                                                                 ages.to.use=ages,
                                                                 age.counts=age.counts,
                                                                 age.cutoffs = settings$AGE_CUTO)
        
        female.sex.by.age = get.age.proportions.from.age.model(components$sexual.transmission$female.age.model,
                                                               ages.to.use=ages,
                                                               age.counts=age.counts,
                                                               age.cutoffs = settings$AGE_CUTOFFS)
        msm.sex.by.age = get.age.proportions.from.age.model(components$sexual.transmission$msm.age.model,
                                                            ages.to.use=ages,
                                                            age.counts=age.counts,
                                                            age.cutoffs = settings$AGE_CUTOFFS)
        
        
        sexual.transmission.by.age[,,'heterosexual_male'] = het.male.sex.by.age
        sexual.transmission.by.age[,,'female'] = female.sex.by.age
        sexual.transmission.by.age[,,'msm'] = msm.sex.by.age
        
        #inflate the transmissibility for 1st age bracket
        sexual.transmission.by.age[1,,] = sexual.transmission.by.age[1,,] *
            sum(age1.raw.counts) / sum(age.1.available.counts)
        
        
        #-- Sex by Race --#
        
        race.counts = apply(components$populations$collapsed.races, 'race', sum)
        sexual.transmission.by.race = get.bho.race.mixing.proportions.from.parameters(oe.black.black = components$sexual.transmission$black.black.oe,
                                                                                      oe.hispanic.hispanic = components$sexual.transmission$hispanic.hispanic.oe,
                                                                                      oe.other.other = components$sexual.transmission$other.other.oe,
                                                                                      race.proportions = race.counts)
        
        #-- Sex by Sex --#
        
        population = stratify.males.to.msm.by.race(components$populations$collapsed,
                                                   components$proportions.msm.of.male)
        sex.counts = apply(population, 'sex', sum)
        fraction.male.male.that.are.with.msm = sex.counts['msm'] / (sex.counts['msm'] + sex.counts['heterosexual_male'] * components$sexual.transmission$fraction.heterosexual.male.pairings.with.male)
        
        mat = array(0, dim=c(sex.from=length(settings$SEXES), sex.to=length(settings$SEXES)),
                    dimnames=list(sex.from=settings$SEXES, sex.to=settings$SEXES))
        
        #females
        mat['heterosexual_male','female'] = sex.counts['heterosexual_male']
        mat['msm','female'] = sex.counts['msm'] * components$sexual.transmission$oe.female.pairings.with.msm
        
        #msm
        mat['female','msm'] = components$sexual.transmission$fraction.msm.pairings.with.female
        mat['msm','msm'] = (1-components$sexual.transmission$fraction.msm.pairings.with.female) * fraction.male.male.that.are.with.msm
        mat['heterosexual_male','msm'] = (1-components$sexual.transmission$fraction.msm.pairings.with.female) * (1-fraction.male.male.that.are.with.msm)
        
        #heterosexual males
        mat['msm','heterosexual_male'] = components$sexual.transmission$fraction.heterosexual.male.pairings.with.male *
            fraction.male.male.that.are.with.msm
        mat['heterosexual_male','heterosexual_male'] = components$sexual.transmission$fraction.heterosexual.male.pairings.with.male *
            (1-fraction.male.male.that.are.with.msm)
        mat['female','heterosexual_male'] = 1 - components$sexual.transmission$fraction.heterosexual.male.pairings.with.male
        
        sexual.transmission.by.sex = calculate.column.proportions(mat)
        
        #-- Sex by IDU --#
        if (components$model.idu)
        {
            population = stratify.population.idu(population,
                                                 active.idu.prevalence=components$active.idu.prevalence,
                                                 idu.ever.prevalence=components$idu.ever.prevalence)
            
            #set up the mixing matrix
            n.risk.states = length(settings$RISK_STRATA)
            sexual.transmission.by.risk = get.contact.array.skeleton(components$jheem,
                                                                     value=0,
                                                                     age.to=T, race.to=T, sex.to=T, risk=T)
            for (risk.from in 1:n.risk.states)
            {
                for (risk.to in 1:n.risk.states)
                    sexual.transmission.by.risk[risk.from,,,,risk.to] = population[,,,risk.from]
            }
            
            #plug in the oe ratios
            sexual.transmission.by.risk['never_IDU',,,,'never_IDU'] = components$never.idu.sexual.oe *
                sexual.transmission.by.risk['never_IDU',,,,'never_IDU']
            
            idu.states = setdiff(settings$RISK_STRATA, 'never_IDU')
            
            sexual.transmission.by.risk[idu.states,,,,'never_IDU'] = components$never.with.idu.sexual.oe *
                sexual.transmission.by.risk[idu.states,,,,'never_IDU']
            
            sexual.transmission.by.risk[idu.states,,,,idu.states] = components$idu.sexual.oe *
                sexual.transmission.by.risk[idu.states,,,,idu.states]
            
            #normalize to proportions
            sexual.transmission.by.risk = sexual.transmission.by.risk /
                rep(colSums(sexual.transmission.by.risk, dims=1), each=n.risk.states)
        }
        else
            sexual.transmission.by.risk = 1
        
        #-- Set up the ingredients for transmission for sex act --#
        
        sexual.transmission.arrays = get.sexual.transmission.arrays(components)
        
        components$sexual.contact.years = sexual.transmission.arrays$times
        
        # Set up the time-specific transmission per sex act, and put together with the rest
        components$sexual.contact.arrays = lapply(1:length(components$sexual.contact.years), function(i){
            
            # Put it all together
            create.contact.array.from.marginals(components$jheem,
                                                sexual.transmission.arrays$rates[[i]],
                                                sexual.transmission.by.age,
                                                sexual.transmission.by.risk,
                                                sexual.transmission.by.race,
                                                sexual.transmission.by.sex)
            
        })
        
        if (!is.null(components$foreground.sexual.transmission))
        {
            rates.and.times = do.get.rates.from.background.and.foreground(background.rates=components$sexual.contact.arrays,
                                                                          background.times=components$sexual.contact.years,
                                                                          background.data.type = 'rate',
                                                                          foreground = components$foreground.sexual.transmission,
                                                                          max.background.time = Inf)
            
            if (1==2) #for debugging
            {
                r.new = sapply(rates.and.times$rates, function(r){
                    r[3,'black','female',1,3,'black','heterosexual_male',1]
                })
                names(r.new) = rates.and.times$times
                qplot(rates.and.times$times, r.new, geom='line')
                
                r.old = sapply(components$sexual.contact.arrays, function(r){
                    r[3,'black','female',1,3,'black','heterosexual_male',1]
                })
                names(r.old) = components$sexual.contact.years
                
                y = intersect(names(r.new), names(r.old))
                r.new[y] / r.old[y]
                
                type=c(rep('new',length(rates.and.times$times)), rep('old', length(components$sexual.contact.years)))
                qplot(c(rates.and.times$times, components$sexual.contact.years),
                      c(r.new, r.old), 
                      color=type, geom='line'
                      ) + ylim(0,NA)
            }
            
            components$sexual.contact.arrays = rates.and.times$rates
            components$sexual.contact.years = rates.and.times$times
        }
    }
    else
    {
        components$sexual.contact.arrays = list(0)
        components$sexual.contact.years = -Inf
    }
    
    components
}

get.sexual.transmission.arrays <- function(components,
                                           idu.applies.to.in.remission=F)
{
    #   sexual.transmission.skeleton = get.contact.array.skeleton(components$jheem, race=T, sex=T, age=T, risk=T)
    #   sexual.transmission.mapping = get.contact.array.skeleton(components$jheem, race=T, sex=T, age=T, risk=T, '')
    sexual.transmission.skeleton = get.contact.array.skeleton(components$jheem, race=T, sex=T, age=T)
    sexual.transmission.mapping = get.contact.array.skeleton(components$jheem, race=T, sex=T, age=T, '')
    sexual.transmission.mapping[] = as.character(NA)
    
    rates.and.times.list = list()
    
    #    if (idu.applies.to.in.remission)
    #        idu.strata = c('active_IDU','IDU_in_remission')
    #    else
    #        idu.strata = 'active_IDU'
    #    non.idu.strata = setdiff(c('never_IDU','active_IDU','IDU_in_remission'), idu.strata)
    
    for (race in components$jheem$race)
    {
        for (age.index in 1:length(components$jheem$age$labels))
        {
            age = paste0('age', age.index)
            
            msm.trates = calculate.changing.trates.and.times(components$msm.trates[[race]][[age]])
            #            msm.idu.trates = calculate.changing.trates.and.times(components$msm.idu.trates[[race]][[age]])
            
            heterosexual.male.trates = calculate.changing.trates.and.times(components$heterosexual.male.trates[[race]][[age]])
            heterosexual.female.trates = calculate.changing.trates.and.times(components$heterosexual.female.trates[[race]][[age]])
            
            msm.name = paste0(race, '.', age, '.msm')
            #            msm.idu.name = paste0(race, '.', age, '.msm.idu')
            het.male.name = paste0(race, '.', age, '.het.male')
            het.female.name = paste0(race, '.', age, '.female')
            
            to.add = list(msm.trates,
                          #                         msm.idu.trates,
                          heterosexual.male.trates,
                          heterosexual.female.trates)
            #           names(to.add) = c(msm.name, msm.idu.name, het.male.name, het.female.name)
            names(to.add) = c(msm.name, het.male.name, het.female.name)
            rates.and.times.list = c(rates.and.times.list,
                                     to.add)
            
            
            #            sexual.transmission.mapping[,,,,age.index,race,'msm',non.idu.strata] = msm.name
            #            sexual.transmission.mapping[,,,,age.index,race,'msm',idu.strata] = msm.idu.name
            #            sexual.transmission.mapping[,,,,age.index,race,'heterosexual_male',] = het.male.name
            #            sexual.transmission.mapping[,,,,age.index,race,'female',] = het.female.name
            sexual.transmission.mapping[,,,age.index,race,'msm'] = msm.name
            sexual.transmission.mapping[,,,age.index,race,'heterosexual_male'] = het.male.name
            sexual.transmission.mapping[,,,age.index,race,'female'] = het.female.name
            
            #These lines lets msm transmission rates apply to heterosexual males engaging in sex with males
            sexual.transmission.mapping[,,c('heterosexual_male','msm'),age.index,race,'heterosexual_male'] = msm.name
            #            sexual.transmission.mapping[,,c('heterosexual_male','msm'),,age.index,race,'heterosexual_male',non.idu.strata] = msm.name
            #            sexual.transmission.mapping[,,c('heterosexual_male','msm'),,age.index,race,'heterosexual_male',idu.strata] = msm.idu.name
        }
    }
    
    sexual.transmission.arrays = array.merge.rates(skeleton.array = sexual.transmission.skeleton,
                                                   rates.and.times.list = rates.and.times.list,
                                                   array.indices.into.list = sexual.transmission.mapping)
    
    sexual.transmission.arrays
}

do.setup.idu.contact <- function(components)
{
    settings = get.components.settings(components)
    
    if (components$model.hiv.transmission && components$model.idu)
    {
        #-- SELECT OUT JUST IDU --#
        idu.to.idu = get.contact.array.skeleton(components$jheem, risk=T)
        idu.to.idu[settings$ACTIVE_IDU_STATE, settings$ACTIVE_IDU_STATE] = 1
        
        #-- IDU by age --#
        ages = min(settings$AGE_CUTOFFS):max(as.numeric(dimnames(components$populations$full.age.collapsed.races)[['age']]))
        age.counts = apply(components$populations$full.age.collapsed.races, 'age', sum)[as.character(ages)]
        
        # reduce per idu availability
        idu.availability = unlist(components$idu.availability, use.names=F)
        names(idu.availability) = unlist(sapply(components$idu.availability, function(x){names(x)}))
        
        age.counts[names(idu.availability)] = age.counts[names(idu.availability)] * idu.availability
        
        idu.transmission.by.age = get.age.proportions.from.age.model(age.model=components$idu.transmission$age.model,
                                                                     ages.to.use=ages,
                                                                     age.counts=age.counts,
                                                                     age.cutoffs = settings$AGE_CUTOFFS)
        
        #-- IDU by race --#
        race.counts = apply(components$populations$collapsed.races, 'race', sum)
        idu.transmission.by.race = get.bho.race.mixing.proportions.from.parameters(oe.black.black = components$idu.transmission$black.black.oe,
                                                                                   oe.hispanic.hispanic = components$idu.transmission$hispanic.hispanic.oe,
                                                                                   oe.other.other = components$idu.transmission$other.other.oe,
                                                                                   race.proportions = race.counts)
        
        #-- IDU by sex --#
        population = stratify.males.to.msm.by.race(components$populations$collapsed,
                                                   components$proportions.msm.of.male)
        sex.counts = apply(population, 'sex', sum)
        idu.transmission.by.sex = get.pairing.proportions(components$idu.transmission$sex.oes, sex.counts)
        
        #-- IDU Transmission --#
        
        idu.transmission.arrays = get.idu.transmission.arrays(components)
        
        if (!is.null(components$foreground.idu.transmission))
        {
            idu.transmission.arrays = do.get.rates.from.background.and.foreground(background.rates=idu.transmission.arrays$rates,
                                                                                  backgrond.times=idu.transmission.arrays$times,
                                                                                  background.data.type = 'rate',
                                                                                  foreground = components$foreground.idu.transmission,
                                                                                  max.background.time = Inf)
            
        }
        
        #-- Put it all together and set it --#
        components$idu.contact.arrays = lapply(idu.transmission.arrays$rates, function(idu.transmission)
        {
            create.contact.array.from.marginals(components$jheem,
                                                idu.transmission,
                                                idu.to.idu,
                                                idu.transmission.by.age,
                                                idu.transmission.by.race,
                                                idu.transmission.by.sex)
        })
        
        components$idu.contact.years = idu.transmission.arrays$times
    }
    else
    {
        components$idu.contact.arrays = list(0)
        components$idu.contact.years = -Inf
    }
    
    components
}


get.idu.transmission.arrays <- function(components)
{
    idu.transmission.skeleton = get.contact.array.skeleton(components$jheem, race=T, sex=T, age=T)
    idu.transmission.mapping = get.contact.array.skeleton(components$jheem, race=T, sex=T, age=T, '')
    idu.transmission.mapping[] = as.character(NA)
    
    rates.and.times.list = list()
    
    for (race in components$jheem$race)
    {
        for (age.index in 1:length(components$jheem$age$labels))
        {
            age = paste0('age', age.index)
            
            msm.trates = calculate.changing.trates.and.times(components$idu.msm.trates[[race]][[age]])
            heterosexual.male.trates = calculate.changing.trates.and.times(components$idu.male.trates[[race]][[age]])
            female.trates = calculate.changing.trates.and.times(components$idu.female.trates[[race]][[age]])
            
            msm.name = paste0(race, '.', age, '.msm')
            het.male.name = paste0(race, '.', age, '.het.male')
            female.name = paste0(race, '.', age, '.female')
            
            to.add = list(msm.trates,
                          heterosexual.male.trates,
                          female.trates)
            names(to.add) = c(msm.name, het.male.name, female.name)
            rates.and.times.list = c(rates.and.times.list,
                                     to.add)
            
            
            idu.transmission.mapping[,,,age.index,race,'msm'] = msm.name
            idu.transmission.mapping[,,,age.index,race,'heterosexual_male'] = het.male.name
            idu.transmission.mapping[,,,age.index,race,'female'] = female.name
        }
    }
    
    idu.transmission.arrays = array.merge.rates(skeleton.array = idu.transmission.skeleton,
                                                rates.and.times.list = rates.and.times.list,
                                                array.indices.into.list = idu.transmission.mapping)
    
    idu.transmission.arrays
}



do.setup.new.infection.proportions <- function(components)
{
    settings = get.components.settings(components)
    
    if (!components$model.hiv.transmission)
    {
        components$new.infection.proportions = list(get.uniform.new.infection.proportions(components$jheem))
        components$new.infection.proportions.years = -Inf
    }
    else if (components$model.prep)
    {
        if (is.null(components$prep.rates.and.times))
            components = do.calculate.prep.coverage(components)
        
        prep.rates.and.times = calculate.aggregate.prep.coverage.and.risk(components)

        components$new.infection.proportions = lapply(1:length(prep.rates.and.times$times), function(i){
            
            prep.coverage = prep.rates.and.times$coverage[[i]]
            prep.risk = prep.rates.and.times$risk[[i]]
            
            non.prep.risk = (1-prep.coverage)
          #  prep.risk = prep.coverage * prep.rrs
#            prep.risk = prep.coverage * prep.rrs['heterosexual']
 #           prep.risk[,,,'msm',,] = prep.coverage[,,,'msm',,] * prep.rrs['msm']
  #          prep.risk[,,,,'active_IDU',] = prep.coverage[,,,,'active_IDU',] * prep.rrs['idu']
            
            new.infection.proportions = get.new.infection.proportions.skeleton(components$jheem)
            
            #            access(new.infection.proportions, continuum = settings$UNDIAGNOSED_NO_PREP, cd4 = settings$CD4_STRATA[1]) =
            #                non.prep.risk / (prep.risk + non.prep.risk)
            new.infection.proportions[,,,,,,settings$UNDIAGNOSED_NO_PREP,settings$CD4_STRATA[1],] = non.prep.risk / (prep.risk + non.prep.risk)
            #            access(new.infection.proportions, continuum = settings$UNDIAGNOSED_FROM_PREP, cd4 = settings$CD4_STRATA[1]) =
            #                prep.risk / (prep.risk + non.prep.risk)
            new.infection.proportions[,,,,,,settings$UNDIAGNOSED_FROM_PREP,settings$CD4_STRATA[1],] = prep.risk / (prep.risk + non.prep.risk)
            
            new.infection.proportions
        })
        
        components$new.infection.proportions.years = prep.rates.and.times$times
    }
    else
    {
        components$new.infection.proportions = list(get.uniform.new.infection.proportions(components$jheem, initial.continuum = settings$UNDIAGNOSED_NO_PREP))
        components$new.infection.proportions.years = -Inf
    }
    
    components
}

##-------------------------------------------##
##-- THE GENERAL SET-UP TRANSITIONS HELPER --##
##-------------------------------------------##

do.setup.transitions <- function(components,
                                 subgroup,
                                 dimension)
{
  #  print(paste0("Starting setup of transitions for '", dimension, "' ('", subgroup, "')"))
    
    if (all(names(TRANSITION.MAPPING.SCHEMA)!=dimension))
        stop(paste0("Have not set up Transition Mapping Schema for dimension of '", dimension, "'"))
    if (all(TRANSITION.MAPPING.SCHEMA[[dimension]]$subgroups != subgroup))
        stop(paste0("'", subgroup, "' is not a valid subgroup for dimension '", dimension, "'"))
    
    # list and calculate all the components of rates
    transition.mapping = get.components.transition.mapping(components)

    transitions = get.transitions(transition.mapping,
                                  subgroup=subgroup,
                                  dimension=dimension)
      
    rate.elements = get.transition.element.names(transitions)
    
    rate.components = list()
    rate.component.times = list()
    
    for (type in rate.elements)
    {
        if (!is.rate.calculated(components, type))
            components = do.calculate.rates(components, type=type)
        
        rates.and.times = calculate.rates(components, type=type)
        rate.components[[type]] = rates.and.times$rates
        rate.component.times[[type]] = rates.and.times$times
    }
    
    # merge all rates for times
#    all.times = sort(unique(unlist(rate.component.times)))
    all.times = union_sorted_vectors(rate.component.times)
    rate.components = lapply(1:length(rate.components), function(i){
        rv = interpolate.parameters(values=rate.components[[i]],
                                    value.times=rate.component.times[[i]],
                                    desired.times = all.times)
        rv
    })
    names(rate.components) = rate.elements
    
    # set up the skeleton transition array
    
    if (subgroup=='hiv.negative')
        trans.skeleton = get.hiv.negative.transition.array.skeleton(components$jheem,
                                                                    transition.dimension = dimension)
    else
        trans.skeleton = get.hiv.positive.transition.array.skeleton(components$jheem,
                                                                    transition.dimension = dimension)
    dim.names = transition.dim.names = dimnames(trans.skeleton)
    transition.dim.names = transition.dim.names[names(transition.dim.names) != paste0(dimension, '.from') &
                                                    names(transition.dim.names) != paste0(dimension, '.to')]
    
    # iterate through each time
    
    masks.for.transitions = lapply(transitions, function(transition){
        
        mask = get.two.dim.access.indices(dim.names = dim.names,
                                          dim1 = paste0(dimension, '.from'),
                                          dim.value1 = transition$from,
                                          dim2 = paste0(dimension, '.to'),
                                          dim.value2 = transition$to)
    })
    
    rates = lapply(1:length(all.times), function(i){
        
        rv = trans.skeleton
        for (j in 1:length(transitions))
        {
            transition = transitions[[j]]
            
            #   map the rates for that time to a list
            rate.components.for.time = lapply(transition$element.names, function(elem){
                prepare.rate.components(rate.components[[elem]][[i]], 
                                        target.dim.names = transition.dim.names, 
                                        access.if.extra.dim=transition$from,
                                        allow.scalar = T)
            })
            names(rate.components.for.time) = transition$element.names
            
                
            #   resolve expressions/references
            t.rate = resolve.transition(transition, bindings=rate.components.for.time)
            
            #   plug in
            rv[ masks.for.transitions[[j]] ] = t.rate
            
#            if (dimension=='continuum')
 #               rv[,,,,,transition$from,,,transition$to] = t.rate
  #          else
   #             stop("Have not yet implemented do.setup.transitions except for dimension=continuum")
        }
        
        rv
    })
    
    
    # package up and return the updated components
    
    comps.names = get.transition.component.names(dimension, subgroup=subgroup)
    
    components[[comps.names$name]] = rates
    components[[comps.names$years.name]] = all.times
    
  #  print(paste0("Finished setup of transitions for '", dimension, "' ('", subgroup, "')"))
    
    components
}

get.transition.component.names <- function(dimension,
                                          subgroup)
{
    if (length(dimension) != 1 || is.na(dimension) || dimension=='')
        stop("dimension must be a non-NA, non-empty single character value")
    
    rv = list()
    
    if (length(TRANSITION.MAPPING.SCHEMA[[dimension]]$subgroups)>1)
    {
        if (length(subgroup)>1)
            stop("'subgroup' must be a single value")
        
        rv$name = paste0(dimension, '.transitions.', subgroup)
        rv$years.name = paste0(rv$name, '.years')
    }
    else
    {
        rv$name = paste0(dimension, ".transitions")
        rv$years.name = paste0(dimension, ".transition.years")
    }
    
    rv
}

# the extra dim stuff is for backwards compatibility
# older components objects (collapsed_1.0 were full arrays)
prepare.rate.components <- function(rate.components,
                                    target.dim.names,
                                    access.if.extra.dim,
                                    allow.scalar)
{
    if (allow.scalar && length(rate.components)==1)
        rate.components
    else
    { 
        extra.dim = setdiff(names(dimnames(rate.components)), names(target.dim.names))
        if (length(extra.dim)==0)
            rv = rate.components
        else if (length(extra.dim)==1)
            rv = single.dim.access(rate.components, extra.dim, access.if.extra.dim, allow.unoptimized = F)
        else
            stop("Cannot handle rate.components array - too many dimensions")
        
        rv = expand.population(rv, target.dim.names=target.dim.names)
    }
}



##-------------##
##-- HELPERS --##
##-------------##

# Convenience wrapper for get.rates.from.background.and.foreground
# Pulls foreground elements from a list
do.get.rates.from.background.and.foreground <- function(background.rates,
                                                        background.times,
                                                        background.data.type,
                                                        foreground,
                                                        max.background.time,
                                                        transform.foreground.rates=function(x){x},
                                                        type='unspecified' #for debugging
                                                        )
{
    if (is.null(max.background.time))
        max.background.time = Inf

    if (is.null(foreground$foreground.scale))
        foreground.data.types=as.list(rep(background.data.type, length(foreground$years)))
    else
        foreground.data.types=foreground$foreground.scale
        
    get.rates.from.background.and.foreground(background.rates = background.rates,
                                             background.times = background.times,
                                             background.data.type = background.data.type,
                                             foreground.rates = foreground$rates,
                                             foreground.times = foreground$years,
                                             foreground.start.times = foreground$start.years,
                                             foreground.end.times = foreground$end.years,
                                             foreground.data.types = foreground.data.types,
                                             foreground.functions = foreground$apply.functions,
                                             max.background.time = max.background.time,
                                             allow.foreground.less = foreground$allow.foreground.less,
                                             allow.foreground.greater = foreground$allow.foreground.greater,
                                             foreground.min = foreground$foreground.min,
                                             foreground.max = foreground$foreground.max,
                                             type=type)
}

#allows multiple foreground settings by recursing on this lists in foreground.rates, foreground.times, etc
get.rates.from.background.and.foreground <- function(background.rates,
                                                     background.times,
                                                     background.data.type,
                                                     foreground.rates,
                                                     foreground.times,
                                                     foreground.start.times,
                                                     foreground.end.times,
                                                     foreground.data.types,
                                                     max.background.time=Inf,
                                                     allow.foreground.less,
                                                     allow.foreground.greater,
                                                     foreground.functions,
                                                     foreground.min,
                                                     foreground.max,
                                                     type='unspecified' #for debugging
                                                     )
{
    if (is.null(foreground.times) || length(foreground.times)==0)
        list(rates=background.rates[background.times<=max.background.time],
             times=background.times[background.times<=max.background.time],
             data.type=background.data.type)
    else
    {
        if (is.null(foreground.rates) || is.null(foreground.rates[[1]]))
            stop("foreground.rates is NULL")
        
        this.foreground.rates = foreground.rates[[1]]
        this.foreground.times = foreground.times[[1]]
        this.foreground.start.times = foreground.start.times[[1]]
        this.foreground.end.times = foreground.end.times[[1]]
        this.foreground.data.type = foreground.data.types[[1]]
        this.allow.foreground.less = allow.foreground.less[[1]]
        this.allow.foreground.greater = allow.foreground.greater[[1]]
        this.foreground.functions = foreground.functions[[1]]
        this.foreground.min = foreground.min[[1]]
        this.foreground.max = foreground.max[[1]]
        
        background.rates = background.rates[background.times<=max.background.time]
        background.times = background.times[background.times<=max.background.time]
        
        if (background.data.type != this.foreground.data.type)
            background.rates = convert.transition.element.type(background.rates,
                                                               convert.from.type = background.data.type,
                                                               convert.to.type = this.foreground.data.type)
        
        # this could be optimized - we'd have to do better on the foreground.start.times and end.times
        all.times = sort(unique(c(background.times, 
                                  this.foreground.times, 
                                  as.numeric(this.foreground.start.times), 
                                  as.numeric(this.foreground.end.times))))
        all.times = all.times[all.times<Inf]
        
        interpolated.background.rates = interpolate.parameters(values=background.rates,
                                                               value.times=background.times,
                                                               desired.times = all.times)
        
        names(interpolated.background.rates) = as.character(all.times)
        names(this.foreground.rates) = as.character(this.foreground.times)
        
        dim.names = dimnames(this.foreground.rates[[1]])
        
        sub.rates = numeric(length(all.times))
        names(sub.rates) = as.character(all.times)
        
        raw.rates = sapply(1:length(this.foreground.rates[[1]]), function(i){
            bg.rates = sub.rates = sapply(interpolated.background.rates, function(bg){bg[i]}) #start off with the background rates
            
            if (is.na(this.foreground.end.times[i]))
                from.foreground.times = as.character(this.foreground.times[this.foreground.times > this.foreground.start.times[i]])
            else
                from.foreground.times = as.character(this.foreground.times[this.foreground.times > this.foreground.start.times[i] & 
                                                                            this.foreground.times < this.foreground.end.times[i]])
            
            
            if (length(from.foreground.times)>0)
            {
                from.foreground.rates = sapply(this.foreground.rates[from.foreground.times], function(fg){fg[i]})
                use.background.not.foreground = from.foreground.rates == -Inf
                from.foreground.times = from.foreground.times[!use.background.not.foreground]
                from.foreground.rates = from.foreground.rates[!use.background.not.foreground]
                
                if (length(from.foreground.times)>0)
                {
                    background.at.from.foreground = bg.rates[from.foreground.times]
    
    #                if (is(foreground.function, 'function'))
     #                   from.foreground.rates = foreground.function(from.foreground.rates, background.at.from.foreground)
                    if (is.na(this.foreground.functions[i]) || this.foreground.functions[i]=='absolute')
                    {}
                    else if (this.foreground.functions[i]=='multiplier')
                        from.foreground.rates = from.foreground.rates * background.at.from.foreground
                    else if (this.foreground.functions[i]=='odds.ratio')
                    {
                        odds = background.at.from.foreground / (1-background.at.from.foreground) * from.foreground.rates
                        from.foreground.rates = odds / (1 + odds)
                    }
                    else if (this.foreground.functions[i]=='additive')
                        from.foreground.rates = from.foreground.rates + background.at.from.foreground
                    else 
                        stop("foreground functions must be one of: 'absolute', 'multiplier', 'odds.ratio', or 'additive'")
                    
                    if (!is.null(this.foreground.min) && !is.na(this.foreground.min[i]))
                        from.foreground.rates[!is.na(from.foreground.rates)] = pmax(from.foreground.rates[!is.na(from.foreground.rates)],
                                                                                    this.foreground.min[i])
                    if (!is.null(this.foreground.max) && !is.na(this.foreground.max[i]))
                        from.foreground.rates[!is.na(from.foreground.rates)] = pmin(from.foreground.rates[!is.na(from.foreground.rates)],
                                                                                    this.foreground.max[i])
                    
                    if (any(!is.na(from.foreground.rates)))
                    {
                        #sub.rates[all.times>=min(from.foreground.times)] = NA #this was an error
                        
                        if (is.na(this.foreground.end.times[i]))
                            sub.rates[all.times > this.foreground.start.times[i]] = NA
                        else
                            sub.rates[all.times > this.foreground.start.times[i] & all.times < this.foreground.end.times[i]] = NA
                        
                        sub.rates[from.foreground.times] = from.foreground.rates
                        
                        if (any(is.na(sub.rates)))
                        {
                            to.interpolate.times = all.times[is.na(sub.rates)]
                            sub.rates[is.na(sub.rates)] = interpolate.parameters(values=sub.rates[!is.na(sub.rates)],
                                                                                 value.times=all.times[!is.na(sub.rates)],
                                                                                 desired.times=to.interpolate.times,
                                                                                 return.list = F)
    #                        bg.time = foreground.start.times[i]
    #                        fg.time = min(all.times[!is.na(sub.rates) & all.times>bg.time])
    #                        sub.rates[is.na(sub.rates)] = interpolate.parameters(values=sub.rates[as.character(c(bg.time,fg.time))],
    #                                                                             value.times=c(bg.time,fg.time),
    #                                                                             desired.times=to.interpolate.times,
    #                                                                             return.list=F)
                        }
                    }
                }
            }
            
            if (!this.allow.foreground.less[i])
                sub.rates[sub.rates < bg.rates] = bg.rates[sub.rates < bg.rates]
            if (!this.allow.foreground.greater[i])
                sub.rates[sub.rates > bg.rates] = bg.rates[sub.rates > bg.rates]
            
            sub.rates
        })
        
        rates = lapply(1:length(all.times), function(i){
            arr = raw.rates[i,]
            dim(arr) = sapply(dim.names, length)
            dimnames(arr) = dim.names
            arr
        })
        
        if (length(foreground.times)>1)
        {
            # The recursive call
            get.rates.from.background.and.foreground(background.rates=rates,
                                                     background.times=all.times,
                                                     foreground.rates=foreground.rates[-1],
                                                     foreground.times=foreground.times[-1],
                                                     foreground.start.times=foreground.start.times[-1],
                                                     foreground.end.times=foreground.end.times[-1],
                                                     foreground.data.types=foreground.data.types[-1],
                                                     max.background.time=Inf,
                                                     allow.foreground.less=allow.foreground.less[-1],
                                                     allow.foreground.greater=allow.foreground.greater[-1],
                                                     foreground.functions=foreground.functions[-1],
                                                     foreground.min = foreground.min[-1],
                                                     foreground.max = foreground.max[-1])
                
        }
        else
        {
            list(rates=rates,
                 times=all.times,
                 data.type=this.foreground.data.type)
        }
    }
}


merge.rates <- function(rates1,
                        times1,
                        rates2,
                        times2)
{
    rv = list()
    
#    rv$times = sort(unique(c(times1,times2)))
    rv$times = union_sorted_vectors(list(times1, times2))
    
    interpolate.fn = function(times, rates){
        lapply(rv$times, function(time){
            n.times = length(times)
            if (n.times==1)
                return (rates[[1]])
            
            index.before = (1:n.times)[times <= time]
            index.before = index.before[length(index.before)]
            index.after = (1:n.times)[times > time][1]
            if (length(index.before)==0)
                rates[[index.after]]
            else if (is.na(index.after) || is.infinite(times[index.before]))
                rates[[index.before]]
            else if (is.infinite(times[index.after]))
                rates[[index.after]]
            else
                (rates[[index.before]] * (times[index.after] - time) +
                     rates[[index.after]] * (time - times[index.before])) /
                (times[index.after] - times[index.before])
        })}
    
    rv$rates1 = interpolate.fn(times1, rates1)
    rv$rates2 = interpolate.fn(times2, rates2)
    
    rv
}

array.merge.rates <- function(skeleton.array,
                              rates.and.times.list,
                              array.indices.into.list)
{
    rv = list()
#    rv$times = unique(sort(unlist(sapply(rates.and.times.list, function(rt){rt$times}))))
    rv$times = union_sorted_vectors(lapply(rates.and.times.list, function(rt){rt$times}))
    
    interpolate.fn = function(times, rates){
        lapply(rv$times, function(time){
            n.times = length(times)
            if (n.times==1)
                return (rates[[1]])
            
            index.before = (1:n.times)[times <= time]
            index.before = index.before[length(index.before)]
            index.after = (1:n.times)[times > time][1]
            
            if (length(index.before)==0)
                rates[[index.after]]
            else if (is.na(index.after) || is.infinite(times[index.before]))
                rates[[index.before]]
            else if (is.infinite(times[index.after]))
                rates[[index.after]]
            else
                (rates[[index.before]] * (times[index.after] - time) +
                     rates[[index.after]] * (time - times[index.before])) /
                (times[index.after] - times[index.before])
        })}
    
    interpolated.rates = lapply(rates.and.times.list, function(rt){
        interpolate.fn(times=rt$times, rates=rt$rates)
    })
    
    rv$rates = lapply(1:length(rv$times), function(t){
        m = skeleton.array
        for (i in 1:length(array.indices.into.list))
        {
            if (!is.na(array.indices.into.list[i]))
                m[i] = interpolated.rates[[array.indices.into.list[i]]][[t]]
        }
        
        dim(m) = dim(skeleton.array)
        dimnames(m) = dimnames(skeleton.array)
        m
    })
    
    rv
}

merge.three.rates <- function(rates1,
                              times1,
                              rates2,
                              times2,
                              rates3,
                              times3)
{
    rv = list()
#    rv$times = sort(unique(c(times1,times2,times3)))
    rv$times = union_sorted_vectors(list(times1, times2, times3))
       
    interpolate.fn = function(times, rates){
        lapply(rv$times, function(time){
            n.times = length(times)
            if (n.times==1)
                return (rates[[1]])
            
            index.before = (1:n.times)[times <= time]
            index.before = index.before[length(index.before)]
            index.after = (1:n.times)[times > time][1]
            
            if (length(index.before)==0)
                rates[[index.after]]
            else if (is.na(index.after) || is.infinite(times[index.before]))
                rates[[index.before]]
            else if (is.infinite(times[index.after]))
                rates[[index.after]]
            else
                (rates[[index.before]] * (times[index.after] - time) +
                     rates[[index.after]] * (time - times[index.before])) /
                (times[index.after] - times[index.before])
        })}
    
    rv$rates1 = interpolate.fn(times1, rates1)
    rv$rates2 = interpolate.fn(times2, rates2)
    rv$rates3 = interpolate.fn(times3, rates3)
    
    rv
}

