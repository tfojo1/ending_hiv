#source('../code/setup/setup_helpers.R')
#source('../code/setup/logit_transformations.R')

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
        components = do.calculate.testing.rates(components)
    if (is.null(components$prep.rates.and.times))
        components = do.calculate.prep.coverage(components)
    
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
                jheem = set.new.infection.proportions(jheem,
                                                      proportions=components$new.infection.proportions[[i]],
                                                      time=components$new.infection.proportions.years[i])
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

clear.dependent.values <- function(components,
                                   dependent.on)
{
    all.dependent.names = c('jheem',
                            'initial.population',
                            'birth.proportions',
                            'general.mortality',
                            'hiv.mortality.rates',
                            'hiv.mortality.years',
                            'idu.transitions',
                            'cd4.transitions',
                            'continuum.transitions',
                            'continuum.transition.years',
                            'sexual.susceptiblity',
                            'idu.susceptibility',
                            'susceptibility',
                            'susceptibility.years',
                            'sexual.transmissibilities',
                            'sexual.transmissibility.years',
                            'idu.transmissibilities',
                            'idu.transmissibility.years',
                            'sexual.contact.arrays',
                            'idu.contact.arrays',
                            'new.infection.proportions',
                            'new.infection.proportions.years',
                            'fertility',
                            'fertility.check',
                            'global.sexual.transmission.rates',
                            'global.idu.transmission.rates',
                            'fix.strata.sizes.check',
                            'track.mortality.check',
                            'sexual.susceptibility',
                            'aging.rates.hiv.positive',
                            'suppression.rates.and.times',
                            'testing.rates.and.times',
                            'prep.rates.and.times')
    
    dependencies = list(model.idu=all.dependent.names,
                        model.hiv.transmission=all.dependent.names,
                        model.prep=all.dependent.names,
                        populations=all.dependent.names,
                        seed.rate.per.stratum='initial.population',
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
                        idu.remission='idu.transitions',
                        idu.relapse='idu.transitions',
                        acute.hiv.duration='cd4.transitions',
                        aids.progression.rate='cd4.transitions',
                        cd4.recovery.rate='cd4.transitions',
                        prep.screening.frequency='continuum.transitions',
                        background.testing.proportions=c('continuum.transitions','testing.rates.and.times'),
                        foreground.testing.proportions=c('continuum.transitions','testing.rates.and.times'),
                        background.testing.rate.ratios=c('continuum.transitions','testing.rates.and.times'),
                        background.suppression=c('suppression.rates.and.times','hiv.mortality.rates','sexual.transmissibilities','idu.transmissibilities'),
                        foreground.suppression=c('suppression.rates.and.times','hiv.mortality.rates','sexual.transmissibilities','idu.transmissibilities'),
                        background.prep.coverage=c('prep.rates.and.times','susceptibility','new.infection.proportions','new.infection.proportions.years'),
                        foreground.prep.coverage=c('prep.rates.and.times','susceptibility','new.infection.proportions','new.infection.proportions.years'),
                        prep.rr.heterosexual='sexual.susceptibility',
                        prep.rr.msm='sexual.susceptibility',
                        prep.rr.idu='idu.susceptibility',
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
                        idu.transmission='idu.contact.arrays',
                        susceptibility=c('sexual.susceptibility','idu.susceptibility'),
                        fertility='fertility.check',
                        global.sexual.transmission.rates='global.sexual.transmission.rates',
                        global.idu.transmission.rates='global.idu.transmission.rates',
                        fix.strata.sizes='fix.strata.sizes.check',
                        track.mortality='track.mortality.check',
                        aging=c('aging.rates.hiv.positive')
    )
    
    #An internal check
    missing.from.all = sapply(unlist(dependencies), function(dep){
        !any(all.dependent.names==dep)
    })
    if (any(missing.from.all))
        stop(paste0("The following are missing from 'all.dependent.names: ",
                    paste0("'", unique(unlist(dependencies)[missing.from.all]), "'", collapse=', ')))
    
    missing.dependency = sapply(dependent.on, function(dep){
        !any(names(dependencies)==dep) && !any(all.dependent.names==dep)
    })
    
    if (any(missing.dependency))
        stop("The following are not valid parameters for dependencies: ",
             paste0("'", dependent.on[missing.dependency], "'", collapse=', '))
    
    for (d in dependent.on)
    {
        #        if (any(all.dependent.names==d))
        #            d.dep = d
        #        else
        d.dep = dependencies[[d]]
        
        for (elem in d.dep)
        {
            if (components$fixed && !is.null(components[[elem]]))
                stop(paste0("The JHEEM components have been fixed. Cannot reset '", d, "'"))
            
            components[[elem]] = NULL
        }
    }
    
    components
}

##----------------------------------##
##--         JIT HELPERS          --##
##-- To Crunch Stuff Just-in-Time --##
##----------------------------------##


##-- THE JHEEM SKELETON --##
do.setup.jheem.skeleton <- function(components)
{
    if (components$model.idu)
        risk.strata = components$settings$RISK_STRATA
    else
        risk.strata = NULL
    
    if (components$model.hiv.transmission)
    {
        if (components$model.prep)
        {
            cd4.strata = components$settings$CD4_STRATA
            continuum = components$settings$CONTINUUM_OF_CARE
            first.diagnosed = components$settings$FIRST_DIAGNOSED_STATE
            undiagnosed = components$settings$UNDIAGNOSED_STATES
        }
        else
        {
            cd4.strata = components$settings$CD4_STRATA
            continuum = setdiff(components$settings$CONTINUUM_OF_CARE, components$settings$UNDIAGNOSED_FROM_PREP)
            first.diagnosed = components$settings$FIRST_DIAGNOSED_STATE
            undiagnosed = setdiff(components$settings$UNDIAGNOSED_STATES, components$settings$UNDIAGNOSED_FROM_PREP)
        }
    }
    else
        cd4.strata = continuum = first.diagnosed = undiagnosed = NULL
    
    jheem = initialize.jheem(age.cutoffs = components$settings$AGE_CUTOFFS,
                             race.strata = components$settings$RACES,
                             subpopulations = components$settings$SUBPOPULATIONS,
                             sex.strata = components$settings$SEXES,
                             risk.strata = risk.strata,
                             nonhiv.subsets = NULL,
                             continuum.of.care.states = continuum,
                             cd4.strata = cd4.strata,
                             hiv.subsets = NULL,
                             transmission.route.names = c('sexual','idu'),
                             diagnosed.hiv.continuum.states = first.diagnosed)
    
    #by default, minimal tracking
    jheem = set.track.incidence.dimensions(jheem, dimensions=c('age','race','subpopulation','sex','risk'))
    
    components$jheem = jheem
    
    components = do.crunch.components.masks(components)
    
    
    components
}

##-- INITIAL POPULATION --##
do.setup.initial.population <- function(components)
{
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
                                                                       seed.to.races = components$settings$RACES,
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
    
    if (is.null(components$settings))
        stop("components has not been initialized as JHEEM components")
    
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
    mortality.rates = collapse.races.for.rates(components$populations$all.races, mortality.rates.all.counties)
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
    if (components$model.hiv.transmission)
    {
        components = do.calculate.suppression(components)
        suppression = calculate.suppression(components)
        
        mortality.rates = get.hiv.mortality.rate.arrays(components)
        
        mortality.and.suppression = merge.rates(rates1=mortality.rates$rates,
                                                times1=mortality.rates$times,
                                                rates2=suppression$rates,
                                                times2=suppression$times)
        
        components$hiv.mortality.rates = lapply(1:length(mortality.and.suppression$times), function(i){
            
            hiv.mortality = mortality.and.suppression$rates1[[i]]
            hiv.mortality[,,,,,components$settings$DIAGNOSED_STATES,,] = hiv.mortality[,,,,,components$settings$DIAGNOSED_STATES,,] *
                (1-mortality.and.suppression$rates2[[i]][,,,,,components$settings$DIAGNOSED_STATES,,])
            
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

do.crunch.components.masks <- function(components)
{
    
    
    components
}

do.setup.idu.transitions <- function(components)
{
    if (components$model.idu)
    {
        if (is.null(components$incident.idu) || is.null(components$idu.remission) || is.null(components$idu.relapse))
            stop("IDU transition parameters have not been set in the components for the JHEEM")
        
        if (length(components$years.for.idu.transitions)==2)
        {
            components$interpolated.idu.transition.years = components$years.for.idu.transitions[1]:components$end.year.idu.transition
            
            multi.incident.idu = calculate.change.ratios.logistic.array(r0.arr=components$incident.idu[[1]],
                                                                        r1.arr=components$incident.idu[[2]],
                                                                        times=components$interpolated.idu.transition.years,
                                                                        t0=components$years.for.idu.transitions[1],
                                                                        t1=components$years.for.idu.transitions[2],
                                                                        fraction.of.asymptote.after.end=components$fraction.idu.transitions.change.after.last.year,
                                                                        fraction.of.asymptote.before.start=0.025)
            multi.idu.remission = calculate.change.ratios.logistic.array(r0.arr=components$idu.remission[[1]],
                                                                         r1.arr=components$idu.remission[[2]],
                                                                         times=components$interpolated.idu.transition.years,
                                                                         t0=components$years.for.idu.transitions[1],
                                                                         t1=components$years.for.idu.transitions[2],
                                                                         fraction.of.asymptote.after.end=components$fraction.idu.transitions.change.after.last.year,
                                                                         fraction.of.asymptote.before.start=0.025)
            multi.idu.relapse = calculate.change.ratios.logistic.array(r0.arr=components$idu.relapse[[1]],
                                                                       r1.arr=components$idu.relapse[[2]],
                                                                       times=components$interpolated.idu.transition.years,
                                                                       t0=components$years.for.idu.transitions[1],
                                                                       t1=components$years.for.idu.transitions[2],
                                                                       fraction.of.asymptote.after.end=components$fraction.idu.transitions.change.after.last.year,
                                                                       fraction.of.asymptote.before.start=0.025)
            
            idu.transitions = get.general.transition.array.skeleton(components$jheem, transition.dimension='risk')
            components$idu.transitions = lapply(1:length(1:length(multi.incident.idu)), function(i){
                idu.transitions[,,,,'never_IDU','active_IDU'] = multi.incident.idu[[i]]
                idu.transitions[,,,,'active_IDU','IDU_in_remission'] = multi.idu.remission[[i]]
                idu.transitions[,,,,'IDU_in_remission', 'active_IDU'] = multi.idu.relapse[[i]] 
                
                idu.transitions
            })
        }
        else
        {
            idu.transitions = get.general.transition.array.skeleton(components$jheem, transition.dimension='risk')
            components$idu.transitions = lapply(1:length(components$years.for.idu.transitions), function(i){
                idu.transitions[,,,,'never_IDU','active_IDU'] = components$incident.idu[[i]]
                idu.transitions[,,,,'active_IDU','IDU_in_remission'] = components$idu.remission[[i]]
                idu.transitions[,,,,'IDU_in_remission', 'active_IDU'] = components$idu.relapse[[i]]
                
                idu.transitions
            })
            components$interpolated.idu.transition.years = components$years.for.idu.transitions
        }
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
    #-- HIV TRANSITIONS --#
    
    if (components$model.hiv.transmission)
    {
        base.transitions = get.hiv.positive.transition.array.skeleton(components$jheem,
                                                                      transition.dimension = 'continuum')
        
        if (components$model.prep)
            base.transitions[,,,,,'undiagnosed_from_prep',,,'diagnosed'] = 1 / components$prep.screening.frequency
        
        #-- HIV Testing --#
        
        components = do.calculate.testing.rates(components) #this caches them
        testing.rates = calculate.testing.rates(components)
        
        components$continuum.transitions = lapply(1:length(testing.rates$rates), function(i){
            continuum.transitions.for.year = base.transitions
            continuum.transitions.for.year[,,,,,'undiagnosed',,,'diagnosed'] = testing.rates$rates[[i]][,,,,,'undiagnosed',,]
            continuum.transitions.for.year
        })
        components$continuum.transition.years = testing.rates$times
    }
    else
    {
        components$continuum.transitions = list(0)#list(get.transition.array.skeleton(components$jheem))
        components$continuum.transition.years=-Inf
    }
    
    components
}

##-----------------------------##
##-- SUPPRESSION AND TESTING --##
##-----------------------------##

calculate.suppression <- function(components)
{
    if (is.null(components$suppression.rates.and.times))
        components = do.calculate.suppression(components)
    components$suppression.rates.and.times
}

do.calculate.suppression <- function(components)
{
    #Pull background suppression proportions from logistic model
    background.suppression = get.background.proportions(base.model = components$background.suppression$model,
                                                        years = components$background.suppression$years,
                                                        additional.intercepts = log(components$background.suppression$additional.intercept.ors),
                                                        additional.slopes = log(components$background.suppression$additional.slope.ors),
                                                        future.slope = log(components$background.suppression$future.slope.or),
                                                        future.slope.after.year = components$background.suppression$future.slope.after.year,
                                                        idu.applies.to.in.remission = T,
                                                        idu.applies.to.msm.idu=T,
                                                        msm.applies.to.msm.idu=T,
                                                        jheem=components$jheem)

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
    suppression = get.rates.from.background.and.foreground(background.rates = background.suppression,
                                                           background.times = background.suppression.years,
                                                           foreground.rates = components$foreground.suppression,
                                                           foreground.times = components$foreground.suppression.years,
                                                           foreground.start.times = components$foreground.suppression.start.years,
                                                           max.background.time = components$background.change.to.years$suppression,
                                                           allow.foreground.less = F)
    
    
    components$suppression.rates.and.times = suppression
    components
}

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
    testing.rates = get.rates.from.background.and.foreground(background.rates = background.testing.rates,
                                                             background.times = background.testing.years,
                                                             foreground.rates = components$foreground.testing.rates,
                                                             foreground.times = components$foreground.testing.years,
                                                             foreground.start.times = components$foreground.testing.start.years,
                                                             max.background.time = components$background.change.to.years$testing,
                                                             allow.foreground.less = F)
    
    #Return
    components$testing.rates.and.times = testing.rates
    components
}

calculate.prep.coverage <- function(components)
{
    if (is.null(components$prep.rates.and.times))
        components = do.calculate.prep.coverage(components)
    components$prep.rates.and.times
}

do.calculate.prep.coverage <- function(components)
{
    components$prep.rates.and.times = get.rates.from.background.and.foreground(background.rates = components$background.prep.coverage,
                                                                               background.times = components$background.prep.years,
                                                                               foreground.rates = components$foreground.prep.coverage,
                                                                               foreground.times = components$foreground.prep.years,
                                                                               foreground.start.times = components$foreground.prep.start.years,
                                                                               max.background.time = components$background.change.to.years$prep,
                                                                               allow.foreground.less = F)
    
    components
}

get.background.proportions <- function(base.model,
                                       years,
                                       additional.intercepts,
                                       additional.slopes,
                                       future.slope=0,
                                       future.slope.after.year=base.model$anchor.year,
                                       idu.applies.to.in.remission=T,
                                       idu.applies.to.msm.idu=T,
                                       msm.applies.to.msm.idu=T,
                                       transformation = function(x){base.model$max.proportion / (1+exp(-x))},
                                       jheem,
                                       expand.population=T)
{
    intercept = add.additional.betas.to.array(base.model$intercept, additional.intercepts,
                                              idu.applies.to.in.remission = idu.applies.to.in.remission,
                                              idu.applies.to.msm.idu = idu.applies.to.msm.idu,
                                              msm.applies.to.msm.idu = msm.applies.to.msm.idu)
    slope = add.additional.betas.to.array(base.model$slope, additional.slopes,
                                          idu.applies.to.in.remission = idu.applies.to.in.remission,
                                          idu.applies.to.msm.idu = idu.applies.to.msm.idu,
                                          msm.applies.to.msm.idu = msm.applies.to.msm.idu)

    lapply(years, function(year){
        p = transformation(intercept + slope * (year-base.model$anchor.year) + future.slope * max(0,year-future.slope.after.year))
        if (expand.population)
            expand.population.to.hiv.positive(jheem, p)
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
        components = do.calculate.prep.coverage(components)
        rates.and.times = calculate.prep.coverage(components)

        components$sexual.susceptibility = lapply(rates.and.times$rates, function(prep.coverage){

            if (is.null(dim(prep.coverage)))
                prep.coverage = as.numeric(prep.coverage)
            prep.coverage = expand.population.to.hiv.negative(components$jheem, prep.coverage)

            non.prep.risk = (1-prep.coverage)
            prep.risk = prep.coverage * components$prep.rr.heterosexual
            prep.risk[,,,'msm',,] = prep.coverage[,,,'msm',,] * components$prep.rr.msm

            susceptibility = non.prep.risk + prep.risk
            susceptibility * base.sexual.susceptibility
        })
        components$sexual.susceptibility.years = rates.and.times$times

        if (components$model.idu)
        {
            components$idu.susceptibility = lapply(rates.and.times$rates, function(prep.coverage){

                if (is.null(dim(prep.coverage)))
                    prep.coverage = as.numeric(prep.coverage)
                prep.coverage = expand.population.to.hiv.negative(components$jheem, prep.coverage)

                non.prep.risk = (1-prep.coverage)
                prep.risk = prep.coverage * components$prep.rr.heterosexual
                prep.risk[,,,'msm',,] = prep.coverage[,,,'msm',,] * components$prep.rr.msm

                susceptibility = non.prep.risk + prep.risk
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
    if (components$model.hiv.transmission)
    {
        #-- Set up general skeleton arrays (not year-specific) --#
        acute.transmissibility.rr = expand.population.to.hiv.positive(components$jheem,
                                                                      components$acute.transmissibility.rr)
        transmissibility = get.transmissibility.array.skeleton(components$jheem, value=1)
        transmissibility[,,,,,,components$settings$ACUTE_STATES,] =
            transmissibility[,,,,,,components$settings$ACUTE_STATES,] * acute.transmissibility.rr[,,,,,,components$settings$ACUTE_STATES,]
        sexual.transmissibility = idu.transmissibility = transmissibility

        idu.transmissibility[,,,,,components$settings$DIAGNOSED_STATES,,] =
            idu.transmissibility[,,,,,components$settings$DIAGNOSED_STATES,,] * components$diagnosed.needle.sharing.rr

        sexual.transmissibility[,,,'heterosexual_male',,components$settings$DIAGNOSED_STATES,,] =
            sexual.transmissibility[,,,'heterosexual_male',,components$settings$DIAGNOSED_STATES,,] * components$diagnosed.het.male.condomless.rr
        sexual.transmissibility[,,,'female',,components$settings$DIAGNOSED_STATES,,] =
            sexual.transmissibility[,,,'female',,components$settings$DIAGNOSED_STATES,,] * components$diagnosed.female.condomless.rr
        sexual.transmissibility[,,,'msm',,components$settings$DIAGNOSED_STATES,,] =
            sexual.transmissibility[,,,'msm',,components$settings$DIAGNOSED_STATES,,] * components$diagnosed.msm.condomless.rr

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

            expanded.unsuppressed = 1 - suppressed.proportions[,,,,,components$settings$DIAGNOSED_STATES,,]

            sexual.transmissibility.for.year[,,,,,components$settings$DIAGNOSED_STATES,,] =
                sexual.transmissibility.for.year[,,,,,components$settings$DIAGNOSED_STATES,,] * expanded.unsuppressed

            sexual.transmissibility.for.year
        })
        components$sexual.transmissibility.years = suppression$times

        if (components$model.idu)
        {
            components$idu.transmissibilities = lapply(suppression$rates, function(suppressed.proportions){

                idu.transmissibility.for.year = idu.transmissibility

                expanded.unsuppressed = 1 - suppressed.proportions[,,,,,components$settings$DIAGNOSED_STATES,,]

                idu.transmissibility.for.year[,,,,,components$settings$DIAGNOSED_STATES,,] =
                    idu.transmissibility.for.year[,,,,,components$settings$DIAGNOSED_STATES,,] * expanded.unsuppressed

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
    if (components$model.hiv.transmission)
    {
        #-- Sex by age --#
        sexual.transmission.by.age = get.contact.array.skeleton(components$jheem, age=T, sex.to=T)

        # Set up full census ages
        ages = min(components$settings$AGE_CUTOFFS):max(as.numeric(dimnames(components$populations$full.age.collapsed.races)[['age']]))
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
                                                                 age.cutoffs = components$settings$AGE_CUTO)

        female.sex.by.age = get.age.proportions.from.age.model(components$sexual.transmission$female.age.model,
                                                                 ages.to.use=ages,
                                                                 age.counts=age.counts,
                                                                 age.cutoffs = components$settings$AGE_CUTOFFS)
        msm.sex.by.age = get.age.proportions.from.age.model(components$sexual.transmission$msm.age.model,
                                                                 ages.to.use=ages,
                                                                 age.counts=age.counts,
                                                                 age.cutoffs = components$settings$AGE_CUTOFFS)


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
        
        mat = array(0, dim=c(sex.from=length(components$settings$SEXES), sex.to=length(components$settings$SEXES)),
                    dimnames=list(sex.from=components$settings$SEXES, sex.to=components$settings$SEXES))

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
            n.risk.states = length(components$settings$RISK_STRATA)
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

            idu.states = setdiff(components$settings$RISK_STRATA, 'never_IDU')

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
    if (components$model.hiv.transmission && components$model.idu)
    {
        #-- SELECT OUT JUST IDU --#
        idu.to.idu = get.contact.array.skeleton(components$jheem, risk=T)
        idu.to.idu[components$settings$ACTIVE_IDU_STATE, components$settings$ACTIVE_IDU_STATE] = 1

        #-- IDU by age --#
        ages = min(components$settings$AGE_CUTOFFS):max(as.numeric(dimnames(components$populations$full.age.collapsed.races)[['age']]))
        age.counts = apply(components$populations$full.age.collapsed.races, 'age', sum)[as.character(ages)]

        # reduce per idu availability
        idu.availability = unlist(components$idu.availability, use.names=F)
        names(idu.availability) = unlist(sapply(components$idu.availability, function(x){names(x)}))

        age.counts[names(idu.availability)] = age.counts[names(idu.availability)] * idu.availability

        idu.transmission.by.age = get.age.proportions.from.age.model(age.model=components$idu.transmission$age.model,
                                                                     ages.to.use=ages,
                                                                     age.counts=age.counts,
                                                                     age.cutoffs = components$settings$AGE_CUTOFFS)

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
    if (!components$model.hiv.transmission)
    {
        components$new.infection.proportions = list(get.uniform.new.infection.proportions(components$jheem))
        components$new.infection.proportions.years = -Inf
    }
    else if (components$model.prep)
    {
        rates.and.times = calculate.prep.coverage(components)

        components$new.infection.proportions = lapply(rates.and.times$rates, function(prep.coverage){

            non.prep.risk = (1-prep.coverage)
            prep.risk = prep.coverage * components$prep.rr.heterosexual

            new.infection.proportions = get.new.infection.proportions.skeleton(components$jheem)

#            access(new.infection.proportions, continuum = components$settings$UNDIAGNOSED_NO_PREP, cd4 = components$settings$CD4_STRATA[1]) =
#                non.prep.risk / (prep.risk + non.prep.risk)
            new.infection.proportions[,,,,,,components$settings$UNDIAGNOSED_NO_PREP,components$settings$CD4_STRATA[1],] = non.prep.risk / (prep.risk + non.prep.risk)
#            access(new.infection.proportions, continuum = components$settings$UNDIAGNOSED_FROM_PREP, cd4 = components$settings$CD4_STRATA[1]) =
#                prep.risk / (prep.risk + non.prep.risk)
            new.infection.proportions[,,,,,,components$settings$UNDIAGNOSED_FROM_PREP,components$settings$CD4_STRATA[1],] = prep.risk / (prep.risk + non.prep.risk)

            new.infection.proportions
        })

        components$new.infection.proportions.years = rates.and.times$times
    }
    else
    {
        components$new.infection.proportions = list(get.uniform.new.infection.proportions(components$jheem, initial.continuum = components$settings$UNDIAGNOSED_NO_PREP))
        components$new.infection.proportions.years = -Inf
    }

    components
}

##-------------##
##-- HELPERS --##
##-------------##

get.rates.from.background.and.foreground <- function(background.rates,
                                                     background.times,
                                                     foreground.rates,
                                                     foreground.times,
                                                     foreground.start.times,
                                                     max.background.time=Inf,
                                                     allow.foreground.less=F)
{
    if (is.null(foreground.times))
        list(rates=background.rates[background.times<=max.background.time],
             times=background.times[background.times<=max.background.time])
    else
    {
        if (is.null(foreground.rates))
            stop("foreground.rates is NULL")
        
        background.rates = background.rates[background.times<=max.background.time]
        background.times = background.times[background.times<=max.background.time]
        
        all.times = sort(unique(c(background.times, foreground.times)))
        interpolated.background.rates = interpolate.parameters(values=background.rates,
                                                               value.times=background.times,
                                                               desired.times = all.times)
        
        names(interpolated.background.rates) = as.character(all.times)
        names(foreground.rates) = as.character(foreground.times)
        
        dim.names = dimnames(foreground.rates[[1]])
        
        sub.rates = numeric(length(all.times))
        names(sub.rates) = as.character(all.times)
        
        raw.rates = sapply(1:length(foreground.rates[[1]]), function(i){
            bg.rates = sub.rates = sapply(interpolated.background.rates, function(bg){bg[i]}) #start off with the backgroundr rates
            
            from.foreground.times = as.character(foreground.times[foreground.times > foreground.start.times[i]])
            if (length(from.foreground.times)>0)
            {
                from.foreground.rates = sapply(foreground.rates[from.foreground.times], function(fg){fg[i]})
                if (any(!is.na(from.foreground.rates)))
                {
                    sub.rates[from.foreground.times] = from.foreground.rates
                    
                    if (any(is.na(sub.rates)))
                    {
                        to.interpolate.times = all.times[is.na(sub.rates)]
                        bg.time = foreground.start.times[i]
                        fg.time = min(all.times[!is.na(sub.rates) & all.times>bg.time])
                        sub.rates[is.na(sub.rates)] = interpolate.parameters(values=sub.rates[as.character(c(bg.time,fg.time))],
                                                                             value.times=c(bg.time,fg.time),
                                                                             desired.times=to.interpolate.times,
                                                                             return.list=F)
                    }
                }
            }
            
            if (!allow.foreground.less)
                sub.rates[sub.rates < bg.rates] = bg.rates[sub.rates < bg.rates]
            
            sub.rates
        })
        
        rates = lapply(1:length(all.times), function(i){
            arr = raw.rates[i,]
            dim(arr) = sapply(dim.names, length)
            dimnames(arr) = dim.names
            arr
        })
            
        list(rates=rates,
             times=all.times)
    }
}

OLD.get.rates.from.background.and.foreground <- function(background.rates,
                                                    background.times,
                                                    foreground.rates,
                                                    foreground.times,
                                                    max.background.time=Inf,
                                                    allow.foreground.less=F)
{
    if (is.null(foreground.times))
        list(rates=background.rates[background.times<=max.background.time],
             times=background.times[background.times<=max.background.time])
    else
    {
        if (is.null(foreground.rates))
            stop("foreground.rates is NULL")
        
        background.rates = background.rates[background.times<=max.background.time]
        background.times = background.times[background.times<=max.background.time]
        
        if (max(background.times) < (min(foreground.times)-1))
        {
            background.times = c(background.times, min(foreground.times)-1)
            background.rates = c(background.rates,
                                 background.rates[length(background.rates)])
        }
        
        rates.and.times = merge.rates(rates1 = background.rates,
                                      times1 = background.times,
                                      rates2 = foreground.rates,
                                      times2 = foreground.times)

        list(rates=lapply(1:length(rates.and.times$rates1), function(i){
            foreground = rates.and.times$rates2[[i]]
            rates = background = rates.and.times$rates1[[i]]
            if (rates.and.times$times[i] >= min(foreground.times))
            {
                mask = !is.na(foreground)
                if (!allow.foreground.less)
                    mask = mask & foreground >= background
                rates[mask] = foreground[mask]
            }

            rates
        }),
                  times=rates.and.times$times)
    }
}

merge.rates <- function(rates1,
                        times1,
                        rates2,
                        times2)
{
    rv = list()
    rv$times = sort(unique(c(times1,times2)))

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
    rv$times = unique(sort(unlist(sapply(rates.and.times.list, function(rt){rt$times}))))

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
    rv$times = sort(unique(c(times1,times2,times3)))

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

