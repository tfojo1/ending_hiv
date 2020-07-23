

##------------------------##
##-- THE MAIN FUNCTIONs --##
##------------------------##

# The public interface for setting up a JHEEM from components
# We include the option to pre-crunch the components (and then later re-crunch which ones we need to)


# "Crunching" means doing the calculation that map a jheem.components object to a jheem object
#  WITHOUT actually pushing to the JHEEM
crunch.all.jheem.components <- function(components, verbose=F)
{
    if (verbose)
        print("CRUNCHING JHEEM COMPONENTS:")
    do.setup.crunch.or.fix(components,
                           setting = CRUNCH,
                           verbose=verbose)
}

# Fixing means specifying that any already crunched components cannot be changed
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
    components$elements$fixed.jheem = NULL
    components$fixed = F
    
    components
}

# Actually set up a JHEEM object
#  The components may be fixed or not, pre-crunched or not
setup.jheem.from.components <- function(components, verbose=F)
{
    do.setup.crunch.or.fix(components,
                           setting=PRODUCE.JHEEM,
                           verbose=verbose)
}


##---------------------------##
##-- THE MAIN SETUP HELPER --##
##---------------------------##

# Actually do the work of setting up a JHEEM from components

#Constants to toggle between operations
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
        jheem = components$elements$fixed.jheem
    else
    {
        if (is.null(components$jheem))
        {
            if (verbose)
                print('Setting up JHEEM skeleton')
            components = do.setup.jheem.skeleton(components)
        }
        jheem = components$elements$jheem
    }
    
    #-- INITIAL POPULATION --#
    comp.was.null = is.null(components$elements$initial.population)
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
                                        expand.population.to.hiv.negative(jheem, components$elements$initial.population$hiv.negative),
                                        expand.population.to.hiv.positive(jheem, components$elements$initial.population$hiv.positive))
    }
    
    #-- BIRTHS --#
    comp.was.null = is.null(components$elements$fertility.check)
    if (comp.was.null && setting != FIX)
    {
        if (is.null(components$data$fertility))
            stop("Fertility rates have not been set up in the components to make the JHEEM")
        components$elements$ferility.check = 'check'
    }
    if (setting == PRODUCE.FROM.UNFIXED ||
        (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
        (setting == FIX && !comp.was.null))
        jheem = set.fertility(jheem, components$data$fertility)
    
    comp.was.null = is.null(components$elements$birth.proportions)
    if (comp.was.null && setting != FIX)
    {
        if (verbose)
            print('Setting up Birth Proportions')
        components = do.setup.birth.proportions(components)
    }
    
    if (setting == PRODUCE.FROM.UNFIXED ||
        (setting == PRODUCE.FROM.FIXED && comp.was.null) ||
        (setting == FIX && !comp.was.null))
        jheem = set.birth.proportions.no.maternal.transmission(jheem, components$elements$birth.proportions)
    
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
        components = do.setup.hiv.mortality(components)
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
        for (i in 1:length(components$years.for.idu.transitions))
        {
            jheem = set.transition.array.hiv.negative(jheem, components$idu.transitions[[i]], time=components$years.for.idu.transitions[i])
            jheem = set.transition.array.hiv.positive(jheem, components$idu.transitions[[i]], time=components$years.for.idu.transitions[i])
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
        components = do.setup.susceptibility(components)
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
