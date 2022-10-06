
##------------------------##
##-- RUN FOR COMPONENTS --##
##------------------------##


##------------------------------##
##-- THE MAIN SETUP FUNCTIONS --##
##------------------------------##

are.components.fixed <- function(components)
{
    components$settings$fixed
}

FIX.COMPONENTS = 1
CRUNCH.COMPONENTS = 2
PRODUCE.COMPONENTS.FROM.FIXED = 3
PRODUCE.COMPONENTS.FROM.UNFIXED = 4
PRODUCE.JHEEM = 5

#-- The Workhorse Helper that actually carries out the operations --#
do.setup.crunch.or.fix <- function(components,
                                   setting,
                                   verbose,
                                   return.components.with.jheem=F)
{
    #--------------------#
    #-- Initial Set-Up --#
    #--------------------#
    
    #-- Queue up the quantity names --#
    transition.mapping = get.components.transition.mapping(components)
    quantity.names = get.model.quantity.names(transition.mapping)
    
    
    #-- If not fixing, calculate the quantities --#
    quantity.was.null = sapply(quantity.names, function(name){is.null(get.calculated.quantity.name(name))})
    names(quantity.was.null) = quantity.names
    
    if (!setting==FIX.COMPONENTS)
    {
        for (name in quantity.names[quantity.was.null])
            components = do.calculate.quantity(components, quantity.name=name) 
    }
        
    
    #-- Set up check functions --#
    need.to.push <- function(...){
        quantity.names = as.character(list(...))
        setting == PRODUCE.COMPONENTS.FROM.UNFIXED ||
            (setting == PRODUCE.COMPONENTS.FROM.FIXED && any(quantity.was.null[quantity.names])) ||
            (setting == FIX.COMPONENTS && all(!quantity.was.null[quantity.names]))
    }
    
    #-----------------------------------------#
    #-- Go one by one and push to the JHEEM --#
    #-----------------------------------------#
    
    #-- SET-UP JHEEM --#
    if (is.null(components$jheem))
        components = do.setup.jheem.skeleton(components)

        
    #-- INITIAL POPULATION --#
    components = push.initial.population(components, need.to.push)

    #-- BIRTHS --#
    components = push.fertility.hiv.negative(components, need.to.push)
    components = push.fertility.hiv.positive(components, need.to.push)
    components = push.birth.proportions(components, need.to.push)

    #-- AGING --#
    components = push.aging.hiv.negative(components, need.to.push)
    components = push.aging.hiv.positive(components, need.to.push)
    
    #-- MORTALITY --#
    components = push.general.mortality.hiv.negative(components, need.to.push)
    components = push.general.mortality.hiv.positive(components, need.to.push)
    components = push.hiv.mortality(components, need.to.push)

    #-- TRANSITIONS --#
    components = push.transitions(components, need.to.push)
    
    #-- TRANSMISSION --#
    components = push.susceptibility(components, need.to.push)
    components = push.transmissibility(components, need.to.push)
    components = push.transmission.contact(components, need.to.push)
    components = push.new.infection.proportions(components, need.to.push)
    
  
    
}

##--------------------------------------##
##--------------------------------------##
##-- CONTENT-SPECIFIC SETUP FUNCTIONS --##
##-- (FOR PASSING ARGUMENTS TO JHEEM) --##
##--------------------------------------##
##--------------------------------------##


do.setup.jheem.skeleton <- function(components)
{
    
    
    
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
    
}


push.initial.population <- function(components, need.to.push)
{
    if (need.to.push('initial.population.hiv.negative', 'initial.population.hiv.positive'))
    {
        components$jheem = set.initial.populations(
            components$jheem,
            init.hiv.negative = get.calculated.quantity(components, 'initial.population.hiv.negative')$values[[1]],
            init.hiv.positive = get.calculated.quantity(components, 'initial.population.hiv.positive')$values[[1]]
        )
    }
    
    components
}

push.fertility.hiv.negative <- function(components, need.to.push)
{
    do.push.quantity.to.jheem(components,
                              quantity.name='fertility',
                              set.function=set.fertility.hiv.negative,
                              need.to.push=need.to.push)
}

push.fertility.hiv.positive <- function(components, need.to.push)
{
    do.push.quantity.to.jheem(components,
                              quantity.name='fertility',
                              set.function=set.fertility.hiv.positive,
                              need.to.push=need.to.push)
}

push.birth.proportions <- function(components, need.to.push)
{
    if (need.to.push('birth.proportions'))
    {
        print("need to implement check that proportions sum to 1")
        
    }
    
    do.push.quantity.to.jheem(components,
                              quantity.name='birth.proportions',
                              set.function=set.birth.proportions.no.maternal.transmission,
                              need.to.push=need.to.push)
}

push.aging.hiv.negative <- function(components, need.to.push)
{
    do.push.quantity.to.jheem(components,
                              quantity.name='aging.hiv.negative',
                              set.function=set.aging.hiv.negative,
                              need.to.push=need.to.push)
}

push.aging.hiv.positive <- function(components, need.to.push)
{
    do.push.quantity.to.jheem(components,
                              quantity.name='aging.hiv.positive',
                              set.function=set.aging.hiv.positive,
                              need.to.push=need.to.push)
}   


push.general.mortality.hiv.negative <- function(components, need.to.push)
{
    do.push.quantity.to.jheem(components,
                              quantity.name='general.mortality.hiv.negative',
                              set.function=set.general.mortality.hiv.negative,
                              need.to.push=need.to.push)
}

push.general.mortality.hiv.negative <- function(components, need.to.push)
{
    do.push.quantity.to.jheem(components,
                              quantity.name='general.mortality.hiv.positive',
                              set.function=set.general.mortality.hiv.positive,
                              need.to.push=need.to.push)
}

push.hiv.mortality <- function(components, need.to.push)
{
    do.push.quantity.to.jheem(components,
                              quantity.name='hiv.mortality',
                              set.function=set.hiv.specific.mortality,
                              need.to.push=need.to.push)
}

push.transitions <- function(components, need.to.push)
{
    transition.mapping = get.components.transition.mapping(components)
    transition.quantity.names = get.transition.model.quantity.names(transition.mapping)
    
    for (name in transition.quantity.names)
    {
        dimension = get.dimension.for.transition.model.quantity(transition.mapping, quantity.name=name)
        subgroups = get.model.quantity.subgroups(transition.mapping, quantity.name=name)
        
        if (any(subgroups=='hiv.negative'))
            components = do.push.quantity.to.jheem(components,
                                                   quantity.name=name,
                                                   set.function=set.transition.array.hiv.negative,
                                                   need.to.push=need.to.push)
        
        if (any(subgroups=='hiv.positive'))
            components = do.push.quantity.to.jheem(components,
                                                   quantity.name=name,
                                                   set.function=set.transition.array.hiv.positive,
                                                   need.to.push=need.to.push)
    }
    
    components
}

push.susceptibility <- function(components, need.to.push)
{
    specification = get.components.specification(components)
    for (mode in specification$TRANSMISSION_MODES)
        do.push.quantity.to.jheem(components,
                                  quantity.name=paste0(mode, '.susceptibility'),
                                  set.function=set.susceptibility,
                                  need.to.push=need.to.push,
                                  transmission.route.names = mode)
}

push.transmissibility <- function(components, need.to.push)
{
    specification = get.components.specification(components)
    for (mode in specification$TRANSMISSION_MODES)
        do.push.quantity.to.jheem(components,
                                  quantity.name=paste0(mode, '.transmissibility'),
                                  set.function=set.transmissibility,
                                  need.to.push=need.to.push,
                                  transmission.route.names = mode)
}

push.transmission.contact <- function(components, need.to.push)
{
    specification = get.components.specification(components)
    for (mode in specification$TRANSMISSION_MODES)
        do.push.quantity.to.jheem(components,
                                  quantity.name=paste0(mode, '.contact'),
                                  set.function=set.transmission.contact.array,
                                  need.to.push=need.to.push,
                                  transmission.route.names = mode)
}

push.new.infection.proportions <- function(components, need.to.push)
{
    if (need.to.push('new.infection.proportions'))
        print("need to check that new.infection.proportions sum to 1")
    
    components = do.push.quantity.to.jheem(components,
                                           quantity.name='new.infection.proportions',
                                           set.function=set.new.infection.proportions,
                                           need.to.push=need.to.push)
}



# A general helper that pulls time varying quantities from the components
# and pushes them to the jheem object
do.push.quantity.to.jheem <- function(components,
                                      quantity.name,
                                      set.function,
                                      need.to.push,
                                      ...)
{
    if (need.to.push(quantity.name))
    {
        quantity = get.calculated.quantity(components, quantity.name)
        if (is.null(quantity$times))
            components$jheem = set.function(jheem=components$jheem,
                                            quantity$values[[1]],
                                            ...)
        else
        {
            for (i in 1:length(quantity$values))
                components$jheem = set.function(jheem=components$jheem,
                                                quantity$values[[i]],
                                                time=quantity$times[i],
                                                ...)
                
        }
    }
    
    components
}

##---------------------------------------##
##---------------------------------------##
##--       GENERIC SETUP FUNCTIONS     --##
##-- FOR MODEL ELEMENTS and QUANTITIES --##
##---------------------------------------##
##---------------------------------------##


##--------------------##
##-- MODEL ELEMENTS --##
##--------------------##

get.calculated.element <- function(components,
                                   element.name)
{
    components$calculated.elements[[element.name]]
}

# Returns the updated components object
do.calculate.element <- function(components,
                                 element.name)
{
    #-- Pull some meta-values --#
    element.background = components$element.backgrounds[[element.name]]
    model = element.background$model
    
    error.prefix = paste0("Unable to calculate values for model.element '", element.name, "': ")
    
    #-- Calculate background values and times --#
    if (is.null(model)) #this is a static value
    {
        values = list(model$value)
        times = NULL
        scale = element.background$scale
    }
    else
    {
        #-- Pull from model --#
        model.from.time = max(components$keep.from.time,
                              element.background$model.from.time)
        model.to.time = min(component$keep.to.time,
                            element.background$model.to.time)
        times = seq.years(model.from.time, model.to.time)
        
        values = project.from.model(model,
                                    years=times)
        
        scale = element.background$model.scale
        
        #-- Add ramp, if applicable --#
        if (!is.null(element.background$ramp.times))
        {
            # Convert the scale, if needed
            values = convert.model.element.scale(values,
                                                 convert.from.scale = scale,
                                                 convert.to.scale = element.background$ramp.scale)
            scale = element.background$ramp.scale
            
            # check for NA values - if ramp was never set
            if (any(is.na(element.background$ramp.multipliers)))
                stop(paste0(error.prefix,
                            "Some ramp.multipliers have not been set and are still NA"))
            
            # calculate the values and times
            ramp.multipliers.and.times = calculate.ramp.multipliers.and.times(ramp.times=element.background$ramp.times,
                                                                              ramp.multipliers=element.background$ramp.multipliers,
                                                                              ramp.scales=element.background$ramp.interpolate.scales,
                                                                              first.non.ramp.times=times[1],
                                                                              check.for.errors=F)
            
            # fold them in
            times = c(ramp.multipliers.and.times$times, times)
            values = c(lapply(ramp.values.and.times$multipliers, function(mult){
                mult * values[[1]]
            }), values)
        }
        
        #-- Add taper, if applicable --#
        if (!is.null(element.background$taper.times))
        {
            # Convert the scale, if needed
            values = convert.model.element.scale(values,
                                                 convert.from.scale = scale,
                                                 convert.to.scale = element.background$taper.scale)
            scale = element.background$taper.scale
            
            # check for NA values - if ramp was never set
            if (any(is.na(element.background$taper.multipliers)))
                stop(paste0(error.prefix,
                            "Some taper.multipliers have not been set and are still NA"))
            
            # calculate the values and times
            taper.multipliers.and.times = calculate.taper.multipliers.and.times(taper.times=element.background$taper.times,
                                                                                taper.multipliers=element.background$taper.multipliers,
                                                                                taper.scales=element.background$taper.interpolate.scales,
                                                                                first.non.taper.times=times[length(times)],
                                                                                check.for.errors=F)
            
            # fold them in
            times = c(times, ramp.multipliers.and.times$times)
            values = c(values,
                       lapply(ramp.values.and.times$multipliers, function(mult){
                           mult * values[[length(values)]]
                       }))
        }
    }
    
    #-- Fold in foreground values and times --#
    
    # we're going to need to deal with NULL times here
    
    values = convert.model.element.scale(values,
                                         convert.from.scale = scale, 
                                         convert.to.scale = element.background$scale)

    #-- Store the calculated values and times, and return the updated components --#
    components$calculated.elements[[element.name]] = list(
        values = values,
        times = times
    )
    
    components
        
}


##----------------------##
##-- MODEL QUANTITIES --##
##----------------------##

get.calculated.quantity <- function(components,
                                    quantity.name)
{
    components$calculated.quantities[[quantity.name]]
}

# Returns the updated components object
do.calculate.quantity <- function(components,
                                  quantity.name)
{
    # Pull the reference model quantity
    transition.mapping = get.components.transition.mapping(components)
    model.quantity = get.model.quantity(transition.mapping,
                                        quantity.name = quantity.name,
                                        throw.error.if.missing = T)
    
    # For every element/quantity on which this quantity depends
    #   calculate the value
    #   store the values and times in a list
    
    depends.on.times = list()
    depends.on.values = list()
    
    for (depends.on in model.quantity$depends.on)
    {
        if (is.model.element.name(transition.mapping, names = depends.on))
        {
            if (is.null(get.calculated.element(components, depends.on)))
                components = do.calculate.element(components, element.name=depends.on)
            
            values.and.times = components$calculated.elements[[depends.on]]
        }
        else
        {
            if (is.null(get.calculated.quantity(components, depends.on)))
                components = do.calculate.quantity(components, quantity.name=depends.on)
            
            values.and.times = components$calculated.quantities[[depends.on]]
        }
        
        depends.on.times = c(depends.on.times,
                             list(values.and.times$times))
        depends.on.values = c(depends.on.values,
                              list(values.and.times$values))
    }
    
    # Interpolate so that everything is on the same time scale
    depends.on.times = depends.on.times[!sapply(depends.on.times, is.null)]
    if (length(depends.on.times)==0)
        all.times = NULL
    else
        all.times = union_sorted_vectors(vectors=depends.on.times)
    
    depends.on.values = lapply(1:length(depends.on.values), function(i){
        if (is.null(depends.on.times[[i]]))
        {
            if (is.null(all.times))
                list(depends.on.values[[i]])
            else
                lapply(1:length(all.times), function(y){
                    depends.on.values[[i]]
                })
        }
        else
        {
            interpolate.parameters(values=depends.on.values[[i]],
                                   value.times=depends.on.times[[i]],
                                   desired.times = all.times)
        }
    })
    names(depends.on.values) = model.quantity$depends.on
    

    # Calculate the background (first component) value
    # Add in subsequent component (overwrite) values
    values = lapply(1:length(all.times), function(y){
        val = NULL
        for (i in 1:length(model.quantity$components))
        {
            comp = model.quantity$components[[i]]
            bindings = lapply(1:length(comp$depends.on), function(j){
                depends.on = comp$depends.on[[j]]
                val = depends.on.values[[depends.on]][[y]]
                apply.subset.expand.instructions(value = val,
                                                 comp$subset.expand.instructions.for.depends.on[[j]],
                                                 pass.through.scalar = T)
            })
            names(bindings) = comp$depends.on
            
            if (is.null(dim(component.value))) #otherwise it should already be the correct dimensions
                component.value = expand.population(resolve.model.quantity.component(comp, bindings=bindings),
                                                    target.dim.names = comp$dim.names)
            if (i==1)
                val = component.value
            else
                val[comp$access.indices.into.quantity] = component.value
        }
        
        val
    })
    
    
    #-- Store the calculated values and times, and return the updated components --#
    components$calculated.quantities[[quantity.name]] = list(
        values = values,
        times = all.times
    )
    
    components
}

##-----------------------##
##-- LOW-LEVEL HELPERS --##
##-----------------------##

seq.years <- function(from, to)
{
    if (ceiling(from)==from)
    {
        if (floor(to)==to)
            from:to
        else
            c(from:floor(to), to)
    }
    else
    {
        if (floor(to)==0)
            c(from, ceiling(from):to)
        else
            c(from, ceiling(from):floor(to), to)
    }
}