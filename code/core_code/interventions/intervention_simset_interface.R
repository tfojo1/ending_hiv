
get.simset.intervention.code <- function(simset)
{
    code = attr(simset, 'intervention.code')
    
    # for backwards compatibility - early versions set the intervention object, not the code
    if (is.null(code))
    {
        int = attr(simset, 'intervention')
        if (is.null(int))
            NULL
        else
            get.intervention.code(int)
    }
    else
        code
}

get.simset.intervention <- function(simset)
{
    code = attr(simset, 'intervention.code')
    
    if (!is.null(code))
        intervention.from.code(code)
    else # for backwards compatibility - early versions set the intervention object, not the code
    {
        int = attr(simset, 'intervention')
        
        if (is.null(int))
            NULL
        else
        {
            # if we can, run it through the intervention manager to get the latest defined intervention
            code = get.intervention.code(int, throw.error.if.missing.intervention = F) 
        
            if (is.null(code))
                int
            else
                intervention.from.code(code)
        }
    }
}

set.simset.intervention.code <- function(simset, code)
{
    attr(simset, 'intervention.code') = code
    simset
}

run.simset.intervention <- function(simset,
                                    intervention,
                                    run.from.year=attr(simset, 'run.from.year'),
                                    run.to.year=2030,
                                    keep.years=(run.from.year-1):run.to.year,
                                    version=NULL,
                                    verbose=F,
                                    update.progress=if (verbose) function(n){print(paste0("Running simulation ", n, " of ", simset@n.sim))} else NULL,
                                    seed = 12343,
                                    allow.run.on.uprepared.simset=F)
{
    # Check classes of arguments
    if (!is(simset, 'simset') || !is(simset@simulations[[1]], 'jheem.results'))
        stop("simset must be an object of class 'simset' which contains 'jheem.results' objects as its simulations")
    if (is.null(intervention))
        stop("intervention cannot be NULL. Use the NO.INTERVENTION object if you want to run projections where nothing from the status quo is changed")
    if (!is(intervention, 'intervention'))
        stop("intervention must be an object of class 'intervention'")
    
    # Check that simset is prepared for intervention
    if (!allow.run.on.uprepared.simset && !is.simset.prepared.for.projection(simset))
        stop("simset has not been prepared for projections. Use the prepare.simset.for.intervention function to do so. If you truly intended to project on an unprepared simset, set the 'allow.run.on.uprepared.simset' flag to TRUE")
    
    # Check the run from year
    if (is.null(run.from.year))
    {
        run.from.year = max(as.numeric(ALL.DATA.MANAGERS$census.totals$years))
        print(paste0("** run.from.year was missing - using ", run.from.year, " **"))
    }
    
    # If we need to prepare intervention, do so
    original.intervention = intervention
    intervention = prepare.intervention.for.simset(intervention, simset)
    
    if (!is.null.intervention(intervention) && !is(intervention, 'standard_intervention'))
        stop("intervention must be converted to a 'standard_intervention' before running on simulations")
    
    # If we need to add parameters from distributions, do so now
    if (!is.null.intervention(intervention) && length(intervention@parameter.distributions)>0)
    {
        if (verbose)
            print("Adding new parameters")
        
        new.params = simulate.values.from.distribution.fixed.seed(distributions = intervention@parameter.distributions,
                                                                  n = simset@n.sim,
                                                                  seed = seed)
        
        simset = add.parameters(simset, 
                                parameters=new.params$values,
                                parameter.names = new.params$names,
                                parameter.lower.bounds = new.params$lower.bounds,
                                parameter.upper.bounds = new.params$upper.bounds)
    }
    
    if (verbose)
        print(paste0("Running intervention ", get.intervention.name(intervention), " on ", simset@n.sim, " simulations"))
    
    need.to.update.version = !is.null(version) && version != get.simset.version(simset)
    if (need.to.update.version)
    {
        init.components = create.initial.components(location = get.simset.location(simset),
                                                    version = version,
                                                    start.values = simset@parameters[1,],
                                                    fix.components = T)
        
        get.components.fn = get.components.function.for.version(version)
    }
        
    need.to.resolve.intervention = !intervention.is.resolved(intervention)
    simset@simulations = lapply(1:simset@n.sim, function(i){
        if (!is.null(update.progress))
            update.progress(i)
        
        if (need.to.update.version)
        {
            # redo components
            components = get.components.fn(simset@parameters[i,], init.components)
            
            # expand sim
            sim = expand.jheem.results(simset@simulations[[i]], components$jheem)
            
            # set it
            simset@simulations[[i]] = set.sim.components(sim, components)
        }
        
        if (need.to.resolve.intervention)
            int = resolve.intervention(intervention, simset@parameters[i,])
        else
            int = intervention
        
        run.sim.intervention(simset@simulations[[i]], int,
                             run.from.year=run.from.year, run.to.year = run.to.year,
                             keep.years = keep.years)
    })
    
    if (verbose)
        print("Done")
    
    set.simset.intervention.code(simset, get.intervention.code(original.intervention))
}

run.sim.intervention <- function(sim,
                                 intervention,
                                 run.from.year,
                                 run.to.year,
                                 keep.years=run.from.year:run.to.year)
{
    if (!is(intervention, 'list'))
        intervention = list(intervention)
    
    components = attr(sim, 'components')
    
    for (int in intervention)
        components = setup.components.for.intervention(components, int, overwrite.prior.intervention=F)
    
    sim = run.jheem.from.components(components, start.year=run.from.year, end.year=run.to.year,
                                    prior.results = sim, keep.components = T, keep.years=keep.years,
                                    pare.components = T)
    
    sim
}



##----------------------##
##-- SETUP COMPONENTS --##
##----------------------##

setup.components.for.intervention <- function(components,
                                              intervention,
                                              overwrite.prior.intervention=F)
{
    if (is.null(intervention) || is.null.intervention(intervention))
        return (components)
    
    settings = get.components.settings(components)
    transition.mapping = settings$transition.mapping
    
    #-- Check that we are resolved --#
    if (!intervention.is.resolved(intervention))
    {
        msg = "The given intervention must have all variables resolved prior to running a simulation with it"
        unresolved.var.names = get.intervention.unresolved.var.names(intervention)
        if (length(unresolved.var.names)>0)
            msg = paste0(msg, ". Missing the following variable(s): ",
                         paste0("'", unresolved.var.names, "'", collapse=', '))
        stop(msg)
    }
    
    #-- Make sure we are processed --#
    if (!intervention.is.processed(intervention))
        intervention = process.intervention(intervention, settings=settings)
    if (!intervention.is.processed(intervention))
        stop("Unable to process intervention")
    
    #-- The General Case --#
    # (iterate over registered transition elements)
    
    for (tr.el in transition.mapping$transition.elements)
    {
        element.name = tr.el$name
        sub.elem = intervention@processed[[element.name]]
        if (!is.null(sub.elem))
        {
            rates = lapply(sub.elem$rates, expand.population, target.dim.names = tr.el$dim.names)

            start.times = expand.population(sub.elem$start.times, target.dim.names = tr.el$dim.names)
            end.times = expand.population(sub.elem$end.times, target.dim.names = tr.el$dim.names)

            min.rates = expand.population(sub.elem$min.rates, target.dim.names = tr.el$dim.names)
            max.rates = expand.population(sub.elem$max.rates, target.dim.names = tr.el$dim.names)
            
            apply.functions = expand.population(sub.elem$apply.functions, target.dim.names = tr.el$dim.names)
            allow.less = expand.population(sub.elem$allow.less.than.otherwise, target.dim.names = tr.el$dim.names)
            allow.greater = expand.population(sub.elem$allow.greater.than.otherwise, target.dim.names = tr.el$dim.names)

            components = set.foreground.rates(components, element.name,
                                              rates = rates,
                                              years = sub.elem$times,
                                              start.years = start.times,
                                              end.years = end.times,
                                              apply.functions = apply.functions,
                                              foreground.scale=sub.elem$scale,
                                              allow.foreground.less = allow.less,
                                              allow.foreground.greater = allow.greater,
                                              overwrite.previous = overwrite.prior.intervention,
                                              foreground.min = min.rates,
                                              foreground.max = max.rates)
        }
    }
    
    #-- Specific Cases --#
    # Suppression
    if (!is.null(intervention@processed$suppression))
    {
        undiagnosed.states = setdiff(components$settings$CONTINUUM_OF_CARE, components$settings$DIAGNOSED_STATES) #setdiff(dimnames(start.times)[['continuum']], 'diagnosed')
        suppressed.proportions = lapply(intervention@processed$suppression$rates, function(r)
        {
            r = expand.population.to.hiv.positive(components$jheem, r)
            r[,,,,,undiagnosed.states,,] = 0
            
            r
        })
        
        start.times = expand.population.to.hiv.positive(components$jheem, intervention@processed$suppression$start.times)
        start.times[,,,,,undiagnosed.states,,] = Inf
        end.times = expand.population.to.hiv.positive(components$jheem, intervention@processed$suppression$end.times)
        end.times[,,,,,undiagnosed.states,,] = Inf
        
        min.rates = expand.population.to.hiv.positive(components$jheem, intervention@processed$suppression$min.rates)
        min.rates[,,,,,undiagnosed.states,,] = -Inf
        max.rates = expand.population.to.hiv.positive(components$jheem, intervention@processed$suppression$max.rates)
        max.rates[,,,,,undiagnosed.states,,] = -Inf
        
        apply.functions = expand.population.to.hiv.positive(components$jheem,
                                                            intervention@processed$suppression$apply.functions)
        apply.functions[,,,,,undiagnosed.states,,] = NA
        allow.less = expand.population.to.hiv.positive(components$jheem, 
                                                       intervention@processed$suppression$allow.less.than.otherwise)
        allow.greater = expand.population.to.hiv.positive(components$jheem, 
                                                          intervention@processed$suppression$allow.greater.than.otherwise)
        
        components = set.foreground.rates(components, 'suppression',
                                          rates = suppressed.proportions,
                                          years = intervention@processed$suppression$times,
                                          start.years = start.times,
                                          end.years = end.times,
                                          apply.functions = apply.functions,
                                          foreground.scale=intervention@processed$suppression$scale,
                                          allow.foreground.less = allow.less,
                                          allow.foreground.greater = allow.greater,
                                          overwrite.previous = overwrite.prior.intervention,
                                          foreground.min = min.rates,
                                          foreground.max = max.rates)
        #                                          convert.proportions.to.rates=F)
    }
    
    # PrEP Coverage
    for(prep.type in c('prep',ADDITIONAL.PREP.TYPES))
    {
        #        if (!is.null(intervention@processed[[prep.type]]))
        if (any(names(intervention@processed)==prep.type))
        {
            rates = lapply(intervention@processed[[prep.type]]$rates, expand.population.to.hiv.negative, jheem=components$jheem)
            
            start.times = expand.population.to.hiv.negative(components$jheem, intervention@processed[[prep.type]]$start.times)
            end.times = expand.population.to.hiv.negative(components$jheem, intervention@processed[[prep.type]]$end.times)
            min.rates = expand.population.to.hiv.negative(components$jheem, intervention@processed[[prep.type]]$min.rates)
            max.rates = expand.population.to.hiv.negative(components$jheem, intervention@processed[[prep.type]]$max.rates)
            apply.functions = expand.population.to.hiv.negative(components$jheem,
                                                                intervention@processed[[prep.type]]$apply.functions)
            allow.less = expand.population.to.hiv.negative(components$jheem, 
                                                           intervention@processed[[prep.type]]$allow.less.than.otherwise)
            allow.greater = expand.population.to.hiv.negative(components$jheem, 
                                                              intervention@processed[[prep.type]]$allow.greater.than.otherwise)
            
            components = register.prep.type(components, prep.type)
            components = set.foreground.rates(components, prep.type,
                                              rates = rates,
                                              years = intervention@processed[[prep.type]]$times,
                                              start.years = start.times,
                                              end.years = end.times,
                                              apply.functions = apply.functions,
                                              foreground.scale=intervention@processed[[prep.type]]$scale,
                                              allow.foreground.less = allow.less,
                                              allow.foreground.greater = allow.greater,
                                              overwrite.previous = overwrite.prior.intervention,
                                              foreground.min = min.rates,
                                              foreground.max = max.rates)
            #                                              convert.proportions.to.rates=F)
        }
        
        
        # PrEP Effectiveness (RR)
        #        if (!is.null(intervention@processed$rr.prep))
        rr.type = paste0('rr.',prep.type)
        if (any(names(intervention@processed)==rr.type))
        {
            rates = lapply(intervention@processed[[rr.type]]$rates, expand.population.to.hiv.negative, jheem=components$jheem)
            
            start.times = expand.population.to.hiv.negative(components$jheem, intervention@processed[[rr.type]]$start.times)
            end.times = expand.population.to.hiv.negative(components$jheem, intervention@processed[[rr.type]]$end.times)
            min.rates = expand.population.to.hiv.negative(components$jheem, intervention@processed[[rr.type]]$min.rates)
            max.rates = expand.population.to.hiv.negative(components$jheem, intervention@processed[[rr.type]]$max.rates)
            apply.functions = expand.population.to.hiv.negative(components$jheem,
                                                                intervention@processed[[rr.type]]$apply.functions)
            allow.less = expand.population.to.hiv.negative(components$jheem, 
                                                           intervention@processed[[rr.type]]$allow.less.than.otherwise)
            allow.greater = expand.population.to.hiv.negative(components$jheem, 
                                                              intervention@processed[[rr.type]]$allow.greater.than.otherwise)
            
            components = register.prep.type(components, prep.type)
            components = set.foreground.rates(components, rr.type,
                                              rates = rates,
                                              years = intervention@processed[[rr.type]]$times,
                                              start.years = start.times,
                                              end.years = end.times,
                                              apply.functions = apply.functions,
                                              foreground.scale=intervention@processed[[rr.type]]$scale,
                                              allow.foreground.less = allow.less,
                                              allow.foreground.greater = allow.greater,
                                              overwrite.previous = overwrite.prior.intervention,
                                              foreground.min = min.rates,
                                              foreground.max = max.rates)
            #                                              convert.proportions.to.rates=F)
        }
    }    
    
    # Needle Exchange
    if (!is.null(intervention@processed$needle.exchange))
    {
        rates = lapply(intervention@processed$needle.exchange$rates, function(r){
            expand.population.to.general(jheem=components$jheem, r)[,,,,idu.states]
        })
        
        start.times = expand.population.to.general(components$jheem, intervention@processed$needle.exchange$start.times)[,,,,idu.states]
        end.times = expand.population.to.general(components$jheem, intervention@processed$needle.exchange$end.times)[,,,,idu.states]
        min.rates = expand.population.to.general(components$jheem, intervention@processed$needle.exchange$min.rates)[,,,,idu.states]
        max.rates = expand.population.to.general(components$jheem, intervention@processed$needle.exchange$max.rates)[,,,,idu.states]
        apply.functions = expand.population.to.general(components$jheem,
                                                       intervention@processed$needle.exchange$apply.functions)[,,,,idu.states]
        allow.less = expand.population.to.general(components$jheem, 
                                                  intervention@processed$needle.exchange$allow.less.than.otherwise)[,,,,idu.states]
        allow.greater = expand.population.to.general(components$jheem, 
                                                     intervention@processed$needle.exchange$allow.greater.than.otherwise)[,,,,idu.states]
        
        components = set.foreground.rates(components, 'needle.exchange',
                                          rates = rates,
                                          years = intervention@processed$needle.exchange$times,
                                          start.years = start.times,
                                          end.years = end.times,
                                          apply.functions = apply.functions,
                                          foreground.scale=intervention@processed$needle.exchange$scale,
                                          allow.foreground.less = allow.less,
                                          allow.foreground.greater = allow.greater,
                                          overwrite.previous = overwrite.prior.intervention,
                                          foreground.min = min.rates,
                                          foreground.max = max.rates)
        #                                          convert.proportions.to.rates=F)
    }
    
    # IDU Transitions
    for (idu.trans in c('idu.incidence','idu.remission','idu.relapse'))
    {
        state.for.trans = c(idu.incidence='never_idu',
                            idu.remission='active_IDU',
                            idu.relapse='IDU_in_remission'
        )[idu.trans]
        if (!is.null(intervention@processed[[idu.trans]]))
        {
            rates = lapply(intervention@processed[[idu.trans]]$rates, function(r){
                expand.population.to.general(jheem=components$jheem, r)[,,,,state.for.trans]
            })
            
            start.times = expand.population.to.general(components$jheem, intervention@processed[[idu.trans]]$start.times)[,,,,state.for.trans]
            end.times = expand.population.to.general(components$jheem, intervention@processed[[idu.trans]]$end.times)[,,,,state.for.trans]
            min.rates = expand.population.to.general(components$jheem, intervention@processed[[idu.trans]]$min.rates)[,,,,state.for.trans]
            max.rates = expand.population.to.general(components$jheem, intervention@processed[[idu.trans]]$max.rates)[,,,,state.for.trans]
            apply.functions = expand.population.to.general(components$jheem,
                                                           intervention@processed[[idu.trans]]$apply.functions)[,,,,state.for.trans]
            allow.less = expand.population.to.general(components$jheem, 
                                                      intervention@processed[[idu.trans]]$allow.less.than.otherwise)[,,,,state.for.trans]
            allow.greater= expand.population.to.general(components$jheem, 
                                                        intervention@processed[[idu.trans]]$allow.greater.than.otherwise)[,,,,state.for.trans]
            
            components = set.foreground.rates(components, idu.trans,
                                              rates = rates,
                                              years = intervention@processed[[idu.trans]]$times,
                                              start.years = start.times,
                                              end.years = end.times,
                                              apply.functions = apply.functions,
                                              foreground.scale=intervention@processed[[idu.trans]]$scale,
                                              allow.foreground.less = allow.less,
                                              allow.foreground.greater = allow.greater,
                                              overwrite.previous = overwrite.prior.intervention,
                                              foreground.min = min.rates,
                                              foreground.max = max.rates)
            #                                              convert.proportions.to.rates=F)
        }
    }
    
    # Sexual Transmission
    if (!is.null(intervention@processed$heterosexual.transmission) ||
        !is.null(intervention@processed$msm.transmission))
    {
        if (!is.null(intervention@processed$heterosexual.transmission) &&
            any(intervention@processed$heterosexual.transmission$apply.functions=='absolute', na.rm=T))
            stop("'absolute' apply.function cannot be used for heterosexual.transmission")
        
        if (!is.null(intervention@processed$msm.transmission) &&
            any(intervention@processed$msm.transmission$apply.functions=='absolute', na.rm=T))
            stop("'absolute' apply.function cannot be used for msm.transmission")
        
        merged.times = union(intervention@processed$heterosexual.transmission$times,
                             intervention@processed$msm.transmission$times)
        rates = lapply(merged.times, function(time){
            r.het = r.msm = NULL
            if (!is.null(intervention@processed$heterosexual.transmission) && 
                any(intervention@processed$heterosexual.transmission$times==time))
            {
                i = (1:length(intervention@processed$heterosexual.transmission$times))[intervention@processed$heterosexual.transmission$times==time][1]
                r.het = intervention@processed$heterosexual.transmission$rates[[i]]
            }
            
            if (!is.null(intervention@processed$msm.transmission) && 
                any(intervention@processed$msm.transmission$times==time))
            {
                i = (1:length(intervention@processed$msm.transmission$times))[intervention@processed$msm.transmission$times==time][1]
                r.msm = intervention@processed$msm.transmission$rates[[i]]
            }
            
            merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                           heterosexual.arr = r.het,
                                                           msm.arr = r.msm,
                                                           default.value = NaN)
        })
        
        start.times = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                     heterosexual.arr = intervention@processed$heterosexual.transmission$start.times,
                                                                     msm.arr = intervention@processed$msm.transmission$start.times,
                                                                     default.value = Inf)
        end.times = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                   heterosexual.arr = intervention@processed$heterosexual.transmission$end.times,
                                                                   msm.arr = intervention@processed$msm.transmission$end.times,
                                                                   default.value = Inf)
        min.rates = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                   heterosexual.arr = intervention@processed$heterosexual.transmission$min.rates,
                                                                   msm.arr = intervention@processed$msm.transmission$min.rates,
                                                                   default.value = -Inf)
        max.rates = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                   heterosexual.arr = intervention@processed$heterosexual.transmission$max.rates,
                                                                   msm.arr = intervention@processed$msm.transmission$max.rates,
                                                                   default.value = Inf)
        character.na = character(); character.na[1] = NA
        apply.functions = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                         heterosexual.arr = intervention@processed$heterosexual.transmission$apply.functions,
                                                                         msm.arr = intervention@processed$msm.transmission$apply.functions,
                                                                         default.value = character.na)
        allow.less = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                    heterosexual.arr = intervention@processed$heterosexual.transmission$allow.less.than.otherwise,
                                                                    msm.arr = intervention@processed$msm.transmission$allow.less.than.otherwise,
                                                                    default.value = F)
        allow.greater = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                       heterosexual.arr = intervention@processed$heterosexual.transmission$allow.greater.than.otherwise,
                                                                       msm.arr = intervention@processed$msm.transmission$allow.greater.than.otherwise,
                                                                       default.value = F)
        
        sexual.transmission.scales = unique(c(intervention@processed$heterosexual.transmission$scale,
                                              intervention@processed$msm.transmission$scale))
        
        if (length(sexual.transmission.scales)>1)
            stop(paste0("Scales are different for heterosexual transmission ('",
                        intervention@processed$heterosexual.transmission$scale, 
                        "') and msm sexual transmission ('",
                        intervention@processed$msm.transmission$scale,
                        "')"))
        
        components = set.foreground.rates(components, 'sexual.transmission',
                                          rates = rates,
                                          years = merged.times,
                                          start.years = start.times,
                                          end.years = end.times,
                                          apply.functions = apply.functions,
                                          foreground.scale=sexual.transmission.scales,
                                          allow.foreground.less = allow.less,
                                          allow.foreground.greater = allow.greater,
                                          overwrite.previous = overwrite.prior.intervention,
                                          foreground.min = min.rates,
                                          foreground.max = max.rates)
        #                                          convert.proportions.to.rates=F)
    }
    
    # IDU Transmission
    if (!is.null(intervention@processed$idu.transmission))
    {
        if (any(intervention@processed$idu.transmission$apply.functions=='absolute', na.rm=T))
            stop("'absolute' apply.function cannot be used for idu.transmission")
        
        rates = lapply(intervention@processed$idu.transmission$rates, function(r){
            expand.array.to.contact(r[,,,idu.states])
        })
        
        start.times = expand.array.to.contact(intervention@processed$idu.transmission$start.times[,,,idu.states])
        end.times = expand.array.to.contact(intervention@processed$idu.transmission$end.times[,,,idu.states])
        min.rates = expand.array.to.contact(intervention@processed$idu.transmission$min.rates[,,,idu.states])
        max.rates = expand.array.to.contact(intervention@processed$idu.transmission$max.rates[,,,idu.states])
        apply.functions = expand.array.to.contact(intervention@processed$idu.transmission$apply.functions[,,,idu.states])
        allow.less = expand.array.to.contact(intervention@processed$idu.transmission$allow.less.than.otherwise[,,,idu.states])
        allow.greater = expand.array.to.contact(intervention@processed$idu.transmission$allow.greater.than.otherwise[,,,idu.states])
        
        components = set.foreground.rates(components, 'idu.transmission',
                                          rates = rates,
                                          years = intervention@processed$idu.transmission$times,
                                          start.years = start.times,
                                          end.years = end.times,
                                          apply.functions = apply.functions,
                                          foreground.scale=intervention@processed$idu.transmission$scale,
                                          allow.foreground.less = allow.less,
                                          allow.foreground.greater = allow.greater,
                                          overwrite.previous = overwrite.prior.intervention,
                                          foreground.min = min.rates,
                                          foreground.max = max.rates)
        #                                          convert.proportions.to.rates=F)
    }
    
    #-- Return --#
    components
}

OLD.setup.components.for.intervention <- function(components,
                                              intervention,
                                              overwrite.prior.intervention=F)
{
    if (is.null(intervention) || is.null.intervention(intervention))
        return (components)
    
    if (!intervention.is.resolved(intervention))
    {
        msg = "The given intervention must have all variables resolved prior to running a simulation with it"
        unresolved.var.names = get.intervention.unresolved.var.names(intervention)
        if (length(unresolved.var.names)>0)
            msg = paste0(msg, ". Missing the following variable(s): ",
                         paste0("'", unresolved.var.names, "'", collapse=', '))
        stop(msg)
    }
    
    if (!intervention.is.processed(intervention))
        intervention = process.intervention(intervention)
    if (!intervention.is.processed(intervention))
        stop("Unable to process intervention")
    
    # Apply static settings
    for (ss in intervention@static.settings)
    {
        components = set.components.to.static.settings(components, ss$name.chain, ss$value)
        if (length(ss$dependencies)>0)
            components = clear.dependent.values(components, ss$dependencies)
    }
    
    # Set up
    idu.states = 'active_IDU'
    
    # Testing
    if (!is.null(intervention@processed$testing))
    {
        diagnosed.states = components$settings$DIAGNOSED_STATES
        rates = lapply(intervention@processed$testing$rates, function(r)
        {
            r = expand.population.to.hiv.positive(components$jheem, r)
            r[,,,,,diagnosed.states,,] = 0
            
            r
        })
        
        start.times = expand.population.to.hiv.positive(components$jheem, intervention@processed$testing$start.times)
        start.times[,,,,,diagnosed.states,,] = Inf
        end.times = expand.population.to.hiv.positive(components$jheem, intervention@processed$testing$end.times)
        end.times[,,,,,diagnosed.states,,] = Inf
        
        min.rates = expand.population.to.hiv.positive(components$jheem, intervention@processed$testing$min.rates)
        min.rates[,,,,,diagnosed.states,,] = -Inf
        max.rates = expand.population.to.hiv.positive(components$jheem, intervention@processed$testing$max.rates)
        max.rates[,,,,,diagnosed.states,,] = -Inf
        
        apply.functions = expand.population.to.hiv.positive(components$jheem,
                                                            intervention@processed$testing$apply.functions)
        apply.functions[,,,,,diagnosed.states,,] = NAtim
        allow.less = expand.population.to.hiv.positive(components$jheem, 
                                                       intervention@processed$testing$allow.less.than.otherwise)
        allow.greater = expand.population.to.hiv.positive(components$jheem, 
                                                       intervention@processed$testing$allow.greater.than.otherwise)
        
        
        components = set.foreground.rates(components, 'testing',
                                          rates = rates,
                                          years = intervention@processed$testing$times,
                                          start.years = start.times,
                                          end.years = end.times,
                                          apply.functions = apply.functions,
                                          foreground.scale=intervention@processed$testing$scale,
                                          allow.foreground.less = allow.less,
                                          allow.foreground.greater = allow.greater,
                                          overwrite.previous = overwrite.prior.intervention,
                                          foreground.min = min.rates,
                                          foreground.max = max.rates)
#                                          convert.proportions.to.rates=F)
    }
    
    # Linkage
    components = do.set.foreground.for.hiv.positive(components=components,
                                                    intervention=intervention,
                                                    intervention.element.name='linkage',
                                                    components.element.name='linkage',
                                                    applies.to.continuum.states='unengaged',
                                                    overwrite.prior.intervention=overwrite.prior.intervention,
                                                    not.applies.value = 0)
#                                                    convert.proportions.to.rates=F)
    
    # Retention
    components = do.set.foreground.for.hiv.positive(components=components,
                                                    intervention=intervention,
                                                    intervention.element.name='retention.naive',
                                                    components.element.name='naive.to.disengaged',
                                                    applies.to.continuum.states='engaged_unsuppressed_naive',
                                                    overwrite.prior.intervention=overwrite.prior.intervention,
                                                    not.applies.value = 0)
#                                                    convert.proportions.to.rates=T,
 #                                                   invert.proportions = T)
    components = do.set.foreground.for.hiv.positive(components=components,
                                                    intervention=intervention,
                                                    intervention.element.name='retention.failing',
                                                    components.element.name='failing.to.disengaged',
                                                    applies.to.continuum.states='engaged_unsuppressed_failing',
                                                    overwrite.prior.intervention=overwrite.prior.intervention,
                                                    not.applies.value = 0)
#                                                    convert.proportions.to.rates=T,
 #                                                   invert.proportions = T)
    components = do.set.foreground.for.hiv.positive(components=components,
                                                    intervention=intervention,
                                                    intervention.element.name='retention.suppressed',
                                                    components.element.name='suppressed.to.disengaged',
                                                    applies.to.continuum.states='engaged_suppressed',
                                                    overwrite.prior.intervention=overwrite.prior.intervention,
                                                    not.applies.value = 0)
#                                                    convert.proportions.to.rates=T,
 #                                                   invert.proportions = T)

    
    # Gain of Suppression
    components = do.set.foreground.for.hiv.positive(components=components,
                                                    intervention=intervention,
                                                    intervention.element.name='gain.of.suppression.naive',
                                                    components.element.name='naive.to.suppressed',
                                                    applies.to.continuum.states='engaged_unsuppressed_naive',
                                                    overwrite.prior.intervention=overwrite.prior.intervention,
                                                    not.applies.value = 0)
#                                                    convert.proportions.to.rates=F)
    components = do.set.foreground.for.hiv.positive(components=components,
                                                    intervention=intervention,
                                                    intervention.element.name='gain.of.suppression.failing',
                                                    components.element.name='failing.to.suppressed',
                                                    applies.to.continuum.states='engaged_unsuppressed_failing',
                                                    overwrite.prior.intervention=overwrite.prior.intervention,
                                                    not.applies.value = 0)
#                                                    convert.proportions.to.rates=T)
    
    # Suppression
    if (!is.null(intervention@processed$suppression))
    {
        undiagnosed.states = setdiff(components$settings$CONTINUUM_OF_CARE, components$settings$DIAGNOSED_STATES) #setdiff(dimnames(start.times)[['continuum']], 'diagnosed')
        suppressed.proportions = lapply(intervention@processed$suppression$rates, function(r)
        {
            r = expand.population.to.hiv.positive(components$jheem, r)
            r[,,,,,undiagnosed.states,,] = 0
            
            r
        })
        
        start.times = expand.population.to.hiv.positive(components$jheem, intervention@processed$suppression$start.times)
        start.times[,,,,,undiagnosed.states,,] = Inf
        end.times = expand.population.to.hiv.positive(components$jheem, intervention@processed$suppression$end.times)
        end.times[,,,,,undiagnosed.states,,] = Inf
        
        min.rates = expand.population.to.hiv.positive(components$jheem, intervention@processed$suppression$min.rates)
        min.rates[,,,,,undiagnosed.states,,] = -Inf
        max.rates = expand.population.to.hiv.positive(components$jheem, intervention@processed$suppression$max.rates)
        max.rates[,,,,,undiagnosed.states,,] = -Inf
        
        apply.functions = expand.population.to.hiv.positive(components$jheem,
                                                            intervention@processed$suppression$apply.functions)
        apply.functions[,,,,,undiagnosed.states,,] = NA
        allow.less = expand.population.to.hiv.positive(components$jheem, 
                                                       intervention@processed$suppression$allow.less.than.otherwise)
        allow.greater = expand.population.to.hiv.positive(components$jheem, 
                                                          intervention@processed$suppression$allow.greater.than.otherwise)
        
        components = set.foreground.rates(components, 'suppression',
                                          rates = suppressed.proportions,
                                          years = intervention@processed$suppression$times,
                                          start.years = start.times,
                                          end.years = end.times,
                                          apply.functions = apply.functions,
                                          foreground.scale=intervention@processed$suppression$scale,
                                          allow.foreground.less = allow.less,
                                          allow.foreground.greater = allow.greater,
                                          overwrite.previous = overwrite.prior.intervention,
                                          foreground.min = min.rates,
                                          foreground.max = max.rates)
#                                          convert.proportions.to.rates=F)
    }
    
    # PrEP Coverage
    for(prep.type in c('prep',ADDITIONAL.PREP.TYPES))
    {
#        if (!is.null(intervention@processed[[prep.type]]))
        if (any(names(intervention@processed)==prep.type))
        {
            rates = lapply(intervention@processed[[prep.type]]$rates, expand.population.to.hiv.negative, jheem=components$jheem)
            
            start.times = expand.population.to.hiv.negative(components$jheem, intervention@processed[[prep.type]]$start.times)
            end.times = expand.population.to.hiv.negative(components$jheem, intervention@processed[[prep.type]]$end.times)
            min.rates = expand.population.to.hiv.negative(components$jheem, intervention@processed[[prep.type]]$min.rates)
            max.rates = expand.population.to.hiv.negative(components$jheem, intervention@processed[[prep.type]]$max.rates)
            apply.functions = expand.population.to.hiv.negative(components$jheem,
                                                                intervention@processed[[prep.type]]$apply.functions)
            allow.less = expand.population.to.hiv.negative(components$jheem, 
                                                           intervention@processed[[prep.type]]$allow.less.than.otherwise)
            allow.greater = expand.population.to.hiv.negative(components$jheem, 
                                                              intervention@processed[[prep.type]]$allow.greater.than.otherwise)
            
            components = register.prep.type(components, prep.type)
            components = set.foreground.rates(components, prep.type,
                                              rates = rates,
                                              years = intervention@processed[[prep.type]]$times,
                                              start.years = start.times,
                                              end.years = end.times,
                                              apply.functions = apply.functions,
                                              foreground.scale=intervention@processed[[prep.type]]$scale,
                                              allow.foreground.less = allow.less,
                                              allow.foreground.greater = allow.greater,
                                              overwrite.previous = overwrite.prior.intervention,
                                              foreground.min = min.rates,
                                              foreground.max = max.rates)
#                                              convert.proportions.to.rates=F)
        }
        
        
        # PrEP Effectiveness (RR)
#        if (!is.null(intervention@processed$rr.prep))
        rr.type = paste0('rr.',prep.type)
        if (any(names(intervention@processed)==rr.type))
        {
            rates = lapply(intervention@processed[[rr.type]]$rates, expand.population.to.hiv.negative, jheem=components$jheem)
            
            start.times = expand.population.to.hiv.negative(components$jheem, intervention@processed[[rr.type]]$start.times)
            end.times = expand.population.to.hiv.negative(components$jheem, intervention@processed[[rr.type]]$end.times)
            min.rates = expand.population.to.hiv.negative(components$jheem, intervention@processed[[rr.type]]$min.rates)
            max.rates = expand.population.to.hiv.negative(components$jheem, intervention@processed[[rr.type]]$max.rates)
            apply.functions = expand.population.to.hiv.negative(components$jheem,
                                                                intervention@processed[[rr.type]]$apply.functions)
            allow.less = expand.population.to.hiv.negative(components$jheem, 
                                                           intervention@processed[[rr.type]]$allow.less.than.otherwise)
            allow.greater = expand.population.to.hiv.negative(components$jheem, 
                                                              intervention@processed[[rr.type]]$allow.greater.than.otherwise)
            
            components = register.prep.type(components, prep.type)
            components = set.foreground.rates(components, rr.type,
                                              rates = rates,
                                              years = intervention@processed[[rr.type]]$times,
                                              start.years = start.times,
                                              end.years = end.times,
                                              apply.functions = apply.functions,
                                              foreground.scale=intervention@processed[[rr.type]]$scale,
                                              allow.foreground.less = allow.less,
                                              allow.foreground.greater = allow.greater,
                                              overwrite.previous = overwrite.prior.intervention,
                                              foreground.min = min.rates,
                                              foreground.max = max.rates)
#                                              convert.proportions.to.rates=F)
        }
    }    
    
    # Needle Exchange
    if (!is.null(intervention@processed$needle.exchange))
    {
        rates = lapply(intervention@processed$needle.exchange$rates, function(r){
            expand.population.to.general(jheem=components$jheem, r)[,,,,idu.states]
        })
        
        start.times = expand.population.to.general(components$jheem, intervention@processed$needle.exchange$start.times)[,,,,idu.states]
        end.times = expand.population.to.general(components$jheem, intervention@processed$needle.exchange$end.times)[,,,,idu.states]
        min.rates = expand.population.to.general(components$jheem, intervention@processed$needle.exchange$min.rates)[,,,,idu.states]
        max.rates = expand.population.to.general(components$jheem, intervention@processed$needle.exchange$max.rates)[,,,,idu.states]
        apply.functions = expand.population.to.general(components$jheem,
                                                       intervention@processed$needle.exchange$apply.functions)[,,,,idu.states]
        allow.less = expand.population.to.general(components$jheem, 
                                                  intervention@processed$needle.exchange$allow.less.than.otherwise)[,,,,idu.states]
        allow.greater = expand.population.to.general(components$jheem, 
                                                          intervention@processed$needle.exchange$allow.greater.than.otherwise)[,,,,idu.states]

        components = set.foreground.rates(components, 'needle.exchange',
                                          rates = rates,
                                          years = intervention@processed$needle.exchange$times,
                                          start.years = start.times,
                                          end.years = end.times,
                                          apply.functions = apply.functions,
                                          foreground.scale=intervention@processed$needle.exchange$scale,
                                          allow.foreground.less = allow.less,
                                          allow.foreground.greater = allow.greater,
                                          overwrite.previous = overwrite.prior.intervention,
                                          foreground.min = min.rates,
                                          foreground.max = max.rates)
#                                          convert.proportions.to.rates=F)
    }
    
    # IDU Transitions
    for (idu.trans in c('idu.incidence','idu.remission','idu.relapse'))
    {
        state.for.trans = c(idu.incidence='never_idu',
                            idu.remission='active_IDU',
                            idu.relapse='IDU_in_remission'
        )[idu.trans]
        if (!is.null(intervention@processed[[idu.trans]]))
        {
            rates = lapply(intervention@processed[[idu.trans]]$rates, function(r){
                expand.population.to.general(jheem=components$jheem, r)[,,,,state.for.trans]
            })
            
            start.times = expand.population.to.general(components$jheem, intervention@processed[[idu.trans]]$start.times)[,,,,state.for.trans]
            end.times = expand.population.to.general(components$jheem, intervention@processed[[idu.trans]]$end.times)[,,,,state.for.trans]
            min.rates = expand.population.to.general(components$jheem, intervention@processed[[idu.trans]]$min.rates)[,,,,state.for.trans]
            max.rates = expand.population.to.general(components$jheem, intervention@processed[[idu.trans]]$max.rates)[,,,,state.for.trans]
            apply.functions = expand.population.to.general(components$jheem,
                                                           intervention@processed[[idu.trans]]$apply.functions)[,,,,state.for.trans]
            allow.less = expand.population.to.general(components$jheem, 
                                                      intervention@processed[[idu.trans]]$allow.less.than.otherwise)[,,,,state.for.trans]
            allow.greater= expand.population.to.general(components$jheem, 
                                                      intervention@processed[[idu.trans]]$allow.greater.than.otherwise)[,,,,state.for.trans]
            
            components = set.foreground.rates(components, idu.trans,
                                              rates = rates,
                                              years = intervention@processed[[idu.trans]]$times,
                                              start.years = start.times,
                                              end.years = end.times,
                                              apply.functions = apply.functions,
                                              foreground.scale=intervention@processed[[idu.trans]]$scale,
                                              allow.foreground.less = allow.less,
                                              allow.foreground.greater = allow.greater,
                                              overwrite.previous = overwrite.prior.intervention,
                                              foreground.min = min.rates,
                                              foreground.max = max.rates)
#                                              convert.proportions.to.rates=F)
        }
    }
    
    # Sexual Transmission
    if (!is.null(intervention@processed$heterosexual.transmission) ||
        !is.null(intervention@processed$msm.transmission))
    {
        if (!is.null(intervention@processed$heterosexual.transmission) &&
            any(intervention@processed$heterosexual.transmission$apply.functions=='absolute', na.rm=T))
            stop("'absolute' apply.function cannot be used for heterosexual.transmission")
        
        if (!is.null(intervention@processed$msm.transmission) &&
            any(intervention@processed$msm.transmission$apply.functions=='absolute', na.rm=T))
            stop("'absolute' apply.function cannot be used for msm.transmission")
        
        merged.times = union(intervention@processed$heterosexual.transmission$times,
                             intervention@processed$msm.transmission$times)
        rates = lapply(merged.times, function(time){
            r.het = r.msm = NULL
            if (!is.null(intervention@processed$heterosexual.transmission) && 
                any(intervention@processed$heterosexual.transmission$times==time))
            {
                i = (1:length(intervention@processed$heterosexual.transmission$times))[intervention@processed$heterosexual.transmission$times==time][1]
                r.het = intervention@processed$heterosexual.transmission$rates[[i]]
            }
            
            if (!is.null(intervention@processed$msm.transmission) && 
                any(intervention@processed$msm.transmission$times==time))
            {
                i = (1:length(intervention@processed$msm.transmission$times))[intervention@processed$msm.transmission$times==time][1]
                r.msm = intervention@processed$msm.transmission$rates[[i]]
            }
            
            merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                           heterosexual.arr = r.het,
                                                           msm.arr = r.msm,
                                                           default.value = NaN)
        })
        
        start.times = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                     heterosexual.arr = intervention@processed$heterosexual.transmission$start.times,
                                                                     msm.arr = intervention@processed$msm.transmission$start.times,
                                                                     default.value = Inf)
        end.times = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                   heterosexual.arr = intervention@processed$heterosexual.transmission$end.times,
                                                                   msm.arr = intervention@processed$msm.transmission$end.times,
                                                                   default.value = Inf)
        min.rates = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                   heterosexual.arr = intervention@processed$heterosexual.transmission$min.rates,
                                                                   msm.arr = intervention@processed$msm.transmission$min.rates,
                                                                   default.value = -Inf)
        max.rates = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                   heterosexual.arr = intervention@processed$heterosexual.transmission$max.rates,
                                                                   msm.arr = intervention@processed$msm.transmission$max.rates,
                                                                   default.value = Inf)
        character.na = character(); character.na[1] = NA
        apply.functions = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                         heterosexual.arr = intervention@processed$heterosexual.transmission$apply.functions,
                                                                         msm.arr = intervention@processed$msm.transmission$apply.functions,
                                                                         default.value = character.na)
        allow.less = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                    heterosexual.arr = intervention@processed$heterosexual.transmission$allow.less.than.otherwise,
                                                                    msm.arr = intervention@processed$msm.transmission$allow.less.than.otherwise,
                                                                    default.value = F)
        allow.greater = merge.heterosexual.and.msm.transmission.arrays(jheem=components$jheem,
                                                                    heterosexual.arr = intervention@processed$heterosexual.transmission$allow.greater.than.otherwise,
                                                                    msm.arr = intervention@processed$msm.transmission$allow.greater.than.otherwise,
                                                                    default.value = F)
        
        sexual.transmission.scales = unique(c(intervention@processed$heterosexual.transmission$scale,
                                              intervention@processed$msm.transmission$scale))
        
        if (length(sexual.transmission.scales)>1)
            stop(paste0("Scales are different for heterosexual transmission ('",
                        intervention@processed$heterosexual.transmission$scale, 
                        "') and msm sexual transmission ('",
                        intervention@processed$msm.transmission$scale,
                        "')"))
        
        components = set.foreground.rates(components, 'sexual.transmission',
                                          rates = rates,
                                          years = merged.times,
                                          start.years = start.times,
                                          end.years = end.times,
                                          apply.functions = apply.functions,
                                          foreground.scale=sexual.transmission.scales,
                                          allow.foreground.less = allow.less,
                                          allow.foreground.greater = allow.greater,
                                          overwrite.previous = overwrite.prior.intervention,
                                          foreground.min = min.rates,
                                          foreground.max = max.rates)
#                                          convert.proportions.to.rates=F)
    }
    
    # IDU Transmission
    if (!is.null(intervention@processed$idu.transmission))
    {
        if (any(intervention@processed$idu.transmission$apply.functions=='absolute', na.rm=T))
            stop("'absolute' apply.function cannot be used for idu.transmission")
        
        rates = lapply(intervention@processed$idu.transmission$rates, function(r){
            expand.array.to.contact(r[,,,idu.states])
        })
        
        start.times = expand.array.to.contact(intervention@processed$idu.transmission$start.times[,,,idu.states])
        end.times = expand.array.to.contact(intervention@processed$idu.transmission$end.times[,,,idu.states])
        min.rates = expand.array.to.contact(intervention@processed$idu.transmission$min.rates[,,,idu.states])
        max.rates = expand.array.to.contact(intervention@processed$idu.transmission$max.rates[,,,idu.states])
        apply.functions = expand.array.to.contact(intervention@processed$idu.transmission$apply.functions[,,,idu.states])
        allow.less = expand.array.to.contact(intervention@processed$idu.transmission$allow.less.than.otherwise[,,,idu.states])
        allow.greater = expand.array.to.contact(intervention@processed$idu.transmission$allow.greater.than.otherwise[,,,idu.states])
        
        components = set.foreground.rates(components, 'idu.transmission',
                                          rates = rates,
                                          years = intervention@processed$idu.transmission$times,
                                          start.years = start.times,
                                          end.years = end.times,
                                          apply.functions = apply.functions,
                                          foreground.scale=intervention@processed$idu.transmission$scale,
                                          allow.foreground.less = allow.less,
                                          allow.foreground.greater = allow.greater,
                                          overwrite.previous = overwrite.prior.intervention,
                                          foreground.min = min.rates,
                                          foreground.max = max.rates)
#                                          convert.proportions.to.rates=F)
    }
    
    
    components
}

do.set.foreground.for.hiv.positive <- function(components,
                                               intervention,
                                               intervention.element.name,
                                               components.element.name,
                                            #   convert.proportions.to.rates,
                                               applies.to.continuum.states,
                                               overwrite.prior.intervention,
                                               not.applies.value = 0
                                              # invert.proportions=F
                                               )
{
    not.applies.states = setdiff(components$settings$CONTINUUM_OF_CARE, applies.to.continuum.states)
    if (!is.null(intervention@processed[[intervention.element.name]]))
    {
        rates = lapply(intervention@processed[[intervention.element.name]]$rates, function(r)
        {
            r = expand.population.to.hiv.positive(components$jheem, r)
            r[,,,,,not.applies.states,,] = not.applies.value
            
            r
        })
        
        start.times = expand.population.to.hiv.positive(components$jheem, intervention@processed[[intervention.element.name]]$start.times)
        start.times[,,,,,not.applies.states,,] = Inf
        end.times = expand.population.to.hiv.positive(components$jheem, intervention@processed[[intervention.element.name]]$end.times)
        end.times[,,,,,not.applies.states,,] = Inf
        
        min.rates = expand.population.to.hiv.positive(components$jheem, intervention@processed[[intervention.element.name]]$min.rates)
        min.rates[,,,,,not.applies.states,,] = -Inf
        max.rates = expand.population.to.hiv.positive(components$jheem, intervention@processed[[intervention.element.name]]$max.rates)
        max.rates[,,,,,not.applies.states,,] = -Inf
        
        apply.functions = expand.population.to.hiv.positive(components$jheem,
                                                            intervention@processed[[intervention.element.name]]$apply.functions)
        apply.functions[,,,,,not.applies.states,,] = NA
        allow.less = expand.population.to.hiv.positive(components$jheem, 
                                                       intervention@processed[[intervention.element.name]]$allow.less.than.otherwise)
        allow.greater = expand.population.to.hiv.positive(components$jheem, 
                                                       intervention@processed[[intervention.element.name]]$allow.greater.than.otherwise)
        
        
        components = set.foreground.rates(components, components.element.name,
                                          rates = rates,
                                          years = intervention@processed[[intervention.element.name]]$times,
                                          start.years = start.times,
                                          end.years = end.times,
                                          apply.functions = apply.functions,
                                          foreground.scale=intervention@processed[[intervention.element.name]]$scale,
                                          allow.foreground.less = allow.less,
                                          allow.foreground.greater = allow.greater,
                                          overwrite.previous = overwrite.prior.intervention,
                                          foreground.min = min.rates,
                                          foreground.max = max.rates
                                        #  convert.proportions.to.rates=convert.proportions.to.rates,
                                        #  invert.proportions=invert.proportions
                                          )
    }
    
    components
}

set.components.to.static.settings <- function(components, 
                                              name.chain,
                                              value)
{
    if (length(name.chain)==1)
        components[[name.chain]] = value
    else
        components = set.components.to.static.settings(components[[ name.chain[1] ]],
                                                       name.chain=name.chain[-1],
                                                       value=value)
    
    components
}

##------------##
##-- NAMING --##
##------------##



get.intervention.filename <- function(int)
{
    name = get.intervention.name(int)
    gsub(' ', '_', name)
}

##-------------##
##-- HELPERS --##
##-------------##


merge.heterosexual.and.msm.transmission.arrays <- function(jheem, heterosexual.arr, msm.arr,
                                                           default.value=NaN)
{
    rv = get.contact.array.skeleton(jheem, value=default.value,
                                    age=T, race=T, sex=T, risk=T)
    
    if (!is.null(heterosexual.arr))
    {
        heterosexual.arr = expand.array.to.contact(heterosexual.arr)
        
        male = c('heterosexual_male','msm')
        rv[,,'female',,,,male,] = heterosexual.arr[,,'female',,,,male,]
        rv[,,male,,,,'female',] = heterosexual.arr[,,male,,,,'female',]
    }
    
    if (!is.null(msm.arr))
    {
        msm.arr = expand.array.to.contact(msm.arr)
        rv[,,'msm',,,,'msm',] = msm.arr[,,'msm',,,,'msm',]
    }
    
    rv
}

expand.array.to.contact <- function(arr)
{
    from.dimnames = to.dimnames = dimnames(arr)
    names(from.dimnames) = paste0(names(from.dimnames), '.from')
    names(to.dimnames) = paste0(names(to.dimnames), '.to')
    
    dim(arr) = sapply(to.dimnames, length)
    dimnames(arr) = to.dimnames
    
    target.dimnames = c(from.dimnames, to.dimnames)
    
    was.logical = is(arr[1], 'logical')
    was.character = is(arr[1], 'character')
    
    if (was.logical)
        arr[] = as.numeric(arr)
    
    if (was.character)
    {
        index.to.char = unique(arr)
        char.to.index = (1:length(index.to.char))
        names(char.to.index) = index.to.char
        
        arr = as.integer(char.to.index[arr])
        dim(arr) = sapply(from.dimnames, length)
        dimnames(arr) = from.dimnames
    }
    
    rv = expand.population(source.population = arr,
                           target.dim.names = target.dimnames)
    
    if (was.logical)
        rv[] = as.logical(rv)
    if (was.character)
    {
        rv = as.character(index.to.char[rv])
        dim(rv) = sapply(target.dimnames, length)
        dimnames(rv) = target.dimnames
    }
    
    rv
}