run.simset.intervention <- function(simset,
                                    intervention,
                                    run.from.year=attr(simset, 'run.from.year'),
                                    run.to.year=2030,
                                    keep.years=(run.from.year-1):run.to.year,
                                    save.intervention=T,
                                    verbose=F,
                                    update.progress=if (verbose) function(n){print(paste0("Running simulation ", n, " of ", simset@n.sim))} else NULL,
                                    seed = 12343)
{
    # If we need to add parameters from distributions, do so now
    if (length(intervention$parameter.distributions)>0)
    {
        if (verbose)
            print("Adding new parameters")
        
        if (!is.null(seed))
            to.reset.seed = round(runif(1, min=0, max=.Machine$integer.max))
        
        new.params = NULL
        new.param.names = character()
        for (i in 1:length(intervention$parameter.distributions))
        {
            dist = intervention$parameter.distributions[[i]]
            if (!is.null(seed))
            {
                dist.names = paste0(dist@var.names, collapse=',')
                dist.seed = as.integer( (sum(as.integer(charToRaw(dist.names))) + seed) %% .Machine$integer.max )
                set.seed(dist.seed)
            }
            
            new.params = cbind(new.params, generate.random.samples(dist, simset@n.sim))
            new.param.names = c(new.param.names, dist@var.names)   
        }
        
        #reset the seed
        if (!is.null(seed))
            set.seed(to.reset.seed)
        
        bounds = get.support.bounds(dist@support)
        
        simset = add.parameters(simset, 
                                parameters=new.params,
                                parameter.names = new.param.names,
                                parameter.lower.bounds = bounds[1,],
                                parameter.upper.bounds = bounds[2,])
    }
    
    if (verbose)
        print(paste0("Running intervention ", get.intervention.name(intervention), " on ", simset@n.sim, " simulations"))
    
    need.to.resolve.intervention = !intervention.is.resolved(intervention)
    simset@simulations = lapply(1:simset@n.sim, function(i){
        if (!is.null(update.progress))
            update.progress(i)
        
        if (need.to.resolve.intervention)
            int = resolve.intervention(intervention, simset@parameters[i,])
        else
            int = intervention
        
        run.sim.intervention(simset@simulations[[i]], int,
                             run.from.year=run.from.year, run.to.year = run.to.year,
                             keep.years = keep.years)
    })
    
    #   simset = extend.simulations(simset, function(sim, parameters){
    #       components = setup.components.for.intervention(attr(sim, 'components'), intervention)
    #       run.jheem.from.components(components, start.year=run.from.year, end.year=run.to.year,
    #                                 prior.results = sim, keep.components = T, keep.years=keep.years)
    #   })
    if (verbose)
        print("Done")
    
    if (save.intervention)
        attr(simset, 'intervention') = intervention
    
    simset
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