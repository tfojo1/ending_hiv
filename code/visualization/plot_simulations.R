source('code/systematic_calibration/postprocessing.R')

do.plot.simulations <- function(baseline.simset,
                                intervention.simsets,
                                years,
                                data.types,
                                facet.by,
                                split.by,
                                dimension.subsets,
                                plot.format,
                                show.truth=T,
                                
                                plot.interval.coverage=0.95,
                                summary.statistic='none',
                                summary.statistic.interval.coverage=0.95,
                                
                                baseline.color='blue',
                                truth.color='green',
                                intervention.colors='red',
                                plot.interval.alpha=0.2,
                                simulation.alpha=0.2,
                                simulation.line.size=0.1,
                                truth.point.size=5,
                                
                                ncol=NULL,
                                nrow=NULL,
                                fixed.y=F)
{
    location = attr(baseline.simset@simulations[[1]], 'location')
    
    keep.dimensions = unique('year', facet.by, split.by)
    
    #-- Name the Interventions --#
    
    #----------------------------#
    #-- Set up the Data Frames --#
    #----------------------------#
    
    df.truth = NULL
    df.sim = NULL
    
    for (data.type in data.types)
    {
        #Get Truth
        df.truth = rbind(df.truth, 
                         get.truth.df(location=location,
                                      data.type=data.type,
                                      years=years,
                                      keep.dimensions=keep.dimensions))
        
        #Get Sim
        df.sim
    }
    
    
    #-- SET UP SPLITS --#
    
    #-------------------#
    #-- SET UP GROUPS --#
    #-------------------#
    
    if (plot.format=='individual.simulations')
        df.sim$group = paste0(df.sim$split, "_", df.sim$simulation)
    else
        df.sim$group = df.sim$split
    
    #-- SET UP STYLES (colors, etc) --#
    
    
    #-------------------#
    #-- MAKE THE PLOT --#
    #-------------------#
    
    plot = ggplot()
    if (plot.format=='individual.simulations')
        plot = plot + geom_line(data=df.sim, aes(x=year, y=value, group=group, color=intervention),
                                alpha=simulation.alpha, size=simulation.line.size)
    else
        plot = plot + 
            geom_ribbon(data=df.sim, aes(x=year, ymin=ci.lower, ymax=ci.upper,
                                         group=split.by, color=intervention, fill=intervention), alpha=plot.interval.alpha) +
            geom_line(data=df.sim, aes(x=year, y=value, group=group, color=intervention), size=simulation.line.size)
    
    if (!is.null(df.truth))
        plot = plot + geom_point(data=df.truth, aes(x=year, y=value, fill=intervention, shape=split.by))
    
    
    #-----------------------#
    #-- SET UP FACET WRAP --#
    #-----------------------#
    
    if (fixed.y)
        facet.scales = 'fixed'
    else
        facet.scales = 'free_y'
    
    if (length(data.types)>1)
    {
        if (length(split.by)>1)
            facet.formula = as.formula(paste0('~data.type+', paste0(split.by, collapse='+')))
        else
            facet.formula = ~data.type
    }
    else
    {
        if (length(split.by)>1)
            facet.formula = as.formula(paste0('~', paste0(split.by, collapse='+')))
        else
            facet.formula = NULL
        
    }
    
    if (!is.null(facet.formula))
        plot = plot + facet_wrap(facet.formula, scales=facet.scales)
    
    
    #------------#
    #-- RETURN --#
    #------------#
    
    plot
}

get.truth.df <- function(location,
                         data.type,
                         years,
                         keep.dimensions,
                         dimension.subsets)
{
    all.dimensions = union(keep.dimensions, names(dimension.subsets))
    
    surv = msa.surveillance
    
    rv = get.surveillance.data(surv, location.codes=location, data.type=data.type.for.surveillance,
                               age=any(all.dimensions=='age'), race=any(all.dimensions=='race'),
                               sex=any(all.dimensions=='sex'), risk=any(all.dimensions=='risk'),
                               aggregate.locations = T, aggregate.years = F,
                               throw.error.if.missing.data = F)
    
    if (data.type=='diagnosed' && is.null(rv) && length(all.dimensions)==1 && all.dimensions=='year')
    {
        rv = get.state.averaged.knowledge.of.status(location,
                                                    state.surveillance,
                                                    years=years,
                                                    census.totals = ALL.DATA.MANAGERS$census.totals)
        
        dim(rv) = c(year=length(years))
        dimnames(rv) = list(year = as.character(years))
    }
    else if (data.type=='suppression' && is.null(rv) &&
             is.null(get.surveillance.data(surv, location.codes=location, data.type='suppression', throw.error.if.missing.data=F)))
    {
        states = states.for.msa(location)
        if (length(states)==1)
        {
            truth.numerators = get.surveillance.data(state.surveillance, location.codes=states, data.type=data.type.for.surveillance,
                                                     age=any(all.dimensions=='age'), race=any(all.dimensions=='race'),
                                                     sex=any(all.dimensions=='sex'), risk=any(all.dimensions=='risk'),
                                                     aggregate.locations = T, aggregate.years = F,
                                                     throw.error.if.missing.data = F)
        }
    }
    
    rv
}

get.sim.df <- function(simset,
                       data.type,
                       years,
                       keep.dimensions,
                       dimension.subsets)
{
    all.dimensions = union(keep.dimensions, names(dimension.subsets))
    
    if (data.type=='suppression')
    {
        rv = sapply(simset@simulations, extract.suppression())
    }
}

get.nontrivial.dimension.subsets <- function()
{
    
}