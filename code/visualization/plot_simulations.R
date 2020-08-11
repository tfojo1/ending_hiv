
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
                                truth.point.size=5)
{
    location = attr(baseline.simset@simulations[[1]], 'location')
    
    keep.dimensions = unique('year', facet.by, split.by)
    
    #-- Name the Interventions --#
    
    #-- Set up the Data Frames --#
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
        df.
    }
    
    plot = ggplot()
    if (plot.format=='individual.simulations')
        plot = plot + geom_line(data=df.sim, aes(x=year, y=value, group=simulation, color=intervention),
                                alpha=simulation.alpha, size=simulation.line.size)
    else
        plot = plot + geom_ribbon(data=df.sim, aes(x=year, y=value, group=split.by, color=intervention, fill=intervention), alpha=plot.interval.alpha) +
            geom_line(data=df.sim, aes(x-year, y=value, group=split.by, color=intervention), size=simulation.line.size)
    
    plot = plot + geom_point(data=df.truth, aes(x=year, y=value, fill=intervention, shape=split.by))
    
    #not sure this is right
    if (length(unique(df.sim$facet.by))>1)
        plot = plot + facet_wrap(~facet.formula)
    
    
    plot
}

get.truth.df <- function(location,
                         data.type,
                         years,
                         keep.dimensions)
{
    
}