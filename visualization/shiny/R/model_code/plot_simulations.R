library(htmltools)

##---------------##
##-- CONSTANTS --##
##---------------##

DATA.TYPE.NAMES = c(new='Reported Cases (n)',
                    prevalence='Prevalent Cases (n)',
                    diagnosed='Knowledge of Status',
                    suppression='Viral Suppression',
                    mortality='Mortality Among PWH',
                    testing.rate='Rate of HIV Testing',
                    incidence='Incidence',
                    population='Population Size')


DATA.TYPE.AXIS.LABELS = c(
    incidence='Incident Cases (n)',
    new='Reported Cases (n)',
    prevalence='Prevalent Cases (n)',
    mortality='Deaths in PWH (n)',
    suppression='Proportion Suppressed (%)',
    diagnosed='Proportion Aware (%)',
    testing.rate='Average Tests per Person per Year',
    population='Population (n)'
)

DATA.TYPE.UNITS = c(
  incidence = 'Cases',
  new = 'Cases',
  prevalence = 'Cases',
  mortality = 'Deaths',
  suppression = 'Suppressed',
  diagnosed = 'Aware',
  testing.rate = 'Tests per year',
  population = 'People'
)

DIMENSION.NAMES = c(age='Age',
                    race='Race',
                    sex='Sex',
                    risk='Risk Factor')

PRETTY.NAMES = list(age=c("13-24 years"="13-24", 
                          "25-34 years"="25-34",
                          "35-44 years"="35-44",
                          "45-54 years"="45-54",
                          "55+ years"="55+"),
                    race=c(black='Black', hispanic='Hispanic', other='Other'),
                    risk=c(msm='MSM', idu='IDU', msm_idu='MSM+IDU', heterosexual='Heterosexual'),
                    sex=c(male='Male', female='Female'))

TRUTH.SHAPES.GGPLOT = c(21,23,22,24,25)
TRUTH.SHAPES.PLOTLY = c(0,2,1,5,6)



FRACTION.PROGRESS.SETUP.DF = 0.5

##-----------------------##
##-- THE MAIN FUNCTION --##
##-----------------------##

#'@param color.by How to set colors. If color.by=='intervention', each intervention (and the baseline) gets a different color. If color.by=='split' each line denoted by split.by gets a different color
#'@param colors Information on what colors to use. Can be either a vector of colors or a function that takes an argument (n) and generates n colors. 
#'
do.plot.simulations <- function(
    simsets,
    years,
    data.types,
    facet.by,
    split.by,
    dimension.subsets,
    plot.format='individual.simulations',
    show.truth=T,
    
    plot.interval.coverage=0.95,
  #  summary.statistic='none',
   # summary.statistic.interval.coverage=0.95,
    
    colors=pal_jama(),
    color.by=c('intervention','split')[1],
    plot.interval.alpha=0.2,
    simulation.alpha=0.1,
    simulation.line.size= (if (plot.format=='individual.simulations') 2 else 5) / (10^(!use.plotly)),
    truth.point.size=3*4^use.plotly,
    truth.shapes= if (use.plotly) TRUTH.SHAPES.PLOTLY else TRUTH.SHAPES.GGPLOT,
    truth.name='Observed Outcome',
    truth.color=NULL,
    ribbon.alpha = 0.25,
    
    label.change=F,
    label.change.ci=T,
    change.years=c(2020,2030),
    change.decrease.is.positive=T,
    label.change.size=5,
    label.change.nudge.x=0,
    label.alpha = 0.5,
    label.digits=0,
    
    name.interventions.by.number=T,
    hide.legend=F,
    
    ncol=NULL,
    nrow=NULL,
    fixed.y=F,
    
    progress.update=function(p){},
    data.type.names = DATA.TYPE.NAMES,
    return.change.data.frame=F,
  
    use.plotly=T,
    condense.legend=F,
    title.subplots=T,
  
    text.size = 12,
    title.size = text.size*1.25,
    legend.text.size = text.size,
    y.title.size = text.size,
    tick.size = text.size*.8,
  
    y.axis.title.function = NULL,
    y.title.standoff=10
  )
{
    keep.dimensions = unique(c('year', facet.by, split.by))
    
    if (is.null(y.axis.title.function))
      y.axis.title.function = function(data.type){DATA.TYPE.NAMES[data.type]}
    
    #-----------------------#
    #-- Argument Checking --#
    #-----------------------#
    
    for (data.type in data.types)
    {
        if (!any(data.type==names(data.type.names)))
            stop(
                paste0("'", data.type, 
                       "' is not a valid data.type. data.type must be one of ",
                       paste0("'", names(data.type.names), "'", collapse=', ')))
    }
    
    #--------------------------------#
    #-- Parse and Name the Simsets --#
    #--------------------------------#
    if (is.null(simsets))
        stop("simsets cannot be null - it must either be either a simset object or a list of simset objects")
    else if (is(simsets, 'simset'))
        simsets = list(simsets)
    else if (is(simsets, 'list'))
    {
        not.simset = !sapply(simsets, is, 'simset')
        if (any(not.simset))
            stop("simsets must be either a simset object or a list of simset objects")
    }
    else
        stop("simsets must be either a simset object or a list of simset objects")
    
    interventions = lapply(simsets, function(ss){attr(ss, 'intervention')})
    is.baseline = sapply(interventions, is.null)
    
    is.no.intervention = sapply(
        interventions, is.null.intervention) & !is.baseline
    
    if (name.interventions.by.number)
    {
        simset.names = character(length(simsets))
        simset.names[is.baseline] = 'Baseline'
        simset.names[is.no.intervention] = 'No Intervention'
        
        non.null.int.mask = !is.baseline & !is.no.intervention
        if (sum(non.null.int.mask)==1)
            simset.names[non.null.int.mask] = 'Intervention'
        else
            simset.names[non.null.int.mask] = paste0('Intervention ', 1:sum(non.null.int.mask))
    }
    else if (is.null(names(simsets)))
            simset.names = sapply(interventions, get.intervention.short.name)
    else
        simset.names = names(simsets)
    
   
    
    if (any(is.baseline) && any(is.no.intervention))
        simset.names[is.baseline] = simset.names[is.no.intervention][1]
    
    years.for.simset = lapply(simsets, function(ss){
        
        simset.years = ss@simulations[[1]]$years
        
        simset.run.from.year = attr(ss, 'run.from.year')
        if (!is.null(simset.run.from.year))
            simset.years = intersect(simset.run.from.year:max(simset.years), simset.years)
        
        intersect(years, simset.years)
    })
    
    if (any(is.baseline) && any(!is.baseline))
    {
        intervention.years = unique(as.numeric(unlist(
            sapply( (1:length(simsets))[!is.baseline], function(i){
                ss = simsets[[i]]
                if (is.no.intervention[i])
                    ss@simulations[[1]]$years
                else
                    ss@simulations[[1]]$years[-1]
            }))))
        for (i in (1:length(simsets))[is.baseline])
            years.for.simset[[i]] = setdiff(years.for.simset[[i]], intervention.years)
    }
    
    location = attr(simsets[[1]]@simulations[[1]], 'location')
    
    #----------------------------#
    #-- Set up the Data Frames --#
    #----------------------------#
    
    #-- Truth --#
    truth.sub.dfs = lapply(data.types, function(data.type){
        
        one.df.truth = get.truth.df(location=location,
                                    data.type=data.type,
                                    years=years,
                                    keep.dimensions=keep.dimensions,
                                    dimension.subsets = dimension.subsets)
        if (!is.null(one.df.truth))
            one.df.truth$data.type = data.type #data.type.names[data.type]
        
        one.df.truth
    })
    
    if (length(truth.sub.dfs)==1)
        df.truth = truth.sub.dfs[[1]]
    else
        df.truth = as.data.frame(rbindlist(truth.sub.dfs))
    
    if (any(keep.dimensions=='risk') && any(keep.dimensions=='sex'))
        df.truth = df.truth[df.truth$sex != 'female' | (df.truth$risk != 'msm' & df.truth$risk != 'msm_idu'),]
    
    #-- Get total population --#
    total.population.per.simset = lapply(1:length(simsets), function(i){
        rv = sapply(simsets[[i]]@simulations, get.total.population, 
                    census.totals=CENSUS.TOTALS, 
                    years=years.for.simset[[i]])
        
        dim(rv) = c(length(years.for.simset[[i]]), simsets[[i]]@n.sim)
        rv
    })
      
    #-- Individual Simulations --#
    n.sim.dfs = length(simsets) * length(data.types)
    
    if (plot.format=='median.and.interval')
        aggregate.statistic='median'
    else
        aggregate.statistic='mean'
    
    if (length(simsets)>0)
    {
        sim.sub.df.pairs = lapply(1:n.sim.dfs, function(i){
            simset.index = ceiling(i/length(data.types))
            data.type.index = (i-1) %% length(data.types) + 1
            
            one.dfs.sim = get.sim.dfs(simset=simsets[[simset.index]],
                                      data.type=data.types[data.type.index],
                                      years=years.for.simset[[simset.index]],
                                      keep.dimensions=keep.dimensions,
                                      dimension.subsets=dimension.subsets,
                                      total.population = total.population.per.simset[[simset.index]],
                                      get.individual.sims = plot.format=='individual.simulations',
                                      get.change.df=label.change || return.change.data.frame,
                                      aggregate.statistic = aggregate.statistic,
                                      ci.coverage=plot.interval.coverage,
                                      change.years=change.years,
                                      change.decrease.is.positive=change.decrease.is.positive)
  
            progress.update(FRACTION.PROGRESS.SETUP.DF * i/n.sim.dfs)
            
            one.dfs.sim
        })
        
        df.sim.subs = lapply(1:length(sim.sub.df.pairs), function(i){
            
            pair = sim.sub.df.pairs[[i]]
            simset.index = ceiling(i/length(data.types))
            data.type.index = (i-1) %% length(data.types) + 1
            
            if (is.null(pair))
                NULL
            else
            {
                pair$df.sim$data.type = data.types[data.type.index]#data.type.names[data.types[data.type.index]]
                pair$df.sim$intervention = simset.names[simset.index]
                pair$df.sim
            }
        })
        
        df.sim = as.data.frame(rbindlist(df.sim.subs))
        
        if (any(keep.dimensions=='risk') && any(keep.dimensions=='sex'))
            df.sim = df.sim[df.sim$sex != 'female' | (df.sim$risk != 'msm' & df.sim$risk != 'msm_idu'),]
        
        if (label.change || return.change.data.frame)
        {
            df.change.subs = lapply(1:length(sim.sub.df.pairs), function(i){
                
                pair = sim.sub.df.pairs[[i]]
                simset.index = ceiling(i/length(data.types))
                data.type.index = (i-1) %% length(data.types) + 1
                
                if (is.null(pair) || is.null(pair$df.change))
                    NULL
                else
                {
                    pair$df.change = cbind(intervention = simset.names[simset.index],
                                           data.type = data.type.names[data.types[data.type.index]],
                                           pair$df.change)
                    pair$df.change
                }
            })
            df.change = as.data.frame(rbindlist(df.change.subs))
            if (any(keep.dimensions=='risk') && any(keep.dimensions=='sex'))
                df.change = df.change[df.change$sex != 'female' | (df.change$risk != 'msm' & df.change$risk != 'msm_idu'),]
            attr(df.change, 'stat') = aggregate.statistic
            attr(df.change, 'interval.coverage') = plot.interval.coverage
            attr(df.change, 'decrease.is.positive') = change.decrease.is.positive
        }
        else
          df.change = NULL
            
    }
    
    #-------------------------------------------------#
    #-- RENAME and FACTOR DIMENSIONS and DATA TYPES --#
    #-------------------------------------------------#
    
    df.sim$data.type = factor(data.type.names[df.sim$data.type],
                              levels=data.type.names[data.types])
    if (!is.null(df.truth))
        df.truth$data.type = factor(data.type.names[df.truth$data.type],
                                    levels=data.type.names[data.types])
    
    for (dimension in keep.dimensions[keep.dimensions!='year'])
    {
        if (use.plotly)
        {
            df.sim[,dimension] = PRETTY.NAMES[[dimension]][as.character(df.sim[,dimension])]
            
            if (!is.null(df.truth))
                df.truth[,dimension] = PRETTY.NAMES[[dimension]][as.character(df.truth[,dimension])]
        }
        else
        {
            df.sim[,dimension] = factor(PRETTY.NAMES[[dimension]][as.character(df.sim[,dimension])],
                                        levels=PRETTY.NAMES[[dimension]][dimension.subsets[[dimension]]])
        
            if (!is.null(df.truth))
                df.truth[,dimension] = factor(PRETTY.NAMES[[dimension]][as.character(df.truth[,dimension])],
                                              levels=PRETTY.NAMES[[dimension]][dimension.subsets[[dimension]]])
        }
    }
    
    #-------------------#
    #-- SET UP SPLITS --#
    #-------------------#
    
    if (length(split.by)==0)
    {
        df.sim$split = 'All'
        if (!is.null(df.truth))
            df.truth$split = 'All'
        if (!is.null(df.change))
            change.split = rep('All', dim(df.change)[1])
    }
    else if (length(split.by)==1)
    {
        df.sim$split = df.sim[,split.by]
        if (!is.null(df.truth))
            df.truth$split = df.truth[,split.by]
        if (!is.null(df.change))
            change.split = df.change[,split.by]
    }
    else
    {
        df.sim$split = apply(df.sim[,split.by], 1, paste0, collapse=", ")
        if (!is.null(df.truth))
            df.truth$split = apply(df.truth[,split.by], 1, paste0, collapse=", ")
        if (!is.null(df.change))
            change.split = apply(df.change[,split.by],1, paste0, collapse=", ")
    }
    
    splits = unique(c(unique(df.truth$split), unique(df.sim$split), unique(df.change$split)))
    split.shapes = rep(truth.shapes, ceiling(length(splits)/length(truth.shapes)))[1:length(splits)]
    names(split.shapes) = splits
    
    
    #-------------------#
    #-- SET UP GROUPS --#
    #-------------------#
    
    if (plot.format=='individual.simulations')
        df.sim$group = paste0(df.sim$split, "_", df.sim$intervention, "_", df.sim$simulation)
    else
        df.sim$group = paste0(df.sim$intervention, "_", df.sim$split)
    
    #-------------------#
    #-- SET UP COLORS --#
    #-------------------#
    
    if (color.by=='split')
        color.names = splits
    else
        color.names = c(truth.name, unique(simset.names))
    
    if (is(colors, 'function'))
    {
        if (color.by=='intervention' && !is.null(truth.color))
            colors = c(truth.color, colors(length(color.names)-1))
        else
            colors = colors(length(color.names))
    }
    else if (is(colors, 'character'))
    {
        if (color.by=='intervention' && !is.null(truth.color))
        {   
            if (length(colors)<(length(color.by)-1))
                stop(paste0("Not enough colors given in 'colors': ", length(color.by)-1, " colors are needed"))
            
            colors = c(truth.color, colors[1:(length(color.names)-1)])
        }
        else
        {
            if (length(colors)<length(color.by))
                stop(paste0("Not enough colors given in 'colors': ", length(color.by), " colors are needed"))
            
            colors = colors[1:length(color.names)]
        }
    }
    else
        stop("'colors' must be either a character vector or a function that creates a character vector")
    
    names(colors) = color.names
    if (color.by=='intervention')
    {
        truth.color = colors[1]
        colors = colors[-1]
    }
    
    if (!is.null(df.sim))
    {
        if (color.by=='split')
            df.sim$color.by = df.sim$split
        else
            df.sim$color.by = df.sim$intervention
    }
    
    if (!is.null(df.truth))
    {
        if (color.by=='split')
            df.truth$color.by = df.truth$split
        else
            df.truth$color.by = truth.name
    }
    
#    if (!is.null(df.change))
#    {
#        if (color.by=='split')
#            df.change$color.by = df.change$split
#        else
#            df.change$color.by = df.change$intervention
#    }
    
    
    
    #-------------------#
    #-------------------#
    #-- MAKE THE PLOT --#
    #-------------------#
    #-------------------#
    
    if (use.plotly)
    {
        #-- SOME INITIAL FORMATTING  (rounding, etc) --#
        df.sim = format.values.for.plotly(df.sim, data.type.names)
        condense.legend.names = condense.legend && 
          (color.by=='split' || length(unique(simset.names))==1)
        
        if (!is.null(df.truth))
            df.truth = format.values.for.plotly(df.truth, data.type.names)
        
        
        #-- SET UP THE FACET CATEGORIES --#
        df.sim$facet = df.sim$data.type
        if (!is.null(df.truth))
            df.truth$facet = df.truth$data.type
        if (!is.null(df.change))
            change.facet = df.change$data.type
        
        if (length(facet.by)>0)
        {
            for (ff in facet.by)
            {
                df.sim$facet = paste0(df.sim$facet, '\n', df.sim[,ff])
                if (!is.null(df.truth))
                    df.truth$facet = paste0(df.truth$facet, '\n', df.truth[,ff])
                if (!is.null(df.change))
                    change.facet = paste0(change.facet, '\n', df.change[,ff])
            }
        }
        facet.categories = as.character(unique(df.sim$facet))
        data.types.for.facet.categories = sapply(strsplit(facet.categories, '\n'), function(spl){spl[1]})
        raw.data.types.for.facet.categories = sapply(data.types.for.facet.categories, function(dt){
            names(data.type.names)[data.type.names==dt]
        })
        names(data.types.for.facet.categories) = facet.categories
        
        
        #-- PRE-CRHUNCH FOR LEGEND --#
        #This lets us show the truth legend exactly once for each split
        show.truth.legend.for.facet = sapply(splits, function(split){
            non.empty = sapply(facet.categories, function(ff){
                mask = df.truth$facet==ff & df.truth$split==split
                any(!is.na(df.truth$value[mask]))
            })
            facet.categories[non.empty][1]
        })
        names(show.truth.legend.for.facet) = splits
        
        
        #-- CALCULATE THE NUMBER OF ROWS --#
        if (is.null(nrow))
        {
            if (is.null(ncol))
                nrow = floor(sqrt(length(facet.categories)))
            else
                nrow = ceiling(length(facet.categories) / ncol)
        }
        

        #-- SET UP FOR PROGRESS --#        
        unique.simset.names = unique(simset.names)
        n.steps = length(facet.categories) * length(unique.simset.names) * length(splits)
        

        
        #-- GENERATE THE PLOTLY OBJECT --#
        #Make a different subplot for each facet panel
        subplots = lapply(1:length(facet.categories), function(ff.i){
            
            ff = facet.categories[ff.i]
            data.type = raw.data.types.for.facet.categories[ff.i]
            
            plot = plot_ly(x=~year)
            
            #df.facet = df.sim[df.sim$facet == ff,]
            facet.mask = df.sim$facet==ff
            
            
            #-- SET UP RIBBONS (if using mean/median + interval) --#
            if (plot.format != 'individual.simulations')
            {
                for (int.i in 1:length(unique.simset.names))
                {
                    intervention = unique.simset.names[int.i]
                    int.mask = facet.mask & df.sim$intervention==intervention
                    
                    sims = unique(df.sim$simulation[int.mask])
                    for (split.i in 1:length(splits))
                    {
                        split = splits[split.i]
                        split.mask = int.mask & df.sim$split==split
                        
                        show.legend = !hide.legend && ff==facet.categories[1] && !hide.legend
                        
                        path.name = intervention
                        if (color.by == 'split' && length(split.by)>0)
                        {
                            color = colors[split]
                            path.name = paste0('Simulations, ', split)
                            show.legend = show.legend && intervention == simset.names[1]
                        }
                        else
                        {
                            color = colors[intervention]
                            path.name = intervention
                            show.legend = show.legend && split == splits[1]
                        }
                        
                        hover.text = make.hover.text(year=df.sim$year[split.mask],
                                                     value=df.sim$value[split.mask],
                                                     lower=df.sim$lower[split.mask],
                                                     upper=df.sim$upper[split.mask],
                                                     data.type=data.type,
                                                     split=split)
                        
                        plot = add_ribbons(plot, data=df.sim[split.mask,],
                                         ymin=~lower, ymax=~upper,
                                         fillcolor=color, 
                                         opacity=ribbon.alpha,
                                         legendgroup=if (condense.legend) NULL else 'Simulations',
                                         line = list(color=color,
                                                     width=simulation.line.size/5),
                                         text = hover.text,
                                         hoverinfo = 'text',
                                         name=path.name,
                                         showlegend = F)
                    }
                }
            }
            
            #-- SET UP THE MAIN LINES (sim or mean/median) AND TRUTH --#
            #make the sim
            for (int.i in 1:length(unique.simset.names))
            {
                intervention = unique.simset.names[int.i]
                int.mask = facet.mask & df.sim$intervention==intervention
                
                sims = unique(df.sim$simulation[int.mask])
                for (split.i in 1:length(splits))
                {
                    split = splits[split.i]
                    split.mask = int.mask & df.sim$split==split
                    
                    show.legend = !hide.legend && ff==facet.categories[1] && !hide.legend
                    
                    path.name = intervention
                    if (color.by == 'split' && length(split.by)>0)
                    {
                        color = colors[split]
                        path.name = paste0('Simulations, ', split)
                        show.legend = show.legend && intervention == simset.names[1]
                    }
                    else
                    {
                        color = colors[intervention]
                        path.name = intervention
                        show.legend = show.legend && split == splits[1]
                    }
                    
                    
                    if (plot.format=='individual.simulations')
                    {
                        #this code makes the lines in the legend show up without opacity
                        if (show.legend && !condense.legend.names)
                            plot = add_paths(plot, data=df.sim[split.mask & !is.na(df.sim$value),],
                                             x=~year[1], y=~value[1], color=color, 
                                             opacity=1,
                                             legendgroup=if (condense.legend) NULL else 'Simulations',
                                             line = list(color=color,
                                                         width=simulation.line.size),
                                             name=path.name,
                                             showlegend = T)
                        
                        hover.text = make.hover.text(year=df.sim$year[split.mask],
                                                     value=df.sim$value[split.mask],
                                                     sim=df.sim$simulation[split.mask],
                                                     data.type=data.type,
                                                     split=split)
                        
                        plot = add_paths(plot, data=df.sim[split.mask,],
                                         y=~value, color=color, 
                                         opacity=simulation.alpha,
                                         legendgroup=if (condense.legend) NULL else 'Simulations',
                                         line = list(color=color,
                                                     width=simulation.line.size),
                                         name=path.name,
                                         text=hover.text,
                                         hoverinfo='text',
                                         showlegend = F,
                                         transforms = list(
                                             list(type = 'groupby',
                                                  groups = ~simulation))
                        )
                    }
                    else
                    {
                        hover.text = make.hover.text(year=df.sim$year[split.mask],
                                                     value=df.sim$value[split.mask],
                                                     lower=df.sim$lower[split.mask],
                                                     upper=df.sim$upper[split.mask],
                                                     data.type=data.type,
                                                     split=split)
                      
                        plot = add_paths(plot, data=df.sim[split.mask & !is.na(df.sim$value),],
                                         y=~value, color=color,
                                         opacity=1,
                                         legendgroup=if (condense.legend) NULL else 'Simulations',
                                         line = list(color=color,
                                                     width=simulation.line.size),
                                         name=path.name,
                                         text=hover.text,
                                         hoverinfo='text',
                                         showlegend = show.legend
                                         )
                    }
                    
                    progress.update(FRACTION.PROGRESS.SETUP.DF +
                                        (1-FRACTION.PROGRESS.SETUP.DF) *
                                        (split.i + (int.i-1) * length(splits) +
                                        (ff.i-1) * length(splits) * length(unique.simset.names)) /
                                        n.steps)
                }
            }
            
            if (!is.null(df.truth))
            {
                for (split in splits)
                {
                    mask = df.truth$facet==ff & df.truth$split==split
                    if (any(!is.na(df.truth$value[mask])))
                    {
                        show.legend = !hide.legend && !is.na(show.truth.legend.for.facet[split]) &&
                            show.truth.legend.for.facet[split] == ff &&
                            !hide.legend
                        
                        if (color.by!='split')
                            show.legend = show.legend && split == splits[1]
                        
                        one.truth.name = truth.name
                        if (color.by == 'split')
                        {
                            color = colors[split]
                            if (condense.legend.names)
                                one.truth.name = split
                            else
                                one.truth.name = paste0(one.truth.name, ', ', split)
                        }
                        else
                            color = truth.color
                        
                        if (show.legend)
                            plot = add_trace(plot, data=df.truth[mask,],
                                             type='scatter',
                                             mode = if (condense.legend.names) 'lines+markers' else 'markers',
                                             x=~year[1], y=~value[1], color=color,
                                             name=one.truth.name,
                                             marker = list(color=color,
                                                           size=truth.point.size,
                                                           symbol=split.shapes[split],
                                                           line = list(
                                                               color = '#000000FF',
                                                               width = 1
                                                           )),
                                             legendgroup=if (condense.legend) NULL else 'Truth',
                                             showlegend = T)
                        
                        hover.text = make.hover.text(year=df.truth$year[mask],
                                                     value=df.truth$value[mask],
                                                     source=df.truth$Source[mask],
                                                     data.type=data.type,
                                                     split=split)
                        
                        plot = add_markers(plot, data=df.truth[mask,],
                                           y=~value, color=color,
                                           name=one.truth.name,
                                           marker = list(color=color,
                                                         size=truth.point.size,
                                                         symbol=split.shapes[split],
                                                         line = list(
                                                             color = '#000000FF',
                                                             width = 1
                                                         )),
                                           text = hover.text,
                                           hoverinfo='text',
                                           legendgroup=if (condense.legend) NULL else 'Truth',
                                           showlegend = F)
                    }
                }
            }
            
            ##-- ADD THE CHANGE LABELS --##
            if (label.change)
            {
                for (int.i in 1:length(unique.simset.names))
                {
                    intervention = unique.simset.names[int.i]
                    for (split in splits)
                    {
                        mask = change.facet==ff & change.split==split & df.change$intervention==intervention
                        
                        if (any(mask))
                        {
                            if (color.by == 'split' && length(split.by)>0)
                                color = colors[split]
                            else
                                color = colors[intervention]
                            
                            change.name = paste0("change_", change.years[1], "_to_", change.years[2])
                            if (plot.format=='median.and.interval')
                                label.text = paste0(round(100*df.change[mask, paste0(change.name, "_median")], digits=label.digits), '%')
                            else
                                label.text = paste0(round(100*df.change[mask, paste0(change.name, "_mean")], digits=label.digits), '%')
                            if (label.change.ci)
                                label.text = paste0(label.text, " [",
                                                    round(100*df.change[mask, paste0(change.name, "_interval_lower")], label.digits),
                                                    " to ",
                                                    round(100*df.change[mask, paste0(change.name, "_interval_upper")], label.digits),
                                                    "%]")
                            
                            if (change.decrease.is.positive)
                                label.text = paste0(label.text, " Reduction")
                            else
                                label.text = paste0(label.text, "")
                            
                            if (plot.format=='median.and.interval')
                            {
                                #   y1.name = paste0(change.years[1], "_median")
                                y2.name = paste0(change.years[2], "_median")
                            }
                            else
                            {
                                #   y1.name = paste0(change.years[1], "_mean")
                                y2.name = paste0(change.years[2], "_mean")
                            }
                            
                            plot = add.plot.label(plot, 
                                                  text=label.text,
                                                  x=change.years[2] + label.change.nudge.x,
                                                  y=df.change[mask, y2.name],
                                                  xanchor = 'left',
                                                  fill=color,
                                                  alpha=label.alpha)
                        }
                    }
                }
            }
            
            axis.title = DATA.TYPE.AXIS.LABELS[names(data.type.names)[data.types.for.facet.categories[ff]==data.type.names]]
            axis.title = sapply(names(data.type.names)[data.types.for.facet.categories[ff]==data.type.names], function(data.type){
                y.axis.title.function(data.type)
            })
            if (plot.format != 'individual.simulations')
              axis.title = paste0(axis.title, " (",
                                  round(100*plot.interval.coverage), 
                                  '% Prediction Interval)')
              
            yaxis.list = list(rangemode = "tozero",
                              title = list(text=axis.title,
                                           standoff=y.title.standoff,
                                           font=list(size=y.title.size)),
                              tickfont = list(size=tick.size))
            
            if (is.pct.data.type(data.types.for.facet.categories[ff], data.type.names))
                yaxis.list$tickformat = '%'
            else
                yaxis.list$tickformat = ',d'
            
            plot = layout(plot,
                          yaxis = yaxis.list,
                          xaxis=list(title='',
                                     tickfont = list(size=tick.size)))
                
            if (title.subplots)
                plot = add_annotations(plot,
                                       text = ff,
                                       x = 0.5,
                                       y = 1.05,
                                       yref = "paper",
                                       xref = "paper",
                                       align = "center",
                                       xanchor = "center",
                                       yanchor = "top",
                                       font = list(size = title.size),
                                       showarrow = FALSE
            )
            
            plot
        })

        
        if (length(facet.categories)==1)
            plot = subplots[[1]]
        else
            plot = subplot(subplots, shareY = fixed.y, titleY=T, nrows=nrow, 
                           margin=c(0.05,0.05,0.06,0.06)) #L,R,T,B
        
        legend.list = list(orientation = 'h', 
                           xanchor = "center",
                           x = 0.5,
                           font = list(size=legend.text.size))
        if (!condense.legend)
            legend.list$traceorder = 'grouped'
        plot = layout(plot, 
                      legend = legend.list)
        
   
    }
    
    
    
    
    
    else
    {
        plot = ggplot()
        if (plot.format=='individual.simulations')
        {
            plot = plot + geom_line(data=df.sim, aes(x=year, y=value, group=group, color=color.by),
                                    alpha=simulation.alpha, size=simulation.line.size)
        }
        else
        {
            plot = plot + 
                geom_ribbon(data=df.sim, aes(x=year, ymin=ci.lower, ymax=ci.upper,
                                             group=split.by, color=color.by, fill=color.by), alpha=plot.interval.alpha) +
                geom_line(data=df.sim, aes(x=year, y=value, group=group, color=color.by), size=simulation.line.size)
        }
        
        if (!is.null(df.truth))
        {
            
            plot = plot + geom_point(data=df.truth, aes(x=year, y=value, shape=split, fill=color.by), 
                                     size=truth.point.size)
        }
        
        
        #Add labeled change
        if (!is.null(df.change))
        {
            plot = plot + geom_label(data=df.change, aes(x=x, y=y, label=label, fill=color.by), 
                                     hjust='center', size=label.change.size, alpha=0.5,
                                     label.padding = unit(0.15, 'lines'), nudge_x = label.change.nudge.x)
        }
        
        #-----------------------#
        #-- SET UP FACET WRAP --#
        #-----------------------#
        
        if (fixed.y)
            facet.scales = 'fixed'
        else
            facet.scales = 'free_y'
        
        if (length(data.types)>1)
        {
            if (length(facet.by)>0 && any(sapply(dimension.subsets[facet.by], length)>1))
                facet.formula = as.formula(paste0('~data.type+', paste0(facet.by, collapse='+')))
            else
                facet.formula = ~data.type
        }
        else
        {
            if (length(facet.by)>0 && any(sapply(dimension.subsets[facet.by], length)>1))
                facet.formula = as.formula(paste0('~', paste0(facet.by, collapse='+')))
            else
                facet.formula = NULL
            
        }
        
        if (!is.null(facet.formula))
            plot = plot + facet_wrap(facet.formula, scales=facet.scales)
        
        
        #----------#
        #-- Axes --#
        #----------#
        
        plot = plot + 
            scale_y_continuous(name=NULL,
                               labels=function(x){
                                   if (all(is.na(x) | x <= 1))
                                       paste0(100*x, '%')
                                   else
                                       format(x, big.mark = ',')
                               },
                               limits = c(0,NA)) +
            scale_x_continuous(name='Year',
                               labels = round)
        
        #------------#
        #-- Styles --#
        #------------#
        
        split.legend.name = NULL#paste0(DIMENSION.NAMES[split.by], collapse=", ")
        
        #colors
        if (color.by=='split')
        {
            if (length(split.by)==0)
                plot = plot + scale_color_manual(values=colors, guide=F) +
                    scale_fill_manual(values=colors, guide=F)
            else
                plot = plot + scale_color_manual(values=colors, name=split.legend.name) +
                    scale_fill_manual(values=colors, name=split.legend.name)
            
        }
        else
        {
            if (length(simsets)>1)
                plot = plot + scale_color_manual(values=colors, name="Intervention")
            else
                plot = plot + scale_color_manual(values=colors, guide=F)
            
            if (!is.null(df.change))
            {
                if (length(simsets)>1)
                    plot = plot + scale_fill_manual(values=colors, name="Intervention")
                else
                    plot = plot + scale_fill_manual(values=colors, guide=F)
            }
        }
        
        if (length(split.by)==0)
            plot = plot +
            scale_shape_manual(values=split.shapes, guide=F)
        else
        {
            plot = plot +
                scale_shape_manual(values=split.shapes, name=split.legend.name)
        }
        
        plot = plot + 
            theme(panel.grid=element_blank(), panel.background=element_blank(),
                  axis.line.x.bottom = element_line(color = 'black'),
                  axis.line.y.left = element_line(color = 'black'),
                  legend.position = 'bottom', legend.direction = 'vertical') +
            ylab(NULL)
    }
    
    #------------#
    #-- RETURN --#
    #------------#
    
    if (return.change.data.frame)
        list(plot=plot, change.df=df.change)
    else
        plot
}

get.data.type.level.and.change.dist <- function(simset,
                                                data.type,
                                                year1,
                                                year2,
                                                decrease.is.positive=T,
                                                statistic=c('mean','median')[1])
{
    if (all(simset@simulations[[1]]$years != year1))
        stop("The year ", year1, " is not contained in the simulation")
    if (all(simset@simulations[[1]]$years != year2))
        stop("The year ", year2, " is not contained in the simulation")
    
    if (data.type=='incidence')
        extract.fn = function(sim, years){extract.incidence(sim, years=years, per.population=NA)}
    else if (data.type=='new')
        extract.fn = function(sim, years){extract.new.diagnoses(sim, years=years, per.population=NA)}
    else if (data.type=='prevalence')
        extract.fn = function(sim, years){extract.prevalence(sim, years=years, per.population=NA)}
    else if (data.type=='diagnosed')
        extract.fn = function(sim, years){extract.diagnosed.hiv(sim, years=years, per.population=1)}
    else if (data.type=='suppression')
        extract.fn = function(sim, years){extract.suppression(sim, years=years, per.population=1)}
    else if (data.type=='mortality')
        extract.fn = function(sim, years){extract.overall.hiv.mortality(sim, years=years, per.population=1)}
    else if (data.type=='testing.rate')
        extract.fn = function(sim, years){extract.testing.rates(sim, years=years, per.population=1)}
    else if (data.type=='population')
        extract.fn = function(sim, years){extract.population(sim, years=years)}
    else
        stop(paste0("'", data.type, "' is not a valid data type"))
    
    
    fn = function(sim,...){
        values = extract.fn(sim, c(year1,year2))
        rv = c(value1=as.numeric(values[1]),
               value2=as.numeric(values[2]),
               change=as.numeric((values[2]-values[1])/values[1]))
        if (decrease.is.positive)
            rv[3] = -rv[3]
        rv
    }
    
    extract.simset.distribution(simset, fn)
}

##---------------------------------------------------------##
##-- HIGH-LEVEL HELPERS TO ASSEMBLE THE MAIN DATA FRAMES --##
##---------------------------------------------------------##

get.truth.df <- function(location,
                         data.type,
                         years,
                         keep.dimensions,
                         dimension.subsets)
{
    surv = msa.surveillance
    dimension.subsets = get.nontrivial.dimension.subsets(dimension.subsets,
                                                         data.type=data.type,
                                                         surv=surv)
    
    all.dimensions = union(keep.dimensions, names(dimension.subsets))
    if (data.type=='incidence')
        return (NULL)
    
    if (data.type=='population')
    {
        rv = get.population.from.census(location,
                                        years=years,
                                        keep.dimensions=keep.dimensions,
                                        dimension.subsets=dimension.subsets)
        rv$Source = 'US Census Bureau'
        rv
    }
    else
    {
      rv = get.surveillance.data(surv, location.codes=location, data.type=data.type,
                                 years=years,
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
              rv = get.surveillance.data(state.surveillance, location.codes=states, data.type=data.type.for.surveillance,
                                         years = years,
                                         age=any(all.dimensions=='age'), race=any(all.dimensions=='race'),
                                         sex=any(all.dimensions=='sex'), risk=any(all.dimensions=='risk'),
                                         aggregate.locations = T, aggregate.years = F,
                                         throw.error.if.missing.data = F)
          }
      }
      
      if (is.null(rv))
          rv
      else
      {
          if (length(dimension.subsets)>0)
              rv = access(rv, 
                          age=dimension.subsets$age,
                          race=dimension.subsets$race,
                          sex=dimension.subsets$sex,
                          risk=dimension.subsets$risk,
                          collapse.length.one.dimensions = F)
          
          rv = reshape2::melt(rv)
          rv = rv[!is.na(rv$value),]
          
          if (data.type=='suppression' || data.type=='diagnosed')
              rv$Source = 'Local Health Dept'
          else
              rv$Source = 'CDC'
          
          rv
      }
    }
}

get.population.from.census <- function(location,
                                       years,
                                       keep.dimensions,
                                       dimension.subsets)
{
    counties = counties.for.msa(location)
    years = intersect(years, CENSUS$years)
    
    if (any(keep.dimensions)=='risk')
        NULL
    else 
    {
        raw.totals = get.census.totals(CENSUS.TOTALS, location=counties, years=years, collapse.counties = T)
        if (length(keep.dimension)==1 && keep.dimensions=='year')
            raw.totals
        else
        {
            stratified = get.census.data()
        }
    }
}

get.sim.dfs <- function(simset,
                       data.type,
                       years,
                       keep.dimensions,
                       dimension.subsets,
                       total.population,
                       get.individual.sims=T,
                       get.change.df=F,
                       aggregate.statistic = 'mean',
                       ci.coverage=0.95,
                       change.years,
                       change.decrease.is.positive)
{
    years = intersect(simset@simulations[[1]]$years, years)
    if (length(years)==0)
        return (NULL)
    
    all.dimensions = union(keep.dimensions, names(dimension.subsets))
    
    #print(paste0('data.type = ', data.type))
    if (data.type=='new')
        arr = extract.simset.new.diagnoses(simset,
                                           years = years, 
                                           all.dimensions = keep.dimensions,
                                           dimension.subsets = dimension.subsets,
                                           total.population = total.population)
    else if (data.type=='incidence')
        arr = extract.simset.incidence(simset,
                                       years = years, 
                                       all.dimensions = keep.dimensions,
                                       dimension.subsets = dimension.subsets,
                                       total.population = total.population)
    else if (data.type=='prevalence')
        arr = extract.simset.prevalence(simset,
                                        years = years, 
                                        all.dimensions = keep.dimensions,
                                        dimension.subsets = dimension.subsets,
                                        total.population = total.population)
    else if (data.type=='mortality')
        arr = extract.simset.hiv.mortality(simset,
                                           years = years, 
                                           all.dimensions = keep.dimensions,
                                           dimension.subsets = dimension.subsets,
                                           total.population = total.population)
    else if (data.type=='suppression')
        arr = extract.simset.suppression(simset,
                                         years = years, 
                                         all.dimensions = keep.dimensions,
                                         dimension.subsets = dimension.subsets)
    else if (data.type=='diagnosed')
        arr = extract.simset.knowledge.of.status(simset,
                                                 years = years, 
                                                 all.dimensions = keep.dimensions,
                                                 dimension.subsets = dimension.subsets)
    else
        stop(paste0("'", data.type, "' is not a valid data.type. data.type must be one of ",
                    paste0("'", names(data.type.names), "'", collapse=', ')))
    
    
    if (get.individual.sims)
      df.sim = reshape2::melt(arr)
    
    #Aggregate to mean/median and CI if requested
    #Generate the change df if requested
    df.change = NULL
    sim.years = as.numeric(dimnames(arr)[['year']])
    if (!get.individual.sims || get.change.df)
    {
        n.dim.arr = length(dim(arr))
        n.sim = dim(arr)['simulation']
        dim.names.arr = dimnames(arr) 
        
        if (aggregate.statistic=='mean')
            stat = rowMeans(arr, dims = length(keep.dimensions))
        else if (aggregate.statistic=='median')
            stat = apply(arr, keep.dimensions, median)
        else
            stop("aggregate.statistic must be either 'mean' or 'median'")
        
        if (is.null(dim(stat)))
        {
            dim(stat) = c(year=length(sim.years))
            dimnames(stat) = list(year=as.character(sim.years))
        }
        else
        {
            dim(stat) = sapply(dim.names.arr[-n.dim.arr], length)
            dimnames(stat) = dim.names.arr[-n.dim.arr]
        }
        
        alpha = (1-ci.coverage)/2
        arr[is.na(arr)] = 0 #this fixes female msm
        ci = apply(arr, keep.dimensions, quantile, probs=c(alpha, 1-alpha), na.rm=T)
        dim(ci) = c(2, dim(ci)[2], prod(dim(ci)[-(1:2)]))
        
        if (!get.individual.sims)
        {
            df.sim = reshape2::melt(stat, value.name='value')
            df.sim = cbind(df.sim, 
                           'lower'=as.numeric(ci[1,,]), 
                           'upper'=as.numeric(ci[2,,]))
        }
        
        if (get.change.df && any(sim.years==change.years[1]) && any(sim.years==change.years[2]))
        {
            year1.index = (1:length(sim.years))[sim.years==change.years[1]]
            year2.index = (1:length(sim.years))[sim.years==change.years[2]]
            
            dim(stat) = c(length(sim.years), prod(dim(stat)[-1]))
            
            
            if (n.dim.arr==2)
            {
                change.v = (arr[year2.index,] - arr[year1.index,]) / arr[year1.index,]
                if (change.decrease.is.positive)
                    change.v = -change.v
                
                if (aggregate.statistic=='mean')
                    stat.change = mean(change.v)
                else
                    stat.change = median(change.v)
                
             #   dim(stat.change) = 1
                
                change.ci = quantile(change.v, probs=c(alpha,1-alpha))
                dim(change.ci) = c(2,1)
                
            }
            else
            {
                dim(arr) = c(length(sim.years), prod(dim(arr)[-c(1,n.dim.arr)]), n.sim)
                change.arr = (arr[year2.index,,] - arr[year1.index,,]) / arr[year1.index,,]
                if (change.decrease.is.positive)
                    change.arr = -change.arr
                dim(change.arr) = sapply(dim.names.arr[-1], length)
                
                if (aggregate.statistic=='mean')
                    stat.change = rowMeans(change.arr, dims=length(dim(change.arr))-1)
                else
                    stat.change = apply(change.arr, 1:(length(dim(change.arr))-1), median)
                
                dim(stat.change) = sapply(dim.names.arr[-c(1,n.dim.arr)], length)
                dimnames(stat.change) = dim.names.arr[-c(1,n.dim.arr)]
                
                change.arr[is.na(change.arr)] = 0
                change.ci = apply(change.arr, 1:(length(dim(change.arr))-1), quantile, probs=c(alpha,1-alpha))
                dim(change.ci) = c(2,prod(dim(stat.change)))
            }
            
            df.change = reshape2::melt(stat.change, value.name='value')
            for (d in names(df.change)[names(df.change)!='value'])
                df.change[,d] = PRETTY.NAMES[[d]][df.change[,d]]
            names(df.change)[names(df.change)=='value'] = paste0("change_", change.years[1], "_to_", change.years[2], "_", aggregate.statistic)
            
            df.change = cbind(df.change,
                              lower=change.ci[1,],
                              upper=change.ci[2,],
                              year1=stat[year1.index,],
                              year1.lower=ci[1,year1.index,],
                              year1.upper=ci[2,year1.index,],
                              year2=stat[year2.index,],
                              year2.lower=ci[1,year2.index,],
                              year2.upper=ci[2,year2.index,])
            names(df.change)[names(df.change)=='lower'] = paste0("change_", change.years[1], "_to_", change.years[2], "_interval_lower")
            names(df.change)[names(df.change)=='upper'] = paste0("change_", change.years[1], "_to_", change.years[2], "_interval_upper")
            
            names(df.change)[names(df.change)=='year1'] = paste0(change.years[1], "_", aggregate.statistic)
            names(df.change)[names(df.change)=='year1.lower'] = paste0(change.years[1], "_interval_lower")
            names(df.change)[names(df.change)=='year1.upper'] = paste0(change.years[1], "_interval_upper")
            
            names(df.change)[names(df.change)=='year2'] = paste0(change.years[2], "_", aggregate.statistic)
            names(df.change)[names(df.change)=='year2.lower'] = paste0(change.years[2], "_interval_lower")
            names(df.change)[names(df.change)=='year2.upper'] = paste0(change.years[2], "_interval_upper")
        }
    }
    
    list(df.sim=df.sim,
         df.change=df.change)
}

##----------------##
##-- ADD LABELS --##
##----------------##

add.plot.label <- function(plot,
                           text,
                           x,
                           y,
                           color='black',
                           fill='white',
                           alpha=0.5,
                           font.size=12,
                           font.family=NULL,
                           align='center',
                           valign='middle',
                           xanchor=c('left','center','right')[2],
                           yanchor=c('top','middle','bottom')[2],
                           pad=2,
                           border.color='black')
{
    rgb = as.character(col2rgb(fill))
    bg.color = paste0("rgba(",
                      rgb[1], ",",
                      rgb[2], ",",
                      rgb[3], ",",
                      alpha, ",",
                      ")")
    
    font.list = list(size=font.size,
                     color=color)
    if (!is.null(font.family))
        font.list$family=font.family
    
    layout(plot, 
           annotations=list(text=text,
                            x=x,
                            y=y,
                            bgcolor=bg.color,
                            font=font.list,
                            showarrow=F,
                            align=align,
                            valign=valign,
                            xanchor=xanchor,
                            yanchor=yanchor,
                            bordercolor=border.color,
                            borderpad=pad)
    )
}

##-----------------------------##
##-- OTHER MID-LEVEL HELPERS --##
##-----------------------------##

get.nontrivial.dimension.subsets <- function(dimension.subsets,
                                             data.type,
                                             surv)
{
    non.trivial.mask = sapply(names(dimension.subsets), function(dimension){
        
        if (is.null(dimension.subsets[[dimension]]))
            F
        else
        {
            surv.elem.mask = grepl(paste0(data.type, '.*', dimension), names(surv))
            if (any(surv.elem.mask))
            {
                surv.elem = surv[surv.elem.mask][[1]]
                all.subsets = dimnames(surv.elem)[[dimension]]
                !setequal(all.subsets, dimension.subsets[[dimension]])
            }
            else
                T
        }
    })
    
    dimension.subsets[non.trivial.mask]
}


make.pretty.change.data.frame <- function(change.df, data.type.names=DATA.TYPE.NAMES)
{
    df.names = names(change.df)
    pre.change.index = (1:length(df.names))[grepl('change',df.names)][1]-1
    stats.description = paste0(",\n", attr(change.df, 'stat'), 
                               " [", round(100*attr(change.df, 'interval.coverage')), "% interval]")
    
    rv = change.df[,1:pre.change.index]
    
    names(rv)[names(rv)=='data.type'] = "Epidemiological Indicator"
    names(rv) = gsub("\\.", " ", names(rv))
    names(rv)[names(rv)=='risk'] = 'Risk Factor'
    names(rv) = toupper.first(names(rv))
    
    year1 = substr(df.names[pre.change.index+4],1,4)
    year2 = substr(df.names[pre.change.index+7],1,4)
    
    rv$change = paste0(round(100*change.df[,pre.change.index+1]),
                          '% [',
                          round(100*change.df[,pre.change.index+2]), 
                          ' to ',
                          round(100*change.df[,pre.change.index+3]),
                          ']')
    if (attr(change.df, 'decrease.is.positive'))
        names(rv)[names(rv)=='change'] = paste0("Reduction ", year1, " to ", year2, 
                                                 stats.description)
    else
        names(rv)[names(rv)=='change'] = paste0("Change ", year1, " to ", year2, 
                                               stats.description)
    
    
    pct.mask = is.pct.data.type(change.df$data.type, data.type.names)
    mult = rep(1, length(pct.mask))
    mult[pct.mask] = 100
    unit = rep('', length(pct.mask))
    unit[pct.mask] = '%'
    
    rv$year1 = paste0(format(round(mult*change.df[,pre.change.index+4]), big.mark = ','), 
                      unit,
                      ' [',
                      format(round(mult*change.df[,pre.change.index+5]), big.mark = ','),
                      ' - ',
                      format(round(mult*change.df[,pre.change.index+6]), big.mark = ','),
                      ']')
    names(rv)[names(rv)=='year1'] = paste0(year1, " Level", stats.description)
    
    rv$year2 = paste0(format(round(mult*change.df[,pre.change.index+7]), big.mark = ','), 
                      unit,
                      ' [',
                      format(round(mult*change.df[,pre.change.index+8]), big.mark = ','),
                      ' - ',
                      format(round(mult*change.df[,pre.change.index+9]), big.mark = ','),
                      ']')
    names(rv)[names(rv)=='year2'] = paste0(year2, " Level", stats.description)
    
    rv
}

toupper.first <- function(x)
{
    spl = strsplit(x, ' ')
    sapply(spl, function(z){
        capitalized = paste0(toupper(substr(z,1,1)), substr(z,2,nchar(z)))
        paste0(capitalized, collapse=' ')
    })
}

##-------------------------------##
##-- EXTRACT SIMSET QUANTITIES --##
##-------------------------------##

#per total population in year
extract.simset.new.diagnoses <- function(simset, years, all.dimensions,
                                         dimension.subsets, total.population)
{
    eg = do.extract.new.diagnoses(simset@simulations[[1]],
                                  years=years, 
                                  keep.dimensions=all.dimensions,
                                  per.population=NA,
                                  ages=dimension.subsets[['age']],
                                  races=dimension.subsets[['race']],
                                  subpopulations=dimension.subsets[['subpopulation']],
                                  sexes=dimension.subsets[['sex']],
                                  risks=dimension.subsets[['risk']],
                                  continuum.from=NULL,
                                  continuum.to=NULL,
                                  cd4=NULL,
                                  hiv.subsets=NULL,
                                  use.cdc.categorizations=T)
    rv = sapply(1:length(simset@simulations), function(i)
    {
        sim = simset@simulations[[i]]
        numerators = do.extract.new.diagnoses(sim,
                                              years=years, 
                                              keep.dimensions=all.dimensions,
                                              per.population=NA,
                                              ages=dimension.subsets[['age']],
                                              races=dimension.subsets[['race']],
                                              subpopulations=dimension.subsets[['subpopulation']],
                                              sexes=dimension.subsets[['sex']],
                                              risks=dimension.subsets[['risk']],
                                              continuum.from=NULL,
                                              continuum.to=NULL,
                                              cd4=NULL,
                                              hiv.subsets=NULL,
                                              use.cdc.categorizations=T)
        denominators = do.extract.population.subset(sim, years=years, keep.dimensions = 'year', use.cdc.categorizations = T)
       
        as.numeric(numerators) / as.numeric(denominators) * total.population[,i]
    })
    
    if (is.null(dim(eg)))
    {
        dim.names = list(names(eg), 1:simset@n.sim)
        names(dim.names) = c(all.dimensions, 'simulation')
    }
    else
        dim.names = c(dimnames(eg), list(simulation=1:simset@n.sim))
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}

#per total population in year
extract.simset.incidence <- function(simset, years, all.dimensions,
                                     dimension.subsets, total.population)
{
    eg = do.extract.incidence(simset@simulations[[1]],
                              years=years, 
                              keep.dimensions=all.dimensions,
                              per.population=NA,
                              ages=dimension.subsets[['age']],
                              races=dimension.subsets[['race']],
                              subpopulations=dimension.subsets[['subpopulation']],
                              sexes=dimension.subsets[['sex']],
                              risks=dimension.subsets[['risk']],
                              non.hiv.subsets = NULL,
                              continuum=NULL,
                              cd4=NULL,
                              hiv.subsets=NULL,
                              use.cdc.categorizations=T)
    rv = sapply(1:length(simset@simulations), function(i)
    {
        sim = simset@simulations[[i]]
        numerators = do.extract.incidence(sim,
                                          years=years, 
                                          keep.dimensions=all.dimensions,
                                          per.population=NA,
                                          ages=dimension.subsets[['age']],
                                          races=dimension.subsets[['race']],
                                          subpopulations=dimension.subsets[['subpopulation']],
                                          sexes=dimension.subsets[['sex']],
                                          risks=dimension.subsets[['risk']],
                                          non.hiv.subsets = NULL,
                                          continuum=NULL,
                                          cd4=NULL,
                                          hiv.subsets=NULL,
                                          use.cdc.categorizations=T)
        denominators = do.extract.population.subset(sim, years=years, keep.dimensions = 'year', use.cdc.categorizations = T)
        
        as.numeric(numerators) / as.numeric(denominators) * total.population[,i]
    })
    
    if (is.null(dim(eg)))
    {
        dim.names = list(names(eg), 1:simset@n.sim)
        names(dim.names) = c(all.dimensions, 'simulation')
    }
    else
        dim.names = c(dimnames(eg), list(simulation=1:simset@n.sim))
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}

#prevalence of those aware of diagnosis
#per total population in year
extract.simset.prevalence <- function(simset, years, all.dimensions,
                                      dimension.subsets, total.population)
{
    eg = do.extract.prevalence(simset@simulations[[1]],
                               years=years, 
                               keep.dimensions=all.dimensions,
                               per.population=NA,
                               ages=dimension.subsets[['age']],
                               races=dimension.subsets[['race']],
                               subpopulations=dimension.subsets[['subpopulation']],
                               sexes=dimension.subsets[['sex']],
                               risks=dimension.subsets[['risk']],
                               continuum='diagnosed',
                               cd4s=NULL,
                               hiv.subsets=NULL,
                               use.cdc.categorizations=T)
    rv = sapply(1:length(simset@simulations), function(i)
    {
        sim = simset@simulations[[i]]
        numerators = do.extract.prevalence(sim,
                                           years=years, 
                                           keep.dimensions=all.dimensions,
                                           per.population=NA,
                                           ages=dimension.subsets[['age']],
                                           races=dimension.subsets[['race']],
                                           subpopulations=dimension.subsets[['subpopulation']],
                                           sexes=dimension.subsets[['sex']],
                                           risks=dimension.subsets[['risk']],
                                           continuum='diagnosed',
                                           cd4s=NULL,
                                           hiv.subsets=NULL,
                                           use.cdc.categorizations=T)
        denominators = do.extract.population.subset(sim, years=years, keep.dimensions = 'year', use.cdc.categorizations = T)
        
        as.numeric(numerators) / as.numeric(denominators) * total.population[,i]
    }) 
    
    if (is.null(dim(eg)))
    {
        dim.names = list(names(eg), 1:simset@n.sim)
        names(dim.names) = c(all.dimensions, 'simulation')
    }
    else
        dim.names = c(dimnames(eg), list(simulation=1:simset@n.sim))
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}

extract.simset.hiv.mortality <- function(simset, years, all.dimensions,
                                         dimension.subsets, total.population)
{
    eg = do.extract.overall.hiv.mortality(simset@simulations[[1]],
                                          years=years, 
                                          keep.dimensions=all.dimensions,
                                          per.population=NA,
                                          ages=dimension.subsets[['age']],
                                          races=dimension.subsets[['race']],
                                          subpopulations=dimension.subsets[['subpopulation']],
                                          sexes=dimension.subsets[['sex']],
                                          risks=dimension.subsets[['risk']],
                                          continuum='diagnosed',
                                          cd4s=NULL,
                                          hiv.subsets=NULL,
                                          use.cdc.categorizations=T)
    rv = sapply(1:length(simset@simulations), function(i)
    {
        sim = simset@simulations[[i]]
        numerators = do.extract.overall.hiv.mortality(sim,
                                                      years=years, 
                                                      keep.dimensions=all.dimensions,
                                                      per.population=NA,
                                                      ages=dimension.subsets[['age']],
                                                      races=dimension.subsets[['race']],
                                                      subpopulations=dimension.subsets[['subpopulation']],
                                                      sexes=dimension.subsets[['sex']],
                                                      risks=dimension.subsets[['risk']],
                                                      continuum='diagnosed',
                                                      cd4s=NULL,
                                                      hiv.subsets=NULL,
                                                      use.cdc.categorizations=T)
        denominators = do.extract.population.subset(sim, years=years, keep.dimensions = 'year', use.cdc.categorizations = T)
        
        as.numeric(numerators) / as.numeric(denominators) * total.population[,i]
    })
    
    if (is.null(dim(eg)))
    {
        dim.names = list(names(eg), 1:simset@n.sim)
        names(dim.names) = c(all.dimensions, 'simulation')
    }
    else
        dim.names = c(dimnames(eg), list(simulation=1:simset@n.sim))
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}

extract.simset.suppression <- function(simset, years, all.dimensions,
                                       dimension.subsets)
{
    eg = extract.suppression(simset@simulations[[1]],
                             years=years, 
                             keep.dimensions=all.dimensions,
                             per.population=1,
                             ages=dimension.subsets[['age']],
                             races=dimension.subsets[['race']],
                             subpopulations=dimension.subsets[['subpopulation']],
                             sexes=dimension.subsets[['sex']],
                             risks=dimension.subsets[['risk']],
                             continuum='diagnosed',
                             cd4=NULL,
                             hiv.subsets=NULL,
                             use.cdc.categorizations=T)
    rv = sapply(simset@simulations, extract.suppression,
                years=years, 
                keep.dimensions=all.dimensions,
                per.population=1,
                ages=dimension.subsets[['age']],
                races=dimension.subsets[['race']],
                subpopulations=dimension.subsets[['subpopulation']],
                sexes=dimension.subsets[['sex']],
                risks=dimension.subsets[['risk']],
                continuum='diagnosed',
                cd4=NULL,
                hiv.subsets=NULL,
                use.cdc.categorizations=T)
    
    if (is.null(dim(eg)))
    {
        dim.names = list(names(eg), 1:simset@n.sim)
        names(dim.names) = c(all.dimensions, 'simulation')
    }
    else
        dim.names = c(dimnames(eg), list(simulation=1:simset@n.sim))
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}

extract.simset.knowledge.of.status <- function(simset, years, all.dimensions,
                                               dimension.subsets)
{
    eg = do.extract.diagnosed.hiv(simset@simulations[[1]],
                                  years=years, 
                                  keep.dimensions=all.dimensions,
                                  per.population=1,
                                  ages=dimension.subsets[['age']],
                                  races=dimension.subsets[['race']],
                                  subpopulations=dimension.subsets[['subpopulation']],
                                  sexes=dimension.subsets[['sex']],
                                  risks=dimension.subsets[['risk']],
                                  cd4=NULL,
                                  hiv.subsets=NULL,
                                  use.cdc.categorizations=T)
    rv = sapply(simset@simulations, do.extract.diagnosed.hiv,
                years=years, 
                keep.dimensions=all.dimensions,
                per.population=1,
                ages=dimension.subsets[['age']],
                races=dimension.subsets[['race']],
                subpopulations=dimension.subsets[['subpopulation']],
                sexes=dimension.subsets[['sex']],
                risks=dimension.subsets[['risk']],
                cd4=NULL,
                hiv.subsets=NULL,
                use.cdc.categorizations=T)
    
    if (is.null(dim(eg)))
    {
        dim.names = list(names(eg), 1:simset@n.sim)
        names(dim.names) = c(all.dimensions, 'simulation')
    }
    else
        dim.names = c(dimnames(eg), list(simulation=1:simset@n.sim))
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}

##------------------##
##-- MISC HELPERS --##
##------------------##


is.pct.data.type <- function(data.type, data.type.names=NULL)
{
    rv = data.type == 'suppression' |
         data.type == 'diagnosed'
    
    if (!is.null(data.type.names))
        rv = rv | 
            data.type == data.type.names['suppression'] | 
            data.type == data.type.names['diagnosed'] 
    
    rv
}

format.values.for.plotly <- function(df,
                                     data.type.names,
                                     pct.digits=1,
                                     non.pct.digits=0)
{
    pct.mask = is.pct.data.type(df$data.type, data.type.names)
    
    df$value[pct.mask] = round(df$value[pct.mask], pct.digits+2)
    df$value[!pct.mask] = round(df$value[!pct.mask], non.pct.digits)
    
    if (any(names(df)=='lower'))
    {
        df$lower[!pct.mask] = round(df$lower[!pct.mask], non.pct.digits)
        df$lower[pct.mask] = round(df$lower[pct.mask], pct.digits+2)
    }
    if (any(names(df)=='upper'))
    {
        df$upper[!pct.mask] = round(df$upper[!pct.mask], non.pct.digits)
        df$upper[pct.mask] = round(df$upper[pct.mask], pct.digits+2)
    }
    
    df
}

make.hover.text <- function(year,
                            value,
                            sim=NULL,
                            lower=NULL,
                            upper=NULL,
                            data.type,
                            split,
                            source=NULL)
{
    if (is.pct.data.type(data.type))
    {
        value = paste0(100*value, '%')
        if (!is.null(lower))
            lower = 100*lower
        if (!is.null(upper))
            upper = paste0(100*upper, '%')
    }
    else
    {
        value = format(value, big.mark=',')
        if (!is.null(lower))
            lower = format(lower, big.mark=',')
        if (!is.null(upper))
            upper = format(upper, big.mark=',')
    }
    
    hover.text = paste0("Year: ",year,
           "\n", DATA.TYPE.UNITS[data.type], ": ", value)
    
    if (!is.null(lower) && !is.null(upper))
        hover.text = paste0(hover.text,
                            " [", trimws(lower), " to ", trimws(upper), "]")
    
    if (!is.null(sim))
        hover.text = paste0(hover.text,
                            "\nSimulation: ", sim)
    
    if (!is.null(source))
        hover.text = paste0(hover.text,
                            "\nSource: ", source)
    
    if (split != 'All')
        hover.text = paste0("<b style='text-decoration: underline'>", split, ":</b>\n", hover.text)
    
    hover.text
}
