
library(jheem)
library(bayesian.simulations)
library(data.table)

DATA.TYPE.NAMES = c(new='Reported Diagnoses',
                    prevalence='PWH',
                    diagnosed='Knowledge of Status',
                    suppression='Viral Suppression',
                    mortality='Mortality Among PWH',
                    testing.rate='Rate of HIV Testing',
                    incidence='Incidence')

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

TRUTH.SHAPES = c(21,23,22,24,25)

RED = '#C75D4D'
GREEN = '#458B00'
#GREEN = 'darkgreen'
BLUE = '#3278FA'
ORANGE = 'darkorange3'


##-----------------------##
##-- THE MAIN FUNCTION --##
##-----------------------##

#'@param color.by How to set colors. If color.by=='intervention', each intervention (and the baseline) gets a different color. If color.by=='split' each line denoted by split.by gets a different color
#'@param colors Information on what colors to use. Can be either a vector of colors or a function that takes an argument (n) and generates n colors. 
#'
do.plot.simulations <- function(simsets,
                                years,
                                data.types,
                                facet.by,
                                split.by,
                                dimension.subsets,
                                plot.format='individual.simulations',
                                show.truth=T,
                                
                                plot.interval.coverage=0.95,
                                summary.statistic='none',
                                summary.statistic.interval.coverage=0.95,
                                
                                colors=pal_jama(),
                                color.by=c('intervention','split')[1],
                                plot.interval.alpha=0.2,
                                simulation.alpha=0.2,
                                simulation.line.size=0.1,
                                truth.point.size=3,
                                truth.shapes=TRUTH.SHAPES,
                                truth.name='Epidemiological Target',
                                truth.color=NULL,
                                
                                label.change=F,
                                label.change.ci=T,
                                label.change.from=c(2020,2030),
                                label.change.decrease.as.positive=T,
                                label.change.size=5,
                                label.change.nudge.x=0,
                                
                                ncol=NULL,
                                nrow=NULL,
                                fixed.y=F)
{
    keep.dimensions = unique(c('year', facet.by, split.by))

    
    #-----------------------#
    #-- Argument Checking --#
    #-----------------------#
    
    for (data.type in data.types)
    {
        if (!any(data.type==names(DATA.TYPE.NAMES)))
            stop(paste0("'", data.type, "' is not a valid data.type. data.type must be one of ",
                        paste0("'", names(DATA.TYPE.NAMES), "'", collapse=', ')))
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
    
    # TODO @Todd: Error:
    # Warning: Error in sapply: object 'get.intervention.short.name' not found
    
    # Warning: Error in sapply: object 'is.null.intervention' not found
    # 77: match.fun [R/model_code/plot_simulations.R#109]
    # 76: sapply
    # 75: do.plot.simulations [R/model_code/plot_simulations.R#109]
    # 74: plot.simulations [/Users/joeflack4/projects/ending-hiv/
    # visualization/shiny/R/plot_shiny_interface.R#420]
    # 73: plotAndCache [/Users/joeflack4/projects/ending-hiv/visualization/
    # shiny/server.R#26]
    # 72: observeEventHandler [/Users/joeflack4/projects/ending-hiv/
    # visualization/shiny/server.R#219]
    # 1: runApp
    is.no.intervention = sapply(
      interventions, is.null.intervention) & !is.baseline
    
    # TODO:
    # Warning: Error in sapply: object 'get.intervention.short.name' not found
    # 77: match.fun [R/model_code/plot_simulations.R#124]
    # 76: sapply
    # 75: do.plot.simulations [R/model_code/plot_simulations.R#124]
    # 74: plot.simulations [/Users/joeflack4/projects/ending-hiv/
    # visualization/shiny/R/plot_shiny_interface.R#420]
    # 73: plotAndCache [/Users/joeflack4/projects/ending-hiv/
    # visualization/shiny/server.R#26]
    # 72: observeEventHandler [/Users/joeflack4/projects/ending-hiv/
    # visualization/shiny/server.R#219]
    # 1: runApp
    if (is.null(names(simsets)))
       simset.names = sapply(interventions, get.intervention.short.name) 
    else
       simset.names = names(simsets)
   
    if (any(is.baseline) && any(is.no.intervention))
        simset.names[is.baseline] = simset.names[is.no.intervention][1]

    years.for.simset = lapply(simsets, function(ss){
        intersect(years, ss@simulations[[1]]$years)
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
            one.df.truth$data.type = data.type #DATA.TYPE.NAMES[data.type]
        
        one.df.truth
    })
    
    if (length(truth.sub.dfs)==1)
        df.truth = truth.sub.dfs[[1]]
    else
        df.truth = as.data.frame(rbindlist(truth.sub.dfs))

    #-- Get total population --#
    total.population = get.census.totals(CENSUS.TOTALS,
                                         location=location,
                                         years=years)[,1]
    
    #-- Individual Simulations --#
    n.sim.dfs = length(simsets) * length(data.types)
    if (plot.format=='individual.simulations' && length(simsets)>0)
    {
        sim.sub.dfs = lapply(1:n.sim.dfs, function(i){
            simset.index = ceiling(i/length(data.types))
            data.type.index = (i-1) %% length(data.types) + 1
            
            one.df.sim = get.individual.sim.df(simset=simsets[[simset.index]],
                                              data.type=data.types[data.type.index],
                                              years=years.for.simset[[simset.index]],
                                              keep.dimensions=keep.dimensions,
                                              dimension.subsets=dimension.subsets,
                                              total.population = total.population)
            
            if (!is.null(one.df.sim))
            {
                one.df.sim$data.type = data.types[data.type.index]#DATA.TYPE.NAMES[data.types[data.type.index]]
                one.df.sim$intervention = simset.names[simset.index]
            }
            
            one.df.sim
        })
        
        df.sim = as.data.frame(rbindlist(sim.sub.dfs))
    }
        
    #-- Aggregated Simulations --#
    if (plot.format!='individual.simulations')
            stop("We have not set up plotting besides individual simsets")
    
    
    #-------------------------------------------------#
    #-- RENAME and FACTOR DIMENSIONS and DATA TYPES --#
    #-------------------------------------------------#
    
    df.sim$data.type = factor(DATA.TYPE.NAMES[df.sim$data.type],
                              levels=DATA.TYPE.NAMES[data.types])
    if (!is.null(df.truth))
        df.truth$data.type = factor(DATA.TYPE.NAMES[df.truth$data.type],
                                    levels=DATA.TYPE.NAMES[data.types])
    
    for (dimension in keep.dimensions[keep.dimensions!='year'])
    {
        df.sim[,dimension] = factor(PRETTY.NAMES[[dimension]][df.sim[,dimension]],
                                      levels=PRETTY.NAMES[[dimension]][dimension.subsets[[dimension]]])
        if (!is.null(df.truth))
            df.truth[,dimension] = factor(PRETTY.NAMES[[dimension]][df.truth[,dimension]],
                                          levels=PRETTY.NAMES[[dimension]][dimension.subsets[[dimension]]])
    }
    
    
    #-------------------#
    #-- SET UP SPLITS --#
    #-------------------#
    
    if (length(split.by)==0)
    {
        df.sim$split = 'All'
        if (!is.null(df.truth))
            df.truth$split = 'All'
    }
    else if (length(split.by)==1)
    {
        df.sim$split = df.sim[,split.by]
        if (!is.null(df.truth))
            df.truth$split = df.truth[,split.by]
    }
    else
    {
        df.sim$split = apply(df.sim[,split.by], 1, paste0, collapse=", ")
        if (!is.null(df.truth))
            df.truth$split = apply(df.truth[,split.by], 1, paste0, collapse=", ")
    }
        
    splits = unique(df.truth$split)
    split.shapes = rep(truth.shapes, ceiling(length(splits)/length(truth.shapes)))[1:length(splits)]
    names(split.shapes) = splits
    
    
    
    ##--------------------------##
    ##-- CALCULATE THE CHANGE --##
    ##--------------------------##
    
    df.change = NULL
    if (label.change && length(split.by)==0)
    {
        for (i in 1:length(simsets))
        {
            if (any(simsets[[i]]@simulations[[1]]$years==label.change.from[1]) &&
                any(simsets[[i]]@simulations[[1]]$years==label.change.from[2]) )
            {
                for (data.type in data.types)
                {
                    if (is.null(splits))
                        splits.for.loop = 'All'
                    else
                      splits.for.loop = splits
                    
                    for (split in splits.for.loop)
                    {
                        dist = get.data.type.level.and.change.dist(simsets[[i]],
                                                                   data.type=data.type,
                                                                   year1=label.change.from[1],
                                                                   year2=label.change.from[2],
                                                                   decrease.is.positive = label.change.decrease.as.positive)
                        
                        
                        if (plot.format=='median.and.ci')
                          stats = get.medians(dist)
                        else
                          stats = get.means(dist)
                        
                        cis = get.intervals(dist)
                        
                        level.2.mask = df.sim$data.type==DATA.TYPE.NAMES[data.type] & 
                                        df.sim$year==label.change.from[2] & 
                                        df.sim$intervention==simset.names[i] &
                                        df.sim$split == split
                        
                        if (plot.format=='individual.simulations')
                          level.2 = mean(df.sim$value[level.2.mask])
                        else
                          level.2 = df.sim$value[level.2.mask]
                        
                        label = paste0(round(100*stats['change']), '%')
                        if (label.change.ci)
                          label = paste0(label, ' [',
                                         round(100*cis['lower','change']),
                                         '-',
                                         round(100*cis['upper','change']),
                                         ']')
                        
                        df.change = rbind(df.change,
                                          data.frame(intervention=simset.names[i],
                                                     data.type = DATA.TYPE.NAMES[data.type],
                                                     label=label,
                                                     x=label.change.from[2],
                                                     y=level.2,
                                                     split=split))
                    }
                }
            }
        }
    }
    
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
#    if (color.by=='intervention')
#    {
#        truth.color = colors[1]
#        colors = colors[-1]
#    }
    
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
    
    if (!is.null(df.change))
    {
      if (color.by=='split')
        df.change$color.by = df.change$split
      else
        df.change$color.by = df.change$intervention
    }
    
    #-------------------#
    #-- MAKE THE PLOT --#
    #-------------------#
    
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
              legend.position = 'bottom', legend.direction = 'vertical')
    
    #------------#
    #-- RETURN --#
    #------------#
    
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
        rv[!is.na(rv$value),]
    }
}

get.individual.sim.df <- function(simset,
                                  data.type,
                                  years,
                                  keep.dimensions,
                                  dimension.subsets,
                                  total.population)
{
    years = intersect(simset@simulations[[1]]$years, years)
    if (length(years)==0)
        return (NULL)
    
    all.dimensions = union(keep.dimensions, names(dimension.subsets))
    
    print(paste0('data.type = ', data.type))
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
                    paste0("'", names(DATA.TYPE.NAMES), "'", collapse=', ')))
    
    reshape2::melt(arr)
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


##-------------------------------##
##-- EXTRACT SIMSET QUANTITIES --##
##-------------------------------##

#per total population in year
extract.simset.new.diagnoses <- function(simset, years, all.dimensions,
                                         dimension.subsets, total.population)
{
    total.population = total.population[as.character(years)]
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
    rv = sapply(simset@simulations, function(sim)
    {
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
        
        as.numeric(numerators) / as.numeric(denominators) * as.numeric(total.population)
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
    total.population = total.population[as.character(years)]
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
    rv = sapply(simset@simulations, function(sim)
    {
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
        
        as.numeric(numerators) / as.numeric(denominators) * as.numeric(total.population)
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
    total.population = total.population[as.character(years)]
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
    rv = sapply(simset@simulations, function(sim)
    {
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
        
        as.numeric(numerators) / as.numeric(denominators) * as.numeric(total.population)
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
    total.population = total.population[as.character(years)]
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
    rv = sapply(simset@simulations, function(sim)
    {
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
        
        as.numeric(numerators) / as.numeric(denominators) * as.numeric(total.population)
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

