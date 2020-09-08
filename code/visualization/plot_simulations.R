

# Set up to make sure we have what we need
if (1==2)
{
    source('code/systematic_calibration/postprocessing.R')
    source('code/data_managers/locale_mappings.R')
    source('code/data_managers/hiv_surveillance_manager.R')
    source('code/data_managers/census_totals.R')
    source('code/setup/setup_jheem_from_components.R')

    
    load('cached/DEFAULT.LOCALE.MAPPING.Rdata')
    load('cached/ALL.DATA.MANAGERS.Rdata')
    CENSUS.TOTALS = ALL.DATA.MANAGERS$census.totals
    load('cached/msa.surveillance.Rdata')
    load('cached/state.surveillance.Rdata')
    
    #for now
    load('mcmc_runs/test_simsets/31080.Rdata')
}

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

PRETTY.NAMES = list(age=c("13-24 years"="13-24 years", 
                          "25-34 years"="25-34 years",
                          "35-44 years"="35-44 years",
                          "45-54 years"="45-54 years",
                          "55+ years"="55+ years"),
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

do.plot.simulations <- function(baseline.simset,
                                intervention.simsets,
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
                                
                                baseline.color=BLUE,
                                truth.color=GREEN,
                                intervention.colors=c(RED,ORANGE),
                                plot.interval.alpha=0.2,
                                simulation.alpha=0.2,
                                simulation.line.size=0.1,
                                truth.point.size=3,
                                truth.shapes=TRUTH.SHAPES,
                                
                                ncol=NULL,
                                nrow=NULL,
                                fixed.y=F)
{
    location = attr(baseline.simset@simulations[[1]], 'location')
    
    keep.dimensions = unique(c('year', facet.by, split.by))
    
    #-----------------------#
    #-- Argument Checking --#
    #-----------------------#
    
    for (data.type in data.types)
    {
        if (!any(data.type==names(DATA.TYPE.NAMES)))
            stop(paste0("'", data.type, "' is not a valid data.type. data.type must be one of ",
                        paste0("'", names(DATA.TYPE.NAMES), "'")))
    }
    
    #------------------------------------#
    #-- Aggregate and Name the Simsets --#
    #------------------------------------#
    
    if (is(intervention.simsets, 'simset'))
        intervention.simsets = list(intervention.simsets)
    
    if (is.null(intervention.simsets))
        intervention.names = intervention.colors = NULL
    else if (is(intervention.simsets, 'list'))
    {
        if (length(intervention.simsets)==0)
            intervention.names = intervention.colors = NULL
        else
        {
            intervention.is.no.intervention = sapply(intervention.simsets, function(simset){
                is.null(attr(simset, 'intervention'))
            })
            
            if (is.null(names(intervention.simsets)))
            {
                intervention.names = paste0("Intervention ", 1:length(intervention.simsets))
                intervention.names[intervention.is.no.intervention] = "No Intervention"
            }
            else
                intervention.names = names(intervention.simsets)
            
            intervention.colors = rep(intervention.colors, ceiling(length(intervention.names)/length(intervention.colors)))[1:length(intervention.names)]
        }
    }
    else
        stop("intervention.simset must be either of class 'simset' or a list containing only 'simset' objects")
    
    baseline.years = NULL
    if (is.null(baseline.simset))
        baseline.name = baseline.color = NULL
    else if (!is(baseline.simset, 'simset'))
        stop("baseline.simset must be of class 'simset")
    else if (!is.null(intervention.names) && any(intervention.is.no.intervention))
    {
        baseline.name = intervention.names[intervention.is.no.intervention][1]
        baseline.years = setdiff(intersect(years, baseline.simset@simulations[[1]]$years),
                                 intervention.simsets[[1]]@simulations[[1]]$years)
        intervention.colors[intervention.is.no.intervention]
    }
    else
    {
        baseline.name = "Pre-Intervention"
        baseline.years = intersect(years, baseline.simset@simulations[[1]]$years)
    }
    
    all.simsets = c(list(baseline.simset), intervention.simsets)
    simset.colors = c(baseline.color, intervention.colors)
    names(all.simsets) = c(baseline.name, intervention.names)
    if (is.null(baseline.name) || is.null(intervention.names) || !any(intervention.is.no.intervention))
        names(simset.colors) = c(baseline.name, intervention.names)
    else
        names(simset.colors) = c(baseline.name, intervention.names[!intervention.is.no.intervention])
    years.for.simset = list(baseline.years,
                            lapply(intervention.simsets, function(simest){
                                intersect(years, simset@simulations[[1]]$years)
                            }))
    
    
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
    n.sim.dfs = length(all.simsets) * length(data.types)
    if (plot.format=='individual.simulations' && length(all.simsets)>0)
    {
        sim.sub.dfs = lapply(1:n.sim.dfs, function(i){
            simset.index = ceiling(i/length(data.types))
            data.type.index = (i-1) %% length(data.types) + 1
            
            one.df.sim = get.individual.sim.df(simset=all.simsets[[simset.index]],
                                              data.type=data.types[data.type.index],
                                              years=years.for.simset[[simset.index]],
                                              keep.dimensions=keep.dimensions,
                                              dimension.subsets=dimension.subsets,
                                              total.population = total.population)
            
            if (!is.null(one.df.sim))
            {
                one.df.sim$data.type = data.types[data.type.index]#DATA.TYPE.NAMES[data.types[data.type.index]]
                one.df.sim$intervention = names(all.simsets)[simset.index]
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
    df.truth$data.type = factor(DATA.TYPE.NAMES[df.truth$data.type],
                                levels=DATA.TYPE.NAMES[data.types])
    
    for (dimension in keep.dimensions[keep.dimensions!='year'])
    {
        df.sim[,dimension] = factor(PRETTY.NAMES[[dimension]][df.sim[,dimension]],
                                      levels=PRETTY.NAMES[[dimension]][dimension.subsets[[dimension]]])
        df.truth[,dimension] = factor(PRETTY.NAMES[[dimension]][df.truth[,dimension]],
                                      levels=PRETTY.NAMES[[dimension]][dimension.subsets[[dimension]]])
    }
    
    
    #-------------------#
    #-- SET UP SPLITS --#
    #-------------------#
    
    if (length(split.by)==0)
    {
        df.sim$split = 'All'
        df.truth$split = 'All'
    }
    else if (length(split.by)==1)
    {
        df.sim$split = df.sim[,split.by]
        df.truth$split = df.truth[,split.by]
    }
    else
    {
        df.sim$split = apply(df.sim[,split.by], 1, paste0, collapse=", ")
        df.truth$split = apply(df.truth[,split.by], 1, paste0, collapse=", ")
    }
        
    splits = unique(df.truth$split)
    split.shapes = rep(truth.shapes, ceiling(length(splits)/length(truth.shapes)))[1:length(splits)]
    names(split.shapes) = splits
    
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
        plot = plot + geom_point(data=df.truth, aes(x=year, y=value, shape=split), 
                                 fill=truth.color, size=truth.point.size)
    
    
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
    
    if (length(split.by)==0)
        plot = plot +
            scale_shape_manual(values=split.shapes, guide=F)
    else
    {
        split.legend.name = paste0(DIMENSION.NAMES[split.by], collapse=", ")
        plot = plot +
            scale_shape_manual(values=split.shapes, name=split.legend.name)
    }
    
    if (length(all.simsets)>1)
        plot = plot + scale_color_manual(values=simset.colors, name="Intervention")
    else
        plot = plot + scale_color_manual(values=simset.colors, guide=F)
    
    plot = plot + 
        theme(panel.grid=element_blank(), panel.background=element_blank(),
              axis.line.x.bottom = element_line(color = 'black'),
              axis.line.y.left = element_line(color = 'black'))
    
    #------------#
    #-- RETURN --#
    #------------#
    
    plot
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
                    paste0("'", names(DATA.TYPE.NAMES), "'")))
    
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
    eg = extract.new.diagnoses(simset@simulations[[1]],
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
        numerators = extract.new.diagnoses(sim,
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
        denominators = extract.population.subset(sim, years=years, keep.dimensions = 'year')
        
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
    eg = extract.prevalence(simset@simulations[[1]],
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
        numerators = extract.prevalence(sim,
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
        denominators = extract.population.subset(sim, years=years, keep.dimensions = 'year')
        
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
    eg = extract.overall.hiv.mortality(simset@simulations[[1]],
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
        numerators = extract.overall.hiv.mortality(sim,
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
        denominators = extract.population.subset(sim, years=years, keep.dimensions = 'year')
        
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
    eg = extract.diagnosed.hiv(simset@simulations[[1]],
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
    rv = sapply(simset@simulations, extract.diagnosed.hiv,
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

