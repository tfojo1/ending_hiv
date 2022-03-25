
# NEED TO DO
# 
# - fix split shapes
# - shape by geography


##-- LIBRARY CALLS --##
library(ggplot2)
library(data.table)
library(ggsci)
source('code/processing/outcome_formatting.R')
source('code/processing/generalized_extract_results.R')

source('code/processing/postprocessing.R')
source('code/core_code/setup/setup_jheem_from_components.R')
source('code/core_code/data_managers/census_totals.R')
source('code/core_code/data_managers/locale_mappings.R')
source('code/core_code/setup/interpolating.R')
source('code/core_code/data_managers/hiv_surveillance_manager.R')

##-- CONSTANTS --##

RED = '#C75D4D'
GREEN = '#458B00'
#GREEN = 'darkgreen'
BLUE = '#3278FA'
ORANGE = 'darkorange3'
MAROON = 'maroon3'

##-- LOAD DATA OBJECTS --##

if (!exists('msa.surveillance'))
    load('cached/msa.surveillance.Rdata')

if (!exists('state.surveillance'))
    load('cached/state.surveillance.Rdata')

if (!exists('county.surveillance'))
    load('cached/county.surveillance.Rdata')

if (!exists('CENSUS.TOTALS'))
    load('cached/census_totals.Rdata')

if (!exists('DEFAULT.LOCALE.MAPPING'))
    load('cached/DEFAULT.LOCALE.MAPPING.Rdata')

##--------------------##
##-- DEFAULT COLORS --##
##--------------------##

make.congruous.palette <- function(base.color, n,
                                   increment=-16)
{
    rgb.base = col2rgb(base.color)
    
    reds = pmin(255, pmax(0, rgb.base[1,1] + (1:n-1)*increment))
    greens = pmin(255, pmax(0, rgb.base[2,1] + (1:n-1)*increment))
    blues = pmin(255, pmax(0, rgb.base[3,1] + (1:n-1)*increment))
    
    rv = sapply(1:n, function(i){
        rgb(red=reds[i]/255,
            green=greens[i]/255,
            blue=blues[i]/255,
            alpha=1)
    })
    
    substr(rv, 1, 7)
}

DEFAULT.SIM.COLORS = pal_nejm()(8)[-1]
DEFAULT.TRUTH.COLORS = c('#357B00','#EF7C00','#C75D4D','grey30','maroon')

##--------------------------------##
##-- THE MAIN PLOTTING FUNCTION --##
##--------------------------------##

#'@param sims Either a JHEEM simulation object, a simset object, or a list containing them
simplot <- function(...,
                    data.types = c('new','prevalence'),
                    
                    # Settings
                    years=NULL,
                    
                    facet.by=NULL,
                    split.by=NULL,
                    
                    ages=NULL,
                    races=NULL,
                    sexes=NULL,
                    risks=NULL,#c('msm','idu','heterosexual'),
                    
                    # Data Managers
                    surv1 = msa.surveillance,
                    surv2 = state.surveillance,
                    surv3 = county.surveillance,
                    location = NULL,
                    location.1to2.mapping = states.for.msa,
                    location.1to3.mapping = counties.for.msa,
                    use.surv2.for.data.types = c('suppression','engagement','suppression.of.engaged','diagnosed','retention','testing','testing.rate','testing.period'),
                    use.surv3.for.data.types=use.surv2.for.data.types,
                    
                    use.state.data = T,
                    use.county.data = T,
                    
                    # Style parameters
                    plot.individual.simset.sims = F,
                    
                    ribbon.alpha=0.25,
                    single.sim.line.size=2,
                    single.simset.line.size=2,
                    multiple.simset.line.size=0.1,
                    multiple.simset.line.alpha=0.1,
                    
                    truth.point.size = 4,
                    truth.line.size=0.5,
                    line.for.truth='auto',
                    
                    truth.shapes = c(21,23,22,24,25),
                    shape.truth.by = c('auto','geography','split','location')[1],
                    
                    color.sims.by = c('auto','split','sim')[1],
                    sim.colors=DEFAULT.SIM.COLORS,
                    
                    color.truth.by = c('auto','location','split')[1],
                    truth.colors=DEFAULT.TRUTH.COLORS,

                    sim.linetype.by = c('auto','none','split','sim')[1],
                    sim.line.types = c('solid','dashed','dotted','twodash','longdash'),
                    
                    # Misc Low-level Settings
                    aggregate.statistic = 'mean',
                    ci.coverage=0.95,
                    year.anchor='mid'
)
{
    
##-- PROCESS SIMS --##
    
    args = list(...)
    
    sims = list()
    sim.names = character()
    counter = 1
    for (i in 1:length(args))
    {
        elem = args[[i]]
        if (is(elem,'list'))
        {
            if (any(sapply(elem, function(sub){
                !is(sub, 'simset') && !is(sub, 'jheem.results')
            })))
                stop("... must contain only objects of class 'jheem.results' or 'simset', or lists with only 'jheem.result' or 'simset' objects.")
            
            to.add = elem
            if (!is.null(names(elem)))
            {
                to.add.names = sapply(1:length(to.add), function(j){
                    if (names(elem)[j]=='')
                    {
                        if (is(elem, 'simset'))
                            to.add.names = paste0('Simset ', counter-1+j)
                        else
                            to.add.names = paste0('Sim ', counter-1+j)
                    }
                    else
                        names(elem)[j]
                })
            }
            else if (!is.null(names(args)) && names(args)[i] != '')
            {
                to.add.names = paste0(names(args)[j], " ", 1:length(to.add))
            }
            else
            {
                to.add.names = paste0(sapply(to.add, function(one.to.add){
                    if (is(to.add, 'simset'))
                        "Simset"
                    else
                        "Sim"
                }), " ", counter-1+1:length(to.add))
            }
        }
        else if (!is(elem, 'simset') && !is(elem, 'jheem.results'))
            stop("... must contain only objects of class 'jheem.results' or 'simset', or lists with only 'jheem.result' or 'simset' objects.")
        else
        {
            to.add = list(elem)
            if (is.null(names(args)) || names(args)[[i]]=='')
            {
                if (is(elem, 'simset'))
                    to.add.names = paste0('Simset ', counter)
                else
                    to.add.names = paste0('Sim ', counter)
            }
        }
        
        sims = c(sims, to.add)
        sim.names = c(sim.names, to.add.names)
        
        counter = counter + length(to.add)
    }
    
    is.individual.sim = sapply(sims, is, class2='jheem.results')

    sims = lapply(sims, function(elem){
        if (is(elem, 'simset'))
            elem
        else if (is(elem, 'jheem.results'))
            new('dummy.simset', simulations=list(elem))
        else
            stop("sims must contain only objects of class 'jheem.results' or 'simset' ")
    })
    
##-- PROCESS OTHER ARGUMENTS --##
    
    #-- location --#
    if (is.null(location))
        location = attr(sims[[1]]@simulations[[1]], 'location')
    
    #-- years --#
    if (is.null(years))
    {
        max.year = max(sapply(sims, function(ss){
            max(ss@simulations[[1]]$years)
        }))
        
        years = 2010:max.year
    }
    
    years.for.sim = lapply(sims, function(ss){
        intersect(years, ss@simulations[[1]]$years)
    })
    
    non.empty.sims = sapply(years.for.sim, length) > 0
    
    #-- dimensions --#
    keep.dimensions = unique(c(
        'year',
        facet.by,
        split.by
    ))
    non.year.keep.dimensions = setdiff(keep.dimensions, 'year')
    
    if (is.null(ages))
        ages = sims[[1]]@simulations[[1]]$ages
    if (is.null(races))
        races = sims[[1]]@simulations[[1]]$races
    if (is.null(sexes))
        sexes = c('female','male')#sims[[1]]@simulations[[1]]$sexes
    if (is.null(risks))
        risks = c('msm','idu','msm_idu','heterosexual')#sims[[1]]@simulations[[1]]$risks
    
    dimension.subsets=list(age=ages,
                           race=races,
                           sex=sexes,
                           risk=risks)
    
    #-- total population --#
    total.population.per.simset = lapply(1:length(sims), function(i){
        if (non.empty.sims[i])
        {
            rv = sapply(sims[[i]]@simulations, get.total.population, 
                    census.totals=CENSUS.TOTALS, 
                    years=as.character(years.for.sim[[i]]))
        
            dim(rv) = c(length(years.for.sim[[i]]), sims[[i]]@n.sim)
            dimnames(rv) = list(year=as.character(years.for.sim[[i]]),
                                sim=NULL)
            rv
        }
        else
            NULL
    })
    
##-- SET UP THE SIM DATA FRAME --##
    
    n.sim.dfs = length(sims) * length(data.types)
    
    df.sim.subs = lapply(1:n.sim.dfs, function(i){
        sim.index = ceiling(i/length(data.types))
        data.type.index = (i-1) %% length(data.types) + 1
        
        if (non.empty.sims[sim.index])
            one.dfs.sim = get.sim.dfs(simset=sims[[sim.index]],
                                      data.type=data.types[data.type.index],
                                      years=years.for.sim[[sim.index]],
                                      keep.dimensions=keep.dimensions,
                                      dimension.subsets=dimension.subsets,
                                      total.population = total.population.per.simset[[sim.index]],
                                      get.individual.sims = plot.individual.simset.sims,
                                      aggregate.statistic = aggregate.statistic,
                                      ci.coverage=ci.coverage,
                                      year.anchor=year.anchor)
        else
            one.dfs.sim = NULL
        
        if (is.null(one.dfs.sim))
            NULL
        else
        {
            one.dfs.sim$data.type = data.types[data.type.index]
            
            if (plot.individual.simset.sims)
            {
                names(one.dfs.sim)[names(one.dfs.sim)=='simulation'] = 'sim.number'
                one.dfs.sim$lower = one.dfs.sim$upper = NA
            }
            else if (is.individual.sim[sim.index])
                one.dfs.sim$lower = one.dfs.sim$upper = NA
            
            one.dfs.sim$is.simset = !is.individual.sim[sim.index]
            one.dfs.sim$sim.name = sim.names[sim.index]
            
            one.dfs.sim
        }
    })
    
    df.sim = as.data.frame(rbindlist(df.sim.subs))
    df.sim$data.type = DATA.TYPE.NAMES[df.sim$data.type]
    for (catg in non.year.keep.dimensions)
        df.sim[,catg] = pretty.format.strata.values(df.sim[,catg])
    
    if (any(keep.dimensions=='risk') && any(keep.dimensions=='sex'))
        df.sim = df.sim[df.sim$sex != 'female' | (df.sim$risk != 'msm' & df.sim$risk != 'msm_idu'),]
    
    #-- Set up some indexing values --#
    if (plot.individual.simset.sims)
        df.sim$sim = paste0(df.sim$sim.name, df.sim$sim.number)
    
    
##-- SET UP THE TRUTH DATA FRAME --##

    locations2 = location.1to2.mapping(location)
    locations3 = location.1to3.mapping(location)
    all.locations = list(location,
                         locations2,
                         locations3)
    
    survs.for.data.type = lapply(data.types, function(data.type){
        (1:3)[c(T,
                use.state.data && !is.null(surv2) && any(data.type==use.surv2.for.data.types),
                use.county.data && !is.null(surv3) && any(data.type==use.surv3.for.data.types))]
    })
    
    iterated.data.types = unlist(sapply(1:length(data.types), function(i){
        rep(data.types[i], length(survs.for.data.type[[i]]))
    }))
    
    iterated.survs.for.data.type = unlist(survs.for.data.type)
    
    iterated.data.types.for.truth = unlist(sapply(1:length(iterated.data.types), function(i){
        surv.index = iterated.survs.for.data.type[[i]]
        rep(iterated.data.types[i], length(all.locations[[surv.index]]))
    }))
    iterated.survs.for.truth = unlist(sapply(1:length(iterated.data.types), function(i){
        surv.index = iterated.survs.for.data.type[[i]]
        rep(surv.index, length(all.locations[[surv.index]]))
    }))
    iterated.locations.for.truth = unlist(sapply(1:length(iterated.data.types), function(i){
        surv.index = iterated.survs.for.data.type[[i]]
        all.locations[[surv.index]]
    }))
    
    survs = list(surv1, surv2, surv3)
    
    truth.sub.dfs = lapply(1:length(iterated.data.types.for.truth), function(i){
        
        one.df.truth = get.truth.df(location=iterated.locations.for.truth[i],
                                    surv = survs[[iterated.survs.for.truth[i] ]],
                                    data.type=iterated.data.types.for.truth[i],
                                    years=years,
                                    keep.dimensions=keep.dimensions,
                                    dimension.subsets = dimension.subsets,
                                    pull.from.state=F)
      
        if (!is.null(one.df.truth))
        {
            one.df.truth$data.type = iterated.data.types.for.truth[i]
            one.df.truth$location = iterated.locations.for.truth[i]
            one.df.truth$surv.category = iterated.survs.for.truth[i]
        }
        
        one.df.truth
    })
    
    if (length(truth.sub.dfs)==1)
        df.truth = truth.sub.dfs[[1]]
    else
        df.truth = as.data.frame(rbindlist(truth.sub.dfs))
    
    if (!is.null(df.truth) && dim(df.truth)[1]==0)
        df.truth = NULL
    
    if (!is.null(df.truth))
    {
        df.truth$data.type = DATA.TYPE.NAMES[df.truth$data.type]
        for (catg in non.year.keep.dimensions)
            df.truth[,catg] = pretty.format.strata.values(df.truth[,catg])
        
        if (any(keep.dimensions=='risk') && any(keep.dimensions=='sex'))
            df.truth = df.truth[df.truth$sex != 'female' | (df.truth$risk != 'msm' & df.truth$risk != 'msm_idu'),]
        
        df.truth$location.name = get.location.name(df.truth$location)
    }
    
##-- SET UP SPLITS, GROUPS, and CATEGORIES --##

    if (!is.null(df.sim))
    {
        if (length(split.by)==0)
            df.sim$split = 'all'
        else if (length(split.by)==1)
            df.sim$split = df.sim[,split.by]
        else
            df.sim$split = apply(df.sim[,split.by], 1, paste0, collapse=', ')
        
        if (length(split.by)==0)
            df.sim$category = df.sim$sim.name
        else
            df.sim$category = paste0(df.sim$sim.name, ": ", df.sim$split)
        
        
        if (plot.individual.simset.sims)
            df.sim$group = paste0(df.sim$category, '_', df.sim$sim.number)
        else
            df.sim$group = paste0(df.sim$category)
        
    }
    
    if (!is.null(df.truth))
    {
        if (length(split.by)==0)
            df.truth$split = 'all'
        else if (length(split.by)==1)
            df.truth$split = df.truth[,split.by]
        else
            df.truth$split = apply(df.truth[,split.by], 1, paste0, collapse=', ')
        
        
        if (length(split.by)==0)
            df.truth$category = df.truth$location.name
        else
            df.truth$category = paste0(df.truth$location.name, ": ", df.truth$split)
    }
    
##-- SET UP SCALES --##
    
    if (!is.null(df.sim))
    {
        #-- Map categories to scales --#
        if (plot.individual.simset.sims)
            unique.df.sim = df.sim[df.sim$sim.number==1,]
        else
            unique.df.sim = df.sim
        
        sim.categories = unique(unique.df.sim$category)
        
        unique.df.sim = unique.df.sim[sapply(sim.categories, function(catg){
           (1:dim(unique.df.sim)[1])[unique.df.sim$category==catg][1] 
        }),]
        
        sim.catg.to.name = sapply(sim.categories, function(catg){
            unique.df.sim$sim.name[unique.df.sim$category==catg][1]
        })
        sim.catg.to.split = sapply(sim.categories, function(catg){
            unique.df.sim$split[unique.df.sim$category==catg][1]
        })
        sim.catg.to.is.simset = sapply(sim.categories, function(catg){
            unique.df.sim$is.simset[unique.df.sim$category==catg][1]
        })
        names(sim.catg.to.name) = names(sim.catg.to.split) = names(sim.catg.to.is.simset) = sim.categories 
        
        #-- Set up colors --#
        if (color.sims.by=='auto')
        {
            if (length(sims)>1 || length(split.by)==0)
                color.sims.by='sim'
            else
                color.sims.by='split'
        }
        
        if (color.sims.by=='split')
            unique.sim.color.by = unique(sim.catg.to.split)
        else
            unique.sim.color.by = unique(sim.catg.to.name)
        
        sim.colors = rep(sim.colors, ceiling(length(unique.sim.color.by)/length(sim.colors)))[1:length(unique.sim.color.by)]
        names(sim.colors) = unique.sim.color.by
        sim.base.colors = sim.colors
    
        if (color.sims.by=='split')
            sim.colors = sim.colors[sim.catg.to.split]
        else
            sim.colors = sim.colors[sim.catg.to.name]
        names(sim.colors) = sim.categories
        
        
        #-- Set up Line Types --#
        if (sim.linetype.by=='auto')
        {
            if (length(sims)<=1 || length(split.by)==0)
                sim.linetype.by = 'none'
            else if (color.sims.by=='split')
                sim.linetype.by = 'sim'
            else
                sim.linetype.by = 'split'
        }
        
        if (sim.linetype.by=='none')
        {
            sim.line.types = rep('solid',length(sim.categories))
        }
        else
        {
            if (sim.linetype.by=='split')
                unique.sim.linetype.by = unique(sim.catg.to.split)
            else
                unique.sim.linetype.by = unique(sim.catg.to.name)
            
            
            sim.line.types = rep(sim.line.types, ceiling(length(unique.sim.linetype.by)/length(sim.line.types)))[1:length(unique.sim.linetype.by)]
            names(sim.line.types) = unique.sim.linetype.by
            sim.base.line.types = sim.line.types
            
            if (sim.linetype.by=='split')
                sim.line.types = sim.line.types[sim.catg.to.split]
            else
                sim.line.types = sim.line.types[sim.catg.to.name]
        }
        names(sim.line.types) = sim.categories
        
        #do we need to overwrite the individual simset lines as solid?
        
        #-- Set up Line Sizes --#
        sim.line.sizes = rep(single.sim.line.size, length(sim.categories))
        if (plot.individual.simset.sims)
            sim.line.sizes[sim.catg.to.is.simset] = multiple.simset.line.size
        else
            sim.line.sizes[sim.catg.to.is.simset] = single.simset.line.size
        names(sim.line.sizes) = sim.categories
        
        #-- Set up Shapes --#
        sim.shapes = rep(32, length(sim.categories))
        names(sim.shapes) = sim.categories
        
        #-- Set up Alphas --#
        sim.alphas = rep(1, length(sim.categories))
        if (plot.individual.simset.sims)
            sim.alphas[sim.catg.to.is.simset] = multiple.simset.line.alpha
        names(sim.alphas) = sim.categories
    }
    else
    {
        sim.colors = character()
        sim.line.types = character()
        sim.line.sizes = numeric()
        sim.shapes = numeric()
        sim.alphas = numeric()
    }
    
    if (!is.null(df.truth))
    {
        if (line.for.truth=='auto')
            line.for.truth = length(unique(df.truth$location))>1
        
        #-- Map categories to scales --#
        truth.categories = unique(df.truth$category)
        unique.df.truth = df.truth[sapply(truth.categories, function(catg){
            (1:dim(df.truth)[1])[df.truth$category==catg][1] 
        }),]
        
        truth.catg.to.location = sapply(truth.categories, function(catg){
            unique.df.truth$location[unique.df.truth$category==catg][1]
        })
        truth.catg.to.split = sapply(truth.categories, function(catg){
            unique.df.truth$split[unique.df.truth$category==catg][1]
        })
        truth.catg.to.surv.category = sapply(truth.categories, function(catg){
            unique.df.truth$surv.category[unique.df.truth$category==catg][1]
        })
        names(truth.catg.to.location) = names(truth.catg.to.split) = names(truth.catg.to.surv.category) = truth.categories 
        
        #-- Set up Shapes --#
        
        if (shape.truth.by=='auto')
        {
            #  if (length(facet.by)==0)
            #      facet = df.truth$data.type
            #  else
            #      facet = apply(df.truth[,c('data.type', facet.by)], 1, paste0, collapse='_')
            
            #  max.locations.per.facet = max(sapply(unique(facet), function(ff){
            #      length(unique(df.truth$location[facet==ff]))
            #  }))
            
            if (length(split.by)>0 && color.truth.by != 'split' && 
                (is.null(df.sim) || color.sims.by != 'split'))# && max.locations.per.facet<=1)
                shape.truth.by = 'split'
            else
                shape.truth.by = 'geography'
        }    
        
        if (shape.truth.by=='split')
           unique.shape.truth.by = unique(df.truth$split)
        else if (shape.truth.by=='location')
            unique.shape.truth.by = unique(df.truth$location)
        else
            unique.shape.truth.by = as.character(unique(df.truth$surv.category))
        
        truth.shapes = rep(truth.shapes, ceiling(length(unique.shape.truth.by)/length(truth.shapes)))[1:length(unique.shape.truth.by)]
        names(truth.shapes) = unique.shape.truth.by
        
        if (shape.truth.by=='split')
            truth.shapes = truth.shapes[truth.catg.to.split]
        else if (shape.truth.by=='location')
            truth.shapes = truth.shapes[truth.catg.to.location]
        else
            truth.shapes = truth.shapes[as.character(truth.catg.to.surv.category)]
        names(truth.shapes) = truth.categories
        
        
        #-- Set up Color --#
        
        if (color.truth.by=='auto')
        {
            if (shape.truth.by=='split' || length(split.by)==0)
                color.truth.by = 'location'
            else
                color.truth.by = 'split'
        }
        
        if (color.truth.by=='split')
            truth.color.by = df.truth$split
        else
            truth.color.by = df.truth$surv.category
        
        if (color.truth.by=='split' && color.sims.by=='split' && !is.null(df.sim))
        {
            truth.base.colors = sim.base.colors
            unique.truth.color.by = names(truth.base.colors)
        }
        else
        {
            unique.truth.color.by = unique(truth.color.by)
            truth.base.colors = rep(truth.colors, ceiling(length(unique.truth.color.by)/length(truth.colors)))[1:length(unique.truth.color.by)]
            names(truth.base.colors) = unique.truth.color.by
        }
        
        locations.for.truth.color.level = lapply(unique.truth.color.by, function(level){
            unique(df.truth$location[truth.color.by==level])
        })
        names(locations.for.truth.color.level) = as.character(unique.truth.color.by)
        truth.expanded.colors = lapply(1:length(unique.truth.color.by), function(i){
            locs = locations.for.truth.color.level[[i]]
            colors = make.congruous.palette(truth.base.colors[i], length(locs), increment = 24)
            names(colors) = locs
            colors
        })
        names(truth.expanded.colors) = as.character(unique.truth.color.by)
        
        truth.colors = sapply(1:length(truth.categories), function(i){
            if (color.truth.by=='split')
                truth.expanded.colors[[truth.catg.to.split[i] ]][truth.catg.to.location[i] ]
            else
                truth.expanded.colors[[as.character(truth.catg.to.surv.category[i]) ]][truth.catg.to.location[i] ]
        })
        
        names(truth.colors) = truth.categories
        
        
        #-- Set up Line Types --#
        if (!line.for.truth)
        {
            truth.line.types = rep('blank', length(truth.categories))
        }
        else
        {
            if (!is.null(df.sim) && sim.linetype.by=='split')
                truth.line.types = sim.base.line.types[truth.catg.to.split]
            else
                truth.line.types = rep('solid', length(truth.categories))
        }
        names(truth.line.types) = truth.categories
        
        #-- Set up Line Sizes --#
        truth.line.sizes = rep(truth.line.size, length(truth.categories))
        names(truth.line.sizes) = truth.categories
        
        #-- Set up Alphas --#
        truth.alphas = rep(1, length(truth.categories))
        names(truth.alphas) = truth.categories
     
    }
    else
    {
        truth.colors = character()
        truth.line.types = character()
        truth.line.sizes = numeric()
        truth.shapes = numeric()
        truth.alphas = numeric()
    }
    
    #-- Put them together --#
    colors = c(sim.colors, truth.colors)
    line.types = c(sim.line.types, truth.line.types)
    line.sizes = c(sim.line.sizes, truth.line.sizes)
    shapes = c(sim.shapes, truth.shapes)
    alphas = c(sim.alphas, truth.alphas)
    
    
##-- MAKE THE PLOT --##
   
    rv = ggplot()
    
    
    # Sim
    
    if (plot.individual.simset.sims)
        individual.mask = !df.sim$is.simset
    else
        individual.mask = T
    
    if (any(!is.na(df.sim$lower) | !is.na(df.sim$upper)))
        rv = rv + 
            geom_ribbon(data=df.sim[individual.mask & !is.na(df.sim$lower),], 
                        aes(x=year, ymin=lower, ymax=upper, fill=category, group=group),
                        alpha=ribbon.alpha)
    
    rv = rv +
        geom_line(data=df.sim[!individual.mask,], 
                  aes(x=year, y=value, color=category,
                      size=category, alpha=category,
                      group=group)) + 
        geom_line(data=df.sim[individual.mask,], 
                  aes(x=year, y=value, color=category,
                      size=category, alpha=category)) 
    
    
    # Truth
    
    if (!is.null(df.truth))
    {
        if (line.for.truth)
            rv = rv + geom_line(data=df.truth,
                                 aes(x=year, y=value, color=category, size=category))
        rv = rv + geom_point(data=df.truth,
                             aes(x=year, y=value, fill=category, shape=category),
                             size=truth.point.size)
        
    }
    
    
    #-- Scales --#
    
    LEGEND.NAME = 'Legend'
    
    # Shape
    rv = rv + scale_shape_manual(values=shapes, name=LEGEND.NAME)
    
    # Color
    rv = rv + 
        scale_fill_manual(values=colors, name=LEGEND.NAME) +
        scale_color_manual(values=colors, name=LEGEND.NAME)
    
    # Line Size
    rv = rv + scale_size_manual(values=line.sizes, name=LEGEND.NAME)
    
    # Line Type
    rv = rv + scale_linetype_manual(values=line.types, name=LEGEND.NAME)
    
    # Alpha
    rv = rv + scale_alpha_manual(values=alphas, guide='none')
    
    #-- The Facet Wrap --#
    facet.elements = c('data.type', facet.by)
    facet.formula = as.formula(paste0('~', paste0(facet.elements, collapse='+')))
    rv = rv + facet_wrap(facet.formula, scales='free_y')    

    
    #-- Axis Titles and Labels --#
    format.number.or.percent = function(x){
        if (all(is.na(x) | 
                (x>=0 & x<=1)))
        {
            na.mask = is.na(x)
            rv = paste0(100*x, '%')
            rv[na.mask] = NA
            rv
        }
        else
            format(x, big.mark=',')
    }
    
    rv = rv + expand_limits(y=1) +
        xlab(NULL) +
        scale_y_continuous(name=NULL, limits = c(0,NA),
                           labels = format.number.or.percent)
    
    #-- RETURN --#
    
    rv
}

##-------------##
##-- HELPERS --##
##-------------##

pretty.format.strata.values <- function(vals)
{
    rv = paste0(toupper(substr(as.character(vals),1,1)),
                substr(as.character(vals),2,nchar(as.character(vals))))
    rv[vals=='msm'] = 'MSM'
    rv[vals=='msm_idu'] = 'MSM/PWID'
    rv[vals=='idu'] = 'PWID'
    
    rv
}


test.palette <- function(pal, n.colors = NA,
                         colors=NULL)
{
  if (is.null(colors))
  {
    if (is.na(n.colors))
    {
      n.colors = 2
      done = F
      while (!done)
      {
        print(paste0("Trying ", n.colors, " colors"))
        colors = suppressWarnings(pal(n.colors))
        if (any(is.na(colors)))
        {
          done = T
          n.colors = n.colors-1
        }
        else 
          n.colors = n.colors + 1
      }
    }
    colors = pal(n.colors)
  }
  
  df = data.frame(
    x = 1:length(colors),
    y = 10 + 1:length(colors),
    group = colors
  )
  
  names(colors)=colors
  ggplot(df) + geom_bar(aes(x, y, fill=group), stat='identity') + 
    scale_fill_manual(values=colors)
}

##------------------##
##-- HELPER CLASS --##
##------------------##

setClass('dummy.simset',
         representation=list(
             simulations='list',
             weights='numeric',
             n.sim='integer')
)

setMethod('initialize',
          signature(.Object='dummy.simset'),
def=function(.Object, simulations){
    
    if (!is.list(simulations))
        simulations = list(simulations)
    
    .Object@simulations = simulations
    .Object@weights = rep(1, length(simulations))
    .Object@n.sim = length(simulations)
    
    .Object
})


