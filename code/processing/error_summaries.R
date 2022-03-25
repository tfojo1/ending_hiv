
# Returns a list with three elements
#   each element is a list with one element for each outcome
# $pretty - th element for each outcome is  a 'pretty' formatted table of the form "<mae> (<mape>%)"
# $mae - the element for each outcome is a table of numeric mean absolute errors
# $mape - the element for each outcome is a table of numeric mean absolute percentage errors
# 
# Each table has one row for each location in 'locations'
# Plus a row for total if 'include.total' = T
# 
# One column for every combination of marginals
make.error.summary.table <- function(dir=file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets'),
                                     outcomes=c('new','prevalence'),
                                     years=2010:2018,
                                     locations=TARGET.MSAS,
                                     include.total=T,
                                     surv=msa.surveillance,
                                     by.total=T,
                                     by.age=T,
                                     by.race=T,
                                     by.sex=T,
                                     by.risk=T,
                                     ages=NULL,
                                     races=NULL,
                                     sexes=NULL,
                                     risks=NULL,
                                     na.rm=F,
                                     round.abs.digits=0,
                                     round.rel.digits=0,
                                     pretty.suffix='',
                                     verbose=T)
{
    if (!is.character(outcomes))
        stop("'outcome' must be a character vector")
    
    invalid.outcomes = setdiff(outcomes, ALLOWED.GENERLIZED.OUTCOMES)
    if (length(invalid.outcomes)>0)
        stop(paste0("The following outcome(s) are not valid: ",
                    paste0("'", invalid.outcomes, "'", collapse=', ')))
    
    # check arguments
    if (include.total)
    {
        non.aggregatable.outcomes = setdiff(outcomes, names(DATA.TYPE.VALUE.TYPE)[DATA.TYPE.VALUE.TYPE=='count'])
        if (length(non.aggregatable.outcomes)>0)
            stop(paste0("Cannot calculate total errors for outcome(s): ",
                        paste0("'", non.aggregatable.outcomes, "'", collapse=', '),
                        ". (Can only calculate totals for 'count' outcomes)"))
    }
    
    # Set up arguments for pulls
    keep.dimensions.per.category = list(
        total=c('year'),
        age=c('year','age'),
        race=c('year','race'),
        sex=c('year','sex'),
        risk=c('year','risk')
    )
    
    prefix.per.category = sapply(names(keep.dimensions.per.category), function(name){
        paste0(toupper(substr(name,1,1)), substr(name,2,nchar(name)))
    })
    names(prefix.per.category) = names(keep.dimensions.per.category)
    
    to.pull = c('total','age','race','sex','risk')[c(by.total, by.age, by.race, by.sex, by.risk)]
    
    rv = list(
        mae = list(),
        mape = list()
    )
    
    locations.without.total = locations
    if (include.total)
        locations = c(locations, 'total')
    
    # pull the sim and surveillance data and compare
    for (to.pull.index in 1:length(to.pull))
    {
        category = to.pull[to.pull.index]
        if (verbose)
        {
            if (category=='total')
                cat("Processing total errors...\n")
            else
                cat("Processing errors by ", category, "...\n", sep='')
        }
        
        keep.dimensions = keep.dimensions.per.category[[category]]

        #-- Pull the simulated values --#
        if (verbose)
            cat('Pulling simulation data...', sep='')
        
        # indexed [year, <demographic dimensions>, sim, outcome, location, intervention]
        sim.arr = generalized.extract.results(dir=dir,
                                              locations=locations.without.total,
                                              outcomes=outcomes, 
                                              years=years,
                                              keep.dimensions = keep.dimensions, 
                                              interventions = NULL,
                                              ages=ages,
                                              races=races,
                                              sexes=sexes,
                                              risks=risks,
                                              use.cdc.categorizations = T,
                                              verbose=verbose)
        
        dim.names = dimnames(sim.arr)
        dim.names = dim.names[setdiff(names(dim.names), c('intervention'))]
        dim(sim.arr) = sapply(dim.names, length)
        dimnames(sim.arr) = dim.names
        # now indexed [year, <demographic dimensions>, sim, outcome, location]

        if (verbose)
            cat('Done pulling simulation data\n')
        
        #-- Pull the truth (surveillance data) --#
        # indexed[year, <demographic dimensions>, outcome, location]
        
        if (verbose)
            cat('Pulling surveillance data for ', length(locations), ' location(s)...', sep='')
        truth.arr = generalized.get.surveillance.data(surv=surv,
                                                      locations=locations.without.total,
                                                      outcomes = outcomes,
                                                      years=years,
                                                      keep.dimensions = keep.dimensions,
                                                      ages=ages,
                                                      races=races,
                                                      sexes=sexes,
                                                      risks=risks,
                                                      verbose=verbose)
        if (verbose)
            cat('Done\n')
        
        if (include.total)
        {
            sim.arr = add.total.to.arr(sim.arr, na.rm=na.rm)
            truth.arr = add.total.to.arr(truth.arr, na.rm=na.rm)
        }
    
        #-- Redo the dimensions to collapse demographic dimensions into one category --#
        
        dim.names = dimnames(sim.arr)
        category.dimensions = prefix.per.category[category]
        demographic.dims = setdiff(names(dim.names), c('year', 'sim', 'outcome', 'location'))
        if (length(demographic.dims)>0)
        {
            category.value.names = get.pretty.dimension.names(dim.names[[ demographic.dims[length(demographic.dims)] ]])
            if (length(demographic.dims)>1)
            {
                for (i in ((length(demographic.dims)-1):1))
                {
                    to.add = dim.names[[ demographic.dims[i] ]]
                    category.value.names = rep(category.value.names, each=length(to.add))
                    category.value.names = paste0(to.add, ", ", category.value.names)
                }
            }
            
            category.dimensions = paste0(category.dimensions, ": ", category.value.names)
        }
        
        dim.names = c(
            dim.names['year'],
            list(category = category.dimensions),
            dim.names[c('sim','outcome','location')]
        )
        
        dim(sim.arr) = sapply(dim.names, length)
        dimnames(sim.arr) = dim.names
        # sim.arr is now indexed [year, category, sim, outcome, location]
        
        dim.names = dim.names[c('year','category','outcome','location')]
        dim(truth.arr) = sapply(dim.names, length)
        dimnames(truth.arr) = dim.names
        # truth.arr is now indexed [year, category, outcome, location]
        
        
        for (outcome in outcomes)
        {
            #-- Calculate the maes and mapes --#
            n.sim = dim(sim.arr)['sim']
            maes = sapply(category.dimensions, function(catg){
                sapply(locations, function(loc)
                {
                    calculate.mae(forecast=sim.arr[,catg,,outcome,loc], 
                                  truth=rep(truth.arr[,catg,outcome,loc], n.sim), 
                                  na.rm=na.rm)
                })
            })
            
            mapes = sapply(category.dimensions, function(catg){
                sapply(locations, function(loc)
                {
                    calculate.mape(forecast=sim.arr[,catg,,outcome,loc], 
                                  truth=rep(truth.arr[,catg,outcome,loc], n.sim), 
                                  na.rm=na.rm)
                })
            })
            
            outcome.dim.names = dim.names[c('location','category')]
            dim(maes) = dim(mapes) = sapply(outcome.dim.names, length)
            dimnames(maes) = dimnames(mapes) = outcome.dim.names
                  
            #-- Tack it on --#
            rv$mae[[outcome]] = cbind(rv$mae[[outcome]], maes)
            rv$mape[[outcome]] = cbind(rv$mape[[outcome]], mapes)
        }        
        
        if (verbose)
        {
            if (category=='total')
                cat("Done processing total errors...\n\n")
            else
                cat("Done processing errors by ", category, "...\n\n", sep='')
        }
        
    }
    
    #-- Make the pretty table --#
    rv$pretty = lapply(1:length(rv$mae), function(i){
        pretty.rv = paste0(
            format(round(rv$mae[[i]], round.abs.digits), big.mark=',', nsmall=round.abs.digits),
            pretty.suffix,
            " (",
            format(round(100*rv$mape[[i]], round.rel.digits), big.mark=',', nsmall=round.rel.digits),
            '%)'
        )
        dim(pretty.rv) = dim(rv$mae[[i]])
        dimnames(pretty.rv) = dimnames(rv$mae[[i]])
        
        pretty.rv
    })
    names(rv$pretty) = outcomes
    
    #-- Return --#
    rv
}


##-- LOW-LEVEL HELPERS --##

calculate.mape <- function(forecast, truth, na.rm=T)
{
    mean(abs((truth-forecast)/truth), na.rm=T)
}

calculate.mae <- function(forecast, truth, na.rm=T)
{
    mean(abs(truth-forecast), na.rm=T)
}

pretty.strat.names <- function(x)
{
    rv = paste0(toupper(substr(x,1,1)),
                sapply(x, function(z){substr(z, 2, nchar(z))}))
    
    rv[x=='msm'] = 'MSM'
    rv[x=='idu'] = "PWID"
    rv[x=='msm_idu'] = "MSM-PWID"
    
    rv
}

