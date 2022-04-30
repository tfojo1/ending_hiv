
# depends on code/processing/outcome_formatting.R
ALLOWED.GENERLIZED.OUTCOMES = names(DATA.TYPE.NAMES)

##-------------------------------------##
##-------------------------------------##
##-- THE MAIN FRONT-FACING FUNCTIONS --##
##-------------------------------------##
##-------------------------------------##


##-----------------------------------------------------------------------##
##-- (1) EXTRACT RESULTS FROM MULTIPLE LOCATIONS/SIMSETS/INTERVENTIONS --##
##-----------------------------------------------------------------------##

# Returns an array indexed
# [<keep.dimensions>, sim, outcome, location, intervention]
generalized.extract.results <- function(dir,
                                        locations,
                                        interventions,
                                        outcomes,
                                        years,
                                        keep.dimensions='year',
                                        use.cdc.categorizations=F,
                                        ages=c('13-24 years','25-34 years','35-44 years','45-54 years','55+ years'),
                                        races=c('black','hispanic','other'),
                                        sexes=if (use.cdc.categorizations) c('male','female') else c('heterosexual_male','msm','female'),
                                        risks=if (use.cdc.categorizations) c('msm','idu','msm_idu','heterosexual') else c('never_IDU','active_IDU','IDU_in_remission'),
                                        n.sim=NULL,
                                        throw.error.if.missing.sims=T,
                                        census.totals=ALL.DATA.MANAGERS$census.totals,
                                        year.anchor='mid',
                                        verbose=T,
                                        verbose.prefix='')
{
    # Check outcomes
    invalid.outcomes.mask = sapply(outcomes, function(outcome){
        all(outcome != ALLOWED.GENERLIZED.OUTCOMES)
    })
    if (any(invalid.outcomes.mask))
        stop("The following outcomes are not valid for generalized.extract.results: ",
             paste0("'", outcomes[invalid.outcomes.mask], "'", collapse=", "))
    
    # Set up dimensions
    inner.dim.names = list(
        year=as.character(years),
        age=ages,
        race=races,
        sex=sexes,
        risk=risks
    )
    
    invalid.keep.dimensions.mask = sapply(keep.dimensions, function(dim){
        all(dim != names(inner.dim.names))
    })
    if (any(invalid.keep.dimensions.mask))
        stop("The following are not valid keep dimensions: ",
             paste0("'", keep.dimensions[invalid.keep.dimensions.mask], "'", collapse=', '))
    
    inner.dim.names = inner.dim.names[keep.dimensions]
    n.inner = prod(sapply(inner.dim.names, length))
    
    # Set up dimension subsets
    dimension.subsets=list(age=ages,
                           race=races,
                           sex=sexes,
                           risk=risks)
    
    # set up intervention names
    if (is.null(interventions))
        intervention.codes = NA
    else if (is(interventions, 'character'))
        intervention.codes = interventions
    else
        intervention.codes = sapply(interventions, get.intervention.code)
    
    # pull the data
    if (verbose)
        cat(verbose.prefix, "Preparing to process results for ", length(intervention.codes), 
                     " intervention(s) and ", length(locations), " location(s)\n", sep='')
    
    rv = sapply(1:length(intervention.codes), function(int.index){
        int.code = intervention.codes[[int.index]]
        if (is.na(int.code))
            int.name = 'baseline'
        else
            int.name = paste0("intervent '", int.code, "'")
        if (verbose)
            cat(verbose.prefix, "- Processing ", length(locations), " location(s) for ", int.name,
            " (", int.index, " of ", length(intervention.codes), ")\n", sep='')
        
        sapply(1:length(locations), function(loc.index){
            loc = locations[loc.index]
            # Load the simset
            if (is.na(int.code))
            {
                filename = get.full.filename(location=loc)
                file = file.path(dir, filename)
            }
            else
            {
                filename = get.simset.filename(location=loc,
                                               intervention.code=int.code)
                file = file.path(dir, loc, filename)
            }
            
            if (file.exists(file))
            {
                if (verbose)
                    cat(verbose.prefix, "  - Loading simset for location '",
                                 loc, "' (", loc.index, " of ", length(locations), ")...", sep='')
                
                load(file)
                if (verbose)
                    cat("Done\n")
                
                simset = flatten.simset(simset)
                if (is.null(n.sim))
                    n.sim <<- simset@n.sim
                
                if (n.sim != simset@n.sim)
                    stop(paste0("Inconsistent number of simulations in simset: location '", 
                         loc, "' with intervention '", int.code, " ('", file, "') has ",
                         simset@n.sim, " simulations, instead of the previously specific ", n.sim))
            
                if (verbose)
                    cat(verbose.prefix, "    Extracting ", length(outcomes), " outcome(s)...", sep='')
                

                total.population = get.total.population.for.simset(simset, 
                                                                   years=years,
                                                                   census.totals=census.totals)
                                
                # pull the data
                rv = sapply(1:length(outcomes), function(outcome.index){
                    
                    outcome = outcomes[outcome.index]
           #         if (verbose)
            #            print(paste0("    - Extracting '", outcome, "' (",
             #                        outcome.index, " of ", length(outcomes), ")"))
                    
                    sub.rv = get.arr.for.data.type(data.type = outcome,
                                          simset=simset,
                                          years=years,
                                          keep.dimensions=keep.dimensions,
                                          dimension.subsets=dimension.subsets,
                                          total.population=total.population,
                                          year.anchor=year.anchor,
                                          use.cdc.categorizations=use.cdc.categorizations)
                    
                    
                    if (any(sapply(inner.dim.names, is.null)))
                    {
                        if (is.null(ages) && any(keep.dimensions=='age'))
                            ages <<- dimnames(sub.rv)$age
                        if (is.null(races) && any(keep.dimensions=='race'))
                            races <<- dimnames(sub.rv)$race
                        if (is.null(sexes) && any(keep.dimensions=='sex'))
                            sexes <<- dimnames(sub.rv)$sex
                        if (is.null(risks) && any(keep.dimensions=='risk'))
                            risks <<- dimnames(sub.rv)$risk
                    
                        inner.dim.names <<- list(
                            year=as.character(years),
                            age=ages,
                            race=races,
                            sex=sexes,
                            risk=risks
                        )[keep.dimensions]
                        
                        n.inner <<- prod(sapply(inner.dim.names, length))
                    }                    
                    sub.rv
                })
                
                if (verbose)
                    cat('Done\n')
                rv
            }
            else if (throw.error.if.missing.sims)
            {
                stop("There is no simset for location '", loc, "' and intervention '",
                     int.code, " ('", file, "')")
            }
            else if (is.null(n.sim))
            {
                stop("There is no simset for location '", loc, "' and intervention '",
                     int.code, " ('", file, "'). This is the first location and intervention, so cannot infer the number of simulations")
            }
            else if (any(sapply(inner.dim.names, is.null)))
            {
                stop("There is no simset for location '", loc, "' and intervention '",
                     int.code, " ('", file, "'). This is the first location and intervention, so cannot infer the values of NULL dimensions")
            }
            else
                rep(NaN, n.sim * n.inner * length(outcomes))
        })
    })
    
    #[<keep.dimensions>, sim, outcome, location, intervention]
    dim.names = c(inner.dim.names,
                  list(sim=as.character(1:n.sim),
                       outcome=outcomes,
                       location=locations,
                       intervention=intervention.codes))
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}

##-----------------------------------------------------------##
##-- (1) EXTRACT SURVEILLANCE DATA FROM MULTIPLE LOCATIONS --##
##-----------------------------------------------------------##

# Returns an array indexed
# [year, <demographic dimensions>, outcome, location]
# The returned demographic dimensions will be in the order: age, race, sex, risk
generalized.get.surveillance.data <- function(surv,
                                              locations,
                                              outcomes,
                                              years=surv$DIMENSION.VALUES$year,
                                              keep.dimensions='year',
                                              ages=surv$DIMENSION.VALUES$age,
                                              races=surv$DIMENSION.VALUES$race,
                                              sexes=surv$DIMENSION.VALUES$sex,
                                              risks=surv$DIMENSION.VALUES$risk,
                                              throw.error.if.missing.data=T,
                                              verbose=T,
                                              verbose.prefix='',
                                              na.rm=F)
{
#-- Check arguments --#
    
    invalid.outcomes = setdiff(outcomes, setdiff(ALLOWED.DATA.TYPES, 'population'))
    if (length(invalid.outcomes)>0)
        stop(paste0("The following outcomes are not valid: ",
                    paste0("'", invalid.outcomes, "'", collapse=', ')))
    
    if (length(setdiff(ages, surv$DIMENSION.VALUES$age))>0)
        stop(paste0("The following are not valid age strata: ",
                    paste0("'", setdiff(ages, surv$DIMENSION.VALUES$age), "'", collapse=', ')))
    if (length(setdiff(races, surv$DIMENSION.VALUES$race))>0)
        stop(paste0("The following are not valid race strata: ",
                    paste0("'", setdiff(races, surv$DIMENSION.VALUES$race), "'", collapse=', ')))
    if (length(setdiff(sexes, surv$DIMENSION.VALUES$sex))>0)
        stop(paste0("The following are not valid sex strata: ",
                    paste0("'", setdiff(sexes, surv$DIMENSION.VALUES$sex), "'", collapse=', ')))
    if (length(setdiff(risks, surv$DIMENSION.VALUES$risk))>0)
        stop(paste0("The following are not valid risk strata: ",
                    paste0("'", setdiff(risks, surv$DIMENSION.VALUES$risk), "'", collapse=', ')))
    
    allowed.keep.dimensions = c('year','age','race','sex','risk')
    if (length(setdiff(keep.dimensions, allowed.keep.dimensions))>0)
        stop(paste0("The following are not valid values for keep dimensions: ",
                    paste0("'", setdiff(keep.dimensions, allowed.keep.dimensions), "'", collapse=', ')))
    keep.dimensions = intersect(allowed.keep.dimensions, keep.dimensions)
    
    
    if (is.null(ages))
        ages=surv$DIMENSION.VALUES$age
    if (is.null(races))
        races=surv$DIMENSION.VALUES$race
    if (is.null(sexes))
        sexes=surv$DIMENSION.VALUES$sex
    if (is.null(risks))
        risks=surv$DIMENSION.VALUES$risk
    
    # if we are not keeping something in the dimensions, then either we need
    # to be picking only one from that dimension, or it needs to be a count
    # that can be aggregated
    
    if (any(DATA.TYPE.VALUE.TYPE[outcomes] != 'count'))
    {
        non.aggregatable.data.types = outcomes[DATA.TYPE.VALUE.TYPE[outcomes] != 'count']
        
        if (all(keep.dimensions!='year'))
            stop(paste0("Cannot aggregate data across years for the following outcome(s): ",
                        paste0("'", non.aggregatable.data.types, "'", collapse=', '),
                        ". keep.dimensions must include 'year' for these outcomes"))
        
        if (all(keep.dimensions!='age') &&
            length(ages) > 1 &&
            !setequal(ages, surv$DIMENSION.VALUES$age))
            stop(paste0("Cannot aggregate data across ages for the following outcome(s): ",
                        paste0("'", non.aggregatable.data.types, "'", collapse=', '),
                        ". To pull data for these outcomes, either keep.dimensions must include 'age' ",
                        "or only one age stratum can be selected."))
        
        if (all(keep.dimensions!='race') &&
            length(races) > 1 &&
            !setequal(races, surv$DIMENSION.VALUES$race))
            stop(paste0("Cannot aggregate data across races for the following outcome(s): ",
                        paste0("'", non.aggregatable.data.types, "'", collapse=', '),
                        ". To pull data for these outcomes, either keep.dimensions must include 'race' ",
                        "or only one race can be selected."))
        
        if (all(keep.dimensions!='sex') &&
            length(sexes) > 1 &&
            !setequal(sexes, surv$DIMENSION.VALUES$sex))
            stop(paste0("Cannot aggregate data across sexes for the following outcome(s): ",
                        paste0("'", non.aggregatable.data.types, "'", collapse=', '),
                        ". To pull data for these outcomes, either keep.dimensions must include 'sex' ",
                        "or only one sex can be selected."))
        
        if (all(keep.dimensions!='risk') &&
            length(risks) > 1 &&
            !setequal(risks, surv$DIMENSION.VALUES$risk))
            stop(paste0("Cannot aggregate data across risks for the following outcome(s): ",
                        paste0("'", non.aggregatable.data.types, "'", collapse=', '),
                        ". To pull data for these outcomes, either keep.dimensions must include 'risk' ",
                        "or only one risk stratum can be selected."))
    }
    
    pull.year = any(keep.dimensions=='year')
    pull.age = any(keep.dimensions=='age') || !setequal(ages, surv$DIMENSION.VALUES$age)
    pull.race = any(keep.dimensions=='race') || !setequal(races, surv$DIMENSION.VALUES$race)
    pull.sex = any(keep.dimensions=='sex') || !setequal(sexes, surv$DIMENSION.VALUES$sex)
    pull.risk = any(keep.dimensions=='risk') || !setequal(risks, surv$DIMENSION.VALUES$risk)
    
    inner.dim.names = list(
        year=as.character(years),
        age=ages,
        race=races,
        sex=sexes,
        risk=risks)[c(pull.year, pull.age, pull.race, pull.sex, pull.risk)]
    
    rv = sapply(locations, function(loc){
        sapply(outcomes, function(outcome){
            
            sub.rv = get.surveillance.data(surv=surv,
                                           location.codes = loc,
                                           data.type = outcome,
                                           years=years,
                                           age=pull.age,
                                           race=pull.race,
                                           sex=pull.sex,
                                           risk=pull.risk,
                                           throw.error.if.missing.data = throw.error.if.missing.data)
            
            if (is.null(sub.rv))
                rep(NaN, prod(sapply(inner.dim.names, length)))
            else
            {
                # to put in the right order
                sub.rv = access(sub.rv, 
                                age=inner.dim.names$age, 
                                race=inner.dim.names$race,
                                sex=inner.dim.names$sex,
                                risk=inner.dim.names$risk)
                apply(sub.rv, keep.dimensions, sum, na.rm=na.rm)
            }
        })
    })
    
    dim.names = c(inner.dim.names[keep.dimensions],
                  list(outcome=outcomes,
                       location=locations))
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}

##---------------------------------------##
##-- EXTRACT RESULTS FROM *ONE* SIMSET --##
##---------------------------------------##

get.arr.for.data.type <- function(simset,
                                  data.type,
                                  years,
                                  keep.dimensions,
                                  dimension.subsets,
                                  total.population,
                                  year.anchor,
                                  use.cdc.categorizations=T)
{
    total.population = total.population[as.character(years),]
    dim(total.population) = c(year=length(years), sim=length(total.population)/length(years))
    dimnames(total.population) = list(year=as.character(years),
                                      sim=NULL)
    
    if (data.type=='new')
        arr = extract.simset.new.diagnoses(simset,
                                           years = years, 
                                           all.dimensions = keep.dimensions,
                                           dimension.subsets = dimension.subsets,
                                           total.population = total.population,
                                           use.cdc.categorizations = use.cdc.categorizations)
    else if (data.type=='incidence')
        arr = extract.simset.incidence(simset,
                                       years = years, 
                                       all.dimensions = keep.dimensions,
                                       dimension.subsets = dimension.subsets,
                                       total.population = total.population,
                                       use.cdc.categorizations = use.cdc.categorizations)
    else if (data.type=='prevalence')
        arr = extract.simset.prevalence(simset,
                                        years = years, 
                                        all.dimensions = keep.dimensions,
                                        dimension.subsets = dimension.subsets,
                                        total.population = total.population,
                                        use.cdc.categorizations = use.cdc.categorizations)
    else if (data.type=='mortality')
        arr = extract.simset.hiv.mortality(simset,
                                           years = years, 
                                           all.dimensions = keep.dimensions,
                                           dimension.subsets = dimension.subsets,
                                           total.population = total.population,
                                           use.cdc.categorizations = use.cdc.categorizations)
    else if (data.type=='suppression')
        arr = extract.simset.suppression(simset,
                                         years = years, 
                                         all.dimensions = keep.dimensions,
                                         dimension.subsets = dimension.subsets,
                                         year.anchor=year.anchor,
                                         use.cdc.categorizations = use.cdc.categorizations)
    else if (data.type=='engagement')
        arr = extract.simset.engagement(simset,
                                        years = years, 
                                        all.dimensions = keep.dimensions,
                                        dimension.subsets = dimension.subsets,
                                        year.anchor=year.anchor,
                                        use.cdc.categorizations = use.cdc.categorizations)
    else if (data.type=='suppression.of.engaged')
    {
        arr = extract.simset.suppression(simset,
                                         years = years, 
                                         all.dimensions = keep.dimensions,
                                         dimension.subsets = dimension.subsets,
                                         year.anchor=year.anchor,
                                         use.cdc.categorizations = use.cdc.categorizations) / 
            extract.simset.engagement(simset,
                                      years = years, 
                                      all.dimensions = keep.dimensions,
                                      dimension.subsets = dimension.subsets,
                                      year.anchor=year.anchor,
                                      use.cdc.categorizations = use.cdc.categorizations)
    }
    else if (data.type=='linkage')
        arr = extract.simset.linkage(simset,
                                     years = years, 
                                     all.dimensions = keep.dimensions,
                                     dimension.subsets = dimension.subsets,
                                     year.anchor=year.anchor,
                                     use.cdc.categorizations = use.cdc.categorizations)
    else if (data.type=='retention')
        arr = extract.simset.retention(simset,
                                       years = years, 
                                       all.dimensions = keep.dimensions,
                                       dimension.subsets = dimension.subsets,
                                       year.anchor=year.anchor,
                                       use.cdc.categorizations = use.cdc.categorizations)
    else if (data.type=='gain.of.suppression')
        arr = extract.simset.gain.of.suppression(simset,
                                                 years = years, 
                                                 all.dimensions = keep.dimensions,
                                                 dimension.subsets = dimension.subsets,
                                                 year.anchor=year.anchor,
                                                 use.cdc.categorizations = use.cdc.categorizations)
    else if (data.type=='diagnosed')
        arr = extract.simset.knowledge.of.status(simset,
                                                 years = years, 
                                                 all.dimensions = keep.dimensions,
                                                 dimension.subsets = dimension.subsets,
                                                 use.cdc.categorizations = use.cdc.categorizations)
    else if (data.type=='testing.period')
        arr = extract.simset.testing.period(simset,
                                            years = years, 
                                            all.dimensions = keep.dimensions,
                                            dimension.subsets = dimension.subsets,
                                            year.anchor=year.anchor,
                                            use.cdc.categorizations = use.cdc.categorizations)
    else if (data.type=='testing.rate')
        arr = extract.simset.testing.rates(simset,
                                           years = years, 
                                           all.dimensions = keep.dimensions,
                                           dimension.subsets = dimension.subsets,
                                           year.anchor=year.anchor,
                                           use.cdc.categorizations = use.cdc.categorizations)
    else if (data.type=='testing')
        arr = extract.simset.testing.proportions(simset,
                                                 years = years, 
                                                 all.dimensions = keep.dimensions,
                                                 dimension.subsets = dimension.subsets,
                                                 year.anchor=year.anchor,
                                                 use.cdc.categorizations = use.cdc.categorizations)
    else if (data.type=='prep')
        arr = extract.simset.prep.coverage(simset,
                                           years = years, 
                                           all.dimensions = keep.dimensions,
                                           dimension.subsets = dimension.subsets,
                                           year.anchor=year.anchor,
                                           use.cdc.categorizations = use.cdc.categorizations)
    else if (data.type=='aids.diagnoses')
        arr = extract.simset.aids.diagnoses(simset,
                                            years = years, 
                                            all.dimensions = keep.dimensions,
                                            dimension.subsets = dimension.subsets,
                                            total.population = total.population,
                                            use.cdc.categorizations = use.cdc.categorizations)
    else if (data.type=='population')
        arr = extract.simset.population(simset,
                                        years = years, 
                                        all.dimensions = keep.dimensions,
                                        dimension.subsets = dimension.subsets,
                                        total.population = total.population,
                                        use.cdc.categorizations = use.cdc.categorizations)
    else
        stop(paste0("'", data.type, "' is not a valid data.type."))
    
    arr
}


##---------------------------------------------------------##
##-- HIGH-LEVEL HELPERS TO ASSEMBLE THE MAIN DATA FRAMES --##
##---------------------------------------------------------##

get.truth.df <- function(location,
                         data.type,
                         years,
                         keep.dimensions,
                         dimension.subsets,
                         surv=msa.surveillance,
                         pull.from.state=T)
{
    dimension.subsets = get.nontrivial.dimension.subsets(dimension.subsets,
                                                         data.type=data.type,
                                                         surv=surv)
    
    all.dimensions = union(keep.dimensions, names(dimension.subsets))
    if (data.type=='incidence' || data.type=='testing.period' || data.type=='prep')
        return (NULL)
    transformation.fn = NULL
    
    if (data.type=='population')
    {
        rv = get.population.from.census(location,
                                        years=years,
                                        keep.dimensions=keep.dimensions,
                                        dimension.subsets=dimension.subsets)
        rv$Source = 'US Census Bureau'
        rv
    }
    else if (data.type=='gain.of.suppression')
        rv = NULL
    else
    {
        if (data.type=='testing.rate' || data.type=='testing.period')
        {
            data.type.to.pull = 'testing'
            if (data.type=='testing.rate')
                transformation.fn = function(p){
                    -log(1-p)
                }
            else #data.type == 'testing.period'
                transformation.fn = function(p){
                    -1/log(1-p)
                }
        }
        else
            data.type.to.pull = data.type
        
        rv = get.surveillance.data(surv, location.codes=location, data.type=data.type.to.pull,
                                   years=years,
                                   age=any(all.dimensions=='age'), race=any(all.dimensions=='race'),
                                   sex=any(all.dimensions=='sex'), risk=any(all.dimensions=='risk'),
                                   aggregate.locations = T, aggregate.years = F,
                                   throw.error.if.missing.data = F)
  
        data.source = get.surveillance.data.source(surv, location.codes=location, data.type=data.type.to.pull,
                                                   years=years,
                                                   age=any(all.dimensions=='age'), race=any(all.dimensions=='race'),
                                                   sex=any(all.dimensions=='sex'), risk=any(all.dimensions=='risk'))
        
        if (pull.from.state && data.type=='diagnosed' && is.null(rv) && length(all.dimensions)==1 && all.dimensions=='year')
        {
            rv = get.state.averaged.knowledge.of.status(location,
                                                        state.surveillance,
                                                        years=years,
                                                        census.totals = ALL.DATA.MANAGERS$census.totals)
            
            data.source = get.surveillance.data.source(state.surveillance, location=states.for.msa(location),
                                                       data.type='diagnosed',
                                                       years=years)
            
            dim(rv) = c(year=length(years))
            dimnames(rv) = list(year = as.character(years))
        }
        else if (pull.from.state &&
                 data.type=='suppression' && is.null(rv) &&
                 is.null(get.surveillance.data(surv, location.codes=location, data.type='suppression', throw.error.if.missing.data=F)))
        {
            states = states.for.msa(location)
            if (length(states)==1)
            {
                rv = get.surveillance.data(state.surveillance, location.codes=states, data.type=data.type,
                                           years = years,
                                           age=any(all.dimensions=='age'), race=any(all.dimensions=='race'),
                                           sex=any(all.dimensions=='sex'), risk=any(all.dimensions=='risk'),
                                           aggregate.locations = T, aggregate.years = F,
                                           throw.error.if.missing.data = F)
                
                data.source = get.surveillance.data.source(state.surveillance, location.codes=states, data.type=data.type,
                                                           years = years,
                                                           age=any(all.dimensions=='age'), race=any(all.dimensions=='race'),
                                                           sex=any(all.dimensions=='sex'), risk=any(all.dimensions=='risk'))
            }
        }
        
        if (is.null(rv) || length(rv)==0 || all(is.na(rv)))
            NULL
        else
        {
            if (!is.null(transformation.fn))
                rv = transformation.fn(rv)
            
            if (length(dimension.subsets)>0)
                rv = access(rv, 
                            age=dimension.subsets$age,
                            race=dimension.subsets$race,
                            sex=dimension.subsets$sex,
                            risk=dimension.subsets$risk,
                            collapse.length.one.dimensions = F)
            
            rv = reshape2::melt(rv)
            rv = rv[!is.na(rv$value),]
            
            rv$Source = data.source
            
            rv
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
                        aggregate.statistic = 'mean',
                        ci.coverage=0.95,
                        year.anchor=c('start','mid','end')[2])
{
    if ((year.anchor=='start' || year.anchor=='mid') &&
        (data.type=='suppression' || data.type=='testing.rate' || data.type=='testing.period' || data.type=='prep'))
        years = intersect(simset@simulations[[1]]$years[-1], years)
    else
        years = intersect(simset@simulations[[1]]$years, years)
    
    if (length(years)==0)
        return (NULL)
    
    all.dimensions = union(keep.dimensions, names(dimension.subsets))
    
    #print(paste0('data.type = ', data.type))
    
    arr = get.arr.for.data.type(simset,
                                data.type=data.type,
                                years=years,
                                keep.dimensions=keep.dimensions,
                                dimension.subsets=dimension.subsets,
                                total.population=total.population,
                                year.anchor=year.anchor)
    
    if (simset@n.sim != sum(simset@weights))
    {
        new.dim.names = dimnames(arr)
        new.dim.names$simulation = 1:sum(simset@weights)
        
        dim(arr) = c(everything.else = prod(dim(arr))/simset@n.sim,
                     simulation=simset@n.sim)
        flattened.indices = unlist(sapply(1:simset@n.sim, function(i){
            rep(i, simset@weights[i])
        }))
        arr = arr[,flattened.indices]
        
        dim(arr) = sapply(new.dim.names, length)
        dimnames(arr) = new.dim.names
    }
    
    if (get.individual.sims)
        df.sim = reshape2::melt(arr)
    
    #Aggregate to mean/median and CI if requested
    #Generate the change df if requested
    sim.years = as.numeric(dimnames(arr)[['year']])
    if (!get.individual.sims)
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
        
        df.sim = reshape2::melt(stat, value.name='value')
        df.sim = cbind(df.sim, 
                       'lower'=as.numeric(ci[1,,]), 
                       'upper'=as.numeric(ci[2,,]))
    }
    
    df.sim
}

get.nontrivial.dimension.subsets <- function(dimension.subsets,
                                             data.type,
                                             surv)
{
    non.trivial.mask = sapply(names(dimension.subsets), function(dimension){
        
        if (is.null(dimension.subsets[[dimension]]))
            F
        else
        {
            !setequal(surv$DIMENSION.VALUES[[dimension]], dimension.subsets[[dimension]])
        #    !setequal(names(PRETTY.NAMES[[dimension]]), dimension.subsets[[dimension]])
        }
    })
    
    dimension.subsets[non.trivial.mask]
}


##----------------------##
##----------------------##
##-- HELPER FUNCTIONS --##
##----------------------##
##----------------------##

##-----------------------------------------------------##
##--          OTHER FORWARD-FACING FUNCTIONS         --##
##-- (helpers that will be useful outside this file) --##
##-----------------------------------------------------##

add.total.to.arr <- function(arr,
                             total.name='total',
                             na.rm=F)
{
    dim.names = dimnames(arr)
    if (is.null(dim.names) || !any(names(dim.names)=='location'))
        stop("'arr' must have named dimensions with one dimension named 'location'")
    
    rv = apply(arr, setdiff(names(dim.names), 'location'), function(x){
        c(x, sum(x, na.rm=na.rm))
    })
    
    # update the dimnames
    names(dim(rv))[1] = 'location'
    dim.names$location = c(dim.names$location, 'total')
    
    dimnames(rv) = dim.names[names(dim(rv))]
    
    # put the dimensions in the right order
    rv = apply(rv, names(dim.names), function(x){x})
    
    # return
    rv
}

get.total.population.for.simset <- function(simset,
                                            years,
                                            census.totals)
{
    # set up total population
    total.population = sapply(simset@simulations, get.total.population, 
                              census.totals=census.totals, 
                              years=years)
    
    dim(total.population) = c(length(years), simset@n.sim)
    dimnames(total.population) = list(year=as.character(years), sim=NULL)
    
    total.population
}



##---------------------------------------------------##
##-- HELPERS to EXTRACT specific SIMSET QUANTITIES --##
##---------------------------------------------------##

#per total population in year
extract.simset.new.diagnoses <- function(simset, years, all.dimensions,
                                         dimension.subsets, total.population,
                                         use.cdc.categorizations=T)
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
                                  use.cdc.categorizations=use.cdc.categorizations,
                                  throw.error.if.missing.years = F)
    
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
                                              use.cdc.categorizations=use.cdc.categorizations,
                                              throw.error.if.missing.years = F)
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

extract.simset.aids.diagnoses <- function(simset, years, all.dimensions,
                                          dimension.subsets, total.population,
                                          hiv.to.aids.diagnoses.ratio=mean(c('1999'=1.45,
                                                                             '2000'=1.56,
                                                                             '2001'=1.51,
                                                                             '2002'=1.39,
                                                                             '2003'=1.35,
                                                                             '2004'=1.25)),
                                          use.cdc.categorizations=T
)
{
    extract.simset.new.diagnoses(simset, years=years, 
                                 all.dimensions = all.dimensions,
                                 dimension.subsets = dimension.subsets,
                                 total.population=total.population,
                                 use.cdc.categorizations=use.cdc.categorizations) / hiv.to.aids.diagnoses.ratio
}

#per total population in year
extract.simset.incidence <- function(simset, years, all.dimensions,
                                     dimension.subsets, total.population,
                                     use.cdc.categorizations=T)
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
                              use.cdc.categorizations=use.cdc.categorizations,
                              throw.error.if.missing.years = F)
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
                                          use.cdc.categorizations=use.cdc.categorizations,
                                          throw.error.if.missing.years = F)
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
                                      dimension.subsets, total.population,
                                      use.cdc.categorizations=T)
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
                               continuum=simset@simulations[[1]]$diagnosed.continuum.states,
                               cd4s=NULL,
                               hiv.subsets=NULL,
                               use.cdc.categorizations=use.cdc.categorizations,
                               throw.error.if.missing.years = F)
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
                                           continuum=simset@simulations[[1]]$diagnosed.continuum.states,
                                           cd4s=NULL,
                                           hiv.subsets=NULL,
                                           use.cdc.categorizations=use.cdc.categorizations,
                                           throw.error.if.missing.years = F)
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
                                         dimension.subsets, total.population,
                                         use.cdc.categorizations=T)
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
                                          continuum=simset@simulations[[1]]$diagnosed.continuum.states,
                                          cd4s=NULL,
                                          hiv.subsets=NULL,
                                          use.cdc.categorizations=use.cdc.categorizations,
                                          throw.error.if.missing.years = F)
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
                                                      continuum=simset@simulations[[1]]$diagnosed.continuum.states,
                                                      cd4s=NULL,
                                                      hiv.subsets=NULL,
                                                      use.cdc.categorizations=use.cdc.categorizations,
                                                      throw.error.if.missing.years = F)
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
                                       dimension.subsets, year.anchor,
                                       use.cdc.categorizations=T)
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
                             continuum=simset@simulations[[1]]$diagnosed.continuum.states,
                             cd4=NULL,
                             hiv.subsets=NULL,
                             use.cdc.categorizations=use.cdc.categorizations,
                             year.anchor=year.anchor,
                             throw.error.if.missing.years = F)
    rv = sapply(simset@simulations, extract.suppression,
                years=years, 
                keep.dimensions=all.dimensions,
                per.population=1,
                ages=dimension.subsets[['age']],
                races=dimension.subsets[['race']],
                subpopulations=dimension.subsets[['subpopulation']],
                sexes=dimension.subsets[['sex']],
                risks=dimension.subsets[['risk']],
                continuum=simset@simulations[[1]]$diagnosed.continuum.states,
                cd4=NULL,
                hiv.subsets=NULL,
                use.cdc.categorizations=use.cdc.categorizations,
                year.anchor=year.anchor,
                throw.error.if.missing.years = F)
    
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

extract.simset.engagement<- function(simset, years, all.dimensions,
                                     dimension.subsets, year.anchor,
                                     use.cdc.categorizations=T)
{
    eg = do.extract.engagement(simset@simulations[[1]],
                               years=years, 
                               keep.dimensions=all.dimensions,
                               per.population=1,
                               ages=dimension.subsets[['age']],
                               races=dimension.subsets[['race']],
                               subpopulations=dimension.subsets[['subpopulation']],
                               sexes=dimension.subsets[['sex']],
                               risks=dimension.subsets[['risk']],
                               continuum=simset@simulations[[1]]$diagnosed.continuum.states,
                               cd4=NULL,
                               hiv.subsets=NULL,
                               use.cdc.categorizations=use.cdc.categorizations,
                               throw.error.if.missing.years = F)
    #                             year.anchor=year.anchor)
    rv = sapply(simset@simulations, do.extract.engagement,
                years=years, 
                keep.dimensions=all.dimensions,
                per.population=1,
                ages=dimension.subsets[['age']],
                races=dimension.subsets[['race']],
                subpopulations=dimension.subsets[['subpopulation']],
                sexes=dimension.subsets[['sex']],
                risks=dimension.subsets[['risk']],
                continuum=simset@simulations[[1]]$diagnosed.continuum.states,
                cd4=NULL,
                hiv.subsets=NULL,
                use.cdc.categorizations=use.cdc.categorizations,
                throw.error.if.missing.years = F)
    #                year.anchor=year.anchor)
    
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

extract.simset.linkage<- function(simset, years, all.dimensions,
                                  dimension.subsets, year.anchor,
                                  use.cdc.categorizations=T)
{
    eg = extract.linkage(simset@simulations[[1]],
                         years=years, 
                         keep.dimensions=all.dimensions,
                         per.population=1,
                         ages=dimension.subsets[['age']],
                         races=dimension.subsets[['race']],
                         subpopulations=dimension.subsets[['subpopulation']],
                         sexes=dimension.subsets[['sex']],
                         risks=dimension.subsets[['risk']],
                         continuum='unengaged',
                         cd4=NULL,
                         hiv.subsets=NULL,
                         use.cdc.categorizations=use.cdc.categorizations,
                         throw.error.if.missing.years = F)
    #                             year.anchor=year.anchor)
    rv = sapply(simset@simulations, extract.linkage,
                years=years, 
                keep.dimensions=all.dimensions,
                per.population=1,
                ages=dimension.subsets[['age']],
                races=dimension.subsets[['race']],
                subpopulations=dimension.subsets[['subpopulation']],
                sexes=dimension.subsets[['sex']],
                risks=dimension.subsets[['risk']],
                continuum='unengaged',
                cd4=NULL,
                hiv.subsets=NULL,
                use.cdc.categorizations=use.cdc.categorizations,
                throw.error.if.missing.years = F)
    #                year.anchor=year.anchor)
    
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

extract.simset.retention<- function(simset, years, all.dimensions,
                                    dimension.subsets, year.anchor,
                                    use.cdc.categorizations=T)
{
    continuum = attr(simset@simulations[[1]], 'components')$settings$ENGAGED_STATES
    
    eg = extract.retention(simset@simulations[[1]],
                           years=years, 
                           keep.dimensions=all.dimensions,
                           per.population=1,
                           ages=dimension.subsets[['age']],
                           races=dimension.subsets[['race']],
                           subpopulations=dimension.subsets[['subpopulation']],
                           sexes=dimension.subsets[['sex']],
                           risks=dimension.subsets[['risk']],
                           continuum=continuum,
                           cd4=NULL,
                           hiv.subsets=NULL,
                           use.cdc.categorizations=use.cdc.categorizations,
                           throw.error.if.missing.years = F)
    #                             year.anchor=year.anchor)
    rv = sapply(simset@simulations, extract.retention,
                years=years, 
                keep.dimensions=all.dimensions,
                per.population=1,
                ages=dimension.subsets[['age']],
                races=dimension.subsets[['race']],
                subpopulations=dimension.subsets[['subpopulation']],
                sexes=dimension.subsets[['sex']],
                risks=dimension.subsets[['risk']],
                continuum=continuum,
                cd4=NULL,
                hiv.subsets=NULL,
                use.cdc.categorizations=use.cdc.categorizations,
                throw.error.if.missing.years = F)
    #                year.anchor=year.anchor)
    
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

extract.simset.gain.of.suppression<- function(simset, years, all.dimensions,
                                              dimension.subsets, year.anchor,
                                              use.cdc.categorizations=T)
{
    continuum = setdiff(attr(simset@simulations[[1]], 'components')$settings$ENGAGED_STATES,
                        attr(simset@simulations[[1]], 'components')$settings$SUPPRESSED_STATES)
    
    eg = extract.gain.of.suppression(simset@simulations[[1]],
                                     years=years, 
                                     keep.dimensions=all.dimensions,
                                     per.population=1,
                                     ages=dimension.subsets[['age']],
                                     races=dimension.subsets[['race']],
                                     subpopulations=dimension.subsets[['subpopulation']],
                                     sexes=dimension.subsets[['sex']],
                                     risks=dimension.subsets[['risk']],
                                     continuum=continuum,
                                     cd4=NULL,
                                     hiv.subsets=NULL,
                                     use.cdc.categorizations=use.cdc.categorizations,
                                     throw.error.if.missing.years = F)
    #                             year.anchor=year.anchor)
    rv = sapply(simset@simulations, extract.gain.of.suppression,
                years=years, 
                keep.dimensions=all.dimensions,
                per.population=1,
                ages=dimension.subsets[['age']],
                races=dimension.subsets[['race']],
                subpopulations=dimension.subsets[['subpopulation']],
                sexes=dimension.subsets[['sex']],
                risks=dimension.subsets[['risk']],
                continuum=continuum,
                cd4=NULL,
                hiv.subsets=NULL,
                use.cdc.categorizations=use.cdc.categorizations,
                throw.error.if.missing.years = F)
    #                year.anchor=year.anchor)
    
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

extract.simset.prep.coverage <- function(simset, years, all.dimensions,
                                         dimension.subsets, year.anchor,
                                         use.cdc.categorizations=T)
{
    eg = extract.prep.coverage(simset@simulations[[1]],
                               years=years, 
                               keep.dimensions=all.dimensions,
                               per.population=1,
                               ages=dimension.subsets[['age']],
                               races=dimension.subsets[['race']],
                               subpopulations=dimension.subsets[['subpopulation']],
                               sexes=dimension.subsets[['sex']],
                               risks=dimension.subsets[['risk']],
                               use.cdc.categorizations=use.cdc.categorizations,
                               year.anchor=year.anchor,
                               throw.error.if.missing.years = F)
    
    rv = sapply(simset@simulations, extract.prep.coverage,
                years=years, 
                keep.dimensions=all.dimensions,
                per.population=1,
                ages=dimension.subsets[['age']],
                races=dimension.subsets[['race']],
                subpopulations=dimension.subsets[['subpopulation']],
                sexes=dimension.subsets[['sex']],
                risks=dimension.subsets[['risk']],
                use.cdc.categorizations=use.cdc.categorizations,
                year.anchor=year.anchor,
                throw.error.if.missing.years = F)
    
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

extract.simset.testing.rates <- function(simset, years, all.dimensions,
                                         dimension.subsets, year.anchor,
                                         use.cdc.categorizations=T)
{
    eg = extract.testing.rates(simset@simulations[[1]],
                               years=years, 
                               keep.dimensions=all.dimensions,
                               per.population=1,
                               ages=dimension.subsets[['age']],
                               races=dimension.subsets[['race']],
                               subpopulations=dimension.subsets[['subpopulation']],
                               sexes=dimension.subsets[['sex']],
                               risks=dimension.subsets[['risk']],
                               use.cdc.categorizations=use.cdc.categorizations,
                               year.anchor=year.anchor,
                               throw.error.if.missing.years = F)
    
    rv = sapply(simset@simulations, extract.testing.rates,
                years=years, 
                keep.dimensions=all.dimensions,
                per.population=1,
                ages=dimension.subsets[['age']],
                races=dimension.subsets[['race']],
                subpopulations=dimension.subsets[['subpopulation']],
                sexes=dimension.subsets[['sex']],
                risks=dimension.subsets[['risk']],
                use.cdc.categorizations=use.cdc.categorizations,
                year.anchor=year.anchor,
                throw.error.if.missing.years = F)
    
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

extract.simset.testing.proportions <- function(simset, years, all.dimensions,
                                               dimension.subsets, year.anchor,
                                               use.cdc.categorizations=T)
{
    eg = extract.testing.proportions(simset@simulations[[1]],
                                     years=years, 
                                     keep.dimensions=all.dimensions,
                                     per.population=1,
                                     ages=dimension.subsets[['age']],
                                     races=dimension.subsets[['race']],
                                     subpopulations=dimension.subsets[['subpopulation']],
                                     sexes=dimension.subsets[['sex']],
                                     risks=dimension.subsets[['risk']],
                                     use.cdc.categorizations=use.cdc.categorizations,
                                     year.anchor=year.anchor,
                                     throw.error.if.missing.years = F)
    
    rv = sapply(simset@simulations, extract.testing.proportions,
                years=years, 
                keep.dimensions=all.dimensions,
                per.population=1,
                ages=dimension.subsets[['age']],
                races=dimension.subsets[['race']],
                subpopulations=dimension.subsets[['subpopulation']],
                sexes=dimension.subsets[['sex']],
                risks=dimension.subsets[['risk']],
                use.cdc.categorizations=use.cdc.categorizations,
                year.anchor=year.anchor,
                throw.error.if.missing.years = F)
    
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

extract.simset.testing.period <- function(simset, years, all.dimensions,
                                          dimension.subsets, year.anchor,
                                          use.cdc.categorizations=T)
{
    eg = extract.testing.period(simset@simulations[[1]],
                                years=years, 
                                keep.dimensions=all.dimensions,
                                per.population=1,
                                ages=dimension.subsets[['age']],
                                races=dimension.subsets[['race']],
                                subpopulations=dimension.subsets[['subpopulation']],
                                sexes=dimension.subsets[['sex']],
                                risks=dimension.subsets[['risk']],
                                use.cdc.categorizations=use.cdc.categorizations,
                                year.anchor=year.anchor,
                                throw.error.if.missing.years = F)
    
    rv = sapply(simset@simulations, extract.testing.period,
                years=years, 
                keep.dimensions=all.dimensions,
                per.population=1,
                ages=dimension.subsets[['age']],
                races=dimension.subsets[['race']],
                subpopulations=dimension.subsets[['subpopulation']],
                sexes=dimension.subsets[['sex']],
                risks=dimension.subsets[['risk']],
                use.cdc.categorizations=use.cdc.categorizations,
                year.anchor=year.anchor,
                throw.error.if.missing.years = F)
    
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
                                               dimension.subsets,
                                               use.cdc.categorizations=T)
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
                                  use.cdc.categorizations=use.cdc.categorizations,
                                  throw.error.if.missing.years = F)
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
                use.cdc.categorizations=use.cdc.categorizations,
                throw.error.if.missing.years = F)
    
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

extract.simset.population <- function(simset, years, all.dimensions,
                                      dimension.subsets, total.population,
                                      use.cdc.categorizations=T)
{
    eg = do.extract.population.subset(simset@simulations[[1]],
                                      years=years, 
                                      keep.dimensions=all.dimensions,
                                      per.population=NA,
                                      ages=dimension.subsets[['age']],
                                      races=dimension.subsets[['race']],
                                      subpopulations=dimension.subsets[['subpopulation']],
                                      sexes=dimension.subsets[['sex']],
                                      risks=dimension.subsets[['risk']],
                                      continuum=NULL,
                                      cd4=NULL,
                                      hiv.subsets=NULL,
                                      use.cdc.categorizations=use.cdc.categorizations)
    
    rv = sapply(1:length(simset@simulations), function(i)
    {
        sim = simset@simulations[[i]]
        numerators = do.extract.population.subset(sim,
                                                  years=years, 
                                                  keep.dimensions=all.dimensions,
                                                  per.population=NA,
                                                  ages=dimension.subsets[['age']],
                                                  races=dimension.subsets[['race']],
                                                  subpopulations=dimension.subsets[['subpopulation']],
                                                  sexes=dimension.subsets[['sex']],
                                                  risks=dimension.subsets[['risk']],
                                                  continuum=NULL,
                                                  cd4=NULL,
                                                  hiv.subsets=NULL,
                                                  use.cdc.categorizations=use.cdc.categorizations)
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

