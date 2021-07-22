

get.sim.absolute.outcome.array <- function(sim,
                                  years=2010:2018,
                                  outcomes=c('incidence','new','prevalence','mortality','suppression','diagnosed'),
                                  stratifications=c('total',
                                                    'age',
                                                    'race',
                                                    'sex',
                                                    'risk',
                                                    'sex.age',
                                                    'sex.race',
                                                    'sex.risk',
                                                    'race.risk')[1:5],
                                  use.cdc.categorizations=T)
{
    outcome.mapping = list(
        incidence='incidence',
        new='new',
        prevalence='prevalence.diagnosed',
        mortality='mortality',
        suppression=c('suppression','prevalence.all'),
        diagnosed=c('prevalence.diagnosed','prevalence.all')
    )
    outcomes.to.get = unique(unlist(outcome.mapping[outcomes]))
    
    strat.categories = list()
    
    rv = sapply(outcomes.to.get, function(outcome){
        unlist(sapply(stratifications, function(strat){
            if (strat=='total')
                to.keep = 'year'
            else
                to.keep = c('year', strsplit(strat, ".", fixed=T)[[1]])
        
            if (outcome=='incidence')
                raw = project.absolute.incidence(sim, years=years, keep.dimensions = to.keep, use.cdc.categorizations=use.cdc.categorizations)
            else if (outcome=='new')
                raw = project.absolute.new.diagnoses(sim, years=years, keep.dimensions = to.keep, use.cdc.categorizations=use.cdc.categorizations)
            else if (outcome=='prevalence.diagnosed')
                raw = project.absolute.prevalence.aware(sim, years=years, keep.dimensions = to.keep, use.cdc.categorizations=use.cdc.categorizations)
            else if (outcome=='mortality')
                raw = project.absolute.overall.hiv.mortality(sim, years=years, keep.dimensions = to.keep, use.cdc.categorizations=use.cdc.categorizations)
            else if (outcome=='suppression')
                raw = project.absolute.n.suppressed(sim, years=years, keep.dimensions = to.keep, use.cdc.categorizations=use.cdc.categorizations)
            else if (outcome=='prevalence.all')
                raw = project.absolute.prevalence(sim, years=years, keep.dimensions = to.keep, use.cdc.categorizations=use.cdc.categorizations)
            else
                stop("Invalid outcome for sim array")
            
            if (is.null(strat.categories[[strat]]))
            {
                if (strat=='total')
                    strat.categories$total <<- 'total'
                else
                    strat.categories[[strat]] <<- collapse.dim.names(dimnames(raw)[-1])
            }
            
            raw
        }))
    })
    
    dim.names = list(year=as.character(years),
                     stratification = as.character(unlist(strat.categories)),
                     outcome = outcomes.to.get)
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}

collapse.dim.names <- function(dim.names,
                               sep='.')
{
    rv = dim.names[[1]]
    if (length(dim.names)>1)
    {
        for (i in 2:length(dim.names))
        {
            rv = paste0(rep(rv, length(dim.names[[i]])),
                        sep,
                        rep(dim.names[[i]], each=length(rv)))
        }
    }
    
    rv
}

get.simset.absolute.outcome.array <- function(simset,
                                              years=2010:2018,
                                              outcomes=c('incidence','new','prevalence','mortality','suppression','diagnosed'),
                                              stratifications=c('total',
                                                                'age',
                                                                'race',
                                                                'sex',
                                                                'risk',
                                                                'sex.age',
                                                                'sex.race',
                                                                'sex.risk',
                                                                'race.risk')[1:5])
{
    sub1 = get.sim.absolute.outcome.array(simset@simulations[[1]], years=years, outcomes, stratifications=stratifications)
    rv = sapply(simset@simulations, get.sim.absolute.outcome.array, years=years, outcomes=outcomes, stratifications=stratifications)
    
    dim.names = c(dimnames(sub1),
                  list(sim=as.character(1:simset@n.sim)))
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}

get.absolute.outcome.arrays <- function(dir = file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets'),
                                        locations=TARGET.MSAS,
                                        interventions=list(NULL),
                                        years=2010:2018,
                                        outcomes=c('incidence','new','prevalence','mortality','suppression','diagnosed'),
                                        stratifications=c('total',
                                                          'age',
                                                          'race',
                                                          'sex',
                                                          'risk',
                                                          'sex.age',
                                                          'sex.race',
                                                          'sex.risk',
                                                          'race.risk')[1:5],
                                        verbose=T)
{
    if (!is(interventions, 'list'))
        interventions = list(interventions)
    
    intervention.names = sapply(interventions, function(int){
        if (is.null(int))
            "Baseline"
        else
            get.intervention.name(int)
    })
    
    if (verbose)
        print(paste0("Extracting arrays for ", 
                     length(interventions), 
                     " intervention", ifelse(length(interventions)==1, "", "s"),
                     " across ",
                     length(locations), 
                     " location", ifelse(length(locations)==1, "", "s")
                     ))
    
    elems = lapply(1:length(interventions), function(int.i){
        int = interventions[[int.i]]
        if (verbose)
            print(paste0("- Extracting ", 
                         length(locations), 
                         " location", ifelse(length(locations)==1, "", "s"),
                         " for intervention '",
                         intervention.names[int.i],
                         "' (", int.i, " of ", length(interventions), ")"))
        
        lapply(1:length(locations), function(loc.i){
            loc = locations[loc.i]
            
            if (is.null(int))
                file = file.path(dir, get.full.filename(location=loc))
            else
                file = file.path(dir, location, get.simset.filename(location=loc, intervention=int))
            
            if (file.exists(file))
            {
                if (verbose)
                    print(paste0("  - Extracting results for '",
                                 locations[loc.i], "' (", loc.i, " of ", length(locations), ")"))
                
                load(file)
                get.simset.absolute.outcome.array(simset, years=years, outcomes=outcomes, stratifications = stratifications)
            }
            else
            {
                if (verbose)
                    print(paste0("  - Skipping ",
                                 locations[i], " (", loc.i, " of ", length(locations), ") - not done"))
                
                NULL
            }
        })
    })
    
    # Set up dim names
    all.null.for.int = sapply(elems, function(elem){all(sapply(elem, is.null))})
    if (all(all.null.for.int))
        stop("None of the requested locations x interventions has been done")
    
    
    non.null.elem = elems[!all.null.for.int][[1]]
    non.null.simset = non.null.elem[!sapply(non.null.elem, is.null)][[1]]
    simset.dim.names = dimnames(non.null.simset)
    dim.names = list(
        year=simset.dim.names$year,
        stratification=simset.dim.names$stratification,
        outcome=simset.dim.names$outcome,
        location=locations,
        intervention=intervention.names,
        sim=simset.dim.names$sim
    )
    
    
    # pull into rv
    rv = array(NaN, dim=sapply(dim.names, length), dimnames=dim.names)
    for (int.i in 1:length(intervention.names))
    {
        for (loc.i in 1:length(locations))
        {
            if (!is.null(elems[[int.i]][[loc.i]]))
                rv[,,,loc.i,int.i,] = elems[[int.i]][[loc.i]]
        }
    }
    
    if (verbose)
        print("All Done")
    
    rv
}

distributed.get.absolute.outcome.array <- function(dir.name = c('full','quick')[1],
                                                   location,
                                                   intervention=NULL,
                                                   years=2010:2018,
                                                   outcomes=c('incidence','new','prevalence','mortality','suppression','diagnosed'),
                                                   stratifications=c('total',
                                                                     'age',
                                                                     'race',
                                                                     'sex',
                                                                     'risk',
                                                                     'sex.age',
                                                                     'sex.race',
                                                                     'sex.risk',
                                                                     'race.risk')[1:5],
                                                   verbose=T,
                                                   overwrite=F)
{
    dst.file = get.distributed.abs.arr.filename(intervention=intervention, location=location, dir.name=dir.name)
    if (is.null(intervention))
        int.name = 'Baseline'
    else
        int.name = get.intervention.name(intervention)
    
    
    if (overwrite || !file.exists(dst.file))
    {
        dir = file.path(SYSTEMATIC.ROOT.DIR, paste0(dir.name, '_simsets'))
        
        if (is.null(intervention))
            file = file.path(dir, get.full.filename(location=location))
        else
            file = file.path(dir, location, get.simset.filename(location=location, intervention=intervention))
        
        if (file.exists(file))
        {
            load(file)
            arr = get.simset.absolute.outcome.array(simset,
                                                    years=years,
                                                    outcomes=outcomes,
                                                    stratifications=stratifications)
            save(arr, file=dst.file)
            
            if (verbose)
                print(paste0("Done extracting for intervention '", int.name, "' at location '", location, "'"))
        }
        else if (verbose)
            print(paste0("The requested intervention (", int.name, ") has not been run at the location (", location, ")"))
    }
    else if (verbose)
        print(paste0("Skipping - absolute array has already been extracted for intervention '", int.name, 
                     "' at location '", location, "'"))
}

get.distributed.abs.arr.filename <- function(intervention,
                                             location,
                                             dir.name = NULL)
{
    if (is.null(intervention))
        int.name = 'Baseline'
    else
        int.name = get.intervention.name(intervention)
    
    rv = paste0(location, "_", int.name, ".Rdata")
    
    if (!is.null(dir.name))
        rv = file.path(SYSTEMATIC.ROOT.DIR, '..', 'results', dir.name, 'scratch', rv)
    
    rv
}


assemble.absolute.outcome.arrays <- function(dir.name = c('full','quick')[1],
                                             locations=TARGET.MSAS,
                                             interventions=list(NULL),
                                             verbose=T)
{
    dir = file.path(SYSTEMATIC.ROOT.DIR, paste0(dir.name, '_simsets'))
    
    if (!is(interventions, 'list'))
        interventions = list(interventions)
    
    intervention.names = sapply(interventions, function(int){
        if (is.null(int))
            "Baseline"
        else
            get.intervention.name(int)
    })
    
    if (verbose)
        print(paste0("Loading arrays for ", 
                     length(interventions), 
                     " intervention", ifelse(length(interventions)==1, "", "s"),
                     " across ",
                     length(locations), 
                     " location", ifelse(length(locations)==1, "", "s")
        ))
    
    elems = lapply(1:length(interventions), function(int.i){
        int = interventions[[int.i]]
        if (verbose)
            print(paste0("- Loading ", 
                         length(locations), 
                         " location", ifelse(length(locations)==1, "", "s"),
                         " for intervention '",
                         intervention.names[int.i],
                         "' (", int.i, " of ", length(interventions), ")"))
        
        lapply(1:length(locations), function(loc.i){
            loc = locations[loc.i]
            
            file = get.distributed.abs.arr.filename(intervention=intervention, location=location, dir.name=dir.name)
            
            if (file.exists(file))
            {
                if (verbose)
                    print(paste0("  - Loading results for '",
                                 locations[i], "' (", loc.i, " of ", length(locations), ")"))
                load(file)
                arr
            }
            else
            {
                if (verbose)
                    print(paste0("  - Skipping ",
                                 locations[i], " (", loc.i, " of ", length(locations), ") - not done"))
                
                NULL
            }
        })
    })
    
    # Set up dim names
    all.null.for.int = sapply(elems, function(elem){all(sapply(elem, is.null))})
    if (all(all.null.for.int))
        stop("None of the requested locations x interventions has been done")
    
    
    non.null.elem = elems[!all.null.for.int][[1]]
    non.null.simset = non.null.elem[!sapply(non.null.elem, is.null)][[1]]
    simset.dim.names = dimnames(non.null.simset)
    dim.names = list(
        year=simset.dim.names$year,
        stratification=simset.dim.names$stratification,
        outcome=simset.dim.names$outcome,
        location=locations,
        intervention=intervention.names,
        sim=simset.dim.names$sim
    )
    
    
    # pull into rv
    rv = array(NaN, dim=sapply(dim.names, length), dimnames=dim.names)
    for (int.i in 1:length(intervention.names))
    {
        for (loc.i in 1:length(locations))
        {
            if (!is.null(elems[[int.i]][[loc.i]]))
                rv[,,,loc.i,int.i,] = elems[[int.i]][[loc.i]]
        }
    }
    
    if (verbose)
        print("All Done")
    
    rv
}

add.abs.total.row <- function(abs.arr,
                              mask=NULL)
{
    non.loc.dims = setdiff(names(dimnames(abs.arr)), 'location')
    if (!is.null(mask))
    {
        mask = expand.population(mask, target.dim.names = dimnames(abs.arr))
        totals = apply(abs.arr * mask, non.loc.dims, sum)
    }
    else
        totals = apply(abs.arr, non.loc.dims, sum)
    
    dim.names = dimnames(abs.arr)
    dim.names$location = c(dim.names$location, 'Total')
    
    rv = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    rv[,,,dimnames(abs.arr)$location,,] = abs.arr
    rv[,,,'Total',,] = totals
    
    rv
}

get.suppression.from.absolute <- function(abs.arr)
{
    abs.arr[,,'suppression',,,] / abs.arr[,,'prevalence.diagnosed',,,]
}

get.diagnosed.from.absolute <- function(abs.arr)
{   
    abs.arr[,,'prevalence.diagnosed',,,] / abs.arr[,,'prevalence.all',,,]
}

get.surveillance.outcomes.array <- function(years=2010:2018,
                                            locations=TARGET.MSAS,
                                            outcomes=c('new','prevalence','suppression'),
                                            stratifications=c('total',
                                                              'age',
                                                              'race',
                                                              'sex',
                                                              'risk',
                                                              'sex.age',
                                                              'sex.race',
                                                              'sex.risk',
                                                              'race.risk')[1:5],
                                            surv=msa.surveillance)
{
    
    strat.categories = list()
    
    rv = sapply(locations, function(loc){
            sapply(outcomes, function(outcome){
                unlist(sapply(stratifications, function(strat){
                    
#                    print(paste0("Location = ", loc, "\nOutcome = ", outcome, "\nStrat = ", strat))

                    raw = get.surveillance.data(surv=surv,
                                                location.codes = loc, 
                                                years = years,
                                                data.type = outcome,
                                                age=grepl('age', strat),
                                                race=grepl('race', strat),
                                                sex=grepl('sex', strat),
                                                risk=grepl('risk', strat),
                                                throw.error.if.missing.data = F,
                                                throw.error.if.missing.years = F)
                    
                    if (is.null(raw))
                    {
                        if (is.null(strat.categories[[strat]]))
                            stop(paste0("Data missing for '", outcome, "' at location '", location, "' for ",
                                        "stratification ", strat, " - and no previous data has been pulled"))
                        
                        raw = array(NaN, dim=c(year=length(years), strat=length(strat.categories[[strat]])))
                    }
                    
                    if (is.null(strat.categories[[strat]]))
                    {
                        if (strat=='total')
                            strat.categories$total <<- 'total'
                        else
                            strat.categories[[strat]] <<- collapse.dim.names(dimnames(raw)[-1])
                    }
                    
                    raw
                    
                }))
            })
    })
    
    dim.names = list(year=as.character(years),
                     stratification = as.character(unlist(strat.categories)),
                     outcome = outcomes,
                     location=locations)

    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}