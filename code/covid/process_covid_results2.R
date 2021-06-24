
source('code/source_code.R')
source('code/covid/prepare_and_run_covid_sims.R')


if (1==2)
{
  #  files = list.files(COVID.DIR, include.dirs = F)
   # files = files[grepl('.Rdata', files)]
    #covid.version.string = gsub("\\.", "\\\\.", COVID.VERSION)
    #files = files[grepl(covid.version.string, files)]
    
 #   msas = unique(gsub("^[^_]+_[^_]+_([0-9]+)_?.*\\.Rdata$", '\\1', files))
    
    msas = c(ATLANTA.MSA, BALTIMORE.MSA, MIAMI.MSA, NYC.MSA)
    
    df = NULL
    for (msa in msas)
    {
        print(paste0("Synthesizing ", msa))
        df = rbind(df, load.files.and.make.summary.data.frame(msa=msa))
    }
    
    write.csv(df, file=paste0("code/covid/results/jheem_covid_", COVID.VERSION, "_results.csv"))
}

FULL.DIR = 'mcmc_runs/full_simsets'
COVID.DIR = 'mcmc_runs/covid_simsets'

load.files.and.make.summary.data.frame <- function(msa,
                                                   outcomes=c('incidence','new','prevalence','diagnosed','suppression'),
                                                   scenarios=c('base','delayed.hiv.care','rebound.sexual.transmission','rebound.sex.delayed.hiv.care'),
                                                   intervention.names = c(NA, 'testing.75.6.6'),
                                                   categories=c('sexual.transmission','testing','suppression','prep'),
                                                   years=2010:2030,
                                                   version=COVID.VERSION,
                                                   verbose=T)
{
    if (verbose)
        print(paste0("Making summary data frame for ",
                     msa.names(msa), " (", msa, ")"))
    
    if (verbose)
        print("Loading files")
    
    noint.filename = get.simset.filename(location=msa, intervention=NO.INTERVENTION)
    load(file=file.path(FULL.DIR, msa, noint.filename))
    noint = simset
    
    baseline.filename = get.full.filename(msa)
    load(file=file.path(FULL.DIR, baseline.filename))
    baseline = simset
    
    n.scenarios = length(scenarios)
    n.interventions = length(intervention.names)
    
    scenarios = rep(scenarios, each=n.interventions)
    intervention.names = rep(intervention.names, n.scenarios)
    
    file.names = sapply(1:length(scenarios), function(i){
        get.covid.simset.name(msa, version, scenario=scenarios[i], intervention.name = intervention.names[i])
    })
    
    simsets = lapply(file.names, function(name){
        load(file=file.path(COVID.DIR, msa, name))
        simset        
    })
    
    simset.names = paste0(scenarios, '.', intervention.names)
    simset.names[is.na(intervention.names)] = scenarios[is.na(intervention.names)]
    names(simsets) = simset.names
    
    rv = make.summary.data.frame(simsets=simsets, 
                                 noint=noint,
                                 baseline=baseline,
                                 outcomes=outcomes, categories=categories, years=years,
                                 verbose=verbose)
    
    print("ALL DONE!")
    
    rv
}

make.summary.data.frame <- function(simsets,
                                    noint,
                                    baseline,
                                    outcomes=c('incidence','new','prevalence','diagnosed','suppression'),
                                    categories=c('sexual.transmission','testing','suppression','prep'),
                                    years=2010:2030,
                                    verbose=T)
{
    # Set up data frame
    msa = attr(simsets[[1]]@simulations[[1]], 'location')
    scenarios = names(simsets)
    
    rv = data.frame(msa=msa,
                    msa.name=unlist(msa.names(msa)))
    
    # Add outcomes
    for (outcome in outcomes)
    {
        if (verbose)
            print(paste0("Adding outcomes for ", outcome))
        
        per.population=c(NA,100000)
        names(per.population) = c('', '.rate')
        if (outcome=='new')
            fn = extract.new.diagnoses
        else if (outcome=='prevalence')
            fn = extract.prevalence
        else if (outcome=='diagnosed' || outcome=='suppression')
        {
            if (outcome=='suppression')
                fn = extract.suppression
            else
                fn = extract.diagnosed.hiv
            
            per.population=1
            names(per.population) = ''
        }
        else
            fn = extract.incidence
        
        for (i in 1:length(per.population))
        {
            per.pop = per.population[i]
            per.pop.name = names(per.population)[i]
            
            noint.years = intersect(years, noint@simulations[[1]]$years)
            noint.outcomes = as.data.frame(t(sapply(noint@simulations, fn, years=noint.years, keep.dimensions='year', per.population=per.pop)))
            dimnames(noint.outcomes)[[2]] = paste0('baseline_',outcome,per.pop.name,'_',noint.years)
            
            baseline.years = setdiff(intersect(years, baseline@simulations[[1]]$years), noint.years)
            baseline.outcomes = as.data.frame(t(sapply(baseline@simulations, fn, years=baseline.years, keep.dimensions='year', per.population=per.pop)))
            dimnames(baseline.outcomes)[[2]] = paste0('baseline_',outcome,per.pop.name,'_',baseline.years)
            
            rv = cbind(rv, baseline.outcomes, noint.outcomes)
                       
            for (i in 1:length(scenarios))
            {
                simset = simsets[[i]]
                simset.years = intersect(years, simset@simulations[[1]]$years)
                simset.outcomes = as.data.frame(t(sapply(simset@simulations, fn, years=simset.years, keep.dimensions='year', per.population=per.pop)))
                dimnames(simset.outcomes)[[2]] = paste0('COVID.', scenarios[i], "_", outcome,per.pop.name,'_',simset.years)
                rv = cbind(rv, simset.outcomes)
            }
        }
                                             
    }
    
    # Plug in parameter values (and synthesis of them)
    for (catg in categories)
    {
            if (verbose)
                print(paste0("Adding parameters relating to ", catg))
            
            rv[,paste0(catg, '.reduction')] = simsets[[1]]@parameters[,paste0(catg, '.reduction')]
            
            if (catg=='sexual.transmission')
                rv[,paste0(catg, '.increase')] = simsets[[1]]@parameters[,paste0(catg, '.increase')]
#            rv[,paste0(catg, '.time.averaged.reduction')] = calculate.reduction.auc(catg, parameters=simset@parameters)
            
        #    param.values = as.data.frame(simset@parameters[,paste0(catg, c('.start.normalize.time','.normal.time'))]) + COVID.TIME.OFFSET
         #   rv = cbind(rv, param.values)
        
    }
    
    # Returns
    
    rv
}

calculate.reduction.auc <- function(catg, parameters,
                          from=COVID.TIME.OFFSET,
                          to=COVID.TIME.OFFSET + 2)
{
    times = cbind(COVID.TIME.OFFSET,
                  COVID.TIME.OFFSET + COVID.EFFECT.ONSET.TIME,
                  COVID.TIME.OFFSET + parameters[,paste0(catg, '.start.normalize.time')],
                  COVID.TIME.OFFSET + parameters[,paste0(catg, '.normal.time')],
                  to)
    n.times = dim(times)[2]
    time.deltas = times[,-1] - times[,-n.times]
    
    reductions = cbind(0,
                       parameters[,paste0(catg, '.reduction')],
                       parameters[,paste0(catg, '.reduction')],
                       0,
                       0)
    reductions.at.start = reductions[,-n.times]
    reductions.at.end = reductions[,-1]
    avg.reductions = (reductions.at.end + reductions.at.start)/2
    
    rowSums(avg.reductions * time.deltas) / (to-from)
}