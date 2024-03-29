
##-----------------##
##-- SOURCE CODE --##
##-----------------##


library(scales)

#source('code/source_code.R')
#source('code/targets/target_msas.R')
#source('code/interventions/systematic_interventions.R')
#source('code/interventions/synthesize_interventions.R')

##--------------------------------------------##
##-- CODE TO RUN OUR MAIN ANALYSIS - BY MSA --##
##--------------------------------------------##

SAVE.DIR = '../Manuscripts/manuscript_1/tables'
N.SIM = 1000

i.total = 1 + length(TARGET.MSAS)
if (1==2)
{
    
    #-- FOR ANALYSIS 1: --#
    
    est.main.10.3y = get.estimates.and.intervals('mcmc_runs/full_simsets/', 
                                              msas=TARGET.MSAS,
                                              interventions=A1.INTERVENTION.SET.3Y,
                                              year1=2020,
                                              year2=2030,
                                              n.sim=N.SIM)
    save(est.main.10.3y, file='results/est.main.10.3y.Rdata')
    load('results/est.main.10.3y.Rdata')
    write.systematic.table(est.main.10.3y, file=file.path(SAVE.DIR, 'main_table_10y_3y.rollout.xlsx'))
    
    est.main.5.3y = get.estimates.and.intervals('mcmc_runs/full_simsets/', 
                                             msas=TARGET.MSAS,
                                             interventions=A2.INTERVENTION.SET.3Y,
                                             year1=2020,
                                             year2=2025,
                                             n.sim=N.SIM)
    save(est.main.5.3y, file='results/est.main.5.3y.Rdata')
    load('results/est.main.5.3y.Rdata')
    write.systematic.table(est.main.5.3y, file=file.path(SAVE.DIR, 'main_table_5y_3y.rollout.xlsx'),
                           threshold = 0.75)

    #-- FOR ANALYSIS 2: --#
    
    est.secondary.10.3y = get.estimates.and.intervals('mcmc_runs/full_simsets/', 
                                      msas=TARGET.MSAS,
                                      interventions=A2.INTERVENTION.SET,
                                      n.sim=N.SIM)
    save(est.secondary.10.3y, file='results/est.secondary.10.3y.Rdata')
    load('results/est.secondary.10.3y.Rdata')
    write.systematic.table(est.secondary.10.3y, file=file.path(SAVE.DIR, 'secondary_table_10y_3y.rollout.xlsx'))
    
    
    est.secondary.5.3y = get.estimates.and.intervals('mcmc_runs/full_simsets/', 
                                                     msas=TARGET.MSAS,
                                                     interventions=A2.INTERVENTION.SET,
                                                     year1=2020,
                                                     year2=2025,
                                                     n.sim=N.SIM)
    save(est.secondary.5.3y, file='results/est.secondary.5.3y.Rdata')
    load('results/est.secondary.5.3y.Rdata')
    write.systematic.table(est.secondary.5.3y, file=file.path(SAVE.DIR, 'secondary_table_5y_3y.rollout.xlsx'),
                           threshold = 0.75)
    
    
    #-- Baselines --#
    
    baselines.msm = get.baseline.estimates(year=2020, risks='msm')
    save(baselines.msm, file='results/baselines.msm.Rdata')
    
    apply(baselines.msm[,,'mean'], 2, range)
    
    #-- Summarizing Main --#
    load('results/est.main.10.3y.Rdata')
    load('results/est.main.5.3y.Rdata')
    
    est = est10 = est.main.10.3y
    est5 = est.main.5.3y
    mask = T#c(TARGET.MSAS != SEATTLE.MSA,F)
    
    # Ranges
    t(floor(100*apply(est$estimates[mask,], 2, range, na.rm=T)))
    
    
    # YBH
    i.high.ybhm = 8
    sum(est$estimates[-i.total,i.high.ybhm]>0.5)
    mean(est$estimates[-i.total,i.high.ybhm]>0.5)
    
    # All MSM
    i.high.mi = 12
    sum(est$estimates[-i.total,i.high.mi]>0.9)
    mean(est$estimates[-i.total,i.high.mi]>0.9)
    
    # Highest Int
    i.highest = dim(est$estimates)[2]
    sum(est$estimates[-i.total,i.highest]>0.9)
    mean(est$estimates[-i.total,i.highest]>0.9)
    
    # CI for total
    round(100*cbind(est$estimates[i.total,],
                    est$ci.lower[i.total,],
                    est$ci.upper[i.total,]))
    
    # NYC CI
    round(100*cbind(est$estimates[1,],
                    est$ci.lower[1,],
                    est$ci.upper[1,]))
    
    
    # Can you get there in 2030 without getting there in 2025?
    sum(est10$estimates[-i.total,-1]>.9 & est5$estimates[-i.total,-1]<=.75, na.rm=T)
    
    sum(est10$estimates[-i.total,-1]<.9 & est5$estimates[-i.total,-1]>.75, na.rm=T)
    mean(est10$estimates[-i.total,-1]<.9 & est5$estimates[-i.total,-1]>.75, na.rm=T)
    
    apply(est10$estimates<.9 & est5$estimates>.75, 1, any)
    
    mean(est5$estimates[-i.total,-1]>=0.75)
    sum(est5$estimates[-i.total,-1]>=0.75)
    
    
    sum(est10$estimates[-i.total,-1]>=0.9 & est5$estimates[-i.total,-1]>=0.75)
    sum(est10$estimates[-i.total,-1]>=0.9 & est5$estimates[-i.total,-1]>=0.75)/sum(est5$estimates[-i.total,-1]>=0.75)
    
    #-- Summarizing Secondary --#
    
    load('results/est.secondary.10.3y.Rdata')
    load('results/est.secondary.5.3y.Rdata')
    
    mask = T#c(TARGET.MSAS != SEATTLE.MSA,F)
    
    # Ranges
    i.testing1 = 8
    i.testing2 = 9
    diff.testing = est.secondary.10.3y$estimates[-i.total,i.testing2] - est.secondary.10.3y$estimates[-i.total,i.testing1]
    range(diff.testing)
    
    x = range(diff.testing)
    names(x) = c(msa.names(names(diff.testing)[diff.testing==min(diff.testing)]),
                 msa.names(names(diff.testing)[diff.testing==max(diff.testing)]))
    x
    
    i.prep1 = 12
    i.prep2 = 13
    dimnames(est.secondary.10.3y$estimates)[[2]][c(i.prep1, i.prep2)] 
    diff.prep = est.secondary.10.3y$estimates[-i.total,i.prep2] - est.secondary.10.3y$estimates[-i.total,i.prep1]
    range(diff.prep)
    
    i.supp1 = 10
    i.supp2 = 11
    dimnames(est.secondary.10.3y$estimates)[[2]][c(i.supp1, i.supp2)] 
    diff.supp = est.secondary.10.3y$estimates[-i.total,i.supp2] - est.secondary.10.3y$estimates[-i.total,i.supp1]
    range(diff.supp)
    
    # PrEP vs Supp
    mask = est.secondary.10.3y$estimates[-i.total,i.supp2] > est.secondary.10.3y$estimates[-i.total,i.prep2]
    sum(mask)
    msa.names(names(diff.prep)[mask])
    
    #totals
    est.secondary.10.3y$estimates[dim(est.secondary.10.3y$estimates)[1],c(i.testing1,i.testing2,i.prep1,i.prep2,i.supp1,i.supp2)]

}

##------------------------------------------------------------##
##-- GENERATING AND WRITING ESTIMATES FOR THE MAIN ANALYSIS --##
##------------------------------------------------------------##

get.raw.estimates <- function(dir,
                              msas=TARGET.MSAS,
                              interventions=INTERVENTION.SET,
                              n.sim,
                              verbose=T,
                              year1=2020,
                              year2=2030,
                              outcome=c('incidence','new')[1])
{
    dir = file.path(dir)
    all.arr = sapply(1:length(interventions), function(i){
        int = interventions[[i]]
        if (verbose)
            print(paste0("Reading ", length(msas), " locations for intervention ", i, " of ", length(interventions),
                         ": ", get.intervention.filename(int)))
        sapply(as.character(msas), function(msa){
            filename = file.path(dir, msa, get.simset.filename(location=msa, intervention=int))
            if (file.exists(filename))
            {
                if (verbose)
                    print(paste0(" - ", msa.names(msa)))
                
                load(filename)
                
                if (sum(simset@weights)<n.sim)
                    stop(paste0("The simset for ", msa.names(msa), " on intervention '",
                                get.intervention.name(int), "' does not have ",
                                n.sim, " simulations"))
                
                indices = unlist(sapply(1:simset@n.sim, function(i){
                    rep(i, simset@weights[i])
                }))
                indices = indices[1:n.sim]
                
                if (outcome=='new')
                    outcome.fn = project.absolute.new.diagnoses
                else
                    outcome.fn = project.absolute.incidence
                
                values = sapply(simset@simulations[unique(indices)], outcome.fn, years=c(year1,year2))
                values = values[,indices]
                values
            }
            else
                rep(NA, n.sim*2)
        })
    })
    
    dim.names = list(year=c(year1,year2),
                     sim=1:n.sim,
                     location=msas,
                     intervention=sapply(interventions, get.intervention.code))
    dim(all.arr) = sapply(dim.names, length)
    dimnames(all.arr) = dim.names
    
    attr(all.arr, 'interventions') = interventions
    
    all.arr
}

get.raw.values.one.intervention <- function(dir,
                                            msas=TARGET.MSAS,
                                            intervention=NO.INTERVENTION,
                                            fn,
                                            value.names=NULL,
                                            n.values=length(value.names),
                                            n.sim=NA,
                                            verbose=T,
                                            ...)
{
    if (verbose)
        print(paste0("Reading ", length(msas), " locations for intervention : ",
                 get.intervention.filename(intervention)))
  
    dir = file.path(dir)
    all.arr = sapply(msas, function(msa){
        
        filename = file.path(dir, msa, get.simset.filename(location=msa, intervention=intervention))
        if (file.exists(filename))
        {
            if (verbose)
                print(paste0(" - ", msa.names(msa)))
            
            tryCatch({
                
                load(filename)
                if (is.na(n.sim))
                    n.sim <<- simset@n.sim
                
                if (sum(simset@weights)<n.sim)
                    stop(paste0("The simset for ", msa.names(msa), " on intervention '",
                                get.intervention.name(intervention), "' does not have ",
                                n.sim, " simulations"))
                
                indices = unlist(sapply(1:simset@n.sim, function(i){
                    rep(i, simset@weights[i])
                }))
                indices = indices[1:n.sim]
                
                values = sapply(simset@simulations[unique(indices)], fn, ...)
                values = values[,indices]
                if (is.null(value.names))
                    value.names <<- dimnames(values)[[1]]
                
                print(paste0("...done with ", msa.names(msa), " (", msa, ")"))
                as.numeric(values)
            },
            error = function(e){
                print(paste0("...error reading ", msa.names(msa), " (", msa, ") - skipping..."))
                rep(NaN, n.sim*n.values)
            })

        }
        else
        {
            print(paste0("...", msa.names(msa), " (", msa, ") not yet done - skipping..."))
            rep(NaN, n.sim*n.values)
        }
    })
    
    dim.names = list(value=value.names,
                     sim=1:n.sim,
                     location=msas)
    
    dim(all.arr) = sapply(dim.names, length)
    dimnames(all.arr) = dim.names
    
    attr(all.arr, 'interventions') = intervention
    
    all.arr
}

get.estimates.and.intervals <- function(dir,
                                        msas=TARGET.MSAS,
                                        interventions=INTERVENTION.SET,
                                        n.sim,
                                        verbose=T,
                                        year1=2020,
                                        year2=2030,
                                        summary.stat=mean,
                                        interval.coverage=0.95,
                                        calculate.total=T)
{
    all.arr = get.raw.estimates(dir=dir,
                                msas=msas,
                                interventions=interventions,
                                n.sim=n.sim,
                                verbose=verbose,
                                year1=year1,
                                year2=year2)
    
    do.crunch.estimates.and.intervals(all.arr,
                                      summary.stat=summary.stat,
                                      interval.coverage=interval.coverage,
                                      calculate.total=calculate.total)
}

do.crunch.estimates.and.intervals <- function(all.arr,
                                              summary.stat=mean,
                                              interval.coverage=0.95,
                                              calculate.total=T)
{
    dim.names = dimnames(all.arr)
    
    if (calculate.total)
    {
        dim.names$location = c(dim.names$location, 'Total')
        new.all.arr = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
        new.all.arr[,,dimnames(all.arr)$location,] = all.arr
        new.all.arr[,,'Total',] = apply(all.arr, c(1,2,4), sum, na.rm=T)
        all.arr = new.all.arr
    }
    
    #indexed [sim, msa, intervention]
    rel.change = -(all.arr[2,,,] - all.arr[1,,,]) / all.arr[1,,,]
    dim.names = dim.names[-1]
    dim(rel.change) = sapply(dim.names, length)
    dimnames(rel.change) = dim.names
    
    alpha = (1-interval.coverage)/2
    
    rv=list(estimates=apply(rel.change, 2:3, summary.stat, na.rm=T),
            ci.lower=apply(rel.change, 2:3, quantile, probs=alpha, na.rm=T),
            ci.upper=apply(rel.change, 2:3, quantile, probs=1-alpha, na.rm=T)
    )
    
    dim.names = dim.names[-1]
    dim(rv$estimates) = sapply(dim.names, length)
    dimnames(rv$estimates) = dim.names
    dim(rv$ci.lower) = sapply(dim.names, length)
    dimnames(rv$ci.lower) = dim.names
    dim(rv$ci.upper) = sapply(dim.names, length)
    dimnames(rv$ci.upper) = dim.names
    
    attr(rv, 'interventions') = attr(all.arr, 'interventions')
    attr(rv, 'locations') = dim.names$location
    
    rv
}


##---------------------------------------------------##
##-- BASELINE TESTING, SUPPRESSION, PrEP ESTIMATES --##
##---------------------------------------------------##

get.baseline.estimates <- function(dir='mcmc_runs/full_simsets',
                                   msas=TARGET.MSAS,
                                   year=2020,
                                   risks='msm',
                                   verbose=T)
{
  dir = file.path(dir)
  all.arr = sapply(msas, function(msa){
    filename = file.path(dir, get.full.filename(location=msa))
    if (file.exists(filename))
    {
      if (verbose)
        print(paste0(" - ", msa.names(msa)))
      
      load(filename)
      
      dist = extract.simset.distribution(simset, fn=get.sim.baseline.estimates, year=year, risks=risks)
      cbind(mean=get.means(dist),
            t(get.intervals(dist)))
    }
    else
      matrix(as.numeric(NA), dim=c(3,3))
  })
  
  rv = t(all.arr)
  dim.names = list(location=msas,
                   aspect=c('testing','prep','suppression'),
                   stat=c('mean','lower','upper')
  )
  dim(rv) = sapply(dim.names, length)
  dimnames(rv) = dim.names
  
  rv
}

get.sim.baseline.estimates <- function(sim, year=2020, risks='msm', races=NULL, ages=NULL)
{
  components = attr(sim, 'components')
  c(testing=1/as.numeric(extract.testing.rates(sim, years=year, races=races, ages=ages, risks = risks, use.cdc.categorizations = T)),
    prep=as.numeric(extract.prep.coverage(sim, years=year, races=races, ages=ages,risks=risks, use.cdc.categorizations = T)),#, multiplier = get.prep.indications.estimate(ALL.DATA.MANAGERS$prep, location=attr(sim, 'location')))),
    suppression=as.numeric(extract.suppression(sim, years=year, races=races, ages=ages, risks=risks, use.cdc.categorizations = T))
  )
}


##-------------------------------------------------------------------------##
##-- GETTING ARBITRARY ESTIMATES FOR AN ARBITRARY TABLE OF INTERVENTIONS --##
##-------------------------------------------------------------------------##

#'@param intervention.codes can be an array, table, whatever
#'@param location can be either a scalar value or an object with the same dimension as intervention.codes 
#'@param dir the directory holding the simsets
#'@param summary.stat A function that creates a summary statistic from a vector of outcomes (one from each simulation)
#'@param outcome.fn A function that takes one argument, a simulation, and returns the output desired
get.estimates.for.interventions <- function(intervention.codes,
                                            location,
                                            dir,
                                            summary.stat = mean,
                                            outcome.fn = extract.total.incidence.reduction.20.30)
{
  rv = sapply(1:length(intervention.codes), function(i){
    int.code = intervention.codes[i]
    int = intervention.from.code(int.code)
    if (length(location)==1)
      loc = location
    else
      loc = location[i]
    
    filename = file.path(dir, loc, get.simset.filename(location=loc, intervention=int))
    load(filename)
    
    sim.outcomes = sapply(simset@simulations, outcome.fn)
    sim.outcomes = unlist(sapply(1:simset@n.sim, function(i){
      rep(sim.outcomes[i], simset@weights[i])
    }))
    summary.stat(sim.outcomes)
  })
  
  if (is.null(dim(intervention.codes)))
  {
    if (is.null(names(intervention.codes)))
      names(rv) = intervention.codes
    else
      names(rv) = names(intervention.codes)
  }
  else
  {
    dim(rv) = dim(intervention.codes)
    dimnames(rv) = dimnames(intervention.codes)
  }
  
  rv
}

#'@param intervention.codes can be an array, table, whatever
#'@param location can be either a scalar value or an object with the same dimension as intervention.codes 
#'@param dir the directory holding the simsets
#'@param prob The quantile to return
#'@param outcome.fn A function that takes one argument, a simulation, and returns the output desired
get.quantiles.for.interventions <- function(intervention.codes,
                                            location,
                                            dir,
                                            prob,
                                            outcome.fn = extract.total.incidence.reduction.20.30)
{
  q.fn = function(x){quantile(x, probs=prob)}
  get.estimates.for.interventions(intervention.codes=intervention.codes,
                                  location=location,
                                  dir=dir,
                                  summary.stat = q.fn,
                                  outcome.fn=outcome.fn)
}
