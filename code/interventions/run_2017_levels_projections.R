source('code/source_code.R')
source('code/targets/target_msas.R')
source('code/interventions/synthesize_interventions.R')

if (1==2)
{
    msas = c(ATLANTA.MSA,
             BALTIMORE.MSA,
             LA.MSA,
             MIAMI.MSA,
             NYC.MSA,
             SEATTLE.MSA)
    
    simsets.2017 = lapply(msas, function(msa){
        
        print(paste0("Running ", msa.names(msa)))
        load(paste0('mcmc_runs/systematic_initial/',msa,'.Rdata'))
        
        simset = extract.simset(mcmc, additional.burn=520, additional.thin=6)
        simset@n.sim
        msa.names(attr(simset@simulations[[1]], 'location'))
        
        run.2017.levels.projections(simset)
    })
    
    tab=sapply(simsets.2017, get.simset.incidence.reduction)
    dimnames(tab)[[2]] = msas
    round(100*tab)
}

run.2017.levels.projections <- function(simset,
                                        run.to.year=2030)
{
    simset = prepare.simset.for.interventions(simset)
    simset@parameters[,c('testing.gains.end.by.year',
                              'prep.gains.end.by.year',
                              'suppression.gains.end.by.year')] = 2017
    attr(simset, 'run.from.year') = 2017
    
    run.simset.intervention(simset, 
                            intervention=NO.INTERVENTION,
                            run.from.year=2017,
                            run.to.year = run.to.year, 
                            keep.years = 2017:2030)
}
