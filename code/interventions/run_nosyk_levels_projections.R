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
    
    simsets.nosyk = lapply(msas, function(msa){
        
        print(paste0("Running ", msa.names(msa)))
#        load(paste0('mcmc_runs/systematic_initial/',msa,'.Rdata'))
#        simset = extract.simset(mcmc, additional.burn=520, additional.thin=6)
 #       simset@n.sim
 
        load(file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets', paste0("1.0_", msa, "_full.Rdata")))
     #   print(paste0("Running on ", msa.names(attr(simset@simulations[[1]], 'location'))))
        
        run.nosyk.projections(simset)
    })
    
    save(simsets.nosyk, file='results/simsets_nosyk.Rdata')
    
    tab=t(sapply(simsets.nosyk, get.simset.incidence.reduction))
    dimnames(tab)[[1]] = msas
    round(100*tab)
    
    tab2 = cbind(Nosyk=c(Atlanta=0,
                         Baltimore=0,
                         LA=0,
                         MIAMI=0,
                         NYC=(24-19)/24,
                         Seattle=(13-11)/13),
                 JHEEM=tab[,1])
}

run.nosyk.projections <- function(simset,
                                  run.to.year=2030)
{
    simset = prepare.simset.for.interventions(simset)
    simset@parameters[,'prep.gains.end.by.year'] = 2017
    simset@parameters[,c('testing.gains.end.by.year',
                         'suppression.gains.end.by.year')] = 2015
    attr(simset, 'run.from.year') = 2015
    
    run.simset.intervention(simset, 
                            intervention=NO.INTERVENTION,
                            run.from.year=2015,
                            run.to.year = run.to.year, 
                            keep.years = 2015:2030)
}
