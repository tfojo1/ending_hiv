source('code/source_code.R')
source('code/targets/target_msas.R')
source('code/interventions/synthesize_interventions.R')

##-- RUN INTERVENTIONS --##
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
        load(file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets', paste0("1.0_", msa, "_full.Rdata")))
        run.nosyk.projections(simset)
    })
    
    save(simsets.nosyk, file='results/simsets_nosyk.Rdata')
}

cities = c('Atlanta',
           'Baltimore',
           'LA',
           'Miami',
           'NYC',
           'Seattle')

if (1==2)
{
    load('results/simsets_nosyk.Rdata')
    
    nosyk.2030.incidence = c(Atlanta=40,
                             Baltimore=30,
                             LA=36,
                             Miami=112,
                             NYC=19,
                             Seattle=11)
    nosyk.2020.incidence = c(Atlanta=40,
                             Baltimore=30,
                             LA=36,
                             Miami=112,
                             NYC=24,
                             Seattle=13)
    
    jheem.2030.incidence.dists = lapply(simsets.nosyk, get.simset.incidence.dist, 
                                        year=2030, per.population=100000, include.hiv.positive.in.denominator=T)
    jheem.2030.incidence = sapply(jheem.2030.incidence.dists, get.means)
    
    jheem.2020.incidence.dists = lapply(simsets.nosyk, get.simset.incidence.dist, 
                                        year=2020, per.population=100000, include.hiv.positive.in.denominator=T)
    jheem.2020.incidence = sapply(jheem.2020.incidence.dists, get.means)
    
    df.2030 = data.frame(Nosyk=nosyk.2030.incidence,
                         JHEEM=round(jheem.2030.incidence));df.2030
    df.2020 = data.frame(Nosyk=nosyk.2020.incidence,
                         JHEEM=round(jheem.2020.incidence));df.2020
    
    
    nosyk.incidence.reduction = (nosyk.2020.incidence-nosyk.2030.incidence)/nosyk.2020.incidence
    nosyk.incidence.reduction['Atlanta'] = -.052
    nosyk.incidence.reduction['NYC'] = .197
    
    jheem.incidence.reduction.dists = lapply(simsets.nosyk, get.simset.incidence.reduction, per.population=100000)
    jheem.incidence.reduction = sapply(jheem.incidence.reduction.dists, function(x){x[1]})
    jheem.incidence.reduction = sapply(jheem.incidence.reduction.dists, function(x){
        x = -round(100*x)
        x[x>0] = paste0('+',x[x>0])
        paste0(x[1], '% [',
               x[3], ' to ',
               x[2], '%]')
    })
    
    
    jheem.incidence.2025.reduction.dists = lapply(simsets.nosyk, get.simset.incidence.reduction, year2=2025, per.population=100000)
    jheem.incidence.2025.reduction = sapply(jheem.incidence.2025.reduction.dists, function(x){x[1]})
    jheem.incidence.2025.reduction = sapply(jheem.incidence.2025.reduction.dists, function(x){
        x = -round(100*x)
        x[x>0] = paste0('+',x[x>0])
        paste0(x[1], '% [',
               x[3], ' to ',
               x[2], '%]')
    })
    
    df.reduction = data.frame(Nosyk=nosyk.incidence.reduction,
                         JHEEM=round(jheem.incidence.reduction,2));df.reduction
    
    write.csv(df.reduction, file='../Manuscripts/manuscript_1/tables/nosyk_comparison.csv')
    
  #  df.jheem = -round(100*cbind('2030'=jheem.incidence.reduction, '2025'=jheem.incidence.2025.reduction))
    df.jheem = cbind('2030'=jheem.incidence.reduction, '2025'=jheem.incidence.2025.reduction))
    dimnames(df.jheem)[[1]]=cities
    
    write.csv(df.jheem, file='../Manuscripts/manuscript_1/appendix_1/scratch/jheem_for_nosyk.csv')
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
