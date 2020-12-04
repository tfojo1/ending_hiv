x = read.csv('results/melissa_croi_v5.csv', header = F)
x[]=as.numeric(substr(as.matrix(x), 1,nchar(as.matrix(x))-1))/100
source('code/process_results/make_systematic_table.R')
source('code/source_code.R')

write.shaded.table(x, file='results/melissa_croi_v5.xlsx', 
                   below.threshold.min.color= 'firebrick2',
                   below.threshold.max.color = 'green1')

col2rgb('firebrick2')
col2rgb('green1')

#For extracting suppression and PrEP rates
if (1==2)
{
    load('mcmc_runs/melissa_croi_simsets/12060/1.0_12060_full.Rdata')
    load('mcmc_runs/melissa_croi_simsets/12580/1.0_12580_full.Rdata')
    load('mcmc_runs/melissa_croi_simsets/31080/1.0_31080_full.Rdata')
    load('mcmc_runs/melissa_croi_simsets/35620/1.0_35620_full.Rdata')
    
    
    msas = list.files('mcmc_runs/melissa_croi_simsets/')
    
    dim.names = list(msa=msas, stat=c('mean','ci.lower','ci.upper'))
    prep.tab = supp.tab = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    for (msa in msas)
    {
        print(msa)
        file = file.path('mcmc_runs/melissa_croi_simsets/', msa, paste0('1.0_', msa, "_full.Rdata"))
        load(file)
        prep.indications = get.prep.indications.estimate()
        prep.dist = extract.simset.distribution(simset, fn=function(sim){
            c1 = attr(sim, 'components')
            prep = c1$prep.rates.and.times$rates[[(1:length(c1$prep.rates.and.times$rates))[c1$prep.rates.and.times$times==2020]]]
            pop = extract.population.subset(sim, years=2020, keep.dimensions = c('age','race','subpopulation','sex','risk'))
            
            numerators = prep[,,1,,]*pop[,,1,,]/prep.indications
            sum(numerators[,,'msm',]) / sum(pop[,,,'msm',])*10
        })
        get.means(prep.dist) * 100
        supp.dist = extract.simset.distribution(simset, fn=function(sim){
            extract.suppression(sim, years=2020, sexes='msm')
        })
        get.means(supp.dist) * 100
        
        prep.tab[msa, 'mean'] = get.means(prep.dist)
        prep.tab[msa, c('ci.lower','ci.upper')] = get.intervals(prep.dist)
        
        supp.tab[msa, 'mean'] = get.means(supp.dist)
        supp.tab[msa, c('ci.lower','ci.upper')] = get.intervals(supp.dist)
    }
    
    write.csv(prep.tab, file='results/melissa_croi_baseline_prep.csv')
    write.csv(supp.tab, file='results/melissa_croi_baseline_suppression.csv')
}