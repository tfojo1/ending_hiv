

source('code/source_code.R')
source('code/covid/prepare_and_run_covid_sims.R')
source('code/targets/target_msas.R')

COVID.MSAS = c(ATLANTA.MSA, BALTIMORE.MSA, MIAMI.MSA, NYC.MSA)
DIR = 'mcmc_runs/covid_simsets'

#load it up
if (1==2)
{
    noint.sims = lapply(file.path('mcmc_runs/full_simsets', COVID.MSAS, paste0('1.0_', COVID.MSAS, '_noint.Rdata')), function(file){
        get(load(file))
    })

    base.sims = lapply(file.path(DIR, paste0("covid_3.0_", COVID.MSAS, "_base.Rdata")), function(file){
        get(load(file))
    })
    
    delayed.hiv.care.sims = lapply(file.path(DIR, paste0("covid_3.0_", COVID.MSAS, "_delayed.hiv.care.Rdata")), function(file){
        get(load(file))
    })
    
    increased.sex.sims = lapply(file.path(DIR, paste0("covid_3.0_", COVID.MSAS, "_increased.sexual.transmission.Rdata")), function(file){
        get(load(file))
    })

    
    #sims.2022 = lapply(file.path(DIR, paste0('1.0_', COVID.MSAS, '_COVID.to.2022.Rdata')), function(file){
    #    get(load(file))
    #})              
    #sims.2022.2023 = lapply(file.path(DIR, paste0('1.0_', COVID.MSAS, '_COVID.to.2022-2023.Rdata')), function(file){
    #    get(load(file))
    #})               
    #sims.2023 = lapply(file.path(DIR, paste0('1.0_', COVID.MSAS, '_COVID.to.2023.Rdata')), function(file){
    #    get(load(file))
    #})   

}

#tables
if (1==2)
{
    inc.noint.by2021 = get.incidences(noint.sims, year2=2021)
    inc.2022.by2021 = get.incidences(sims.2022, year2=2021)
    inc.2022.2023.by2021 = get.incidences(sims.2022.2023, year2=2021)
    
    
    inc.noint.by2025 = get.incidences(noint.sims, year2=2025)
    inc.2022.by2025 = get.incidences(sims.2022, year2=2025)
    inc.2022.2023.by2025 = get.incidences(sims.2022.2023, year2=2025)
    
    
        
    df.2022 = data.frame(MSA=c(unlist(msa.names(COVID.MSAS)), 'Total'),
                            delta_2021 = summarize.deltas(inc.2022.by2021, inc.noint.by2021),
                            delta_2025 = summarize.deltas(inc.2022.by2025, inc.noint.by2025))
    
    
    
    df.2022.2023 = data.frame(MSA=c(unlist(msa.names(COVID.MSAS)), 'Total'),
                         delta_2021 = summarize.deltas(inc.2022.2023.by2021, inc.noint.by2021),
                         delta_2025 = summarize.deltas(inc.2022.2023.by2025, inc.noint.by2025))
    
    

    df.2022.vs.2023 = data.frame(MSA=c(unlist(msa.names(COVID.MSAS)), 'Total'),
                                 delta_2021 = summarize.deltas(inc.2022.2023.by2021, inc.2022.by2021),
                                 delta_2025 = summarize.deltas(inc.2022.2023.by2025, inc.2022.by2025))
    
    write.csv(df.2022, file='../presentations/images_covid/df_2022.csv')
    write.csv(df.2022.vs.2023, file='../presentations/images_covid/df_2022_vs_2023.csv')
}

summarize.deltas <- function(v1, v2,
                             interval.coverage=0.95,
                             digits=0)
{
    delta = v1 - v2
    
    mean.delta = apply(delta, 2, mean)
    
    alpha = (1-interval.coverage)/2
    ci.lower = apply(delta, 2, quantile, probs=alpha)
    ci.upper = apply(delta, 2, quantile, probs=1-alpha)
    
    
    total.delta = rowSums(delta)
    
    mean.delta = c(mean.delta, total=mean(total.delta))
    ci.lower = c(ci.lower, total = quantile(total.delta, probs=alpha))
    ci.upper = c(ci.upper, total = quantile(total.delta, probs=1-alpha))
    
    paste0(format(mean.delta, digits=digits, nsmall=digits, big.mark=','),
           " [",
           format(ci.lower, digits=digits, nsmall=digits, big.mark=','),
           " to ",
           format(ci.upper, digits=digits, nsmall=digits, big.mark=','),
           "]")
}

get.incidences <- function(simsets,
                           year1=2020, year2=2021,
                           mask=T)
{
    sapply(simsets, function(simset){
        sapply(simset@simulations[mask], function(sim){
            extract.incidence(sim, years=year1:year2, keep.dimensions=character(), per.population = NA)
        })   
    })
}
