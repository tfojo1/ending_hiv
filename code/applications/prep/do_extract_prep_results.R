
source('code/source_code.R')
source('code/processing/outcome_formatting.R')
source('code/processing/generalized_extract_results.R')

total.msm.results = generalized.extract.results(dir=file.path(SYSTEMATIC.ROOT.DIR, 'prep_simsets'),
                                                locations=TARGET.MSAS,
                                                interventions=STANDARD.LAI.PREP.INTERVENTION.CODES,
                                                sexes='msm',
                                                outcomes=c('incidence',
                                                           'prevalence',
                                                           'new',
                                                           'suppression',
                                                           'prep',
                                                           'diagnosed',
                                                           'population'),
                                                keep.dimensions='year',#c('year','race'),
                                                years=2015:2030)

save(total.msm.results, file=paste0('results/prep/total.msm.results_', Sys.Date(), '.Rdata'))


msm.results.by.race = generalized.extract.results(dir=file.path(SYSTEMATIC.ROOT.DIR, 'prep_simsets'),
                                                locations=TARGET.MSAS,
                                                interventions=STANDARD.LAI.PREP.INTERVENTION.CODES,
                                                sexes='msm',
                                                outcomes=c('incidence',
                                                           'prevalence',
                                                           'new',
                                                           'suppression',
                                                           'prep',
                                                           'diagnosed',
                                                           'population'),
                                                keep.dimensions=c('year','race'),
                                                years=2015:2030)

save(msm.results.by.race, file=paste0('results/prep/msm.results.by.race_', Sys.Date(), '.Rdata'))


load('Q:Ending_HIV/mcmc_runs/prep_simsets/12580/1.0_12580_msm.baseline.oral_23_27.Rdata')

prep.parameters = simset@parameters[,c('oral.prep.rr','inj.vs.oral.hr','oral.prep.persistence','inj.vs.oral.discontinuation.rr')]
save(prep.parameters, file=paste0('results/prep/prep.parameters_', Sys.Date(), '.Rdata'))



# quick checking
if (1==2)
{
    inc.per.pop = msm.results.by.race[,,,'incidence',,] / msm.results.by.race[,,,'population',,]
    
    b.vs.o = inc.per.pop[,'black',,,] / inc.per.pop[,'other',,,]
    h.vs.o = inc.per.pop[,'hispanic',,,] / inc.per.pop[,'other',,,]
    
    zb=cbind(colMeans(b.vs.o['2020',,,1]),colMeans(b.vs.o['2030',,,]));dimnames(zb)[[2]]=NULL;round(zb,1)
    zh=cbind(colMeans(h.vs.o['2020',,,1]),colMeans(h.vs.o['2030',,,]));dimnames(zh)[[2]]=NULL;round(zh,1)
}
