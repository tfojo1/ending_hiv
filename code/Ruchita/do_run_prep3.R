


source('code/source_code.R')
source('code/Ruchita/create_prep_interventions4.R')
source('code/Ruchita/systematic_prep_2.R')
source('code/Ruchita/create_prep_study_comparisons.R')

base.index = 6
n.chunks = 6
n.per = ceiling(length(TARGET.MSAS) / n.chunks)
indices = intersect((1:n.per-1)*n.chunks + base.index, 1:length(TARGET.MSAS))
MSAs = TARGET.MSAS[indices]

INTERVENTION.CODES.TO.RUN = c(
    paste0(c(
        'msm.baseline.oral',
        'msm.baseline.lai',
        'msm.baseline.oral.plus.10.oral',
        'msm.baseline.oral.plus.10.lai',
        'msm.baseline.lai.plus.10.lai',
        'msm.baseline.oral.plus.25.oral',
        'msm.baseline.oral.plus.25.lai',
        'msm.baseline.lai.plus.25.lai'
    ), '_23_27')
)

print("JUST DOING COMPARISONS")
INTERVENTION.CODES.TO.RUN = 'baseline.maloney' #sapply(COMPARISON.INTERVENTIONS.TO.RUN, get.intervention.code)
MSAs = ATLANTA.MSA


run.prep.simulations(msas = MSAs,
                     intervention.codes = INTERVENTION.CODES.TO.RUN,
                     dst.dir = 'Q:/Ending_HIV/mcmc_runs/prep_simsets',
                     src.dir = 'Q:/Ending_HIV/mcmc_runs/full_simsets')

sub.results = aggregate.raw.prep.results(msas=MSAs,
                                         intervention.codes = INTERVENTION.CODES.TO.RUN,
                                         years=2020:2030,
                                         dir='Q:/Ending_HIV/mcmc_runs/prep_simsets',
                                         calculate.total=F)
save(sub.results, file=paste0('results/prep/subs/prep_sub_', base.index, '.Rdata'))

if (1==2)
{
    prep.results = aggregate.raw.prep.results(msas=TARGET.MSAS,
                                              intervention.codes = INTERVENTION.CODES.TO.RUN,
                                              years=2020:2030,
                                              dir='Q:/Ending_HIV/mcmc_runs/prep_simsets',
                                              calculate.total=F)
    
    
    prep.results = NULL
    for (i in 1:6)
    {
       load(file=paste0('results/prep/subs/prep_sub_', i, '.Rdata'))
       if (is.null(prep.results))
       {
          dim.names = dimnames(sub.results)
          dim.names$location = TARGET.MSAS
          prep.results = array(0, dim=sapply(dim.names, length), dimnames = dim.names)
       }
       
       prep.results[,,dimnames(sub.results)$location,] = sub.results
    }
    
    
    
    
    load('Q:/Ending_HIV/mcmc_runs/prep_simsets/12580/1.0_12580_msm.baseline.oral_23_27.Rdata')
    parameters = simset@parameters[,simset@n.parameters-0:3]
    
    save(prep.results, parameters, file=paste0('results/prep/baseline_prep_and_parameters_1K_',Sys.Date(),'.Rdata'))
}