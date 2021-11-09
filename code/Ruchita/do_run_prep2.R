


source('code/source_code.R')
source('code/Ruchita/create_prep_interventions2.R')
source('code/Ruchita/systematic_prep_2.R')

MSAs = TARGET.MSAS[4 + (0:7)*4]

INTERVENTION.CODES.TO.RUN = c(
    'baseline.oral.variable.efficacy',
    paste0(c(
        'msm.oral.10.uptake',
        'msm.inj.10.uptake',
        'msm.combined.10.uptake',
        'msm.oral.25.uptake',
        'msm.inj.25.uptake',
        'msm.combined.25.uptake',
        'msm.oral.35.uptake',
        'msm.inj.35.uptake',
        'msm.combined.35.uptake'
    ), '_23_27')
)
#INTERVENTION.CODES.TO.RUN = INTERVENTION.CODES.TO.RUN[8:10]


run.prep.simulations(msas = MSAs,
                     intervention.codes = INTERVENTION.CODES.TO.RUN,
                     dst.dir = 'Q:/Ending_HIV/mcmc_runs/prep_simsets',
                     src.dir = 'Q:/Ending_HIV/mcmc_runs/full_simsets')

if (1==2)
{
    prep.results = aggregate.raw.prep.results(msas=TARGET.MSAS,
                                              intervention.codes = INTERVENTION.CODES.TO.RUN,
                                              years=2020:2030,
                                              dir='Q:/Ending_HIV/mcmc_runs/prep_simsets',
                                              calculate.total=F)
    
    
    save(prep.results, file='results/prep/raw_prep_results_1K.Rdata')
    
    baseline.prep = aggregate.prep.coverage(msas=TARGET.MSAS,
                                            intervention.codes = INTERVENTION.CODES.TO.RUN[1],
                                            years=2020:2030,
                                            dir='Q:/Ending_HIV/mcmc_runs/prep_simsets')
    
    load('Q:/Ending_HIV/mcmc_runs/prep_simsets/12580/1.0_12580_msm.oral.25.uptake_23_27.Rdata')
    parameters = simset@parameters[,simset@n.parameters-0:3]
    
    save(baseline.prep, parameters, file=paste0('results/prep/baseline_prep_and_parameters_1K_',Sys.Date(),'.Rdata'))
}