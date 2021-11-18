
source('code/source_code.R')
source('code/covid/prepare_covid_parameters.R')
source('code/covid/prepare_and_run_covid_sims.R')
source('code/covid/run_covid_simset_3.0.R')
source('code/targets/target_msas.R')

source('code/covid/process_covid_results3.R')

msas = TARGET.MSAS[28]#[8 + 8*0:3][-1] 
scenarios = c('base','delayed.hiv.care','rebound.sexual.transmission','rebound.sex.delayed.hiv.care')[c(1,2,4)]
interventions.per.scenario = NA


#END.TIMES.SEXUAL.TRANSMISSION = c(2022)
#END.TIMES.TPS = c(2023)
SRC.DIR = 'mcmc_runs/full_simsets'
DST.DIR = 'mcmc_runs/covid_simsets'

for (msa in msas)
{
    print("---------------------------------------------------------------------------")
    print(paste0("RUNNING COVID SIMULATIONS FOR MSA ", toupper(msa.names(msa))))
    print("---------------------------------------------------------------------------")
    
    full.filename = get.full.filename(location=msa)
    load(file=file.path(SRC.DIR, full.filename))
    base.simset = simset
    
#for testing:
  #  base.simset = thin.simset(base.simset, 200)
   # base.simset = thin.simset(base.simset, 40)

    print("- Preparing simset for simulations")
    set.seed(1234)
    base.simset = prepare.simset.for.interventions(base.simset)
    base.simset = prepare.simset.for.covid(base.simset)
    
    for (scenario in scenarios)
    {
        for (int.name in interventions.per.scenario)
        {
            if (is.na(int.name))
            {
                print(paste0("- Running sims for '", scenario, "' scenario, with no secondary intervention"))
                int = NULL
            }
            else
            {
                print(paste0("- Running sims for '", scenario, "' scenario, with secondary intervention '", int.name, "'"))
                int = parse.secondary.intervention(int.name)
            }
            
            simset = run.covid.for.simset(base.simset, scenario, intervention=int)
    
            if (!dir.exists(file.path(DST.DIR, msa)))
                dir.create(file.path(DST.DIR, msa))
            save(simset, file=file.path(DST.DIR, msa, get.covid.simset.name(msa, scenario=scenario, intervention.name=int.name)))
            
        }
    }
    
    print("Processing Outcomes")
    
    outcomes.sub.arr = process.covid.outcomes(locations=msa)
    save(outcomes.sub.arr, file=paste0('results/covid/subs/covid_4.1_results_', msa, '.Rdata'))
    
    print(paste0("Done with ", msa))
}

if (1==2)
{
    plot.calibration(list(simset),plot.individual.simset.sims = F, data.types='testing', years=2020:2030)
    plot.calibration(list(simset.noint, simset.int),plot.individual.simset.sims = F,
                     data.types='incidence', facet.by=NULL, years=2020:2030)
}

