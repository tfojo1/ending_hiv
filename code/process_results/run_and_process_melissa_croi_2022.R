
source('code/source_code.R')
source('code/process_results/make_systematic_table.R')
source('code/targets/target_msas.R')
source('code/interventions/melissa_croi_interventions_2022.R')
source('code/interventions/synthesize_interventions.R')

if (1==2)
{
    location = BALTIMORE.MSA
    load(file.path('mcmc_runs', "baltimore_initial_simset_v2.Rdata"))
    
    run.systematic.interventions(simset,
                                 dst.dir = 'mcmc_runs/quick_simsets',
                                 interventions = MELISSA.CROI.INTERVENTIONS.2022,
                                 save.baseline.and.seed = F)
    
}

if (1==2)
{
    
    location = BALTIMORE.MSA
    
    intervention.code.table = matrix(c('noint', 'ybhmsm.p10', 'ybhmsm.p25', 'ybhmsm.l90', 'ybhmsm.l95', 'ybhmsm.r80', 'ybhmsm.r90', 'ybhmsm.as90', 'ybhmsm.as95', 'ybhmsm.c.low', 'ybhmsm.c.high',
                                       'noint', 'msmidu.p10', 'msmidu.p25', 'msmidu.l90', 'msmidu.l95', 'msmidu.r80', 'msmidu.r90', 'msmidu.as90', 'msmidu.as95','msmidu.c.low', 'msmidu.c.high',
                                       'noint', 'wholepop.p10', 'wholepop.p25', 'wholepop.l90', 'wholepop.l95', 'wholepop.r80', 'wholepop.r90', 'wholepop.as90', 'wholepop.as95', 'wholepop.c.low','wholepop.c.high'),
                                     nrow = 3, ncol=11, byrow = T)
    
    tab = get.estimates.for.interventions(intervention.codes=intervention.code.table,
                                          location=location,
                                          dir='mcmc_runs/quick_simsets')
    
    
}

write.csv(tab, file = "Melissa_CROI_2022.csv")

