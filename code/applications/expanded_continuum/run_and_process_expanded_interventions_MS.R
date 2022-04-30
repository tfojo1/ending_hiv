source('code/source_code.R')
source('code/applications/expanded_continuum/expanded_continuum_interventions_MS.R')
source('code/processing/pretty_table/make_pretty_table.R')
# source('code/targets/target_msas.R')
# source('code/interventions/synthesize_interventions.R')
# source('code/process_results/make_systematic_table.R') 


if (1==2)
{
    location = BALTIMORE.MSA
    # load(file.path('mcmc_runs', "baltimore_initial_simset_v2.Rdata")) - replace with new data 
    
    run.systematic.interventions(simset,
                                 dst.dir = 'mcmc_runs/quick_simsets',
                                 interventions = EXPANDED.CONTINUUM.INTERVENTIONS,
                                 save.baseline.and.seed = F) # can't remember if I should change this option 
    
}

if (1==2)
{
    
    location = BALTIMORE.MSA
    
    intervention.code.table = matrix(c('noint', 'ybhmsm.p10', 'ybhmsm.p25', 'ybhmsm.l90', 'ybhmsm.l95', 'ybhmsm.r90', 'ybhmsm.r95', 'ybhmsm.as90', 'ybhmsm.as95', 'ybhmsm.c.low', 'ybhmsm.c.high', 'ybhmsm.c.p.low', 'ybhmsm.c.p.high',
                                       'noint', 'msmidu.p10', 'msmidu.p25', 'msmidu.l90', 'msmidu.l95', 'msmidu.r90', 'msmidu.r95', 'msmidu.as90', 'msmidu.as95','msmidu.c.low', 'msmidu.c.high', 'msmidu.c.p.low', 'msmidu.c.p.high',
                                       'noint', 'wholepop.p10', 'wholepop.p25', 'wholepop.l90', 'wholepop.l95', 'wholepop.r90', 'wholepop.r95', 'wholepop.as90', 'wholepop.as95', 'wholepop.c.low','wholepop.c.high', 'wholepop.c.p.low','wholepop.c.p.high'),
                                     nrow = 3, ncol=13, byrow = T)
    
    # tab = get.estimates.for.interventions(intervention.codes=intervention.code.table, # need new function - I think this is old 
    #                                       location=location,
    #                                       dir='mcmc_runs/quick_simsets')
    
    
}

write.csv(tab, file = "expanded_continuum_interventions.csv")

