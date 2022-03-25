
source('code/source_code.R')
source('code/processing/pretty_table/make_systematic_table.R')
source('code/targets/target_msas.R')
source('code/interventions/melissa_croi_interventions_2022_v2.R')
source('code/interventions/synthesize_interventions.R')

if (1==2)
{
    location = BALTIMORE.MSA
    load(file.path('mcmc_runs', "baltimore_initial_simset_v2.Rdata"))
    
    #old version
    run.systematic.interventions(simset,
                                 dst.dir = 'mcmc_runs/quick_simsets',
                                 interventions = MELISSA.CROI.INTERVENTIONS.2022,
                                 save.baseline.and.seed = F)
    
    #new version
    run.systematic.interventions(simset, 
                                 interventions = MELISSA.CROI.INTERVENTIONS.2022,
                                 dst.dir=file.path('mcmc_runs/visualization_simsets', attr(simset@simulations[[1]], 'location')),
                                 overwrite = T)
    
}

if (1==2)
{
    
    location = BALTIMORE.MSA
    
    intervention.code.table = matrix(c('noint', 'ybhmsm.p10', 'ybhmsm.p25', 'ybhmsm.l90', 'ybhmsm.l95', 'ybhmsm.r90', 'ybhmsm.r95', 'ybhmsm.as90', 'ybhmsm.as95', 'ybhmsm.c.p.low', 'ybhmsm.c.p.high',
                                       'noint', 'msmidu.p10', 'msmidu.p25', 'msmidu.l90', 'msmidu.l95', 'msmidu.r90', 'msmidu.r95', 'msmidu.as90', 'msmidu.as95','msmidu.c.p.low', 'msmidu.c.p.high',
                                       'noint', 'wholepop.p10', 'wholepop.p25', 'wholepop.l90', 'wholepop.l95', 'wholepop.r90', 'wholepop.r95', 'wholepop.as90', 'wholepop.as95', 'wholepop.c.p.low','wholepop.c.p.high'),
                                     nrow = 3, ncol=11, byrow = T)
    
    tab = get.estimates.for.interventions(intervention.codes=intervention.code.table,
                                          location=location,
                                          dir='mcmc_runs/visualization_simsets')
    
    
}

write.csv(tab, file = "Melissa_CROI_2022.csv")

simplot(simset, data.types = c('incidence'))