
source('code/source_code.R')
source('code/processing/pretty_table/make_systematic_table.R')
#source('code/targets/target_msas.R')
source('code/interventions/synthesize_interventions.R')
source('code/applications/expanded_continuum/melissa_croi_interventions_2022_v2.R')


if (1==2)
{
    load('mcmc_runs/quick_simsets/ex1.0_12580_full.Rdata')
    # simset = subset.simset(simset, 1:3)
    
    run.systematic.interventions(simset,
                                 dst.dir = 'mcmc_runs/quick_simsets',
                                 interventions = MELISSA.CROI.INTERVENTIONS.2022,
                                 save.baseline.and.seed = F)
    
}

if (1==2)
{
    locations = c(BALTIMORE.MSA)
    
    interventions = c('noint', 'ybhmsm.p10', 'ybhmsm.p25', 'ybhmsm.l90', 'ybhmsm.l95', 'ybhmsm.r90', 'ybhmsm.r95', 'ybhmsm.as90', 'ybhmsm.as95', 'ybhmsm.c.p.low', 'ybhmsm.c.p.high',
                      'msmidu.p10', 'msmidu.p25', 'msmidu.l90', 'msmidu.l95', 'msmidu.r90', 'msmidu.r95', 'msmidu.as90', 'msmidu.as95','msmidu.c.p.low', 'msmidu.c.p.high',
                      'wholepop.p10', 'wholepop.p25', 'wholepop.l90', 'wholepop.l95', 'wholepop.r90', 'wholepop.r95', 'wholepop.as90', 'wholepop.as95', 'wholepop.c.p.low','wholepop.c.p.high')
    
    sims = paste0("sim",1:length(simset@simulations))
    
    test.result = sapply(locations, function(location){
        sapply(interventions, function(intervention){
            
            file = paste0("mcmc_runs/quick_simsets/",location,"/ex1.0_",location,"_",intervention,".Rdata")
            load(file)
            
            inc.reduction = sapply(simset@simulations, function(x){
                
                inc.2030 = extract.incidence(x, years = 2030)
                inc.2020 = extract.incidence(x, years = 2020)
                
                (inc.2020-inc.2030)/inc.2020
                
            })
        })
    })
    
    dim.names = list(sim = sims,
                     intervention = interventions,
                     location = locations)
    
    test.result = array(c(test.result),
                        dim = sapply(dim.names,length),
                        dimnames = dim.names)
     
    summary.result = apply(test.result,c("intervention","location"),quantile,probs=c(.025,.5,.975))
     
     # intervention.code.table = matrix(c('noint', 'ybhmsm.p10', 'ybhmsm.p25', 'ybhmsm.l90', 'ybhmsm.l95', 'ybhmsm.r90', 'ybhmsm.r95', 'ybhmsm.as90', 'ybhmsm.as95', 'ybhmsm.c.p.low', 'ybhmsm.c.p.high',
     #                                    'noint', 'msmidu.p10', 'msmidu.p25', 'msmidu.l90', 'msmidu.l95', 'msmidu.r90', 'msmidu.r95', 'msmidu.as90', 'msmidu.as95','msmidu.c.p.low', 'msmidu.c.p.high',
     #                                    'noint', 'wholepop.p10', 'wholepop.p25', 'wholepop.l90', 'wholepop.l95', 'wholepop.r90', 'wholepop.r95', 'wholepop.as90', 'wholepop.as95', 'wholepop.c.p.low','wholepop.c.p.high'),
     #                                  nrow = 3, ncol=11, byrow = T)
     # 
     # tab = get.estimates.for.interventions(intervention.codes=intervention.code.table,
     #                                       location=location,
     #                                       dir='mcmc_runs/quick_simsets')
     
    
    tab = apply(summary.result,c("location","intervention"),function(x){
        
        paste0()
        
        # this is from other paper: 
        # tab.1 = c(paste0(round(100*age.summary.1[2,],1),"% [",
        #                  round(100*age.summary.1[1,],1),"-",
        #                  round(100*age.summary.1[3,],1),"]"),
        #           paste0(round(100*age.summary.2[2,],1),"% [",
        #                  round(100*age.summary.2[1,],1),"-",
        #                  round(100*age.summary.2[3,],1),"]"),
        #           paste0(round(100*age.summary.3[2,],1),"% [",
        #                  round(100*age.summary.3[1,],1),"-",
        #                  round(100*age.summary.3[3,],1),"]"))
    })
     

    
}

#write.csv(tab, file = "Melissa_CROI_2022.csv")

