

# MSAs are rows; columns are select interventions: PrEP 10/25; linkage 90/95; retention 90/95; suppression 90/95; combined low/high
# one table each for YBHMSM, MSM/IDU, whole pop
create.intervention.table.melissa = function(n.sim,
                                             target.population){
    files = list.files('mcmc_runs/quick_simsets')
    files = files[grepl("ex",files)]
    locations = substr(files,7,11)
    # locations = locations[1:n.locations]
    
    # for now, removing 16980
    locations = locations[locations!="16980"]
    
    if(target.population=="ybhmsm"){
        interventions = c('noint', 'ybhmsm.p10', 'ybhmsm.p25', 
                          'ybhmsm.l90', 'ybhmsm.l95', 
                          'ybhmsm.r90', 'ybhmsm.r95', 
                          'ybhmsm.as90', 'ybhmsm.as95', 
                          'ybhmsm.c.p.low', 'ybhmsm.c.p.high')
        
    } else if(target.population=="msmidu"){
        interventions = c('noint','msmidu.p10', 'msmidu.p25', 
                          'msmidu.l90', 'msmidu.l95', 
                          'msmidu.r90', 'msmidu.r95', 
                          'msmidu.as90', 'msmidu.as95',
                          'msmidu.c.p.low', 'msmidu.c.p.high')
    } else if(target.population=="wholepop"){
        interventions = c('noint', 'wholepop.p10', 'wholepop.p25',
                          'wholepop.l90', 'wholepop.l95',
                          'wholepop.r90', 'wholepop.r95',
                          'wholepop.as90', 'wholepop.as95',
                          'wholepop.c.p.low','wholepop.c.p.high')
    } else
        stop("invalid target population (ybhmsm, msmidu, or wholepop)")
    
    test.result = sapply(locations, function(location){
        sapply(interventions, function(intervention){
            
            file = paste0("mcmc_runs/quick_simsets/",location,"/ex1.0_",location,"_",intervention,".Rdata")
            
            # print(paste0("loading file ",location," ",intervention))
            load(file)
            
            inc.reduction = sapply(simset@simulations, function(x){
                
                inc.2030 = extract.incidence(x, years = 2030)
                inc.2020 = extract.incidence(x, years = 2020)
                
                (inc.2020-inc.2030)/inc.2020
                
            })
        })
    })
    
    sims = paste0("sim",1:n.sim)
    
    dim.names = list(sim = sims,
                     intervention = interventions,
                     location = locations)
    
    test.result = array(unlist(test.result),
                        dim = sapply(dim.names,length),
                        dimnames = dim.names)
    
    summary.result = apply(test.result,c("intervention","location"),quantile,probs=c(.025,.5,.975))
    
    tab = apply(summary.result,c("location","intervention"),function(x){
        
        paste0(round(100*x[2]),"% [",
               round(100*x[1]),"-",
               round(100*x[3]),"]")
    })
    
    location.names = sapply(locations, function(x){
        names(TARGET.MSAS)[TARGET.MSAS==x]
    })
    
    dimnames(tab)[[1]] = location.names
    
    tab
    
}


tab.1 = create.intervention.table.melissa(n.sim = N.SIM,
                                          target.population = "ybhmsm")
tab.2 = create.intervention.table.melissa(n.sim = N.SIM,
                                          target.population = "msmidu")
tab.3 = create.intervention.table.melissa(n.sim = N.SIM,
                                          target.population = "wholepop")

table.for.color.coding = function(tab){
    
    tab = substr(tab,1,2)
    
}

test = table.for.color.coding(tab.1)

if(1==2){
    
    write.csv(tab.1, file="~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/EHE/Results/table_ybhmsm.csv")
    write.csv(tab.2, file="~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/EHE/Results/table_msmidu.csv")
    write.csv(tab.3, file="~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/EHE/Results/table_wholepop.csv")
    
    write.csv(test, file="~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/EHE/Results/table_ybhmsm_color.csv")
}

