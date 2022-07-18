

# return a function that takes one argument - sim - and returns the log-likelihood
library(emdbook)

make.depression.likelihood <- function(location,
                                       
                                       # years
                                       total.prevalence.years =3,
                                       prevalence.ratio.years = 3,
                                       treatment.proportion.years =3,
                                       
                                       # correlation
                                       total.prevalence.error.correlation.age=0.5,
                                       total.prevalence.error.correlation.time=0.5,
                                       prevalence.ratio.error.correlation=0.8,
                                       treatment.proportion.error.correlation=0.8,
                                       
                                       # prevalence.ratio
                                       prevalence.ratio = 3.08,
                                       prevalence.ratio.log.sd = .014, 
                                       
                                       # treatment.proportion
                                       treatment.proportion=0.18,
                                       treatment.proportion.log.sd=log(2)/2 
                                       
                        
                                       )
{
    

    obs.prevalence.age1 = rep(.05,total.prevalence.years)
    obs.prevalence.age2 = rep(.03,total.prevalence.years)
    
    log.sd.prevalence.age1 = rep(.01, total.prevalence.years)
    log.sd.prevalence.age2 = rep(.01, total.prevalence.years)
    
    
    function(sim, log=T)
    {
        # Pull from sim

        sim.prevalence.general.age1 = rep(.06,total.prevalence.years)
        sim.prevalence.general.age2 = rep(.04, total.prevalence.years)
        sim.prevalence.general = c(sim.prevalence.general.age1,sim.prevalence.general.age2)
        
        sim.prevalence.hiv.total = rep(.14, length(total.prevalence.years))
        sim.treated.prevalence.hiv.total = sim.prevalence.hiv.total * treatment.proportion
        
        # Calculate the likelihoods
        lik.prevalence.general.age1 = prod(dlnorm(obs.prevalence.age1,meanlog =  (log(sim.prevalence.general.age1)-.5*(log((log.sd.prevalence.age1/sim.prevalence.general.age1)^2+1))),sdlog  =  log.sd.prevalence.age1, log = log)) 
        lik.prevalence.general.age2 = prod(dlnorm(obs.prevalence.age2,meanlog =  (log(sim.prevalence.general.age2)-.5*(log((log.sd.prevalence.age2/sim.prevalence.general.age2)^2+1))),sdlog  =  log.sd.prevalence.age2, log = log)) 
        lik.prevalence.ratio = prod(dlnorm(prevalence.ratio,meanlog = (log(sim.prevalence.hiv.total/sim.prevalence.general.total)-.5*(log((prevalence.ratio.log.sd/sim.prevalence.hiv.total/sim.prevalence.general.total)^2+1))),sdlog =  prevalence.ratio.log.sd, log = log))
        lik.treatment.proportion = prod(dlnorm(treatment.proportion,meanlog= (log(sim.treated.prevalence.hiv.total/sim.prevalence.hiv.total)-.5*(log((treatment.proportion.log.sd/sim.treated.prevalence.hiv.total/sim.prevalence.hiv.total)^2+1))),sdlog=treatment.proportion.log.sd, log = log))
        
        #Correlation 
        obs.prevalence = c(obs.prevalence.age1,obs.prevalence.age2)
        
        N_prevalence_ratio = length(prevalence.ratio.years)
        N_treatment_proportion = length(treatment.proportion.years)
        
        #Correlation Matrices 
  
        #Create new correaltion matrix to consolidate age 
        cor_obs_total_prevalence.age = matrix(total.prevalence.error.correlation.age, nrow = total.prevalence.years, ncol = total.prevalence.years)
        diag(cor_obs_total_prevalence.age) = 1
        
        cor_obs_total_prevalence.time = matrix(total.prevalence.error.correlation.time, nrow = total.prevalence.years, ncol = total.prevalence.years)
        diag(cor_obs_total_prevalence.time) = 1
        
        cor_obs_total_combined = matrix(total.prevalence.error.correlation.age*total.prevalence.error.correlation.time, nrow = total.prevalence.years,ncol = total.prevalence.years)
        diag(cor_obs_total_combined) = total.prevalence.error.correlation.time
        
        cor_obs_total_combined.1 = cbind(cor_obs_total_prevalence.age, cor_obs_total_combined)
        cor_obs_total_combined.2 = cbind(cor_obs_total_combined,cor_obs_total_prevalence.time)
        
        cor_obs_total_prevalence = rbind(cor_obs_total_combined.1,cor_obs_total_combined.2 )
        
        cor_obs_prevalence_ratio = matrix(prevalence.ratio.error.correlation, nrow = N_prevalence_ratio , ncol = N_prevalence_ratio)
        diag(cor_obs_prevalence_ratio) = 1
        cor_obs_treatment_proportion = matrix(treatment.proportion.error.correlation, nrow = N_treatment_proportion, ncol = N_treatment_proportion)
        diag(cor_obs_treatment_proportion) = 1
        
        #Stadard deviation observation errors
        
        sd.obs_total_prevalence = c(log.sd.prevalence.age1,log.sd.prevalence.age2)
        sd.obs_prevalence_ratio = rep(prevalence.ratio.log.sd, length(prevalence.ratio.years))
        sd.obs_treatment_proportion = rep(treatment.proportion.log.sd, length(treatment.proportion.years))
        
        #Use instead for sd 
        S_obs_total_prevalence = (sd.obs_total_prevalence) %*% t(sd.obs_total_prevalence) * cor_obs_total_prevalence
        S_obs_prevalence_ratio = (sd.obs_prevalence_ratio) %*% t(sd.obs_prevalence_ratio) * cor_obs_prevalence_ratio
        S_obs_treatment_proportion = (sd.obs_treatment_proportion) %*% t(sd.obs_treatment_proportion) * cor_obs_treatment_proportion
        
        if (log){
          mult.lik.prevalence.general = dmvnorm(log(obs.prevalence),log(sim.prevalence.general), S_obs_total_prevalence, log = T) + sum(-log(obs.prevalence))
          mult.lik.prevalence.ratio = dmvnorm(log(prevalence.ratio), log(sim.prevalence.hiv.total/sim.prevalence.general.total), S_obs_prevalence_ratio, log = T) + sum(-log(prevalence.ratio)) 
          mult.lik.treatment.proportion = dmvnorm(log(treatment.proportion), log(sim.treated.prevalence.hiv.total/sim.prevalence.hiv.total), S_obs_treatment_proportion, log = T) + sum(-log(treatment.proportion)) 
        
          
        } else{
          mult.lik.prevalence.general = dmvnorm(log(obs.prevalence), log(sim.prevalence.general), S_obs_total_prevalence)*(1/prod(obs.prevalence)) 
          mult.lik.prevalence.ratio = dmvnorm(log(prevalence.ratio), log(sim.prevalence.hiv.total/sim.prevalence.general.total), S_obs_prevalence_ratio)*(1/prod(prevalence.ratio)) 
          mult.lik.treatment.proportion = dmvnorm(log(treatment.proportion),  log(sim.treated.prevalence.hiv.total/sim.prevalence.hiv.total), S_obs_treatment_proportion)*(1/prod(treatment.proportion)) 
        }
        
        # Put them together
        if (log) {
          lik.prevalence.general.age1 + lik.prevalence.general.age2 + lik.prevalence.ratio + lik.treatment.proportion
          mult.lik.prevalence.general + mult.lik.prevalence.ratio + mult.lik.treatment.proportion
        }
            
        else{
          lik.prevalence.general.age1 * lik.prevalence.general.age2 * lik.prevalence.ratio * lik.treatment.proportion
          mult.lik.prevalence.general*mult.lik.prevalence.ratio*mult.lik.treatment.proportion
        }
            
    }
}
