

# return a function that takes one argument - sim - and returns the log-likelihood
make.depression.likelihood <- function(location,
                                       
                                       # years
                                       total.prevalence.years,
                                       prevalence.ratio.years,
                                       treatment.proportion.years,
                                       
                                       # correlation
                                       total.prevalence.error.correlation=0.5,
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
    

    obs.prevalence.age1 = rep(.05, length(total.prevalence.years))
    obs.prevalence.age2 = rep(.03, length(total.prevalence.years))
    
    log.sd.prevalence.age1 = rep(.01, length(total.prevalence.years))
    log.sd.prevalence.age2 = rep(.01, length(total.prevalence.years))
    
    
    function(sim, log=T)
    {
        # Pull from sim

        sim.prevalence.general.age1 = rep(.06, length(total.prevalence.years))
        sim.prevalence.general.age2 = rep(.04, length(total.prevalence.years))
        
        sim.prevalence.general.total = rep(.05, length(total.prevalence.years))
        sim.prevalence.hiv.total = rep(.14, length(total.prevalence.years))
        sim.treated.prevalence.hiv.total = sim.prevalence.hiv.total * treatment.proportion
        
        # Calculate the likelihoods
        lik.prevalence.general.age1 = prod(dlnorm(obs.prevalence.age1,meanlog =  (log(sim.prevalence.general.age1)-.5*(log((log.sd.prevalence.age1/sim.prevalence.general.age1)^2+1))),sdlog  =  log.sd.prevalence.age1, log = log)) 
        lik.prevalence.general.age2 = prod(dlnorm(obs.prevalence.age2,meanlog =  (log(sim.prevalence.general.age2)-.5*(log((log.sd.prevalence.age2/sim.prevalence.general.age2)^2+1))),sdlog  =  log.sd.prevalence.age2, log = log)) 
        lik.prevalence.ratio = prod(dlnorm(prevalence.ratio,meanlog = (log(sim.prevalence.hiv.total/sim.prevalence.general.total)-.5*(log((prevalence.ratio.log.sd/sim.prevalence.hiv.total/sim.prevalence.general.total)^2+1))),sdlog =  prevalence.ratio.log.sd, log = log))
        lik.treatment.proportion = prod(dlnorm(treatment.proportion,meanlog= (log(sim.treated.prevalence.hiv.total/sim.prevalence.hiv.total)-.5*(log((treatment.proportion.log.sd/sim.treated.prevalence.hiv.total/sim.prevalence.hiv.total)^2+1))),sdlog=treatment.proportion.log.sd, log = log))
        
        #Correlation 
        
        N_total_prevalence = length(total.prevalence.years)
        N_prevalence_ratio = length(prevalence.ratio.years)
        N_treatment_proportion = length(treatment.proportion.years)
        
        #Correlation Matrices 
  
        
        cor_obs_total_prevealence = matrix(total.prevalence.error.correlation, nrow = N_total_prevalence , ncol = N_total_prevalence)
        diag(cor_obs_total_prevealence) = 1
        cor_obs_prevalence_ratio = matrix(prevalence.ratio.error.correlation, nrow = N_prevalence_ratio , ncol = N_prevalence_ratio)
        diag(cor_obs_prevalence_ratio) = 1
        cor_obs_treatment_proportion = matrix(treatment.proportion.error.correlation, nrow = N_treatment_proportion, ncol = N_treatment_proportion)
        diag(cor_obs_treatment_proportion) = 1
        
        #Stadard deviation observation errors
        
        sd.obs_total_prevalence.age1 = rep(.06, length(total.prevalence.years))
        sd.obs_total_prevalence.age2 =
        sd.obs_prevalence_ratio = rep(prevalence.ratio.log.sd, length(prevalence.ratio.years))
        sd.obs_treatment_proportion = rep(treatment.proportion.sd, length(treatment.proportion.years))
        
        #Use instead for sd 
        S_obs_total_prevalence = (sd.obs_total_prevalence) %*% t(sd.obs_total_prevalence) * cor_obs_total_prevealence
        S_obs_prevalence_ratio = (sd.obs_prevalence_ratio) %*% t(sd.obs_prevalence_ratio) * cor_obs_prevalence_ratio
        S_obs_treatment_proportion = (sd.obs_treatment_proportion) %*% t(sd.obs_treatment_proportion) * cor_obs_treatment_proportion
        
        #?dmunorm where log = TRUE?
        
        
        # Put them together
        if (log)
            lik.prevalence.general.age1 + lik.prevalence.general.age2 + lik.prevalence.ratio + lik.treatment.proportion
        else
            lik.prevalence.general.age1 * lik.prevalence.general.age2 * lik.prevalence.ratio * lik.treatment.proportion
    }
}