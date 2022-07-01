

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
                                       prevalence.ratio.log.sd = 0.1, #sd from normal distribution is .1 ==> transform to log sd? 
                                       
                                       # treatment.proportion
                                       treatment.proportion=0.18,
                                       treatment.proportion.sd=log(2)/2 
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
        sim.treated.prevalence.hiv.total = sim.prevalence.hiv.total * 0.2
        
        # Calculate the likelihoods
        lik.prevalence.general.age1 = prod(dlnorm(sim.prvealence.general.age1,obs.prevalence.age1,sd = .2)) #Ok to just use dlnorm?, assume likelihood sd of .2
        lik.prevalence.general.age2 = prod(dlnorm(sim.prvealence.general.age2,obs.prevaleence.age2,sd = .2)) 
        lik.prevalence.ratio = prod(dnorm(sim.prevalence.hiv.total/sim.prevalence.general.total,prevalence.ratio,sd = .2))
        lik.treatment.proportion = prod(sim.treated.prevalence.hiv.total/sim.prevalence.hiv.total,treatment.proportion)
        
        #Correlation 
        
        N_total_prevalence = length(total.prevalence.years)
        N_prevalence_ratio = length(prevalence.ratio.years)
        N_treatment_proportion = length(treatment.proportion.years)
        
        #Correlation Matrices 
        total.prevalence.error.correlation=0.5
        prevalence.ratio.error.correlation=0.8
        treatment.proportion.error.correlation=0.8
        
        cor_obs_total_prevealence = matrix(total.prevalence.error.correlation, nrow = N_total_prevalence , ncol = N_total_prevalence)
        diag(cor_obs_total_prevealence) = 1
        cor_obs_prevalence_ratio = matrix(prevalence.ratio.error.correlation, nrow = N_prevalence_ratio , ncol = N_prevalence_ratio)
        diag(cor_obs_prevalence_ratio) = 1
        cor_obs_treatment_proportion = matrix(treatment.proportion.error.correlation, nrow = N_treatment_proportion, ncol = N_treatment_proportion)
        diag(cor_obs_treatment_proportion) = 1
        
        #Stadard deviation observation errors (change)
        
        sd.obs_total_prevalence = rep(.06, length(total.prevalence.years))
        sd.obs_prevalence_ratio = rep(prevalence.ratio.log.sd, length(prevalence.ratio.years))
        sd.obs_treatment_proportion = rep(treatment.proportion.sd, length(treatment.proportion.years))
        
        #Use instead for sd 
        S_obs_total_prevalence = (sd.obs_total_prevalence) %*% t(sd.obs_total_prevalence) * cor_obs_total_prevealence
        S_obs_prevalence_ratio = (sd.obs_prevalence_ratio) %*% t(sd.obs_prevalence_ratio) * cor_obs_prevalence_ratio
        S_obs_treatment_proportion = (sd.obs_treatment_proportion) %*% t(sd.obs_treatment_proportion) * cor_obs_treatment_proportion
        
  
        # Put them together
        if (log)
            lik.prevalence.general.age1 + lik.prevalence.general.age2 + lik.prevalence.ratio + lik.treatment.proportion
        else
            lik.prevalence.general.age1 * lik.prevalence.general.age2 * lik.prevalence.ratio * lik.treatment.proportion
    }
}