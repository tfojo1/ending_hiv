

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
                                       prevalence.ratio.log.sd = 1, #fill this in here
                                       
                                       # treatment.proportion
                                       treatment.proportion=0.18,
                                       treatment.proportion.sd=.02 #fill in here
                                       )
{
    
    # assume we have this (we're going to pull from the data)
    obs.prevalence.age1 = rep(.05, length(total.prevalence.years))
    obs.prevalence.age2 = rep(.03, length(total.prevalence.years))
    
    log.sd.prevalence.age1 = rep(.01, length(total.prevalence.years))
    log.sd.prevalence.age2 = rep(.01, length(total.prevalence.years))
    
    
    function(sim, log=T)
    {
        # Pull from sim
        # we'll fill in later the code to pull these from the simulations
        sim.prevalence.general.age1 = rep(.06, length(total.prevalence.years))
        sim.prevalence.general.age2 = rep(.04, length(total.prevalence.years))
        
        sim.prevalence.general.total = rep(.05, length(total.prevalence.years))
        sim.prevalence.hiv.total = rep(.14, length(total.prevalence.years))
        sim.treated.prevalence.hiv.total = sim.prevalence.hiv.total * 0.2
        
        # Calculate the likelihoods
        lik.prevalence.general.age1
        lik.prevalence.general.age2
        lik.prevalence.ratio
        lik.treatment.proportion
        
        # Put them together
        if (log)
            lik.prevalence.general.age1 + lik.prevalence.general.age2 + lik.prevalence.ratio + lik.treatment.proportion
        else
            lik.prevalence.general.age1 * lik.prevalence.general.age2 * lik.prevalence.ratio + lik.treatment.proportion
    }
}