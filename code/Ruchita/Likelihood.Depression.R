
#Initialize N,M,y,sd,cor

N = 3

#Correlation Matrix for Observeations
theta = .5

cor_obs = matrix(theta, nrow = N, ncol = N)
diag(cor_obs) = 1

#Stadard deviation observation errors

sd.obs = c(.2,.3,.2)


#Correlation Matrix for Ratio
theta = .5

cor_ratio = matrix(theta, nrow = N, ncol = N)
diag(cor_ratio) = 1

#Stadard deviation ratio errors (log transformed anywhere?)

sd.obs = c(.2,.2,.2)


likelihood.depression <- function(obs.prob #Observered porbabilities, log transformed 
                                  ,sim.prob,#simulated probabilities, log transformed,
                                  HIV.gen.pop, #ratio of HIV to general population, log transformed
                                  sd.obs,# standard deviation of observation errors
                                  sd.ratio,# standard devaition of ratio errors
                                  cor_obs, # correaltion matrix of observation errors
                                  cor_ratio)# correlation matrix of ratio errors
                                  {
  
  S_obs = (sd.obs) %*% t(sd.obs) * cor_obs 
  
  
  S_ratio = rep(HIV.gen.pop,length(obs.prob)) %*% t(rep(HIV.gen.pop,length(obs.prob))) * cor_ratio
  
  
  cov = S_obs + S_ratio
  
  return(dmvnorm(obs.prob, mean=sim.prob-HIV.gen.pop, cov),log =TRUE)))
 
  
  
}