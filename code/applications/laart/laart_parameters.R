library('ggplot2')
library('betareg')
getlognormalparams<- function(ci){
  log.mu = mean(log(ci))
  log.ci = log(c(min(ci), max(ci)))
  log.sigma = (log.ci[2] - log.ci[1]) / 2 / 1.96
  return(c(log.mu, log.sigma))
}

getbetaparams<- function(ci, confidence) {
  if(confidence == 1){
    est.mu <- mean(ci)
    ci = c(min(ci), max(ci))
    est.sigma <- (ci[2]-ci[1])/2/1.96
    alpha<-((1 - est.mu) / (est.sigma^2) - (1 / est.mu)) * (est.mu ^ 2)
    beta<-alpha * (1 / est.mu - 1)}
  else{est.mu <- mean(ci)
  ci = c(min(ci), max(ci))
  est.sigma <- 2*((ci[2]-ci[1])/2/1.96)
  alpha<-((1 - est.mu) / (est.sigma^2) - (1 / est.mu)) * (est.mu ^ 2)
  beta<-alpha * (1 / est.mu - 1)}
  return(c(alpha, beta))
}

LAART.PARAMETER.DISTRIBUTION = join.distributions(
  
  #for multiple studies jeffreys, sum up alpha = 0.5 + sum of success, beta = 0.5 + sum of failure
  #use jeffrey prior for exact LAART values, use getbetaparams when more uncertain and multipley ets.siga by 2 or level of uncertainty

  #laart.versus.oral.disengagement.rr = Lognormal.Distribution(meanlog = getlognormalparams(c(0.993421, 1.120315582))[1], sdlog = getlognormalparams(c(0.993421, 1.120315582))[2]),
  laart.versus.oral.disengagement.rr = Uniform.Distribution(min = 0.5, max = 1),
  
  #Using Jeffrey Prior Distribution with % gain of resistance from Flair trial 
  laart.recently.suppressed.to.resistant.disengaged = Beta.Distribution(alpha=3.5, beta=280.5),
  
  #Using getbetaparams (not confident)
  laart.discontinuation = Beta.Distribution(alpha=getbetaparams(c(.0205,.0453), 0)[1], beta=getbetaparams(c(.0205,.0453), 0)[2]), 
  
  #Using Jeffrey Prior Distribution with % gain of resistance from Flair trial 
  #laart.recently.suppressed.to.resistant.unsuppressed = Beta.Distribution(alpha=3.5, beta=280.5), 
  
  #Using Jeffrey Prior Distribution with % gain of resistance from Atlas trial 
  #laart.durably.suppressed.to.resistant.disengaged = Beta.Distribution(alpha=3.5, beta=305.5),
  
  #Using Jeffrey Prior Distribution with % gain of resistance from Atlas trial 
  #laart.durably.suppressed.to.resistant.unsuppressed = Beta.Distribution(alpha=3.5, beta=305.5), 
  
  unsuppressed.vs.recently.suppressed.resistance.disengaged.or = Lognormal.Distribution(meanlog=getlognormalparams(c(1,5))[1], sdlog = getlognormalparams(c(1,5))[2]), 
  
  #Using Jeffrey Prior Distribution with % maintaining suppression from Flair trial 
  laart.unsuppressed.to.laart.recently.suppressed = Beta.Distribution(alpha=280.5, beta=3.5), 

  resistant.versus.oral.loss.of.suppression.rr = Lognormal.Distribution(meanlog=getlognormalparams(c(0.6,1.55))[1], sdlog=getlognormalparams(c(0.6,1.55))[2]), 

  resistant.versus.oral.disengagement.rr = Lognormal.Distribution(meanlog=getlognormalparams(c(1,1.61))[1], sdlog=getlognormalparams(c(1,1.61))[2]), 
  
  resistant.versus.oral.gain.of.suppression.rr = Lognormal.Distribution(meanlog=getlognormalparams(c(0.64,1.65))[1], sdlog=getlognormalparams(c(0.64,1.65))[2]),
  
  resistant.versus.oral.reengagement.rr = Constant.Distribution(1), 
  
  engaged.suppressed.to.resistant.unsuppressed = Beta.Distribution(alpha=6.5, beta=585.5), 
  
  engaged.unsuppressed.vs.suppressed.resistant.unsuppressed.or = Lognormal.Distribution(meanlog=getlognormalparams(c(1,4))[1], sdlog = getlognormalparams(c(1,4))[2], lower = 1),
  
  laart.suppressed.to.resistant.unsuppressed = Beta.Distribution(alpha = 1.5, beta = 555.5),
  
  laart.suppressed.to.resistant.disengaged = Beta.Distribution(alpha=6.5/4, beta=585.5/4)
  
)

VERSION.MANAGER = register.projection.parameters.distribution(VERSION.MANAGER,
                                                              version='laart',
                                                              distribution = LAART.PARAMETER.DISTRIBUTION,
                                                              join.with.previous.version.distribution = T)


# if (1==2)
# {
#     reported.est = 3
#     ci = c(2,4)
#     log.mean = log(reported.est)
#     log.mean = mean(log(ci))
#     log.sd = (log(ci[2])-log(ci[1])) / 2 / qnorm(.975)
# }

# rands = rlnorm(100000, 0.8047190, 0.4105709)
# mean(rands)
# median(rands)
# quantile(rands, c(.025, .975))




