library('ggplot2')
library('betareg')

LAART.PARAMETER.DISTRIBUTION = join.distributions(
  
  # or.disengagement.laart.vs.oral,
  # 
  # p.resistance.up.front.laart.recently.suppressed,
  # p.resistance.up.front.laart.durably.suppressed = Beta.Distribution(alpha=0.5 + 3, beta=0.5+279),
  # p.resistance.up.front.laart.unsuppressed,
  # 
  # rr.resistance.ongoing.vs.upfront.laart,
  # rr.resistance.disengage.vs.upfront.laart,
  # 
  # or.gain.suppression.resistance.vs.oral,
  # or.lose.suppression.resistance.vs.oral.recently.suppressed,
  # or.lose.suppression.resistance.vs.oral.durably.suppressed,
  # 
  # or.disengagement.resistance.vs.oral,
  ##############################################
  #for multiple studies jeffreys, sum up alpha = 0.5 + sum of success, beta = 0.5 + sum of failure
  #use jeffrey prior for exact LAART values, use getbetaparams when more uncertain and multipley ets.siga by 2 or level of uncertainty
  laart.versus.oral.disengagement.rr = Lognormal.Distribution(meanlog = 0.03183364, sdlog = 0.02772465),
  laart.recently.suppressed.to.resistant.disengaged = Beta.Distribution(alpha=3.5, beta=280.5), #3/283 Flair, set flair as lower limit and make 12.89% upper limit?
  laart.discontinuation = Beta.Distribution(alpha=6.414881, beta=189.959034), #switch to art ci 2.05 to 4.53
  laart.recently.suppressed.to.resistant.unsuppressed = Beta.Distribution(alpha=0.5 + 3, beta=0.5 + 280), #3/283 Flair
  laart.durably.suppressed.to.resistant.disengaged = Beta.Distribution(alpha=3.5, beta=305.5), #3/308 Atlas,set atlas as lower limit and make 12.89% upper limit?
  laart.durably.suppressed.to.resistant.unsuppressed = Beta.Distribution(alpha=3.5, beta=305.5), #3/308 Atlas,
  #or unsuppressed vs recently suppressed resistant disengagement (1-5?) lognormal*  
  unsuppressed.vs.recently.suppressed.resistance.disengaged.or = Lognormal.Distribution(meanlog=0.804719, sdlog = 0.4105709), 
  laart.unsuppressed.to.laart.recently.suppressed = Beta.Distribution(alpha=280.5, beta=3.5), #280/283 from FLAIR is closest gain of suppression estimate
  
  
  #bound resistance lower=1
  #recently.suppressed.to.failing = , expanded continuum parameter
  resistant.versus.oral.loss.of.suppression.rr = Lognormal.Distribution(meanlog=-0.03045921, sdlog=0.2421124), 
  #recently.suppressed.to.disengaged = , expanded continuum parameter
  resistant.versus.oral.disengagement.rr = Lognormal.Distribution(meanlog=0.2350018, sdlog=0.1198989), #look into medication complexity and adherence
  resistant.versus.oral.gain.of.suppression.rr = Lognormal.Distribution(meanlog=0.03045921, sdlog=0.2421124),
  resistant.versus.reengagement.rr = Constant.Distribution(1)
)

VERSION.MANAGER = register.projection.parameters.distribution(VERSION.MANAGER,
                                                              version='laart',
                                                              distribution = LAART.PARAMETER.DISTRIBUTION,
                                                              join.with.previous.version.distribution = T)

# hr.of.viral.rebound
# 

getlognormalparams<- function(sample.vals, ci){
  if(missing(ci)){
    log.mu = mean(log(sample.vals))
    new_ci = c(min(sample.vals), max(sample.vals))
    logci = log(new_ci)
    log.sigma = (logci[2] - logci[1]) / 2 / 1.96
  }
  else{
    log.mu = mean(log(sample.vals))
    log.ci = log(c(min(ci), max(ci)))
    log.sigma = (log.ci[2] - log.ci[1]) / 2 / 1.96
  }
  return(c(log.mu, log.sigma))
}

if (1==2)
{
    reported.est = 3
    ci = c(2,4)
    log.mean = log(reported.est)
    log.mean = mean(log(ci))
    log.sd = (log(ci[2])-log(ci[1])) / 2 / qnorm(.975)
}

# rands = rlnorm(100000, log.mu, log.sigma)
# mean(rands)
# median(rands)
# quantile(rands, c(.025, .975))
# 
# 
# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }
# 
# rands = rbeta(10000, 4, 281)
# mean(rands)
# getmode(rands)
# round(quantile(rands, c(.025,.975)),3)
# qplot(rands)

getbetaparams<- function(sample.vals, confidence) {
  if(confidence == 1){
    est.mu <- mean(sample.vals)
    ci = c(min(sample.vals), max(sample.vals))
    est.sigma <- (ci[2]-ci[1])/2/1.96
    alpha<-((1 - est.mu) / (est.sigma^2) - (1 / est.mu)) * (est.mu ^ 2)
    beta<-alpha * (1 / est.mu - 1)}
  else{est.mu <- mean(sample.vals)
  ci = c(min(sample.vals), max(sample.vals))
  est.sigma <- 2*((ci[2]-ci[1])/2/1.96)
  alpha<-((1 - est.mu) / (est.sigma^2) - (1 / est.mu)) * (est.mu ^ 2)
  beta<-alpha * (1 / est.mu - 1)}
  return(c(alpha, beta))
}

# quad <- function(a, b, c){
#   a <- as.complex(a)
#   answer <- c((-b + sqrt(b^2 - 4 * a * c)) / (2 * a),
#               (-b - sqrt(b^2 - 4 * a * c)) / (2 * a))
#   if(all(Im(answer) == 0)) answer <- Re(answer)
#   if(answer[1] == answer[2]) return(answer[1])
#   answer
# }
# normalizeodds<-function(num1, den1){
#   x<-min(quad(1, -2*den1, num1*den1))
#   prob1<-x/den1
#   return(prob1/(1-prob1))
# }
# normalizeor<-function(num1, den1, num2, den2){
#   odds1<-normalizeodds(num1,den1)
#   odds2<-normalizeodds(num2,den2)
#   return(odds1/odds2)
# }
