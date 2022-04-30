

calc.waiting.time <- function(times,
                              n.sim=10000)
{
    sum(times / 2 * times) / sum(times) 
}

if (1==2)
{
    N.SIM = 10000

    # exponential
    sapply(c(1,2,3), function(interval){
        calc.waiting.time(rexp(N.SIM, 1/interval))
    })
    
    # uniform
    sapply(c(1,2,3), function(upper){
        calc.waiting.time(runif(N.SIM, 0, upper))
    })
    

    # gamma
    sapply(c(1,2,3), function(inv.alpha){
        calc.waiting.time(rgamma(N.SIM, 1/inv.alpha, 1))
    })
    
    
    #again a gamma
    alpha=1.1;beta=2.2;x=seq(0,24,length=1000);qplot(x, dgamma(x/12, alpha, beta));print(alpha/beta)
    qgamma(c(.025,.5,.975), alpha, beta) * 12
    calc.waiting.time(rgamma(N.SIM, alpha, beta)) * 12
    
    
    #lognormal
    mu = 0.5
    sigma = 0.5
    log.sd = sqrt(log(sigma^2/mu^2 + 1))
    log.mean = log(mu) - log.sd^2/2
    
    x = rlnorm(N.SIM, log.mean, log.sd)
    mean(x)
    sd(x)
    median(x) * 12
    calc.waiting.time(x)
    
    x=seq(0,24,length=1000);qplot(x, dlnorm(x/12, log.mean, log.sd))
    qlnorm(c(.025,.5,.975), log.mean, log.sd) * 24
    
    
    x=seq(0,24,length=1000);qplot(c(x,x),
                                  c(dlnorm(x/12, log.mean, log.sd), dexp(x/12, 1/mu)),
                                  color=rep(c('lognormal','exponential'), each=length(x)))
    
    z = 1:12
    round(100*cbind(lognormal=plnorm(z/12, log.mean, log.sd),
          exponential=pexp(z/12, 1/mu)))
}
