

prior = join.distributions(
#-- Suppression --#
heterosexual.linked.or = Lognormal.Distribution(0, log(2)),
msm.linked.or = Lognormal.Distribution(0, log(2)),
idu.linked.or = Lognormal.Distribution(0, log(2)),
msm.idu.linked.or = Lognormal.Distribution(0, log(2)),

black.linked.or = Lognormal.Distribution(0, log(2)),
hispanic.linked.or = Lognormal.Distribution(0, log(2)),

age1.linked.or = Lognormal.Distribution(0, log(2)),
age2.linked.or = Lognormal.Distribution(0, log(2)),
age4.linked.or = Lognormal.Distribution(0, log(2)),
age5.linked.or = Lognormal.Distribution(0, log(2)),

heterosexual.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
msm.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
idu.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
msm.idu.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),

black.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
hispanic.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),

age1.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
age2.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
age4.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
age5.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5)
)

#A list with two components
# $intercepts = An array indexed [age,race,sex,risk] of intercepts on the log scale
# $slopes = An array indexed [age,race,sex,risk] of slopes on the log scale
get.linkage.model <- function(alphas, base.model,
                              anchor.year)
{
    dim.names = list(age=SETTINGS$AGES$labels,
                     race=SETTINGS$RACES,
                     sex=SETTINGS$SEXES,
                     risk=SETTINGS$RISK_STRATA)
    
    intercepts = base.model$intercepts
    slopes = base.model$slopes
    
    
    # Age
    intercepts[1,,,] = intercepts[1,,,] + log(alphas['age1.linked.or'])
    slopes[1,,,] = slopes[1,,,] + log(alphas['age1.linked.slope.or'])
    
    intercepts[2,,,] = intercepts[2,,,] + log(alphas['age2.linked.or'])
    slopes[2,,,] = slopes[2,,,] + log(alphas['age2.linked.slope.or'])
    
    intercepts[4,,,] = intercepts[4,,,] + log(alphas['age4.linked.or'])
    slopes[4,,,] = slopes[4,,,] + log(alphas['age4.linked.slope.or'])
    
    intercepts[5,,,] = intercepts[5,,,] + log(alphas['age5.linked.or'])
    slopes[5,,,] = slopes[5,,,] + log(alphas['age5.linked.slope.or'])
    
    # Race
    
    #black
    intercepts[,'black',,] = intercepts[,'black',,] + log(alphas['black.linked.or'])
    slopes[,'black',,] = slopes[,'black',,] + log(alphas['black.linked.slope.or'])
    
    #hispanic
    intercepts[,'hispanic',,] = intercepts[,'hispanic',,] + log(alphas['hispanic.linked.or'])
    slopes[,'hispanic',,] = slopes[,'hispanic',,] + log(alphas['hispanic.linked.slope.or'])
    
    # Sex + Risk
    idu = c('active_IDU', 'IDU_in_remission')
    non.idu = c('never_IDU')
    
    #het
    intercepts[,,c('heterosexual_male','female'),non.idu] = intercepts[,,c('heterosexual_male','female'),non.idu] + 
        log(alphas['heterosexual.linked.or'])
    slopes[,,c('heterosexual_male','female'),non.idu] = slopes[,,c('heterosexual_male','female'),non.idu] + 
        log(alphas['heterosexual.linked.slope.or'])
    
    #msm
    intercepts[,,'msm',non.idu] = intercepts[,,'msm',non.idu] + log(alphas['msm.linked.or'])
    slopes[,,'msm',non.idu] = slopes[,,'msm',non.idu] + log(alphas['msm.linked.slope.or'])
    
    #idu
    intercepts[,,c('heterosexual_male','female'),idu] = intercepts[,,c('heterosexual_male','female'),idu] +
        log(alphas['idu.linked.or'])
    slopes[,,c('heterosexual_male','female'),idu] = slopes[,,c('heterosexual_male','female'),idu] +
        log(alphas['idu.linked.slope.or'])
    
    #msm-idu
    intercepts[,,'msm',idu] = intercepts[,,'msm',idu] + log(alphas['msm.idu.linked.or'])
    slopes[,,'msm',idu] = slopes[,,'msm',idu] + log(alphas['msm.idu.linked.slope.or'])
    

    # Return it
    model = list(intercepts=intercepts,
                 slopes=slopes,
                 anchor.year = base.model$anchor.year)
    model
}

# Take the logistic models and map them to probabilities over a time span
# 
# Return an array indexed [year, age, race, sex, risk]
# The input model is a list with two elements
#   $intercepts = An array indexed [age,race,sex,risk] of intercepts on the log scale
#   $slopes = An array indexed [age,race,sex,risk] of slopes on the log scale
#   $anchor.year = the year at which logit(p) = intercept
get.linked.proportions <- function(model, years)
{
    dim.names = c(list(year=as.character(years)),
                  dimnames(model$intercepts))
    rv = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    for (year in years)
    {
        lo.year = model$intercepts + (year - model$anchor.year) * model$slopes
        p.year = 1 / (1 + exp(-lo.year))
        
        rv[as.character(year),,,,] = p.year
    }
    
    p.year
}

calculate.linkage.likelihood <- function(model,
                                         location,
                                         surv=msa.surveillance,
                                         sim)
{
    total.linked = get.surveillance.data(surv, location.codes=location, data.type='linkage')
    age.linked = get.surveillance.data(surv, location.codes=location, data.type='linkage', age=T)
    race.linked = get.surveillance.data(surv, location.codes=location, data.type='linkage', race=T)
    sex.linked = get.surveillance.data(surv, location.codes=location, data.type='linkage', sex=T)
    risk.linked = get.surveillance.data(surv, location.codes=location, data.type='linkage', risk=T)
    
       
       # do some stuff
      
    years #the years we are interested in
    new.dx #array indexed [year, age, race, sex, risk] FROM OUR SIMULATION
    p.linked #array indexed [year, age, race, sex, risk] - what comes out of get.linked.proportions
    total.linked #a vector of length=length(years) the proportion linked in a year
    rho #compound symmetry correlation
    
    
    obs = numeric()
    sd.measurement = numeric()
    mu.0 = numeric()
    sd.model = numeric()
    
    for (year in years)
    {
        n.by.stratum = new.dx[as.character(year),,,,]
        n.total = sum(n.by.stratum)
        
        obs[as.character(year)] = total.linked[as.character(year)] * n.total
        sd.measurement[as.character(year)] = sqrt(n.total[as.character(year)] * total.linked[as.character(year)] *
                                  (1-total.linked[as.character(year)]))
        
        p.linked.by.stratum = p.linked[as.character(year),,,,]
        n.linked.by.stratum = p.linked.by.stratum * n.by.stratum
        mu.0[as.character(year)] = sum(n.linked.by.stratum)
        
        var.model.by.stratum = n.by.stratum * p.linked.by.stratum * (1-p.linked.by.stratum)
        var.model = sum(var.model.by.by.stratum)
        sd.model[as.character(year)] = sqrt(var.model)
            
        #dnorm(obs, mu.0, sqrt(sd.model^2 + sd.measurement^2))
    }
    
    corr.matrix = matrix(rho, nrow=length(obs), ncol=length(obs))
    diag(corr.matrix) = 1
    
    sigma.measurement = sd.measurement %*% t(sd.measurement) * corr.matrix
    sigma.model = diag(sd.model^2)
    sigma = sigma.measurement + sigma.model
    
    dmvnorm(x=obs,
            mean=mu.0,
            sigma=sigma)
}