
library(reshape2)

if (1==2)
{
    p = create.strata.logistic.models(total.proportions = get.surveillance.data(msa.surveillance, '31080', data.type='linkage'),
                                    
                                    age.proportions = get.surveillance.data(msa.surveillance, '31080', data.type='linkage', age=T),
                                    race.proportions = get.surveillance.data(msa.surveillance, '31080', data.type='linkage', race=T),
                                    sex.proportions = get.surveillance.data(msa.surveillance, '31080', data.type='linkage', sex=T),
                                    risk.proportions = get.surveillance.data(msa.surveillance, '31080', data.type='linkage', risk=T),
                                    
                                    n.age = get.surveillance.data(msa.surveillance, '31080', data.type='new.for.continuum', age=T),
                                    n.race = get.surveillance.data(msa.surveillance, '31080', data.type='new.for.continuum', race=T),
                                    n.sex = get.surveillance.data(msa.surveillance, '31080', data.type='new.for.continuum', sex=T),
                                    n.risk = get.surveillance.data(msa.surveillance, '31080', data.type='new.for.continuum', risk=T))
    
    }

#Return a list with two elements
# $logit.intercept - an array indexed [age, race, sex, risk]
# $logit.slope - an array indexed [age, race, sex, risk]

create.strata.logistic.models <- function(total.proportions,
                                          
                                          age.proportions, #an array indexed[year, age]
                                          race.proportions, #an array indexed[year, race]
                                          sex.proportions, #an array indexed[year, sex]
                                          risk.proportions, #an array indexed[year, risk],
                                          
                                          n.age,
                                          n.race,
                                          n.sex,
                                          n.risk,
                                          
                                          anchor.year=2010,
                                          
                                          settings=SETTINGS)
{
    #-- Fit Age --#
    df.age = reshape2::melt(age.proportions)
    df.age = df.age[!is.na(df.age$value),]
    df.age$year = df.age$year - anchor.year
    
    df.age$age1 = as.numeric(df.age$age=='13-24 years')
    df.age$age2 = as.numeric(df.age$age=='25-34 years')
    df.age$age4 = as.numeric(df.age$age=='45-54 years')
    df.age$age5 = as.numeric(df.age$age=='55+ years')
    
    fit.age = glm(value ~ age1 + age2 + age4 + age5 +
                      year + year:age1 + year:age2 + year:age4 + year:age5, data=df.age,
                  family='binomial')
    
    #-- Fit Race --#
    df.race = reshape2::melt(race.proportions)
    df.race = df.race[!is.na(df.race$value),]
    df.race$year = df.race$year - anchor.year
    
    df.race$black = as.numeric(df.race$race=='black')
    df.race$hispanic = as.numeric(df.race$race=='hispanic')
    
    fit.race = glm(value ~ black + hispanic + year + black:year + hispanic:year,
                   data=df.race, family='binomial')
    
    
    #-- Fit Risk Factor --#
    
    df.risk = reshape2::melt(risk.proportions)
    df.risk = df.risk[!is.na(df.risk$value),]
    df.risk$year = df.risk$year - anchor.year
    
    df.risk$msm = as.numeric(df.risk$risk=='msm')
    df.risk$msm_idu = as.numeric(df.risk$risk=='msm_idu')
    df.risk$idu = as.numeric(df.risk$risk=='idu')
    
    fit.risk = glm(value ~ msm + msm_idu + idu + year +
                       msm:year + msm_idu:year + idu:year,
                   data=df.risk, family='binomial')
    
    #-- Fit Sex --#

    years = intersect(intersect(dimnames(sex.proportions)[['year']],
                                dimnames(n.sex)[['year']]),
                      intersect(dimnames(risk.proportions)[['year']],
                                dimnames(n.risk)[['year']]))
    
    denominators.male = n.sex[years,'male']
    numerators.male = sex.proportions[years,'male'] * denominators.male
    
    
    denominators.msm = n.risk[years,'msm'] + n.risk[years,'msm_idu']
    numerators.msm = risk.proportions[years,'msm'] * n.risk[years,'msm'] +
        risk.proportions[years,'msm_idu'] * n.risk[years,'msm_idu']
    
    numerators.het.male = numerators.male - numerators.msm
    denominators.het.male = denominators.male - denominators.msm
    
    p.het.male = numerators.het.male / denominators.het.male
    
    
    df.sex = data.frame(value=c(p.het.male, sex.proportions[,'female']),
                        year=c(years, dimnames(sex.proportions)[['year']]),
                        female=as.numeric(c(rep(F,length(p.het.male)), rep(T,dim(sex.proportions)[1]))))
    df.sex$year = as.numeric(df.sex$year) - anchor.year
    df.sex = df.sex[!is.na(df.sex$value),]
    
    fit.sex = glm(value ~ female + year + female:year,
                  data=df.sex, family='binomial')
    
    
    #-- SET UP THE ARRAYS --#
    
    dim.names = list(age=settings$AGES$labels,
                     race=settings$RACES,
                     sex=settings$SEXES,
                     risk=settings$RISK_STRATA)
    
    log.odds.intercepts = log.odds.slopes = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    
    # Age
    log.odds.intercepts[1,,,] = log.odds.intercepts[1,,,] + fit.age$coefficients['age1']
    log.odds.slopes[1,,,] = log.odds.slopes[1,,,] + fit.age$coefficients['age1:year']
    
    log.odds.intercepts[2,,,] = log.odds.intercepts[2,,,] + fit.age$coefficients['age2']
    log.odds.slopes[2,,,] = log.odds.slopes[2,,,] + fit.age$coefficients['age2:year']
    
    log.odds.intercepts[4,,,] = log.odds.intercepts[4,,,] + fit.age$coefficients['age4']
    log.odds.slopes[4,,,] = log.odds.slopes[4,,,] + fit.age$coefficients['age4:year']
    
    log.odds.intercepts[5,,,] = log.odds.intercepts[5,,,] + fit.age$coefficients['age5']
    log.odds.slopes[5,,,] = log.odds.slopes[5,,,] + fit.age$coefficients['age5:year']
    
    # Race
    log.odds.intercepts[,'black',,] = log.odds.intercepts[,'black',,] + fit.race$coefficients['black']
    log.odds.slopes[,'black',,] = log.odds.slopes[,'black',,] + fit.race$coefficients['black:year']
    
    log.odds.intercepts[,'hispanic',,] = log.odds.intercepts[,'hispanic',,] + fit.race$coefficients['hispanic']
    log.odds.slopes[,'hispanic',,] = log.odds.slopes[,'hispanic',,] + fit.race$coefficients['hispanic:year']
    
    # Risk
    idu = c('active_IDU', 'IDU_in_remission')
    non.idu = 'never_IDU'
    non.msm = c('heterosexual_male','female')
    
    log.odds.intercepts[,,'msm',non.idu] = log.odds.intercepts[,,'msm',non.idu] + fit.risk$coefficients['msm']
    log.odds.slopes[,,'msm',non.idu] = log.odds.slopes[,,'msm',non.idu] + fit.risk$coefficients['msm:year']
    
    log.odds.intercepts[,,'msm',idu] = log.odds.intercepts[,,'msm',idu] + fit.risk$coefficients['msm_idu']
    log.odds.slopes[,,'msm',idu] = log.odds.slopes[,,'msm',idu] + fit.risk$coefficients['msm_idu:year']
    
    log.odds.intercepts[,,non.msm,idu] = log.odds.intercepts[,,non.msm,idu] + fit.risk$coefficients['idu']
    log.odds.slopes[,,non.msm,idu] = log.odds.slopes[,,non.msm,idu] + fit.risk$coefficients['idu:year']
    
    # Sex (non-MSM)
    log.odds.intercepts[,,'female',] = log.odds.intercepts[,,'female',] + fit.sex$coefficients['female']
    log.odds.slopes[,,'female',] = log.odds.slopes[,,'female',] + fit.sex$coefficients['female:year']
    
    #-- Fit the total --#
    
    total.proportions = total.proportions[!is.na(total.proportions)]
    total.years = as.numeric(names(total.proportions))
    years = as.character(total.years)
    
    df.total = melt(total.proportions)
    df.total$year = df.total$year - anchor.year
    df.total = df.total[!is.na(df.total$value),]
    fit.total = glm(value~year, data=df.total, family='binomial')
    base.log.intercept = fit.total$coefficients[1]
    base.log.slope = fit.total$coefficients[2]
    
    
    
    #-- Guess the number in each stratum --#
    
    frac.age = n.age[years,] / rowSums(n.age[years,])
    frac.race = n.race[years,] / rowSums(n.race[years,])
    
    dim.names.sex.risk = c(list(year=years), dim.names[3:4])
    frac.het.male = denominators.het.male[years] / (denominators.het.male[years] + n.sex[years,'female'])
    n.sex.risk = array(0, dim=sapply(dim.names.sex.risk,length), dimnames=dim.names.sex.risk)
    n.sex.risk[,'msm','never_IDU'] = n.risk[years,'msm']
    n.sex.risk[,'msm','active_IDU'] = n.risk[years,'msm_idu']
    n.sex.risk[,'female','never_IDU'] = n.risk[years,'heterosexual'] * (1-frac.het.male)
    n.sex.risk[,'heterosexual_male','never_IDU'] = n.risk[years,'heterosexual'] * frac.het.male
    n.sex.risk[,'female','active_IDU'] = n.risk[years,'idu'] * (1-frac.het.male)
    n.sex.risk[,'heterosexual_male','active_IDU'] = n.risk[years,'idu'] * frac.het.male
    frac.sex.risk = n.sex.risk / rowSums(n.sex.risk)
    
    frac.stratum = sapply(1:length(years), function(i){
        outer(outer(frac.age[i,], frac.race[i,]), frac.sex.risk[i,,])
    })
    frac.stratum = t(frac.stratum)
    all.dim.names = c(list(year=years), dim.names)
    dim(frac.stratum) = sapply(all.dim.names, length)
    dimnames(frac.stratum) = all.dim.names
    
    #-- Fit the overall intercept and slope --#
    expit = function(lo){1/(1+exp(-lo))}
    total.proportions = total.proportions[!is.na(total.proportions)]
    calc.p.total.by.year <- function(base.log.intercept, base.log.slope)
    {
        sapply(1:length(total.years), function(i){
            year = total.years[i]-anchor.year
            p.stratum = expit(log.odds.intercepts + log.odds.slopes * year + 
                              base.log.intercept + base.log.slope*year)
        
            p.total = sum(p.stratum * frac.stratum[i,,,,])
            p.total
        })
    }
    
    fn <- function(params)
    {
        p.total.by.year = calc.p.total.by.year(params[1], params[2])
        score = sum( (p.total.by.year - total.proportions)^2 )
        score
    }
    
    opt = optim(fn=fn, par=c(0,0))
    
    base.log.intercept = opt$par[1]
    base.log.slope = opt$par[2]
    
    list(intercept=log.odds.intercepts+base.log.intercept,
         slope=log.odds.slopes+base.log.slope)
}

