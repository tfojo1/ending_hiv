
source('code/applications/covid/covid_plots.R')
CUM.INC.YEARS = 2020:2025

##-- SOURCE CODE --##
if (1==2)
{
    load('results/covid/covid_4.2_results.Rdata')
    
}
DELAYED = 'covid.delayed.mobility'
RAPID = 'covid.rapid.resumption.mobility'
ALL.COVID.SCENARIOS = c(DELAYED, RAPID)

YEARS.OF.INTEREST = 2019:2025


##----------##
##-- TEXT --##
##----------##

## FIRST PARAGRAPH
## Narrative on changes in reported vs incidence
if (1==2)
{
    abs.diff.from.prior = outcomes.arr[,ALL.COVID.SCENARIOS,as.character(YEARS.OF.INTEREST[-1]),,] - 
        outcomes.arr[,ALL.COVID.SCENARIOS,as.character(YEARS.OF.INTEREST[-length(YEARS.OF.INTEREST)]),,]
    rel.diff.from.prior = abs.diff.from.prior /  
        outcomes.arr[,ALL.COVID.SCENARIOS,as.character(YEARS.OF.INTEREST[-length(YEARS.OF.INTEREST)]),,]
 
    # the year-on-year reductions - new
    round(100*data.frame(
        mean = apply(rel.diff.from.prior[,,,'new',], 'year', mean),
        ci.lower = apply(rel.diff.from.prior[,,,'new',], 'year', quantile, probs=.025),
        ci.upper = apply(rel.diff.from.prior[,,,'new',], 'year', quantile, probs=.975)
    )   )
    
    decrease.new.from.prior = apply(rel.diff.from.prior[,,,'new',]<0, 'year', mean)
    decrease.new.in.both.prior = apply(rel.diff.from.prior[,,-1,'new',]<0 & 
                                           rel.diff.from.prior[,,-dim(rel.diff.from.prior)['year'],'new',]<0, 'year', mean)
    
    round(100-100*apply(rel.diff.from.prior[,RAPID,,'new',]<0, 'year', mean))
    round(100-100*apply(rel.diff.from.prior[,DELAYED,,'new',]<0, 'year', mean))
    round(100*apply(rel.diff.from.prior[,RAPID,,'new',]<0, 'year', mean))
    round(100*apply(rel.diff.from.prior[,DELAYED,,'new',]<0, 'year', mean))
    
    round(100-100*apply(rel.diff.from.prior[,RAPID,,'incidence',]<0, 'year', mean))
    round(100-100*apply(rel.diff.from.prior[,DELAYED,,'incidence',]<0, 'year', mean))
    
    round(100*c(1-decrease.new.from.prior['2021'],
                1-decrease.new.in.both.prior['2022'] - (1-decrease.new.from.prior['2021'])))
    
    # the year-on-year reductions - incidence
    round(100*data.frame(
      mean = apply(rel.diff.from.prior[,,,'incidence',], 'year', mean),
      ci.lower = apply(rel.diff.from.prior[,,,'incidence',], 'year', quantile, probs=.025),
      ci.upper = apply(rel.diff.from.prior[,,,'incidence',], 'year', quantile, probs=.975)
    )   )
    round(100*data.frame(
      mean = apply(rel.diff.from.prior[,,,'new',], 'year', mean),
      ci.lower = apply(rel.diff.from.prior[,,,'new',], 'year', quantile, probs=.025),
      ci.upper = apply(rel.diff.from.prior[,,,'new',], 'year', quantile, probs=.975)
    )   )
    
    decrease.inc.from.prior = apply(rel.diff.from.prior[,,,'incidence',]<0, 'year', mean)
    decrease.inc.in.both.prior = apply(rel.diff.from.prior[,,-1,'incidence',]<0 & 
                                           rel.diff.from.prior[,,-dim(rel.diff.from.prior)['year'],'incidence',]<0, 'year', mean)
    
    
    decrease.new = rel.diff.from.prior[,,,'new',]<0
    decrease.inc = rel.diff.from.prior[,,,'incidence',]<0
    increase.inc.decrease.new = decrease.new & !decrease.inc
    decrease.inc.increase.new = !decrease.new & decrease.inc
    
    apply(increase.inc.decrease.new | decrease.inc.increase.new, 'year', mean)
    
    sum(increase.inc.decrease.new[,,c('2021','2022'),]) / sum(decrease.new[,,c('2021','2022'),])
    sum(decrease.inc.increase.new[,,c('2021','2022'),]) / sum(!decrease.new[,,c('2021','2022'),])
    
    
    # in the delayed scenario - discrepancy between new and inc
    # the year-on-year reductions - new
    round(100*data.frame(
        mean = apply(rel.diff.from.prior[,DELAYED,,'new',], 'year', mean),
        ci.lower = apply(rel.diff.from.prior[,DELAYED,,'new',], 'year', quantile, probs=.025),
        ci.upper = apply(rel.diff.from.prior[,DELAYED,,'new',], 'year', quantile, probs=.975)
    )   )
    round(100*data.frame(
        mean = apply(rel.diff.from.prior[,DELAYED,,'incidence',], 'year', mean),
        ci.lower = apply(rel.diff.from.prior[,DELAYED,,'incidence',], 'year', quantile, probs=.025),
        ci.upper = apply(rel.diff.from.prior[,DELAYED,,'incidence',], 'year', quantile, probs=.975)
    )   )
    
    
    #narrative split good/bad
    high.suppression.mask = parameters[,'suppression.reduction'] < 0.2
    low.suppression.mask = parameters[,'suppression.reduction'] >= 0.2
    
    high.transmission.mask = parameters[,'sexual.transmission.reduction'] < 0.2
    low.transmission.mask = parameters[,'sexual.transmission.reduction'] > 0.3
    
    high.s.low.t = high.suppression.mask & low.transmission.mask
    low.s.high.t = low.suppression.mask & high.transmission.mask
    
    round(100*data.frame(
      mean = apply(rel.diff.from.prior[,DELAYED,,'new',low.s.high.t], 'year', mean),
      ci.lower = apply(rel.diff.from.prior[,DELAYED,,'new',low.s.high.t], 'year', quantile, probs=.025),
      ci.upper = apply(rel.diff.from.prior[,DELAYED,,'new',low.s.high.t], 'year', quantile, probs=.975)
    )   )
    round(100*data.frame(
      mean = apply(rel.diff.from.prior[,DELAYED,,'incidence',low.s.high.t], 'year', mean),
      ci.lower = apply(rel.diff.from.prior[,DELAYED,,'incidence',low.s.high.t], 'year', quantile, probs=.025),
      ci.upper = apply(rel.diff.from.prior[,DELAYED,,'incidence',low.s.high.t], 'year', quantile, probs=.975)
    )   )
    
    round(100*data.frame(
      mean = apply(rel.diff.from.prior[,DELAYED,,'new',high.s.low.t], 'year', mean),
      ci.lower = apply(rel.diff.from.prior[,DELAYED,,'new',high.s.low.t], 'year', quantile, probs=.025),
      ci.upper = apply(rel.diff.from.prior[,DELAYED,,'new',high.s.low.t], 'year', quantile, probs=.975)
    )   )
    round(100*data.frame(
      mean = apply(rel.diff.from.prior[,DELAYED,,'incidence',high.s.low.t], 'year', mean),
      ci.lower = apply(rel.diff.from.prior[,DELAYED,,'incidence',high.s.low.t], 'year', quantile, probs=.025),
      ci.upper = apply(rel.diff.from.prior[,DELAYED,,'incidence',high.s.low.t], 'year', quantile, probs=.975)
    )   )
    
}


# Changes in cumulative
CUMULATIVE.YEARS = 2020:2025
if (1==2)
{
    cumulative = apply(outcomes.arr[,,as.character(CUMULATIVE.YEARS),,], c('location','scenario','outcome','sim'), sum)
    
    
    total.cumulative = apply(cumulative, c('scenario','outcome','sim'), sum)
    
    format(round(c(mean(total.cumulative['baseline','incidence',]),
      quantile(total.cumulative['baseline','incidence',], probs=c(0.025,0.975)))),
      big.mark=',')
    
    
    format(round(c(mean(total.cumulative['covid.rapid.resumption.mobility','incidence',]),
                   quantile(total.cumulative['covid.rapid.resumption.mobility','incidence',], probs=c(0.025,0.975)))),
           big.mark=',')
    
    cumulative.change.from.baseline = lapply(ALL.COVID.SCENARIOS, function(sc){
        (cumulative[,sc,,]-cumulative[,'baseline',,]) / cumulative[,'baseline',,]
    })
    names(cumulative.change.from.baseline) = ALL.COVID.SCENARIOS
    
    total.cumulative.change.from.baseline = lapply(ALL.COVID.SCENARIOS, function(sc){
        (total.cumulative[sc,,]-total.cumulative['baseline',,]) / total.cumulative['baseline',,]
    })
    names(total.cumulative.change.from.baseline) = ALL.COVID.SCENARIOS
    
    # total change
    format(round(t(sapply(ALL.COVID.SCENARIOS, function(sc){
        c(mean=mean(total.cumulative[sc,'incidence',]-total.cumulative['baseline','incidence',]),
          ci.lower=quantile(total.cumulative[sc,'incidence',]-total.cumulative['baseline','incidence',], probs=0.025),
          ci.upper=quantile(total.cumulative[sc,'incidence',]-total.cumulative['baseline','incidence',], probs=0.975)
        )
    }))), big.mark=',')
    round(100*t(sapply(total.cumulative.change.from.baseline, function(delta){
        c(mean=mean(delta['incidence',]),
          ci.lower=quantile(delta['incidence',], probs=0.025),
          ci.upper=quantile(delta['incidence',], probs=0.975)
        )
    })))
    
    # delayed vs rapid
    delayed.vs.base.inc = total.cumulative['covid.delayed.mobility','incidence',] - total.cumulative['covid.rapid.resumption.mobility','incidence',]
    delayed.vs.base.rel.inc = delayed.vs.base.inc / total.cumulative['covid.delayed.mobility','incidence',]
    
    #delayed.vs.base.rel.delta = (total.cumulative[sc,'incidence',]-total.cumulative['baseline','incidence',]) 
    
    format(round(
        c(mean=mean(delayed.vs.base.inc),
          ci.lower=quantile(delayed.vs.base.inc, probs=0.025),
          ci.upper=quantile(delayed.vs.base.inc, probs=0.975)
        )), big.mark=',')
    
    round(100* c(mean=mean(delayed.vs.base.rel.inc),
                 ci.lower=quantile(delayed.vs.base.rel.inc, probs=0.025),
                 ci.upper=quantile(delayed.vs.base.rel.inc, probs=0.975)
    ))
    
    msa.rel.change.inc = reshape2::melt(sapply(cumulative.change.from.baseline, function(delta){
        apply(delta[,'incidence',], 'location', mean)
    }))
    msa.rel.change.inc$ci.lower = as.numeric(sapply(cumulative.change.from.baseline, function(delta){
        apply(delta[,'incidence',], 'location', quantile, probs=0.025)
    }))
    msa.rel.change.inc$ci.upper = as.numeric(sapply(cumulative.change.from.baseline, function(delta){
        apply(delta[,'incidence',], 'location', quantile, probs=0.975)
    }))
    
    o = order(msa.rel.change.inc$value)
    
    most.least = msa.rel.change.inc[o[c(1,length(o))],]
    
    t(apply(most.least, 1, function(x){
        c(location=location.names[x[1]],
          scenario=x[2],
          round(100*as.numeric(x[3:5])))
    }))
    
    #for prevalence
    prev.year = '2025'
    round(100*t(sapply(ALL.COVID.SCENARIOS, function(sc){
        x = (outcomes.arr[,sc,prev.year,'prevalence.all',] - outcomes.arr[,'baseline',prev.year,'prevalence.all',]) /
            outcomes.arr[,'baseline',prev.year,'prevalence.all',]
        
        c(mean=mean(x),
          ci.lower=quantile(x,probs=0.025),
          ci.upper=quantile(x,probs=0.975),
          min=min(x),
          max=max(x)
          )
    })),1)
    
    #for acute
    
    n.acute = apply(outcomes.arr[,,as.character(YEARS.OF.INTEREST), 'prevalence.acute.all',],
                    c('location','scenario','year','sim'), sum)
    
    
    

    round(100*t(rbind(apply(n.acute[,DELAYED,,] /
                              n.acute[,'baseline',,],
                            'year', mean),
                      
                      apply(n.acute[,DELAYED,,] /
                              n.acute[,'baseline',,],
                            'year', quantile, probs=c(.025,.975))
    ))-100)
    
    round(100*t(rbind(apply(outcomes.arr[,DELAYED, as.character(YEARS.OF.INTEREST), 'prevalence.acute.all',low.s.high.t] /
            outcomes.arr[,'baseline', as.character(YEARS.OF.INTEREST), 'prevalence.acute.all',low.s.high.t],
          'year', mean),
          
          apply(outcomes.arr[,DELAYED, as.character(YEARS.OF.INTEREST), 'prevalence.acute.all',low.s.high.t] /
                  outcomes.arr[,'baseline', as.character(YEARS.OF.INTEREST), 'prevalence.acute.all',low.s.high.t],
                'year', quantile, probs=c(.025,.975))
    ))-100)
    
    
    round(100*t(rbind(apply(outcomes.arr[,DELAYED, as.character(YEARS.OF.INTEREST), 'prevalence.acute.all',high.s.low.t] /
                    outcomes.arr[,'baseline', as.character(YEARS.OF.INTEREST), 'prevalence.acute.all',high.s.low.t],
                  'year', mean),
            
            apply(outcomes.arr[,DELAYED, as.character(YEARS.OF.INTEREST), 'prevalence.acute.all',high.s.low.t] /
                    outcomes.arr[,'baseline', as.character(YEARS.OF.INTEREST), 'prevalence.acute.all',high.s.low.t],
                  'year', quantile, probs=c(.025,.975))
    ))-100)
    
    
    #for acute-diagnosed
    
    #narrative split good/bad
    high.suppression.mask = parameters[,'suppression.reduction'] < 0.2
    low.suppression.mask = parameters[,'suppression.reduction'] >= 0.2
    
    high.transmission.mask = parameters[,'sexual.transmission.reduction'] < 0.2
    low.transmission.mask = parameters[,'sexual.transmission.reduction'] > 0.3
    
    high.s.low.t = high.suppression.mask & low.transmission.mask
    low.s.high.t = low.suppression.mask & high.transmission.mask
    
        
    n.acute.dx = outcomes.arr[,,as.character(YEARS.OF.INTEREST),'prevalence.acute.all',] -
                          outcomes.arr[,,as.character(YEARS.OF.INTEREST),'prevalence.acute.undiagnosed',]
    
    pseudo.new.acute.dx = 12/2.9 * n.acute.dx[,,-1,]
    pseudo.new.acute.dx = 12/2.9 * (n.acute.dx[,,-1,] + n.acute.dx[,,-length(YEARS.OF.INTEREST),])/2

    prev.year.weights = c('2019'=0.25, '2020'=.75, '2021'=0.5, '2022'=0.5, '2023'=0.5, '2024'=0.5)
    prev.year.weights = expand.population(prev.year.weights, dimnames(n.acute.dx[,,-length(YEARS.OF.INTEREST),]))
    pseudo.new.acute.dx = 12/2.9 * (n.acute.dx[,,-1,] * (1-prev.year.weights) +
                                      n.acute.dx[,,-length(YEARS.OF.INTEREST),] * prev.year.weights)
    
    sc = c(DELAYED,RAPID)#'baseline'
    
sc= DELAYED
    if (length(sc)==1)
    {
       total.pseudo.new.acute.dx = apply(pseudo.new.acute.dx[,sc,,], c('year','sim'), sum)
       total.new = apply(outcomes.arr[,sc,as.character(YEARS.OF.INTEREST[-1]),'new',], c('year','sim'), sum)
    
         frac.acute = total.pseudo.new.acute.dx / total.new
       cbind(
          all=apply(frac.acute, 'year', mean),
          good=apply(frac.acute[,high.s.low.t], 'year', mean),
          bad=apply(frac.acute[,low.s.high.t], 'year', mean)
       )
    }
    else
    {
       total.pseudo.new.acute.dx = apply(pseudo.new.acute.dx[,sc,,], c('year','scenario','sim'), sum)
       total.new = apply(outcomes.arr[,sc,as.character(YEARS.OF.INTEREST[-1]),'new',], c('year','scenario','sim'), sum)
       
       frac.acute = total.pseudo.new.acute.dx / total.new
       cbind(
          all=apply(frac.acute, 'year', mean),
          good=apply(frac.acute[,,high.s.low.t], 'year', mean),
          bad=apply(frac.acute[,,low.s.high.t], 'year', mean)
       )
       
    }
    
}


#Parameters influence
if (1==2)
{
    PARAMETERS.OF.INTEREST = dimnames(parameters)[[2]][1:4]
    cumulative = apply(outcomes.arr[,,1,as.character(CUMULATIVE.YEARS),,], c('location','scenario','outcome','sim'), sum)
    
    
    total.cumulative = apply(cumulative, c('scenario','outcome','sim'), sum)
    
    format(round(c(mean(total.cumulative['baseline','incidence',]),
                   quantile(total.cumulative['baseline','incidence',], probs=c(0.025,0.975)))),
           big.mark=',')
    
    
    format(round(c(mean(total.cumulative[RAPID,'incidence',]),
                   quantile(total.cumulative[RAPID,'incidence',], probs=c(0.025,0.975)))),
           big.mark=',')
    
    cumulative.change.from.baseline = lapply(ALL.COVID.SCENARIOS, function(sc){
        (cumulative[,sc,,]-cumulative[,'baseline',,]) / cumulative[,'baseline',,]
    })
    names(cumulative.change.from.baseline) = ALL.COVID.SCENARIOS
    
    total.cumulative.change.from.baseline = lapply(ALL.COVID.SCENARIOS, function(sc){
        (total.cumulative[sc,,]-total.cumulative['baseline',,]) / total.cumulative['baseline',,]
    })
    names(total.cumulative.change.from.baseline) = ALL.COVID.SCENARIOS
    
    total.corr = sapply(total.cumulative.change.from.baseline, function(change){
        sapply(PARAMETERS.OF.INTEREST, function(var){
            cor(change['incidence',], parameters[,var], method='spearman')
        })
    })
    
    round(100*total.corr)
    
    msa.corr = lapply(cumulative.change.from.baseline, function(change){
        rv = sapply(PARAMETERS.OF.INTEREST, function(var){
            apply(change[,'incidence',], 'location', cor, parameters[,var], method='spearman')
        })
        
        names(dimnames(rv)) = c('location','parameter')
        rv
    })
    
    msa.corr$base
    
    msa.corr.rank = lapply(msa.corr, function(corr){
        rv = t(apply(corr, 'location', function(x){
            o = order(abs(x), decreasing=T)
            rv = sapply(1:length(o), function(i){
                (1:length(o))[i==o]
            })
            names(rv) = names(x)
            
            rv
        }))
        
        rv
    })
}


# high vs low
if (1==2)
{
    high.suppression.mask = parameters[,'suppression.reduction'] < 0.2
    low.suppression.mask = parameters[,'suppression.reduction'] >= 0.2
    
    high.transmission.mask = parameters[,'sexual.transmission.reduction'] < 0.2
    low.transmission.mask = parameters[,'sexual.transmission.reduction'] > 0.3
    
    # need to run the block of code above
    high.s.low.t = high.suppression.mask & low.transmission.mask
    sum(high.s.low.t)
    round(100*t(sapply(total.cumulative.change.from.baseline, function(change){
        x = change['incidence',high.s.low.t]
        c(mean=mean(x),
          ci.lower=quantile(x, probs=.025),
          ci.upper=quantile(x, probs=0.975))
    })))
    format(round(t(sapply(ALL.COVID.SCENARIOS, function(sc){
       c(mean=mean(total.cumulative[sc,'incidence',high.s.low.t]-total.cumulative['baseline','incidence',high.s.low.t]),
         ci.lower=quantile(total.cumulative[sc,'incidence',high.s.low.t]-total.cumulative['baseline','incidence',high.s.low.t], probs=0.025),
         ci.upper=quantile(total.cumulative[sc,'incidence',high.s.low.t]-total.cumulative['baseline','incidence',high.s.low.t], probs=0.975)
       )
    }))), big.mark=',')
    
    low.s.high.t = low.suppression.mask & high.transmission.mask
    sum(low.s.high.t)
    round(100*t(sapply(total.cumulative.change.from.baseline, function(change){
        x = change['incidence',low.s.high.t]
        c(mean=mean(x),
          ci.lower=quantile(x, probs=.025),
          ci.upper=quantile(x, probs=0.975))
    })))
    format(round(t(sapply(ALL.COVID.SCENARIOS, function(sc){
       c(mean=mean(total.cumulative[sc,'incidence',low.s.high.t]-total.cumulative['baseline','incidence',low.s.high.t]),
         ci.lower=quantile(total.cumulative[sc,'incidence',low.s.high.t]-total.cumulative['baseline','incidence',low.s.high.t], probs=0.025),
         ci.upper=quantile(total.cumulative[sc,'incidence',low.s.high.t]-total.cumulative['baseline','incidence',low.s.high.t], probs=0.975)
       )
    }))), big.mark=',')
}



# City level
if (1==2)
{
    cumulative.change.from.baseline = lapply(ALL.COVID.SCENARIOS, function(sc){
        (cumulative[,sc,,]-cumulative[,'baseline',,]) / cumulative[,'baseline',,]
    })
    names(cumulative.change.from.baseline) = ALL.COVID.SCENARIOS
    
    
    
    low.sex.change = parameters[,'sexual.transmission.reduction'] < 0.1
    high.sex.change = parameters[,'sexual.transmission.reduction'] > 0.4
    low.supp.change = parameters[,'suppression.reduction'] < 0.1
    high.supp.change = parameters[,'suppression.reduction'] > 0.3
    
    msa = CHICAGO
    msa = BOSTON
    msa = SAN.DIEGO
    
    round(100*mean(cumulative.change.from.baseline$delayed.hiv.care[msa, 'incidence', low.sex.change]))
    round(100*mean(cumulative.change.from.baseline$delayed.hiv.care[msa, 'incidence', high.sex.change]))
    
    round(100*mean(cumulative.change.from.baseline$delayed.hiv.care[msa, 'incidence', high.supp.change]))
    round(100*mean(cumulative.change.from.baseline$delayed.hiv.care[msa, 'incidence', low.supp.change]))
}

# between-scenario difference vs between-msa difference
if (1==2)
{
    cumulative.change.from.baseline = lapply(ALL.COVID.SCENARIOS, function(sc){
        (cumulative[,sc,,]-cumulative[,'baseline',,]) / cumulative[,'baseline',,]
    })
    names(cumulative.change.from.baseline) = ALL.COVID.SCENARIOS
    
    mean.cumulative.change.from.baseline = lapply(cumulative.change.from.baseline, function(cc){
        rowMeans(cc, dims=2)
    })
    
    between.scenario.difference = cumulative.change.from.baseline$delayed.hiv.care[,'incidence',] - 
        cumulative.change.from.baseline$base[,'incidence',]
    mean(abs(between.scenario.difference))
    round(100*c(mean((between.scenario.difference)),
                quantile((between.scenario.difference), c(.025,.975))))
    qplot(between.scenario.difference)
    round(100*c(mean(abs(between.scenario.difference)),
                quantile(abs(between.scenario.difference), c(.025,.975))))
    
    msas = dimnames(mean.cumulative.change.from.baseline$base)$location
    msa.pairs = combn(msas, 2)
    between.msa.difference = sapply(cumulative.change.from.baseline, function(cc){
        apply(msa.pairs, 2, function(msa.pair){
            cc[msa.pair[1],'incidence',] - cc[msa.pair[2], 'incidence',]
        })
    })
    mean(between.msa.difference)
    mean(abs(between.msa.difference))
    qplot(between.msa.difference)
    
    round(100*c(mean(abs(between.msa.difference)),
                quantile(abs(between.msa.difference), c(.025,.975))))
}
