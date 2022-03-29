
# for checking that these things make sense
if (1==2)
{
    outcome='new'
    years = 2010:2018
    
    truth = get.surveillance.outcomes.array(years=years,
                                            locations=dimnames(abs.arr)$location,
                                            outcomes=outcome)
    
    abs.arr.z = abs.arr[as.character(years),,outcome,,1,]
    truth = truth[,dimnames(abs.arr.z)$stratification,1,]
    
    abs.arr.z = add.abs.total.row(abs.arr.z, mask=!is.na(truth))
    truth = add.abs.total.row(truth)
    
    msa = '16740'
    strat= 'total'
    df.sim = data.frame(year=years,
                    estimate=apply(abs.arr.z[,strat,msa,], 'year', mean),
                    ci.lower=apply(abs.arr.z[,strat,msa,], 'year', quantile, probs=0.025),rep(NA, length(years)),
                    ci.upper=apply(abs.arr.z[,strat,msa,], 'year', quantile, probs=0.975),rep(NA, length(years))
                    )
    df.truth = data.frame(year=years,
                          estimate=truth[,strat,msa])
    
    ggplot() +
        geom_ribbon(data=df.sim, aes(year, ymin=ci.lower, ymax=ci.upper), alpha=0.2, fill='blue') +
        geom_line(data=df.sim, aes(year, estimate), size=2, color='blue') +
        geom_point(data=df.truth, aes(year, estimate), size=4) + ylim(0,NA)

    calculate.mape(df.sim$estimate, df.truth$estimate)   
    calculate.mape(abs.arr.z[,strat,msa,], truth[,strat,msa])
    
    
    #checking awareness in the 95-95-95 sim
    load('results/full/estimates/approx959595.v2.Rdata')
    dx = apply(abs.arr['2025','total','prevalence.diagnosed',,1,], 'sim', sum) /
      apply(abs.arr['2025','total','prevalence.all',,1,], 'sim', sum)
    mean(dx)
    quantile(dx, probs=c(0.025,0.975))
    
    load('results/full/estimates/ests.approx959595.3y.new.Rdata')
    ests.approx959595.3y.new$estimates['Total',2]
    ests.approx959595.3y.new$ci.lower['Total',2]
    ests.approx959595.3y.new$ci.upper['Total',2]
}

calculate.mape <- function(forecast, truth, na.rm=T)
{
    mean(abs((truth-forecast)/truth), na.rm=T)
}

calculate.mae <- function(forecast, truth, na.rm=T)
{
    mean(abs(truth-forecast), na.rm=T)
}

make.pretty.error.summary.table <- function(mae, mape,
                                            sep=' ',
                                            pct.digits=0,
                                            insert.row.and.col.names=F)
{
    pretty.table = paste0(format(round(mae), big.mark=','),
                          sep, "(",
                          round(100*mape, digits = pct.digits),
                          "%)")
    dim(pretty.table) = dim(mae)

    if (insert.row.and.col.names)
    {
        pretty.table = rbind(pretty.strat.names(dimnames(mae)[[2]]),
                             pretty.table)
        
        location.names = unlist(msa.names(dimnames(mae)[[1]]))
        location.names['Total'] = 'Total'
        pretty.table = cbind(c("", location.names),
                             pretty.table)
        
        dimnames(pretty.table) = NULL
    }
    
#    if (!is.null(file))
 #       make.latex.table(pretty.table, file=file,
  #                       above.last.row = '\\hline',
   ##                      last.row.suffix = '}')
    #
    pretty.table
    
    
}

get.error.summary.table <- function(abs.arr,
                           outcome,
                           years=2010:2018,
                           take.mean.first=F,
                           error.fn=calculate.mape)
{
    dimnames(abs.arr)$outcome[dimnames(abs.arr)$outcome=='prevalence.diagnosed'] = 'prevalence'
    
    truth = get.surveillance.outcomes.array(years=years,
                                            locations=dimnames(abs.arr)$location,
                                            outcomes=outcome)
    
    abs.arr = abs.arr[as.character(years),,outcome,,1,]
    truth = truth[,dimnames(abs.arr)$stratification,1,]
    
    abs.arr = add.abs.total.row(abs.arr, mask=!is.na(truth))
    truth = add.abs.total.row(truth)

    if (take.mean.first)
    {
        abs.arr = apply(abs.arr, names(dimnames(truth)), mean)    
        rv = sapply(dimnames(abs.arr)$stratification, function(strat){
            sapply(dimnames(abs.arr)$location, function(loc){
                error.fn(forecast=abs.arr[,strat,loc],
                               truth=truth[,strat,loc])
            })
        })
    }
    else
    {
        rv = sapply(dimnames(abs.arr)$stratification, function(strat){
            sapply(dimnames(abs.arr)$location, function(loc){
                error.fn(forecast=abs.arr[,strat,loc,],
                               truth=truth[,strat,loc])
            })
        })
    }
        
        rv
}

pretty.strat.names <- function(x)
{
    rv = paste0(toupper(substr(x,1,1)),
           sapply(x, function(z){substr(z, 2, nchar(z))}))
    
    rv[x=='msm'] = 'MSM'
    rv[x=='idu'] = "PWID"
    rv[x=='msm_idu'] = "MSM-PWID"
    
    rv
}
