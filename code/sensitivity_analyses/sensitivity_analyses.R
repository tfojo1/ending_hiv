library(data.table)
#library(sensitivity)
library(reshape2)
library(ggsci)
library(scales)

source("code/process_results/pretty_formatting.R")

source('code/interventions/synthesize_interventions.R')
source('code/interventions/systematic_interventions.R')
source('code/targets/target_msas.R')


IMAGE.DIR = '../Manuscripts/manuscript_1/Annals Submission/revision 1/images/'
library(ggplot2)
THEME = theme(text = element_text(size=11))

N.SIM = 1000
if (1==2)
{
#    intervention.for.sensitivity = intervention.from.code('mi.t2x.p25.s90_23.27')   
#    sensitivity.dfs = make.sensitivity.dfs('mcmc_runs/full_simsets',
#                                           interventions=list(intervention.for.sensitivity),
#                                           n.sim=N.SIM)
#    save(sensitivity.dfs, file='results/sensitivity.dfs.Rdata')
    
    load('results/full/sensitivity.dfs.Rdata')
    
    merged.sensitvity.df = merge.sensitivity.dfs(sensitivity.dfs)
    ranked.sensitvity.df = merge.sensitivity.dfs(sensitivity.dfs, rank=T)
    
    
    prccs = get.locationwise.prccs(sensitivity.dfs)
    save(prccs, file='results/full/prccs.Rdata')
    
    mean.prccs = rowMeans(prccs); mean.prccs = mean.prccs[order(abs(mean.prccs), decreasing = T)]
    
    vars.to.show = names(mean.prccs)[1:10]
    
    ranked.out = order.outcomes.by.ranked.variables(sensitivity.dfs,
                                                    variables=vars.to.show)
    
    ranked.aggregate.change = order.aggregate.change.outcome.by.ranked.variables(sensitivity.dfs,
                                                                                 variables=vars.to.show)

    png(file.path(IMAGE.DIR, 'sensitivity/PRCCs.png'), pointsize=10, width=6.5, height=3.25, res=300, units='in')
    plot.locationwise.prccs(prccs, variables=vars.to.show) + THEME
    dev.off()
    
        png(file.path(IMAGE.DIR, 'sensitivity/high_v_low.png'), pointsize=10, width=6.5, height=4, res=300, units='in')
#    plot.high.vs.low.outcomes(ranked.out)
    plot.high.vs.low.outcomes(ranked.aggregate.change, limits=c(NA,.863)) + THEME
    dev.off()
    
    #not using this
 #   png(file.path(IMAGE.DIR, 'sensitivity/high_v_low v1.png'), pointsize=10, width=6, height=4, res=300, units='in')
 #   plot.locationwise.outcome.diff(ranked.out, variables=vars.to.show) + 
 #       ylab("Difference in Projected Reduction in Incidence\nfor 200 Simulations with Highest Value of\nEach Parameter vs. 200 Simulations with Lowest")
 #   dev.off()
    
    #for text
    get.high.vs.low.outcomes(ranked.out[,1:2,])
    mean.prccs[1:2]
#    get.locationwise.outcome.diff(ranked.out, vars.to.show[1:2], stat=mean)
 #   hl=get.locationwise.outcome.diff(ranked.out, vars.to.show, summarize = F)
  #  hl1 = hl[hl$variable==hl$variable[1],]; hl1=hl1[order(hl1$value),]; hl1$location=msa.names(hl1$location);hl1[1:2,]
   # hl2 = hl[hl$variable==hl$variable[2],]; hl2=hl2[order(hl2$value),]; hl2$location=msa.names(hl2$location);hl2[1:2,]
    
    # for text of high vs low
    hl = get.locationwise.high.low(ranked.out, variables=vars.to.show[1:2], interval.coverage = .95)
    hl = hl[hl$variable==hl$variable[1],]
    greatest.diff.index = (1:dim(hl)[1])[abs(hl$diff)==max(abs(hl$diff))][1]
    
    hl[greatest.diff.index,]
    msa.names(hl$location[greatest.diff.index])
    
    x=prccs[vars.to.show[1],]
    max.prcc.loc = names(x)[abs(x)==max(abs(x))][1]
    msa.names(max.prcc.loc)
    hl[hl$location==max.prcc.loc,]
}


if (1==2)
{
    #test
    sensitivity.dfs = make.sensitivity.dfs('mcmc_runs/full_simsets',
                                         interventions=list(MSM.IDU.1.50.90.YBH.HIGH.X),
                                         n.sim=N.SIM)
    
    merged.sensitvity.df = merge.sensitivity.dfs(sensitivity.dfs)
    ranked.sensitvity.df = merge.sensitivity.dfs(sensitivity.dfs, rank=T)
    ranked.cor = get.outcome.parameter.correlations(ranked.sensitvity.df)
    raw.cor = get.outcome.parameter.correlations(merged.sensitvity.df)
    
    sort(abs(raw.cor), decreasing=T)[1:20]
    sort(abs(ranked.cor), decreasing=T)[1:20]
    
    sorted.variables = names(sort(abs(ranked.cor), decreasing=T))
    
    prccs = get.locationwise.prccs(sensitivity.dfs,
                                   variables=sorted.variables[1:40])
    mean.prccs = rowMeans(prccs); mean.prccs = mean.prccs[order(abs(mean.prccs), decreasing = T)]
    
    vars.to.show = names(mean.prccs)[1:10]
    
    ranked.out = order.outcomes.by.ranked.variables(sensitivity.dfs,
                                                    variables=vars.to.show)
    
    
    png(file.path(IMAGE.DIR, 'sensitivity/PRCCs.png'), pointsize=10, width=6, height=3.25, res=300, units='in')
    plot.locationwise.prccs(prccs, variables=vars.to.show)
    dev.off()
    
    png(file.path(IMAGE.DIR, 'sensitivity/high_v_low.png'), pointsize=10, width=6, height=4, res=300, units='in')
    plot.high.vs.low.outcomes(ranked.out)
    dev.off()
}


##---------------------##
##-- STYLE CONSTANTS --##
##---------------------##

RED = '#C75D4D'
GREEN = '#458B00'
BLUE = '#3278FA'
ORANGE = 'darkorange3'


##--------------------------------------##
##-- HIGH vs LOW (Calculate and Plot) --##
##--------------------------------------##

get.high.vs.low.outcomes <- function(ranked.out,
                                     frac=0.2)
{
    if (length(dim(ranked.out))==2)
        mat = ranked.out
    else
        mat = rowMeans(ranked.out, dims=2)
    
    n.sim = dim(mat)[1]
    low.indices = 1:ceiling(n.sim*frac)
    high.indices = n.sim + 1 - low.indices
    
    df.low = reshape2::melt(mat[low.indices,])
    df.high = reshape2::melt(mat[high.indices,])
    
    df.low$quantile='low'
    df.high$quantile='high'
    
    df = rbind(df.low, df.high)
    df$variable = factor(get.pretty.parameter.names(df$variable),
                         levels=get.pretty.parameter.names(rev(dimnames(mat)[[2]])))
    
    df.low = summarize.for.boxplot(df[df$quantile=='low',])
    df.low$quantile = 'low'
    df.high = summarize.for.boxplot(df[df$quantile=='high',])
    df.high$quantile = 'high'
    df = rbind(df.low, df.high)
    
    attr(df, 'mean') = mean(mat)
    df
}

plot.high.vs.low.outcomes <- function(ranked.out,
                                      frac=0.2,
                                      box.width=0.6,
                                      limits=c(NA,NA))
{
    df = get.high.vs.low.outcomes(ranked.out=ranked.out, frac=frac)
    
    
    palette = pal_jama()(3)
    colors = c(high=palette[3], low=palette[2])
    labels = c(high=paste0("Simulations with highest ", 100*frac, "% of Parameter Values"),
               low=paste0("Simulations with lowest ", 100*frac, "% of Parameter Values"))
    
    ggplot(df) + geom_boxplot(aes(x=variable, middle=stat, lower=quartile.1, upper=quartile.3, ymin=min, ymax=max, fill=quantile),
                              stat='identity', position=position_dodge(box.width), width=box.width) +
        coord_flip() + 
        geom_hline(yintercept = attr(df, 'mean'), linetype='dashed') +
        xlab(NULL) + ylab("Incidence Reduction 2020 to 2030\n(Averaged Across MSAs)") +
        scale_fill_manual(name=NULL,
                          values=colors,
                          labels=labels) + 
        theme(legend.position = 'bottom',
              legend.direction='vertical',
              panel.background=element_blank()) +
        scale_y_continuous(labels=percent, limits = limits)
}


order.aggregate.change.outcome.by.ranked.variables <- function(sensitivity.dfs,
                                                               outcome1='incidence_2020',
                                                               outcome2='incidence_2030',
                                                               variables=NULL)
{
    if (is.null(variables))
        variables = names(sensitivity.dfs)[1][attr(sensitivity.dfs[[1]])]
    
    
    dim.names = list(sim=1:(dim(sensitivity.dfs[[1]])[1]),
                     variable=variables,
                     location=attr(sensitivity.dfs, 'location')
    )
    df1 = sapply(sensitivity.dfs, function(df){
        one.loc = sapply(variables, function(var){
            o = order(df[,var])
            df[o,outcome1]
        })
        one.loc
    })
    dim(df1) = sapply(dim.names, length)
    dimnames(df1) = dim.names
    df1 = rowSums(df1, dims=2)
    
    df2 = sapply(sensitivity.dfs, function(df){
        one.loc = sapply(variables, function(var){
            o = order(df[,var])
            df[o,outcome2]
        })
        one.loc
    })
    
    dim(df2) = sapply(dim.names, length)
    df2 = rowSums(df2, dims=2)
    
    rv = (df1-df2)/df1
}


order.outcomes.by.ranked.variables <- function(sensitivy.dfs,
                                               outcome='delta_2020_2030',
                                               variables=NULL)
{
    if (is.null(variables))
        variables = names(sensivity.dfs)[1][attr(sensitivity.dfs[[1]])]
    
    rv = sapply(sensitivy.dfs, function(df){
        one.loc = sapply(variables, function(var){
            o = order(df[,var])
            df[o,outcome]
        })
        one.loc
    })
    
    dim.names = list(sim=1:(dim(sensitivity.dfs[[1]])[1]),
                     variable=variables,
                     location=attr(sensitivity.dfs, 'location')
                     )
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}

get.locationwise.outcome.diff <- function(ranked.out,
                                          variables=rev(dimnames(ranked.out)[['variable']]),
                                          threshold=0.2,
                                          stat=median,
                                          summarize=T)
{
    n = floor(threshold * dim(ranked.out)[1])
    low = apply(ranked.out[1:n,variables,], 2:3, stat)
    high = apply(ranked.out[dim(ranked.out)[1]-n+1:n,variables,], 2:3, stat)
    
    variables = dimnames(ranked.out)[['variable']]
    var.levels = rev(get.pretty.parameter.names(variables))
    df = reshape2::melt(high-low)
    
    df$variable = factor(get.pretty.parameter.names(df$variable), levels=var.levels)
    
    if (summarize)
        df = summarize.for.boxplot(df, stat=stat)
    
    df
}

get.locationwise.high.low <- function(ranked.out,
                                      variables=rev(dimnames(ranked.out)[['variable']]),
                                      threshold=0.2,
                                      stat=mean,
                                      interval.coverage=0.95)
{
    n = floor(threshold * dim(ranked.out)[1])
    alpha = (1-interval.coverage)/2
    
    low.indices = 1:n
    high.indices = dim(ranked.out)[1]-n+1:n
    
    low = apply(ranked.out[low.indices,variables,], 2:3, stat)
    high = apply(ranked.out[high.indices,variables,], 2:3, stat)
    
    low.lower = apply(ranked.out[low.indices,variables,], 2:3, quantile, alpha)
    low.upper = apply(ranked.out[low.indices,variables,], 2:3, quantile, 1-alpha)
    
    high.lower = apply(ranked.out[high.indices,variables,], 2:3, quantile, alpha)
    high.upper = apply(ranked.out[high.indices,variables,], 2:3, quantile, 1-alpha)
    
    variables = dimnames(ranked.out)[['variable']]
    var.levels = rev(get.pretty.parameter.names(variables))
    df = reshape2::melt(low, value.name='low')
    
    df$variable = factor(get.pretty.parameter.names(df$variable), levels=var.levels)
    
    df$low.lower = as.numeric(low.lower)
    df$low.upper = as.numeric(low.upper)
    df$high = as.numeric(high)
    df$high.lower = as.numeric(high.lower)
    df$high.upper = as.numeric(high.upper)
    df$diff = df$high - df$low
    
    df
}
    


plot.locationwise.outcome.diff <- function(ranked.out,
                                          threshold=0.2,
                                          variables=rev(dimnames(ranked.out)[['variable']]),
                                          fill=pal_jama()(5)[5],
                                          stat=median)
{
    n = floor(threshold * dim(ranked.out)[1])
    
    df = get.locationwise.outcome.diff(ranked.out, variables=variables, threshold=threshold, stat=stat)
    
    ggplot(df) + geom_boxplot(aes(x=variable, middle=stat, lower=quartile.1, upper=quartile.3, ymin=min, ymax=max),
                              stat='identity', fill=fill) +
        geom_hline(yintercept = 0, linetype='dashed') +
        xlab(NULL) + ylab(paste0("Difference Between High vs Low Simulations")) +
        theme(panel.background=element_blank()) + coord_flip()
    
    #    ggplot(df) + geom_boxplot(aes(value, variable), fill=fill) +
    #        geom_vline(xintercept = 0, linetype='dashed') +
    #        xlim(-1,1) +
    #        ylab(NULL) + xlab("Partial Rank Correlation Coefficient (PRCC)") +
    #        theme(panel.background=element_blank())
}

##--------------------------------##
##-- PRCCs (Calculate and Plot) --##
##--------------------------------##

get.locationwise.prccs <- function(sensitivity.dfs,
                                   outcome='delta_2020_2030',
                                   variables=NULL,
                                   verbose=T)
{
    prccs.list = lapply(1:length(sensitivity.dfs), function(i){
        df = sensitivity.dfs[[i]]
        if (verbose)
            print(paste0("Calculating PRCCs for ", 
                         msa.names(attr(sensitivity.dfs, 'location')[i])))
       # pcc(y=df[,outcome],
        #    X=df[,attr(df,'parameter.columns')],
          #  rank=T)$PRCC
        
        if (is.null(variables))
            variables = names(df)[attr(df, 'parameter.columns')]
        calculate.pccs(outcome=outcome,
                       variables=variables,
                       df=df,
                       rank=T)
    })
    
    prccs = unlist(prccs.list)
    if (is.null(variables))
        variables = names(prccs.list[[1]])
    dim.names = list(variable=variables, location=attr(sensitivity.dfs, 'location'))

    dim(prccs) = sapply(dim.names, length)
    dimnames(prccs) = dim.names
    

    prccs
}

plot.locationwise.prccs <- function(prccs,
                                    variables=NULL,
                                    fill=pal_jama()(5)[5],
                                    box.width=0.6)
{
    if (!is.null(prccs))
        prccs = prccs[variables,]
    
    mean.prccs = rowMeans(prccs)
    var.levels = get.pretty.parameter.names(names(sort(abs(mean.prccs), decreasing=F)))
    
    df = reshape2::melt(prccs)
    
    df$variable = factor(get.pretty.parameter.names(df$variable), levels=var.levels)

    df = summarize.for.boxplot(df)
    
    ggplot(df) + geom_boxplot(aes(x=variable, middle=stat, lower=quartile.1, upper=quartile.3, ymin=min, ymax=max),
                              stat='identity', fill=fill, width=box.width) +
        geom_hline(yintercept = 0, linetype='dashed') +
        ylim(-1,1) +
        xlab(NULL) + ylab("Partial Rank Correlation Coefficient") +
        theme(panel.background=element_blank()) + coord_flip()
    
#    ggplot(df) + geom_boxplot(aes(value, variable), fill=fill) +
#        geom_vline(xintercept = 0, linetype='dashed') +
#        xlim(-1,1) +
#        ylab(NULL) + xlab("Partial Rank Correlation Coefficient (PRCC)") +
#        theme(panel.background=element_blank())
}

summarize.for.boxplot <- function(df,
                                  variable.name='variable',
                                  value.name='value',
                                  stat=median)
{
    variables = unique(df[,variable.name])
    values.for.variable = lapply(variables, function(v){df[df[,variable.name]==v,value.name]})
    rv = data.frame(variable=variables,
                    stat=sapply(values.for.variable, stat),
                    quartile.1=sapply(values.for.variable, quantile, probs=0.25),
                    quartile.3=sapply(values.for.variable, quantile, probs=0.75),
                    min=sapply(values.for.variable, min),
                    max=sapply(values.for.variable, max))
    rv
}

##----------------------------------##
##-- QUICK AND DUMB: CORRELATIONS --##
##----------------------------------##

get.outcome.parameter.correlations <- function(df,
                                  outcome='delta_2020_2030',
                                  variables=NULL)
{
    if (is.null(variables))
        variables = names(df)[attr(df, 'parameter.columns')]
    
    sapply(variables, function(one.var){
        cor(df[,one.var], df[,outcome])
    })
}

##--------------------------------##
##-- SETTING UP THE DATA FRAMES --##
##--------------------------------##

make.sensitivity.dfs <- function(dir,
                                interventions,
                                n.sim,
                                msas=TARGET.MSAS,
                                year1=c(2020,2020),
                                year2=c(2025,2030),
                                verbose=T)
{
    dir = file.path(dir)
    
    n.int.times.msa = length(interventions) * length(msas)
    dfs = lapply(1:n.int.times.msa, function(i){
        i.msa = ceiling(i / length(interventions))
        i.int = 1 + (i-1) %% length(interventions)
        
        int = interventions[[i.int]]
        msa = msas[i.msa]
        
        filename = file.path(dir, msa, get.simset.filename(location=msa, intervention=int))
        if (file.exists(filename))
        {
            if (verbose)
                print(paste0("Reading file ", i, " of ", n.int.times.msa, 
                             " (", get.intervention.filename(int), " for ", msa.names(msa)))
            
            
            load(filename)
            
            #multiple count weighted sims and cut off at n.sim
            if (sum(simset@weights)<n.sim)
                stop(paste0("The simset for ", msa.names(msa), " on intervention '",
                            get.intervention.name(int), "' does not have ",
                            n.sim, " simulations"))
            
            indices = unlist(sapply(1:simset@n.sim, function(i){
                rep(i, simset@weights[i])
            }))
            indices = indices[1:n.sim]
            
            sub.df = as.data.frame(simset@parameters[indices,])
            
            for (j in 1:length(year1))
            {
                values = sapply(simset@simulations[1:max(indices)], function(sim){
                    get.incidence.reduction(sim, year1=year1[j], year2=year2[j])[1]
                })
                
                sub.df[[paste0('delta_',year1[j],'_',year2[j])]] = values[indices]
            }
            
            unique.years = unique(c(year1, year2))
            for (year in unique.years)
            {
                values = sapply(simset@simulations[1:max(indices)], function(sim){
                    extract.incidence(sim, years=year, keep.dimensions = 'year', per.population = NA)
                })
                
                sub.df[[paste0('incidence_', year)]] = values[indices]
            }

            attr(sub.df, 'parameter.columns') = 1:simset@n.parameters
            sub.df
        }
        else
        {
            if (verbose)
                print(paste0("Skipping file ", i, " of ", n.int.times.msa, 
                            "(", get.intervention.filename(int), " for ", msa.names(msa)))
            NULL
        }
            
    })
    
    missing.mask = sapply(dfs, is.null)
    dfs = dfs[!missing.mask]
    
    attr(dfs, 'location') = sapply(1:n.int.times.msa, function(i){
        i.msa = ceiling(i / length(interventions))
        msa = msas[i.msa]
        msa})[!missing.mask]
    
    attr(dfs, 'intervention') = sapply(1:n.int.times.msa, function(i){
        i.int = 1 + (i-1) %% length(interventions)
        int = interventions[[i.int]]
        get.intervention.code(int)
    })[!missing.mask]
    
    dfs
}

merge.sensitivity.dfs <- function(dfs, rank=F)
{
    locations = attr(dfs, 'location')
    interventions = attr(dfs, 'intervention')
    
    dfs = lapply(1:length(dfs), function(i){
        df = dfs[[i]]
        
        if (rank)
        {
            for (j in 1:dim(df)[2])
                df[,j] = rank(df[,j])
        }
        
        df$location = locations[i]
        df$intervention = interventions[i]
        
        df
    })
    
    rv = as.data.frame(rbindlist(dfs))
    
    attr(rv, 'parameter.columns') = attr(dfs[[1]], 'parameter.columns')
    rv
}

##--------------------------------##
##-- LOW-LEVEL: CALCULATE PRCCS --##
##--------------------------------##

calculate.pccs <- function(outcome,
                           variables,
                           df,
                           rank=T)
{
    sapply(variables, calculate.one.pcc,
           outcome=outcome,
           variables=variables,
           df=df,
           rank=rank)
}

#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2570191/
calculate.one.pcc <- function(outcome,
                              variable.of.interest,
                              variables,
                              df,
                              rank=T)
{
    if (rank)
    {
        df[,outcome] = rank(df[,outcome])
        df[,variable.of.interest] = rank(df[,variable.of.interest])
    }
    
    ff.outcome = as.formula(paste0(outcome, '~',
                                   paste0(setdiff(variables, variable.of.interest), collapse='+')))
    fit.outcome = lm(ff.outcome, data=df)
    
    ff.var = as.formula(paste0(variable.of.interest, '~',
                                   paste0(setdiff(variables, variable.of.interest), collapse='+')))
    fit.var = lm(ff.var, data=df)
    
    y = df[,outcome] - fit.outcome$fitted.values
    x = df[,variable.of.interest] - fit.var$fitted.values
    
    ybar = mean(y)
    xbar = mean(x)
    
    cov.xy = sum( (x-xbar) * (y-ybar) )
    var.x = sum( (x-xbar)^2 )
    var.y = sum( (y-ybar)^2 )
    
    cov.xy / sqrt(var.x * var.y)
}