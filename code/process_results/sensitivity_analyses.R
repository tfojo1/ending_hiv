library(data.table)
library(sensitivity)
library(reshape2)
library(ggsci)

source("code/process_results/pretty_formatting.R")

source('code/interventions/synthesize_interventions.R')


IMAGE.DIR = 'results/figures'

if (1==2)
{
    #test
    sensitivity.dfs = make.sensitivity.dfs('mcmc_runs/visualization_simsets',
                                         interventions=list(MSM.IDU.1.50.90.YBH.HIGH.X),
                                         n.sim=80)
    
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

plot.high.vs.low.outcomes <- function(ranked.out,
                                      frac=0.2)
{
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
    
    palette = pal_jama()(3)
    colors = c(high=palette[3], low=palette[2])
    labels = c(high=paste0("Simulations with highest ", 100*frac, "% of Parameter Values"),
               low=paste0("Simulations with lowest ", 100*frac, "% of Parameter Values"))
    
    ggplot(df) + geom_boxplot(aes(value, variable, fill=quantile)) +
        geom_vline(xintercept = mean(mat), linetype='dashed') +
        ylab(NULL) + xlab("Incidence Reduction 2020 to 2030\n(Averaged Across MSAs)") +
        scale_fill_manual(name=NULL,
                          values=colors,
                          labels=labels) + 
        theme(legend.position = 'bottom',
              legend.direction='vertical',
              panel.background=element_blank()) +
        scale_x_continuous(labels=percent)
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
    dim.names = list(variable=variables, location=attr(sensitivity.dfs, 'location'))
    dim(prccs) = sapply(dim.names, length)
    dimnames(prccs) = dim.names
    

    prccs
}

plot.locationwise.prccs <- function(prccs,
                                    variables=NULL,
                                    fill=pal_jama()(5)[5])
{
    if (!is.null(prccs))
        prccs = prccs[variables,]
    
    mean.prccs = rowMeans(prccs)
    var.levels = get.pretty.parameter.names(names(sort(abs(mean.prccs), decreasing=F)))
    
    df = reshape2::melt(prccs)
    
    df$variable = factor(get.pretty.parameter.names(df$variable), levels=var.levels)
        
    ggplot(df) + geom_boxplot(aes(value, variable), fill=fill) +
        geom_vline(xintercept = 0, linetype='dashed') +
        xlim(-1,1) +
        ylab(NULL) + xlab("Partial Rank Correlation Coefficient (PRCC)") +
        theme(panel.background=element_blank())
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
                                msas=get.hiv.burden()$CBSA,
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
                    red = get.incidence.reduction(sim, year1=year1[j], year2=year2[j])[1]
                })
                
                sub.df[[paste0('delta_',year1[j],'_',year2[j])]] = values[indices]
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