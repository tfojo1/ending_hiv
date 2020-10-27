library(data.table)
library(sensitivity)

if (1==2)
{
    #test
    sensitivity.dfs = make.sensitivity.df('mcmc_runs/visualization_simsets',
                                         interventions=list(MSM.IDU.1.50.90.YBH.HIGH.X))
}

get.locationwise.prccs <- function(sensitivity.dfs,
                                   outcome='delta_2020_2030',
                                   variables=NULL,
                                   verbose=T)
{
    prccs = lapply(1:length(sensitivity.dfs), function(i){
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
    
    prcc.dfs
}

make.sensitivity.df <- function(dir,
                                interventions,
                                msas=get.hiv.burden()$CBSA,
                                year1=c(2020,2020),
                                year2=c(2025,2030),
                                verbose=T,
                                merge=F)
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
            if (merge)
                sub.df = data.frame(location=msa,
                                    intervention=get.intervention.code(int),
                                    as.data.frame(simset@parameters))
            else
                sub.df = as.data.frame(simset@parameters)
            
            for (j in 1:length(year1))
            {
                sub.df[[paste0('delta_',year1[j],'_',year2[j])]] =
                    sapply(simset@simulations, function(sim){
                        red = get.incidence.reduction(sim, year1=year1[j], year2=year2[j])[1]
                    })
            }
            
            
            attr(sub.df, 'parameter.columns') = 1:simset@n.sim + 2*(!merge)
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
    
    if (merge)
    {
        df = rbindlist(dfs)
    
        df
    }
    else
    {
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
}

##-----------##
##-- PRCCS --##
##-----------##

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