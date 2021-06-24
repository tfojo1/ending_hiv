
if (1==2)
{
    tab = make.covid.summary.table(df)
    write.csv(tab, 'code/covid/summaries/jheem_covid_summary_4_cities.csv')
}

library(ggplot2)
library(scales)


OUTCOME.LABELS = c(incidence="Incident Cases (n)",
                   new='Reported Cases (n)',
                   prevalence="Prevalent Cases (n)",
                   incidence.rate="Incidence (per 100,000)",
                   new.rate='Reported Diagnoses (per 100,000)',
                   prevalence.rate="Prevalence (per 100,000)",
                   diagnosed='Serostatus-Aware (%)',
                   suppression='Virally Suppressed (%)')

VARIABLE.LABELS = c(sexual.transmission='Sexual Transmission',
                    testing='HIV Testing',
                    prep='PrEP Uptake',
                    suppression='Viral Suppression')

VARIABLE.CATEGORY.LABELS = c(reduction='Maximal Reduction in XXX due to COVID (%)',
                             time.averaged.reduction='Time-Averaged Reduction in XXX due to COVID (%)',
                             start.normalize.time="Time When XXX Begins Returning to pre-COVID Levels",
                             increase='Rebound Increase in XXX as Pandemic Recedes (%)')

VARIABLE.RANGES = rbind(
    sexual.transmission = c(lower=0, upper=0.5)
)


##------------##
##-- TABLES --##
##------------##

make.covid.summary.table <- function(df,
                                     years=c(2021, 2030),
                                     outcomes=c('incidence','incidence.rate','new','new.rate', 'prevalence', 'prevalence.rate', 'diagnosed','suppression'),
                                     scenarios=c('base','delayed.hiv.care','rebound.sexual.transmission','rebound.sex.delayed.hiv.care'),
                                     msas=unique(df$msa),
                                     duplicate.msa.name=F,
                                     summary.stat = mean,
                                     include.ci=T,
                                     ci.coverage=.95)
{
    rv = NULL
    
    scenarios=c('baseline',scenarios)
    ci.alpha = (1-ci.coverage)/2
    
    for (msa in msas)
    {
        for (scenario in scenarios)
        {
            col.values = character()
            for (outcome in outcomes)
            {
                for (year in years)
                {
                    col.name = paste0(ifelse(scenario=='baseline','','COVID.'), scenario,
                                      "_", outcome, "_", year)
                    values = df[df$msa==msa, col.name]
                    
                    is.pct.outcome = outcome=='diagnosed' || outcome=='suppression'
                    if (is.pct.outcome)
                        values = 100*values
                    
                    one.col.value = format(round(summary.stat(values)), big.mark=',')
                    if (is.pct.outcome)
                        one.col.value = paste0(one.col.value, '%')
                    
                    if (include.ci)
                        one.col.value = paste0(one.col.value, " [",
                                               format(round(quantile(values, probs=ci.alpha)), big.mark=','),
                                               "-",
                                               format(round(quantile(values, probs=1-ci.alpha)), big.mark=','),
                                               "]"
                                              )
                    names(one.col.value) = paste0(OUTCOME.LABELS[outcome], ", ", year)
                    col.values = c(col.values, one.col.value)
                }
            }
            
       #     col.values = c(MSA=ifelse(scenario=='baseline' || duplicate.msa.name, 
        #                              as.character(df$msa.name[df$msa==msa][1]), ''),
         #                  Scenario=as.character(COVID.SCENARIO.NAMES[scenario]),
          #                 col.values)
          
            scenario.value = as.character(COVID.SCENARIO.NAMES[scenario])
            names(scenario.value) = 'MSA/Scenario'
            col.values = c(scenario.value, col.values)
            
            if (scenario=='baseline')
            {
                header.row = col.values
                header.row[] = ''
                header.row[1] = as.character(df$msa.name[df$msa==msa][1])
                
                rv = rbind(rv,
                           as.data.frame(matrix(header.row, nrow=1, dimnames=list(NULL, names(col.values)))))
            }
            
            rv = rbind(rv,
                       as.data.frame(matrix(col.values, nrow=1, dimnames=list(NULL, names(col.values)))))
        }
    }
    
    rv
}

##-------------##
##-- FIGURES --##
##-------------##

make.variable.vs.outcome.figure <- function(variable=c('sexual.transmission','testing','prep','suppression')[1],
                                            outcome=c('incidence','new','prevalence','diagnosed')[1],
                                            variable.category=c('reduction','time.averaged.reduction','start.normalize.time','increase')[1],
                                            scenario=c('base','delayed.hiv.care','increased.sexual.transmission')[1],
                                            df,
                                            msa=NULL,
                                            years=2020:2030,
                                            outcome.as.percent=T,
                                            point.size=3,
                                            point.shape=19,
                                            alpha=0.8,
                                            add.smoother = T,
                                            label.rho = T)
{
    if (!is.null(msa))
        df = df[df$msa==msa,]
    
    plot.df = data.frame(
        delta=get.delta.outcome(df, outcome=outcome, scenario=scenario, years=years, as.percent=outcome.as.percent),
        variable = df[,paste0(variable, '.', variable.category)],
        msa=df$msa.name
    )
    
    x.label = gsub("XXX", VARIABLE.LABELS[variable], VARIABLE.CATEGORY.LABELS[variable.category])
    y.label = paste0("Change in ", OUTCOME.LABELS[outcome],
                     ifelse(outcome.as.percent, " (%)", ""), 
                     "\n (COVID - baseline)")
    
    rv = ggplot(plot.df) + 
        geom_point(aes(x=variable, y=delta), size=point.size, shape=point.shape, alpha=alpha) + 
        xlab(x.label) + 
        ylab(y.label)
    
    if (outcome.as.percent)
        rv = rv + scale_y_continuous(labels = percent)
    
    if (variable.category=='reduction' || variable.category=='time.averaged.reduction')
        rv = rv + scale_x_continuous(labels = percent)
    
    if (length(unique(plot.df$msa))>1)
        rv = rv + facet_wrap(~msa)
    
    if (add.smoother)
        rv = rv + geom_smooth(data=plot.df, aes(x=variable, y=delta))
    
    if (label.rho)
    {
        msas = unique(plot.df$msa)
        rho.df = data.frame(msa=msas,
                            rho=sapply(msas, function(msa){
                                mask = plot.df$msa == msa
                                cor(plot.df$variable[mask], plot.df$delta[mask], method='spearman')
                            }),
                            x = max(plot.df$variable),
                            y = max(plot.df$delta)
                            )
        rho.df$label = paste0("Spearman's rho = ", round(rho.df$rho, 2))
        
        rv = rv + geom_label(data=rho.df, aes(x,y,label=label), vjust='top', hjust='right')
    }
    
    rv    
}

make.one.variable.reduction.vs.time.figure <- function(variable=c('sexual.transmission','testing','prep','suppression')[1],
                                                       outcome=c('incidence','new','prevalence','diagnosed')[1],
                                                       df,
                                                       msa=NULL,
                                                       years=2020:2030,
                                                       outcome.as.percent=T,
                                                       averted.color = 'green4',
                                                       excess.color = 'red2',
                                                       no.change.color = 'yellow',
                                                       point.size=4,
                                                       point.shape=15,
                                                       alpha=0.6,
                                                       use.tiles=T)
{
    if (!is.null(msa))
        df = df[df$msa==msa,]
    
    plot.df = data.frame(
        delta=get.delta.outcome(df, outcome=outcome, years=years, as.percent=outcome.as.percent),
        start.normalize.time = df[,paste0(variable, '.start.normalize.time')],
        reduction = df[,paste0(variable, '.reduction')],
        msa = df$msa.name
    )
    
    color.scale.title = paste0("Change in ", OUTCOME.LABELS[outcome],
                               ifelse(outcome.as.percent, " (%)", ""), 
                               "\n (COVID - baseline)")
    
    if (use.tiles)
    {
        tile.df = make.tile.df()
        rv = ggplot(plot.df) +
            geom_tile(aes(x=x.bin, y=y.bin, color=delta))
    }
    else
    {
        rv = ggplot(plot.df) + 
            geom_point(aes(x=start.normalize.time, y=reduction, color=delta), shape=point.shape, size=point.size, alpha=alpha) +
            scale_y_continuous(labels = percent)
    }
    
    rv = rv +
        xlab(paste0("Time (year) When ", VARIABLE.LABELS[variable]," Begins to Normalize")) + 
        ylab(paste0("Reduction (%) in ", VARIABLE.LABELS[variable]," Due to COVID"))
    
    if (outcome.as.percent)
        rv = rv + scale_color_gradient2(name=color.scale.title,
                                        labels = percent,
                                        low=averted.color, mid=no.change.color, high=excess.color)
    else
        rv = rv + scale_color_gradient2(name=color.scale.title,
                                        low=averted.color, mid=no.change.color, high=excess.color)
    
    if (length(unique(plot.df$msa))>1)
        rv = rv + facet_wrap(~msa)
    
    rv
}

make.two.variable.reduction.figure <- function(variable1=c('sexual.transmission','testing','prep','suppression')[1],
                                               variable2=c('sexual.transmission','testing','prep','suppression')[4],
                                               variable1.category=c('reduction','time.averaged.reduction','start.normalize.time','increase')[1],
                                               variable2.category=c('reduction','time.averaged.reduction','start.normalize.time','increase')[1],
                                               scenario=c('base','delayed.hiv.care','increased.sexual.transmission')[1],
                                               outcome=c('incidence','new','prevalence','diagnosed')[1],
                                               df,
                                               msa=NULL,
                                               years=2020:2025,
                                               outcome.as.percent=T,
                                               averted.color = 'green4',
                                               excess.color = 'red2',
                                               no.change.color = 'white',
                                               point.size=4,
                                               point.shape=15,
                                               alpha=0.6,
                                               use.tiles=T,
                                               show.tile.n=F,
                                               outcome.tile.stat=mean,
                                               bin.width=0.05,
                                               min.change=-0.5,
                                               max.change=0.5)
{
    if (!is.null(msa))
        df = df[df$msa==msa,]
    
    var1.name = paste0(variable1, ".", variable1.category)
    var2.name = paste0(variable2, ".", variable2.category)
    
    plot.df = data.frame(
        delta=get.delta.outcome(df, outcome=outcome, scenario=scenario, years=years, as.percent=outcome.as.percent),
        reduction1 = df[,var1.name],
        reduction2 = df[,var2.name],
        msa=df$msa.name
    )
    
    color.scale.title = paste0("Change in ", OUTCOME.LABELS[outcome],
                               ifelse(outcome.as.percent, " (%)", ""), 
                               "\n (COVID - baseline)")
    
    if (use.tiles)
    {
        tile.df = make.tile.df(plot.df,
                               x.name = 'reduction1',
                               y.name = 'reduction2',
                               outcome = 'delta',
                               outcome.stat = outcome.tile.stat,
                               separate.by = 'msa',
                               x.binwidth = bin.width,
                               y.binwidth = bin.width,
                               x.is.percent = T,
                               y.is.percent = T)
    print(round(range(tile.df$delta),2))    
        tile.df$delta = pmax(min.change, pmin(max.change, tile.df$delta))
        
        rv = ggplot(tile.df) +
            geom_tile(aes(x.bin, y.bin, fill=delta))
        
        if (show.tile.n)
            rv = rv + geom_text(aes(x.bin, y.bin, label=n))
        
        x.bin.names = attr(tile.df, 'x.bin.names')
        y.bin.names = attr(tile.df, 'y.bin.names')
        
        rv = rv +
            scale_x_continuous(breaks=1:length(x.bin.names), labels=x.bin.names) + 
            scale_y_continuous(breaks=1:length(y.bin.names), labels=y.bin.names) + 
            theme(axis.text.x=element_text(angle = 45, hjust = 1))
    }
    else
    {
        plot.df$delta = pmax(min.change, pmin(max.change, plot.df$delta))
        
        rv = ggplot(plot.df) + 
            geom_point(aes(x=reduction1, y=reduction2, color=delta), shape=point.shape, size=point.size, alpha=alpha) +
            scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent)
    }
    
    x.label = gsub("XXX", VARIABLE.LABELS[variable1], VARIABLE.CATEGORY.LABELS[variable1.category])
    y.label = gsub("XXX", VARIABLE.LABELS[variable2], VARIABLE.CATEGORY.LABELS[variable2.category])
    
    rv = rv + 
        xlab(x.label) +
        ylab(y.label) +
        theme(panel.background = element_blank())
    
    
    if (use.tiles)
        scale.color.fn = scale_fill_gradient2
    else
        scale.color.fn = scale_color_gradient2
    
    if (outcome.as.percent)
        rv = rv + scale.color.fn(name=color.scale.title,
                                 labels = percent,
                                 limits=c(min.change,max.change),#c(-1,MAX.FOLD.INCREASE-1), 
                                 midpoint=0,
                                 low=averted.color, mid=no.change.color, high=excess.color)
    else
        rv = rv + scale.color.fn(name=color.scale.title,
                                 limits=c(min.change, max.change), midpoint=0,
                                 low=averted.color, mid=no.change.color, high=excess.color)
    
    if (length(unique(plot.df$msa))>1)
        rv = rv + facet_wrap(~msa)
    
    rv = rv + theme(legend.position = 'bottom')
    
    rv
}

##-- HELPER FUNCTIONS --##

get.delta.outcome <- function(df,
                              outcome,
                              scenario,
                              years,
                              as.percent)
{
    baseline.colnames = paste0('baseline_', outcome, '_', years)
    covid.colnames = paste0('COVID.', scenario, '_', outcome, '_', years)
    
    if (length(years)==1)
    {
        baseline = df[,baseline.colnames]
        covid = df[,covid.colnames]
    }
    else
    {
        baseline = rowSums(df[,baseline.colnames])
        covid = rowSums(df[,covid.colnames])
    }
    
    rv = covid - baseline
    if (as.percent)
        rv = rv / baseline# * 100
    
    rv
}

##------------------##
##-- PLOT HELPERS --##
##------------------##

make.tile.df <- function(df,
                         x.name,
                         y.name,
                         outcome,
                         outcome.stat=mean,
                         x.binwidth=0.05,
                         y.binwidth=0.05,
                         separate.by='msa',
                         x.is.percent=T,
                         y.is.percent=T)
{
    x.range = c(x.binwidth * floor(min(df[,x.name]) / x.binwidth),
                x.binwidth * ceiling(max(df[,x.name]) / x.binwidth))
    y.range = c(y.binwidth * floor(min(df[,y.name]) / y.binwidth),
                y.binwidth * ceiling(max(df[,y.name]) / y.binwidth))
    
    x.n.bins = (x.range[2] - x.range[1]) / x.binwidth
    y.n.bins = (y.range[2] - y.range[1]) / y.binwidth
    
    x.uppers = x.binwidth * (1:x.n.bins)
    x.lowers = x.uppers - x.binwidth
    
    orig.x.uppers = x.uppers = x.binwidth * (1:x.n.bins)
    x.lowers = x.uppers - x.binwidth
    x.uppers[x.n.bins] = Inf
    
    orig.y.uppers = y.uppers = y.binwidth * (1:y.n.bins)
    y.lowers = y.uppers - y.binwidth
    y.uppers[y.n.bins] = Inf
    
    df$x.bin = sapply(df[,x.name], function(x){
        (1:x.n.bins)[x>=x.lowers & x<x.uppers][1]
    })
    
    df$y.bin = sapply(df[,y.name], function(y){
        (1:y.n.bins)[y>=y.lowers & y<y.uppers][1]
    })
    
    rv = NULL
    unique.splits = unique(df[,separate.by])
    for (split in unique.splits)
    {
        for (x.bin in 1:x.n.bins)
        {
            for (y.bin in 1:y.n.bins)
            {
                mask = df[,separate.by] == split & df$x.bin == x.bin & df$y.bin == y.bin
                if (any(mask))
                {
                    rv = rbind(rv,
                               data.frame(split=split,
                                          x.bin=as.integer(x.bin),
                                          y.bin=as.integer(y.bin),
                                          outcome=outcome.stat(df[mask,outcome]),
                                          n=sum(mask)
                                          ))
                }
            }
        }
    }
    
    names(rv)[names(rv)=='outcome'] = outcome
    names(rv)[names(rv)=='split'] = separate.by
    
    attr(rv, 'x.bin.lowers') = x.lowers
    attr(rv, 'x.bin.uppers') = x.uppers
    attr(rv, 'y.bin.lowers') = y.lowers
    attr(rv, 'y.bin.uppers') = y.uppers
    
    if (x.is.percent)
    {
        x.lowers = paste0(100*x.lowers, '%')
        x.uppers = paste0(100*orig.x.uppers, '%')
    }
 
    if (y.is.percent)
    {
        y.lowers = paste0(100*y.lowers, '%')
        y.uppers = paste0(100*orig.y.uppers, '%')
    }
    
    attr(rv, 'x.bin.names') = paste0(x.lowers, ' to ', x.uppers)
    attr(rv, 'y.bin.names') = paste0(y.lowers, ' to ', y.uppers)
    
    rv
}