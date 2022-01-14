library(ggplot2)
library(reshape2)
library(ggsci)
library(ggrepel)
library(scales)


##---------------##
##-- CONSTANTS --##
##---------------##

COVID.SCENARIO.NAMES = c(
    baseline = 'Absent COVID',
    noint = "Absent COVID",
    covid.rapid.resumption = "Rapid Resumption of Care",
    covid.delayed = "Prolonged Barriers to Care",
    covid.rapid.resumption.mobility = "Rapid Resumption of Care",
    covid.delayed.mobility = "Prolonged Barriers to Care"
)

COVID.OUTCOME.NAMES = c(incidence="Incident Cases",
                        new='Reported Cases',
                        prevalence="Prevalent Cases (n)",
                        incidence.rate="Incidence (per 100,000)",
                        new.rate='Reported Diagnoses (per 100,000)',
                        prevalence.rate="Prevalence (per 100,000)",
                        diagnosed='Serostatus-Aware (%)',
                        suppression='Virally Suppressed (%)')

BLANK.THEME = theme(
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA))

##-----------##
##-- PLOTS --##
##-----------##

PCT.VARIABLES = c('suppression')
make.covid.scatterplot <- function(results=outcomes.arr,
                                   params=parameters,
                                   locations=dimnames(results)[['location']],
                                   aggregate.locations = T,
                                   aggregate.simulations.fn = NULL,
                                   var1='suppression.reduction',
                                   var2='incidence',
                                   var1.years=2020:2025,
                                   var2.years=2020:2025,
                                   scenario1='base',
                                   scenario2=scenario1,
                                   subtract.scenario1=NA,
                                   subtract.relative1=T,
                                   subtract.scenario2='baseline',
                                   subtract.relative2=T,
                                   cor.method='spearman',
                                   #size by
                                   size.by=NULL,
                                   size.by.years=2019,
                                   size.by.scenario='baseline',
                                   #style arguments
                                   point.size=5,
                                   point.size.range=c(2,10),
                                   point.fill='gray',
                                   point.alpha=0.4,
                                   label.rho=T,
                                   label.rho.size=7,
                                   label.rho.hjust='middle',
                                   label.rho.vjust='center',
                                   label.locations=F,
                                   add.smoother = T,
                                   highlight.labeled=T,
                                   print.df=F)
{
    x.as.pct = (!is.na(subtract.scenario1) && subtract.relative1 ) ||
        any(var1 == PCT.VARIABLES) ||
        any(dimnames(params)[[2]]==var1)
    x.force.plus.sign = !is.na(subtract.scenario1)
    y.as.pct = (!is.na(subtract.scenario2) && subtract.relative2 ) ||
        any(var2 == PCT.VARIABLES) ||
        any(dimnames(params)[[2]]==var2)
    y.force.plus.sign = !is.na(subtract.scenario2)
    
    values1 = get.variable(results=results,
                           locations=locations,
                           params=params,
                           var.name = var1,
                           years=var1.years,
                           scenario=scenario1,
                           aggregate.locations = aggregate.locations)
    
    if (!is.null(subtract.scenario1) && !is.na(subtract.scenario1))
    {
        relative.to1 = get.variable(results=results,
                                    locations=locations,
                                   params=params,
                                   var.name = var1,
                                   years=var1.years,
                                   scenario=subtract.scenario1,
                                   aggregate.locations = aggregate.locations)
        
        values1 = values1 - relative.to1
        if (subtract.relative1)
            values1 = values1/relative.to1
    }
    
    values2 = get.variable(results=results,
                           locations=locations,
                           params=params,
                           var.name = var2,
                           years=var2.years,
                           scenario=scenario2,
                           aggregate.locations = aggregate.locations)
    
    if (!is.null(subtract.scenario2) && !is.na(subtract.scenario2))
    {
        relative.to2 = get.variable(results=results,
                                    locations=locations,
                                    params=params,
                                    var.name = var2,
                                    years=var2.years,
                                    scenario=subtract.scenario2,
                                    aggregate.locations = aggregate.locations)
        
        values2 = values2 - relative.to2
        if (subtract.relative2)
            values2 = values2/relative.to2
    }
    
    if (!is.null(size.by))
    {
        size.by.values = get.variable(results=results,
                                      locations=locations,
                                      params=params,
                                      var.name = size.by,
                                      years=size.by.years,
                                      scenario=size.by.scenario,
                                      aggregate.locations=aggregate.locations)
    }
    else
        size.by.values = NULL
    
    if (is.null(aggregate.simulations.fn))
    {
        df = reshape2::melt(values1, value.name = 'value1')
        df$value2 = as.numeric(values2)
        
        if (!is.null(size.by.values))
            df$size.by = as.numeric(size.by.values)
    }
    else
    {
        df = data.frame(
            location = dimnames(values1)$location,
            value1 = apply(values1, 'location', aggregate.simulations.fn),
            value2 = apply(values2, 'location', aggregate.simulations.fn)
        )
        
        if (!is.null(size.by.values))
            df$size.by = apply(size.by.values, 'location', aggregate.simulations.fn)
    }
    
    
    if (print.df)
    {
        o = order(df$value2)
        print(df[o,])
    }
    
    if (is.null(size.by))
      rv = ggplot(df, aes(value1, value2)) +
            geom_point(size=point.size, shape=21, fill=point.fill, alpha=point.alpha)
    else
      rv = ggplot(df, aes(value1, value2, size=size.by)) +
            geom_point(shape=21, fill=point.fill, alpha=point.alpha) +
            scale_size_continuous(range=point.size.range) + BLANK.THEME
    
     rv = rv + BLANK.THEME

    if (!aggregate.locations && is.null(aggregate.simulations.fn) && length(locations)>1)    
        rv = rv + facet_wrap(~location)
    
    if (add.smoother)
        rv = rv + geom_smooth(data=df, aes(x=value1, y=value2))
    
    if (label.rho)
    {
        if (label.rho.hjust=='right')
            x.fn = max
        else
            x.fn = min
        
        if (label.rho.vjust=='top')
            y.fn = max
        else
            y.fn = min
        
        locations = unique(df$location)
        rho.df = data.frame(location=locations,
                            rho=sapply(locations, function(loc){
                                mask = df$location == loc
                                cor(df$value1[mask], df$value2[mask], method=cor.method)
                            }),
                            x = x.fn(df$value1),
                            y = y.fn(df$value2)
        )
        rho.df$label = paste0("Spearman's rho = ", 100*round(rho.df$rho, 2), "%")
        
        rv = rv + geom_text(data=rho.df, aes(x,y,label=label), vjust=label.rho.vjust, hjust=label.rho.hjust, size=label.rho.size)
        
    }
     
     if (label.locations)
         rv = rv + geom_text(aes(label=location), hjust=0)
    
    #-- Axis Label Formatting --#
    force.plus = function(x){
        mask = !is.na(x) & x>0
        x = as.character(x)
        x[mask] = paste0("+", x[mask])
        x
    } 
    force.plus.percent = function(x){
        mask = !is.na(x) & x>0
        x = percent(x)
        x[mask] = paste0("+", x[mask])
        x
    }
    
    if (x.as.pct && x.force.plus.sign)
        rv = rv + scale_x_continuous(labels = force.plus.percent)
    else if (x.as.pct)
        rv = rv + scale_x_continuous(labels = percent)
    else if (x.force.plus.sign)
        rv = rv + scale_x_continuous(labels = force.plus)
    
    if (y.as.pct && y.force.plus.sign)
        rv = rv + scale_y_continuous(labels = force.plus.percent)
    else if (y.as.pct)
        rv = rv + scale_y_continuous(labels = percent)
    else if (y.force.plus.sign)
        rv = rv + scale_y_continuous(labels = force.plus)
    
    
    rv    
}

make.covid.binned.boxplot <- function(results=outcomes.arr,
                                   params=parameters,
                                   locations=dimnames(results)[['location']],
                                   aggregate.locations = T,
                                   
                                   #binning arguments
                                   var1.bin.width=0.05,
                                   summary.stat=median,
                                   interval1.coverage = 0.5,
                                   interval2.coverage = 0.95,
                                   
                                   #other arguments
                                   var1='suppression.reduction',
                                   var2='incidence',
                                   var1.years=2020:2025,
                                   var2.years=2020:2025,
                                   scenario1='base',
                                   scenario2=scenario1,
                                   subtract.scenario1=NA,
                                   subtract.relative1=T,
                                   subtract.scenario2='baseline',
                                   subtract.relative2=T,
                                   cor.method='spearman',
                                   #style arguments
                                   boxplot.fill='gray',
                                   label.rho=T,
                                   label.rho.size=7,
                                   label.rho.hjust='middle',
                                   label.rho.vjust='center',
                                   highlight.labeled=T,
                                   x.angle=40,
                                   ylim=c(NA,NA))
{
    x.as.pct = (!is.na(subtract.scenario1) && subtract.relative1 ) ||
        any(var1 == PCT.VARIABLES) ||
        any(dimnames(params)[[2]]==var1)
    x.force.plus.sign = !is.na(subtract.scenario1)
    y.as.pct = (!is.na(subtract.scenario2) && subtract.relative2 ) ||
        any(var2 == PCT.VARIABLES) ||
        any(dimnames(params)[[2]]==var2)
    y.force.plus.sign = !is.na(subtract.scenario2)
    
    values1 = get.variable(results=results,
                           locations=locations,
                           params=params,
                           var.name = var1,
                           years=var1.years,
                           scenario=scenario1,
                           aggregate.locations = aggregate.locations)
    
    if (!is.null(subtract.scenario1) && !is.na(subtract.scenario1))
    {
        relative.to1 = get.variable(results=results,
                                    locations=locations,
                                    params=params,
                                    var.name = var1,
                                    years=var1.years,
                                    scenario=subtract.scenario1,
                                    aggregate.locations = aggregate.locations)
        
        values1 = values1 - relative.to1
        if (subtract.relative1)
            values1 = values1/relative.to1
    }
    
    values2 = get.variable(results=results,
                           locations=locations,
                           params=params,
                           var.name = var2,
                           years=var2.years,
                           scenario=scenario2,
                           aggregate.locations = aggregate.locations)
    
    if (!is.null(subtract.scenario2) && !is.na(subtract.scenario2))
    {
        relative.to2 = get.variable(results=results,
                                    locations=locations,
                                    params=params,
                                    var.name = var2,
                                    years=var2.years,
                                    scenario=subtract.scenario2,
                                    aggregate.locations = aggregate.locations)
        
        values2 = values2 - relative.to2
        if (subtract.relative2)
            values2 = values2/relative.to2
    }
    
    range1 = c(var1.bin.width * floor(min(values1) / var1.bin.width),
               var1.bin.width * ceiling(max(values1) / var1.bin.width))
    
    x.n.bins = (range1[2] - range1[1]) / var1.bin.width

    orig.x.uppers = x.uppers = var1.bin.width * (1:x.n.bins)
    x.lowers = x.uppers - var1.bin.width
    x.uppers[x.n.bins] = Inf

    x.bins = apply(values1, 1:2, function(x){
        (1:x.n.bins)[x>=x.lowers & x<x.uppers][1]
    })
    
    alpha1 = (1-interval1.coverage)/2
    alpha2 = (1-interval2.coverage)/2
    df = as.data.frame(t(sapply(1:x.n.bins, function(bin){
        mask = values1 >= x.lowers[bin] & values1<x.uppers[bin]
        vals2 = values2[mask]
        
        rv = quantile(vals2, probs=c(alpha1, 1-alpha1, alpha2, 1-alpha2))
        names(rv) = c('interval1.lower', 'interval1.upper', 'interval2.lower', 'interval2.upper')
        rv = c(stat=summary.stat(vals2), rv)
    })))
    
    if (x.as.pct)
    {
        x.lowers = paste0(100*x.lowers, '%')
        x.uppers = paste0(100*orig.x.uppers, '%')
    }
    x.bin.names = paste0(x.lowers, ' to ', x.uppers)
    df$bin = factor(x.bin.names, levels=x.bin.names)

    rv = ggplot(df) + 
        geom_boxplot(aes(x=bin, middle=stat, group=bin,
                         lower=interval1.lower, upper=interval1.upper,
                         ymin=interval2.lower, ymax=interval2.upper),
                     stat='identity', fill=boxplot.fill) +
        BLANK.THEME + theme(axis.text.x = element_text(angle=x.angle, hjust=1))


    if (label.rho && F)
    {
        if (label.rho.hjust=='right')
            x.fn = max
        else
            x.fn = min
        
        if (label.rho.vjust=='top')
            y.fn = max
        else
            y.fn = min
        
        locations = unique(df$location)
        rho.df = data.frame(rho=sapply(locations, function(loc){
                                mask = df$location == loc
                                cor(df$value1[mask], df$value2[mask], method=cor.method)
                            }),
                            x = x.fn(df$value1),
                            y = y.fn(df$value2)
        )
        rho.df$label = paste0("Spearman's rho = ", 100*round(rho.df$rho, 2), "%")
        
        rv = rv + geom_text(data=rho.df, aes(x,y,label=label), vjust=label.rho.vjust, hjust=label.rho.hjust, size=label.rho.size)
        
    }
    
    #-- Axis Label Formatting --#
    force.plus = function(x){
        mask = !is.na(x) & x>0
        x = as.character(x)
        x[mask] = paste0("+", x[mask])
        x
    } 
    force.plus.percent = function(x){
        mask = !is.na(x) & x>0
        x = percent(x)
        x[mask] = paste0("+", x[mask])
        x
    }
    
    

    if (y.as.pct && y.force.plus.sign)
        rv = rv + scale_y_continuous(labels = force.plus.percent, limits=ylim)
    else if (y.as.pct)
        rv = rv + scale_y_continuous(labels = percent, limits=ylim)
    else if (y.force.plus.sign)
        rv = rv + scale_y_continuous(labels = force.plus, limits=ylim)
    
    
    rv    
}


make.correlation.scatterplot <- function(results=outcomes.arr,
                                         params=parameters,
                                         locations=dimnames(results)[['location']],
                                         loc.names = location.names,
                                         outcome='incidence',
                                         outcome.years=2019:2025,
                                         var1='sexual.transmission.reduction',
                                         var2='suppression.reduction',
                                         correlate.var1=T,
                                         correlate.var2=T,
                                         var1.year=2019,
                                         var2.year=2019,
                                         scenario='base',
                                         subtract.scenario='baseline',
                                         subtract.relative=T,
                                         cor.method='spearman',
                                         #style arguments
                                         point.alpha=0.4,
                                         point.fill='blue',
                                         label.locations=character(),
                                         point.size.range=c(2,10),
                                         label.size=5,
                                         highlight.labeled=T,
                                         ylim=c(NA,NA))
{
    x.as.pct = correlate.var1 || 
        any(PCT.VARIABLES==var1) ||
        any(dimnames(params)[[2]]==var1)
    y.as.pct = correlate.var2 || 
        any(PCT.VARIABLES==var2) ||
        any(dimnames(params)[[2]]==var2)
    
    values1 = get.variable(results=results,
                           params=params,
                           locations=locations,
                           var.name = var1,
                           years=var1.year,
                           scenario=scenario,
                           aggregate.locations = F)
    
    values2 = get.variable(results=results,
                           params=params,
                           locations=locations,
                           var.name = var2,
                           years=var2.year,
                           scenario=scenario,
                           aggregate.locations = F)
    
    abs.outcome = get.variable(results=results,
                           params=params,
                           locations=locations,
                           var.name = outcome,
                           years=outcome.years,
                           scenario=scenario,
                           aggregate.locations = F)
    
    if (!is.null(subtract.scenario) && !is.na(subtract.scenario))
    {
        relative.to = get.variable(results=results,
                                   params=params,
                                   locations=locations,
                                   var.name = outcome,
                                   years=outcome.years,
                                   scenario=subtract.scenario,
                                   aggregate.locations = F)
        
        outcome = abs.outcome - relative.to
     #   if (subtract.relative)
      #      outcome = outcome/relative.to
    }
    else
        outcome = abs.outcome
    
    if (correlate.var1)
        correlations.1 = sapply(locations, function(loc){
            cor(outcome[loc,], values1[loc,], method=cor.method)
        })
    else
        correlations.1 = sapply(locations, function(loc){
            mean(values1[loc,])
        })
        
    if (correlate.var2)
        correlations.2 = sapply(locations, function(loc){
            cor(outcome[loc,], values2[loc,], method=cor.method)
        })
    else
        correlations.2 = sapply(locations, function(loc){
            mean(values2[loc,])
        })
    
    mean.outcome = sapply(locations, function(loc){
        mean(outcome[loc,])
    })
    
    if (!is.null(subtract.scenario) && !is.na(subtract.scenario))
    {
      
        if (subtract.relative)
            outcome = outcome/relative.to
    }
    
    df = data.frame(location = locations,
                    cor1 = correlations.1,
                    cor2 = correlations.2,
                    mean.outcome = mean.outcome
                    )
    df$location.name = loc.names[as.character(df$location)]
    df$location.name[!sapply(df$location, function(loc){any(loc==label.locations)})] = NA
    df$label.alpha=1
  #  df$label.alpha = 0
   # df$label.alpha[sapply(df$location, function(loc){any(loc==label.locations)})] = 1

    rv = ggplot(df, aes(x=cor1, y=cor2, size=mean.outcome)) +
        geom_point(shape=21, fill=point.fill, alpha=point.alpha) +
        geom_text(aes(label=location.name), size=label.size, hjust='left') + 
        scale_size_continuous(range=point.size.range) + BLANK.THEME
    
    if (highlight.labeled)
        rv = rv + geom_point(data=df[!is.na(df$location.name),], shape=23, fill='red')
    
    if (x.as.pct)
        rv = rv + scale_x_continuous(labels=percent)
    if (y.as.pct)
        rv = rv + scale_y_continuous(labels=percent, limits = ylim)
    
    rv
}

##---------------##
##-- BAR PLOTS --##
##---------------##

make.covid.boxplot <- function(results=outcomes.arr,
                                     years=2019:2025,
                                     locations=dimnames(results)[['location']],
                                     scenarios=dimnames(results)[['scenario']],#c('base','delayed.hiv.care','rebound.sexual.transmission','rebound.sex.delayed.hiv.care'),
                                     outcomes = c('incidence','prevalence','new')[1],
                                     include.baseline=T,
                                     aggregate.locations = T,
                                     subtract.scenario=NA,
                                     interval.coverage=0.95,
                                     summary.stat=median,
                                     #style arguments
                                     colors=pal_jama(),
                                     ribbon.alpha=0.2,
                                     line.size=2,
                                     fixed.scale=F)
{
    if (!is.na(subtract.scenario) && subtract.scenario=='self')
    {
        df = NULL
        for (sc in scenarios)
        {
            one.df = prepare.timeline.df(results=results,
                                         years=years,
                                         locations=locations,
                                         scenarios=sc,
                                         outcomes = outcomes,
                                         include.baseline=include.baseline,
                                         aggregate.locations = aggregate.locations,
                                         cumulative=T,
                                         subtract.scenario=sc,
                                         interval.coverage=interval.coverage,
                                         summary.stat=summary.stat)
            
            df = rbind(df, one.df)
        }
    }
    else
        df = prepare.timeline.df(results=results,
                                 years=years,
                                 locations=locations,
                                 scenarios=scenarios,
                                 outcomes = outcomes,
                                 include.baseline=include.baseline,
                                 aggregate.locations = aggregate.locations,
                                 cumulative=T,
                                 subtract.scenario=subtract.scenario,
                                 interval.coverage=interval.coverage,
                                 summary.stat=summary.stat)
    df = df[df$year==max(years),]
    
    ##-- MAKE THE PLOT --##
    if (is(colors, 'function'))
        colors = colors(length(unique(df$scenario)))
    
    if (length(colors) < length(unique(df$scenario)))
        stop("Not enough colors were supplied to cover all scenarios")
    colors = colors[1:length(unique(df$scenario))]
    names(colors) = unique(df$scenario) #COVID.SCENARIO.NAMES[scenarios]
    
    
    rv = ggplot(df) + 
        geom_boxplot(aes(x=scenario, fill=scenario,
                         middle=estimate,
                         lower=ci2.lower, upper=ci2.upper,
                         ymin=ci.lower, ymax=ci.upper),
                     stat='identity') +
        scale_fill_manual(values=colors, name='Scenario') + 
        scale_y_continuous(labels=function(y){format(y, big.mark=',')}) +
        theme(axis.text.x=element_text(angle = 45, hjust = 1))
    
    if (length(outcomes)==1)
    {
        rv = rv + ylab(COVID.OUTCOME.NAMES[outcomes])
        
        if (!aggregate.locations)
        {
            if (fixed.scale)
                rv = rv + facet_wrap(~location, scales='fixed')
            else
                rv = rv + facet_wrap(~location, scales='free_y')
        }
    }
    else
    {
        rv = rv + ylab("Outcome")
        
        if (aggregate.locations)
            rv = rv + facet_wrap(~outcome, scales='free_y')
        else
            rv = rv + facet_wrap(~location + outcome, scales='free_y')
    }
    
    if (!is.null(subtract.scenario) && !is.na(subtract.scenario))
        rv = rv + geom_hline(yintercept = 0, linetype='dashed')
    
    rv = rv + BLANK.THEME
    
    rv
}

make.location.boxplot <- function(results=outcomes.arr,
                                  params=parameters,
                                  years=2019:2025,
                                  locations=dimnames(results)[['location']],
                                  scenarios='base',
                                  outcome = c('incidence','prevalence','new')[1],
                                  include.total = T,
                                  subtract.scenario='baseline',
                                  subtract.relative=T,
                                  interval.coverage=0.95,
                                  interval2.coverage=0.5,
                                  summary.stat=median,
                                  loc.names=location.names,
                                  colors=pal_jama(),
                                  box.width=0.5,
                                  n.spacers=1,
                                  vertical=T,
                                  outcome.axis.name=NULL
                                  #style arguments
                               )
{
    df = df.total = NULL
    alpha = (1-interval.coverage)/2
    alpha2 = (1-interval2.coverage)/2
    
    for (scenario in scenarios)
    {
        values = get.variable(results=results,
                              locations=locations,
                              params=params,
                              var.name = outcome,
                              years=years,
                              scenario=scenario,
                              aggregate.locations = F,
                              aggregate.years = T)
        values.total = get.variable(results=results,
                                    locations=locations,
                                    params=params,
                                    var.name = outcome,
                                    years=years,
                                    scenario=scenario,
                                    aggregate.locations = T,
                                    aggregate.years = T)
        
        if (!is.null(subtract.scenario) && !is.na(subtract.scenario))
        {
            relative.to = get.variable(results=results,
                                       locations=locations,
                                       params=params,
                                       var.name = outcome,
                                       years=years,
                                       scenario=subtract.scenario,
                                       aggregate.locations = F,
                                       aggregate.years = T)
            
            values = values - relative.to
            if (subtract.relative)
                values = values/relative.to
            
            relative.to.total = get.variable(results=results,
                                       locations=locations,
                                       params=params,
                                       var.name = outcome,
                                       years=years,
                                       scenario=subtract.scenario,
                                       aggregate.locations = T,
                                       aggregate.years = T)
            
            values.total = values.total - relative.to.total
            if (subtract.relative)
                values.total = values.total/relative.to.total
        }
        
        df = rbind(df,
                   data.frame(location=loc.names[locations],
                              scenario=scenario,
                        estimate=apply(values, 'location', summary.stat),
                        ci.lower=apply(values, 'location', quantile, probs=alpha),
                        ci.upper=apply(values, 'location', quantile, probs=1-alpha),
                        ci2.lower=apply(values, 'location', quantile, probs=alpha2),
                        ci2.upper=apply(values, 'location', quantile, probs=1-alpha2)))
        
        df.total = rbind(df.total,
                         data.frame(location='Total',
                                    scenario=scenario,
                              estimate=apply(values.total, 'location', summary.stat),
                              ci.lower=apply(values.total, 'location', quantile, probs=alpha),
                              ci.upper=apply(values.total, 'location', quantile, probs=1-alpha),
                              ci2.lower=apply(values.total, 'location', quantile, probs=alpha2),
                              ci2.upper=apply(values.total, 'location', quantile, probs=1-alpha2))
        )
    }
    
    mean.estimate = sapply(loc.names, function(loc){
        mean(df$estimate[df$location==loc])
    })
    
    if (include.total)
        location.levels = rev(c(loc.names[locations][order(mean.estimate, decreasing = T)],
                                'Total'))
    else
        location.levels = rev(loc.names[locations][order(mean.estimate, decreasing = T)])
    
    if (include.total)
        df = rbind(df, df.total)
    df$location = factor(df$location, levels=location.levels)
    

    
    #-- Plot --#
    
    unique.scenarios = scenarios
    if (is(colors, 'function'))
        colors = colors(length(unique.scenarios))
    
    if (length(colors) < length(unique.scenarios))
        stop("Not enough colors were supplied to cover all scenarios")
    colors = colors[1:length(unique.scenarios)]
    names(colors) = unique.scenarios #COVID.SCENARIO.NAMES[scenarios]
    colors = rev(colors)
    
    #-- Add Spacers --#
    for (i in 1:n.spacers)
    {
        spacer = df[df$scenario==df$scenario[1],]
#        spacer = spacer[dim(spacer)[1],]
        spacer$scenario = paste0('spacer',i)
        spacer[,setdiff(names(spacer), c('location','scenario'))] = NaN
        df = rbind(df, spacer)
        spacer.color = 'white'
        names(spacer.color) = paste0('spacer',i)
        colors = c(colors, spacer.color)
    }
    
    #-- Other Formatting --#
    force.plus = function(x){
        mask = !is.na(x) & x>0
        x = format(x, big.mark=',')
        x[mask] = paste0("+", x[mask])
        x
    } 
    force.plus.percent = function(x){
        mask = !is.na(x) & x>0
        x = percent(x)
        x[mask] = paste0("+", x[mask])
        x
    }
    if (!is.na(subtract.scenario) && subtract.relative)
        x.label = force.plus.percent
    else
        x.label = force.plus
    
    df$scenario = factor(df$scenario, levels = (unique(df$scenario)))
    
    
    rv = ggplot(df) + 
        geom_boxplot(aes(x=location,
                         middle=estimate,
                         lower=ci2.lower, upper=ci2.upper,
                         ymin=ci.lower, ymax=ci.upper,
                         fill=scenario),
                     stat='identity',
                     position = position_dodge2(box.width, 'single')
        ) +
        scale_fill_manual(values=colors, name='Scenario:', labels=COVID.SCENARIO.NAMES[names(colors)],
                          limits=names(colors)[1:(length(colors)-n.spacers)]) + 
        scale_y_continuous(labels=x.label, name = outcome.axis.name)
     
    if (vertical)
        rv = rv + coord_flip() 
   
    if (!is.null(subtract.scenario) && !is.na(subtract.scenario))
        rv = rv + geom_hline(yintercept = 0, linetype='dashed')
    
    rv = rv + BLANK.THEME# + theme(axis.title.y = element_blank())
    
    rv
}

##---------------##
##-- HEAT MAPS --##
##---------------##


make.covid.heat.map <- function(results=outcomes.arr,
                                params=parameters,
                                locations=dimnames(results)[['location']],
                                loc.names = location.names,
                                aggregate.locations=T,
                                outcome='incidence',
                                outcome.years=2019:2025,
                                var1='sexual.transmission.reduction',
                                var2='suppression.reduction',
                                correlate.var1=T,
                                correlate.var2=T,
                                var1.year=2019,
                                var2.year=2019,
                                scenario='base',
                                subtract.scenario='baseline',
                                subtract.relative=T,
                                outcome.tile.stat=mean,
                                #style arguments
                                averted.color = 'green4',
                                excess.color = 'red2',
                                no.change.color = 'white',
                                label.locations=character(),
                                show.tile.n=F,
                                bin.width1=0.05,
                                bin.width2=bin.width1,
                                min.change=-0.25,
                                max.change=0.25,
                                color.scale.title="Change in Cumulative Incidence",
                                x.angle=40)
{
    x.as.pct = correlate.var1 || 
        any(PCT.VARIABLES==var1) ||
        any(dimnames(params)[[2]]==var1)
    y.as.pct = correlate.var2 || 
        any(PCT.VARIABLES==var2) ||
        any(dimnames(params)[[2]]==var2)
    
    tile.df = do.make.tile.df(results=results,
                              params=params,
                              locations=locations,
                              loc.names = loc.names,
                              aggregate.locations=aggregate.locations,
                              outcome=outcome,
                              outcome.years=outcome.years,
                              var1=var1,
                              var2=var2,
                              correlate.var1=correlate.var1,
                              correlate.var2=correlate.var2,
                              var1.year=var1.year,
                              var2.year=var2.year,
                              scenario=scenario,
                              subtract.scenario=subtract.scenario,
                              subtract.relative=subtract.relative,
                              outcome.tile.stat=outcome.tile.stat,
                              bin.width1=bin.width1,
                              bin.width2=bin.width2)
    
    print(range(tile.df$outcome)) 
    tile.df$outcome = pmax(min.change, pmin(max.change, tile.df$outcome))   
    
    rv = ggplot(tile.df) +
        geom_tile(aes(x.bin, y.bin, fill=outcome))
    
    if (show.tile.n)
        rv = rv + geom_text(aes(x.bin, y.bin, label=n))
        
    x.bin.names = attr(tile.df, 'x.bin.names')
    y.bin.names = attr(tile.df, 'y.bin.names')
    
    rv = rv +
        scale_x_continuous(breaks=1:length(x.bin.names), labels=x.bin.names) + 
        scale_y_continuous(breaks=1:length(y.bin.names), labels=y.bin.names) + 
        theme(axis.text.x=element_text(angle = x.angle, hjust = 1))

    force.plus = function(x){
        mask = !is.na(x) & x>0
        x = format(x, big.mark=',')
        x[mask] = paste0("+", x[mask])
        x
    } 
    force.plus.percent = function(x){
        mask = !is.na(x) & x>0
        x = percent(x)
        x[mask] = paste0("+", x[mask])
        x
    }
    if (!is.na(subtract.scenario) && subtract.relative)
        fill.label = force.plus.percent
    else
        fill.label = force.plus
    rv = rv + scale_fill_gradient2(name=color.scale.title,
                                 labels = fill.label,
                                 limits=c(min.change,max.change),#c(-1,MAX.FOLD.INCREASE-1), 
                                 midpoint=0,
                                 breaks = c(min.change,0,max.change),
                                 low=averted.color, mid=no.change.color, high=excess.color)

    if (length(unique(tile.df$location))>1)
        rv = rv + facet_wrap(~location)
    
    rv = rv + BLANK.THEME + theme(legend.position = 'bottom')
    
    rv
}

##-------------------##
##-- TIMELINE PLOT --##
##-------------------##

make.covid.timeline.plot <- function(results=outcomes.arr,
                                     years=2019:2025,
                                     locations=dimnames(results)[['location']],
                                     scenarios=dimnames(results)[['scenario']],#c('base','delayed.hiv.care','rebound.sexual.transmission','rebound.sex.delayed.hiv.care'),
                                     outcomes = c('incidence','prevalence','new')[1],
                                     groups=NULL,
                                     include.baseline=T,
                                     aggregate.locations = T,
                                     cumulative=F,
                                     subtract.scenario=NA,
                                     subtract.relative=F,
                                     interval.coverage=0.95,
                                     summary.stat=mean,
                                     baseline.outcomes=outcomes,
                                     #style arguments
                                     ylim=c(NaN,NaN),
                                     colors=pal_jama(),
                                     ribbon.alpha=0.2,
                                     line.size=2,
                                     fixed.scale=F,
                                     line.types='solid',
                                     baseline.linetype='solid',
                                     show.baseline.ribbon=T,
                                     linetype.by.group = !is.null(groups),
                                     linetype.by.outcome = F,
                                     color.by.outcome = F,
                                     show.ribbon.outline=F)
{
    if (is.null(groups))
        groups = rep('All', dim(results)['sim'])
    if (!is(groups, 'list'))
        groups = list(groups)
    
    unique.groups = sort(unique(unlist(sapply(groups, unique))))
    if (length(groups)>1)
        names(groups) = setdiff(scenarios, 'baseline')
    
    df = NULL    
    if (include.baseline)
    { 
        sim.mask = rep(T, dim(results)['sim'])
        #        sim.mask = !is.na(groups)
        sub.results = results[,,,,sim.mask]
        sub.dim.names = dimnames(results)
        sub.dim.names$sim = sub.dim.names$sim[sim.mask]
        dim(sub.results) = sapply(sub.dim.names, length)
        dimnames(sub.results) = sub.dim.names
        one.df = prepare.timeline.df(results=sub.results,
                                     years=years,
                                     locations=locations,
                                     scenarios='baseline',
                                     outcomes = baseline.outcomes,
                                     include.baseline=F,
                                     aggregate.locations = aggregate.locations,
                                     cumulative=cumulative,
                                     subtract.scenario=subtract.scenario,
                                     interval.coverage=interval.coverage,
                                     summary.stat=summary.stat,
                                     subtract.relative=subtract.relative)
        one.df$group = 'baseline'
        if (!show.baseline.ribbon)
            one.df$ci.lower = one.df$ci.upper = NaN
        
        df = rbind(df, one.df)
    }
    
    for (scenario in setdiff(scenarios, 'baseline'))
    {
        if (length(groups)==1)
            gg = groups[[1]]
        else
            gg = groups[[scenario]]
        
        unique.groups.for.scenario = unique(gg[!is.na(gg)])
        for (group in unique.groups.for.scenario)
        {
            sim.mask = !is.na(gg) & gg == group
            sub.results = results[,,,,sim.mask]
            sub.dim.names = dimnames(results)
            sub.dim.names$sim = sub.dim.names$sim[sim.mask]
            dim(sub.results) = sapply(sub.dim.names, length)
            dimnames(sub.results) = sub.dim.names
            
            one.df = prepare.timeline.df(results=sub.results,
                                     years=years,
                                     locations=locations,
                                     scenarios=scenario,
                                     outcomes = outcomes,
                                     include.baseline=F,
                                     aggregate.locations = aggregate.locations,
                                     cumulative=cumulative,
                                     subtract.scenario=subtract.scenario,
                                     subtract.relative = subtract.relative,
                                     interval.coverage=interval.coverage,
                                     summary.stat=summary.stat)
            one.df$group = group
            
            df = rbind(df, one.df)
        }
    }
    
    ##-- MAKE THE PLOT --##
    unique.scenarios = unique(df$scenario)
    if (color.by.outcome)
    {
        if (is(colors, 'function'))
            colors = colors(length(outcomes))
        
        if (length(colors) < length(outcomes))
            stop("Not enough colors were supplied to cover all outcomes")
        colors = colors[1:length(outcomes)]
        names(colors) = outcomes #COVID.SCENARIO.NAMES[scenarios]
    }
    else
    {
        if (is(colors, 'function'))
            colors = colors(length(unique.scenarios))
        
        if (length(colors) < length(unique.scenarios))
            stop("Not enough colors were supplied to cover all scenarios")
        colors = colors[1:length(unique.scenarios)]
        names(colors) = unique.scenarios #COVID.SCENARIO.NAMES[scenarios]
    }      
    
    if (linetype.by.outcome && length(outcomes)>1)
    {
        if (length(line.types) < length(outcomes))
            stop("Not enough line types were supplied to cover all outcomes")
        line.types = line.types[1:length(outcomes)]
        line.type.labels = COVID.OUTCOME.NAMES[outcomes]
        linetype.name='Outcome'
    }
    else if (linetype.by.group)
    {
        if (length(line.types) < length(unique.groups))
            stop("Not enough line types were supplied to cover all groups")
        line.types = line.types[1:length(unique.groups)]
        names(line.types) = unique.groups
        line.types = c(line.types, baseline=baseline.linetype)
        line.type.labels = unique.groups
        linetype.name = 'Group'
    }
    else
    {
        if (length(line.types)==1)
            line.types = rep(line.types, length(unique.scenarios))
        
        if (length(line.types) < length(unique.scenarios))
            stop("Not enough line types were supplied to cover all scenarios")
        line.types = line.types[1:length(unique.scenarios)]
        names(line.types) = unique.scenarios
        line.type.labels = unique.scenarios
        linetype.name = 'Scenario'
    }
    
    if (linetype.by.outcome && length(outcomes)>1)
        rv = ggplot(df, aes(linetype=outcome))
    else if (linetype.by.group)
        rv = ggplot(df, aes(linetype=group))
    else
        rv = ggplot(df, aes(linetype=scenario))
    
    
    if (color.by.outcome)
    {
        if (show.ribbon.outline)
            rv = rv +
                geom_ribbon(aes(year, ymin=ci.lower, ymax=ci.upper, fill=outcome, color=outcome), alpha=ribbon.alpha, size=0) 
        else
            rv = rv +
                geom_ribbon(aes(year, ymin=ci.lower, ymax=ci.upper, fill=outcome),
                            color=NA, alpha=ribbon.alpha, size=0) 
        
        rv = rv +
            geom_line(aes(year, estimate, color=outcome), size=line.size) +
            scale_color_manual(values=colors, name='Outcome') +
            scale_fill_manual(values=colors, name='Outcome')
    }
    else
    {
        if (show.ribbon.outline)
            rv = rv +
            geom_ribbon(aes(year, ymin=ci.lower, ymax=ci.upper, fill=scenario, color=scenario), alpha=ribbon.alpha, size=0) 
        else
            rv = rv +
                geom_ribbon(aes(year, ymin=ci.lower, ymax=ci.upper, fill=scenario),
                            color=NA, alpha=ribbon.alpha, size=0) 
        
        rv = rv +
            geom_line(aes(year, estimate, color=scenario), size=line.size) +
            scale_color_manual(values=colors, name='Scenario') +
            scale_fill_manual(values=colors, name='Scenario')
    }
    
    rv = rv +
        xlab("Year")  + 
        scale_linetype_manual(values=line.types, name=linetype.name, labels=line.type.labels)
    
    force.plus.percent = function(x){
        mask = !is.na(x) & x>0
        x = percent(x)
        x[mask] = paste0("+", x[mask])
        x
    }
    
    if (!is.null(subtract.scenario) && !is.na(subtract.scenario) && subtract.relative)
        rv = rv + scale_y_continuous(labels=force.plus.percent, limits = ylim)
    else
        rv = rv + scale_y_continuous(labels=function(y){format(y, big.mark=',')}, limits = ylim)
    
    if (length(outcomes)==1 || linetype.by.outcome)
    {
        rv = rv + ylab(COVID.OUTCOME.NAMES[outcomes])
        
        if (!aggregate.locations)
        {
            if (fixed.scale)
                rv = rv + facet_wrap(~location, scales='fixed')
            else
                rv = rv + facet_wrap(~location, scales='free_y')
        }
    }
    else
    {
        rv = rv + ylab("Outcome")
        
        if (aggregate.locations)
            rv = rv + facet_wrap(~outcome, scales='free_y')
        else
            rv = rv + facet_wrap(~location + outcome, scales='free_y')
    }
    
    if (!is.null(subtract.scenario) && !is.na(subtract.scenario))
        rv = rv + geom_hline(yintercept = 0, linetype='dashed')
    
    rv = rv + BLANK.THEME
    
    rv
}

make.quantile.timeline.plot <- function(results=outcomes.arr, 
                                        params = parameters,
                                        years=2019:2025,
                                        n.quantiles=5,
                                        show.quantiles=1:5,
                                        quantile.scenario='base',
                                        quantile.var = 'incidence',
                                        quantile.subtract.scenario='baseline',
                                        quantile.subtract.relative=F,
                                        quantile.years=years,
                                        locations=dimnames(results)[['location']],
                                        scenarios='base',
                                        outcomes = c('incidence','prevalence','new')[1],
                                        baseline.outcomes=outcomes,
                                        include.baseline=T,
                                        cumulative=F,
                                        subtract.scenario=NA,
                                        interval.coverage=0.95,
                                        summary.stat=mean,
                                        #style arguments
                                        colors=pal_jama(),
                                        ribbon.alpha=0.2,
                                        line.size=2,
                                        fixed.scale=F,
                                        quantile.line.type='solid',
                                        baseline.linetype='solid',
                                        show.baseline.ribbon=T,
                                        line.types='solid',
                                        show.ribbon.outline=F,
                                        linetype.by.outcome=F,
                                        color.by.outcome=F,
                                        ylim=c(NaN,NaN))
{
    abs.outcome = get.variable(results=results,
                               params=params,
                               locations=locations,
                               var.name = quantile.var,
                               years=quantile.years,
                               scenario=quantile.scenario,
                               aggregate.locations = T)
    
    if (!is.null(subtract.scenario) && !is.na(subtract.scenario))
    {
        relative.to = get.variable(results=results,
                                   params=params,
                                   locations=locations,
                                   var.name = quantile.var,
                                   years=quantile.years,
                                   scenario=quantile.subtract.scenario,
                                   aggregate.locations = T)
        
        aggregated.results = abs.outcome - relative.to
        if (quantile.subtract.relative)
            aggregated.results = aggregated.results/relative.to
    }
    else
        aggregated.results = abs.outcome
    
    # Split quantiles
    cutoffs = quantile(aggregated.results, probs=seq(0,1,length=n.quantiles+1))[1:n.quantiles]
    groups = sapply(aggregated.results, function(x){
        max((1:n.quantiles)[x>=cutoffs])
    })
    groups[sapply(groups, function(g){all(g != show.quantiles)})] = NA
    
    # Call sub-function
    make.covid.timeline.plot(results=results,
                              years=years,
                              locations=locations,
                              scenarios=scenarios,
                              outcomes = outcomes,
                             baseline.outcomes=baseline.outcomes,
                              groups=groups,
                              include.baseline=include.baseline,
                              aggregate.locations = T,
                              cumulative=cumulative,
                              subtract.scenario=subtract.scenario,
                              interval.coverage=interval.coverage,
                              summary.stat=mean,
                              #style arguments
                              colors=colors,
                              ribbon.alpha=ribbon.alpha,
                              line.size=line.size,
                              fixed.scale=fixed.scale,
                              line.types=line.types,
                             linetype.by.outcome = linetype.by.outcome,
                              baseline.linetype='solid',
                              show.baseline.ribbon=show.baseline.ribbon,
                              show.ribbon.outline=show.ribbon.outline,
                             color.by.outcome = color.by.outcome,
                             ylim=ylim)
    
}

make.stacked.timeline.plot <- function(results=outcomes.arr,
                                       years=2019:2025,
                                       locations=dimnames(results)[['location']],
                                       scenarios=setdiff(dimnames(results)[['scenario']], 'baseline'),
                                       outcomes = c('incidence','prevalence','new')[1],
                                       include.baseline=F,
                                       aggregate.locations = T,
                                       cumulative=F,
                                       subtract.scenario='baseline',
                                       interval.coverage=0.95,
                                       summary.stat=mean,
                                       #style arguments
                                       colors=pal_jama(),
                                       fill.alpha=0.2,
                                       line.size=2,
                                       fixed.scale=F)
{
    df = prepare.timeline.df(results=results,
                             years=years,
                             locations=locations,
                             scenarios=scenarios,
                             outcomes = outcomes,
                             include.baseline=include.baseline,
                             aggregate.locations = aggregate.locations,
                             cumulative=cumulative,
                             subtract.scenario=subtract.scenario,
                             interval.coverage=interval.coverage,
                             summary.stat=summary.stat)
    
    df$lower = pmin(df$estimate, 0)
    df$upper = pmax(df$estimate, 0)
    
    ##-- MAKE THE PLOT --##
    
    unique.scenarios = unique(df$scenario)
    df$scenario = factor(df$scenario, levels=unique.scenarios)
    
    if (is(colors, 'function'))
        colors = colors(length(unique.scenarios))
    
    if (length(colors) < length(unique.scenarios))
        stop("Not enough colors were supplied to cover all scenarios")
    colors = colors[1:length(unique.scenarios)]
    names(colors) = rev(unique.scenarios) #COVID.SCENARIO.NAMES[scenarios]
    
    rv = ggplot(df) + 
        geom_ribbon(aes(x=year, ymin=lower, ymax=upper, fill=scenario, color=scenario),
                    size=line.size, alpha=fill.alpha) +
       # geom_bar(aes(x=year, y=estimate, fill=scenario), stat='identity', position='dodge') + 
        xlab("Year") +
        scale_color_manual(values=colors, name='Scenario') +
        scale_fill_manual(values=colors, name='Scenario') + 
        scale_y_continuous(labels=function(y){format(y, big.mark=',')})
    
    if (length(outcomes)==1)
    {
        rv = rv + ylab(COVID.OUTCOME.NAMES[outcomes])
        
        if (!aggregate.locations)
        {
            if (fixed.scale)
                rv = rv + facet_wrap(~location, scales='fixed')
            else
                rv = rv + facet_wrap(~location, scales='free_y')
        }
    }
    else
    {
        rv = rv + ylab("Outcome")
        
        if (aggregate.locations)
            rv = rv + facet_wrap(~outcome, scales='free_y')
        else
            rv = rv + facet_wrap(~location + outcome, scales='free_y')
    }
    
    if (!is.null(subtract.scenario) && !is.na(subtract.scenario))
        rv = rv + geom_hline(yintercept = 0, linetype='dashed')
    
    rv = rv + BLANK.THEME
    
    rv
}


##-------------##
##-- HELPERS --##
##-------------##

get.variable <- function(results=outcomes.arr,
                         params=parameters,
                         var.name=c('incidence','prevalence','prevalence.diagnosed','new','suppression',
                                    'incidence.prevalence.ratio',
                                    "sexual.transmission.reduction","testing.reduction","prep.reduction", 
                                    "suppression.reduction", "sexual.transmission.increase")[1],
                         years=2019:2025,
                         scenario='base',
                         locations=unique(dimnames(results)[['location']]),
                         aggregate.locations=T,
                         aggregate.years=T)
{
    if (any(var.name==dimnames(params)[[2]]))
    {
        params = params[,var.name]
        
        if (aggregate.locations)
            dim.names = list(location='total',
                             sim=dimnames(results)[['sim']])
        else
        {
            dim.names = list(location=locations,
                             sim=dimnames(results)[['sim']])
            
            params = t(sapply(locations, function(loc){
                params
            }))
        }
        
        dim(params) = sapply(dim.names, length)
        dimnames(params) = dim.names
        
        params
    }
    else if (var.name=='incidence.prevalence.ratio')
    {
        get.variable(results=results,
                     params=params,
                     var.name='incidence',
                     years=years,
                     scenario=scenario,
                     locations=locations,
                     aggregate.locations=aggregate.locations,
                     aggregate.years=aggregate.years) /
            get.variable(results=results,
                         params=params,
                         var.name='prevalence.all',
                         years=years,
                         scenario=scenario,
                         locations=locations,
                         aggregate.locations=aggregate.locations,
                         aggregate.years=aggregate.years)
        
        
        
        
    }
    else if (var.name=='diagnosed')
    {
        get.variable(results=results,
                     params=params,
                     var.name='prevalence.diagnosed',
                     years=years,
                     scenario=scenario,
                     locations=locations,
                     aggregate.locations=aggregate.locations,
                     aggregate.years=aggregate.years) /
            get.variable(results=results,
                         params=params,
                         var.name='prevalence.all',
                         years=years,
                         scenario=scenario,
                         locations=locations,
                         aggregate.locations=aggregate.locations,
                         aggregate.years=aggregate.years)
    }
    else if (var.name=='fraction.acute')
    {
        get.variable(results=results,
                     params=params,
                     var.name='prevalence.acute.all',
                     years=years,
                     scenario=scenario,
                     locations=locations,
                     aggregate.locations=aggregate.locations,
                     aggregate.years=aggregate.years) /
            get.variable(results=results,
                         params=params,
                         var.name='prevalence.all',
                         years=years,
                         scenario=scenario,
                         locations=locations,
                         aggregate.locations=aggregate.locations,
                         aggregate.years=aggregate.years)
    }
    else
    {
        dim.names = list(location=locations,
                         year=as.character(years),
                         sim=dimnames(results)[['sim']])
        
        outcome = var.name
        if (var.name=='prevalence')
            outcome = 'prevalence.all'
        
        orig.results = results
        
        results = results[locations, scenario, as.character(years), outcome, ]
        
        dim(results) = sapply(dim.names, length)
        dimnames(results) = dim.names
        
        keep.dimensions = c('location','year','sim')[c(T,!aggregate.years,T)]
        results = apply(results, keep.dimensions, sum)
        
        if (aggregate.locations)
        {
            dim.names = list(location='Total',
                             year=as.character(years),
                             sim=dimnames(results)[['sim']])[c(T,!aggregate.years,T)]
            results = apply(results, setdiff(names(dim.names), 'location'), sum)
            dim(results) = sapply(dim.names, length)
            dimnames(results) = dim.names
        }
        
        if (aggregate.years)
        {
            dim.names = dimnames(results)[setdiff(names(dimnames(results)), 'year')]
            results = apply(results, names(dim.names), sum)
            dim(results) = sapply(dim.names, length)
            dimnames(results) = dim.names 
        }
        
        if (var.name=='suppression')
        {
            denominator = get.variable(results=orig.results,
                                       params=params,
                                       var.name='prevalence.diagnosed',
                                       years=years,
                                       scenario=scenario,
                                       locations=locations,
                                       aggregate.locations=aggregate.locations,
                                       aggregate.years=aggregate.years)
                
            results = results / denominator
        }
        
        results
    }
}

prepare.timeline.df <- function(results=outcomes.arr,
                                years=2019:2025,
                                locations=dimnames(results)[['location']],
                                scenarios=dimnames(results)[['scenario']],#c('base','delayed.hiv.care','rebound.sexual.transmission','rebound.sex.delayed.hiv.care'),
                                outcomes = c('incidence','prevalence','new')[1],
                                include.baseline=T,
                                aggregate.locations = T,
                                cumulative=F,
                                subtract.scenario='baseline',
                                subtract.relative=F,
                                interval.coverage=0.95,
                                interval.coverage2=0.5,
                                summary.stat=mean)
{
    orig.arr = results
    new.dim.names = dimnames(results)
    new.dim.names$outcome = c(new.dim.names$outcome, 'one')
    default.denominator = 1
    if (aggregate.locations)
        default.denominator = 1/length(locations)
    results = array(default.denominator, dim=sapply(new.dim.names, length), dimnames=new.dim.names)
    results[,,,dimnames(orig.arr)$outcome,] = orig.arr
    
    outcome.numerators = outcomes
    outcome.denominators = rep('one', length(outcomes))
    outcome.numerators[outcomes=='diagnosed'] = 'prevalence.diagnosed'
    outcome.denominators[outcomes=='diagnosed'] = 'prevalence.all'
    
    do.subtract = !is.null(subtract.scenario) && !is.na(subtract.scenario)

    if (include.baseline)
        scenarios = union('baseline',scenarios)
  #  if (do.subtract)
    #    scenarios = setdiff(scenarios, 'baseline')
    
    dim.names = dimnames(results)
    dim.names[['scenario']] = scenarios
    dim.names[['location']] = locations
    dim.names[['outcome']] = outcomes
    dim.names[['year']] = as.character(years)
    
    # Subset
    if (do.subtract)
    {   
        to.subtract.numerators = results[locations, subtract.scenario, as.character(years), outcome.numerators, ]
        to.subtract.denominators = results[locations, subtract.scenario, as.character(years), outcome.denominators, ]
        
    }
    result.numerators = results[locations, scenarios, as.character(years), outcome.numerators, ]
    result.denominators = results[locations, scenarios, as.character(years), outcome.denominators, ]
    
    dim(result.numerators) = dim(result.denominators) = sapply(dim.names, length)
    dimnames(result.numerators) = dimnames(result.denominators) = dim.names

    if (do.subtract)
    {
        result.numerators = apply(result.numerators, c('location','year','outcome','sim','scenario'), function(x){x}) - 
            as.numeric(to.subtract.numerators)
        result.denominators = apply(result.denominators, c('location','year','outcome','sim','scenario'), function(x){x}) - 
            as.numeric(to.subtract.denominators)
        subtract.keep.dimensions = names(dimnames(to.subtract.numerators))
    }

    # Aggregate
    keep.dimensions = names(dim.names)
    if (aggregate.locations)
    {
        keep.dimensions = setdiff(keep.dimensions, 'location')
        result.numerators = apply(result.numerators, keep.dimensions, sum)
        result.denominators = apply(result.denominators, keep.dimensions, sum)
        
        if (do.subtract && subtract.relative)
        {
            subtract.keep.dimensions = setdiff(subtract.keep.dimensions, 'location')
            to.subtract.numerators = apply(to.subtract.numerators, subtract.keep.dimensions, sum)
            to.subtract.denominators = apply(to.subtract.denominators, subtract.keep.dimensions, sum)
        }
    }
    if (cumulative)
    {
        result.numerators = apply(result.numerators, setdiff(keep.dimensions, 'year'), cumsum)
        result.denominators = apply(result.denominators, setdiff(keep.dimensions, 'year'), cumsum)
        dim.names = dim.names[c('year', setdiff(keep.dimensions, 'year'))]
        dim(result.numerators) = dim(result.denominators) = sapply(dim.names, length)
        dimnames(result.numerators) = dimnames(result.denominators) = dim.names
        
        if (do.subtract && subtract.relative)
        {
            to.subtract.numerators = apply(to.subtract.numerators, setdiff(subtract.keep.dimensions, 'year'), cumsum)
            to.subtract.denominators = apply(to.subtract.denominators, setdiff(subtract.keep.dimensions, 'year'), cumsum)
            
            subtract.dim.names = dimnames(to.subtract.numerators)[c('year', setdiff(subtract.keep.dimensions, 'year'))]
            dim(to.subtract.numerators) = dim(to.subtract.denominators) = sapply(to.subtract.numerators, length)
            dimnames(to.subtract.numerators) = dimnames(to.subtract.denominators) = subtract.dim.names 
        }
    }
    
    results = result.numerators / result.denominators
    
    if (do.subtract && subtract.relative)
            results = results / as.numeric(to.subtract)

    
    keep.dimensions = setdiff(keep.dimensions, 'sim')
    stat.arr = apply(results, keep.dimensions, summary.stat)
    
    alpha = (1-interval.coverage)/2
    
    ci.lower = apply(results, keep.dimensions, quantile, probs=alpha, na.rm=T)
    ci.upper = apply(results, keep.dimensions, quantile, probs=1-alpha, na.rm=T)
    
    alpha2 = (1-interval.coverage2)/2
    
    ci2.lower = apply(results, keep.dimensions, quantile, probs=alpha2, na.rm=T)
    ci2.upper = apply(results, keep.dimensions, quantile, probs=1-alpha2, na.rm=T)
    
    # Set up the data frame
    df = reshape2::melt(stat.arr, value.name = 'estimate')
    df$ci.lower = as.numeric(ci.lower)
    df$ci.upper = as.numeric(ci.upper)
    df$ci2.lower = as.numeric(ci2.lower)
    df$ci2.upper = as.numeric(ci2.upper)
    
    df$scenario = as.character(df$scenario)
    df$scenario = COVID.SCENARIO.NAMES[df$scenario]

    if (aggregate.locations)
        df$location = "Total"
    
    df
}

do.make.tile.df <- function(results=outcomes.arr,
                            params=parameters,
                            locations=dimnames(results)[['location']],
                            loc.names = location.names,
                            aggregate.locations=T,
                            outcome='incidence',
                            outcome.years=2019:2025,
                            var1='sexual.transmission.reduction',
                            var2='suppression.reduction',
                            correlate.var1=T,
                            correlate.var2=T,
                            var1.year=2019,
                            var2.year=2019,
                            scenario='base',
                            subtract.scenario='baseline',
                            subtract.relative=T,
                            outcome.tile.stat=mean,
                            bin.width1=0.05,
                            bin.width2=bin.width1)
{
    x.as.pct = correlate.var1 || 
        any(PCT.VARIABLES==var1) ||
        any(dimnames(params)[[2]]==var1)
    y.as.pct = correlate.var2 || 
        any(PCT.VARIABLES==var2) ||
        any(dimnames(params)[[2]]==var2)
    
    values1 = get.variable(results=results,
                           params=params,
                           locations=locations,
                           var.name = var1,
                           years=var1.year,
                           scenario=scenario,
                           aggregate.locations = aggregate.locations)
    
    values2 = get.variable(results=results,
                           params=params,
                           locations=locations,
                           var.name = var2,
                           years=var2.year,
                           scenario=scenario,
                           aggregate.locations = aggregate.locations)
    
    abs.outcome = get.variable(results=results,
                               params=params,
                               locations=locations,
                               var.name = outcome,
                               years=outcome.years,
                               scenario=scenario,
                               aggregate.locations = aggregate.locations)
    
    if (!is.null(subtract.scenario) && !is.na(subtract.scenario))
    {
        relative.to = get.variable(results=results,
                                   params=params,
                                   locations=locations,
                                   var.name = outcome,
                                   years=outcome.years,
                                   scenario=subtract.scenario,
                                   aggregate.locations = aggregate.locations)
        
        outcome = abs.outcome - relative.to
        if (subtract.relative)
            outcome = outcome/relative.to
    }
    else
        outcome = abs.outcome
    
    
    # Make Tile DF
    
    tile.df = make.tile.df(x.values = values1,
                           y.values = values2,
                           outcome = outcome,
                           x.binwidth = bin.width1,
                           y.binwidth = bin.width2,
                           x.is.percent = x.as.pct,
                           y.is.percent = y.as.pct)

    tile.df
}

make.tile.df <- function(x.values,
                         y.values,
                         outcome,
                         outcome.stat=mean,
                         x.binwidth=0.05,
                         y.binwidth=0.05,
                         x.is.percent=T,
                         y.is.percent=T)
{
    x.range = c(x.binwidth * floor(min(x.values) / x.binwidth),
                x.binwidth * ceiling(max(x.values) / x.binwidth))
    y.range = c(y.binwidth * floor(min(y.values) / y.binwidth),
                y.binwidth * ceiling(max(y.values) / y.binwidth))
    
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
    
    x.bins = apply(x.values, 1:2, function(x){
        (1:x.n.bins)[x>=x.lowers & x<x.uppers][1]
    })
    
    y.bins = apply(y.values, 1:2, function(y){
        (1:y.n.bins)[y>=y.lowers & y<y.uppers][1]
    })
    
    locations = dimnames(x.values)[['location']]
    
    arr = sapply(1:x.n.bins, function(x.bin){
        sapply(1:y.n.bins, function(y.bin){
            sapply(1:length(locations), function(loc){
                mask = x.bins[loc,]==x.bin & y.bins[loc,]==y.bin
                outcome.stat(outcome[loc, mask])
            })
        })
    })
    
    dim.names = list(location=locations,
                  y.bin = 1:y.n.bins,
                  x.bin = 1:x.n.bins)

    dim(arr) = sapply(dim.names, length)
    dimnames(arr) = dim.names
    
    n.arr = sapply(1:x.n.bins, function(x.bin){
            sapply(1:y.n.bins, function(y.bin){
                sapply(locations, function(loc){
                    mask = x.bins[loc,]==x.bin & y.bins[loc,]==y.bin
                    sum(mask)
                })
            })
        })
    
    rv = reshape2::melt(arr, value.name='outcome')
    rv$x.bin = as.numeric(rv$x.bin)
    rv$y.bin = as.numeric(rv$y.bin)
    rv$n = as.numeric(n.arr)
    
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