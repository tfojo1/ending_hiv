
library(colorspace)

##-- SET-UP --##
IMAGE.DIR = '../Manuscripts/covid_manuscript/images'
library(ggsci)
PALETTE = pal_jama()
COLORS = PALETTE(6)[-5]
names(COLORS) = c('baseline',
                  'base',
                  'delayed.hiv.care',
                  'rebound.sexual.transmission',
                  'rebound.sex.delayed.hiv.care')
DARK.COLORS = darken(COLORS)


PANEL.WIDTH = 3
PANEL.HEIGHT = 2
PNG.POINT.SIZE = 5
RES = 600

LINE.SIZE = 1

THEME = theme(text = element_text(size=8), legend.direction = 'horizontal',
              legend.position = 'none')
CUM.INC.YEARS = 2020:2025

##-- SOURCE CODE --##
if (1==2)
{
    source('code/covid/covid_plots.R')
    load('code/covid/results/covid_4.0_results.Rdata')

}

##---------------------------##
##-- OVERVIEW AND TIMELINE --##
##---------------------------##

##-- TIMELINE and BOX-PLOTS --##
TIMELINE.RIBBON.ALPHA = 0.1
if (1==2)
{    
    png(file.path(IMAGE.DIR, 'timeline/all_scenarios_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=names(COLORS), outcomes='incidence', line.size = LINE.SIZE,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                                    colors = COLORS) + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'timeline/all_scenarios_new.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=names(COLORS), outcomes='new', line.size = LINE.SIZE,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                             colors = COLORS) + THEME
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'timeline/all_scenarios_cum_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.boxplot(scenarios=names(COLORS)[-1], outcomes='incidence', 
                       years = CUM.INC.YEARS,
                       subtract.scenario = 'baseline', include.baseline = F,
                       line.size = LINE.SIZE,
                       ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                       colors = COLORS[-1]) + THEME + 
        theme(axis.text.x=element_blank(), axis.title.x=element_blank(), axis.ticks.x=element_blank())
    dev.off()
    
    png(file.path(IMAGE.DIR, 'timeline/all_scenarios_cum_new.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.boxplot(scenarios=names(COLORS)[-1], outcomes='new', 
                       years = CUM.INC.YEARS,
                       subtract.scenario = 'baseline', include.baseline = F,
                       line.size = LINE.SIZE,
                       ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                       colors = COLORS[-1]) + THEME + 
        theme(axis.text.x=element_blank(), axis.title.x=element_blank(), axis.ticks.x=element_blank())
    dev.off()
    
}

##-- Narrative for cumulative change --##
if (1==2)
{
    abs.df = prepare.timeline.df(results=outcomes.arr, subtract.scenario = NA)
    baseline.mask = abs.df$scenario==abs.df$scenario[1]
    paste0("2019: ", 
           format(round(abs.df$estimate[baseline.mask & abs.df$year==2019]), big.mark=','),
           " (", format(round(abs.df$ci.lower[baseline.mask & abs.df$year==2019]), big.mark=','),
           " - ", format(round(abs.df$ci.upper[baseline.mask & abs.df$year==2019]), big.mark=','), ")")
    paste0("2025: ", 
           format(round(abs.df$estimate[baseline.mask & abs.df$year==2025]), big.mark=','),
           " (", format(round(abs.df$ci.lower[baseline.mask & abs.df$year==2025]), big.mark=','),
           " - ", format(round(abs.df$ci.upper[baseline.mask & abs.df$year==2025]), big.mark=','), ")")
    
    
    cum.df = prepare.timeline.df(results=outcomes.arr, subtract.scenario = NA, cumulative = T, years=CUM.INC.YEARS)
    baseline.mask = cum.df$scenario==cum.df$scenario[1]
    paste0("Cumulative inc ", min(CUM.INC.YEARS), "-", max(CUM.INC.YEARS), " (baseline): ", 
           format(round(cum.df$estimate[baseline.mask & cum.df$year==max(CUM.INC.YEARS)]), big.mark=','),
           " (", format(round(cum.df$ci.lower[baseline.mask & cum.df$year==max(CUM.INC.YEARS)]), big.mark=','),
           " - ", format(round(cum.df$ci.upper[baseline.mask & cum.df$year==max(CUM.INC.YEARS)]), big.mark=','), ")")
    
    
    cum.sub.df.abs = prepare.timeline.df(results=outcomes.arr, subtract.scenario = 'baseline', 
                                         cumulative = T, years=CUM.INC.YEARS)
    base.mask = cum.sub.df.abs$scenario==cum.rel.df$scenario[2]
    paste0("Cumulative inc ", min(CUM.INC.YEARS), "-", max(CUM.INC.YEARS), " (base COVID): ", 
           format(round(cum.sub.df.abs$estimate[base.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','),
           " (", format(round(cum.sub.df.abs$ci.lower[base.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','),
           " to ", format(round(cum.sub.df.abs$ci.upper[base.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','), ")")
    
    delayed.mask = cum.sub.df.abs$scenario==cum.rel.df$scenario[3]
    paste0("Cumulative inc ", min(CUM.INC.YEARS), "-", max(CUM.INC.YEARS), " (delayed): ", 
           format(round(cum.sub.df.abs$estimate[delayed.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','),
           " (", format(round(cum.sub.df.abs$ci.lower[delayed.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','),
           " to ", format(round(cum.sub.df.abs$ci.upper[delayed.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','), ")")
    
    rebound.mask = cum.sub.df.abs$scenario==cum.rel.df$scenario[4]
    paste0("Cumulative inc ", min(CUM.INC.YEARS), "-", max(CUM.INC.YEARS), " (rebound): ", 
           format(round(cum.sub.df.abs$estimate[rebound.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','),
           " (", format(round(cum.sub.df.abs$ci.lower[rebound.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','),
           " to ", format(round(cum.sub.df.abs$ci.upper[rebound.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','), ")")

    rebound.delayed.mask = cum.sub.df.abs$scenario==cum.rel.df$scenario[5]
    paste0("Cumulative inc ", min(CUM.INC.YEARS), "-", max(CUM.INC.YEARS), " (rebound+delayed): ", 
           format(round(cum.sub.df.abs$estimate[rebound.delayed.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','),
           " (", format(round(cum.sub.df.abs$ci.lower[rebound.delayed.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','),
           " to ", format(round(cum.sub.df.abs$ci.upper[rebound.delayed.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','), ")")
    
}

##-----------------------##
##-- IMPORTANT FACTORS --##
##-----------------------##

##-- HEAT MAPS --##
HEAT.MAP.RANGE = c(-.2,.2)#c(-20000, 20000)
HEAT.MAP.PANEL.HEIGHT = 2.25
HEAT.MAP.THEME = THEME + theme(legend.position = 'bottom')

do.make.covid.heat.map <- function(loc=NULL,
                                   subtract.relative=T)
{
    aggregate.loc = is.null(loc)
    if (is.null(loc))
        loc = names(location.names)
    
    make.covid.heat.map(var1 = 'sexual.transmission.reduction',
                        var2 = 'suppression.reduction',
                        min.change = HEAT.MAP.RANGE[1],
                        max.change = HEAT.MAP.RANGE[2],
                        scenario='base',
                        outcome = 'incidence', outcome.years = CUM.INC.YEARS,
                        locations = loc, aggregate.locations = aggregate.loc,
                        color.scale.title = " ", #"Change in Cumulative\nIncidence 2020-2025",
                        subtract.relative = subtract.relative) + 
        xlab("Reduction in Sexual Transmission") + ylab("Reduction in Viral Suppression") + THEME
}

if (1==2)
{
    
    png(file.path(IMAGE.DIR, 'heat_maps/raw_for_legend.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    make.covid.heat.map(var1 = 'sexual.transmission.reduction',
                        var2 = 'suppression.reduction',
                        min.change = HEAT.MAP.RANGE[1],
                        max.change = HEAT.MAP.RANGE[2],
                        scenario='base',
                        color.scale.title = " ", #"Change in Cumulative\nIncidence 2020-2025",
                        subtract.relative = F) + 
        xlab("Reduction in Sexual Transmission") + ylab("Reduction in Viral Suppression") +
        THEME + theme(legend.position = 'bottom')
    dev.off()
    
    
    
    png(file.path(IMAGE.DIR, 'heat_maps/chicago_base_sex_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('16980')
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/boston_base_sex_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('14460')
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/sd_base_sex_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('41740')
    dev.off()
}

##-- CITY-SPECIFIC CORRELATIONS --##
CITY.COR.SIZE.RANGE = c(1,3)
CITY.COR.THEME = theme(text = element_text(size=7),
                       axis.title = element_text(size=7),
                       legend.position = 'none')
CITY.COR.PANEL.HEIGHT = 2.5
if (1==2)
{
    png(file.path(IMAGE.DIR, 'msa_scatter/suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=CITY.COR.PANEL.HEIGHT, res=RES, units='in')
    make.correlation.scatterplot(var1='suppression', var1.year=2019, correlate.var1 = F,
                                 var2='suppression.reduction', 
                                 scenario='base',
                                 outcome = 'incidence', outcome.years = CUM.INC.YEARS,
                                 point.size.range = CITY.COR.SIZE.RANGE,
                                 label.locations = character()) + #xlim(0,1) + ylim(0,1) +
        ylab("Correlation Between Reduction in Viral\nSuppression and Incidence 2020-2025") +
        xlab("Mean Proportion Suppressed in 2019") + CITY.COR.THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'msa_scatter/suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=CITY.COR.PANEL.HEIGHT, res=RES, units='in')
    make.correlation.scatterplot(var1='suppression', var1.year=2019, correlate.var1 = F,
                                 var2='suppression.reduction', 
                                 scenario='base',
                                 outcome = 'incidence', outcome.years = CUM.INC.YEARS,
                                 point.size.range = CITY.COR.SIZE.RANGE,
                                 label.locations = character()) + #xlim(0,1) + ylim(0,1) +
        ylab("Correlation Between Reduction in Viral\nSuppression and Incidence 2020-2025") +
        xlab("Mean Proportion Suppressed in 2019") + CITY.COR.THEME
    dev.off()
    
    #For identifying specific cities:
    location.codes = names(location.names);names(location.codes)=location.codes
    make.correlation.scatterplot(var1='suppression', var1.year=2019, correlate.var1 = F,
                                 var2='suppression.reduction', 
                                 scenario='base',
                                 outcome = 'incidence', outcome.years = CUM.INC.YEARS,
                                 point.size.range = CITY.COR.SIZE.RANGE,
                                 #label.locations=names(location.names))
                                 label.locations=c('41740','14460','16980'),
                                 loc.names = location.codes) 
    location.names[c('41740','14460','16980')]
}

##-------------------


##-- New according to incidence --##
if (1==2)
{
    cum.inc.baseline = apply(outcomes.arr[,'baseline','none',as.character(CUM.INC.YEARS),'incidence',], c('location','sim'), sum)
    cum.inc.base = apply(outcomes.arr[,'base','none',as.character(CUM.INC.YEARS),'incidence',], c('location','sim'), sum) - cum.inc.baseline
    
    mask = colSums(cum.inc.base) > 0
    groups = rep("Cumulative Incidence Less Than If No COVID", length(mask))
    groups[mask] = "Cumulative Incidence Greater Than If No COVID"
    
    
    png(file.path(IMAGE.DIR, 'timeline/base_new_by_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=names(COLORS)[1:2], outcomes='new', line.size = LINE.SIZE,
                             groups = groups,
                             line.types=c('solid','dashed'),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = COLORS[1:2]) + THEME
    dev.off()
    
    
    
    cum.inc.delayed = apply(outcomes.arr[,'delayed.hiv.care','none',as.character(CUM.INC.YEARS),'incidence',], c('location','sim'), sum) - cum.inc.baseline
    
    mask = colSums(cum.inc.delayed) > 0; mean(mask)
    groups = rep("Cumulative Incidence Less Than If No COVID", length(mask))
    groups[mask] = "Cumulative Incidence Greater Than If No COVID"
    
    
    png(file.path(IMAGE.DIR, 'timeline/delayed_new_by_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=names(COLORS)[c(1,3)], outcomes='new', line.size = LINE.SIZE,
                             groups = groups,
                             line.types=c('solid','dashed'),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = COLORS[c(1,3)]) + THEME
    dev.off()
    
}

##-- Interventions --##
if (1==2)
{
    png(file.path(IMAGE.DIR, 'timeline/base_int_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=names(COLORS)[1:2], outcomes='incidence', line.size = LINE.SIZE,
                             intervention.names = c(NA, 'testing.50.6.6'),
                             line.types=c('solid','solid','dashed'),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = COLORS[c(1:2,2)]) + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'timeline/delayed_int_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=names(COLORS)[c(1,3)], outcomes='incidence', line.size = LINE.SIZE,
                             intervention.names = c(NA, 'testing.50.6.6'),
                             line.types=c('solid','solid','dashed'),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = COLORS[c(1,3,3)]) + THEME
    dev.off()
    
 
    png(file.path(IMAGE.DIR, 'timeline/int_boxplots.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.boxplot(scenarios=names(COLORS)[-1], outcomes='incidence', 
                       subtract.scenario='self',
                       intervention.names = 'testing.50.6.6',
                       years = CUM.INC.YEARS,
                       include.baseline = F,
                       line.size = LINE.SIZE,
                       ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                       colors = COLORS[-1]) + THEME + 
        theme(axis.text.x=element_blank(), axis.title.x=element_blank(), axis.ticks.x=element_blank())
    dev.off()
    
    
    diff.by.int = outcomes.arr[,,2,as.character(CUM.INC.YEARS),,] - outcomes.arr[,,1,as.character(CUM.INC.YEARS),,]
    cum.diff.by.int = apply(diff.by.int, setdiff(names(dim(diff.by.int)), 'year'), sum)
    aggregate.cum.diff.by.int = apply(cum.diff.by.int, c('scenario','outcome','sim'), sum)
    
    int.inc.effect = cbind(apply(aggregate.cum.diff.by.int[-1,'incidence',], 1, mean),
                           t(apply(aggregate.cum.diff.by.int[-1,'incidence',], 1, quantile, probs=c(.025,.975))))
    round(int.inc.effect)
}


#change by year
if (1==2)
{
    diff.years = 2020:2030
    n.year = dim(outcomes.arr)['year']
    delta.outcomes = outcomes.arr[,,,-1,,] - outcomes.arr[,,,-n.year,,]
    delta.outcomes = delta.outcomes[,,,as.character(diff.years),,]
    
    outcomes.minus.2019 = sapply(diff.years, function(year){
        outcomes.arr[,,,as.character(year),,] - outcomes.arr[,,,'2019',,]
    })
    dim.names = c(dimnames(delta.outcomes)[setdiff(names(dimnames(delta.outcomes)), 'year')],
                  list(year=as.character(diff.years)))
    dim(outcomes.minus.2019) = sapply(dim.names, length)
    dimnames(outcomes.minus.2019) = dim.names
    outcomes.minus.2019 = apply(outcomes.minus.2019, names(dimnames(delta.outcomes)), function(x){x})
    
    sc = 'base'
    int = 'none'
    non.baseline = setdiff(dimnames(delta.outcomes)[['scenario']], 'baseline')
    delayed.scenarios = c('delayed.hiv.care', 'rebound.sex.delayed.hiv.care')
    non.delayed.scenarios = c('base','rebound.sexual.transmission')
    
    new.decreasing = delta.outcomes[,non.baseline,int,,'new',] < 0
    inc.decreasing = delta.outcomes[,non.baseline,int,,'incidence',] < 0
    new.lt.2019 = outcomes.minus.2019[,non.baseline,int,,'new',]<0
    
    round(100*apply(delta.outcomes[,'baseline',int,,'new',]<0, c('year'), mean))
    
   # round(100*apply(new.decreasing[,'baseline',,], 'year', mean))
    
    #top line numbers
    round(100*apply(new.decreasing, 'year', mean))
    round(100*apply(inc.decreasing, 'year', mean))
    round(100*apply(new.decreasing & !inc.decreasing, 'year', sum) / apply(!inc.decreasing, 'year', sum))
    round(100*apply(!new.decreasing & inc.decreasing, 'year', sum) / apply(inc.decreasing, 'year', sum))
    round(100*apply(new.lt.2019, 'year', mean))
    
    #by scenario and year
    round(100*apply(new.decreasing, c('scenario','year'), mean))
    round(100*apply(new.decreasing & !inc.decreasing, c('scenario','year'), sum) / apply(!inc.decreasing, c('scenario','year'), sum))
    
    round(100*apply(new.decreasing[,delayed.scenarios,,], c('year'), mean))
    round(100*apply(new.decreasing[,non.delayed.scenarios,,], c('year'), mean))
    
    round(100*apply(new.lt.2019[,delayed.scenarios,,], 'year', mean))
    round(100*apply(new.lt.2019[,non.delayed.scenarios,,], 'year', mean))
    
    round(100*apply(new.decreasing[,delayed.scenarios,,] & !inc.decreasing[,delayed.scenarios,,], 'year', sum) / apply(!inc.decreasing[,delayed.scenarios,,], 'year', sum))
    round(100*apply(new.decreasing[,non.delayed.scenarios,,] & !inc.decreasing[,non.delayed.scenarios,,], 'year', sum) / apply(!inc.decreasing[,non.delayed.scenarios,,], 'year', sum))
    
    
    #correlation with parameters
    
    dim(delta.outcomes)
    sc='base'
    year=2022
    base.delta.2022.cor = sapply(dimnames(parameters)[[2]], function(param){
        sapply(names(location.names), function(loc){
            cor(delta.outcomes[loc,sc,'none',as.character(2022),'new',],
                parameters[,param],
                method='spearman')
        })
    })
    round(100*base.delta.2022.cor)
    
    base.delta.2021.cor = sapply(dimnames(parameters)[[2]], function(param){
        sapply(names(location.names), function(loc){
            cor(delta.outcomes[loc,sc,'none',as.character(2021),'new',],
                parameters[,param],
                method='spearman')
        })
    })
    round(100*base.delta.2021.cor)
    
    
    round(100*apply(base.delta.2020.cor, 2, range))
    round(100*apply(base.delta.2021.cor, 2, range))
    round(100*apply(base.delta.2022.cor, 2, range))
    
    base.delta.2021.cor = sapply(dimnames(parameters)[[2]], function(param){
        sapply(names(location.names), function(loc){
            cor(delta.outcomes[loc,sc,'none',as.character(2020),'new',],
                parameters[,param],
                method='spearman')
        })
    })
    round(100*base.delta.2020.cor)
    
    
    
    # scatter
    qplot(delta.outcomes[,sc,int,'2022','incidence',], delta.outcomes[,sc,int,'2022','new',])
    
    
    round(100*apply(delta.outcomes[,sc,int,,outc,]<0, c('year'), mean))
    
    aggregate.delta.outcomes = apply(delta.outcomes, setdiff(names(dim(delta.outcomes)), 'location'), sum)
    apply(aggregate.delta.outcomes[sc,int,,outc,], c('year'), range)
    
    qplot(aggregate.delta.outcomes[sc,int,'2020','new',], aggregate.delta.outcomes[sc,int,'2020','incidence',])
}

#text for drivers 
if (1==2)
{
    tile.df = do.make.tile.df(subtract.relative = F, out)
}


##-- MSA-level Correlations --##
if (1==2)
{
    cum.inc.baseline = apply(outcomes.arr[,'baseline','none',as.character(CUM.INC.YEARS),'incidence',], c('location','sim'), sum)
    cum.inc.base = apply(outcomes.arr[,'base','none',as.character(CUM.INC.YEARS),'incidence',], c('location','sim'), sum) - cum.inc.baseline
    cum.inc.delayed = apply(outcomes.arr[,'delayed.hiv.care','none',as.character(CUM.INC.YEARS),'incidence',], c('location','sim'), sum) - cum.inc.baseline
    cum.inc.rebound = apply(outcomes.arr[,'rebound.sexual.transmission','none',as.character(CUM.INC.YEARS),'incidence',], c('location','sim'), sum) - cum.inc.baseline
    
   
    # Base
    msa.param.cors.base = sapply(dimnames(parameters)[[2]], function(pp){
        sapply(names(location.names), function(loc){
            cor(cum.inc.base[loc,], parameters[,pp], method='spearman')
        })
    })
    
    round(100*t(apply(msa.param.cors.base, 2, range)))
    
    # Base - rank
    rank.cor.base = t(apply(msa.param.cors.base, 1, function(x){
        length(x) + 1 - rank(abs(x))
    }));dimnames(rank.cor.base)=list(location=names(location.names), parameter=dimnames(parameters)[[2]]);rank.cor.base
    mean(rank.cor.base[,'sexual.transmission.reduction']==1)
    mean(rank.cor.base[,'suppression.reduction']==2)
    mean(rank.cor.base[,'testing.reduction']==2)
    
    
    # Delayed
    msa.param.cors.delayed = sapply(dimnames(parameters)[[2]], function(pp){
        sapply(names(location.names), function(loc){
            cor(cum.inc.delayed[loc,], parameters[,pp], method='spearman')
        })
    })
    
    round(100*t(apply(msa.param.cors.delayed, 2, range)))
    rank.cor.delayed = t(apply(msa.param.cors.delayed, 1, function(x){
        length(x) + 1 - rank(abs(x))
    }));dimnames(rank.cor.delayed)=list(location=names(location.names), parameter=dimnames(parameters)[[2]]);rank.cor.delayed
    mean(rank.cor.delayed[,'sexual.transmission.reduction']==1)
    mean(rank.cor.delayed[,'suppression.reduction']==2)
    mean(rank.cor.delayed[,'testing.reduction']==2)
    
    # Rebound
    msa.param.cors.rebound = sapply(dimnames(parameters)[[2]], function(pp){
        sapply(names(location.names), function(loc){
            cor(cum.inc.rebound[loc,], parameters[,pp], method='spearman')
        })
    })
    
    round(100*t(apply(msa.param.cors.rebound, 2, range)))
    rank.cor.rebound = t(apply(msa.param.cors.rebound, 1, function(x){
        length(x) + 1 - rank(abs(x))
    }));dimnames(rank.cor.rebound)=list(location=names(location.names), parameter=dimnames(parameters)[[2]]);rank.cor.rebound
    mean(rank.cor.rebound[,'sexual.transmission.reduction']==1)
    mean(rank.cor.rebound[,'suppression.reduction']==2)
    mean(rank.cor.rebound[,'testing.reduction']==2)
    
    # Base - New
    cum.new.baseline = apply(outcomes.arr[,'baseline','none',as.character(CUM.INC.YEARS),'new',], c('location','sim'), sum)
    cum.new.base = apply(outcomes.arr[,'base','none',as.character(CUM.INC.YEARS),'new',], c('location','sim'), sum) - cum.new.baseline
    new.2020.baseline = apply(outcomes.arr[,'baseline','none',as.character(2020),'new',], c('location','sim'), sum)
    new.2020.base = apply(outcomes.arr[,'base','none',as.character(2020),'new',], c('location','sim'), sum) - new.2020.baseline
    
    msa.param.cors.base.new = sapply(dimnames(parameters)[[2]], function(pp){
        sapply(names(location.names), function(loc){
            #or(cum.new.base[loc,], parameters[,pp], method='spearman')
            cor(new.2020.base[loc,], parameters[,pp], method='spearman')
        })
    })
    
    round(100*t(apply(msa.param.cors.base.new, 2, range)))
    
    # Base - rank new
    rank.cor.base = t(apply(msa.param.cors.base, 1, function(x){
        length(x) + 1 - rank(abs(x))
    }));dimnames(rank.cor.base)=list(location=names(location.names), parameter=dimnames(parameters)[[2]]);rank.cor.base
    mean(rank.cor.base[,'sexual.transmission.reduction']==1)
    mean(rank.cor.base[,'suppression.reduction']==2)
    mean(rank.cor.base[,'testing.reduction']==2)
    
    
    #Plot sex vs supp
    msa.supp.cors = msa.param.cors[,'suppression.reduction']
    names(msa.supp.cors) = location.names
    sort(msa.supp.cors)
    
    msa.sex.cors = msa.param.cors[,'sexual.transmission.reduction']
    names(msa.sex.cors) = location.names
    sort(msa.sex.cors)
    
    cor.df = data.frame(location=names(location.names),
                        sex.cor=msa.sex.cors,
                        supp.cor=msa.supp.cors)
    ggplot(cor.df, aes(x=sex.cor, y=supp.cor)) + geom_point() +
        geom_text(aes(label=location))
}

##-- Multi-param subsets --##
if (1==2)
{
    normal.sex.low.supp.mask = parameters[,'sexual.transmission.reduction'] < 0.2 &
        parameters[,'suppression.reduction'] > 0.2# &
       # parameters[,'testing.reduction'] > 0.25
    low.sex.normal.supp.mask = parameters[,'sexual.transmission.reduction'] > 0.3 &
        parameters[,'suppression.reduction'] < 0.2# &
       # parameters[,'testing.reduction'] < 0.25
    
    groups.high.low = groups.high = groups.low = rep(NA, dim(parameters)[1])
    groups.high.low[normal.sex.low.supp.mask] = groups.high[normal.sex.low.supp.mask] = 'Normal Sex, Low Suppression'
    groups.high.low[low.sex.normal.supp.mask] = groups.low[low.sex.normal.supp.mask] = 'Low Sex, Normal Suppression'
    
    png(file.path(IMAGE.DIR, 'timeline/split_timeline_two_base.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=names(COLORS)[1:2], outcomes='incidence', line.size = LINE.SIZE,
                             groups = groups.high.low,
                             line.types=c('dashed','solid'),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = COLORS[1:2]) + THEME
    dev.off()
    
    
    make.covid.timeline.plot(scenarios=names(COLORS)[c(1,3)], outcomes='incidence', line.size = LINE.SIZE,
                             groups = groups.high.low,
                             line.types=c('solid','dashed'),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = COLORS[c(1,3)]) + THEME
    
    make.covid.timeline.plot(scenarios=names(COLORS)[c(1,2,3)], outcomes='incidence', line.size = LINE.SIZE,
                             groups = groups.high.low,
                             line.types=c('solid','dashed'),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = COLORS[c(1,2,3)]) + THEME
    
    
    png(file.path(IMAGE.DIR, 'timeline/split_timeline_three_base_rd.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=names(COLORS)[c(1,2,5)], outcomes='incidence', line.size = LINE.SIZE,
                             groups = list(groups.high.low, groups.high),
                             line.types=c('dashed','solid'),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = COLORS[c(1,2,5)]) + THEME
    dev.off()
    
    make.covid.timeline.plot(scenarios=names(COLORS)[c(5)], outcomes=c('incidence','new'), line.size = LINE.SIZE,
                             line.types=c('solid','dashed'),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = COLORS[c(5)],
                             groups=groups.high,
                             linetype.by.outcome = T, include.baseline = F ) + THEME
    
    
    make.covid.timeline.plot(scenarios=names(COLORS)[c(2)], outcomes=c('incidence','new'), line.size = LINE.SIZE,
                             line.types=c('solid','dashed'),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = COLORS[c(2)],
                             groups=groups.low,
                             linetype.by.outcome = T, include.baseline = F ) + THEME
    
    
    make.covid.timeline.plot(scenarios=names(COLORS)[c(2)], outcomes=c('incidence','new'), line.size = LINE.SIZE,
                             line.types=c('solid','dashed'),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = COLORS[c(2)],
                             groups=groups.high,
                             linetype.by.outcome = T, include.baseline = F ) + THEME
    
    groups.low.testing = rep(NA, dim(parameters)[1])
    groups.low.testing[parameters[,'testing.reduction']>.3] = 'Low Testing'
    groups.high.testing = rep(NA, dim(parameters)[1])
    groups.high.testing[parameters[,'testing.reduction']<.2] = 'High Testing'
    
    
    make.covid.timeline.plot(scenarios=names(COLORS)[c(2)], outcomes=c('incidence','new'), line.size = LINE.SIZE,
                             line.types=c('solid','dashed'),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = COLORS[c(2)],
                             groups=groups.low.testing,
                             linetype.by.outcome = T, include.baseline = F ) + THEME
    
    
    
    make.covid.timeline.plot(scenarios=names(COLORS)[c(5)], outcomes=c('incidence','new'), line.size = LINE.SIZE,
                             line.types=c('solid','dashed'),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = COLORS[c(5)],
                             groups=groups.low.testing,
                             linetype.by.outcome = T, include.baseline = F ) + THEME
    
}