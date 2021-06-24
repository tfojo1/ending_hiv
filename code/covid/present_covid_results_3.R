
source('code/covid/covid_plots.R')

library(colorspace)

##-- SET-UP --##
IMAGE.DIR = '../Manuscripts/covid_manuscript/images'
library(ggsci)
PALETTE = pal_jama()
COLORS = PALETTE(6)[-5][c(1,2,5,3,4)]
#COLORS = PALETTE(7)[-c(5,6)]#[c(1,2,5,3,4)]
names(COLORS) = c('baseline',
                  'base',
                  'delayed.hiv.care',
                  'rebound.sexual.transmission',
                  'rebound.sex.delayed.hiv.care')

DARK.COLORS = darken(darken(COLORS))
names(DARK.COLORS) = names(COLORS)

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
    load('code/covid/results/covid_4.0_results.Rdata')
    
}

##----------##
##-- TEXT --##
##----------##

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
    base.mask = cum.sub.df.abs$scenario==cum.sub.df.abs$scenario[2]
    paste0("Cumulative inc ", min(CUM.INC.YEARS), "-", max(CUM.INC.YEARS), " (base COVID): ", 
           format(round(cum.sub.df.abs$estimate[base.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','),
           " (", format(round(cum.sub.df.abs$ci.lower[base.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','),
           " to ", format(round(cum.sub.df.abs$ci.upper[base.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','), ")")
    
    cum.sub.df.rel = prepare.timeline.df(results=outcomes.arr, subtract.scenario = 'baseline', 
                                         subtract.relative = T,
                                         cumulative = T, years=CUM.INC.YEARS)
    paste0("Relative Change ", min(CUM.INC.YEARS), "-", max(CUM.INC.YEARS), " (base COVID): ", 
           round(100*cum.sub.df.rel$estimate[base.mask & cum.sub.df.rel$year==max(CUM.INC.YEARS)]),
           "% (", round(100*cum.sub.df.rel$ci.lower[base.mask & cum.sub.df.rel$year==max(CUM.INC.YEARS)]),
           " to ", round(100*cum.sub.df.rel$ci.upper[base.mask & cum.sub.df.rel$year==max(CUM.INC.YEARS)]), "%)")
    
    
    delayed.mask = cum.sub.df.abs$scenario==cum.sub.df.abs$scenario[3]
    paste0("Cumulative inc ", min(CUM.INC.YEARS), "-", max(CUM.INC.YEARS), " (delayed): ", 
           format(round(cum.sub.df.abs$estimate[delayed.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','),
           " (", format(round(cum.sub.df.abs$ci.lower[delayed.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','),
           " to ", format(round(cum.sub.df.abs$ci.upper[delayed.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','), ")")
    paste0("Relative Change ", min(CUM.INC.YEARS), "-", max(CUM.INC.YEARS), " (delayed): ", 
           round(100*cum.sub.df.rel$estimate[delayed.mask & cum.sub.df.rel$year==max(CUM.INC.YEARS)]),
           "% (", round(100*cum.sub.df.rel$ci.lower[delayed.mask & cum.sub.df.rel$year==max(CUM.INC.YEARS)]),
           " to ", round(100*cum.sub.df.rel$ci.upper[delayed.mask & cum.sub.df.rel$year==max(CUM.INC.YEARS)]), "%)")
    
    rebound.mask = cum.sub.df.abs$scenario==cum.sub.df.abs$scenario[4]
    paste0("Cumulative inc ", min(CUM.INC.YEARS), "-", max(CUM.INC.YEARS), " (rebound): ", 
           format(round(cum.sub.df.abs$estimate[rebound.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','),
           " (", format(round(cum.sub.df.abs$ci.lower[rebound.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','),
           " to ", format(round(cum.sub.df.abs$ci.upper[rebound.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','), ")")
    paste0("Relative Change ", min(CUM.INC.YEARS), "-", max(CUM.INC.YEARS), " (delayed): ", 
           round(100*cum.sub.df.rel$estimate[rebound.mask & cum.sub.df.rel$year==max(CUM.INC.YEARS)],1),
           "% (", round(100*cum.sub.df.rel$ci.lower[rebound.mask & cum.sub.df.rel$year==max(CUM.INC.YEARS)]),
           " to ", round(100*cum.sub.df.rel$ci.upper[rebound.mask & cum.sub.df.rel$year==max(CUM.INC.YEARS)]), "%)")
    
    rebound.delayed.mask = cum.sub.df.abs$scenario==cum.sub.df.abs$scenario[5]
    paste0("Cumulative inc ", min(CUM.INC.YEARS), "-", max(CUM.INC.YEARS), " (rebound+delayed): ", 
           format(round(cum.sub.df.abs$estimate[rebound.delayed.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','),
           " (", format(round(cum.sub.df.abs$ci.lower[rebound.delayed.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','),
           " to ", format(round(cum.sub.df.abs$ci.upper[rebound.delayed.mask & cum.sub.df.abs$year==max(CUM.INC.YEARS)]), big.mark=','), ")")
    paste0("Relative Change ", min(CUM.INC.YEARS), "-", max(CUM.INC.YEARS), " (delayed): ", 
           round(100*cum.sub.df.rel$estimate[rebound.delayed.mask & cum.sub.df.rel$year==max(CUM.INC.YEARS)],0),
           "% (", round(100*cum.sub.df.rel$ci.lower[rebound.delayed.mask & cum.sub.df.rel$year==max(CUM.INC.YEARS)]),
           " to ", round(100*cum.sub.df.rel$ci.upper[rebound.delayed.mask & cum.sub.df.rel$year==max(CUM.INC.YEARS)]), "%)")
    
    
    msa.cum.sub.df.rel = prepare.timeline.df(results=outcomes.arr, subtract.scenario = 'baseline', 
                                         subtract.relative = T,
                                         aggregate.locations = F,
                                         cumulative = T, years=CUM.INC.YEARS)
    
    boston.base.mask = msa.cum.sub.df.rel$scenario==msa.cum.sub.df.rel$scenario[32+1] & msa.cum.sub.df.rel$location==14460
    paste0("BOSTON: Relative Change ", min(CUM.INC.YEARS), "-", max(CUM.INC.YEARS), " (base COVID): ", 
           round(100*msa.cum.sub.df.rel$estimate[boston.base.mask & msa.cum.sub.df.rel$year==max(CUM.INC.YEARS)]),
           "% (", round(100*msa.cum.sub.df.rel$ci.lower[boston.base.mask & msa.cum.sub.df.rel$year==max(CUM.INC.YEARS)]),
           " to ", round(100*msa.cum.sub.df.rel$ci.upper[boston.base.mask & msa.cum.sub.df.rel$year==max(CUM.INC.YEARS)]), "%)")
    
    chicago.base.mask = msa.cum.sub.df.rel$scenario==msa.cum.sub.df.rel$scenario[32+1] & msa.cum.sub.df.rel$location==16980
    paste0("CHICAGO: Relative Change ", min(CUM.INC.YEARS), "-", max(CUM.INC.YEARS), " (base COVID): ", 
           round(100*msa.cum.sub.df.rel$estimate[chicago.base.mask & msa.cum.sub.df.rel$year==max(CUM.INC.YEARS)]),
           "% (", round(100*msa.cum.sub.df.rel$ci.lower[chicago.base.mask & msa.cum.sub.df.rel$year==max(CUM.INC.YEARS)]),
           " to ", round(100*msa.cum.sub.df.rel$ci.upper[chicago.base.mask & msa.cum.sub.df.rel$year==max(CUM.INC.YEARS)]), "%)")
    
    
    prev.df.rel = prepare.timeline.df(results=outcomes.arr, subtract.scenario = 'baseline', 
                                          outcomes = 'prevalence.all',
                                         subtract.relative = T,
                                         cumulative = F, years=max(CUM.INC.YEARS))
    round(100*prev.df.rel[,c('estimate','ci.lower','ci.upper')],1)
    
    
    msa.prev.df.rel = prepare.timeline.df(results=outcomes.arr, subtract.scenario = 'baseline', 
                                      outcomes = 'prevalence.all',
                                      aggregate.locations = F,
                                      subtract.relative = T,
                                      cumulative = F, years=max(CUM.INC.YEARS))
    round(100*msa.prev.df.rel[,c('estimate','ci.lower','ci.upper')])
    round(range(100*msa.prev.df.rel[,'ci.lower']),1)
    round(range(100*msa.prev.df.rel[,'ci.upper']),1)
    round(range(100*msa.prev.df.rel[,'estimate']),1)
    
    o = order(msa.prev.df.rel$estimate)
    cbind(msa.prev.df.rel[o,c('location','scenario')],
          round(100*msa.prev.df.rel[o,c('estimate','ci.lower','ci.upper')],1))
    
    #inc
    
    inc.df.rel = prepare.timeline.df(results=outcomes.arr, subtract.scenario = 'baseline', 
                                      outcomes = 'incidence',
                                      subtract.relative = T,
                                      cumulative = F, years=max(CUM.INC.YEARS))
    round(100*inc.df.rel[,c('estimate','ci.lower','ci.upper')],1)
    
    
    msa.inc.df.rel = prepare.timeline.df(results=outcomes.arr, subtract.scenario = 'baseline', 
                                          outcomes = 'incidence',
                                          aggregate.locations = F,
                                          subtract.relative = T,
                                          cumulative = F, years=2030)#max(CUM.INC.YEARS))
    round(100*msa.inc.df.rel[,c('estimate','ci.lower','ci.upper')])
    round(range(100*msa.inc.df.rel[,'ci.lower']),1)
    round(range(100*msa.inc.df.rel[,'ci.upper']),1)
    round(range(100*msa.inc.df.rel[,'estimate']),1)
    
    o = order(msa.inc.df.rel$estimate)
    cbind(msa.inc.df.rel[o,c('location','scenario')],
          round(100*msa.inc.df.rel[o,c('estimate','ci.lower','ci.upper')],1))
    
}


##---------------------------##
##-- OVERVIEW AND TIMELINE --##
##---------------------------##

##-- TIMELINE and BOX-PLOTS --##
TIMELINE.RIBBON.ALPHA = 0.1
LOCATION.BOXPLOT.PANEL.HEIGHT = 6
LOCATION.BOXPLOT.PANEL.WIDTH = 6
if (1==2)
{    
    png(file.path(IMAGE.DIR, 'timelines/all_scenarios_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=names(COLORS), outcomes='incidence', line.size = LINE.SIZE,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                             colors = COLORS) + THEME
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'timelines/all_scenarios_new.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=names(COLORS), outcomes='new', line.size = LINE.SIZE,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                             colors = COLORS) + THEME
    dev.off()
    
    

    png(file.path(IMAGE.DIR, 'boxplots/all_scenarios_cum_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.boxplot(scenarios=names(COLORS)[-1], outcomes='incidence', 
                       years = CUM.INC.YEARS,
                       subtract.scenario = 'baseline', include.baseline = F,
                       line.size = LINE.SIZE,
                       ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                       colors = COLORS[-1]) + THEME + 
        theme(axis.text.x=element_blank(), axis.title.x=element_blank(), axis.ticks.x=element_blank())
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'boxplots/all_scenarios_cum_new.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.boxplot(scenarios=names(COLORS)[-1], outcomes='new', 
                       years = CUM.INC.YEARS,
                       subtract.scenario = 'baseline', include.baseline = F,
                       line.size = LINE.SIZE,
                       ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                       colors = COLORS[-1]) + THEME + 
        theme(axis.text.x=element_blank(), axis.title.x=element_blank(), axis.ticks.x=element_blank())
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'boxplots/location_estimates_base.png'), pointsize=PNG.POINT.SIZE, 
        width=LOCATION.BOXPLOT.PANEL.WIDTH, height=LOCATION.BOXPLOT.PANEL.HEIGHT, res=RES, units='in')
    make.location.boxplot(colors=COLORS[-1],
                          scenarios = c('base')) +
        THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'boxplots/location_estimates_base_delayed.png'), pointsize=PNG.POINT.SIZE, 
        width=LOCATION.BOXPLOT.PANEL.WIDTH, height=LOCATION.BOXPLOT.PANEL.HEIGHT, res=RES, units='in')
    make.location.boxplot(colors=COLORS[-1],
                          scenarios = c('base','delayed.hiv.care')) + THEME
    dev.off()
    
    
    
    png(file.path(IMAGE.DIR, 'boxplots/location_estimates_base_r.d.png'), pointsize=PNG.POINT.SIZE, 
        width=LOCATION.BOXPLOT.PANEL.WIDTH, height=LOCATION.BOXPLOT.PANEL.HEIGHT, res=RES, units='in')
    make.location.boxplot(colors=COLORS[c('base','rebound.sex.delayed.hiv.care')],
                          scenarios = c('base','rebound.sex.delayed.hiv.care')) + THEME
    dev.off()
    
}


##-- HEAT MAPS --##

HEAT.MAP.RANGE = c(-.25,.25)#c(-20000, 20000)
HEAT.MAP.PANEL.HEIGHT = 2.25
HEAT.MAP.THEME = THEME + theme(legend.position = 'bottom')

do.make.covid.heat.map <- function(loc=NULL,
                                   subtract.relative=T,
                                   scenario='base',
                                   outcome.years=CUM.INC.YEARS)
{
    aggregate.loc = is.null(loc)
    if (is.null(loc))
        loc = names(location.names)
    
    make.covid.heat.map(var1 = 'sexual.transmission.reduction',
                        var2 = 'suppression.reduction',
                        min.change = HEAT.MAP.RANGE[1],
                        max.change = HEAT.MAP.RANGE[2],
                        scenario=scenario,
                        outcome = 'incidence', outcome.years = outcome.years,
                        locations = loc, aggregate.locations = aggregate.loc,
                        color.scale.title = " ", #"Change in Cumulative\nIncidence 2020-2025",
                        subtract.relative = subtract.relative) + 
        xlab("Reduction in Sexual Transmission") + ylab("Reduction in Viral Suppression") + THEME
}

if (1==2)
{
    # For the legend
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
    
    
    png(file.path(IMAGE.DIR, 'heat_maps/total_base_sex_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map()
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/total_r.d_sex_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map(scenario='rebound.sex.delayed.hiv.care')
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'heat_maps/chicago_base_sex_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('16980')
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/chicago_r.d_sex_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('16980', scenario='rebound.sex.delayed.hiv.care')
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/boston_base_sex_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('14460')
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/boston_r.d_sex_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('14460', scenario='rebound.sex.delayed.hiv.care')
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/sd_base_sex_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('41740')
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/sd_r.d_sex_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('41740', scenario='rebound.sex.delayed.hiv.care')
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
    
    png(file.path(IMAGE.DIR, 'msa_scatter/transmission_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=CITY.COR.PANEL.HEIGHT, res=RES, units='in')
    make.correlation.scatterplot(var1='incidence.prevalence.ratio', var1.year=2019, correlate.var1 = F,
                                 var2='sexual.transmission.reduction', 
                                 locations = setdiff(names(location.names), '17140'), #minus cincinatti
                                 scenario='base',
                                 outcome = 'incidence', outcome.years = CUM.INC.YEARS,
                                 point.size.range = CITY.COR.SIZE.RANGE,
                                 label.locations = character()) + #xlim(0,1) + ylim(0,1) +
        ylab("Correlation Between Reduction in Viral\nSexual Transmission and Incidence 2020-2025") +
        xlab("Incidence/Prevalence Ratio in 2019") + CITY.COR.THEME
    dev.off()
    
    #For identifying specific cities:
    location.codes = names(location.names);names(location.codes)=location.codes
    make.correlation.scatterplot(var1='suppression', var1.year=2019, correlate.var1 = F,
                                 var2='suppression.reduction', 
                                 scenario='base',
                                 outcome = 'incidence', outcome.years = CUM.INC.YEARS,
                                 point.size.range = CITY.COR.SIZE.RANGE,
                                 #label.locations=names(location.names))
                                 locations=c('41740','14460','16980'),
                                 label.locations=c('41740','14460','16980'),
                                 loc.names = location.codes) 
    location.names[c('41740','14460','16980')]
    make.correlation.scatterplot(var1='incidence.prevalence.ratio', var1.year=2019, correlate.var1 = F,
                                 var2='sexual.transmission.reduction', 
                                 scenario='base',
                                 outcome = 'incidence', outcome.years = CUM.INC.YEARS,
                                 point.size.range = CITY.COR.SIZE.RANGE,
                              #   locations=c('41740','14460','16980'),
                                 label.locations = c('41740','14460','16980')) + #xlim(0,1) + ylim(0,1) +
        ylab("Correlation Between Reduction in Viral\nSexual Transmission and Incidence 2020-2025") +
        xlab("Incidence/Prevalence Ratio in 2019") + CITY.COR.THEME
}

##-- INCIDENCE vs REPORTED --##

QUANTILE.YLIM = c(12000,33000)
if (1==2)
{
    
    png(file.path(IMAGE.DIR, 'timelines/incvnew_base_low.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.quantile.timeline.plot(scenarios='base', outcomes=c('incidence','new'), line.size = LINE.SIZE,
                                show.quantiles = 1,
                                line.types=c('solid','dashed'),
                                ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                                show.ribbon.outline = T,
                                colors = COLORS[c('baseline','base')], color.by.outcome = F,
                                linetype.by.outcome = T, include.baseline = T,
                                baseline.outcomes = 'incidence',
                                ylim=QUANTILE.YLIM) + 
        ylab('Cases (n): Incident or Reported') + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'timelines/incvnew_base_high.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.quantile.timeline.plot(scenarios='base', outcomes=c('incidence','new'), line.size = LINE.SIZE,
                                show.quantiles = 5,
                                line.types=c('solid','dashed'),
                                ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                                show.ribbon.outline = T,
                                colors = COLORS[c('baseline','base')], color.by.outcome = F,
                                linetype.by.outcome = T, include.baseline = T,
                                baseline.outcomes = 'incidence',
                                ylim=QUANTILE.YLIM) +
        ylab('Cases (n): Incident or Reported') + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'timelines/incvnew_r.d_low.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.quantile.timeline.plot(scenarios='rebound.sex.delayed.hiv.care', outcomes=c('incidence','new'), line.size = LINE.SIZE,
                                show.quantiles = 1,
                                line.types=c('solid','dashed'),
                                ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                                show.ribbon.outline = T,
                                colors = COLORS[c('baseline','rebound.sex.delayed.hiv.care')], color.by.outcome = F,
                                linetype.by.outcome = T, include.baseline = T,
                                baseline.outcomes = 'incidence',
                                ylim=QUANTILE.YLIM) +
        ylab('Cases (n): Incident or Reported') + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'timelines/incvnew_r.d_high.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.quantile.timeline.plot(scenarios='rebound.sex.delayed.hiv.care', outcomes=c('incidence','new'), line.size = LINE.SIZE,
                                show.quantiles = 5,
                                line.types=c('solid','dashed'),
                                ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                                show.ribbon.outline = T,
                                colors = COLORS[c('baseline','rebound.sex.delayed.hiv.care')], color.by.outcome = F,
                                linetype.by.outcome = T, include.baseline = T,
                                baseline.outcomes = 'incidence',
                                ylim=QUANTILE.YLIM) +
        ylab('Cases (n): Incident or Reported') + THEME
    dev.off()
    
    
    
    normal.sex.low.supp.mask = parameters[,'sexual.transmission.reduction'] < 0.2 &
        parameters[,'suppression.reduction'] > 0.2# &
    # parameters[,'testing.reduction'] > 0.25
    low.sex.normal.supp.mask = parameters[,'sexual.transmission.reduction'] > 0.3 &
        parameters[,'suppression.reduction'] < 0.2# &
    # parameters[,'testing.reduction'] < 0.25
    
    groups.high.low = groups.high = groups.low = rep(NA, dim(parameters)[1])
    groups.high.low[normal.sex.low.supp.mask] = groups.high[normal.sex.low.supp.mask] = 'Normal Sex, Low Suppression'
    groups.high.low[low.sex.normal.supp.mask] = groups.low[low.sex.normal.supp.mask] = 'Low Sex, Normal Suppression'
    
    groups.low.testing = rep(NA, dim(parameters)[1])
    groups.low.testing[parameters[,'testing.reduction']>.3] = 'Low Testing'
    groups.high.testing = rep(NA, dim(parameters)[1])
    groups.high.testing[parameters[,'testing.reduction']<.2] = 'High Testing'
    
    
    png(file.path(IMAGE.DIR, 'timelines/incvnew_base_low.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='base', outcomes=c('incidence','new'), line.size = LINE.SIZE,
                             line.types=c('solid','dashed'),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = c(COLORS['base'],DARK.COLORS['base']), color.by.outcome = T,
                             groups=groups.low,
                             linetype.by.outcome = T, include.baseline = F ) + THEME
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'timelines/incvnew_base_high.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='base', outcomes=c('incidence','new'), line.size = LINE.SIZE,
                             line.types=c('solid','dashed'),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = c(COLORS['base'],DARK.COLORS['base']), color.by.outcome = T,
                             groups=groups.high,
                             linetype.by.outcome = T, include.baseline = F ) + THEME
    dev.off()
 
    png(file.path(IMAGE.DIR, 'timelines/incvnew_r.d_low.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='rebound.sex.delayed.hiv.care', outcomes=c('incidence','new'), line.size = LINE.SIZE,
                             line.types=c('solid','dashed'),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = c(COLORS['rebound.sex.delayed.hiv.care'],DARK.COLORS['rebound.sex.delayed.hiv.care']), color.by.outcome = T,
                             groups=groups.low,
                             linetype.by.outcome = T, include.baseline = F ) + THEME
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'timelines/incvnew_r.d_high.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='rebound.sex.delayed.hiv.care', outcomes=c('incidence','new'), line.size = LINE.SIZE,
                             line.types=c('solid','dashed'),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = c(COLORS['rebound.sex.delayed.hiv.care'],DARK.COLORS['rebound.sex.delayed.hiv.care']), color.by.outcome = T,
                             groups=groups.high,
                             linetype.by.outcome = T, include.baseline = F ) + THEME
    dev.off()   
}




##-- EXPLORING 2025 Inc instead of Cum Inc --##

if (1==2)
{
    aggregated.outcome = get.variable(scenario='base', years = 2025)[1,] - 
        get.variable(scenario='baseline', years = 2025)[1,]
    msa.outcome = get.variable(scenario='base', years=2025, aggregate.locations = F) -
        get.variable(scenario='baseline', years=2025, aggregate.locations = F)
    
    apply(parameters, 2, cor, aggregated.outcome, method='spearman')
    z=apply(parameters, 2, function(pp){
        apply(msa.outcome, 1, cor, pp, method='spearman')
    })
    apply(z, 2, range)
    
    
    
    aggregated.cumulative = get.variable(scenario='base', years = CUM.INC.YEARS)[1,] - 
        get.variable(scenario='baseline', years = CUM.INC.YEARS)[1,]
    qplot(aggregated.cumulative, aggregated.outcome)
    
    
    msa.cumulative = get.variable(scenario='base', years=CUM.INC.YEARS, aggregate.locations = F) -
        get.variable(scenario='baseline', years=CUM.INC.YEARS, aggregate.locations = F)
    qplot(msa.cumulative, msa.outcome)
}