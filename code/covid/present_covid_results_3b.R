
source('code/covid/covid_plots.R')

library(colorspace)

##-- SET-UP --##
IMAGE.DIR = '../Manuscripts/covid_manuscript/images'
library(ggsci)
PALETTE = pal_jama()
COLORS = PALETTE(6)[-5]#[c(1,2,5,3,4)]
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
}

# High vs low cumulative inc
if (1==2)
{
    normal.sex.low.supp.mask = parameters[,'sexual.transmission.reduction'] < 0.2 &
        parameters[,'suppression.reduction'] > 0.2# &
    # parameters[,'testing.reduction'] > 0.25
    low.sex.normal.supp.mask = parameters[,'sexual.transmission.reduction'] > 0.3 &
        parameters[,'suppression.reduction'] < 0.2# &
    # parameters[,'testing.reduction'] < 0.25
    # 
    total.cum.rel.inc = (get.variable(var.name='incidence', scenario='base', years = CUM.INC.YEARS) - 
                         get.variable(var.name='incidence', scenario='baseline', years = CUM.INC.YEARS)) /
        get.variable(var.name='incidence', scenario='baseline', years = CUM.INC.YEARS)
    
    round(100*c(mean(total.cum.rel.inc[,low.sex.normal.supp.mask]), quantile(total.cum.rel.inc[,low.sex.normal.supp.mask], probs=c(.025,.975))) , 1)
    round(100*c(mean(total.cum.rel.inc[,normal.sex.low.supp.mask]), quantile(total.cum.rel.inc[,normal.sex.low.supp.mask], probs=c(.025,.975))) , 1)
}

# Prevalence
PROJECT.TO.YEAR = 2030
if (1==2)
{
    
    total.rel.inc = (get.variable(var.name='incidence', scenario='base', years = 2030) - 
                         get.variable(var.name='incidence', scenario='baseline', years = 2030)) /
        get.variable(var.name='incidence', scenario='baseline', years = 2030)
    
    round(100*c(mean(total.rel.inc), quantile(total.rel.inc, probs=c(.025,.975))) , 1)
    
    
    msa.rel.inc = (get.variable(var.name='incidence', scenario='base', years = 2030, aggregate.locations = F) - 
                       get.variable(var.name='incidence', scenario='baseline', years = 2030, aggregate.locations = F)) /
        get.variable(var.name='incidence', scenario='baseline', years = 2030, aggregate.locations = F)
    round(100*cbind(mean=apply(msa.rel.inc, 1, mean),
          ci.lower=apply(msa.rel.inc, 1, quantile, probs=.025),
          ci.upper=apply(msa.rel.inc, 1, quantile, probs=0.975))[order(apply(msa.rel.inc, 1, mean)),])
    
    
    total.rel.inc.delayed = (get.variable(var.name='incidence', scenario='delayed.hiv.care', years = 2030) - 
                         get.variable(var.name='incidence', scenario='baseline', years = 2030)) /
        get.variable(var.name='incidence', scenario='baseline', years = 2030)
    
    round(100*c(mean(total.rel.inc.delayed), quantile(total.rel.inc.delayed, probs=c(.025,.975))) , 1)
    
    
    msa.rel.inc.delayed = (get.variable(var.name='incidence', scenario='delayed.hiv.care', years = 2030, aggregate.locations = F) - 
                       get.variable(var.name='incidence', scenario='baseline', years = 2030, aggregate.locations = F)) /
        get.variable(var.name='incidence', scenario='baseline', years = 2030, aggregate.locations = F)
    round(100*cbind(mean=apply(msa.rel.inc.delayed, 1, mean),
                    ci.lower=apply(msa.rel.inc.delayed, 1, quantile, probs=.025),
                    ci.upper=apply(msa.rel.inc.delayed, 1, quantile, probs=0.975))[order(apply(msa.rel.inc.delayed, 1, mean)),],1)
    
    
    # Prevalence
    
    total.rel.prev = (get.variable(var.name='prevalence.all', scenario='base', years = 2030) - 
                         get.variable(var.name='prevalence.all', scenario='baseline', years = 2030)) /
        get.variable(var.name='prevalence.all', scenario='baseline', years = 2030)
    
    round(100*c(mean(total.rel.prev), quantile(total.rel.prev, probs=c(.025,.975))) , 1)
    
    
    msa.rel.prev = (get.variable(var.name='prevalence.all', scenario='base', years = 2030, aggregate.locations = F) - 
                       get.variable(var.name='prevalence.all', scenario='baseline', years = 2030, aggregate.locations = F)) /
        get.variable(var.name='prevalence.all', scenario='baseline', years = 2030, aggregate.locations = F)
    round(100*cbind(mean=apply(msa.rel.prev, 1, mean),
                    ci.lower=apply(msa.rel.prev, 1, quantile, probs=.025),
                    ci.upper=apply(msa.rel.prev, 1, quantile, probs=0.975))[order(apply(msa.rel.prev, 1, mean)),])
    
    
    total.rel.prev.delayed = (get.variable(var.name='prevalence.all', scenario='delayed.hiv.care', years = 2030) - 
                                 get.variable(var.name='prevalence.all', scenario='baseline', years = 2030)) /
        get.variable(var.name='prevalence.all', scenario='baseline', years = 2030)
    
    round(100*c(mean(total.rel.prev.delayed), quantile(total.rel.prev.delayed, probs=c(.025,.975))) , 1)
    
    
    msa.rel.prev.delayed = (get.variable(var.name='prevalence.all', scenario='delayed.hiv.care', years = 2030, aggregate.locations = F) - 
                               get.variable(var.name='prevalence.all', scenario='baseline', years = 2030, aggregate.locations = F)) /
        get.variable(var.name='prevalence.all', scenario='baseline', years = 2030, aggregate.locations = F)
    round(100*cbind(mean=apply(msa.rel.prev.delayed, 1, mean),
                    ci.lower=apply(msa.rel.prev.delayed, 1, quantile, probs=.025),
                    ci.upper=apply(msa.rel.prev.delayed, 1, quantile, probs=0.975))[order(apply(msa.rel.prev.delayed, 1, mean)),],1)
    
    
    
    
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
    png(file.path(IMAGE.DIR, 'timelines/base_delayed_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=c('base','delayed.hiv.care'), outcomes='incidence', line.size = LINE.SIZE,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                             colors = COLORS) + THEME
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'timelines/boston_base_delayed_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=c('base','delayed.hiv.care'), outcomes='incidence', line.size = LINE.SIZE,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                             colors = COLORS, locations='14460') + THEME
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'timelines/chicago_base_delayed_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=c('base','delayed.hiv.care'), outcomes='incidence', line.size = LINE.SIZE,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                             colors = COLORS, locations='16980') + THEME
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'timelines/sd_base_delayed_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=c('base','delayed.hiv.care'), outcomes='incidence', line.size = LINE.SIZE,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                             colors = COLORS, locations='41740') + THEME
    dev.off()
    
    
    
    
    
    
    png(file.path(IMAGE.DIR, 'timelines/all_scenarios_new.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=names(COLORS), outcomes='new', line.size = LINE.SIZE,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                             colors = COLORS) + THEME
    dev.off()
    

    

    png(file.path(IMAGE.DIR, 'boxplots/location_estimates_base_delayed.png'), pointsize=PNG.POINT.SIZE, 
        width=LOCATION.BOXPLOT.PANEL.WIDTH, height=LOCATION.BOXPLOT.PANEL.HEIGHT, res=RES, units='in')
    make.location.boxplot(colors=COLORS[-1],
                          scenarios = c('base','delayed.hiv.care')) + THEME
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
    
    
    png(file.path(IMAGE.DIR, 'heat_maps/chicago_base_sex_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('16980')
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/chicago_delayed_sex_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('16980', scenario='delayed.hiv.care')
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/boston_base_sex_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('14460')
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/boston_delayed_sex_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('14460', scenario='delayed.hiv.care')
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/sd_base_sex_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('41740')
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/sd_delayed_sex_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('41740', scenario='delayed.hiv.care')
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
    
    
    
    
    # delayed 
    png(file.path(IMAGE.DIR, 'msa_scatter/suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=CITY.COR.PANEL.HEIGHT, res=RES, units='in')
    make.correlation.scatterplot(var1='suppression', var1.year=2019, correlate.var1 = F,
                                 var2='suppression.reduction', 
                                 scenario='delayed.hiv.care',
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
                                 scenario='delayed.hiv.care',
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

QUANTILE.YLIM = c(12000,31000)
if (1==2)
{
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported/incvnew_base_low.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
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
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported/incvnew_base_high.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
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
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported/incvnew_delayed_low.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.quantile.timeline.plot(scenarios='delayed.hiv.care', outcomes=c('incidence','new'), line.size = LINE.SIZE,
                                show.quantiles = 1,
                                line.types=c('solid','dashed'),
                                ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                                show.ribbon.outline = T,
                                colors = COLORS[c('baseline','delayed.hiv.care')], color.by.outcome = F,
                                linetype.by.outcome = T, include.baseline = T,
                                baseline.outcomes = 'incidence',
                                ylim=QUANTILE.YLIM) +
        ylab('Cases (n): Incident or Reported') + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported/incvnew_delayed_high.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.quantile.timeline.plot(scenarios='delayed.hiv.care', outcomes=c('incidence','new'), line.size = LINE.SIZE,
                                show.quantiles = 5,
                                line.types=c('solid','dashed'),
                                ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                                show.ribbon.outline = T,
                                colors = COLORS[c('baseline','delayed.hiv.care')], color.by.outcome = F,
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
    
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported/incvnew_base_low_parameters.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='base', outcomes=c('incidence','new'), line.size = LINE.SIZE,
                             line.types=c('solid','dashed'),
                             groups=groups.low,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = COLORS[c('baseline','base')], color.by.outcome = F,
                             linetype.by.outcome = T, include.baseline = T,
                             baseline.outcomes = 'incidence',
                             ylim=QUANTILE.YLIM) + 
        ylab('Cases (n): Incident or Reported') + THEME
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported/incvnew_base_high_parameters.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='base', outcomes=c('incidence','new'), line.size = LINE.SIZE,
                             line.types=c('solid','dashed'),
                             groups=groups.high,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = COLORS[c('baseline','base')], color.by.outcome = F,
                             linetype.by.outcome = T, include.baseline = T,
                             baseline.outcomes = 'incidence',
                             ylim=QUANTILE.YLIM) + 
        ylab('Cases (n): Incident or Reported') + THEME
    
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported/incvnew_delayed_low_parameters.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='delayed.hiv.care', outcomes=c('incidence','new'), line.size = LINE.SIZE,
                             line.types=c('solid','dashed'),
                             groups=groups.low,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = COLORS[c('baseline','delayed.hiv.care')], color.by.outcome = F,
                             linetype.by.outcome = T, include.baseline = T,
                             baseline.outcomes = 'incidence',
                             ylim=QUANTILE.YLIM) + 
        ylab('Cases (n): Incident or Reported') + THEME
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported/incvnew_delayed_high_parameters.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='delayed.hiv.care', outcomes=c('incidence','new'), line.size = LINE.SIZE,
                             line.types=c('solid','dashed'),
                             groups=groups.high,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             colors = COLORS[c('baseline','delayed.hiv.care')], color.by.outcome = F,
                             linetype.by.outcome = T, include.baseline = T,
                             baseline.outcomes = 'incidence',
                             ylim=QUANTILE.YLIM) + 
        ylab('Cases (n): Incident or Reported') + THEME
    
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



##-- MSA-level Correlations --##
if (1==2)
{
    cum.inc.baseline = apply(outcomes.arr[,'baseline','none',as.character(CUM.INC.YEARS),'incidence',], c('location','sim'), sum)
    cum.inc.base = apply(outcomes.arr[,'base','none',as.character(CUM.INC.YEARS),'incidence',], c('location','sim'), sum) - cum.inc.baseline
    cum.inc.delayed = apply(outcomes.arr[,'delayed.hiv.care','none',as.character(CUM.INC.YEARS),'incidence',], c('location','sim'), sum) - cum.inc.baseline
    
    
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



#change by year
FOCUS.SCENARIOS = c('base','delayed.hiv.care')
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
    non.baseline = FOCUS.SCENARIOS#setdiff(dimnames(delta.outcomes)[['scenario']], 'baseline')
    delayed.scenarios = 'delayed.hiv.care'#c('delayed.hiv.care', 'rebound.sex.delayed.hiv.care')
    non.delayed.scenarios = 'base'#c('base','rebound.sexual.transmission')
    
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

