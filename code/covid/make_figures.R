
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

PANEL.WIDTH = 2.5
PANEL.HEIGHT = 1.75
PNG.POINT.SIZE = 5
RES = 600

LINE.SIZE = 1

THEME = theme(text = element_text(size=8), legend.direction = 'horizontal',
              legend.position = 'none')
CUM.INC.YEARS = 2020:2025

##-- SOURCE CODE --##
if (1==2)
{
    load('results/covid/covid_4.0_results.Rdata')
    
}

ALL.COVID.SCENARIOS = c('base','delayed.hiv.care','rebound.sex.delayed.hiv.care')
YEARS.OF.INTEREST = 2019:2025

NEW.LINE.TYPE = 'longdash'
##---------------------------##
##-- OVERVIEW AND TIMELINE --##
##---------------------------##

##-- TIMELINE and BOX-PLOTS --##
TIMELINE.RIBBON.ALPHA = 0.1
LOCATION.BOXPLOT.PANEL.HEIGHT = 6
LOCATION.BOXPLOT.PANEL.WIDTH = 6
if (1==2)
{    
    #total
    png(file.path(IMAGE.DIR, 'timelines/all_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=ALL.COVID.SCENARIOS, outcomes='incidence', line.size = LINE.SIZE,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                             colors = COLORS) + THEME
    dev.off()
    
    #total - new
    png(file.path(IMAGE.DIR, 'timelines/all_new.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=ALL.COVID.SCENARIOS, outcomes='new', line.size = LINE.SIZE,
                             line.types = NEW.LINE.TYPE, linetype.by.outcome = T, linetype.by.group = F,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                             colors = COLORS) + THEME
    dev.off()
    
    
    #boston
    png(file.path(IMAGE.DIR, 'timelines/boston_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=ALL.COVID.SCENARIOS, outcomes='incidence', line.size = LINE.SIZE,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                             colors = COLORS,
                             locations='14460') + THEME
    dev.off()
    
    #boston - new
    png(file.path(IMAGE.DIR, 'timelines/boston_new.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=ALL.COVID.SCENARIOS, outcomes='new', line.size = LINE.SIZE,
                             line.types = NEW.LINE.TYPE, linetype.by.outcome = T, linetype.by.group = F,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                             colors = COLORS,
                             locations='14460') + THEME
    dev.off()
    
    
    
    #chicago
    png(file.path(IMAGE.DIR, 'timelines/chicago_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=ALL.COVID.SCENARIOS, outcomes='incidence', line.size = LINE.SIZE,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                             colors = COLORS,
                             locations='16980') + THEME
    dev.off()
    
    #chicago - new
    png(file.path(IMAGE.DIR, 'timelines/chicago_new.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=ALL.COVID.SCENARIOS, outcomes='new', line.size = LINE.SIZE,
                             line.types = NEW.LINE.TYPE, linetype.by.outcome = T, linetype.by.group = F,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                             colors = COLORS,
                             locations='16980') + THEME
    
    dev.off()
   
    #sd
    png(file.path(IMAGE.DIR, 'timelines/sd_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=ALL.COVID.SCENARIOS, outcomes='incidence', line.size = LINE.SIZE,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                             colors = COLORS,
                             locations='41740') + THEME
    dev.off()
    
    #sd - new
    png(file.path(IMAGE.DIR, 'timelines/sd_new.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios=ALL.COVID.SCENARIOS, outcomes='new', line.size = LINE.SIZE,
                             line.types = NEW.LINE.TYPE, linetype.by.outcome = T, linetype.by.group = F,
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA,
                             colors = COLORS,
                             locations='41740') + THEME
    
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

