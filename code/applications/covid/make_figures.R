
source('code/applications/covid/covid_plots.R')

library(colorspace)

##-- SET-UP --##
<<<<<<< HEAD
<<<<<<< HEAD
=======
IMAGE.DIR = '../Manuscripts/covid_manuscript/CID submission/CID Revision/images'
>>>>>>> cd653bf0c14570643f9d595cd2a7ccacae9b37f3
=======
IMAGE.DIR = '../Manuscripts/covid_manuscript/CID submission/CID Revision/images'
>>>>>>> 42461671cc2ceca64462160b88a8ed1b6fe3f42f
IMAGE.DIR = '../CROI/CROI 2022 JHEEM-COVID'
library(ggsci)
PALETTE = pal_jama()
COLORS = PALETTE(6)[-5]#[c(1,2,5,3,4)]
#COLORS = PALETTE(7)[-c(5,6)]#[c(1,2,5,3,4)]
names(COLORS) = c('baseline',
                  'covid.rapid.resumption.mobility',
                  'covid.delayed.mobility',
                  'rebound.sexual.transmission',
                  'rebound.sex.delayed.hiv.care')

DARK.COLORS = darken(darken(COLORS))
names(DARK.COLORS) = names(COLORS)

PNG.POINT.SIZE = 5
RES = 600

LINE.SIZE = .75

THEME = theme(text = element_text(size=8), legend.direction = 'horizontal',
              legend.position = 'none')
THEME.WITH.LEGEND = theme(text = element_text(size=8), legend.direction = 'horizontal',
              legend.position = 'bottom')
CUM.INC.YEARS = 2020:2025

##-- SOURCE CODE --##
if (1==2)
{
    load('results/covid/covid_4.2_results.Rdata')
    
}

DELAYED = 'covid.delayed.mobility'
RAPID = 'covid.rapid.resumption.mobility'
ALL.COVID.SCENARIOS = c(RAPID,DELAYED)
YEARS.OF.INTEREST = 2019:2025

INCIDENCE.LINE.TYPE = 'solid'
NEW.LINE.TYPE = 'dashed'
TIMELINE.RIBBON.ALPHA = 0.05


# Cities
BOSTON = '14460'
CHICAGO = '16980'
SAN.DIEGO = '41740'

# Dimensions

PANEL.WIDTH = 2.5
PANEL.HEIGHT = 1.75

TIMELINE.WIDTH = 2.75
TIMELINE.HEIGHT = 1.6

##-- Fig 1 - New vs Inc --##

if (1==2)
{
    #-- Total --#
    png(file.path(IMAGE.DIR, 'inc_vs_reported/incvnew_rapid_total.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='covid.rapid.resumption.mobility',
                             outcomes = c('incidence','new'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline','covid.rapid.resumption.mobility')], 
                             ylim = c(12000,27000)) +  
        ylab('Incident or Reported\nCases (n)') + THEME
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported/incvnew_delayed_total.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='covid.delayed.mobility',
                             outcomes = c('incidence','new'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline','covid.delayed.mobility')], 
                             ylim = c(12000,27000)) + 
        ylab('Incident or Reported\nCases (n)') + THEME
    dev.off()
    
    
    #-- Boston --#
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported/incvnew_rapid_boston.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(locations = BOSTON,
                             scenarios=RAPID,
                             outcomes = c('incidence','new'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline',RAPID)],
                             ylim=c(200,1000)) + 
        ylab('Incident or Reported\nCases (n)') + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported/incvnew_delayed_boston.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(location=BOSTON,
                             scenarios=DELAYED,
                             outcomes = c('incidence','new'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline',DELAYED)],
                             ylim=c(200,1000)) + 
        ylab('Incident or Reported\nCases (n)') + THEME
    dev.off()
    
    
    #-- San Diego --#
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported/incvnew_rapid_sd.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(locations = SAN.DIEGO,
                             scenarios=RAPID,
                             outcomes = c('incidence','new'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline',RAPID)],
                             ylim=c(125,450)) + 
        ylab('Incident or Reported\nCases (n)') + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported/incvnew_delayed_sd.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(location=SAN.DIEGO,
                             scenarios=DELAYED,
                             outcomes = c('incidence','new'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline',DELAYED)],
                             ylim=c(125, 450)) + 
        ylab('Incident or Reported\nCases (n)') + THEME
    dev.off()
    
    
    #-- Chicago --#
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported/incvnew_rapid_chicago.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(locations = CHICAGO,
                             scenarios=RAPID,
                             outcomes = c('incidence','new'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline',RAPID)]) + 
        ylab('Incident or Reported\nCases (n)') + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported/incvnew_delayed_chicago.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(location=CHICAGO,
                             scenarios=DELAYED,
                             outcomes = c('incidence','new'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline',DELAYED)]) + 
        ylab('Incident or Reported\nCases (n)') + THEME
    dev.off()
    
    # for label
    png(file.path(IMAGE.DIR, 'inc_vs_reported/for_legend.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    ggplot() + 
        geom_hline(yintercept = 2:7,
                   size=0.5,
                   linetype=c('solid','dashed','solid','dashed','solid','dashed'),
                   color=rep(COLORS[c('baseline','base','delayed.hiv.care')], each=2)) +
        xlim(0,10) + ylim(0,8) +
        theme(panel.background = element_blank(), 
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    dev.off()
}


##-- Timeline Plots by Category of Parameters --##

if (1==2)
{
    high.suppression.mask = parameters[,'suppression.reduction'] < 0.2
    low.suppression.mask = parameters[,'suppression.reduction'] >= 0.2
    
    high.transmission.mask = parameters[,'sexual.transmission.reduction'] < 0.2
    low.transmission.mask = parameters[,'sexual.transmission.reduction'] > 0.3
    
    # need to run the block of code above
    high.s.low.t = high.suppression.mask & low.transmission.mask
    low.s.high.t = low.suppression.mask & high.transmission.mask
    
    results.high.s.low.t = outcomes.arr[,,,,high.s.low.t]
    results.low.s.high.t = outcomes.arr[,,,,low.s.high.t]
    
    
    #-- Total --#
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported_split/incvnew_delayed_total_good.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(results = results.high.s.low.t,
                             scenarios='covid.delayed.mobility',
                             outcomes = c('incidence','new'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline','covid.delayed.mobility')], 
                             ylim = c(12000,27000)) + 
        ylab('Incident or Reported\nCases (n)') + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported_split/incvnew_delayed_total_bad.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(results = results.low.s.high.t,
                             scenarios='covid.delayed.mobility',
                             outcomes = c('incidence','new'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline','covid.delayed.mobility')], 
                             ylim = c(12000,27000)) + 
        ylab('Incident or Reported\nCases (n)') + THEME
    dev.off()
    
    #-- Boston --#
    png(file.path(IMAGE.DIR, 'inc_vs_reported_split/incvnew_delayed_boston_good.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(results = results.high.s.low.t,
                             locations = BOSTON,
                             scenarios='covid.delayed.mobility',
                             outcomes = c('incidence','new'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline','covid.delayed.mobility')], 
                             ylim = c(250,1050)) + 
        ylab('Incident or Reported\nCases (n)') + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported_split/incvnew_delayed_boston_bad.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(results = results.low.s.high.t,
                             locations = BOSTON,
                             scenarios='covid.delayed.mobility',
                             outcomes = c('incidence','new'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline','covid.delayed.mobility')], 
                             ylim = c(250,1050)) + 
        ylab('Incident or Reported\nCases (n)') + THEME
    dev.off()
    
    #-- Chicago --#
    png(file.path(IMAGE.DIR, 'inc_vs_reported_split/incvnew_delayed_chicago_good.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(results = results.high.s.low.t,
                             locations = CHICAGO,
                             scenarios='covid.delayed.mobility',
                             outcomes = c('incidence','new'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline','covid.delayed.mobility')], 
                             ylim = c(600,1600)) + 
        ylab('Incident or Reported\nCases (n)') + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported_split/incvnew_delayed_chicago_bad.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(results = results.low.s.high.t,
                             locations = CHICAGO,
                             scenarios='covid.delayed.mobility',
                             outcomes = c('incidence','new'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline','covid.delayed.mobility')], 
                             ylim = c(600,1600)) + 
        ylab('Incident or Reported\nCases (n)') + THEME
    dev.off()
    
    #-- San Diego --#
    png(file.path(IMAGE.DIR, 'inc_vs_reported_split/incvnew_delayed_sd_good.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(results = results.high.s.low.t,
                             locations = SAN.DIEGO,
                             scenarios='covid.delayed.mobility',
                             outcomes = c('incidence','new'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline','covid.delayed.mobility')], 
                             ylim = c(125, 450)) + 
        ylab('Incident or Reported\nCases (n)') + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'inc_vs_reported_split/incvnew_delayed_sd_bad.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(results = results.low.s.high.t,
                             locations = SAN.DIEGO,
                             scenarios='covid.delayed.mobility',
                             outcomes = c('incidence','new'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline','covid.delayed.mobility')], 
                             ylim = c(125, 450)) + 
        ylab('Incident or Reported\nCases (n)') + THEME
    dev.off()
}

# proportion aware
if (1==2)
{
    #-- Total --#
    png(file.path(IMAGE.DIR, 'inc_vs_reported/incvnew_base_total.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='base',
                             outcomes = 'diagnosed',
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline','base')]) + 
        ylab('Knowledge of Status (%)') + THEME
    dev.off()
}

##-- BOX-PLOT --##

LOCATION.BOXPLOT.PANEL.HEIGHT = 7
LOCATION.BOXPLOT.PANEL.WIDTH = 6
BOXPLOT.THEME = THEME + theme(legend.position = 'bottom')

if (1==2)
{
    png(file.path(IMAGE.DIR, 'boxplots/location_estimates_base_delayed.png'), pointsize=PNG.POINT.SIZE, 
        width=LOCATION.BOXPLOT.PANEL.WIDTH, height=LOCATION.BOXPLOT.PANEL.HEIGHT, res=RES, units='in')
    make.location.boxplot(colors=COLORS[-1],
                        scenarios = ALL.COVID.SCENARIOS,
                        outcome.axis.name = "Change in Cumulative Incident HIV Infections,\n2020-2025, due to the COVID-19 Pandemic") +
        BOXPLOT.THEME + 
        xlab(NULL)
    dev.off()
}


##-- HEAT MAPS --##

HEAT.MAP.RANGE = c(-.2,.2)#c(-20000, 20000)
HEAT.MAP.PANEL.HEIGHT = 1.6
HEAT.MAP.PANEL.WIDTH = 2.25
HEAT.MAP.THEME = theme(text = element_text(size=6.5), legend.direction = 'horizontal',
                      legend.position = 'none')

do.make.covid.heat.map <- function(loc=NULL,
                                   scenario,
                                   subtract.relative=T,
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
        xlab("Reduction in Sexual Transmission") + ylab("Reduction in Suppression") + HEAT.MAP.THEME
}

if (1==2)
{
    # For the legend
    png(file.path(IMAGE.DIR, 'heat_maps/raw/raw_for_legend.png'), 
        pointsize=PNG.POINT.SIZE, width=HEAT.MAP.PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    make.covid.heat.map(var1 = 'sexual.transmission.reduction',
                        var2 = 'suppression.reduction',
                        min.change = HEAT.MAP.RANGE[1],
                        max.change = HEAT.MAP.RANGE[2],
                        scenario=RAPID,
                        color.scale.title = " ", #"Change in Cumulative\nIncidence 2020-2025",
                        subtract.relative = F) + 
        xlab("Reduction in Sexual Transmission") + ylab("Reduction in Viral Suppression") +
        THEME + theme(legend.position = 'bottom')
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'heat_maps/raw/total_rapid_sex_suppression_raw.png'), 
        pointsize=PNG.POINT.SIZE, width=HEAT.MAP.PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map(NULL, scenario=RAPID)
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/raw/total_delayed_sex_suppression_raw.png'), 
        pointsize=PNG.POINT.SIZE, width=HEAT.MAP.PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map(NULL, scenario=DELAYED)
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/raw/chicago_rapid_sex_suppression_raw.png'), 
        pointsize=PNG.POINT.SIZE, width=HEAT.MAP.PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('16980', scenario=RAPID)
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/raw/chicago_delayed_sex_suppression_raw.png'), 
        pointsize=PNG.POINT.SIZE, width=HEAT.MAP.PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('16980', scenario=DELAYED)
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/raw/boston_rapid_sex_suppression_raw.png'), 
        pointsize=PNG.POINT.SIZE, width=HEAT.MAP.PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('14460', scenario=RAPID)
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/raw/boston_delayed_sex_suppression_raw.png'), 
        pointsize=PNG.POINT.SIZE, width=HEAT.MAP.PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('14460', scenario=DELAYED)
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/raw/sd_rapid_sex_suppression_raw.png'), 
        pointsize=PNG.POINT.SIZE, width=HEAT.MAP.PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('41740', scenario=RAPID)
    dev.off()
    
    png(file.path(IMAGE.DIR, 'heat_maps/raw/sd_delayed_sex_suppression_raw.png'), 
        pointsize=PNG.POINT.SIZE, width=HEAT.MAP.PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('41740', scenario=DELAYED)
    dev.off()
    
}


##--        THE ORIGINAL        --##
##-- CITY-SPECIFIC CORRELATIONS --##
CITY.COR.SIZE.RANGE = c(1,3)
CITY.COR.THEME = theme(text = element_text(size=7),
                       axis.title = element_text(size=7),
                       legend.position = 'none')
CITY.COR.PANEL.HEIGHT = 2.5
CITY.COR.PANEL.WIDTH = 3
if (1==2)
{
    TO.LABEL = c('14460','41740','16980')
    TO.LABEL = character()
    
    png(file.path(IMAGE.DIR, 'msa_scatter/base_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=CITY.COR.PANEL.WIDTH, height=CITY.COR.PANEL.HEIGHT, res=RES, units='in')
    make.correlation.scatterplot(var1='suppression', var1.year=2019, correlate.var1 = F,
                                 var2='suppression.reduction', 
                                 scenario=RAPID,
                                 point.fill = COLORS[RAPID],
                                 outcome = 'incidence', outcome.years = CUM.INC.YEARS,
                                 label.locations = TO.LABEL,
                                 ylim=c(.15,.7),
                                 point.size.range = CITY.COR.SIZE.RANGE) + 
        ylab("Correlation Between Reduction in Viral\nSuppression and Incidence 2020-2025") +
        xlab("Mean Proportion Suppressed in 2019") + CITY.COR.THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'msa_scatter/base_transmission_raw.png'), pointsize=PNG.POINT.SIZE, width=CITY.COR.PANEL.WIDTH, height=CITY.COR.PANEL.HEIGHT, res=RES, units='in')
    make.correlation.scatterplot(var1='incidence.prevalence.ratio', var1.year=2019, correlate.var1 = F,
                                 var2='sexual.transmission.reduction', 
                                 locations = setdiff(names(location.names), c('17140','14460')), #minus cincinatti
                                 scenario=RAPID,
                                 point.fill = COLORS[RAPID],
                                 outcome = 'incidence', outcome.years = CUM.INC.YEARS,
                                 point.size.range = CITY.COR.SIZE.RANGE,
                                 ylim=c(-.95,-.65),
                                 label.locations = TO.LABEL) +
        ylab("Correlation Between Reduction in Viral\nSexual Transmission and Incidence 2020-2025") +
        xlab("Incidence/Prevalence Ratio in 2019") + CITY.COR.THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'msa_scatter/delayed_suppression_raw.png'), pointsize=PNG.POINT.SIZE, width=CITY.COR.PANEL.WIDTH, height=CITY.COR.PANEL.HEIGHT, res=RES, units='in')
    make.correlation.scatterplot(var1='suppression', var1.year=2019, correlate.var1 = F,
                                 var2='suppression.reduction', 
                                 scenario=DELAYED,
                                 point.fill = COLORS[DELAYED],
                                 outcome = 'incidence', outcome.years = CUM.INC.YEARS,
                                 point.size.range = CITY.COR.SIZE.RANGE,
                                 ylim=c(.15,0.7),
                                 label.locations = TO.LABEL) + #xlim(0,1) + ylim(0,1) +
        ylab("Correlation Between Reduction in Viral\nSuppression and Incidence 2020-2025") +
        xlab("Mean Proportion Suppressed in 2019") + CITY.COR.THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'msa_scatter/delayed_transmission_raw.png'), pointsize=PNG.POINT.SIZE, width=CITY.COR.PANEL.WIDTH, height=CITY.COR.PANEL.HEIGHT, res=RES, units='in')
    make.correlation.scatterplot(var1='incidence.prevalence.ratio', var1.year=2019, correlate.var1 = F,
                                 var2='sexual.transmission.reduction', 
                                 locations = setdiff(names(location.names), c('17140','14460')), #minus cincinatti
                                 scenario=DELAYED,
                                 point.fill = COLORS[DELAYED],
                                 ylim=c(-.95,-.65),
                                 outcome = 'incidence', outcome.years = CUM.INC.YEARS,
                                 point.size.range = CITY.COR.SIZE.RANGE,
                                 label.locations = TO.LABEL) + #xlim(0,1) + ylim(0,1) +
        ylab("Correlation Between Reduction in Viral\nSexual Transmission and Incidence 2020-2025") +
        xlab("Incidence/Prevalence Ratio in 2019") + CITY.COR.THEME
    dev.off()
    
    #not using
    png(file.path(IMAGE.DIR, 'msa_scatter/base_testing_raw.png'), pointsize=PNG.POINT.SIZE, width=CITY.COR.PANEL.WIDTH, height=CITY.COR.PANEL.HEIGHT, res=RES, units='in')
    make.correlation.scatterplot(var1='testing', var1.year=2019, correlate.var1 = F,
                                 var2='testing.reduction', 
                                 scenario='base',
                                 point.fill = COLORS['base'],
                                 outcome = 'incidence', outcome.years = CUM.INC.YEARS,
                                 point.size.range = CITY.COR.SIZE.RANGE,
                                 label.locations = character()) + #xlim(0,1) + ylim(0,1) +
     #   ylim(-.75,-.35) +
        ylab("Correlation Between Reduction in Testing\nRate and Incidence 2020-2025") +
        xlab("Mean Testing Rate in 2019") + CITY.COR.THEME
    dev.off()
}



##-- BINNED BOX-PLOTS for VARIABLES --##
BINNED.BOXPLOT.THEME = theme(text = element_text(size=7),
                      axis.title = element_text(size=7),
                      legend.position = 'none')
BINNED.BOXPLOT.WIDTH = 2.25
BINNED.BOXPLOT.HEIGHT = 1.5
BINNED.BOXPLOT.LIMITS = c(-.25,.25)

if (1==2)
{
    # base
    png(file.path(IMAGE.DIR, 'binned_boxplots/base_transmission.png'), 
        pointsize=PNG.POINT.SIZE, width=BINNED.BOXPLOT.WIDTH, height=BINNED.BOXPLOT.HEIGHT, res=RES, units='in')
    make.covid.binned.boxplot(var1='sexual.transmission.reduction', 
                              scenario1=RAPID,
                              boxplot.fill=COLORS[RAPID],
                              ylim=BINNED.BOXPLOT.LIMITS,
                              label.rho.size = SCATTER.FONT.SIZE,
                              label.rho.hjust = 'left',
                              label.rho.vjust = 'top',aggregate.locations = F) + 
        ylab("Change in Incidence,\n2020-2025") +
        xlab("Reduction in Viral Suppression") +
        BINNED.BOXPLOT.THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'binned_boxplots/base_suppression.png'), 
        pointsize=PNG.POINT.SIZE, width=BINNED.BOXPLOT.WIDTH, height=BINNED.BOXPLOT.HEIGHT, res=RES, units='in')
    make.covid.binned.boxplot(var1='suppression.reduction', 
                              scenario1=RAPID,
                              boxplot.fill=COLORS[RAPID],
                              ylim=BINNED.BOXPLOT.LIMITS,
                              label.rho.size = SCATTER.FONT.SIZE,
                              label.rho.hjust = 'left',
                              label.rho.vjust = 'top',aggregate.locations = F) + 
        ylab("Change in Incidence,\n2020-2025") +
        xlab("Reduction in Viral Suppression") +
        BINNED.BOXPLOT.THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'binned_boxplots/base_testing.png'), 
        pointsize=PNG.POINT.SIZE, width=BINNED.BOXPLOT.WIDTH, height=BINNED.BOXPLOT.HEIGHT, res=RES, units='in')
    make.covid.binned.boxplot(var1='testing.reduction', 
                              scenario1=RAPID,
                              boxplot.fill=COLORS[RAPID],
                              ylim=BINNED.BOXPLOT.LIMITS,
                              label.rho.size = SCATTER.FONT.SIZE,
                              label.rho.hjust = 'left',
                              label.rho.vjust = 'top',aggregate.locations = F) + 
        ylab("Change in Incidence,\n2020-2025") +
        xlab("Reduction in Viral Suppression") +
        BINNED.BOXPLOT.THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'binned_boxplots/base_prep.png'), 
        pointsize=PNG.POINT.SIZE, width=BINNED.BOXPLOT.WIDTH, height=BINNED.BOXPLOT.HEIGHT, res=RES, units='in')
    make.covid.binned.boxplot(var1='prep.reduction', 
                              scenario1=RAPID,
                              boxplot.fill=COLORS[RAPID],
                              ylim=BINNED.BOXPLOT.LIMITS,
                              label.rho.size = SCATTER.FONT.SIZE,
                              label.rho.hjust = 'left',
                              label.rho.vjust = 'top',aggregate.locations = F) + 
        ylab("Change in Incidence,\n2020-2025") +
        xlab("Reduction in Viral Suppression") +
        BINNED.BOXPLOT.THEME
    dev.off()
    
    # delayed
    png(file.path(IMAGE.DIR, 'binned_boxplots/delayed_transmission.png'), 
        pointsize=PNG.POINT.SIZE, width=BINNED.BOXPLOT.WIDTH, height=BINNED.BOXPLOT.HEIGHT, res=RES, units='in')
    make.covid.binned.boxplot(var1='sexual.transmission.reduction', 
                              scenario1=DELAYED,
                              boxplot.fill=COLORS[DELAYED],
                              ylim=BINNED.BOXPLOT.LIMITS,
                              label.rho.size = SCATTER.FONT.SIZE,
                              label.rho.hjust = 'left',
                              label.rho.vjust = 'top',aggregate.locations = F) + 
        ylab("Change in Incidence,\n2020-2025") +
        xlab("Reduction in Viral Suppression") +
        BINNED.BOXPLOT.THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'binned_boxplots/delayed_suppression.png'), 
        pointsize=PNG.POINT.SIZE, width=BINNED.BOXPLOT.WIDTH, height=BINNED.BOXPLOT.HEIGHT, res=RES, units='in')
    make.covid.binned.boxplot(var1='suppression.reduction', 
                              scenario1=DELAYED,
                              boxplot.fill=COLORS[DELAYED],
                              ylim=BINNED.BOXPLOT.LIMITS,
                              label.rho.size = SCATTER.FONT.SIZE,
                              label.rho.hjust = 'left',
                              label.rho.vjust = 'top',aggregate.locations = F) + 
        ylab("Change in Incidence,\n2020-2025") +
        xlab("Reduction in Viral Suppression") +
        BINNED.BOXPLOT.THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'binned_boxplots/delayed_testing.png'), 
        pointsize=PNG.POINT.SIZE, width=BINNED.BOXPLOT.WIDTH, height=BINNED.BOXPLOT.HEIGHT, res=RES, units='in')
    make.covid.binned.boxplot(var1='testing.reduction', 
                              scenario1=DELAYED,
                              boxplot.fill=COLORS[DELAYED],
                              ylim=BINNED.BOXPLOT.LIMITS,
                              label.rho.size = SCATTER.FONT.SIZE,
                              label.rho.hjust = 'left',
                              label.rho.vjust = 'top',aggregate.locations = F) + 
        ylab("Change in Incidence,\n2020-2025") +
        xlab("Reduction in Viral Suppression") +
        BINNED.BOXPLOT.THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'binned_boxplots/delayed_prep.png'), 
        pointsize=PNG.POINT.SIZE, width=BINNED.BOXPLOT.WIDTH, height=BINNED.BOXPLOT.HEIGHT, res=RES, units='in')
    make.covid.binned.boxplot(var1='prep.reduction', 
                              scenario1=DELAYED,
                              boxplot.fill=COLORS[DELAYED],
                              ylim=BINNED.BOXPLOT.LIMITS,
                              label.rho.size = SCATTER.FONT.SIZE,
                              label.rho.hjust = 'left',
                              label.rho.vjust = 'top',aggregate.locations = F) + 
        ylab("Change in Incidence,\n2020-2025") +
        xlab("Reduction in Viral Suppression") +
        BINNED.BOXPLOT.THEME
    dev.off()
    
    
    
    
}


##-- SCATTER-PLOTS for VARIABLES --##
SCATTER.POINT.SIZE = 0.75
SCATTER.FONT.SIZE = 2
SCATTER.ALPHA = 0.8
SCATTER.THEME = theme(text = element_text(size=7),
                      axis.title = element_text(size=7),
                      legend.position = 'none')
if (1==2)
{
    png(file.path(IMAGE.DIR, 'scatter/base_suppression.png'), 
        pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(var1='suppression.reduction',
                           scenario1=RAPID,
                           point.fill=COLORS[RAPID],
                           point.size=SCATTER.POINT.SIZE,
                           point.alpha=SCATTER.ALPHA,
                           label.rho.size = SCATTER.FONT.SIZE,
                           label.rho.hjust = 'left',
                           label.rho.vjust = 'top') + 
        ylab("Change in 2020-2025 Incidence,\nBase vs. No COVID") +
        xlab("Reduction in Viral Suppression") +
        SCATTER.THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'scatter/base_transmission.png'), 
        pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(var1='sexual.transmission.reduction',
                           scenario1='base',
                           point.fill=COLORS['base'],
                           point.size=SCATTER.POINT.SIZE,
                           label.rho.size = SCATTER.FONT.SIZE,
                           point.alpha=SCATTER.ALPHA,
                           label.rho.hjust = 'right',
                           label.rho.vjust = 'top') + 
        ylab("Change in 2020-2025 Incidence,\nBase vs. No COVID") +
        xlab("Reduction in Sexual Transmission") +
        SCATTER.THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'scatter/base_testing.png'), 
        pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(var1='testing.reduction',
                           scenario1='base',
                           point.fill=COLORS['base'],
                           point.size=SCATTER.POINT.SIZE,
                           point.alpha=SCATTER.ALPHA,
                           label.rho.size = SCATTER.FONT.SIZE,
                           label.rho.hjust = 'right',
                           label.rho.vjust = 'bottom') + 
        ylab("Change in 2020-2025 Incidence,\nBase vs. No COVID") +
        xlab("Reduction in HIV Testing Rate") +
        SCATTER.THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'scatter/base_prep.png'), 
        pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(var1='prep.reduction',
                           scenario1='base',
                           point.fill=COLORS['base'],
                           point.size=SCATTER.POINT.SIZE,
                           point.alpha=SCATTER.ALPHA,
                           label.rho.size = SCATTER.FONT.SIZE,
                           label.rho.hjust = 'left',
                           label.rho.vjust = 'top') + 
        ylab("Change in 2020-2025 Incidence,\nBase vs. No COVID") +
        xlab("Reduction in PrEP Coverage") +
        SCATTER.THEME
    dev.off()
    
    
    # Delayed
    png(file.path(IMAGE.DIR, 'scatter/delayed_suppression.png'), 
        pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(var1='suppression.reduction',
                           scenario1='delayed.hiv.care',
                           point.fill=COLORS['delayed.hiv.care'],
                           point.size=SCATTER.POINT.SIZE,
                           point.alpha=SCATTER.ALPHA,
                           label.rho.size = SCATTER.FONT.SIZE,
                           label.rho.hjust = 'left',
                           label.rho.vjust = 'top') + 
        ylab("Change in 2020-2025 Incidence,\nDelayed Healthcare vs. No COVID") +
        xlab("Reduction in Viral Suppression") +
        SCATTER.THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'scatter/delayed_transmission.png'), 
        pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(var1='sexual.transmission.reduction',
                           scenario1='delayed.hiv.care',
                           point.fill=COLORS['delayed.hiv.care'],
                           point.size=SCATTER.POINT.SIZE,
                           label.rho.size = SCATTER.FONT.SIZE,
                           point.alpha=SCATTER.ALPHA,
                           label.rho.hjust = 'right',
                           label.rho.vjust = 'top') + 
        ylab("Change in 2020-2025 Incidence,\nDelayed Healthcare vs. No COVID") +
        xlab("Reduction in Sexual Transmission") +
        SCATTER.THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'scatter/delayed_testing.png'), 
        pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(var1='testing.reduction',
                           scenario1='delayed.hiv.care',
                           point.fill=COLORS['delayed.hiv.care'],
                           point.size=SCATTER.POINT.SIZE,
                           point.alpha=SCATTER.ALPHA,
                           label.rho.size = SCATTER.FONT.SIZE,
                           label.rho.hjust = 'right',
                           label.rho.vjust = 'bottom') + 
        ylab("Change in 2020-2025 Incidence,\nDelayed Healthcare vs. No COVID") +
        xlab("Reduction in HIV Testing Rate") +
        SCATTER.THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'scatter/delayed_prep.png'), 
        pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(var1='prep.reduction',
                           scenario1='delayed.hiv.care',
                           point.fill=COLORS['delayed.hiv.care'],
                           point.size=SCATTER.POINT.SIZE,
                           point.alpha=SCATTER.ALPHA,
                           label.rho.size = SCATTER.FONT.SIZE,
                           label.rho.hjust = 'left',
                           label.rho.vjust = 'top') + 
        ylab("Change in 2020-2025 Incidence,\nDelayed Healthcare vs. No COVID") +
        xlab("Reduction in PrEP Coverage") +
        SCATTER.THEME
    dev.off()
}

if (1==2)
{
    png(file.path(IMAGE.DIR, 'scatter/base_suppression.png'), 
        pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(var1='sexual.transmission.reduction', 
                           var2 = 'diagnosed', var2.years = 2025, subtract.scenario2 = NA,
                           scenario1=RAPID,
                           point.fill=COLORS[RAPID],
                           point.size=SCATTER.POINT.SIZE,
                           point.alpha=SCATTER.ALPHA,
                           label.rho.size = SCATTER.FONT.SIZE,
                           label.rho.hjust = 'left',
                           label.rho.vjust = 'top') + 
        ylab("Knowledge of Status (%), 2025") +
        xlab("Reduction in HIV Testing") +
        SCATTER.THEME
    dev.off()
}

