RENDER.ALL = F


source('code/applications/covid/covid_plots.R')

library(colorspace)

##-- SET-UP --##
IMAGE.DIR = '../Manuscripts/covid_manuscript/CID submission/proofs'#'../CROI/CROI 2022 JHEEM-COVID'
library(ggsci)
PALETTE = pal_jama()
COLORS = PALETTE(6)[-5]#[c(1,2,5,3,4)]
#COLORS = PALETTE(7)[-c(5,6)]#[c(1,2,5,3,4)]
names(COLORS) = c('baseline',
                  'covid.rapid.resumption.mobility',
                  'covid.delayed.mobility',
                  'rebound.sexual.transmission',
                  'rebound.sex.delayed.hiv.care')

DELAYED = 'covid.delayed.mobility'
RAPID = 'covid.rapid.resumption.mobility'

#BLACK AND WHITE COLORS
COLORS[DELAYED] = 'gray65'
COLORS[RAPID] = 'gray40'

DARK.COLORS = darken(darken(COLORS))
names(DARK.COLORS) = names(COLORS)

PNG.POINT.SIZE = 5
RES = 1200

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


##-- Figure 2 --#
##-- Timeline Plots by Category of Parameters --##

if (RENDER.ALL)
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
    
    
    #-- For Legend --#
    tiff(file.path(IMAGE.DIR, 'figure_2/for_legend.tif'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    ggplot() + 
        geom_hline(yintercept = 2:7,
                   size=0.5,
                   linetype=c('solid','dashed','solid','dashed','solid','dashed'),
                   color=rep(COLORS[c('baseline',RAPID,DELAYED)], each=2)) +
        xlim(0,10) + ylim(0,8) +
        theme(panel.background = element_blank(), 
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    dev.off()
    
    #-- Total --#
    
    tiff(file.path(IMAGE.DIR, 'figure_2/figure_2a.tif'), 
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
    
    tiff(file.path(IMAGE.DIR, 'figure_2/figure_2b.tif'), 
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
    tiff(file.path(IMAGE.DIR, 'figure_2/figure_2e.tif'), 
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
    
    tiff(file.path(IMAGE.DIR, 'figure_2/figure_2f.tif'), 
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
    tiff(file.path(IMAGE.DIR, 'figure_2/figure_2g.tif'), 
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
    
    tiff(file.path(IMAGE.DIR, 'figure_2/figure_2h.tif'), 
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
    tiff(file.path(IMAGE.DIR, 'figure_2/figure_2c.tif'), 
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
    
    tiff(file.path(IMAGE.DIR, 'figure_2/figure_2d.tif'), 
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

##-- Figure 4 --##
##-- BOX-PLOT --##

LOCATION.BOXPLOT.PANEL.HEIGHT = 7
LOCATION.BOXPLOT.PANEL.WIDTH = 6
BOXPLOT.THEME = THEME + theme(legend.position = 'bottom')

if (RENDER.ALL)
{
    tiff(file.path(IMAGE.DIR, 'figure_4.tif'), pointsize=PNG.POINT.SIZE, 
        width=LOCATION.BOXPLOT.PANEL.WIDTH, height=LOCATION.BOXPLOT.PANEL.HEIGHT, res=RES, units='in')
    make.location.boxplot(colors=COLORS[-1],
                        scenarios = ALL.COVID.SCENARIOS,
                        outcome.axis.name = "Change in Cumulative Incident HIV Infections,\n2020-2025, due to the COVID-19 Pandemic") +
        BOXPLOT.THEME + 
        xlab(NULL)
    dev.off()
}

##-- Figure 3  --##
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

if (RENDER.ALL)
{
    # For the legend
    tiff(file.path(IMAGE.DIR, 'figure_3/raw_for_legend.png'), 
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
    

    tiff(file.path(IMAGE.DIR, 'figure_3/figure_3a.tif'), 
        pointsize=PNG.POINT.SIZE, width=HEAT.MAP.PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map(NULL, scenario=DELAYED)
    dev.off()
    
    tiff(file.path(IMAGE.DIR, 'figure_3/figure_3d.tif'), 
        pointsize=PNG.POINT.SIZE, width=HEAT.MAP.PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('16980', scenario=DELAYED)
    dev.off()
    
    tiff(file.path(IMAGE.DIR, 'figure_3/figure_3c.tif'), 
        pointsize=PNG.POINT.SIZE, width=HEAT.MAP.PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('14460', scenario=DELAYED)
    dev.off()
    
    tiff(file.path(IMAGE.DIR, 'figure_3/figure_3b.tif'), 
        pointsize=PNG.POINT.SIZE, width=HEAT.MAP.PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('41740', scenario=DELAYED)
    dev.off()
    
}


