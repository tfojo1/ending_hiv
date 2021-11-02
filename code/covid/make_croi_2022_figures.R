
IMAGE.DIR = '../CROI/CROI 2022 JHEEM-COVID'


source('code/covid/covid_plots.R')

library(colorspace)

##-- SET-UP --##
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

PNG.POINT.SIZE = 5
RES = 600

LINE.SIZE = .75

TEXT.SIZE = 7

THEME = theme(text = element_text(size=TEXT.SIZE), legend.direction = 'horizontal',
              legend.position = 'none')
THEME.WITH.LEGEND = theme(text = element_text(size=TEXT.SIZE), legend.direction = 'horizontal',
                          legend.position = 'bottom')
CUM.INC.YEARS = 2020:2025

##-- SOURCE CODE --##
if (1==2)
{
    load('results/covid/covid_4.1_results.Rdata')
    
}

ALL.COVID.SCENARIOS = c('base','delayed.hiv.care','rebound.sex.delayed.hiv.care')
YEARS.OF.INTEREST = 2019:2025

INCIDENCE.LINE.TYPE = 'solid'
NEW.LINE.TYPE = 'dashed'
TIMELINE.RIBBON.ALPHA = 0.05


# Cities
BOSTON = '14460'
CHICAGO = '16980'
SAN.DIEGO = '41740'

# Dimensions

HEIGHT = 1.6
WIDTH = 4


TIMELINE.WIDTH = WIDTH/2-.1
TIMELINE.HEIGHT = HEIGHT

##-- BOXPLOT --##
LOCATION.BOXPLOT.PANEL.HEIGHT = HEIGHT + .1
LOCATION.BOXPLOT.PANEL.WIDTH = WIDTH
BOXPLOT.THEME = THEME + theme(legend.position = 'none',
                              axis.text.x = element_text(angle = 45, hjust=1))#, vjust = 0.5, hjust=1))

source('code/targets/target_msas.R')
if (1==2)
{
    png(file.path(IMAGE.DIR, 'location_boxplots.png'), pointsize=PNG.POINT.SIZE, 
        width=LOCATION.BOXPLOT.PANEL.WIDTH, height=LOCATION.BOXPLOT.PANEL.HEIGHT, res=RES, units='in')
    make.location.boxplot(colors=COLORS[3],
                          scenarios = 'delayed.hiv.care',
                          loc.names = MSA.BRIEF.NAMES,
                          include.total = F,
                          outcome.axis.name = 'Change in Cumulative\nIncidence 2020-2025,\nRelative to No-COVID',
                          vertical = F) + 
        BOXPLOT.THEME +
        xlab(NULL) 
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'location_boxplots_no_whiskers.png'), pointsize=PNG.POINT.SIZE, 
        width=LOCATION.BOXPLOT.PANEL.WIDTH, height=LOCATION.BOXPLOT.PANEL.HEIGHT, res=RES, units='in')
    make.location.boxplot(colors=COLORS[3],
                          scenarios = 'delayed.hiv.care',
                          loc.names = MSA.BRIEF.NAMES,
                          include.total = F,
                          interval.coverage=NA,
                          outcome.axis.name = 'Change in Cumulative\nIncidence 2020-2025,\nRelative to No-COVID',
                          vertical = F) + 
        BOXPLOT.THEME +
        xlab(NULL) 
    dev.off()
}


##-- INC --##
if (1==2)
{
    COVID.SCENARIO.NAMES['delayed.hiv.care'] = 'With Pandemic'
    COVID.SCENARIO.NAMES['baseline'] = 'Absent Pandemic'
    png(file.path(IMAGE.DIR, 'inc_delayed_total.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='delayed.hiv.care',
                             outcomes = c('incidence'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             linetype.by.group = F,
                       #      line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline','delayed.hiv.care')], 
                             ylim = c(10000,30000)) + #c(12000,27000)) + 
        ylab('Projected Incident\nHIV Cases') + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'inc_delayed_total with legend.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH+1, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='delayed.hiv.care',
                             outcomes = c('incidence'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             linetype.by.group = F,
                             #      line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline','delayed.hiv.care')], 
                             ylim = c(10000,30000)) + #c(12000,27000)) + 
        ylab('Projected Incident\nHIV Cases') + THEME.WITH.LEGEND
    dev.off()
}

##-- SCATTER PLOTS --##

SCATTER.SIZE.RANGE = c(1,3)
SCATTER.THEME = theme(text = element_text(size=TEXT.SIZE),
                      axis.title = element_text(size=7),
                      legend.position = 'none')
SCATTER.THEME = THEME
SCATTER.PANEL.HEIGHT = HEIGHT
SCATTER.PANEL.WIDTH = WIDTH/2-.1

if (1==2)
{

    big.supp.drop.mask = parameters[,'suppression.reduction'] > 0.25
    big.supp.drop.results = outcomes.arr[,,,,,big.supp.drop.mask]
    dim.names = dimnames(outcomes.arr)
    dim.names$sim = dimnames(big.supp.drop.results)$sim
    dim(big.supp.drop.results) = sapply(dim.names, length)
    dimnames(big.supp.drop.results) = dim.names
    
    png(file.path(IMAGE.DIR, 'supp_vs_inc_change_big_suppression_reduction.png'), 
        pointsize=PNG.POINT.SIZE, width=SCATTER.PANEL.WIDTH, height=SCATTER.PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(results = big.supp.drop.results,
                           scenario2 = 'delayed.hiv.care',
                           var1='suppression', var1.years = 2019,
                           var2='incidence', var2.years = CUM.INC.YEARS,
                           subtract.scenario1 = NA,
                           subtract.scenario2 = 'baseline',
                           subtract.relative2 = T,
                           size.by = 'incidence',
                           size.by.years=2019,
                           point.size.range = c(1,3),
                           
                           aggregate.locations = F,
                           aggregate.simulations.fn = median,
                           add.smoother = F,
                           label.rho = F,
                           point.size = 3,
                           point.fill = COLORS['delayed.hiv.care']) +
        ylab("Median Change in Cumulative\nHIV Incidence 2020-25") + #,\nRelative to No-COVID") +
        xlab("Median Proportion Virally Suppressed in 2019") + SCATTER.THEME
    dev.off()
    
    
    
    # For labels
    make.covid.scatterplot(results = big.supp.drop.results,
                           scenario2 = 'delayed.hiv.care',
                           var1='suppression', var1.years = 2019,
                           var2='incidence', var2.years = CUM.INC.YEARS,
                           subtract.scenario1 = NA,
                           subtract.scenario2 = 'baseline',
                           subtract.relative2 = T,
                           size.by = 'incidence',
                           size.by.years=2019,
                           point.size.range = 2*c(1,3),
                           
                           label.locations=T,
                           print.df=T,
                           
                           aggregate.locations = F,
                           aggregate.simulations.fn = median,
                           add.smoother = F,
                           label.rho = F,
                           point.size = 3,
                           point.fill = COLORS['delayed.hiv.care'])
}


##-- FOR TEXT --##
if (1==2)
{
    
    sort(calculate.delta.summary(big.supp.drop.results, relative=T)) * 100
    sort(calculate.delta.summary(outcomes.arr, relative=T)) * 100
    
    
    sort(calculate.delta.summary(big.supp.drop.results, relative=T, summary.stat='median')) * 100
    sort(calculate.delta.summary(outcomes.arr, relative=T, summary.stat='median')) * 100
    
    
    sc='delayed.hiv.care'
    int=1
    outcome='incidence'
    delta.2020.2019 = (outcomes.arr[,sc,int,'2020',outcome,] - outcomes.arr[,sc,int,'2019',outcome,])/outcomes.arr[,sc,int,'2019',outcome,]
    mean(delta.2020.2019<0)
    median(delta.2020.2019)
    
    delta.2021.2020 = (outcomes.arr[,sc,int,'2021',outcome,] - outcomes.arr[,sc,int,'2020',outcome,])/outcomes.arr[,sc,int,'2020',outcome,]
    delta.2022.2021 = (outcomes.arr[,sc,int,'2022',outcome,] - outcomes.arr[,sc,int,'2021',outcome,])/outcomes.arr[,sc,int,'2021',outcome,]
    
    mean(delta.2021.2020>0)
    median(delta.2021.2020)
    
    mean(delta.2022.2021>0)
    median(delta.2022.2021)
    
}

calculate.delta.summary <- function(results=outcomes.arr,
                                  relative=F,
                                  summary.stat=mean,
                                  outcome='incidence',
                                  scenario1='baseline',
                                  scenario2='delayed.hiv.care',
                                  years=CUM.INC.YEARS,
                                  intervention=1)
{
    v1 = results[,scenario1,intervention,as.character(years),outcome,]
    v2 = results[,scenario2,intervention,as.character(years),outcome,]
    
    v1 = apply(v1, setdiff(names(dimnames(v1)), 'year'), sum)
    v2 = apply(v2, setdiff(names(dimnames(v2)), 'year'), sum)
    
    if (relative)
        deltas = (v2-v1)/v1
    else
        deltas = v2-v1
    
    apply(deltas, 'location', summary.stat)
}
