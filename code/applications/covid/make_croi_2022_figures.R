
IMAGE.DIR = '../CROI/CROI 2022 JHEEM-COVID/poster'


source('code/applications/covid/covid_plots.R')

library(colorspace)

##-- SET-UP --##
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

LINE.SIZE = 3

THEME = theme(text = element_text(size=24), legend.direction = 'horizontal',
              legend.position = 'none')
THEME.WITH.LEGEND = theme(text = element_text(size=24), legend.direction = 'horizontal',
                          legend.position = 'bottom')
CUM.INC.YEARS = 2020:2025

##-- SOURCE CODE --##
if (1==2)
{
    load('results/covid/covid_4.2_results.Rdata')
    
}

ALL.COVID.SCENARIOS = c('base','delayed.hiv.care','rebound.sex.delayed.hiv.care')
YEARS.OF.INTEREST = 2019:2025

INCIDENCE.LINE.TYPE = 'solid'
NEW.LINE.TYPE = 'dashed'
TIMELINE.RIBBON.ALPHA = 0.2


# Cities
BOSTON = '14460'
CHICAGO = '16980'
SAN.DIEGO = '41740'

# Dimensions

PANEL.WIDTH = 2.5
PANEL.HEIGHT = 1.75

TIMELINE.WIDTH = 7.4
TIMELINE.HEIGHT = 3.2

##-- BOXPLOT --##
LOCATION.BOXPLOT.PANEL.HEIGHT = 4
LOCATION.BOXPLOT.PANEL.WIDTH = 15
BOXPLOT.THEME = THEME + theme(legend.position = 'none',
                              text = element_text(size=24),
                              axis.text.x = element_text(angle = 45, hjust=1))#, vjust = 0.5, hjust=1))

source('code/calibration/target_msas.R')
if (1==2)
{
    png(file.path(IMAGE.DIR, 'location_boxplots.png'), pointsize=PNG.POINT.SIZE, 
        width=LOCATION.BOXPLOT.PANEL.WIDTH, height=LOCATION.BOXPLOT.PANEL.HEIGHT, res=RES, units='in')
    make.location.boxplot(colors=COLORS[3],
                          scenarios = 'covid.delayed.mobility',
                          loc.names = MSA.BRIEF.NAMES,
                          include.total = F,
                          outcome.axis.name = 'Change in Cumulative\nHIV Incidence 2020-25,\nRelative to No-COVID',
                          ylim = c(-.1,.2),
                          vertical = F, interval.coverage = NA) + 
        BOXPLOT.THEME +
        xlab(NULL) 
    dev.off()
}


##-- INC --##
if (1==2)
{
    png(file.path(IMAGE.DIR, 'inc.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='covid.delayed.mobility',
                             outcomes = c('incidence'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline','covid.delayed.mobility')], 
                             ylim = c(10000,30000)) + #c(12000,27000)) + 
        ylab('Projected Incident\nInfections') + xlab(NULL) + THEME
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'new.png'), 
        pointsize=PNG.POINT.SIZE, width=TIMELINE.WIDTH, height=TIMELINE.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='covid.delayed.mobility',
                             outcomes = c('new'),
                             line.size = LINE.SIZE,
                             linetype.by.outcome=T,
                             line.types=c(INCIDENCE.LINE.TYPE,NEW.LINE.TYPE),
                             ribbon.alpha = TIMELINE.RIBBON.ALPHA, show.baseline.ribbon = F,
                             show.ribbon.outline = T,
                             include.baseline = T,
                             colors=COLORS[c('baseline','covid.delayed.mobility')], 
                             ylim = c(10000,30000)) + #c(12000,27000)) + 
        ylab('Projected Reported\nDiagnoses') + xlab(NULL) + THEME
    dev.off()
}

##-- SCATTER PLOTS --##

SCATTER.SIZE.RANGE = c(1,3)
SCATTER.THEME = theme(text = element_text(size=24),
                      axis.title = element_text(size=20),
                      legend.position = 'none')
SCATTER.PANEL.HEIGHT = 3.2
SCATTER.PANEL.WIDTH = 7.4

if (1==2)
{

    big.supp.drop.mask = parameters[,'suppression.reduction'] > 0.25
    big.supp.drop.results = outcomes.arr[,,,,big.supp.drop.mask]
    dim.names = dimnames(outcomes.arr)
    dim.names$sim = dimnames(big.supp.drop.results)$sim
    dim(big.supp.drop.results) = sapply(dim.names, length)
    dimnames(big.supp.drop.results) = dim.names
    
    png(file.path(IMAGE.DIR, 'scatter.png'), 
        pointsize=PNG.POINT.SIZE, width=SCATTER.PANEL.WIDTH, height=SCATTER.PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(results = big.supp.drop.results,
                           scenario2 = 'covid.delayed.mobility',
                           var1='suppression', var1.years = 2019,
                           var2='incidence', var2.years = CUM.INC.YEARS,
                           subtract.scenario1 = NA,
                           subtract.scenario2 = 'baseline',
                           subtract.relative2 = T,
                           size.by = 'incidence',
                           size.by.years=2019,
                           point.size.range = c(2,10),
                           
                           aggregate.locations = F,
                           aggregate.simulations.fn = median,
                           add.smoother = F,
                           label.rho = F,
                           point.size = 3,
                           point.fill = COLORS['covid.delayed.mobility']) +
        ylab("Median Change in\nCumulative HIV\nIncidence 2020-25,\nRelative to No-COVID") +
        xlab("Median Proportion Suppressed in 2019") + SCATTER.THEME
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
