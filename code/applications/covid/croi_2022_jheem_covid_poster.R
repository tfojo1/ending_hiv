
source('code/applications/covid/covid_plots.R')
source("../jheem_interactive/shiny/helpers/location_names.R")

library(colorspace)

##-- SET-UP --##
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




##-- BOX-PLOT --##

LOCATION.BOXPLOT.PANEL.HEIGHT = 7
LOCATION.BOXPLOT.PANEL.WIDTH = 6
BOXPLOT.THEME = THEME + theme(legend.position = 'bottom')

if (1==2)
{
    cum.inc.covid = apply(outcomes.arr[,'covid.delayed.mobility',as.character(YEARS.OF.INTEREST),'incidence',],c('location','sim'), sum)
    cum.inc.baseline = apply(outcomes.arr[,'baseline',as.character(YEARS.OF.INTEREST),'incidence',],c('location','sim'), sum)
    
    rel.diff = (cum.inc.covid-cum.inc.baseline)/cum.inc.baseline
    
    rel.diff.quantiles = apply(rel.diff, 'location', quantile, probs=c(0.025,0.25,.5,.75,.975))
    dimnames(rel.diff.quantiles)[[1]] = c('lower2','lower1','median','upper1','upper2')
   # dimnames(rel.diff.quantiles)[[2]] = MSA.SHORT.NAMES[dimnames(rel.diff.quantiles)[[2]]]
    
    df = as.data.frame(t(rel.diff.quantiles))
    df$location = as.character(MSA.SHORT.NAMES[dimnames(rel.diff.quantiles)[[2]]])
    df = df[order(df$median),]
    df$location = factor(df$location, levels=df$location)
        
    ggplot(df, aes(x=location, middle=median, lower=lower1, upper=upper1, ymin=lower2, ymax=upper2)) +
        geom_boxplot(stat='identity', position = position_dodge(width=.5)) +
        geom_hline(yintercept = 0, linetype='dashed') +
        scale_y_continuous(labels=percent, limits = c(-.4,.4)) +
        theme(panel.background = element_blank())
    
    png(file.path(IMAGE.DIR, 'boxplot.png'), pointsize=PNG.POINT.SIZE, 
        width=LOCATION.BOXPLOT.PANEL.WIDTH, height=LOCATION.BOXPLOT.PANEL.HEIGHT, res=RES, units='in')
    make.location.boxplot(colors=COLORS[-1],
                          scenarios = ALL.COVID.SCENARIOS,
                          outcome.axis.name = "Change in Cumulative Incident HIV Infections,\n2020-2025, due to the COVID-19 Pandemic") +
        BOXPLOT.THEME + 
        xlab(NULL) + coord_flip()
    dev.off()
}