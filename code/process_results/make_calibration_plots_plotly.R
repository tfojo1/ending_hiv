
library(png)
library(ggsci)
source('code/visualization/plot_wrappers.R')
IMAGE.DIR = '../Manuscripts/manuscript_1/Annals Submission/revision 1/images/'

ALPHA = 1#0.2
LINE.SIZE = 1#0.3#0.4 for intervention
COLORS = pal_jama()
WIDTH = 3
HEIGHT = 1.75
POINT.SIZE = 15
THEME = theme(legend.position='bottom', axis.title.x = element_blank(), 
              legend.margin=margin(t=-.125, r=0, b=-0.075, l=0, unit="in"),
            #  legend.text=element_text(size=7),
              legend.direction = 'horizontal',
              legend.spacing.x = unit(0, 'in'))#,
          #    legend.text = element_text(size=8))
TEXT.SIZE = 36
LEGEND.TEXT.SIZE = TEXT.SIZE * 0.75

LABEL.SIZE = 26
LABEL.ALPHA = 0.25

#load('mcmc_runs/visualization_simsets/35620/1.0_35620_baseline.Rdata')


#INTERVENTIONS
if (1==2)
{
   # load('mcmc_runs/full_simsets/35620/1.0_35620_baseline.Rdata')
    load('mcmc_runs/full_simsets/1.0_35620_full.Rdata')
    base = simset
    load('mcmc_runs/full_simsets/35620/1.0_35620_noint.Rdata')
    noint = simset
    load('mcmc_runs/full_simsets/35620/1.0_35620_ybhm.t2x.p25.s80_23.25.Rdata')
    int1 = simset
    load('mcmc_runs/full_simsets/35620/1.0_35620_mi.')
    int2 = simset
}

INTERVENTION.COLORS = pal_jama()(4)
DO.INTERVENTIONS = F
DO.CALIBRATION = F

if (DO.INTERVENTIONS)
{
    
    
    #-- 3B: Int1 x Reported --#
    x = panel.b = plot.simulations.flex(list(base, noint, int1), data.type='new', years=2010:2030,
                          color.by='intervention', colors=INTERVENTION.COLORS[1:3],
                          linetype.by='intervention', linetypes=c('solid','dash'),
                          plot.format = 'mean.and.interval', label.axis.ci=F,
                          title.subplots=F, hide.legend=T,
                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                          simulation.line.size=LINE.SIZE,
                          text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                          return.change.data.frame=T,
                          y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
                          ); x$plot
    
    plot = panel.b = add.plot.label(x$plot, 
                          x=2023, 
                          y=4350,
                          fill=INTERVENTION.COLORS[2],
                          font.size=LABEL.SIZE,
                          alpha=LABEL.ALPHA,
                          align='left',
                          pad=5,
                          text=paste0('No Intervention:\n<b>&#129094;', 
                                      round(100*x$change.df$change_2020_to_2030_mean[1]), "%",
                                      " [", round(100*x$change.df$change_2020_to_2030_interval_lower[1]),
                                      "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[1]),
                                      '%] reduction</b>')
    );plot

    plot2 = add.plot.label(plot, 
                          x=2015, 
                          y=1000,
                          fill=INTERVENTION.COLORS[3],
                          font.size=LABEL.SIZE,
                          alpha=LABEL.ALPHA,
                          align='left',
                          text=paste0('Black/Hispanic MSM <35yo:\n',
                                        ' &#8226; Twice-Yearly Testing\n',
                                        ' &#8226; 25% on PrEP\n',
                                        ' &#8226; 80% Suppressed\n',
                                        '<b>&#129094;', 
                                      round(100*x$change.df$change_2020_to_2030_mean[2]), "%",
                                      " [", round(100*x$change.df$change_2020_to_2030_interval_lower[2]),
                                      "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[2]),
                                      '%] reduction</b>')
    );plot2
    
    do.save.plot(plot2, file.path(IMAGE.DIR, 'interventions/Figure 3b.png'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    #-- 3D: Int2 x Reported --#
    x = panel.d = plot.simulations.flex(list(base, noint, int2), data.type='new', years=2010:2030,
                              color.by='intervention', colors=INTERVENTION.COLORS[c(1,2,4)],
                              linetype.by='intervention', linetypes=c('solid','dash'),
                              plot.format = 'mean.and.interval', label.axis.ci=F,
                              title.subplots=F, hide.legend=T,
                              simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                              simulation.line.size=LINE.SIZE,
                              text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                              return.change.data.frame=T,
                              y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
    ); x$plot
    
    plot = add.plot.label(x$plot, 
                          x=2023, 
                          y=4350,
                          fill=INTERVENTION.COLORS[2],
                          font.size=LABEL.SIZE,
                          alpha=LABEL.ALPHA,
                          align='left',
                          pad=5,
                          text=paste0('No Intervention:\n<b>&#129094;', 
                                      round(100*x$change.df$change_2020_to_2030_mean[1]), "%",
                                      " [", round(100*x$change.df$change_2020_to_2030_interval_lower[1]),
                                      "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[1]),
                                      '%] reduction</b>')
    );plot
    
    plot2 = add.plot.label(plot, 
                           x=2015, 
                           y=1000,
                           fill=INTERVENTION.COLORS[4],
                           font.size=LABEL.SIZE,
                           alpha=LABEL.ALPHA,
                           align='left',
                           text=paste0('All MSM and All PWID:\n',
                                       ' &#8226; Twice-Yearly Testing\n',
                                       ' &#8226; 50% on PrEP\n',
                                       ' &#8226; 90% Suppressed\n',
                                       '<b>&#129094;', 
                                       round(100*x$change.df$change_2020_to_2030_mean[2]), "%",
                                       " [", round(100*x$change.df$change_2020_to_2030_interval_lower[2]),
                                       "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[2]),
                                       '%] reduction</b>')
    );plot2 
    
    do.save.plot(plot2, file.path(IMAGE.DIR, 'interventions/Figure 3d.png'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    
    
    #-- 3A: Int1 x Incidence --#
    x = plot.simulations.flex(list(base, noint, int1), data.type='incidence', years=2010:2030,
                              color.by='intervention', colors=INTERVENTION.COLORS[1:3],
                              linetype.by='intervention', linetypes=c('solid','dash'),
                              plot.format = 'mean.and.interval', label.axis.ci=F,
                              title.subplots=F, hide.legend=T,
                              simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                              simulation.line.size=LINE.SIZE,
                              text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                              return.change.data.frame=T,
                              y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
    ); x$plot
    
    plot = add.plot.label(x$plot, 
                          x=2023, 
                          y=3600,
                          fill=INTERVENTION.COLORS[2],
                          font.size=LABEL.SIZE,
                          alpha=LABEL.ALPHA,
                          align='left',
                          pad=5,
                          text=paste0('No Intervention:\n<b>&#129094;', 
                                      round(100*x$change.df$change_2020_to_2030_mean[1]), "%",
                                      " [", round(100*x$change.df$change_2020_to_2030_interval_lower[1]),
                                      "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[1]),
                                      '%] reduction</b>')
    );plot
    
    plot2 = add.plot.label(plot, 
                           x=2015, 
                           y=900,
                           fill=INTERVENTION.COLORS[3],
                           font.size=LABEL.SIZE,
                           alpha=LABEL.ALPHA,
                           align='left',
                           text=paste0('Black/Hispanic MSM <35yo:\n',
                                       ' &#8226; Twice-Yearly Testing\n',
                                       ' &#8226; 25% on PrEP\n',
                                       ' &#8226; 80% Suppressed\n',
                                       '<b>&#129094;', 
                                       round(100*x$change.df$change_2020_to_2030_mean[2]), "%",
                                       " [", round(100*x$change.df$change_2020_to_2030_interval_lower[2]),
                                       "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[2]),
                                       '%] reduction</b>')
    );plot2
    
    do.save.plot(plot2, file.path(IMAGE.DIR, 'interventions/Figure 3a.png'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    
    #-- 3C: Int2 x Incidence --#
    x = plot.simulations.flex(list(base, noint, int2), data.type='incidence', years=2010:2030,
                              color.by='intervention', colors=INTERVENTION.COLORS[c(1,2,4)],
                              linetype.by='intervention', linetypes=c('solid','dash'),
                              plot.format = 'mean.and.interval', label.axis.ci=F,
                              title.subplots=F, hide.legend=T,
                              simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                              simulation.line.size=LINE.SIZE,
                              text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                              return.change.data.frame=T,
                              y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
    ); x$plot
    
    plot = add.plot.label(x$plot, 
                          x=2023, 
                          y=3600,
                          fill=INTERVENTION.COLORS[2],
                          font.size=LABEL.SIZE,
                          alpha=LABEL.ALPHA,
                          align='left',
                          pad=5,
                          text=paste0('No Intervention:\n<b>&#129094;', 
                                      round(100*x$change.df$change_2020_to_2030_mean[1]), "%",
                                      " [", round(100*x$change.df$change_2020_to_2030_interval_lower[1]),
                                      "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[1]),
                                      '%] reduction</b>')
    );plot
    
    plot2 = add.plot.label(plot, 
                           x=2014, 
                           y=1100,
                           fill=INTERVENTION.COLORS[4],
                           font.size=LABEL.SIZE,
                           alpha=LABEL.ALPHA,
                           align='left',
                           text=paste0('All MSM and All PWID:\n',
                                       ' &#8226; Twice-Yearly Testing\n',
                                       ' &#8226; 50% on PrEP\n',
                                       ' &#8226; 90% Suppressed\n',
                                       '<b>&#129094;', 
                                       round(100*x$change.df$change_2020_to_2030_mean[2]), "%",
                                       " [", round(100*x$change.df$change_2020_to_2030_interval_lower[2]),
                                       "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[2]),
                                       '%]\n   reduction</b>')
    );plot2
    
    do.save.plot(plot2, file.path(IMAGE.DIR, 'interventions/Figure 3c.png'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
}

CALIBRATION.NEW.YLIM = c(0,2500)
CALIBRATION.PREV.YLIM = c(0,70000)
#CALIBRATION 
if (DO.CALIBRATION)
{
    # MARGINALS of RACE, RISK, AGE
    
    
    ##-- RACE --#
    plot = plot.simulations.flex(base, split.by='race', data.type='new',
                                 color.by='split', colors=COLORS, 
                                 linetype.by='split', linetypes=c('solid','dash','dot'),
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 plot.format = 'mean.and.interval', label.axis.ci=F,
                                 ylim = CALIBRATION.NEW.YLIM,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration/Figure 2a.png'), width=WIDTH, height=HEIGHT)
    
    
    plot = plot.simulations.flex(base, split.by='race', data.type='prevalence',
                                 color.by='split', colors=COLORS, 
                                 linetype.by='split', linetypes=c('solid','dash','dot'),
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 plot.format = 'mean.and.interval', label.axis.ci=F,
                                 ylim = CALIBRATION.PREV.YLIM,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration/Figure 2b.png'), width=WIDTH, height=HEIGHT)
    
    
    ##-- RISK --#
    plot = plot.simulations.flex(base, split.by='risk', data.type='new',
                                 color.by='split', colors=COLORS, 
                                 linetype.by='split', linetypes=c('solid','dot','dashdot','dash'),
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 plot.format = 'mean.and.interval', label.axis.ci=F,
                                 ylim = CALIBRATION.NEW.YLIM,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration/Figure 2c.png'), width=WIDTH, height=HEIGHT)
    
    
    plot = plot.simulations.flex(base, split.by='risk', data.type='prevalence',
                                 color.by='split', colors=COLORS, 
                                 linetype.by='split', linetypes=c('solid','dot','dashdot','dash'),
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 plot.format = 'mean.and.interval', label.axis.ci=F,
                                 ylim = CALIBRATION.PREV.YLIM,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration/Figure 2d.png'), width=WIDTH, height=HEIGHT)
    
    
    
    ##-- AGE --#
    plot = plot.simulations.flex(base, split.by='age', data.type='new',
                                 color.by='split', colors=COLORS, 
                                 linetype.by='split', linetypes=c('longdash','solid','dash','dot','dashdot'),
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 plot.format = 'mean.and.interval', label.axis.ci=F,
                                 ylim = CALIBRATION.NEW.YLIM,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration/Figure 2e.png'), width=WIDTH, height=HEIGHT)
    
    
    plot = plot.simulations.flex(base, split.by='age', data.type='prevalence',
                                 color.by='split', colors=COLORS, 
                                 linetype.by='split', linetypes=c('longdash','solid','dash','dot','dashdot'),
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 plot.format = 'mean.and.interval', label.axis.ci=F,
                                 ylim = CALIBRATION.PREV.YLIM,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration/Figure 2f.png'), width=WIDTH, height=HEIGHT)
    
    
}

##-- THE BATON ROUGE SENSITIVITY ANALYSIS --##

if (1==2)
{
    load('mcmc_runs/full_simsets/1.0_12940_full.Rdata')
    baseline = simset
    load('mcmc_runs/full_simsets/12940/1.0_12940_noint.Rdata')
    noint = simset
    load('mcmc_runs/full_simsets/12940/1.0_12940_mi.t2x.p25.s90_23.27.Rdata')
    int = simset
    
    parameter = 'black.heterosexual.trate.2'
    o = order(simset@parameters[,parameter])
    
    int.low = subset.simset(int, o[1:200])
    int.high = subset.simset(int, o[801:1000])
    
    x = plot.simulations.flex(list(int.low, int.high), 
                                 years=2020:2030, data.type='incidence',
                                 color.by='intervention', colors=COLORS, 
                                 linetype.by='intervention', linetypes=c('solid','dash','dash'),
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 plot.format = 'mean.and.interval', label.axis.ci=F,
                              return.change.data.frame=T,
                               #  ylim = CALIBRATION.NEW.YLIM,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); x$plot
    
    plot.calibration(list(noint, int.low, int.high), data.types = 'incidence', years=2020:2030, facet.by=NULL)
    plot.calibration(list(noint, int), data.types = 'incidence', years=2020:2030, facet.by=NULL, plot.individual.simset.sims = F)
}

##-- RENDER A PLOTLY TO PNG WITH A SPECIFIC RESOLUTION --#
do.save.plot <- function(plot,
                         file,
                         width,
                         height,
                         resolution=300)
{
    
    orca(plot,
         file = file,
         width = width*resolution, 
         height = height*resolution)
    
    # Re-render at desired resolution    
    img<-readPNG(file)
    
    
    png(file, width=width, height=height, res=resolution, units='in')
    par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
    plot.new()
    plot.window(0:1, 0:1)
    usr<-par("usr")    
    rasterImage(img, usr[1], usr[3], usr[2], usr[4])
    dev.off()
}
