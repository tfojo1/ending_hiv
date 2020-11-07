
library(png)
library(ggsci)
source('code/visualization/plot_wrappers.R')
IMAGE.DIR = 'results/figures'

ALPHA = 0.2
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
    load('mcmc_runs/visualization_simsets/35620/1.0_35620_baseline.Rdata')
    base = simset
    load('mcmc_runs/visualization_simsets/35620/1.0_35620_noint.Rdata')
    noint = simset
    load('mcmc_runs/visualization_simsets/35620/1.0_35620_ybhm.1.25.80.Rdata')
    int1 = simset
    load('mcmc_runs/visualization_simsets/35620/1.0_35620_mi.1.50.90.ybh.high.x.Rdata')
    int2 = simset
}

if (1==2)
{
    INTERVENTION.COLORS = pal_jama()(4)
    
    
    #-- 3A: Int1 x Reported --#
    x = plot.simulations.flex(list(base, noint, int1), data.type='new', years=2000:2030,
                          color.by='intervention', colors=INTERVENTION.COLORS[1:3],
                          title.subplots=F, hide.legend=T,
                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                          text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                          return.change.data.frame=T,
                          y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
                          ); x$plot
    
    plot = add.plot.label(x$plot, 
                          x=2022, 
                          y=4500,
                          fill=INTERVENTION.COLORS[2],
                          font.size=LABEL.SIZE,
                          alpha=LABEL.ALPHA,
                          align='left',
                          pad=5,
                          text=paste0('No Intervention:\n<b>&#129094;', 
                                      round(100*x$change.df$change_2020_to_2030[1]), "%",
                                      " [", round(100*x$change.df$change_2020_to_2030_interval_lower[1]),
                                      "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[1]),
                                      '%] reduction</b>')
    );plot

    plot2 = add.plot.label(plot, 
                          x=2013, 
                          y=1200,
                          fill=INTERVENTION.COLORS[3],
                          font.size=LABEL.SIZE,
                          alpha=LABEL.ALPHA,
                          align='left',
                          text=paste0('Black/Hispanic MSM <35yo:\n',
                                        ' &#8226; Yearly Testing\n',
                                        ' &#8226; 25% on PrEP\n',
                                        ' &#8226; 90% Suppressed\n',
                                        '<b>&#129094;', 
                                      round(100*x$change.df$change_2020_to_2030[2]), "%",
                                      " [", round(100*x$change.df$change_2020_to_2030_interval_lower[2]),
                                      "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[2]),
                                      '%] reduction</b>')
    );plot2
    
    do.save.plot(plot2, file.path(IMAGE.DIR, 'interventions/Figure 3a.png'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    
    #-- 3B: Int1 x Incidence --#
    x = plot.simulations.flex(list(base, noint, int1), data.type='incidence', years=2000:2030,
                              color.by='intervention', colors=INTERVENTION.COLORS[1:3],
                              title.subplots=F, hide.legend=T,
                              simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                              text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                              return.change.data.frame=T,
                              y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
    ); x$plot
    
    plot = add.plot.label(x$plot, 
                          x=2022, 
                          y=3700,
                          fill=INTERVENTION.COLORS[2],
                          font.size=LABEL.SIZE,
                          alpha=LABEL.ALPHA,
                          align='left',
                          pad=5,
                          text=paste0('No Intervention:\n<b>&#129094;', 
                                      round(100*x$change.df$change_2020_to_2030[1]), "%",
                                      " [", round(100*x$change.df$change_2020_to_2030_interval_lower[1]),
                                      "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[1]),
                                      '%] reduction</b>')
    );plot
    
    plot2 = add.plot.label(plot, 
                           x=2013, 
                           y=900,
                           fill=INTERVENTION.COLORS[3],
                           font.size=LABEL.SIZE,
                           alpha=LABEL.ALPHA,
                           align='left',
                           text=paste0('Black/Hispanic MSM <35yo:\n',
                                       ' &#8226; Yearly Testing\n',
                                       ' &#8226; 25% on PrEP\n',
                                       ' &#8226; 90% Suppressed\n',
                                       '<b>&#129094;', 
                                       round(100*x$change.df$change_2020_to_2030[2]), "%",
                                       " [", round(100*x$change.df$change_2020_to_2030_interval_lower[2]),
                                       "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[2]),
                                       '%] reduction</b>')
    );plot2

    do.save.plot(plot2, file.path(IMAGE.DIR, 'interventions/Figure 3b.png'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    #-- 3C: Int2 x Reported --#
    x = plot.simulations.flex(list(base, noint, int2), data.type='new', years=2000:2030,
                              color.by='intervention', colors=INTERVENTION.COLORS[c(1,2,4)],
                              title.subplots=F, hide.legend=T,
                              simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                              text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                              return.change.data.frame=T,
                              y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
    ); x$plot
    
    plot = add.plot.label(x$plot, 
                          x=2022, 
                          y=4500,
                          fill=INTERVENTION.COLORS[2],
                          font.size=LABEL.SIZE,
                          alpha=LABEL.ALPHA,
                          align='left',
                          pad=5,
                          text=paste0('No Intervention:\n<b>&#129094;', 
                                      round(100*x$change.df$change_2020_to_2030[1]), "%",
                                      " [", round(100*x$change.df$change_2020_to_2030_interval_lower[1]),
                                      "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[1]),
                                      '%] reduction</b>')
    );plot
    
    plot2 = add.plot.label(plot, 
                           x=2013, 
                           y=1200,
                           fill=INTERVENTION.COLORS[4],
                           font.size=LABEL.SIZE,
                           alpha=LABEL.ALPHA,
                           align='left',
                           text=paste0('All MSM and All IDU:\n',
                                       ' &#8226; Yearly Testing\n',
                                       ' &#8226; 25% on PrEP\n',
                                       ' &#8226; 90% Suppressed\n',
                                       '<b>&#129094;', 
                                       round(100*x$change.df$change_2020_to_2030[2]), "%",
                                       " [", round(100*x$change.df$change_2020_to_2030_interval_lower[2]),
                                       "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[2]),
                                       '%] reduction</b>')
    );plot2 

    do.save.plot(plot2, file.path(IMAGE.DIR, 'interventions/Figure 3c.png'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    
    #-- 3D: Int2 x Incidence --#
    x = plot.simulations.flex(list(base, noint, int2), data.type='incidence', years=2000:2030,
                              color.by='intervention', colors=INTERVENTION.COLORS[c(1,2,4)],
                              title.subplots=F, hide.legend=T,
                              simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                              text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                              return.change.data.frame=T,
                              y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
    ); x$plot
    
    plot = add.plot.label(x$plot, 
                          x=2022, 
                          y=3700,
                          fill=INTERVENTION.COLORS[2],
                          font.size=LABEL.SIZE,
                          alpha=LABEL.ALPHA,
                          align='left',
                          pad=5,
                          text=paste0('No Intervention:\n<b>&#129094;', 
                                      round(100*x$change.df$change_2020_to_2030[1]), "%",
                                      " [", round(100*x$change.df$change_2020_to_2030_interval_lower[1]),
                                      "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[1]),
                                      '%] reduction</b>')
    );plot
    
    plot2 = add.plot.label(plot, 
                           x=2013, 
                           y=900,
                           fill=INTERVENTION.COLORS[4],
                           font.size=LABEL.SIZE,
                           alpha=LABEL.ALPHA,
                           align='left',
                           text=paste0('All MSM and All IDU:\n',
                                       ' &#8226; Yearly Testing\n',
                                       ' &#8226; 25% on PrEP\n',
                                       ' &#8226; 90% Suppressed\n',
                                       '<b>&#129094;', 
                                       round(100*x$change.df$change_2020_to_2030[2]), "%",
                                       " [", round(100*x$change.df$change_2020_to_2030_interval_lower[2]),
                                       "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[2]),
                                       '%] reduction</b>')
    );plot2
    
    do.save.plot(plot2, file.path(IMAGE.DIR, 'interventions/Figure 3d.png'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
}

#CALIBRATION 
if (1==2)
{
    # MARGINALS of RACE, RISK, AGE
    
    
    ##-- RACE --#
    plot = plot.simulations.flex(base, split.by='race', data.type='new',
                                 color.by='split', colors=COLORS, 
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration/Figure 2a.png'), width=WIDTH, height=HEIGHT)
    
    
    plot = plot.simulations.flex(base, split.by='race', data.type='prevalence',
                                 color.by='split', colors=COLORS, 
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration/Figure 2b.png'), width=WIDTH, height=HEIGHT)
    
    
    ##-- RISK --#
    plot = plot.simulations.flex(base, split.by='risk', data.type='new',
                                 color.by='split', colors=COLORS, 
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration/Figure 2c.png'), width=WIDTH, height=HEIGHT)
    
    
    plot = plot.simulations.flex(base, split.by='risk', data.type='prevalence',
                                 color.by='split', colors=COLORS, 
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration/Figure 2d.png'), width=WIDTH, height=HEIGHT)
    
    
    
    ##-- AGE --#
    plot = plot.simulations.flex(base, split.by='age', data.type='new',
                                 color.by='split', colors=COLORS, 
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration/Figure 2e.png'), width=WIDTH, height=HEIGHT)
    
    
    plot = plot.simulations.flex(base, split.by='age', data.type='prevalence',
                                 color.by='split', colors=COLORS, 
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration/Figure 2f.png'), width=WIDTH, height=HEIGHT)
    

}

##-_ RENER A PLOTLY TO PNG WITH A SPECIFIC RESOLUTION --#
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
    
    
    png(file, width=WIDTH, height=HEIGHT, res=resolution, units='in')
    par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
    plot.new()
    plot.window(0:1, 0:1)
    usr<-par("usr")    
    rasterImage(img, usr[1], usr[3], usr[2], usr[4])
    dev.off()
}
