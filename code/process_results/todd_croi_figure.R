

source('code/process_results/make_calibration_plots_plotly.R')
INTERVENTION.COLORS = pal_jama()(4)

HEIGHT = 1.5 * 1.3
WIDTH = 1.9 * 1.3

#LA
if (1==2)
{
    #LA
    load('mcmc_runs/visualization_simsets/31080/1.0_31080_baseline.Rdata')
    base = simset
    load('mcmc_runs/visualization_simsets/31080/1.0_31080_noint.Rdata')
    noint = simset
    load('mcmc_runs/visualization_simsets/31080/1.0_31080_het.s90.mi.s90.x.Rdata')
    int1 = simset
    
    x = plot.simulations.flex(list(base, noint, int1), data.type='incidence', years=2010:2030,
                              color.by='intervention', colors=INTERVENTION.COLORS[1:3],
                              title.subplots=F, hide.legend=T,
                              simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                              text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                              return.change.data.frame=T,
                              y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
    ); x$plot
    
    plot = add.plot.label(x$plot, 
                          x=2022, 
                          y=2750,
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
                           x=2018.5, 
                           y=300,
                           fill=INTERVENTION.COLORS[3],
                           font.size=LABEL.SIZE,
                           alpha=LABEL.ALPHA,
                           align='left',
                           text=paste0('All MSM and all IDU 90% Suppressed:\n',
                                       '<b>&#129094;',  
                                       round(100*x$change.df$change_2020_to_2030_mean[2]), "%",
                                       " [", round(100*x$change.df$change_2020_to_2030_interval_lower[2]),
                                       "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[2]),
                                       '%] reduction</b>')
    );plot2
    
    la.inc=plot2
    do.save.plot(la.inc, file.path('../Manuscripts/CROI 2021 web tool abstract/la inc.png'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    x = plot.simulations.flex(list(base, noint, int1), data.type='new', years=2010:2030,
                              color.by='intervention', colors=INTERVENTION.COLORS[1:3],
                              title.subplots=F, hide.legend=T,
                              simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                              text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                              return.change.data.frame=T,
                              y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
    ); x$plot
    
    plot = add.plot.label(x$plot, 
                          x=2022, 
                          y=2750,
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
                           x=2018.5, 
                           y=300,
                           fill=INTERVENTION.COLORS[3],
                           font.size=LABEL.SIZE,
                           alpha=LABEL.ALPHA,
                           align='left',
                           text=paste0('All MSM and all IDU 90% Suppressed:\n',
                                       '<b>&#129094;', 
                                       round(100*x$change.df$change_2020_to_2030_mean[2]), "%",
                                       " [", round(100*x$change.df$change_2020_to_2030_interval_lower[2]),
                                       "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[2]),
                                       '%] reduction</b>')
    );plot2
    
    la.new=plot2
    do.save.plot(la.new, file.path('../Manuscripts/CROI 2021 web tool abstract/la new.png'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
}

#MIAMI
if (1==2)
{
    #Miami
    load('mcmc_runs/quick_simsets/visualization_simsets/33100/1.0_33100_baseline.Rdata')
    base = simset
    load('mcmc_runs/visualization_simsets/33100/1.0_33100_noint.Rdata')
    noint = simset
    #load('mcmc_runs/visualization_simsets/33100/1.0_33100_ybhm.1.25.90.Rdata')
    load('mcmc_runs/visualization_simsets/33100/1.0_33100_mi.p25.ybh.p50.x.Rdata')
    int1 = simset
    
    x = plot.simulations.flex(list(base, noint, int1), data.type='incidence', years=2010:2030,
                              color.by='intervention', colors=INTERVENTION.COLORS[1:3],
                              title.subplots=F, hide.legend=T,
                              simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                              text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                              return.change.data.frame=T,
                              y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
    ); x$plot
    
    plot = add.plot.label(x$plot, 
                          x=2022, 
                          y=2700,
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
                           x=2013, 
                           y=700,
                           fill=INTERVENTION.COLORS[3],
                           font.size=LABEL.SIZE,
                           alpha=LABEL.ALPHA,
                           align='left',
                           text=paste0('Black/Hispanic MSM <35yo:\n',
                                       ' &#8226; Yearly Testing\n',
                                       ' &#8226; 25% on PrEP\n',
                                       ' &#8226; 90% Suppressed\n',
                                       '<b>&#129094;', 
                                       round(100*x$change.df$change_2020_to_2030_mean[2]), "%",
                                       " [", round(100*x$change.df$change_2020_to_2030_interval_lower[2]),
                                       "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[2]),
                                       '%] reduction</b>')
    );plot2
    
    miami=plot2
    do.save.plot(miami, file.path('../Manuscripts/CROI 2021 web tool abstract/miami.png'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
}


#DC
if (1==2)
{
    #DC
    load('mcmc_runs/visualization_simsets/47900/1.0_47900_baseline.Rdata')
    base = simset
    load('mcmc_runs/visualization_simsets/47900/1.0_47900_noint.Rdata')
    noint = simset
    load('mcmc_runs/visualization_simsets/47900/1.0_47900_ybhm.1.50.90.Rdata')
    int1 = simset
    
    x = plot.simulations.flex(list(base, noint, int1), data.type='incidence', years=2010:2030,
                              color.by='intervention', colors=INTERVENTION.COLORS[1:3],
                              title.subplots=F, hide.legend=T,
                              simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                              text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                              return.change.data.frame=T,
                              y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
    ); x$plot
    
    plot = add.plot.label(x$plot, 
                          x=2022, 
                          y=1200,
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
                           x=2013, 
                           y=300,
                           fill=INTERVENTION.COLORS[3],
                           font.size=LABEL.SIZE,
                           alpha=LABEL.ALPHA,
                           align='left',
                           text=paste0('Black/Hispanic MSM <35yo:\n',
                                       ' &#8226; Yearly Testing\n',
                                       ' &#8226; 25% on PrEP\n',
                                       ' &#8226; 90% Suppressed\n',
                                       '<b>&#129094;', 
                                       round(100*x$change.df$change_2020_to_2030_mean[2]), "%",
                                       " [", round(100*x$change.df$change_2020_to_2030_interval_lower[2]),
                                       "-",  round(100*x$change.df$change_2020_to_2030_interval_upper[2]),
                                       '%] reduction</b>')
    );plot2
    
    dc = plot2
    do.save.plot(dc, file.path('../Manuscripts/CROI 2021 web tool abstract/dc.png'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
}