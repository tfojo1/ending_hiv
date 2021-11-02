
library(png)
library(ggsci)
source('code/visualization/plot_wrappers.R')
msa = BALTIMORE.MSA
IMAGE.DIR = paste0('../../../Talks/Ending HIV Model v1/Talks post Annals/plots/', msa)

ALPHA = 0.3
LINE.SIZE = 1#0.4 for intervention
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


THIN.TO = 100


#RUN ON DESKTOP - to save interventions
if (1==2)
{
    source('code/targets/target_msas.R')
    msa = NYC.MSA
    
    load(paste0('Q:/Ending_HIV/full_runs_from_annals/mcmc_runs/full_simsets/1.0_',msa,'_full.Rdata'))
    base = subset.simset(thin.simset(simset, floor(simset@n.sim/THIN.TO)), 1:THIN.TO)
    
    load(paste0('Q:/Ending_HIV/full_runs_from_annals/mcmc_runs/full_simsets/',msa,'/1.0_',msa,'_noint.Rdata'))
    noint = subset.simset(thin.simset(simset, floor(simset@n.sim/THIN.TO)), 1:THIN.TO)
    
    load(paste0('Q:/Ending_HIV/full_runs_from_annals/mcmc_runs/full_simsets/',msa,'/1.0_',msa,'_ybhm.t1x.p10.s80_23.27.Rdata'))
    int1 = subset.simset(thin.simset(simset, floor(simset@n.sim/THIN.TO)), 1:THIN.TO)
    
    load(paste0('Q:/Ending_HIV/full_runs_from_annals/mcmc_runs/full_simsets/',msa,'/1.0_',msa,'_mi.t2x.p25.s90_23.27.Rdata'))
    int2 = subset.simset(thin.simset(simset, floor(simset@n.sim/THIN.TO)), 1:THIN.TO)
    
    save(base, noint, int1, int2, file=paste0('tmp/simsets_for_talk_plots_',msa,'.Rdata'))
}

#RUN ON LAPTOP - for making plots
if (!exists('base'))
{
    msa = NYC.MSA
    load(file=paste0('tmp/simsets_for_talk_plots_',msa,'.Rdata'))
}

##-- GENERATE TEXT for LABELS --##
if (1==2)
{
    inc.reduction = function(sim){
        inc = get.sim.absolute.incidence(sim, years=c(2020,2030))
        (inc[2]-inc[1])/inc[1]
    }
    
    dist.noint = extract.simset.distribution(noint, inc.reduction)
    floor(100*c(get.means(dist.noint), rev(get.intervals(dist.noint))))
    
    dist.int1 = extract.simset.distribution(int1, inc.reduction)
    floor(100*c(get.means(dist.int1), rev(get.intervals(dist.int1))))
    
    dist.int2= extract.simset.distribution(int2, inc.reduction)
    floor(100*c(get.means(dist.int2), rev(get.intervals(dist.int2))))
}

##-- RENDER A PLOTLY TO PNG WITH A SPECIFIC RESOLUTION --#
do.save.plot <- function(plot,
                         file,
                         width,
                         height,
                         resolution=300)
{
    if (!grepl('\\.png$', file))
        file = paste0(file, '.png')
    
    cat(paste0("Saving figure ", file, '...', sep=''))   
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
    cat("DONE\n")
}


# EG for calibration
if (1==2)
{
    one.sim.simset = subset.simset(base, 1:100)
    one.sim.simset@n.sim=as.integer(1)
    one.sim.simset@simulations = one.sim.simset@simulations[1]
    one.sim.simset@weights = 1
    
    plot = plot.simulations.flex(list(one.sim.simset), data.type='new', years=2010:2020,
                                 color.by='intervention', colors=INTERVENTION.COLORS[1:2],
                                 title.subplots=T, hide.legend=T,
                                 simulation.alpha=1, truth.point.size=POINT.SIZE,
                                 simulation.line.size=5,
                                 text.size=TEXT.SIZE*.9, legend.text.size = LEGEND.TEXT.SIZE*.9,
                                 return.change.data.frame=F,
                                 y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
                                 ); plot
    
    
    do.save.plot(plot, file.path(IMAGE.DIR, 'eg_single_sim'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
}

INTERVENTION.COLORS = pal_jama()(4)
DO.INTERVENTIONS = T
DO.CALIBRATION = T
if (DO.INTERVENTIONS)
{
    
    
    x.n = panel.b = plot.simulations.flex(list(base, noint), data.type='new', years=2010:2030,
                                        color.by='intervention', colors=INTERVENTION.COLORS[1:3],
                                        title.subplots=F, hide.legend=T,
                                        simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                        simulation.line.size=LINE.SIZE,
                                        text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                        return.change.data.frame=T,
                                        y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
    ); x.n$plot
    
    do.save.plot(x.n$plot, file.path(IMAGE.DIR, 'projection_new_noint'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    x.i = panel.b = plot.simulations.flex(list(base, noint), data.type='incidence', years=2010:2030,
                                        color.by='intervention', colors=INTERVENTION.COLORS[1:3],
                                        title.subplots=F, hide.legend=T,
                                        simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                        simulation.line.size=LINE.SIZE,
                                        text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                        return.change.data.frame=T,
                                        y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
    ); x.i$plot
    
    do.save.plot(x.i$plot, file.path(IMAGE.DIR, 'projection_inc_noint'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    
    y.n = panel.b = plot.simulations.flex(list(base, noint, int1), data.type='new', years=2010:2030,
                                        color.by='intervention', colors=INTERVENTION.COLORS[1:3],
                                        title.subplots=F, hide.legend=T,
                                        simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                        simulation.line.size=LINE.SIZE,
                                        text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                        return.change.data.frame=T,
                                        y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
    ); y.n$plot
    do.save.plot(y.n$plot, file.path(IMAGE.DIR, 'projection_new_int1'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    y.i = panel.b = plot.simulations.flex(list(base, noint, int1), data.type='incidence', years=2010:2030,
                                          color.by='intervention', colors=INTERVENTION.COLORS[1:3],
                                          title.subplots=F, hide.legend=T,
                                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                          simulation.line.size=LINE.SIZE,
                                          text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                          return.change.data.frame=T,
                                          y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
    ); y.i$plot
    do.save.plot(y.i$plot, file.path(IMAGE.DIR, 'projection_inc_int1'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    z.n = panel.b = plot.simulations.flex(list(base, noint, int2), data.type='new', years=2010:2030,
                                          color.by='intervention', colors=INTERVENTION.COLORS[c(1,2,4)],
                                          title.subplots=F, hide.legend=T,
                                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                          simulation.line.size=LINE.SIZE,
                                          text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                          return.change.data.frame=T,
                                          y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
    ); z.n$plot
    do.save.plot(z.n$plot, file.path(IMAGE.DIR, 'projection_new_int2'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    
    z.i = panel.b = plot.simulations.flex(list(base, noint, int2), data.type='incidence', years=2010:2030,
                                          color.by='intervention', colors=INTERVENTION.COLORS[c(1,2,4)],
                                          title.subplots=F, hide.legend=T,
                                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                          simulation.line.size=LINE.SIZE,
                                          text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                          return.change.data.frame=T,
                                          y.axis.title.function = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
    ); z.i$plot
    do.save.plot(z.i$plot, file.path(IMAGE.DIR, 'projection_inc_int2'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    #-- 3B: Int1 x Reported --#
    x = panel.b = plot.simulations.flex(list(base, noint, int1), data.type='new', years=2010:2030,
                          color.by='intervention', colors=INTERVENTION.COLORS[1:3],
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

#CALIBRATION 
if (DO.CALIBRATION)
{
    # MARGINALS of RACE, RISK, AGE
    
    
    ##-- Total --##
    plot = plot.simulations.flex(base, split.by=NULL, data.type='new',
                                colors=COLORS, 
                                condense.legend=T, title.subplots=T,
                                simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                simulation.line.size=LINE.SIZE,
                                text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration_total_new'), width=WIDTH, height=HEIGHT)
    
    plot = plot.simulations.flex(base, data.type='prevalence',
                                 colors=COLORS, 
                                 condense.legend=T, title.subplots=T,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration_total_prevalence'), width=WIDTH, height=HEIGHT)
    
    plot = plot.simulations.flex(base, data.type='mortality',
                                 colors=COLORS, 
                                 condense.legend=T, title.subplots=T,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration_total_mortality'), width=WIDTH, height=HEIGHT)
    
    plot = plot.simulations.flex(base, split.by=NULL, data.type='suppression',
                                 colors=COLORS, 
                                 condense.legend=T, title.subplots=T,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration_total_suppression'), width=WIDTH, height=HEIGHT)
    
    plot = plot.simulations.flex(base, split.by=NULL, data.type='diagnosed',
                                 colors=COLORS, 
                                 condense.legend=T, title.subplots=T,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration_total_diagnosed'), width=WIDTH, height=HEIGHT)
    
    plot = plot.simulations.flex(base, split.by=NULL, data.type=c('new','prevalence','mortality','suppression'),
                                 color.by='split', colors=COLORS, 
                                 condense.legend=T, title.subplots=T,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration_totals'), width=WIDTH, height=HEIGHT)
    
    
    
    
    
    
    
    ##-- RACE --#
    plot = plot.simulations.flex(base, split.by='race', data.type='new',
                                 color.by='split', colors=COLORS, 
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration_new_race'), width=WIDTH, height=HEIGHT)
    
    
    
    ##-- RISK --#
    plot = plot.simulations.flex(base, split.by='risk', data.type='new',
                                 color.by='split', colors=COLORS, 
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration_new_risk'), width=WIDTH, height=HEIGHT)
    
    
    
    ##-- AGE --#
    plot = plot.simulations.flex(base, split.by='age', data.type='new',
                                 color.by='split', colors=COLORS, 
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration_new_age'), width=WIDTH, height=HEIGHT)
    
    
    ##-- SEX --#
    plot = plot.simulations.flex(base, split.by='sex', data.type='new',
                                 color.by='split', colors=COLORS, 
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration_new_sex'), width=WIDTH, height=HEIGHT)
    
    
    
    
    ##-- RISK x RACE --#
    plot = plot.simulations.flex(base, split.by='race', data.type='new',
                                 color.by='split', colors=COLORS, 
                                 risks='msm',
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration_new_msm_race'), width=WIDTH, height=HEIGHT)
    
    
    plot = plot.simulations.flex(base, split.by='race', data.type='new',
                                 color.by='split', colors=COLORS, 
                                 risks='idu',
                                 facet.by = 'risk',
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration_new_idu_race'), width=WIDTH, height=HEIGHT)

    plot = plot.simulations.flex(base, split.by='race', data.type='new',
                                 color.by='split', colors=COLORS, 
                                 risks='msm_idu',
                                 facet.by = 'risk',
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration_new_msmidu_race'), width=WIDTH, height=HEIGHT)
    
    plot = plot.simulations.flex(base, split.by='race', data.type='new',
                                 color.by='split', colors=COLORS, 
                                 risks='heterosexual',
                                 facet.by = 'risk',
                                 condense.legend=T, title.subplots=F,
                                 simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                 simulation.line.size=LINE.SIZE,
                                 text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE); plot
    do.save.plot(plot, file.path(IMAGE.DIR, 'calibration_new_het_race'), width=WIDTH, height=HEIGHT)
}


## for labels
if (1==2)
{
    floor(-100*x.i$change.df[1,3:5])
    floor(-100*y.i$change.df[2,3:5])
    floor(-100*z.i$change.df[2,3:5])
    
}