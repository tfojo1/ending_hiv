
source('code/calibration/target_msas.R')
msa = DALLAS.MSA
source('code/processing/for_talks/talk_plot_settings.R')

DO.INTERVENTIONS = F
DO.CALIBRATION = T
DO.STRATIFIED.INTERVENTIONS = F

#RUN ON DESKTOP - to save interventions
if (is.desktop)
{
    base.filename = paste0('1.0_',msa,'_full.Rdata')
    noint.filename = paste0('1.0_',msa,'_noint.Rdata')
    int1.filename = paste0('1.0_',msa,'_ybhm.t1x.p10.s80_23.27.Rdata')
    int2.filename = paste0('1.0_',msa,'_mi.t2x.p25.s90_23.27.Rdata')
    
    src.dir = SIMULATIONS.DIR
#    if (!file.exists(file.path(src.dir, msa, int2.filename)))
#        src.dir = file.path(SYSTEMATIC.ROOT.DIR, 'full_runs_from_annals/mcmc_runs/full_simsets')
    
    
    load(file.path(src.dir, 'baseline_collapsed', base.filename))
    simset = flatten.simset(simset)
    base = subset.simset(thin.simset(simset, floor(simset@n.sim/THIN.TO)), 1:THIN.TO)
    
    load(file.path(src.dir, 'ehe', msa, noint.filename))
    noint = subset.simset(thin.simset(simset, floor(simset@n.sim/THIN.TO)), 1:THIN.TO)
    
    load(file.path(src.dir, 'ehe', msa, int1.filename))
    int1 = subset.simset(thin.simset(simset, floor(simset@n.sim/THIN.TO)), 1:THIN.TO)
    
    load(file.path(src.dir, 'ehe', msa, int2.filename))
    int2 = subset.simset(thin.simset(simset, floor(simset@n.sim/THIN.TO)), 1:THIN.TO)
    
    save(base, noint, int1, int2, file=paste0('tmp/simsets_for_talk_plots_',msa,'.Rdata'))
    
    print(paste0("Done saving simsets for ", msa.names(msa)))
}

#RUN ON LAPTOP - for making plots
if (!is.desktop && !exists('base'))
{
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
    
    
    inc.reduction.race = function(sim){
        inc = get.sim.absolute.incidence(sim, years=c(2020,2030), keep.dimensions=c('year','race'))
        (inc[2,]-inc[1,])/inc[1,]
    }
    
    dist.race.noint = extract.simset.distribution(noint, inc.reduction.race)
    floor(100*cbind(get.means(dist.race.noint), t(get.intervals(dist.race.noint))[,c(2,1)]))
    
    dist.race.int1 = extract.simset.distribution(int1, inc.reduction.race)
    floor(100*cbind(get.means(dist.race.int1), t(get.intervals(dist.race.int1))[,c(2,1)]))
    
    dist.race.int2 = extract.simset.distribution(int2, inc.reduction.race)
    floor(100*cbind(get.means(dist.race.int2), t(get.intervals(dist.race.int2))[,c(2,1)]))
}



# EG for calibration
if (DO.CALIBRATION && !is.desktop)
{
    one.sim.simset = subset.simset(base, 1+50+10*(1:3))#1:100)
    one.sim.simset@n.sim=as.integer(1)
    one.sim.simset@simulations = one.sim.simset@simulations[2]
    one.sim.simset@weights = 1
    
    plot = plot.simulations.flex(list(one.sim.simset), data.type='new', years=2010:2020,
                                 color.by='intervention', colors=INTERVENTION.COLORS[1:2],
                                 title.subplots=T, hide.legend=T,
                                 simulation.alpha=1, truth.point.size=POINT.SIZE,
                                 simulation.line.size=5,
                                 text.size=TEXT.SIZE*.9, legend.text.size = LEGEND.TEXT.SIZE*.9,
                                 return.change.data.frame=F,
                                 y.axis.title.function = Y.TITLE.FUNCTION
                                 ); plot
    
    
    do.save.plot(plot, file.path(IMAGE.DIR, 'eg_single_sim'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
}

if (DO.INTERVENTIONS && !is.desktop)
{
    x.n = panel.b = plot.simulations.flex(list(base, noint), data.type='new', years=2010:2030,
                                        color.by='intervention', colors=INTERVENTION.COLORS[1:3],
                                        title.subplots=F, hide.legend=T,
                                        simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                        simulation.line.size=LINE.SIZE,
                                        text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                        return.change.data.frame=T,
                                        y.axis.title.function = Y.TITLE.FUNCTION
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
                                        y.axis.title.function = Y.TITLE.FUNCTION
    ); x.i$plot
    print(paste0('NO INTERVENTION: ', 
                 floor(-100*x.i$change.df[1,3]), '% [',
                 floor(-100*x.i$change.df[1,5]), ' to ',
                 floor(-100*x.i$change.df[1,4]), '%]'))
    do.save.plot(x.i$plot, file.path(IMAGE.DIR, 'projection_inc_noint'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    
    y.n = panel.b = plot.simulations.flex(list(base, noint, int1), data.type='new', years=2010:2030,
                                        color.by='intervention', colors=INTERVENTION.COLORS[1:3],
                                        title.subplots=F, hide.legend=T,
                                        simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                        simulation.line.size=LINE.SIZE,
                                        text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                        return.change.data.frame=T,
                                        y.axis.title.function = Y.TITLE.FUNCTION
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
                                          y.axis.title.function = Y.TITLE.FUNCTION
    ); y.i$plot
    print(paste0('INTERVENTION 1: ', 
                 floor(-100*y.i$change.df[2,3]), '% [',
                 floor(-100*y.i$change.df[2,5]), ' to ',
                 floor(-100*y.i$change.df[2,4]), '%]'))
    do.save.plot(y.i$plot, file.path(IMAGE.DIR, 'projection_inc_int1'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    z.n = panel.b = plot.simulations.flex(list(base, noint, int2), data.type='new', years=2010:2030,
                                          color.by='intervention', colors=INTERVENTION.COLORS[c(1,2,4)],
                                          title.subplots=F, hide.legend=T,
                                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                          simulation.line.size=LINE.SIZE,
                                          text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                          return.change.data.frame=T,
                                          y.axis.title.function = Y.TITLE.FUNCTION
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
                                          y.axis.title.function = Y.TITLE.FUNCTION
    ); z.i$plot
    print(paste0('INTERVENTION 2: ', 
                 floor(-100*z.i$change.df[2,3]), '% [',
                 floor(-100*z.i$change.df[2,5]), ' to ',
                 floor(-100*z.i$change.df[2,4]), '%]'))
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
                          y.axis.title.function = Y.TITLE.FUNCTION
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
                              y.axis.title.function = Y.TITLE.FUNCTION
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
                              y.axis.title.function = Y.TITLE.FUNCTION
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
                              y.axis.title.function = Y.TITLE.FUNCTION
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

#STRATIFIED INTERVENTIONS
if (DO.STRATIFIED.INTERVENTIONS && !is.desktop)
{
    x.i = panel.b = plot.simulations.flex(list(base, noint), data.type='incidence', years=2010:2030,
                                          race='black',
                                          color.by='intervention', colors=INTERVENTION.COLORS[1:3],
                                          title.subplots=F, hide.legend=T,
                                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                          simulation.line.size=LINE.SIZE,
                                          text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                          return.change.data.frame=T,
                                          y.axis.title.function = Y.TITLE.FUNCTION
    ); x.i$plot
    
    do.save.plot(x.i$plot, file.path(IMAGE.DIR, 'projection_inc_noint'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
}

#CALIBRATION 
if (DO.CALIBRATION && !is.desktop)
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