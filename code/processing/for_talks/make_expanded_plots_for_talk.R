
source('code/calibration/target_msas.R')
msa = BALTIMORE.MSA
source('code/settings_and_files/systematic_settings.R')
source('code/processing/for_talks/talk_plot_settings.R')

DO.INTERVENTIONS = T

#RUN ON DESKTOP - to save interventions
if (ON.DESKTOP)
{
    #expanded.int1 = 'mi.t2x_het.t1x_23.27'
    #expanded.int2 = 'all.p25_23.27'
    #expanded.int3 = 'all.s90_23.27'
    
    expanded.int1 = 'all.p25_23.27'
    expanded.int2 = 'mi.t2x_het.t1x_23.27'
    expanded.int3 = 'all.s90_23.27'
    expanded.int4 = 'all.marginal.t125.p05.s10_23.27'
    
    ex1.filename = paste0('1.0_',msa,'_', expanded.int1, '.Rdata')
    ex2.filename = paste0('1.0_',msa,'_', expanded.int2, '.Rdata')
    ex3.filename = paste0('1.0_',msa,'_', expanded.int3, '.Rdata')
    ex4.filename = paste0('1.0_',msa,'_', expanded.int4, '.Rdata')
    
    
    src.dir = 'Q:/JHEEM/simulations/ehe'
    
    
    load(file.path(src.dir, msa, ex1.filename))
    ex1 = subset.simset(thin.simset(simset, floor(simset@n.sim/THIN.TO)), 1:THIN.TO)
    
    load(file.path(src.dir, msa, ex2.filename))
    ex2 = subset.simset(thin.simset(simset, floor(simset@n.sim/THIN.TO)), 1:THIN.TO)
    
    load(file.path(src.dir, msa, ex3.filename))
    ex3 = subset.simset(thin.simset(simset, floor(simset@n.sim/THIN.TO)), 1:THIN.TO)
    
    load(file.path(src.dir, msa, ex4.filename))
    ex4 = subset.simset(thin.simset(simset, floor(simset@n.sim/THIN.TO)), 1:THIN.TO)
    
    save(ex1, ex2, ex3, ex4, file=paste0('tmp/simsets_for_talk_expanded_plots_',msa,'.Rdata'))
    
    print(paste0("Done saving *expanded* simsets for ", msa.names(msa)))
}


#RUN ON LAPTOP - for making plots

if (!ON.DESKTOP &&
    (!exists('base') || 
    attr(base@simulations[[1]], 'location')!=msa))
{
    load(file=paste0('tmp/simsets_for_talk_plots_',msa,'.Rdata'))
}

if (!ON.DESKTOP &&
    (!exists('ex1') || 
    attr(ex1@simulations[[1]], 'location')!=msa))
{
    load(file=paste0('tmp/simsets_for_talk_expanded_plots_',msa,'.Rdata'))
}

if (DO.INTERVENTIONS && !ON.DESKTOP)
{
    # EX 1
    y1.n = panel.b = plot.simulations.flex(list(base, noint, ex1), data.type='new', years=2010:2030,
                                          color.by='intervention', colors=INTERVENTION.COLORS[c('truth','base','ex1')],
                                          title.subplots=F, hide.legend=T,
                                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                          simulation.line.size=LINE.SIZE,
                                          text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                          return.change.data.frame=T,
                                          y.axis.title.function = Y.TITLE.FUNCTION
    ); y1.n$plot
    do.save.plot(y1.n$plot, file.path(IMAGE.DIR, 'projection_new_ex1'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    y1.i = panel.b = plot.simulations.flex(list(base, noint, ex1), data.type='incidence', years=2010:2030,
                                          color.by='intervention', colors=INTERVENTION.COLORS[c('truth','base','ex1')],
                                          title.subplots=F, hide.legend=T,
                                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                          simulation.line.size=LINE.SIZE,
                                          text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                          return.change.data.frame=T,
                                          y.axis.title.function = Y.TITLE.FUNCTION
    ); y1.i$plot
    print(paste0('EX INTERVENTION 1: ', 
                 floor(-100*y1.i$change.df[2,3]), '% [',
                 floor(-100*y1.i$change.df[2,5]), ' to ',
                 floor(-100*y1.i$change.df[2,4]), '%]'))
    do.save.plot(y1.i$plot, file.path(IMAGE.DIR, 'projection_inc_ex1'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    # EX 2
    y2.n = panel.b = plot.simulations.flex(list(base, noint, ex2), data.type='new', years=2010:2030,
                                           color.by='intervention', colors=INTERVENTION.COLORS[c('truth','base','ex2')],
                                           title.subplots=F, hide.legend=T,
                                           simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                           simulation.line.size=LINE.SIZE,
                                           text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                           return.change.data.frame=T,
                                           y.axis.title.function = Y.TITLE.FUNCTION
    ); y2.n$plot
    do.save.plot(y2.n$plot, file.path(IMAGE.DIR, 'projection_new_ex2'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    y2.i = panel.b = plot.simulations.flex(list(base, noint, ex2), data.type='incidence', years=2010:2030,
                                           color.by='intervention', colors=INTERVENTION.COLORS[c('truth','base','ex2')],
                                           title.subplots=F, hide.legend=T,
                                           simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                           simulation.line.size=LINE.SIZE,
                                           text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                           return.change.data.frame=T,
                                           y.axis.title.function = Y.TITLE.FUNCTION
    ); y2.i$plot
    print(paste0('EX INTERVENTION 2: ', 
                 floor(-100*y2.i$change.df[2,3]), '% [',
                 floor(-100*y2.i$change.df[2,5]), ' to ',
                 floor(-100*y2.i$change.df[2,4]), '%]'))
    do.save.plot(y2.i$plot, file.path(IMAGE.DIR, 'projection_inc_ex2'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    # EX 3
    y3.n = panel.b = plot.simulations.flex(list(base, noint, ex3), data.type='new', years=2010:2030,
                                           color.by='intervention', colors=INTERVENTION.COLORS[c('truth','base','ex3')],
                                           title.subplots=F, hide.legend=T,
                                           simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                           simulation.line.size=LINE.SIZE,
                                           text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                           return.change.data.frame=T,
                                           y.axis.title.function = Y.TITLE.FUNCTION
    ); y3.n$plot
    do.save.plot(y3.n$plot, file.path(IMAGE.DIR, 'projection_new_ex3'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    y3.i = panel.b = plot.simulations.flex(list(base, noint, ex3), data.type='incidence', years=2010:2030,
                                           color.by='intervention', colors=INTERVENTION.COLORS[c('truth','base','ex3')],
                                           title.subplots=F, hide.legend=T,
                                           simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                           simulation.line.size=LINE.SIZE,
                                           text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                           return.change.data.frame=T,
                                           y.axis.title.function = Y.TITLE.FUNCTION
    ); y3.i$plot
    print(paste0('EX INTERVENTION 3: ', 
                 floor(-100*y3.i$change.df[2,3]), '% [',
                 floor(-100*y3.i$change.df[2,5]), ' to ',
                 floor(-100*y3.i$change.df[2,4]), '%]'))
    do.save.plot(y3.i$plot, file.path(IMAGE.DIR, 'projection_inc_ex3'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    
    # EX 4
    y4.n = panel.b = plot.simulations.flex(list(base, noint, ex4), data.type='new', years=2010:2030,
                                           color.by='intervention', colors=INTERVENTION.COLORS[c('truth','base','ex4')],
                                           title.subplots=F, hide.legend=T,
                                           simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                           simulation.line.size=LINE.SIZE,
                                           text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                           return.change.data.frame=T,
                                           y.axis.title.function = Y.TITLE.FUNCTION
    ); y4.n$plot
    do.save.plot(y4.n$plot, file.path(IMAGE.DIR, 'projection_new_ex4'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    y4.i = panel.b = plot.simulations.flex(list(base, noint, ex4), data.type='incidence', years=2010:2030,
                                           color.by='intervention', colors=INTERVENTION.COLORS[c('truth','base','ex4')],
                                           title.subplots=F, hide.legend=T,
                                           simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                           simulation.line.size=LINE.SIZE,
                                           text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                           return.change.data.frame=T,
                                           y.axis.title.function = Y.TITLE.FUNCTION
    ); y4.i$plot
    print(paste0('EX INTERVENTION 4: ', 
                 floor(-100*y4.i$change.df[2,3]), '% [',
                 floor(-100*y4.i$change.df[2,5]), ' to ',
                 floor(-100*y4.i$change.df[2,4]), '%]'))
    do.save.plot(y4.i$plot, file.path(IMAGE.DIR, 'projection_inc_ex4'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
}