
source('code/calibration/target_msas.R')
msa = SAN.DIEGO.MSA
source('code/processing/for_talks/talk_plot_settings.R')

DO.RENDER = T

COVID.COLORS = c(INTERVENTION.COLORS[1:2], COLORS(7)[3+c(3,1,2,4)])

THIN.TO = 100
THIN.TO.COVID = 100
COVID.YEARS = 2015:2025





# load up and save the simsets
if (1==2)
{
    source('code/source_code.R')
    
    #-- Load Simsets --#
    
    load(paste0('Q:/Ending_HIV/full_runs_from_annals/mcmc_runs/full_simsets/1.0_',msa,'_full.Rdata'))
    base = simset
    
    load(paste0('Q:/Ending_HIV/full_runs_from_annals/mcmc_runs/full_simsets/',msa,'/1.0_',msa,'_noint.Rdata'))
    noint = simset
    
    load(paste0('Q:/Ending_HIV/mcmc_runs/covid_simsets/',msa,'/1.0_',msa,'_covid.delayed.mobility.Rdata'))
    delayed = simset
    
    load(paste0('Q:/Ending_HIV/mcmc_runs/covid_simsets/',msa,'/1.0_',msa,'_covid.rapid.resumption.mobility.Rdata'))
    rapid = simset
    
    
    #-- Set the Masks --#
    
    bad.mask = delayed@parameters[,'suppression.reduction'] > 0.25 &
        delayed@parameters[,'sexual.transmission.reduction'] < 0.15
    bad.indices = sort(sample((1:length(bad.mask))[bad.mask], size=THIN.TO.COVID, replace=F))
    
    good.mask = delayed@parameters[,'suppression.reduction'] < 0.1 &
        delayed@parameters[,'sexual.transmission.reduction'] > 0.3
    good.indices = sort(sample((1:length(good.mask))[good.mask], size=THIN.TO.COVID, replace=F))
    
    
    
    #-- Parse by mask --#
    
    base.all = subset.simset(thin.simset(base, floor(simset@n.sim/THIN.TO)), 1:THIN.TO)
    base.bad = subset.simset(base, bad.indices)
    base.good = subset.simset(base, good.indices)
    
    noint.all = subset.simset(thin.simset(noint, floor(simset@n.sim/THIN.TO)), 1:THIN.TO)
    noint.bad = subset.simset(noint, bad.indices)
    noint.good = subset.simset(noint, good.indices)
    
    delayed.all = subset.simset(thin.simset(delayed, floor(simset@n.sim/THIN.TO.COVID)), 1:THIN.TO.COVID)
   # dummy.int1 = create.intervention(WHOLE.POPULATION, create.intervention.unit('suppression', 2020, rates=0.5, 2021))
   # attr(delayed.all, 'intervention') = dummy.int1
    
    delayed.bad = subset.simset(delayed, bad.indices)
#    dummy.int2 = create.intervention(WHOLE.POPULATION, create.intervention.unit('suppression', 2020, rates=0.25, 2021))
 #   attr(delayed.bad, 'intervention') = dummy.int2
    
    delayed.good = subset.simset(delayed, good.indices)
  #  dummy.int3 = create.intervention(WHOLE.POPULATION, create.intervention.unit('suppression', 2020, rates=0.25, 2021))
   # attr(delayed.good, 'intervention') = dummy.int3
    
    rapid.bad = subset.simset(rapid, bad.indices)
#    dummy.int4 = create.intervention(WHOLE.POPULATION, create.intervention.unit('suppression', 2020, rates=0.33, 2021))
#    attr(rapid.bad, 'intervention') = dummy.int4
    
    
    save(base.all, base.bad, base.good,
         noint.all, noint.bad, noint.good, 
         delayed.all, delayed.bad, delayed.good,
         rapid.bad,
         file=paste0('tmp/covid_simsets_for_talk_plots_',msa,'.Rdata'))
}

#RUN ON LAPTOP - for making plots

if (!is.desktop && !exists('base.all'))
{
    x=load(file=paste0('tmp/covid_simsets_for_talk_plots_',msa,'.Rdata'))
}


if (!is.desktop && DO.RENDER)
{
    print("*** RENDERING ***")
    # All - individual lines
    x.n = plot.simulations.flex(list(base.all, noint.all, delayed.all), data.type='new', years=COVID.YEARS,
                                color.by='intervention', colors=COVID.COLORS[1:3],
                                title.subplots=F, hide.legend=T,
                                simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                simulation.line.size=LINE.SIZE,
                                text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                return.change.data.frame=T,
                                change.years=c(2020,2025),
                                change.statistic='cumulative.diff.relative',
                              #  plot.format='mean.and.interval',
                              label.axis.ci=F,
                                y.axis.title.function = Y.TITLE.FUNCTION
    ); x.n$plot
    
    do.save.plot(x.n$plot, file.path(COVID.IMAGE.DIR, 'projection_new_delayed.all_lines'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    x.i = plot.simulations.flex(list(base.all, noint.all, delayed.all), data.type='incidence', years=COVID.YEARS,
                                color.by='intervention', colors=COVID.COLORS[1:3],
                                title.subplots=F, hide.legend=T,
                                simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                simulation.line.size=LINE.SIZE,
                                text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                return.change.data.frame=T,
                                change.years=c(2020,2025),
                                change.statistic='cumulative.diff.absolute',
                             #   plot.format='mean.and.interval',
                             label.axis.ci=F,
                             y.axis.title.function = Y.TITLE.FUNCTION
    ); x.i$plot
    do.save.plot(x.i$plot, file.path(COVID.IMAGE.DIR, 'projection_inc_delayed.all_lines'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    # All
    x.n = plot.simulations.flex(list(base.all, noint.all, delayed.all), data.type='new', years=COVID.YEARS,
                                color.by='intervention', colors=COVID.COLORS[1:3],
                                title.subplots=F, hide.legend=T,
                                simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                simulation.line.size=LINE.SIZE,
                                text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                return.change.data.frame=T,
                                change.years=c(2020,2025),
                                change.statistic='cumulative.diff.relative',
                                plot.format='mean.and.interval',
                                label.axis.ci=F,
                                y.axis.title.function = Y.TITLE.FUNCTION
    ); x.n$plot
    
    do.save.plot(x.n$plot, file.path(COVID.IMAGE.DIR, 'projection_new_delayed.all'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    x.i = plot.simulations.flex(list(base.all, noint.all, delayed.all), data.type='incidence', years=COVID.YEARS,
                                          color.by='intervention', colors=COVID.COLORS[1:3],
                                          title.subplots=F, hide.legend=T,
                                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                          simulation.line.size=LINE.SIZE,
                                          text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                          return.change.data.frame=T,
                                          change.years=c(2020,2025),
                                          change.statistic='cumulative.diff.absolute',
                                          plot.format='mean.and.interval',
                                label.axis.ci=F,
                                y.axis.title.function = Y.TITLE.FUNCTION
    ); x.i$plot
    print(paste0('ALL, ABSENT COVID: ', 
                 format(ceiling(x.i$change.df[1,6]),big.mark=','), ' cases [',
                 format(ceiling(x.i$change.df[1,7]),big.mark=','), ' to ',
                 format(ceiling(x.i$change.df[1,8]),big.mark=','), ']'))
    print(paste0('ALL, WITH COVID: ', 
                 format(ceiling(abs(x.i$change.df[2,3])),big.mark=','), ' ', 
                 "(", ceiling(100*abs(x.i$change.df[2,3])/x.i$change.df[1,6]), "%) ",
                 ifelse(x.i$change.df[2,3]<0, 'fewer', 'more'),
                 ' cases [',
                 
                 format(ceiling(abs(x.i$change.df[2,4])),big.mark=','), ' ',
                 ifelse(x.i$change.df[2,4]<0, 'fewer', 'more'),
                 ' to ',
                 
                 format(ceiling(abs(x.i$change.df[2,5])),big.mark=','), ' ',
                 ifelse(x.i$change.df[2,5]<0, 'fewer', 'more'), 
                 
                 ']'))
    
    do.save.plot(x.i$plot, file.path(COVID.IMAGE.DIR, 'projection_inc_delayed.all'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    #-- Bad --#
    x.n = plot.simulations.flex(list(base.bad, noint.bad, delayed.bad), data.type='new', years=COVID.YEARS,
                                color.by='intervention', colors=COVID.COLORS[c(1,2,4)],
                                title.subplots=F, hide.legend=T,
                                simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                simulation.line.size=LINE.SIZE,
                                text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                return.change.data.frame=T,
                                change.years=c(2020,2025),
                                change.statistic='cumulative.diff.relative',
                                plot.format='mean.and.interval',
                                label.axis.ci=F,
                                y.axis.title.function = Y.TITLE.FUNCTION
    ); x.n$plot
    
    do.save.plot(x.n$plot, file.path(COVID.IMAGE.DIR, 'projection_new_delayed.bad'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    x.i = plot.simulations.flex(list(base.bad, noint.bad, delayed.bad), data.type='incidence', years=COVID.YEARS,
                                color.by='intervention', colors=COVID.COLORS[c(1,2,4)],
                                title.subplots=F, hide.legend=T,
                                simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                simulation.line.size=LINE.SIZE,
                                text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                return.change.data.frame=T,
                                change.years=c(2020,2025),
                                change.statistic='cumulative.diff.absolute',
                                plot.format='mean.and.interval',
                                label.axis.ci=F,
                                y.axis.title.function = Y.TITLE.FUNCTION
    ); x.i$plot
    print(paste0('ALL, WITH COVID: ', 
                 format(ceiling(abs(x.i$change.df[2,3])),big.mark=','), ' ', 
                 "(", ceiling(100*abs(x.i$change.df[2,3])/x.i$change.df[1,6]), "%) ",
                 ifelse(x.i$change.df[2,3]<0, 'fewer', 'more'),
                 ' cases [',
                 
                 format(ceiling(abs(x.i$change.df[2,4])),big.mark=','), ' ',
                 ifelse(x.i$change.df[2,4]<0, 'fewer', 'more'),
                 ' to ',
                 
                 format(ceiling(abs(x.i$change.df[2,5])),big.mark=','), ' ',
                 ifelse(x.i$change.df[2,5]<0, 'fewer', 'more'), 
                 
                 ']'))
    do.save.plot(x.i$plot, file.path(COVID.IMAGE.DIR, 'projection_inc_delayed.bad'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    #-- Good --#
    x.n = plot.simulations.flex(list(base.good, noint.good, delayed.good), data.type='new', years=COVID.YEARS,
                                color.by='intervention', colors=COVID.COLORS[c(1,2,5)],
                                title.subplots=F, hide.legend=T,
                                simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                simulation.line.size=LINE.SIZE,
                                text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                return.change.data.frame=T,
                                change.years=c(2020,2025),
                                change.statistic='cumulative.diff.relative',
                                plot.format='mean.and.interval',
                                label.axis.ci=F,
                                y.axis.title.function = Y.TITLE.FUNCTION
    ); x.n$plot
    
    do.save.plot(x.n$plot, file.path(COVID.IMAGE.DIR, 'projection_new_delayed.good'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    x.i = plot.simulations.flex(list(base.good, noint.good, delayed.good), data.type='incidence', years=COVID.YEARS,
                                color.by='intervention', colors=COVID.COLORS[c(1,2,5)],
                                title.subplots=F, hide.legend=T,
                                simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                simulation.line.size=LINE.SIZE,
                                text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                return.change.data.frame=T,
                                change.years=c(2020,2025),
                                change.statistic='cumulative.diff.absolute',
                                plot.format='mean.and.interval',
                                label.axis.ci=F,
                                y.axis.title.function = Y.TITLE.FUNCTION
    ); x.i$plot
    print(paste0('ALL, WITH COVID: ', 
                 format(ceiling(abs(x.i$change.df[2,3])),big.mark=','), ' ', 
                 "(", ceiling(100*abs(x.i$change.df[2,3])/x.i$change.df[1,6]), "%) ",
                 ifelse(x.i$change.df[2,3]<0, 'fewer', 'more'),
                 ' cases [',
                 
                 format(ceiling(abs(x.i$change.df[2,4])),big.mark=','), ' ',
                 ifelse(x.i$change.df[2,4]<0, 'fewer', 'more'),
                 ' to ',
                 
                 format(ceiling(abs(x.i$change.df[2,5])),big.mark=','), ' ',
                 ifelse(x.i$change.df[2,5]<0, 'fewer', 'more'), 
                 
                 ']'))
    do.save.plot(x.i$plot, file.path(COVID.IMAGE.DIR, 'projection_inc_delayed.good'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    #-- Vs Rapid - bad --#    
    x.n = plot.simulations.flex(list(base.bad, noint.bad, rapid.bad), data.type='incidence', years=COVID.YEARS,
                                color.by='intervention', colors=COVID.COLORS[c(1,2,6)],
                                title.subplots=F, hide.legend=T,
                                simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                simulation.line.size=LINE.SIZE,
                                text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                return.change.data.frame=T,
                                change.years=c(2020,2025),
                                change.statistic='cumulative.diff.relative',
                                plot.format='mean.and.interval',
                                label.axis.ci=F,
                                y.axis.title.function = Y.TITLE.FUNCTION
    ); x.n$plot
    do.save.plot(x.n$plot, file.path(COVID.IMAGE.DIR, 'projection_new_rapid.bad'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
    x.i = plot.simulations.flex(list(base.bad, noint.bad, rapid.bad), data.type='incidence', years=COVID.YEARS,
                                color.by='intervention', colors=COVID.COLORS[c(1,2,6)],
                                title.subplots=F, hide.legend=T,
                                simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                                simulation.line.size=LINE.SIZE,
                                text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                return.change.data.frame=T,
                                change.years=c(2020,2025),
                                change.statistic='cumulative.diff.absolute',
                                plot.format='mean.and.interval',
                                label.axis.ci=F,
                                y.axis.title.function = Y.TITLE.FUNCTION
    ); x.i$plot
    do.save.plot(x.i$plot, file.path(COVID.IMAGE.DIR, 'projection_inc_rapid.bad'),
                 width=WIDTH, height=HEIGHT, resolution = 300)
    
    
}

#heat map
# need to source make figures
if (1==2)
{
    source("code/covid/make_figures.R")
    png(file.path(COVID.IMAGE.DIR, 'heat_map_sex_suppression_raw.png'), 
        pointsize=PNG.POINT.SIZE, width=HEAT.MAP.PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    do.make.covid.heat.map('12580', scenario='delayed.hiv.care')
    dev.off()
}