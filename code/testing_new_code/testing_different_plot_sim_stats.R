
source('code/visualization/plot_wrappers.R')

load('mcmc_runs/visualization_simsets/12580/1.0_12580_baseline.Rdata')
baseline = simset
load('mcmc_runs/visualization_simsets/12580/1.0_12580_noint.Rdata')
noint = simset
load('mcmc_runs/visualization_simsets/12580/1.0_12580_all.p25_23.25.Rdata')
int1 = simset

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

INTERVENTION.COLORS = pal_jama()(4)

source('../jheem_interactive/shiny/model_code/plot_simulations.R')
source('code/visualization/plot_simulations.R')

x = plot.simulations.flex(list(baseline, noint, int1), 
                          data.type=c('new','diagnosed'), 
                          years=2010:2030,
                                    color.by='intervention', colors=INTERVENTION.COLORS[1:3],
                          linetype.by='intervention', linetypes=c('solid','dash'),
                          plot.format = 'mean.and.interval', label.axis.ci=F,
                          title.subplots=F, hide.legend=T,
                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE,
                          simulation.line.size=LINE.SIZE,
                          change.statistic='cumulative.diff.relative',
                          label.change.abbreviated=F,
                          text.size=TEXT.SIZE, legend.text.size = LEGEND.TEXT.SIZE,
                                    return.change.data.frame=T, label.change=T); x$plot; make.pretty.change.data.frame(x$change.df)

x$change.df
make.pretty.change.data.frame(x$change.df)


ORIG = x
