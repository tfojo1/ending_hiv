
library(ggsci)
source('code/visualization/plot_wrappers.R')
IMAGE.DIR = 'results/figures'

ALPHA = 0.2
COLORS = pal_jama()
WIDTH = 3
HEIGHT = 1.75
POINT.SIZE = 2.5
THEME = theme(legend.position='bottom', axis.title.x = element_blank(), 
              legend.margin=margin(t=-.125, r=0, b=-0.075, l=0, unit="in"),
            #  legend.text=element_text(size=7),
              legend.direction = 'horizontal',
              legend.spacing.x = unit(0, 'in'))#,
          #    legend.text = element_text(size=8))

load('mcmc_runs/visualization_simsets/35620/1.0_35620_baseline.Rdata')

#INTERVENTIONS
if (1==2)
{
    load('mcmc_runs/visualization_simsets/35620/1.0_35620_baseline.Rdata')
    base = simset
    load('mcmc_runs/visualization_simsets/35620/1.0_35620_noint.Rdata')
    noint = simset
    load('mcmc_runs/visualization_simsets/35620/1.0_35620_ybhm.1.25.80.Rdata')
    int1 = simset
    load('mcmc_runs/visualization_simsets/35620/1.0_35620_mi.1.50.90.ybh.high.Rdata')
    int2 = simset
    
    INTERVENTION.COLORS = pal_jama()(4)
    THEME.NO.LEGEND = THEME = theme(legend.position='none', axis.title.x = element_blank(), 
                                    legend.margin=margin(t=-.125, r=0, b=-0.075, l=0, unit="in"),
                                    legend.direction = 'horizontal',
                                    legend.spacing.x = unit(0, 'in'))#,
    
    png(file.path(IMAGE.DIR, 'interventions/Figure 3a.png'), pointsize=10, width=WIDTH, height=HEIGHT, res=300, units='in')
    plot.simulations.flex(list(base, noint, int1), data.type='new', years=2000:2030,
                          color.by='intervention', colors=INTERVENTION.COLORS[1:3],
                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE) +
        THEME.NO.LEGEND
    dev.off() 
    
    png(file.path(IMAGE.DIR, 'interventions/Figure 3b.png'), pointsize=10, width=WIDTH, height=HEIGHT, res=300, units='in')
    plot.simulations.flex(list(base, noint, int1), data.type='incidence', years=2000:2030,
                          color.by='intervention', colors=INTERVENTION.COLORS[1:3],
                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE, 
                          label.change=T, label.change.size=2.7, label.change.nudge.x=-1.2) +
        THEME.NO.LEGEND + xlim(NA,2030.95)
    dev.off() 
    
    png(file.path(IMAGE.DIR, 'interventions/Figure 3c.png'), pointsize=10, width=WIDTH, height=HEIGHT, res=300, units='in')
    plot.simulations.flex(list(base, noint, int2), data.type='new', years=2000:2030,
                          color.by='intervention', colors=INTERVENTION.COLORS[c(1,2,4)],
                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE) +
        THEME.NO.LEGEND
    dev.off() 
    
    png(file.path(IMAGE.DIR, 'interventions/Figure 3d.png'), pointsize=10, width=WIDTH, height=HEIGHT, res=300, units='in')
    plot.simulations.flex(list(base, noint, int2), data.type='incidence', years=2000:2030,
                          color.by='intervention', colors=INTERVENTION.COLORS[c(1,2,4)],
                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE, 
                          label.change=T, label.change.size=2.7, label.change.nudge.x=-1.2) +
        THEME.NO.LEGEND + xlim(NA,2030.95)
    dev.off() 
}

#CALIBRATION 
if (1==2)
{
    # MARGINALS of RACE, RISK, AGE
    
    png(file.path(IMAGE.DIR, 'calibration/Figure 2a.png'), pointsize=10, width=WIDTH, height=HEIGHT, res=300, units='in')
    plot.simulations.flex(simset, split.by='race', data.type='new',
                          color.by='split', colors=COLORS,
                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE) +
        THEME
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'calibration/Figure 2b.png'), pointsize=10, width=WIDTH, height=HEIGHT, res=300, units='in')
    plot.simulations.flex(simset, split.by='race', data.type='prevalence',
                          color.by='split', colors=COLORS,
                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE) + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'calibration/Figure 2c.png'), pointsize=10, width=WIDTH, height=HEIGHT, res=300, units='in')
    plot.simulations.flex(simset, split.by='risk', data.type='new',
                          color.by='split', colors=COLORS,
                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE) +
        THEME# + guides(color = guide_legend(title.position="top", title.hjust = 0.5),
              #         size = guide_legend(title.position="top", title.hjust = 0.5),
               #        fill = guide_legend(title.position = 'tope', title.hjust = 0.5))
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'calibration/Figure 2d.png'), pointsize=10, width=WIDTH, height=HEIGHT, res=300, units='in')
    plot.simulations.flex(simset, split.by='risk', data.type='prevalence',
                          color.by='split', colors=COLORS,
                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE) +
        THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'calibration/Figure 2e.png'), pointsize=10, width=WIDTH, height=HEIGHT, res=300, units='in')
    plot.simulations.flex(simset, split.by='age', data.type='new',
                          color.by='split', colors=COLORS,
                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE) +
        THEME
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'calibration/Figure 2f.png'), pointsize=10, width=WIDTH, height=HEIGHT, res=300, units='in')
    plot.simulations.flex(simset, split.by='age', data.type='prevalence',
                          color.by='split', colors=COLORS,
                          simulation.alpha=ALPHA, truth.point.size=POINT.SIZE) +
        THEME
    dev.off()
    
    
    
    
    
    
    
    
    
    
    
    
    ##-- OLDER --##
    
    # RACE x RISK
    png(file.path(IMAGE.DIR, 'Figure 2a.png'), pointsize=10, width=3, height=1.75, res=300, units='in')
    plot.simulations.race.risk(simset, risk='msm', data.type='new', simulation.alpha=ALPHA, truth.point.size=2.5, truth.color='gray40', baseline.color='gray60') +
        theme(legend.position = 'none', axis.title.x = element_blank())
    dev.off()
    
    png(file.path(IMAGE.DIR, 'Figure 2b.png'), pointsize=10, width=3, height=1.75, res=300, units='in')
    plot.simulations.race.risk(simset, risk='msm', data.type='prevalence', simulation.alpha=ALPHA, truth.point.size=2.5, truth.color='gray40', baseline.color='gray60') +
        theme(legend.position = 'none', axis.title.x = element_blank())
    dev.off()
    
    png(file.path(IMAGE.DIR, 'Figure 2c.png'), pointsize=10, width=3, height=1.75, res=300, units='in')
    plot.simulations.race.risk(simset, risk='idu', data.type='new', simulation.alpha=ALPHA, truth.point.size=2.5, truth.color='gray40', baseline.color='gray60') +
        theme(legend.position = 'none', axis.title.x = element_blank())
    dev.off()
    
    png(file.path(IMAGE.DIR, 'Figure 2d.png'), pointsize=10, width=3, height=1.75, res=300, units='in')
    plot.simulations.race.risk(simset, risk='idu', data.type='prevalence', simulation.alpha=ALPHA, truth.point.size=2.5, truth.color='gray40', baseline.color='gray60') +
        theme(legend.position = 'none', axis.title.x = element_blank())
    dev.off()
    
    png(file.path(IMAGE.DIR, 'Figure 2e.png'), pointsize=10, width=3, height=1.75, res=300, units='in')
    plot.simulations.race.risk(simset, risk='msm_idu', data.type='new', simulation.alpha=ALPHA, truth.point.size=2.5, truth.color='gray40', baseline.color='gray60') +
        theme(legend.position = 'none', axis.title.x = element_blank())
    dev.off()
    
    png(file.path(IMAGE.DIR, 'Figure 2f.png'), pointsize=10, width=3, height=1.75, res=300, units='in')
    plot.simulations.race.risk(simset, risk='msm_idu', data.type='prevalence', simulation.alpha=ALPHA, truth.point.size=2.5, truth.color='gray40', baseline.color='gray60') +
        theme(legend.position = 'none', axis.title.x = element_blank())
    dev.off()
    
    png(file.path(IMAGE.DIR, 'Figure 2g.png'), pointsize=10, width=3, height=1.75, res=300, units='in')
    plot.simulations.race.risk(simset, risk='heterosexual', data.type='new', simulation.alpha=ALPHA, truth.point.size=2.5, truth.color='gray40', baseline.color='gray60') +
        theme(legend.position = 'none', axis.title.x = element_blank())
    dev.off()
    
    png(file.path(IMAGE.DIR, 'Figure 2h.png'), pointsize=10, width=3, height=1.75, res=300, units='in')
    plot.simulations.race.risk(simset, risk='heterosexual', data.type='new', simulation.alpha=ALPHA, truth.point.size=2.5, truth.color='gray40', baseline.color='gray60') +
        theme(legend.position = 'none', axis.title.x = element_blank())
    dev.off()
    
    
    
    png(file.path(IMAGE.DIR, 'Figure 2h.png'), pointsize=10, width=3, height=1.75, res=300, units='in')
    plot.simulations.flex(simset, split.by='risk', data.type='new', simulation.alpha=ALPHA, truth.point.size=2.5, truth.color='gray40', baseline.color='gray60') +
        theme(legend.position = 'none', axis.title.x = element_blank())
    dev.off()
    
    
    plot.simulations.flex(simset, split.by='risk', risk=c('idu','msm_idu','heterosexual'), data.type='new', simulation.alpha=ALPHA, truth.point.size=2.5, truth.color='gray40', baseline.color='gray60') +
        theme(legend.position = 'none', axis.title.x = element_blank())
    
    
    plot.simulations.total(simset)
    plot.simulations.flex(simset, split.by='race', color.by='split', split.colors=pal_jama())
    plot.simulations.flex(simset, split.by='risk', color.by='split', split.colors=pal_jama())
}
