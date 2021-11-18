
##-- SET-UP --##
IMAGE.DIR = '../Manuscripts/covid_manuscript/images'
PALETTE = pal_jama()
COLORS = PALETTE(5)
names(COLORS) = c('baseline',
                  'base',
                  'delayed.hiv.care',
                  'rebound.sex',
                  'rebound.sex.delayed.hiv.care')


PANEL.WIDTH = 3
PANEL.HEIGHT = 2
PNG.POINT.SIZE = 5
RES = 600

LINE.SIZE = 1

THEME = theme(text = element_text(size=8), legend.direction = 'horizontal',
              legend.position = 'none')

##-- SOURCE CODE --##
if (1==2)
{
    source('code/covid/covid_plots.R')
    load('code/covid/results/covid_4.0_results.Rdata')

}

##-- FIGURE 1 --##
if (1==2)
{    
    png(file.path(IMAGE.DIR, 'figure1/base_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='base', outcomes='incidence', line.size = LINE.SIZE,
                                    colors = COLORS[c('baseline','base')]) + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'figure1/base_new.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='base', outcomes='new', line.size = LINE.SIZE,
                                    colors = COLORS[c('baseline','base')]) + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'figure1/base_cum_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='base', outcomes='incidence',
                                    cumulative=T, subtract.scenario = 'baseline', include.baseline = F,
                                    colors = COLORS[c('base')]) + THEME +
        ylab("Change in Incident Cases (n)")
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'figure1/delayed_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='delayed.hiv.care', outcomes='incidence', line.size = LINE.SIZE,
                             colors = COLORS[c('baseline','delayed.hiv.care')]) + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'figure1/delayed_new.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='delayed.hiv.care', outcomes='new', line.size = LINE.SIZE,
                             colors = COLORS[c('baseline','delayed.hiv.care')]) + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'figure1/delayed_cum_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='delayed.hiv.care', outcomes='incidence',
                             cumulative=T, subtract.scenario = 'baseline', include.baseline = F,
                             colors = COLORS[c('delayed.hiv.care')]) + THEME +
        ylab("Change in Incident Cases (n)")
    dev.off()
    
    
    
    png(file.path(IMAGE.DIR, 'figure1/rebound_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='rebound.sexual.transmission', outcomes='incidence', line.size = LINE.SIZE,
                             colors = COLORS[c('baseline','rebound.sex')]) + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'figure1/rebound_new.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='rebound.sexual.transmission', outcomes='new', line.size = LINE.SIZE,
                             colors = COLORS[c('baseline','rebound.sex')]) + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'figure1/rebound_cum_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='rebound.sexual.transmission', outcomes='incidence',
                             cumulative=T, subtract.scenario = 'baseline', include.baseline = F,
                             colors = COLORS[c('rebound.sex')]) + THEME +
        ylab("Change in Incident Cases (n)")
    dev.off()
    
    
    
    png(file.path(IMAGE.DIR, 'figure1/rebound_delayed_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='rebound.sex.delayed.hiv.care', outcomes='incidence', line.size = LINE.SIZE,
                             colors = COLORS[c('baseline','rebound.sex.delayed.hiv.care')]) + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'figure1/rebound_delayed_new.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='rebound.sex.delayed.hiv.care', outcomes='new', line.size = LINE.SIZE,
                             colors = COLORS[c('baseline','rebound.sex.delayed.hiv.care')]) + THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'figure1/rebound_delayed_cum_inc.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.timeline.plot(scenarios='rebound.sex.delayed.hiv.care', outcomes='incidence',
                             cumulative=T, subtract.scenario = 'baseline', include.baseline = F,
                             colors = COLORS[c('rebound.sex.delayed.hiv.care')]) + THEME +
        ylab("Change in Incident Cases (n)")
    dev.off()
    
    
    
    
    make.covid.timeline.plot(scenarios='delayed.hiv.care', outcomes='new',
                             cumulative=T, subtract.scenario = 'baseline', include.baseline = F,
                             colors = COLORS[c('delayed.hiv.care')]) + THEME +
        ylab("Change in Incident Cases (n)")
    make.covid.timeline.plot(scenarios='base', outcomes='new',
                             cumulative=T, subtract.scenario = 'baseline', include.baseline = F,
                             colors = COLORS[c('base')]) + THEME +
        ylab("Change in Incident Cases (n)")
    
    make.covid.timeline.plot(scenarios=c('base','delayed.hiv.care'), outcomes='incidence', line.size = LINE.SIZE,
                             colors = COLORS[c('baseline','base','delayed.hiv.care')]) + THEME
    
    
    
    make.covid.timeline.plot(scenarios=c('delayed.hiv.care','rebound.sexual.transmission','rebound.sex.delayed.hiv.care'), 
                             outcomes='incidence',
                             cumulative=T, subtract.scenario = 'base', include.baseline = F
    ) +
        ylab("Change in Incident Cases (n)")
}

##-- STACKED PLOTS --##
STACKED.LINE.SIZE = 0.5
if (1==2)
{
    png(file.path(IMAGE.DIR, 'stacked/stacked_three.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.stacked.timeline.plot(cumulative=T, 
                               subtract.scenario = 'base', 
                               scenarios=rev(c('rebound.sexual.transmission',
                                               'delayed.hiv.care',
                                               'rebound.sex.delayed.hiv.care')),
                               colors = COLORS[c('rebound.sex','delayed.hiv.care','rebound.sex.delayed.hiv.care')],
                               line.size=STACKED.LINE.SIZE,
                               fill.alpha=0.9) + THEME
    dev.off()
    
    make.stacked.timeline.plot(cumulative=T, 
                               subtract.scenario = 'baseline', 
                               scenarios=rev(c('rebound.sexual.transmission',
                                               'delayed.hiv.care',
                                               'rebound.sex.delayed.hiv.care')),
                               colors = COLORS[c('rebound.sex','delayed.hiv.care','rebound.sex.delayed.hiv.care')],
                               line.size=STACKED.LINE.SIZE,
                               fill.alpha=1)
    
    make.covid.timeline.plot(cumulative=T, 
                               subtract.scenario = 'baseline', 
                               scenarios=rev(c('rebound.sexual.transmission',
                                               'delayed.hiv.care',
                                               'rebound.sex.delayed.hiv.care')),
                               colors = COLORS[c('rebound.sex','delayed.hiv.care','rebound.sex.delayed.hiv.care')],
                               line.size=STACKED.LINE.SIZE,
                             include.baseline=F,
                               ribbon.alpha=0.25)
    
    make.stacked.timeline.plot(cumulative=T, 
                               outcomes = 'incidence',
                               subtract.scenario = 'baseline', 
                               scenarios=rev(c('base',
                                               'rebound.sexual.transmission',
                                               'delayed.hiv.care',
                                               'rebound.sex.delayed.hiv.care')),
                               colors = COLORS[c('base','rebound.sex','delayed.hiv.care','rebound.sex.delayed.hiv.care')],
                               line.size=STACKED.LINE.SIZE,
                               fill.alpha=1)
}

##-- HEAT MAPS --##
HEAT.MAP.RANGE = c(-.25,.25)
HEAT.MAP.PANEL.HEIGHT = 2.5
if (1==2)
{
    png(file.path(IMAGE.DIR, 'heat_maps/base_sex_suppression.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    make.covid.heat.map(var1 = 'sexual.transmission.reduction',
                        var2 = 'suppression.reduction',
                        min.change = HEAT.MAP.RANGE[1],
                        max.change = HEAT.MAP.RANGE[2],
                        scenario='base',
                        color.scale.title = "Change in Cumulative Incidence 2020-2025") + 
        xlab("Reduction in Sexual Transmission") + ylab("Reduction in Viral Suppression") + THEME
    dev.off()
    
    
    png(file.path(IMAGE.DIR, 'heat_maps/delayed_sex_suppression.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=HEAT.MAP.PANEL.HEIGHT, res=RES, units='in')
    make.covid.heat.map(var1 = 'sexual.transmission.reduction',
                        var2 = 'suppression.reduction',
                        min.change = HEAT.MAP.RANGE[1],
                        max.change = HEAT.MAP.RANGE[2],
                        scenario='delayed.hiv.care',
                        color.scale.title = "Change in Cumulative Incidence 2020-2025") + 
        xlab("Reduction in Sexual Transmission") + ylab("Reduction in Viral Suppression") + THEME
    dev.off()
}

##-- SCATTERPLOTS --##
SCATTER.POINT.SIZE = 1
SCATTER.RHO.SIZE = 2
SCATTER.THEME = theme(text = element_text(size=7),
                       axis.title = element_text(size=5.5),
                       legend.position = 'none')
if (1==2)
{
    png(file.path(IMAGE.DIR, 'scatters/base_sex.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(var1='sexual.transmission.reduction', 
                           point.size=SCATTER.POINT.SIZE, label.rho.size = SCATTER.RHO.SIZE) + SCATTER.THEME +
        xlab("Reduction in Sexual Transmission") + ylab("Change in Cumulative Incidence 2020-2025\n(Relative to Projections Absent COVID)")
    dev.off()
    
    png(file.path(IMAGE.DIR, 'scatters/base_suppression.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(var1='suppression.reduction', 
                           point.size=SCATTER.POINT.SIZE, label.rho.vjust = 'bottom', label.rho.size = SCATTER.RHO.SIZE) + SCATTER.THEME +
        xlab("Reduction in Viral Suppression") + ylab("Change in Cumulative Incidence 2020-2025\n(Relative to Projections Absent COVID)")
    dev.off()
    
    png(file.path(IMAGE.DIR, 'scatters/base_testing.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(var1='testing.reduction', 
                           point.size=SCATTER.POINT.SIZE, label.rho.vjust = 'bottom', label.rho.size = SCATTER.RHO.SIZE) + SCATTER.THEME +
        xlab("Reduction in HIV Testing") + ylab("Change in Cumulative Incidence 2020-2025\n(Relative to Projections Absent COVID)")
    dev.off()
    
    png(file.path(IMAGE.DIR, 'scatters/base_prep.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(var1='prep.reduction', 
                           point.size=SCATTER.POINT.SIZE, label.rho.vjust = 'bottom', label.rho.size = SCATTER.RHO.SIZE) + SCATTER.THEME +
        xlab("Reduction in PrEP Use") + ylab("Change in Cumulative Incidence 2020-2025\n(Relative to Projections Absent COVID)")
    dev.off()
    
    
    
    
    make.covid.scatterplot(var1='sexual.transmission.reduction', point.size=SCATTER.POINT.SIZE,
                           scenario1='delayed.hiv.care') + 
        xlab("Reduction in Sexual Transmission") + ylab("Change in Cumulative Incidence 2020-2025\n(Relative to Projections Absent COVID)")
    make.covid.scatterplot(var1='suppression.reduction', point.size=SCATTER.POINT.SIZE,
                           scenario1='delayed.hiv.care') + 
        xlab("Reduction in Viral Suppression") + ylab("Change in Cumulative Incidence 2020-2025\n(Relative to Projections Absent COVID)")
    make.covid.scatterplot(var1='testing.reduction', point.size=SCATTER.POINT.SIZE,
                           scenario1='delayed.hiv.care') + 
        xlab("Reduction in HIV Testing") + ylab("Change in Cumulative Incidence 2020-2025\n(Relative to Projections Absent COVID)")
    make.covid.scatterplot(var1='prep.reduction', point.size=SCATTER.POINT.SIZE,
                           scenario1='delayed.hiv.care') + 
        xlab("Reduction in PrEP Use") + ylab("Change in Cumulative Incidence 2020-2025\n(Relative to Projections Absent COVID)")
    
    make.covid.scatterplot(var1='sexual.transmission.reduction', point.size=SCATTER.POINT.SIZE,
                           scenario1='rebound.sexual.transmission') + 
        xlab("Reduction in Sexual Transmission") + ylab("Change in Cumulative Incidence 2020-2025\n(Relative to Projections Absent COVID)")
    make.covid.scatterplot(var1='sexual.transmission.increase', point.size=SCATTER.POINT.SIZE,
                           scenario1='rebound.sexual.transmission') + 
        xlab("Rebound in Sexual Transmission") + ylab("Change in Cumulative Incidence 2020-2025\n(Relative to Projections Absent COVID)")
    make.covid.scatterplot(var1='suppression.reduction', point.size=SCATTER.POINT.SIZE,
                           scenario1='delayed.hiv.care') + 
        xlab("Reduction in Viral Suppression") + ylab("Change in Cumulative Incidence 2020-2025\n(Relative to Projections Absent COVID)")
    make.covid.scatterplot(var1='testing.reduction', point.size=SCATTER.POINT.SIZE,
                           scenario1='delayed.hiv.care') + 
        xlab("Reduction in HIV Testing") + ylab("Change in Cumulative Incidence 2020-2025\n(Relative to Projections Absent COVID)")
    make.covid.scatterplot(var1='prep.reduction', point.size=SCATTER.POINT.SIZE,
                           scenario1='delayed.hiv.care') + 
        xlab("Reduction in PrEP Use") + ylab("Change in Cumulative Incidence 2020-2025\n(Relative to Projections Absent COVID)")
    
}


##-- CITY-SPECIFIC CORRELATIONS --##
CITY.COR.SIZE.RANGE = c(1,3)
CITY.COR.THEME = theme(text = element_text(size=7),
                       axis.title = element_text(size=5.5),
                       legend.position = 'none')
CITY.COR.PANEL.HEIGHT = 2.5
if (1==2)
{
    png(file.path(IMAGE.DIR, 'city_correlations/suppression.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=CITY.COR.PANEL.HEIGHT, res=RES, units='in')
    make.correlation.scatterplot(var1='suppression', var1.year=2019, correlate.var1 = F,
                                 var2='suppression.reduction', 
                                 scenario='base',
                                 point.size.range = CITY.COR.SIZE.RANGE,
                                 label.locations = character()) + #xlim(0,1) + ylim(0,1) +
        ylab("Correlation Between Reduction in Viral Suppression\nand Cumulative Incidence 2020-2025") +
        xlab("Mean Proportion Suppressed in 2019") + CITY.COR.THEME
    dev.off()
    
    png(file.path(IMAGE.DIR, 'city_correlations/sexual_transmission.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=CITY.COR.PANEL.HEIGHT, res=RES, units='in')
    make.correlation.scatterplot(var1='incidence.prevalence.ratio', var1.year=2019, correlate.var1 = F,
                                 var2='sexual.transmission.reduction', 
                                 scenario='base',
                                 point.size.range = CITY.COR.SIZE.RANGE) +
        ylab("Correlation Between Reduction in Sexual Transmission\nand Cumulative Incidence 2020-2025") +
        xlab("Mean Incidence-Prevalence Ratio in 2019") + CITY.COR.THEME
    dev.off()
}

##-- REPORTED vs. INCIDENCE --##
if (1==2)
{
    png(file.path(IMAGE.DIR, 'new_vs_inc/base_2020.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(var1='new', var1.years = 2020,
                           var2='incidence', var2.years = 2020:2025,
                           subtract.intervention.name1 = 'baseline',
                           add.smoother = T,
                           point.size=SCATTER.POINT.SIZE, label.rho.size = SCATTER.RHO.SIZE) + SCATTER.THEME +
        xlab("Reported Diagnoses in 2020") + ylab("Change in Cumulative Incidence 2020-2025\n(Relative to Projections Absent COVID)")
    dev.off()
    
    png(file.path(IMAGE.DIR, 'new_vs_inc/base_2021.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(var1='new', var1.years = 2021,
                           var2='incidence', var2.years = 2020:2025,
                           subtract.intervention.name1 = 'baseline',
                           add.smoother = T,
                           label.rho.vjust = 'bottom',
                           point.size=SCATTER.POINT.SIZE, label.rho.size = SCATTER.RHO.SIZE) + SCATTER.THEME +
        xlab("Reported Diagnoses in 2021") + ylab("Change in Cumulative Incidence 2020-2025\n(Relative to Projections Absent COVID)")
    dev.off()
    
    png(file.path(IMAGE.DIR, 'new_vs_inc/delayed_2020.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(var1='new', var1.years = 2020,
                           scenario1 = 'delayed.hiv.care',
                           var2='incidence', var2.years = 2020:2025,
                           subtract.intervention.name1 = 'baseline',
                           add.smoother = T,
                           point.size=SCATTER.POINT.SIZE, label.rho.size = SCATTER.RHO.SIZE) + SCATTER.THEME +
        xlab("Reported Diagnoses in 2020") + ylab("Change in Cumulative Incidence 2020-2025\n(Relative to Projections Absent COVID)")
    dev.off()
    
    png(file.path(IMAGE.DIR, 'new_vs_inc/delayed_2021.png'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=PANEL.HEIGHT, res=RES, units='in')
    make.covid.scatterplot(var1='new', var1.years = 2021,
                           scenario1 = 'delayed.hiv.care',
                           var2='incidence', var2.years = 2020:2025,
                           subtract.intervention.name1 = 'baseline',
                           add.smoother = T,
                           label.rho.vjust = 'bottom',
                           point.size=SCATTER.POINT.SIZE, label.rho.size = SCATTER.RHO.SIZE) + SCATTER.THEME +
        xlab("Reported Diagnoses in 2021") + ylab("Change in Cumulative Incidence 2020-2025\n(Relative to Projections Absent COVID)")
    dev.off()
    
    
    
    
    
    png(file.path(IMAGE.DIR, 'city_correlations/x'), pointsize=PNG.POINT.SIZE, width=PANEL.WIDTH, height=CITY.COR.PANEL.HEIGHT, res=RES, units='in')
    make.correlation.scatterplot(var1='suppression', var1.year=2019, correlate.var1 = F,
                                 var2='new', var2.year = 2021,
                                 scenario='delayed.hiv.care',
                                 point.size.range = CITY.COR.SIZE.RANGE) +
        ylab("Correlation Between Reduction in Sexual Transmission\nand Cumulative Incidence 2020-2025") +
        xlab("Mean Incidence-Prevalence Ratio in 2019") + CITY.COR.THEME
    dev.off()
}