
##-- SOURCE CODE --##
library(png)
library(ggsci)
source('code/visualization/plot_wrappers.R')

##-- WHERE YOU WANT TO SAVE THE IMAGES --##
IMAGE.DIR = '../Manuscripts/manuscript_1/Annals Submission/final fix/images/'

##-- STYLE SETTINGS --##

# image size
WIDTH = 3
HEIGHT = 1.75

# aesthetics
ALPHA = 1 #the shading of individual lines
LINE.SIZE = 1#0.3#0.4 for intervention
COLORS = pal_jama()
POINT.SIZE = 15
THEME = theme(legend.position='bottom', axis.title.x = element_blank(), 
              legend.margin=margin(t=-.125, r=0, b=-0.075, l=0, unit="in"),
              #  legend.text=element_text(size=7),
              legend.direction = 'horizontal',
              legend.spacing.x = unit(0, 'in'))#,
TEXT.SIZE = 36
LEGEND.TEXT.SIZE = TEXT.SIZE * 0.75

LABEL.SIZE = 26
LABEL.ALPHA = 0.25


INTERVENTION.COLORS = pal_jama()(4)


#INTERVENTIONS
if (1==2)
{
    load('mcmc_runs/quick_simsets/1.0_12580_full.Rdata')
    base = simset
 
    load('mcmc_runs/prep_simsets/12580/1.0_12580_noint.Rdata')
    noint = simset
    
    load('mcmc_runs/prep_simsets/12580/1.0_12580_msm.p10.oralinj.variable_23_27.Rdata')
    int1 = simset
    
    load('mcmc_runs/full_simsets/35620/1.0_35620_mi.t2x.p25.s90_23.27.Rdata')
    int2 = simset
}


#Do the plots
if (1==2)
{
    #-- 3B: Int1 x Reported --#
    x = panel.b = plot.simulations.flex(list(base, noint, int1), data.type='new', years=2010:2030,
                                        risks='msm',
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
                                    y=1000,
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
