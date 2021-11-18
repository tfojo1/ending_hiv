
library(png)
library(ggsci)
source('code/visualization/plot_wrappers.R')
source('code/targets/target_msas.R')
msa = BALTIMORE.MSA
IMAGE.DIR = paste0('../../../Talks/Ending HIV Model v1/Talks post Annals/plots/', msa)

ALPHA = 0.3
LINE.SIZE = 1#0.4 for intervention
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

# COLORS
COLORS = pal_jama()
INTERVENTION.COLORS = pal_jama()(4)

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
