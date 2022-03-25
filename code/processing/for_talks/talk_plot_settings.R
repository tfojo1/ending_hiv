
library(png)
library(ggsci)
source('code/processing/visualization/plot_wrappers.R')
source('code/calibration/target_msas.R')


if (!exists('msa'))
    stop("Need to set 'msa'")

print('*****************************')
print(paste0("** MSA is ", msa, " (", msa.names(msa), ") **"))

is.desktop = dir.exists('Q:Ending_HIV')
if (is.desktop)
    print("** Running on DESKTOP **")
if (!is.desktop)
    print("** Running on LAPTOP **")

print('*****************************')
print('')

IMAGE.DIR = paste0('../../../Talks/Ending HIV Model v1/Talks post Annals/plots/', msa)
if (!dir.exists(IMAGE.DIR))
    dir.create(IMAGE.DIR)
    
COVID.IMAGE.DIR = file.path(IMAGE.DIR, 'covid')
if (!dir.exists(COVID.IMAGE.DIR))
    dir.create(COVID.IMAGE.DIR)


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

#Y.TITLE.FUNCTION = function(d){paste0("Total ", DATA.TYPE.AXIS.LABELS[d])}
Y.TITLE.FUNCTION = function(d){DATA.TYPE.AXIS.LABELS[d]}

#load('mcmc_runs/visualization_simsets/35620/1.0_35620_baseline.Rdata')


THIN.TO = 100

# COLORS
COLORS = pal_jama()
INTERVENTION.COLORS = pal_jama()(7)
INTERVENTION.COLORS = INTERVENTION.COLORS[c(1:4,3,5:7)]

names(INTERVENTION.COLORS) = c(
    'truth',
    'base',
    'int1',
    'int2',
    'ex1',
    'ex2',
    'ex3',
    'ex4'
)

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
