library(ggplot2)
library(ggmap)

if (1==2)
{
    png(filename='../Manuscripts/manuscript_1/Annals Submission/video/map.png',
        height = 1200, width=2400, res = 300)
    plot.us.hiv.map(TARGET.MSAS) + theme(legend.position = 'none')
    dev.off()
}


MAP.LABELS = c(
    prevalence='HIV Prevalence',
    new='Reported HIV Diagnoses'
)

plot.us.hiv.map <- function(locations,
                         surv=msa.surveillance,
                         size.by='new',
                         size.by.year=2018,
                         size.range=c(3,7),
                         color.by=NULL,
                         color.by.year=2018,
                         color.range='red',
                         size.label=paste0(size.by.year, ' ', MAP.LDABELS[size.by]),
                         color.label=paste0(color.by.year, ' ', MAP.LABELS[size.by]),
                         alpha=0.5
)
{
    lat.long = get.location.latitude.longitude(locations)
    if (is.null(size.by))
        size = size.range
    else
        size = get.surveillance.data(surv, location.codes = locations, data.type=size.by, years=size.by.year,
                                     aggregate.locations = F, aggregate.years=T)
    
    if (is.null(color.by))
        color = color.range
    else
        color = get.surveillance.data(surv, location.codes = locations, data.type=color.by, years=color.by.year,
                                      aggregate.locations = F, aggregate.years=T)
    
    plot.us.map(latitude = lat.long$latitude,
                longitude = lat.long$longitude,
                size=size,
                color=color,
                size.range = if(is.null(size.by)) NULL else size.range,
                color.range= if(is.null(color.by)) NULL else color.range,
                size.label = size.label,
                color.label = color.label,
                alpha=alpha)
}

##--------------------------------------------------##
##-- THE GENERAL PLOT FUNCTION AND NECESSARY DATA --##
##--------------------------------------------------##

US.MAP = get_stamenmap(bbox=c(left=-125,bottom=24, right=-66, top=50),zoom=4, maptype='toner-background')
attr_map <- attr(US.MAP, "bb")    # save attributes from original

# change color in raster
US.MAP[US.MAP == "#000000"] <- "#C0C0C0"

# correct class, attributes
class(US.MAP) <- c("ggmap", "raster")
attr(US.MAP, "bb") <- attr_map

plot.us.map <- function(latitude,
                        longitude,
                        size=1,
                        color=1,
                        size.range=c(1,5),
                        color.range=c('blue', 'red'),
                        pch=21,
                        size.label='',
                        color.label='',
                        alpha=1)
{
    df = data.frame(lon=longitude,
                    lat=latitude,
                    size=size,
                    color=color)
    
    rv = ggmap(US.MAP) + 
        geom_point(data=df, aes(lon, lat, size=size, fill=color), shape=pch, alpha=alpha) +
        theme(panel.background = element_rect(fill='white'), axis.ticks = element_blank(), 
              axis.text = element_blank(), axis.title = element_blank())
    
    if (!is.null(size.range))
        rv = rv + scale_size_continuous(name=size.label, range=size.range)
    else
    {
        sizes = unique(df$size)
        names(sizes) = sizes
        
        rv = rv + scale_size_manual(guide='none', values=sizes)
    }
    
    if (!is.null(color.range))
        rv = rv + scale_fill_gradient(name=color.label, low=color.range[1], high=color.range[2])
    else
    {
        colors = unique(df$color)
        names(colors) = colors
        
        rv = rv + scale_fill_manual(guide='none', values=colors)
    }
    
    rv
}
