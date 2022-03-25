
source('code/calibration/target_msas.R')
msa = SAN.DIEGO.MSA
DO.RENDER = T

#-- Load up libraries/source --#


source('code/processing/for_talks/talk_plot_settings.R')

library(scales)
library(reshape2)
source('code/source_code.R')


#-- Style Settings --#

TYPE.NAMES = c(
    workplace='Workplace',
    grocery.pharmacy='Grocery/Pharmacy',
    transit='Transit',
    retail='Retail',
    residential='Residential',
    total='Total'
)

THEME = theme(
    text = element_text(size=14),
    axis.text.x = element_text(angle = 30, vjust = 0.5),
    strip.background = element_rect(color = "black", size = 1),
    legend.position = 'bottom',
    panel.background = element_blank(), 
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA))

THEME2 = THEME + theme(text = element_text(size=16))

parse.time.to.date <- function(times)
{
    year = as.integer(gsub('\\d+-(\\d\\d\\d\\d)', '\\1', times))
    month = as.integer(gsub('(\\d+)-.*', '\\1', times))
    
    year + (month-.5)/12
}

pretty.label.date <- function(dates)
{
    year = floor(dates)
    month = round((dates-floor(dates)) * 12 + .5)
    month = pmax(1, month)

    MONTH.NAMES = c('Jan',
                    'Feb',
                    'March',
                    'April',
                    'May',
                    'June',
                    'July',
                    'Aug',
                    'Sept',
                    'Oct',
                    'Nov',
                    'Dec')
    
    
    paste0(MONTH.NAMES[month], ", ", year)
}

to.pretty.date <- function(times)
{
    year = gsub('\\d+-(\\d\\d\\d\\d)', '\\1', df1$time)
    month = gsub('(\\d+)-.*', '\\1', df1$time) 
    
    MONTH.NAMES = c('Jan',
                    'Feb',
                    'March',
                    'April',
                    'May',
                    'June',
                    'July',
                    'Aug',
                    'Sept',
                    'Oct',
                    'Nov',
                    'Dec')
    
    paste0(MONTH.NAMES[month], ", ",)
}

if (DO.RENDER)
{
    source('code/core_code/data_managers/mobility_data_manager.R')
    load('cached/MOBILITY.DATA.MANAGER.Rdata')
    
    mobility = get.mobility.data.by.month(MOBILITY.DATA.MANAGER,
                                          locations=msa,
                                          types = NON.PARK.MOBILITY.TYPES)

    
    # Raw data
    df1 = reshape2::melt(mobility)
    df1$date = parse.time.to.date(df1$time)
    df1$type = TYPE.NAMES[df1$type]
    df1$type = factor(df1$type, levels=TYPE.NAMES)
    
    
    
    png(filename=file.path(COVID.IMAGE.DIR, 'raw_monthly_mobility.png'), res=300, 
        width=6, height=4.5, units='in')
    ggplot(df1, aes(date, value)) + 
        geom_hline(yintercept = 0, linetype='dashed') +
        geom_line() + 
        scale_y_continuous(labels=percent, name = 'Change Relative to Baseline') +
        scale_x_continuous(labels=pretty.label.date, name = 'Date') +
        facet_wrap(~type) +
        THEME
    dev.off()

    #normalized
    mobility.normalized = mobility / mobility[,2]
    mobility.normalized = rbind(mobility.normalized,
                     total=colMeans(mobility.normalized))
    names(dimnames(mobility.normalized)) = c('type','time')
    
    df2 = reshape2::melt(mobility.normalized)
    df2$date = parse.time.to.date(df2$time)
    df2$type = TYPE.NAMES[df2$type]
    df2$type = factor(df2$type, levels=TYPE.NAMES)
    df2$value = pmax(0, df2$value)
    png(filename=file.path(COVID.IMAGE.DIR, 'mobility_normalized_monthly.png'), res=300, 
        width=4.5, height=4.5, units='in')
    ggplot(df2[df2$type=='Total',], aes(date, value)) + 
        geom_hline(yintercept = 0, linetype='dashed') +
        geom_line() + 
        scale_y_continuous(labels=percent, name = 'Normalized Change Relative to Baseline') +
        scale_x_continuous(labels=pretty.label.date, name = 'Date') +
        xlab('Date') +
        facet_wrap(~type) +
        THEME
   
    dev.off()
}

if (DO.RENDER)
{
    reduction = 0.25
    
    #Total
    total.mobility = mobility.normalized['total',]
    dates = parse.time.to.date(names(total.mobility))
    
    multipliers = c(0, total.mobility, total.mobility[length(total.mobility)], 0)
    dates = c(2020 + 2/12, dates, 2021 + 11.5/12, 2022 + 1/10/12)
    
    taper.rapid = c(0,
                    rep(1,13),
                    0.75,
                    0.5,
                    0.25,
                    rep(0,7))
    
    taper.delayed = c(0,
                      rep(1,19),
                      0.75,0.5,0.25,
                      0)
    
    
    baseline.intercept = 100
    baseline.date = 2020
    baseline.slope = 5
    
    df3 = data.frame(
        date = dates,
        scenario = 'no covid',
        value = baseline.intercept + baseline.slope * (dates-baseline.date)
    )
    
    mobility.weight = 0.5
    df3 = rbind(df3,
                data.frame(
                    date = dates,
                    scenario = 'covid',
                    value = (baseline.intercept + baseline.slope * (dates-baseline.date)) * 
                        (1-reduction * taper.rapid * (1-mobility.weight + mobility.weight * multipliers))
    ))
    
    png(filename=file.path(COVID.IMAGE.DIR, 'eg_rapid.png'), res=300, 
        width=8, height=4, units='in')
    ggplot(df3, aes(x=date, y=value, linetype=scenario)) + 
        geom_hline(yintercept = 0, linetype='dashed') +
        geom_line(size=1) + 
        scale_y_continuous(labels=NULL, name = 'Rate of Sexual Transmission') +
        scale_x_continuous(labels=pretty.label.date, name = 'Date') +
        scale_linetype_manual(guide='none', name='Mobility Weight', values=c('covid'='dashed', 'no covid'='solid')) +
        THEME #+ scale_linetype_discrete(name='Mobility Weight', guide='none')
    dev.off()
    
    
    
    baseline.intercept = 70/100
    baseline.date = 2020
    baseline.slope = 2/100
    
    df4 = data.frame(
        date = dates,
        scenario = 'no covid',
        value = baseline.intercept + baseline.slope * (dates-baseline.date)
    )
    
    mobility.weight = 0.5
    df4 = rbind(df4,
                data.frame(
                    date = dates,
                    scenario = 'covid',
                    value = (baseline.intercept + baseline.slope * (dates-baseline.date)) * 
                        (1-reduction * taper.delayed * (1-mobility.weight + mobility.weight * multipliers))
                ))
    
    png(filename=file.path(COVID.IMAGE.DIR, 'eg_delayed.png'), res=300, 
        width=8, height=4, units='in')
    ggplot(df4, aes(x=date, y=value, linetype=scenario)) + 
        geom_hline(yintercept = 0, linetype='dashed') +
        geom_line(size=1) + 
        scale_y_continuous(labels=percent, name = 'Viral Suppression') +
        scale_x_continuous(labels=pretty.label.date, name = 'Date') +
        scale_linetype_manual(guide='none', name='Mobility Weight', values=c('covid'='dashed', 'no covid'='solid')) +
        THEME# + scale_linetype_discrete(name='Mobility Weight', guide='none')
    dev.off()


        
#    different mobility weights
    
    df5 = data.frame(
        date = dates,
        scenario = 'no covid',
        value = baseline.intercept + baseline.slope * (dates-baseline.date),
        weight = 'baseline'
    )
    
    mobility.weights = c(1,.5,0)
    for (i in 1:length(mobility.weights))
    {
        mobility.weight = mobility.weights[i]
        df5 = rbind(df5,
                    data.frame(
                        date = dates,
                        scenario = 'covid',
                        value = (baseline.intercept + baseline.slope * (dates-baseline.date)) * 
                            (1-reduction * taper.delayed * (1-mobility.weight + mobility.weight * multipliers)),
                        weight = paste0('w',i)
                    ))
    }
    
    png(filename=file.path(COVID.IMAGE.DIR, 'eg_delayed_multiple_weights.png'), res=300, 
        width=8, height=4, units='in')
    ggplot(df5, aes(x=date, y=value, linetype=scenario, color=weight)) + 
        geom_hline(yintercept = 0, linetype='dashed') +
        geom_line(size=1) + 
        scale_y_continuous(labels=percent, name = 'Viral Suppression') +
        scale_x_continuous(labels=pretty.label.date, name = 'Date') +
        scale_linetype_manual(guide='none', name='Mobility Weight', values=c('covid'='dashed', 'no covid'='solid')) +
        scale_color_manual(guide='none',
                           values=c(baseline='black',
                                    w1='darkgreen',
                                    w2='blue',
                                    w3='red')) +
        THEME# + scale_linetype_discrete(name='Mobility Weight', guide='none')
    dev.off()
    
}


if (1==2)
{
    df4 = NULL
    reduction = 0.25
    taper = taper.delayed
    
    for (mobility.weight in c(0,0.5,1))
    {
        
        df4 = rbind(df4,
                    data.frame(
                        weight = format(mobility.weight, digits=1, nsmall=1),
                        date = dates,
                        value = reduction * taper * (1-mobility.weight + mobility.weight * multipliers)
                    ))
    }
    
    png(filename=file.path(IMAGE.DIR, 'eg_delayed.png'), res=300, 
        width=4, height=3, units='in')
    ggplot(df4, aes(x=date, y=value, color=weight)) + 
        geom_hline(yintercept = 0, linetype='dashed') +
        geom_line(size=1) + 
        scale_y_continuous(labels=percent, name = 'Reduction in Viral Suppression') +
        xlab('Date') +
        THEME + scale_color_discrete(name='Mobility Weight')
    dev.off()
    
}

#This was for multiple weights - save in case of future use
if (1==2)
{

    #Total
    total.mobility = mobility.normalized['total',]
    dates = parse.time.to.date(names(total.mobility))
    
    multipliers = c(0, total.mobility, total.mobility[length(total.mobility)], 0)
    dates = c(as.Date('2020-03-01'), dates, as.Date('2021-12-15'), as.Date('2022-01-04'))
    
    taper.rapid = c(0,
                    rep(1,13),
                    0.75,
                    0.5,
                    0.25,
                    rep(0,7))
    
    taper.delayed = c(0,
                      rep(1,19),
                      0.75,0.5,0.25,
                      0)
    
    
    df3 = NULL
    reduction = 0.25
    taper = taper.rapid
    
    for (mobility.weight in c(0,0.5,1))
    {
    
        df3 = rbind(df3,
                    data.frame(
                        weight = format(mobility.weight, digits=1, nsmall=1),
                        date = dates,
                        value = reduction * taper * (1-mobility.weight + mobility.weight * multipliers)
                    ))
    }
    
    png(filename=file.path(IMAGE.DIR, 'eg_rapid.png'), res=300, 
        width=4, height=3, units='in')
    ggplot(df3, aes(x=date, y=value, color=weight)) + 
        geom_hline(yintercept = 0, linetype='dashed') +
        geom_line(size=1) + 
        scale_y_continuous(labels=percent, name = 'Reduction in Viral Suppression') +
        xlab('Date') +
        THEME2 + scale_color_discrete(name='Mobility Weight')
    dev.off()
    
    df4 = NULL
    reduction = 0.25
    taper = taper.delayed
    
    for (mobility.weight in c(0,0.5,1))
    {
        
        df4 = rbind(df4,
                    data.frame(
                        weight = format(mobility.weight, digits=1, nsmall=1),
                        date = dates,
                        value = reduction * taper * (1-mobility.weight + mobility.weight * multipliers)
                    ))
    }
    
    png(filename=file.path(IMAGE.DIR, 'eg_delayed.png'), res=300, 
        width=4, height=3, units='in')
    ggplot(df4, aes(x=date, y=value, color=weight)) + 
        geom_hline(yintercept = 0, linetype='dashed') +
        geom_line(size=1) + 
        scale_y_continuous(labels=percent, name = 'Reduction in Viral Suppression') +
        xlab('Date') +
        THEME2 + scale_color_discrete(name='Mobility Weight')
    dev.off()

}

print("ALL DONE MAKING MOBILITY IMAGES")