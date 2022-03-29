
TYPE.NAMES = c(
    workplace='Workplace',
    grocery.pharmacy='Grocery/Pharmacy',
    transit='Transit',
    retail='Retail',
    residential='Residential',
    total='Total'
)

library(scales)
library(reshape2)
THEME = theme(
    text = element_text(size=8),
    legend.position = 'bottom',
    panel.background = element_blank(), 
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA))

source('code/source_code.R')
msa = CHICAGO.MSA

IMAGE.DIR = '../jheem_supplements/covid/images/mobility/'

if (1==2)
{
source('code/core_code/data_managers/mobility_data_manager.R')
load('cached/MOBILITY.DATA.MANAGER.Rdata')

mobility = get.mobility.data.by.month(MOBILITY.DATA.MANAGER,
                                      locations=msa,
                                      types = NON.PARK.MOBILITY.TYPES)


#mobility = rbind(mobility,
#                 total=colMeans(mobility))
#names(dimnames(mobility)) = c('type','time')


# Raw data
df1 = reshape2::melt(mobility)
df1$date = as.Date(paste0(gsub('\\d+-(\\d\\d\\d\\d)', '\\1', df1$time),
                          '-',gsub('(\\d+)-.*', '\\1', df1$time),
                          '-15'
                          ))
df1$type = TYPE.NAMES[df1$type]
df1$type = factor(df1$type, levels=TYPE.NAMES)

png(filename=file.path(IMAGE.DIR, 'raw_monthly.png'), res=300, 
    width=6, height=3, units='in')
ggplot(df1, aes(date, value)) + 
    geom_hline(yintercept = 0, linetype='dashed') +
    geom_line() + 
    scale_y_continuous(labels=percent, name = 'Change Relative to Baseline') +
    xlab('Date') +
    facet_wrap(~type) +
    THEME
dev.off()

#normalized
mobility.normalized = mobility / mobility[,2]
mobility.normalized = rbind(mobility.normalized,
                 total=colMeans(mobility.normalized))
names(dimnames(mobility.normalized)) = c('type','time')

df2 = reshape2::melt(mobility.normalized)
df2$date = as.Date(paste0(gsub('\\d+-(\\d\\d\\d\\d)', '\\1', df2$time),
                          '-',gsub('(\\d+)-.*', '\\1', df2$time),
                          '-15'
))
df2$type = TYPE.NAMES[df2$type]
df2$type = factor(df2$type, levels=TYPE.NAMES)
df2$value = pmax(0, df2$value)
png(filename=file.path(IMAGE.DIR, 'normalized_monthly.png'), res=300, 
    width=6, height=3, units='in')
ggplot(df2, aes(date, value)) + 
    geom_hline(yintercept = 0, linetype='dashed') +
    geom_line() + 
    scale_y_continuous(labels=percent, name = 'Normalized Change Relative to Baseline') +
    xlab('Date') +
    facet_wrap(~type) +
    THEME
}
dev.off()

#Total
total.mobility = mobility.normalized['total',]
dates = as.Date(paste0(gsub('\\d+-(\\d\\d\\d\\d)', '\\1', names(total.mobility)),
                       '-',gsub('(\\d+)-.*', '\\1', names(total.mobility)),
                       '-15'))

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
    THEME + scale_color_discrete(name='Mobility Weight')
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
    THEME + scale_color_discrete(name='Mobility Weight')
dev.off()
