
library(ggplot2)
library(scales)

df = melt(MOBILITY.DATA.MANAGER$data['24510',,])
df$date = as.Date(df$date)

ggplot(df, aes(date, value)) + geom_point() + 
    facet_wrap(~type) + scale_y_continuous(labels = percent) +
    ylab('Percent Change from Baseline') + xlab("Date")

x = get.mobility.data.by.month(MOBILITY.DATA.MANAGER,
                               locations='12580',
                               types=NON.PARK.MOBILITY.TYPES)

x = rbind(x, total=colMeans(x))

df = melt(x)
df$date = as.Date(paste0(gsub('.*-(\\d\\d\\d\\d)','\\1',df$Var2),
                  '-',
                  gsub('(.*)-\\d\\d\\d\\d','\\1',df$Var2),
                  '-15'))
ggplot(df, aes(date, value)) + geom_line() + geom_point() +
    facet_wrap(~Var1) + scale_y_continuous(labels = percent) +
    ylab('Percent Change from Baseline') + xlab("Date")
    
y = cbind(raw=x['total',],
          normalized=x['total',]/x['total',2])
df = melt(y)
df$date = as.Date(paste0(gsub('.*-(\\d\\d\\d\\d)','\\1',df$Var1),
                         '-',
                         gsub('(.*)-\\d\\d\\d\\d','\\1',df$Var1),
                         '-15'))
ggplot(df, aes(date, value)) + geom_line() + geom_point() +
    facet_wrap(~Var2, scales='free') + scale_y_continuous(labels = percent, limits=c(-.3,1)) +
    ylab('Percent Change from Baseline') + xlab("Date") 



#v1
PARAM.VALUE = 0.5

library(ggsci)
colors = pal_jama()(6)[-1][-3]
names(colors) = c(
    'Fixed Effect',
    'Normalized to Mobility',
    'Normalized to Mobility, with Taper',
    'Half-Normalized, Half-Fixed, with Taper'
)

df = data.frame(
    date=as.Date(c('2020-03-15','2021-03-08','2021-07-04')),
    value=c(PARAM.VALUE,PARAM.VALUE,0),
    Version='Fixed Effect'
)

ggplot(df, aes(date, value, color=Version)) + geom_line(size=2) +
    geom_hline(yintercept = 0, linetype='dashed') +
    scale_y_continuous(labels = percent, limits=c(0,0.6)) +
    ylab('Change in Sexual Transmission') + xlab("Date") +
    scale_color_manual(values=colors[1])

normalized=x['total',]/x['total',2]
dates = as.Date(paste0(gsub('.*-(\\d\\d\\d\\d)','\\1',names(normalized)),
                       '-',
                       gsub('(.*)-\\d\\d\\d\\d','\\1',names(normalized)),
                       '-15'))

df2 = rbind(df,
            data.frame(
                date=dates,
                value=PARAM.VALUE * normalized,
                Version='Normalized to Mobility'
            ))

ggplot(df2, aes(date, value, color=Version)) + geom_line(size=2) +
    geom_hline(yintercept = 0, linetype='dashed') +
    scale_y_continuous(labels = percent, limits=c(0,0.6)) +
    ylab('Change in Sexual Transmission') + xlab("Date")  +
    scale_color_manual(values=colors[1:2])

force.to.zero = rep(1, length(normalized))
force.to.zero[dates>as.Date('2021-07-04')] = 0
force.to.zero[14]=0.75
force.to.zero[15]=0.5
force.to.zero[16]=0.25

df3 = rbind(df2,
            data.frame(
                date=dates,
                value=PARAM.VALUE * normalized * force.to.zero,
                Version='Normalized to Mobility, with Taper'
            ))

ggplot(df3, aes(date, value, color=Version)) + geom_line(size=2) +
    geom_hline(yintercept = 0, linetype='dashed') +
    scale_y_continuous(labels = percent, limits=c(0,0.6)) +
    ylab('Change in Sexual Transmission') + xlab("Date")  +
    scale_color_manual(values=colors[1:3])


orig = PARAM.VALUE * force.to.zero

df4 = rbind(df3,
            data.frame(
                date=dates,
                value = PARAM.VALUE * force.to.zero * (0.5 + 0.5*normalized),
                Version='Half-Normalized, Half-Fixed, with Taper'
            ))
ggplot(df4[df4$Version!='Normalized to Mobility',], aes(date, value, color=Version)) + geom_line(size=2) +
    geom_hline(yintercept = 0, linetype='dashed') +
    scale_y_continuous(labels = percent, limits=c(0,0.6)) +
    ylab('Change in Sexual Transmission') + xlab("Date")  +
    scale_color_manual(values=colors[c(1,3,4)])

