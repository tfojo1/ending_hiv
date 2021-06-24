

source('code/source_code.R')
source('code/visualization/plot_wrappers.R')
library(ggsci)

load('mcmc_runs/visualization_simsets/35620/1.0_35620_baseline.Rdata')
baseline = simset
load('mcmc_runs/visualization_simsets/35620/1.0_35620_noint.Rdata')
noint = simset
load('mcmc_runs/visualization_simsets/35620/1.0_35620_mi.1.50.90.ybh.high.Rdata')
int1 = simset

plot.simulations.total(list(baseline, noint), data.types=c('new'), years=2010:2030)
plot.simulations.total(list(baseline, noint, int1), data.types=c('new'), years=2010:2030)


plot.simulations.total(list(baseline, noint), years=2010:2030)

plot.simulations.total(list(baseline, noint, int1), data.types=c('new'), years=2010:2030, use.plotly=F)

plot.simulations.flex(list(baseline, noint), data.types='new', years=2010:2030, facet.by='sex', split.by='race',
                      return.change.data.frame=T)

plot.simulations.flex(list(baseline, noint), data.types='new', years=2010:2030, facet.by='race', split.by='sex',
                        color.by='split',
                      return.change.data.frame=F, plot.format='individual.simulations')

plot.simulations.total(list(baseline, noint), data.types=c('incidence','new'), years=2010:2030, use.plotly=T,
                       plot.format='mean.and.interval')

plot.simulations.flex(list(baseline, noint), data.types='new', years=2010:2030, facet.by='race', split.by='sex',
                      color.by='split',
                      return.change.data.frame=F,
                      plot.format='mean.and.interval')


x=plot.simulations.flex(list(baseline, noint), data.types='new', years=2010:2030, facet.by='risk', split.by='age',
                      color.by='split',
                      return.change.data.frame=T, plot.format='individual.simulations')

x=plot.simulations.total(list(baseline, noint), years=2010:2030, return.change.data.frame=T)

#tick format
fig <- plot_ly(
    type = "scatter",
    x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
    y = c(0.18, 0.38, 0.56, 0.46, 0.59, 0.4, 0.78, 0.77, 0.74, 0.42, 0.45, 0.39), 
    mode = "markers+lines") 
fig <- fig %>%
    layout(title = 'Primates Brain and Body Weight',
        yaxis = list(
            tickformat = "%",
            title='y title'
        ))

fig

#group by

fig <- plot_ly()

fig = add_paths(fig,
    data=mtcars,
    x = ~hp,
    y = ~qsec,
    transforms = list(
        list(
            type = 'groupby',
            groups = ~cyl
        )
    )
)

fig
