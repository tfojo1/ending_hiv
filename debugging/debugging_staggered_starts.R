
source('code/source_code.R')
source('code/plots.R')

load('mcmc_runs/full_simsets/35620/1.0_35620_mi.6m.50.90.ybh.high6.3y.Rdata')

sim1 = simset@simulations[[3]]
sim2 = simset@simulations[[4]]

pp1 = simset@parameters[3,]
pp2 = simset@parameters[4,]

cbind(pp1,pp2)[130:138,]

plot.calibration.risk(list(sim1,sim2), data.types='testing', years=2020:2030) + geom_vline(xintercept = c(2021,2024))

c1 = attr(sim1, 'components')
c2 = attr(sim2, 'components')
int = attr(simset, 'intervention')

INDEX = 16
int$processed$testing$start.times[INDEX]
melt(int$processed$testing$start.times)[INDEX,]

cx1 = do.calculate.testing.rates(c1)
cx2 = do.calculate.testing.rates(c2)
sim1x = sim1; attr(sim1x, 'components') = cx1
sim2x = sim2; attr(sim2x, 'components') = cx2

plot.calibration.risk(list(sim2x,sim2), data.types='testing', years=2020:2030) + geom_vline(xintercept = c(2021,2024))
plot.calibration.risk(list(sim1x,sim1), data.types='testing', years=2020:2030) + geom_vline(xintercept = c(2021,2024))
