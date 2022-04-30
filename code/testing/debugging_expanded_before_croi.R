
source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

state.surveillance$retention.all = array(
    c(0.692, 0.714, 0.673, 0.685, 0.713, 0.746, 0.748),
    dim=c(year=7, location=1),
    dimnames=list(year=as.character(2013:2019), location='MD')
)

prior = get.parameters.prior.for.version(VERSION.MANAGER, 'expanded_1.0')
pp = suppressWarnings(get.medians(prior))
load('mcmc_runs/start_values/12580.Rdata')
matching = intersect(names(pp), names(starting.parameters))
pp[matching] = starting.parameters[matching]

run.simulation = create.run.simulation.function('12580', pp, version='expanded_1.0')

sim1 = run.simulation(pp)
simplot(sim1, data.types='retention', years=2010:2019)


c1 = attr(sim1, 'components')
x = calculate.rates(c1, 'failing.to.disengaged')
range(x$rates[[2]])


pp2 = pp
pp2['msm.proportion.lost.or'] = pp2['idu.proportion.lost.or'] =
    pp2['heterosexual.proportion.lost.or'] = pp2['msm.idu.proportion.lost.or'] = 2
pp2['msm.proportion.lost.slope.or'] = pp2['idu.proportion.lost.slope.or'] =
    pp2['heterosexual.proportion.lost.slope.or'] = pp2['msm.idu.proportion.lost.slope.or'] = 1

sim2 = run.simulation(pp2)
simplot(sim1, sim2, data.types='retention', years=2010:2019)


cbind(extract.retention(sim1,2010:2019),
      extract.retention(sim2,2010:2019))

c2 = attr(sim2, 'components')

range(expit(c2$background.failing.to.disengaged$model$intercept))
