

source('code/source_code.R')
source('code/calibration/calibrated_parameters_expanded_1.R')

load('mcmc_runs/start_values/12580.Rdata')

pp = suppressWarnings(get.medians(parameters.prior))
matching.names = intersect(names(pp), names(starting.parameters))
pp[matching.names] = starting.parameters[matching.names]
run.simulation = create.run.simulation.function(BALTIMORE.MSA,
                                                start.values=pp,
                                                version='expanded_1.0')


sim = run.simulation(pp)


# Likelihood
likelihood = create.msa.likelihood(BALTIMORE.MSA,
                                   include.engagement=T, include.linkage=T,
                                   verbose=T)

lik.components = attr(likelihood, 'components')
for (cname in names(lik.components)[-2])
{
    print(cname)
    print(lik.components[[cname]](sim))
}

likelihood(sim)

source('code/plots.R')
plot.calibration(sim)
plot.calibration.total(sim, data.type='suppression')
plot.calibration.total(sim, data.type='engagement')
plot.calibration.total(sim, data.type='linkage')



pp2 = pp
pp2['msm.proportion.linked.slope.or'] = pp2['msm.idu.proportion.linked.slope.or'] =
    pp2['idu.proportion.linked.slope.or'] = pp2['heterosexual.proportion.linked.slope.or'] = 1.2
pp2['msm.proportion.lost.slope.or'] = pp2['msm.idu.proportion.lost.slope.or'] = 
    pp2['idu.proportion.lost.slope.or'] = pp2['heterosexual.proportion.lost.slope.or'] = .85
pp2['msm.proportion.lost.or'] = pp2['msm.idu.proportion.lost.or'] = 
    pp2['idu.proportion.lost.or'] = pp2['heterosexual.proportion.lost.or'] = 1.5

sim2 = run.simulation(pp2)

plot.calibration.total(list(sim,sim2), data.type=c('suppression','engagement'))
plot.calibration.total(list(sim,sim2), data.type='linkage')
plot.calibration.total(list(sim,sim2), data.type='engagement')
plot.calibration.total(list(sim,sim2), data.type='suppression')

plot.calibration(list(sim,sim2), data.type='engagement', facet.by='risk')
plot.calibration(list(sim,sim2), data.type='suppression', facet.by='risk')

c(likelihood(sim), likelihood(sim2))

sapply(list(sim, sim2), function(a.sim){
    sapply(lik.components, function(lik.sub){
        lik.sub(a.sim)
    })
})


#to test out the new likelihood function
source('code/calibration/likelihoods_nested_location.R')



lik = create.nested.likelihood(data.type='suppression',
                               msa=BALTIMORE.MSA,
                               msa.surveillance = msa.surveillance,
                               state.surveillance = state.surveillance,
                               county.surveillance = county.surveillance,
                               years=2010:2020,
                               observation.error.fn = function(years,num){num*.05},
                               verbose=T
)
lik(sim)

