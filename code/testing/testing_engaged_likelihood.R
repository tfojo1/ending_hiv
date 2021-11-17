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


engagement.years = 2010:2020
EVERYTHING.WEIGHT=1/2
ENGAGEMENT.WEIGHT = 1/16
ENGAGEMENT.SD = 0.01

#-- Elements for Engagement --#
ENGAGEMENT.OBS.ERROR.SD = function(...){ENGAGEMENT.SD}
ENGAGEMENT.SD.INFLATION = 1/sqrt(ENGAGEMENT.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
PROBABILITY.ENGAGEMENT.DECREASING = NA

engaged.lik = create.nested.likelihood(data.type='engagement',
                                       years=engagement.years,
                                       msa=attr(sim, 'location'),
                                       
                                       msa.surveillance = msa.surveillance,
                                       state.surveillance = state.surveillance,
                                       county.surveillance = county.surveillance,
                                       
                                       observation.error.fn = ENGAGEMENT.OBS.ERROR.SD,
                                       sd.inflation = ENGAGEMENT.SD.INFLATION,
                                       
                                       probability.decreasing.slope=PROBABILITY.ENGAGEMENT.DECREASING,
                                       
                                       include.msa = T,
                                       include.states = F,
                                       include.counties = F,
                                       
                                       by.total=T,
                                       by.age=T,
                                       by.race=T,
                                       by.sex=T,
                                       by.risk=T
)

c(engaged.lik(sim), engaged.lik(sim2))


supp.lik = create.nested.likelihood(data.type='suppression',
                                                  years=engagement.years,
                                                  msa=attr(sim, 'location'),
                                                  
                                                  msa.surveillance = msa.surveillance,
                                                  state.surveillance = state.surveillance,
                                                  county.surveillance = county.surveillance,
                                                  
                                                  observation.error.fn = ENGAGEMENT.OBS.ERROR.SD,
                                                  sd.inflation = ENGAGEMENT.SD.INFLATION,
                                                  
                                                  probability.decreasing.slope=PROBABILITY.ENGAGEMENT.DECREASING,
                                                  
                                                  include.msa = T,
                                                  include.states = F,
                                                  include.counties = F)
c(supp.lik(sim), supp.lik(sim2))
                                                  