
source('code/calibration/likelihoods/likelihoods_nested_location2.R')
source('code/calibration/likelihoods/likelihoods_2.R')
Rcpp::sourceCpp('code/calibration/likelihoods/likelihoods.cpp')
source('code/settings_and_files/version_manager.R')

load('simulations/test_sim.Rdata')

msa = '12580'  
SUPPRESSION.WEIGHT = 1/16
SUPPRESSION.SD = 0.01
SUPPRESSION.OBS.ERROR.SD = function(...){SUPPRESSION.SD}
SUPPRESSION.SD.INFLATION = 1/sqrt(SUPPRESSION.WEIGHT)/sqrt(1/2)
PROBABILITY.SUPPRESSION.DECREASING = 0.05
suppressed.lik = create.nested.likelihood(data.type='suppression',
                                          years=2008:2019,
                                          msa=msa,
                                          
                                          msa.surveillance = msa.surveillance,
                                          state.surveillance = state.surveillance,
                                          county.surveillance = county.surveillance,
                                          
                                          census.totals=ALL.DATA.MANAGERS$census.totals,
                                          census.full.msm=ALL.DATA.MANAGERS$census.full.msm,
                                          census.collapsed=ALL.DATA.MANAGERS$census.collapsed,
                                          settings=get.settings.for.version('collapsed_1.0'),
                                          
                                          observation.error.fn = SUPPRESSION.OBS.ERROR.SD,
                                          sd.inflation = SUPPRESSION.SD.INFLATION,
                                          sd.inflation.extra.msa.to.msa = SUPPRESSION.SD.INFLATION,
                                          
                                          probability.decreasing.slope=PROBABILITY.SUPPRESSION.DECREASING
)

suppressed.lik(sim)
