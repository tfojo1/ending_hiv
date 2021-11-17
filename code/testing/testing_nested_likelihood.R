
source('code/source_code.R')
source('code/calibration/likelihoods_nested_location.R')
load('cached/county.surveillance.Rdata')

#lets check it against an old style sim
load('mcmc_runs/quick_simsets/1.0_12580_full.Rdata')
sim = simset@simulations[[1]]
msa = attr(sim, 'location')

#make an old likelihood
EVERYTHING.WEIGHT=1/2
SUPPRESSION.SD = 0.01
suppression.years = 2016:2018#2010:2018
SUPPRESSION.WEIGHT = 1/16

SUPPRESSED.SD = function(...){SUPPRESSION.SD}
SUPPRESSION.SD.INFLATION = 1/sqrt(SUPPRESSION.WEIGHT)/sqrt(EVERYTHING.WEIGHT)
SUPPRESSED.STATE.SD.INFLATION = 3
PROBABILITY.SUPPRESSION.DECREASING = 0.05
CONSIDER.DECREASING.ON.MARGINALS = F


old.lik = suppressed.lik = create.suppressed.likelihood(location=msa,
                                                        years=suppression.years,
                                                        surv=msa.surveillance,
                                                        numerator.year.to.year.chunk.correlation=0.5,
                                                        numerator.chunk.years=list(suppression.years),
                                                        numerator.sd = SUPPRESSED.SD,
                                                        sd.inflation=SUPPRESSION.SD.INFLATION,
                                                        inflate.sd.by.n.obs.per.year = F,
                                                        numerator.sd.inflation.if.backup=SUPPRESSED.STATE.SD.INFLATION,
                                                        probability.decreasing.slope=PROBABILITY.SUPPRESSION.DECREASING,
                                                        consider.decreasing.on.marginals=CONSIDER.DECREASING.ON.MARGINALS)

old.lik(sim)

lik = create.nested.likelihood(data.type='suppression',
                               msa=msa,
                               msa.surveillance = msa.surveillance,
                               state.surveillance = state.surveillance,
                               county.surveillance = county.surveillance,
                               years=suppression.years,
                               observation.error.fn = SUPPRESSED.SD,
                               verbose=T,
                               
                               sd.inflation = SUPPRESSION.SD.INFLATION,
                               include.states=T, include.counties = T, include.msa=T,
)

lik(sim)
