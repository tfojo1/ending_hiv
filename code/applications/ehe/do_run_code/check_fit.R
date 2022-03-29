

##-- START --##
source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')
#load the mcmc

desired.nsim = 100
simset = extract.simset(mcmc,
                        additional.burn=mcmc@n.iter/2, 
                        additional.thin=floor(mcmc@n.iter*mcmc@n.chains/2/desired.nsim))

##-- FIT TO EPI DATA --##

simplot(simset)
simplot(simset, facet.by = 'race')
simplot(simset, facet.by = 'age')
simplot(simset, facet.by = 'sex')

simplot(simset, data.types='suppression')
simplot(simset, data.types='suppression', facet.by='risk')

##-- TRACE PLOTS --##
