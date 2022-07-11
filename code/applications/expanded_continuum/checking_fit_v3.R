
source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

msa = LA.MSA
mcmc = assemble.mcmc.from.cache(paste0('Q:JHEEM/mcmc_runs/systematic_caches_expanded/',
                                                              msa, '_1x25K_2022-06-07'),T)

mcmc@n.iter
simset = extract.simset(mcmc, additional.burn=625, additional.thin=10)
simset@n.sim

simplot(simset)
simplot(simset, facet.by='risk')
simplot(simset, data.types=c('linkage','suppression','engagement','retention'))
simplot(simset, data.types=c('suppression.of.engaged','suppression','engagement','retention'))


mcmc.balt=mcmc
mcmc.miami = mcmc
mcmc.la = mcmc

ATL.COUNTIES = as.character(c(13067,13089,13121,13135))

get.surveillance.data(county.surveillance, ATL.COUNTIES, 'prevalence') /
get.surveillance.data(msa.surveillance, ATLANTA.MSA, 'prevalence')

# where did LA linkage go?
# remove ATL MSA continuum data?
# M: revise LA starting point