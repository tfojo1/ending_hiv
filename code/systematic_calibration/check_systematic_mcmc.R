if (1==2)
{
    setwd('../../../Ending HIV/Ending_HIV/')
}

source('code/source_code.R')
source('code/plots.R')

mcmc = assemble.mcmc.from.cache('mcmc_runs/systematic_caches/29820_4x100K_v2_2020-11-03/',T)
mcmc = assemble.mcmc.from.cache('mcmc_runs/systematic_caches/35620_4x100K_2020-11-07/',T)


simset = extract.simset(mcmc, additional.burn=mcmc@n.iter/2, 
                        additional.thin=8)


acceptance.plot(mcmc, window.iterations = 200) + geom_hline(yintercept = c(0.238,0.1))
#simset = extract.simset(mcmc, additional.burn=mcmc@n.iter/2, additional.thin=2^(as.numeric(mcmc@n.iter>100)+as.numeric(mcmc@n.iter>1000)))

plot.calibration.sex.risk(simset)
plot.calibration.sex.age(simset)

plot.calibration.total(simset, data.types=c('diagnosed','testing'))
plot.calibration.risk(simset, data.types='testing')
plot.calibration.race(simset, data.types='suppression')

#plot.calibration.sex.age(simset, sex='female')
plot.calibration.race.risk(simset)
plot.calibration.total(simset)

#plot.calibration(simset, facet.by=c('race'), risk='msm')
plot.calibration.risk(simset)
plot.calibration.risk.race(simset)
plot.calibration.race(simset)
plot.calibration.age(simset)




get.rhats(mcmc, additional.burn = mcmc@n.iter/2)
#trace.plot(mcmc, '*ramp', additional.burn = mcmc@n.iter/2)


print(paste0('N Iterations = ', mcmc@n.iter))

likelihood.plot(mcmc, show.log.prior = F)
likelihood.plot(mcmc, show.log.likelihood = F, show.log.prior.plus.likelihood = F)
#qplot(1:mcmc@n.iter, as.numeric(mcmc@log.likelihoods + mcmc@log.priors), geom = 'line')
#trace.plot(mcmc)

a=get.total.acceptance.rate(mcmc, by.block = T);a
qplot(names(a), a) + geom_bar(stat='identity', alpha=.25) + coord_flip() + geom_hline(yintercept = .238) + xlab('Parameter') + ylab('Total Acceptance Rate')

acceptance.plot(mcmc, window.iterations = 200)
acceptance.plot(mcmc, window.iterations = 200, by.block = T)

simset = extract.simset(mcmc)
simset = extract.simset(mcmc, additional.burn=mcmc@n.iter*.75, additional.thin=1)
simset = extract.simset(mcmc, additional.burn=mcmc@n.iter/2, additional.thin=2^(as.numeric(mcmc@n.iter>100)+as.numeric(mcmc@n.iter>1000)))
plot.calibration(simset)
plot.calibration.total(simset)
plot.calibration.race(simset)
plot.calibration.age(simset)
plot.calibration.sex.age(simset)

plot.calibration.sex.age(simset, years=1970:2020)
plot.calibration.risk(simset, years=1970:2020)
plot.calibration.total(simset, years=1970:2020)
plot.calibration.total(simset, years=1970:2020, data.types=c('new','incidence'))

plot.calibration.race.risk(simset)
plot.calibration.risk.race(simset)

plot.calibration.total.incidence(simset, years=2010:2030)

plot.calibration.idu.prevalence(simset)
plot.calibration.idu.prevalence(simset, facet.by='age')
plot.calibration.idu.prevalence(simset, facet.by='sex')

plot.calibration(simset, facet.by='age', split.by='risk')

plot.calibration.age.distribution(simset, facet.by=c('sex','year'), years=c(2010,2016), data.type='prevalence')


plot.calibration(simset, facet.by=c('sex','age'), split.by='risk', sex='male', show.points = T)


#Difficult mixers
ADDITIONAL.BURN = 0#mcmc@n.iter/2
get.rhats(mcmc, additional.burn = ADDITIONAL.BURN)[1:24]
trace.plot(mcmc, names(get.rhats(mcmc, additional.burn = ADDITIONAL.BURN))[1:6], additional.burn = ADDITIONAL.BURN)
trace.plot(mcmc, names(get.rhats(mcmc, additional.burn = ADDITIONAL.BURN))[6+1:6], additional.burn = ADDITIONAL.BURN)

trace.plot(mcmc, c('global.trate','diagnosed.tra','acute.trans'), additional.burn = ADDITIONAL.BURN)
trace.plot(mcmc, '*sexual.oe', additional.burn=ADDITIONAL.BURN)

trace.plot(mcmc, '*mortality', additional.burn=ADDITIONAL.BURN)

trace.plot(mcmc, '*trate.peak', additional.burn=ADDITIONAL.BURN)
trace.plot(mcmc, '*trate.0', additional.burn=ADDITIONAL.BURN)
trace.plot(mcmc, '*trate.1', additional.burn=ADDITIONAL.BURN)
trace.plot(mcmc, '*trate.2', additional.burn=ADDITIONAL.BURN)


trace.plot(mcmc, c('age.mixing'), additional.burn=ADDITIONAL.BURN)

trace.plot(mcmc, '*suppressed', additional.burn=ADDITIONAL.BURN)

trace.plot(mcmc, '*sexual.oe', additional.burn=ADDITIONAL.BURN)

trace.plot(mcmc, '*suppressed.or', additional.burn=ADDITIONAL.BURN)
trace.plot(mcmc, '*suppressed.slope.or', additional.burn=ADDITIONAL.BURN)

#break it down

get.rhats(mcmc)

trace.plot(mcmc, '*heterosexual')
trace.plot(mcmc, '*idu')
trace.plot(mcmc, '*msm')
trace.plot(mcmc, 'age*msm')
trace.plot(mcmc, 'age*het')

trace.plot(mcmc, '*mort')
trace.plot(mcmc, '*aging')
trace.plot(mcmc, '*peak')

trace.plot(mcmc, '*age*rr')
trace.plot(mcmc, c('*incident','*remission','*relapse'))

#trace.plot(mcmc, '*peak.aging')
#trace.plot(mcmc, '*trough.aging')
#trace.plot(mcmc, c('*black.aging','*hispanic.aging','*other.aging'))
#trace.plot(mcmc, '*time')
#save the simset

save(simset, file='../code/result summaries/simset.Rdata')
