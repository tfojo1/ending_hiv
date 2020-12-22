if (1==2)
{
    setwd('../../../Ending HIV/Ending_HIV/')
}

source('code/source_code.R')
source('code/plots.R')
mcmc = assemble.mcmc.from.cache('mcmc_runs/systematic_caches/35620_1x20K_117e_2020-12-09/',T)

acceptance.plot(mcmc, window.iterations = 200) + geom_hline(yintercept = c(0.238,0.1))
simset = extract.simset(mcmc, additional.burn=mcmc@n.iter/2, additional.thin=2^(as.numeric(mcmc@n.iter>100)+as.numeric(mcmc@n.iter>1000)))

#Key visualizations
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


plot.calibration.risk.race(simset, risk='msm_idu')

plot.calibration.idu.prevalence(simset, facet.by='sex')
plot.calibration.idu.prevalence(simset, facet.by='race')
plot.calibration.idu.prevalence(simset, facet.by=c('sex','race'))

#Break out PrEP
plot.calibration.total(simset, data.types='prep')
plot.calibration.sex(simset, data.types='prep')
plot.calibration.age(simset, data.types='prep')
#plot.calibration.race(simset, data.types='prep')
#plot.calibration.risk(simset, data.types='prep')

trace.plot(mcmc, '*prep')

sapply(c('2018'=9, '2021'=12, '2025'=16), function(year){
    dist = extract.simset.distribution(simset, function(sim){
        c1 = attr(sim, 'components')
        x=c1$prep.rates.and.times$rates[[year]][,,,'msm','never_IDU',]
        df = reshape2::melt(x)
        rv = df$value
        names(rv) = paste0(df$race, '_', df$age)
        rv
    })
    round(100*get.means(dist),1)
})

dist = extract.simset.distribution(simset, function(sim){
    year = c('2025'=16, '2018'=9, '2021'=12)[3]
    c1 = attr(sim, 'components')
    x=c1$prep.rates.and.times$rates[[year]][,,,'msm','never_IDU',]
    df = reshape2::melt(x)
    rv = df$value
    names(rv) = paste0(df$race, '_', df$age)
    rv
})
cbind(get.means(dist) * 100, t(get.intervals(dist)*100))


dist = extract.simset.distribution(simset, function(sim){
    c1 = attr(sim, 'components')
    race='other'#'other'
    x=sapply(c1$prep.rates.and.times$rates, function(r){
        r[2,race,1,'msm','never_IDU',1]
    })
    names(x) = c1$prep.rates.and.times$times
    x
})
df=data.frame(cbind(get.means(dist) * 100, t(get.intervals(dist)*100))); df$year = dimnames(df)[[1]]
ggplot(df, aes(x=year)) + geom_ribbon(aes(ymin=lower, ymax=upper), group=1, alpha=0.2) + geom_line(aes(y=V1), group=1) +
    geom_vline(xintercept = 2021)


#Plot by indication
dist = extract.simset.distribution(simset, extract.prep.coverage, years=2010:2020, risk='msm', use.cdc.categorizations=T)
df=data.frame(cbind(get.means(dist) * 100, t(get.intervals(dist)*100))); df$year = dimnames(df)[[1]]
ggplot(df, aes(x=year)) + geom_ribbon(aes(ymin=lower, ymax=upper), group=1, alpha=0.2) + geom_line(aes(y=V1), group=1)

dist = extract.simset.distribution(simset, extract.prep.coverage, years=2010:2020, risk='heterosexual', use.cdc.categorizations=T)
df=data.frame(cbind(get.means(dist) * 100, t(get.intervals(dist)*100))); df$year = dimnames(df)[[1]]
ggplot(df, aes(x=year)) + geom_ribbon(aes(ymin=lower, ymax=upper), group=1, alpha=0.2) + geom_line(aes(y=V1), group=1)

dist = extract.simset.distribution(simset, extract.prep.coverage, years=2010:2020, risk='active_IDU', use.cdc.categorizations=F)
df=data.frame(cbind(get.means(dist) * 100, t(get.intervals(dist)*100))); df$year = dimnames(df)[[1]]
ggplot(df, aes(x=year)) + geom_ribbon(aes(ymin=lower, ymax=upper), group=1, alpha=0.2) + geom_line(aes(y=V1), group=1)


#Break out suppression
plot.calibration.total(simset, data.types='suppression')
plot.calibration.race(simset, data.types='suppression')
plot.calibration.risk(simset, data.types='suppression')
plot.calibration.age(simset, data.types='suppression')
plot.calibration.sex(simset, data.types='suppression')

plot.calibration.total(simset, data.types='testing')
plot.calibration.race(simset, data.types='testing')
plot.calibration.risk(simset, data.types='testing')
plot.calibration.age(simset, data.types='testing')
plot.calibration.sex(simset, data.types='testing')


cvs = apply(simset@parameters, 2, sd) / colMeans(simset@parameters)
cvs = sort(cvs)
cvs[1:10]
trace.plot(mcmc, names(cvs)[1:16])

print(paste0('N Iterations = ', mcmc@n.iter))

likelihood.plot(mcmc, show.log.prior = F)
likelihood.plot(mcmc, show.log.likelihood = F, show.log.prior.plus.likelihood = F)
#qplot(1:mcmc@n.iter, as.numeric(mcmc@log.likelihoods + mcmc@log.priors), geom = 'line')
#trace.plot(mcmc)

a=get.total.acceptance.rate(mcmc, by.block = T);a
qplot(names(a), a) + geom_bar(stat='identity', alpha=.25) + coord_flip() + geom_hline(yintercept = 0.238) + xlab('Parameter') + ylab('Total Acceptance Rate')

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
plot.calibration.total(simset, years=1970:2020, data.types=c('new','prevalence'))

plot.calibration.race.risk(simset)
plot.calibration.risk.race(simset)

plot.calibration.total.incidence(simset, years=2010:2030)

plot.calibration.idu.prevalence(simset)
plot.calibration.idu.prevalence(simset, facet.by='age')
plot.calibration.idu.prevalence(simset, facet.by='sex')

plot.calibration(simset, facet.by='age', split.by='risk')

plot.calibration.cumulative.mortality(simset)

plot.calibration.age.distribution(simset, facet.by=c('sex','year'), years=c(2010,2016), data.type='prevalence')


plot.calibration(simset, facet.by=c('sex','age'), split.by='risk', sex='male', show.points = T)

#break it down

trace.plot(mcmc, c('global','*transmiss'))


trace.plot(mcmc, '*heterosexual')
trace.plot(mcmc, '*idu')
trace.plot(mcmc, '*msm.trate')
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

save(simset, file='result_summaries/simset.Rdata')

round(cbind(get.sds(parameters.prior), apply(mcmc@samples, 3, sd), get.sds(parameters.prior)/apply(mcmc@samples,3, sd)),4)

