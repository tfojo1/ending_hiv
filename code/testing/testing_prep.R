load('mcmc_runs/full_simsets/1.0_35620_full.Rdata')

plot.calibration.sex(simset, data.types='prep', plot.individual.simset.sims = F)

i=1000
plot.calibration.sex(simset@simulations[[i]], data.types='prep', plot.individual.simset.sims = F)

convert.true.prep.to.rx <- function(x)
{
    x / 0.724 / PREP.RX.3MO.To.1Y['total'] / PREP.RETENTION.3MO * FRACTION.PREP.STARTS.RECORDED
}


simset = s


pp = extract.prep.coverage(simset@simulations[[i]], keep.dimensions = c('year','risk'), years=2010:2020, use.cdc.categorizations = T)
pp[,'msm']
pp[-1,] - pp[-11,]
pp = extract.prep.coverage(simset@simulations[[i]], keep.dimensions = c('year','risk','race'), years=2010:2020, use.cdc.categorizations = T)
pp[,'msm',]
pp = extract.prep.coverage(simset@simulations[[i]], keep.dimensions = c('age','risk','race'), years=2020, use.cdc.categorizations = T)
pp[,'msm',]
pp = extract.prep.coverage(simset@simulations[[i]], keep.dimensions = c('year','age','risk'), years=2010:2020, use.cdc.categorizations = T)
pp[,,'msm'] * 1.8

ind = get.prep.indications.estimate()
apply(ind, c('sex','risk'), mean)
