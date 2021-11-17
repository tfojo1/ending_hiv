
source('code/source_code.R')
load('mcmc_runs/baltimore_initial_simset_v2.Rdata')


if (1==2)
{
load('mcmc_runs/baltimore_initial_simset.Rdata')
simset@simulations = lapply(simset@simulations, function(sim){
    comps = attr(sim, 'components')
    comps = set.gain.and.loss.of.suppression.rates(comps,
                                                   gain.of.suppression.rate = 12/3,
                                                   loss.of.suppression.rate = 12/1)
    attr(sim, 'components') = comps
    sim
})
save(simset, file='mcmc_runs/baltimore_initial_simset.Rdata')
}


LINKAGE.95 = create.intervention.unit(type='linkage', start.year=2023, rates=0.95, years=2027)
RETENTION.S.95 = create.intervention.unit(type='retention.suppressed', start.year=2023, rates=0.95, years=2027)
RETENTION.U.95 = create.intervention.unit(type='retention.unsuppressed', start.year=2023, rates=0.95, years=2027)
ADHERENCE.S.95 = create.intervention.unit(type='art.adherence.suppressed', start.year=2023, rates=0.95, years=2027)
ADHERENCE.U.95 = create.intervention.unit(type='art.adherence.unsuppressed', start.year=2023, rates=0.95, years=2027)
GOS.95 = create.intervention.unit(type='gain.of.suppression', start.year=2023, rates=0.95, years=2027)

int.link = create.intervention(WHOLE.POPULATION, LINKAGE.95)
int.ret = create.intervention(WHOLE.POPULATION, RETENTION.U.95, RETENTION.S.95)
int.adh = create.intervention(WHOLE.POPULATION, ADHERENCE.U.95, ADHERENCE.S.95)
int.gos = create.intervention(WHOLE.POPULATION, GOS.95)

base = subset.simset(simset, 1:5)
base = prepare.simset.for.interventions(base)

source('code/plots.R')

simset.noint = run.simset.intervention(base, NO.INTERVENTION, keep.years=2010:2030)

simset.gos = run.simset.intervention(base, int.gos, keep.years=2010:2030)
plot.calibration.total(simset.gos, data.types=c('engagement','suppression'), years=2010:2030)
plot.calibration.total(list(simset.noint, simset.gos), data.types=c('engagement','suppression'), years=2010:2030)
plot.calibration.total(list(simset.noint@simulations[[1]], simset.gos@simulations[[1]]), data.types=c('engagement','suppression'), years=2010:2030)

simset.link = run.simset.intervention(base, int.link, keep.years = 2010:2030)
plot.calibration(simset.link, data.types='linkage', years=2010:2030)
plot.calibration.total(simset.link, data.types=c('engagement','suppression'), years=2010:2030)

simset.ret = run.simset.intervention(base, int.ret, keep.years = 2010:2030)
plot.calibration.total(simset.ret, data.types=c('engagement','suppression'), years=2010:2030)

simset.adh = run.simset.intervention(base, int.adh, keep.years = 2010:2030)
plot.calibration(simset.adh, data.types=c('engagement','suppression'), years=2010:2030)

plot.calibration(list(simset.noint, simset.adh), data.types=c('engagement','suppression'), years=2010:2030)
