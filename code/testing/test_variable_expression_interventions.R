
load('mcmc_runs/quick_simsets/1.0_12580_full.Rdata')
base = subset.simset(simset, 1:5)
base = prepare.simset.for.interventions(base)
noint = run.simset.intervention(base, NO.INTERVENTION)

int = run.simset.intervention(base, intervention.eg)


plot.calibration(c(noint,int), years=2020:2030, data.types='incidence', plot.individual.simset.sims = F)



persistence.to.coverage = function(persistence)
{
    lambda = -log(persistence)
    (1/lambda - exp(-lambda)/lambda)
}

ORAL.VARIABLE.50.50 = create.intervention.unit(type='rr.prep',
                                             start.year=2014,
                                             years = c(2014.001,
                                                       2023,
                                                       2027),
                                             rates=c('oral.rr',
                                                     'oral.rr',
                                                     expression(0.5*oral.rr + 0.5*inj.rr*oral.rr)))

PREP.ORAL.INJ.25.25 = create.intervention.unit(type='prep',
                                             start.year=2023,
                                             years=2027,
                                             apply.function = 'additive',
                                             rates=expression(.25*persistence.to.coverage(oral.persistence) + .25*persistence.to.coverage(inj.persistence)))

int = create.intervention(ALL.MSM,
                          ORAL.VARIABLE.50.50,
                          PREP.ORAL.INJ.25.25,
                          Lognormal.Distribution(meanlog=log(.14), sdlog=1, var.name = 'oral.rr'),
                          Lognormal.Distribution(meanlog=log(.34), sdlog=1, var.name = 'inj.rr'),
                          Normal.Distribution(mean=.6, sd=.05, upper=1,lower=0, var.name = 'oral.persistence'),
                          Normal.Distribution(mean=.8, sd=.03, upper=1,lower=0, var.name = 'inj.persistence'))


simset.int = run.simset.intervention(base, int)


plot.calibration(list(noint, simset.int), years=2010:2030, plot.individual.simset.sims = F,
                 data.types=c('incidence','new'))

extract.prep.coverage(simset.int@simulations[[1]], years=2020:2030, keep.dimensions = c('year','sex'))
