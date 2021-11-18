
T0 = 2000 #2004
T1 = 2010
T2 = 2016
T1.IDU = 2010 #2008
T0.5 = 2006
YOUNG.T0=2000

setup.initial.components <- function(msa=BALTIMORE.MSA,
                                     settings=SETTINGS,
                                     population.year=2007,
                                     max.smoothed.suppressed.proportion=0.8,

                                     msm.pre.spike.year = msm.spike.start.year-10,
                                     msm.spike.start.year = msm.spike.end.year-10,
                                     msm.spike.end.year = 1980,
                                     msm.post.spike.year = 1990,
                                     msm.t0 = T0,
                                     msm.t1 = T1,
                                     msm.t0.5 = T0.5,
                                     msm.t2 = T2,
                                     msm.young.t0 = YOUNG.T0,

                                     heterosexual.pre.spike.year = heterosexual.spike.start.year-10,
                                     heterosexual.spike.start.year = heterosexual.spike.end.year-10,
                                     heterosexual.spike.end.year = 1990,
                                     heterosexual.post.spike.year = 2000,
                                     heterosexual.t0 = T0,
                                     heterosexual.t1 = T1,
                                     heterosexual.t0.5 = T0.5,
                                     heterosexual.t2 = T2,
                                     heterosexual.young.t0 = YOUNG.T0,

                                     idu.pre.spike.year = idu.spike.start.year-10,
                                     idu.spike.start.year = idu.spike.end.year-10,
                                     idu.spike.end.year = 1990,
                                     idu.post.spike.year = 2000,
                                     idu.t0 = T0,
                                     idu.t1 = T1.IDU,
                                     idu.t0.5 = T0.5,
                                     idu.t2 = T2,
                                     idu.young.t0 = YOUNG.T0,

                                     mortality.pre.spike.year = 1970,
                                     mortality.spike.start.year=1980,
                                     mortality.spike.end.year=1996, #https://www.thebodypro.com/article/mortality-trends-toward-new-definition-aids
                                     mortality.post.spike.year=2000,
                                     mortality.t0=2000,
                                     mortality.t1=T1,
                                     mortality.t2=T2,

                                     start.spike.mortality.year = 1970,
                                     peak.mortality.year = 1995,
                                     end.spike.mortality.year = 2010,

                                     msm.aging.base.time = 1980-5,
                                     heterosexual.aging.base.time = 1980-5,
                                     idu.aging.base.time = 1980-5,

                                     last.background.year = 2030,

                                     idu.transition.years = c(idu.post.spike.year,idu.t2),

                                     run.to.year = 2030,
                                     fix=F)
{
    msa.components = setup.components.for.msa(msa,
                                              settings=settings,
                                              population.year=population.year,
                                              max.smoothed.suppressed.proportion = max.smoothed.suppressed.proportion,
                                              smooth.to.year = last.background.year,
                                              idu.transition.years=idu.transition.years,
                                              idu.transition.end.year=run.to.year)

    msa.components = setup.trate.years(msa.components,
                                       routes=c('msm','msm.idu'),
                                       t.pre.peak=msm.pre.spike.year,
                                       t.peak.start=msm.spike.start.year,
                                       t.peak.end=msm.spike.end.year,
                                       t0.start=msm.post.spike.year,
                                       t0.end=msm.t0,
                                       t1=msm.t1,
                                       t0.5=msm.t0.5,
                                       t2=msm.t2,
                                       t.end=run.to.year)

    msa.components = setup.trate.years(msa.components,
                                       routes=c('msm','msm.idu'),
                                       age.indices = 1:2,
                                       t0.end = msm.young.t0)

    msa.components = setup.trate.years(msa.components,
                                       routes=c('heterosexual.male','heterosexual.female'),
                                       t.pre.peak=heterosexual.pre.spike.year,
                                       t.peak.start=heterosexual.spike.start.year,
                                       t.peak.end=heterosexual.spike.end.year,
                                       t0.start=heterosexual.post.spike.year,
                                       t0.end=heterosexual.t0,
                                       t1=heterosexual.t1,
                                       t0.5=heterosexual.t0.5,
                                       t2=heterosexual.t2,
                                       t.end=run.to.year)

    msa.components = setup.trate.years(msa.components,
                                       routes=c('heterosexual.male','heterosexual.female'),
                                       age.indices = 1:2,
                                       t0.end = heterosexual.young.t0)

    msa.components = setup.trate.years(msa.components,
                                       routes=c('idu.msm','idu.male','idu.female'),
                                       t.pre.peak=idu.pre.spike.year,
                                       t.peak.start=idu.spike.start.year,
                                       t.peak.end=idu.spike.end.year,
                                       t0.start=idu.post.spike.year,
                                       t0.end=idu.t0,
                                       t1=idu.t1,
                                       t0.5=idu.t0.5,
                                       t2=idu.t2,
                                       t.end=run.to.year)


    msa.components = set.aging.times(msa.components, route='msm',
                                     t.pre.spike=1980,
                                     t.spike=1990,
                                     t0=2000,
                                     t1=2010,
                                     t2=2020,
                                     t3=2030)
    msa.components = set.aging.times(msa.components, route=c('heterosexual.male','heterosexual.female'),
                                     t.pre.spike=1980,
                                     t.spike=1990,
                                     t0=2000,
                                     t1=2010,
                                     t2=2020,
                                     t3=2030)
    msa.components = set.aging.times(msa.components, route='idu',
                                     t.pre.spike=1980,
                                     t.spike=1990,
                                     t0=2000,
                                     t1=2010,
                                     t2=2020,
                                     t3=2030)

    msa.components = setup.trate.years(msa.components,
                                       routes='idu',
                                       age.indices = 1:2,
                                       t0.end = idu.young.t0)

    msa.components = setup.hiv.mortality.years(msa.components,
                                               t.pre.peak=mortality.pre.spike.year,
                                               t.peak.start=mortality.spike.start.year,
                                               t.peak.end=mortality.spike.end.year,
                                               t0.start=mortality.post.spike.year,
                                               t0.end=mortality.t0,
                                               t1=mortality.t1,
                                               t2=mortality.t2,
                                               t.end=run.to.year)

    msa.components = setup.trates(msa.components, fraction.change.after.end=0.05)

    if (fix)
        msa.components = fix.components.for.calibration(msa.components)

    msa.components
}
