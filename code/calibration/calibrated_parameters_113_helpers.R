
##------------------------##
##-- RUN FOR COMPONENTS --##
##------------------------##


run.jheem.from.components <- function(components,
                                      start.year=1970, end.year=2020,
                                      max.run.time.seconds=Inf,
                                      prior.results=NULL,
                                      keep.components=T,
                                      keep.years=start.year:end.year,
                                      atol=atol, rtol=rtol)
{
    #-- Pull JHEEM --#
    jheem = setup.jheem.from.components(components)
    
    #-- Run it -_#
    results = run.jheem(jheem,
                        prior.run.results = prior.results,
                        start.year = start.year, end.year = end.year,
                        verbose=F, print.warnings = F,
                        max.run.time.seconds=max.run.time.seconds,
                        keep.years=keep.years)
    
    #-- Store some attributes --#
    attr(results, 'msm.proportions.by.race') = components$proportions.msm.of.male
    attr(results, 'smoothed.suppressed.proportions') = attr(components, 'smoothed.suppressed.proportions')
    attr(results, 'smoothed.testing.proportions') = attr(components, 'smoothed.testing.proportions')
    
    attr(results, 'location') = attr(components, 'location')
    
    if (keep.components)
        attr(results, 'components') = crunch.intervention.rates(components)
    
    #-- Return it --#
    results
}


##-------------------------------------------------------------##
##-- HELPERS FOR MANAGING DEPENDENCIES AND FIXING COMPONENTS --##
##-------------------------------------------------------------##

clear.calibration.dependencies <- function(components)
{
    dependencies = list(base.heterosexual.transmission=c('male.to.female.sexual.transmission',
                                                         'female.to.male.sexual.transmission'),
                        male.to.male.transmission='male.to.male.sexual.transmission',
                        idu.trate='global.idu.transmission.rates',
                        idu.transmission='idu.transmission',
                        male.vs.female.heterosexual.rr='female.to.male.sexual.transmission',
                        peak.heterosexual.trate.mult=c('male.to.female.sexual.transmission',
                                                       'female.to.male.sexual.transmission'),
                        peak.msm.trate.mult='male.to.male.sexual.transmission',
                        peak.idu.trate.mult='global.idu.transmission.rates',
                        heterosexual.change.ratio.1=c('male.to.female.sexual.transmission',
                                                      'female.to.male.sexual.transmission'),
                        heterosexual.change.ratio.2.to.1=c('male.to.female.sexual.transmission',
                                                           'female.to.male.sexual.transmission'),
                        male.to.male.change.ratio.1='male.to.male.sexual.transmission',
                        male.to.male.change.ratio.2.to.1='male.to.male.sexual.transmission',
                        idu.change.ratio.1='global.idu.transmission.rates',
                        idu.change.ratio.2.to.1='global.idu.transmission.rates',
                        total.proportion.tested.or='background.testing.proportions',
                        msm.proportion.tested.or='background.testing.proportions',
                        idu.proportion.tested.or='background.testing.proportions',
                        total.testing.slope.or='background.testing.proportions',
                        proportion.msm.of.male.mult='proportions.msm.of.male',
                        hiv.mortality='untreated.hiv.mortality',
                        peak.hiv.mortality='untreated.hiv.mortality',
                        oe.female.pairings.with.msm='sexual.transmission',
                        oe.never.idu.pairings.with.idu='sexual.transmission',
                        young.msm.susceptibility='susceptibility',
                        age5.susceptibility.rr='susceptibility',
                        black.msm.susceptibility.rr='susceptibility',
                        hispanic.msm.susceptibility.rr='susceptibility',
                        black.heterosexual.susceptibility.rr='susceptibility',
                        hispanic.heterosexual.susceptibility.rr='susceptibility',
                        black.idu.susceptibility.rr1='susceptibility',
                        hispanic.idu.susceptibility.rr1='susceptibility',
                        black.black.oe='sexual.transmission',
                        hispanic.hispanic.oe='sexual.transmission',
                        other.other.oe='sexual.transmission',
                        age.mixing.sd.mult='sexual.transmission',
                        acute.transmissibility.rr=c('acute.transmissibility.rr','diagnosed.needle.sharing.rr',
                                                    'diagnosed.het.male.condomless.rr','diagnosed.female.condomless.rr','diagnosed.msm.condomless.rr',
                                                    'idu.transmissibility.rr.by.race','sexual.transmissibility.rr.by.race'),
                        diagnosed.transmission.rr.mult=c('acute.transmissibility.rr','diagnosed.needle.sharing.rr',
                                                         'diagnosed.het.male.condomless.rr','diagnosed.female.condomless.rr','diagnosed.msm.condomless.rr',
                                                         'idu.transmissibility.rr.by.race','sexual.transmissibility.rr.by.race'),
                        total.suppressed.or='background.suppression',
                        total.future.suppressed.slope.or='background.suppression',
                        other='background.prep.coverage',
                        aging='aging',
                        idu.transitions=c('incident.idu','idu.remission','idu.relapse'),
                        global.trate=c('global.sexual.transmission.rates','global.idu.transmission.rates'),
                        idu.mortality='excess.idu.mortality')

    dependencies = unique(unlist(dependencies))
    clear.dependent.values(components, dependencies)
}

fix.components.for.calibration <- function(components)
{
    components = crunch.all.jheem.components(components)
    components = clear.calibration.dependencies(components)
    fix.jheem.components(components)
}

##-----------------------------##
##-- HELPERS FOR AGING RATES --##
##-----------------------------##

#assumes that the n in each age = m*a + b
# if we set a=0 for the last age bracket, then
# -35*m + 5*b = n.first.5 
#    and
# -10*m + 5*b = n.second.5
get.aging.rate.last.of.10 <- function(n.first.5, n.second.5)
{
    m = (n.second.5 - n.first.5) / 25
    b = (n.second.5 + 10*m) / 5
    
    b / (n.first.5 + n.second.5)
}

#assumes that the n in each age = m*a + b
# if we set a=0 for the last of the first 10 age brackets, then
# -45*m + 10*b = n.first.10
#    and
# 55*m + 10*b = n.second.10
get.aging.rate.mid.of.20 <- function(n.first.10, n.second.10)
{
    m = (n.second.10 - n.first.10) / 100
    b = (n.first.10 + 45*m) / 10
    
    b / n.first.10
}

##--------------------------------------------------------##
##-- HELPERS TO CREATE MULTIVARIATE COMPONENTS OF PRIOR --##
##--------------------------------------------------------##

create.transmission.prior.distribution <- function(r1.log.mean,
                                                   r1.log.sd,
                                                   rr.2.to.1.log.mean=0,
                                                   rr.2.to.1.log.sd,
                                                   rr.0.to.1.log.mean=rr.2.to.1.log.mean,
                                                   rr.0.to.1.log.sd=rr.2.to.1.log.sd,
                                                   #rr.peak.to.0.log.mean=rr.0.to.1.log.mean,
                                                   #rr.peak.to.0.log.sd=rr.0.to.1.log.sd,
                                                   race='black',
                                                   route=c('msm')
)
{
    mean = c(r1 = r1.log.mean,
             rr.2.to.1 = rr.2.to.1.log.mean,
             rr.0.to.1 = rr.0.to.1.log.mean)#,
#             rr.peak.to.0 = rr.peak.to.0.log.mean)

    var.mat = diag(c(r1.log.sd,
                     rr.2.to.1.log.sd,
                     rr.0.to.1.log.sd)^2)#,
#                     rr.peak.to.0.log.sd)^2)

    M = rbind(r0 = c(1,0,1),
              r1 = c(1,0,0),
              r2 = c(1,1,0))

    Multivariate.Lognormal.Distribution(mu = M %*% mean,
                                        sigma = M %*% var.mat %*% t(M),
                                        var.names = paste0(race,
                                                           '.',
                                                           route,
                                                           '.trate.',
                                                           c(0,1,2))
    )
}

create.mortality.prior.distribution <- function(mort2.log.mean = log(23/1000),
                                                mort2.log.sd = 0.25*log(2),
                                                mort2.to.0.log.mean = log(9.5/6.1),
                                                mort2.to.0.log.sd = 0.25*log(2),
                                                mort2.to.peak.log.mean = log(41/6.1),
                                                mort2.to.peak.log.sd = 0.5*log(2))
{
    mean = c(r2 = mort2.log.mean,
             rr2.to.0 = mort2.to.0.log.mean,
             rr.peak.to.0 = mort2.to.peak.log.mean)

    var.mat = diag(c(r2 = mort2.log.sd,
                rr2.to.0 = mort2.to.0.log.sd,
                rr.peak.to.0 = mort2.to.peak.log.sd))

    M = rbind(r.peak=c(1,0,1),
              r0 = c(1,1,0),
              r2 = c(1,0,0))

    Multivariate.Lognormal.Distribution(mu = M %*% mean,
                                        sigma = M %*% var.mat %*% t(M),
                                        var.names = c('peak.hiv.mortality','hiv.mortality.0','hiv.mortality.2'))
}
