

##-------------------------------------------------------------##
##-- HELPERS FOR MANAGING DEPENDENCIES AND FIXING COMPONENTS --##
##-------------------------------------------------------------##

# This function is no longer in use
# Keeping the code around for a bit until we're sure we no longer need it
OLD.clear.calibration.dependencies <- function(components)
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
                        total.proportion.tested.or='background.testing',
                        msm.proportion.tested.or='background.testing',
                        idu.proportion.tested.or='background.testing',
                        total.testing.slope.or='background.testing',
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
                        prep.rr = c('prep.rr.heterosexual','prep.rr.msm','prep.rr.idu'),
                        other=c('background.prep',
                                'background.linkage',
                                
                                'background.naive.to.suppressed',
                                'background.naive.to.disengaged',
                                'background.start.art',
                                
                                'background.failing.to.suppressed',
                                'background.failing.to.disengaged',
                                
                                'background.suppressed.to.failing',
                                'background.suppressed.to.disengaged',
                                
                                'background.reengagement'),
                        
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
    dependencies.to.clear = get.tracked.cleared.dependencies(components)
    components = clear.dependent.values(components, dependencies.to.clear)
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


#-- SETTING ALPHAS --#

set.intercept.and.slope.alphas.from.parameters <- function(components,
                                                           type,
                                                           parameters,
                                                           intercept.parameter.suffix,
                                                           slope.parameter.suffix,
                                                           idu.applies.to.in.remission,
                                                           transformation = 'identity',
                                                           sex.risk.intercept.multiplier = 1,
                                                           exclude.from.check = character())
{
    components = set.alphas.from.parameters(components,
                                            type=type,
                                            category='intercept',
                                            parameters=parameters,
                                            parameter.suffix=intercept.parameter.suffix,
                                            idu.applies.to.in.remission = idu.applies.to.in.remission,
                                            transformation=transformation,
                                            sex.risk.multiplier=sex.risk.intercept.multiplier,
                                            exclude.from.check=exclude.from.check)
    
    if (!is.null(slope.parameter.suffix))
        components = set.alphas.from.parameters(components,
                                                type=type,
                                                category='slope',
                                                parameters=parameters,
                                                parameter.suffix=slope.parameter.suffix,
                                                idu.applies.to.in.remission = idu.applies.to.in.remission,
                                                transformation=transformation,
                                                sex.risk.multiplier=1,
                                                exclude.from.check=exclude.from.check)
    
    components
}

# sex.risk.multiplier gets applied (multiplied in) BEFORE applying transformation
set.alphas.from.parameters <- function(components,
                                       type,
                                       category,
                                       parameters,
                                       parameter.suffix,
                                       idu.applies.to.in.remission,
                                       transformation,
                                       sex.risk.multiplier,
                                       exclude.from.check=character())
{
    if (length(sex.risk.multiplier)!=1 || is.na(sex.risk.multiplier))
        stop("sex.risk.multiplier must be a single value that is not NA")
    
    #-- Set up parameter names --#
    
    # By Race
    race.prefixes = c(
        # Race
        black = 'black',
        hispanic = 'hispanic'
    )
    
    # By Sex/Risk
    non.idu.sex.prefixes = c(
        heterosexual_male = 'heterosexual',
        female = 'heterosexual',
        msm = 'msm'
    )
    active.idu.sex.prefixes = c(
        heterosexual_male = 'idu',
        female = 'idu',
        msm = 'msm.idu'
    )
    if (idu.applies.to.in.remission)
        idu.in.remission.sex.prefixes = active.idu.sex.prefixes
    else
        idu.in.remission.sex.prefixes = non.idu.sex.prefixes
    
    names(non.idu.sex.prefixes) = collapse.dim.values(names(non.idu.sex.prefixes), 'never_IDU')
    names(active.idu.sex.prefixes) = collapse.dim.values(names(active.idu.sex.prefixes), 'active_IDU')
    names(idu.in.remission.sex.prefixes) = collapse.dim.values(names(idu.in.remission.sex.prefixes), 'IDU_in_remission')
    
    sex.risk.prefixes = c(non.idu.sex.prefixes,
                          active.idu.sex.prefixes,
                          idu.in.remission.sex.prefixes)
    
    # By Age
    age.strata = c(1,2,4,5)
    age.prefixes = paste0('age', age.strata)
    names(age.prefixes) = get.components.settings(components)$DIMENSION.NAMES$age[age.strata]
    
    # Pull it together
    all.prefixes = c(age.prefixes,
                     race.prefixes,
                     sex.risk.prefixes)
    
    all.parameter.names = paste0(all.prefixes, ".", parameter.suffix)
    names(all.parameter.names) = names(all.prefixes)
    
    all.dimensions = c(rep('age', length(age.prefixes)),
                       rep('race', length(race.prefixes)),
                       rep(collapse.dim.values('sex','risk'), length(sex.risk.prefixes)))

    
    # A check to make sure we're neglecting to use any matching parameters
    matching.suffix.mask = paste0('.', parameter.suffix) ==
        substr(names(parameters), nchar(names(parameters))-nchar(parameter.suffix), nchar(names(parameters)))
    matching.suffix.names = names(parameters)[matching.suffix.mask]
    unused.parameter.names = setdiff(matching.suffix.names, all.parameter.names)
    unused.parameter.names = setdiff(unused.parameter.names, exclude.from.check)
    if (length(unused.parameter.names)>0)
    {
        stop(paste0("When setting ", category, " alphas on '", type, 
                    "', we are not using the following parameter(s): ",
                    paste0("'", unused.parameter.names, "'", collapse=', '),
                    ". Is this intentional?"))
    }
    
    # Pull the values
    all.parameter.values = parameters[all.parameter.names]
    names(all.parameter.values) = names(all.parameter.names)
    all.parameter.values[names(sex.risk.prefixes)] = all.parameter.values[names(sex.risk.prefixes)] * sex.risk.multiplier
    
    values.present = !is.na(all.parameter.values)
    all.parameter.values = all.parameter.values[values.present]
    all.dimensions = all.dimensions[values.present]
    all.prefixes = all.prefixes[values.present]
    
    if (!any(values.present))
        stop(paste0("No values are present with the suffix '", parameter.suffix,
                    "' in the given parameters vector."))
    
    if (transformation=='reciprocal')
        all.parameter.values = 1/all.parameter.values
    else if (transformation!='identity')
        stop(paste0("Invalid transformation for setting alphas: '", transformation, "'"))
    
    # Set it to components
    components = do.set.transition.alphas.for.category(components,
                                            type=type,
                                            category=category,
                                            values=all.parameter.values,
                                            dim.values=names(all.prefixes),
                                            dimensions=all.dimensions,
                                            interact.sex.risk=T,
                                            as.interaction=F)

    # Return
    components
}
