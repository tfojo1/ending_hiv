DEPRESSION.PARAMETERS.PRIOR = join.distributions(
    suppression.untreated.vs.no.depression.or = Lognormal.Distribution((log(.96)+log(.89))/2, (log(.96)-log(.89))/4), #https://link.springer.com/article/10.1007/s10461-019-02613-6
    suppression.treated.vs.untreated.depression.or = Lognormal.Distribution((log(1.03)+log(1.06))/2,(log(1.06)-log(1.03))/4), 
    
    lost.untreated.vs.no.depression.or = Lognormal.Distribution((log(1/.8)+log(1/.97))/2, (log(1/.8)-log(1/.97))/4),
    lost.treated.vs.untreated.depression.or = Lognormal.Distribution((log(1/1.36)+log(1/1.57))/2, (log(1/1.36)-log(1/1.57))/4),
    
    depression.incidence.rr = Lognormal.Distribution(0, log(2)/2),
    depression.remission.rr = Lognormal.Distribution(0, log(2)/2),
    
    depression.rr.age1 = Lognormal.Distribution(0, log(2)/2),
    #    depression.rr.age2 = Lognormal.Distribution(0, log(2)/2),
    #    depression.rr.age4 = Lognormal.Distribution(0, log(2)/2),
    #    depression.rr.age5 = Lognormal.Distribution(0, log(2)/2),
    
    depression.incidence.hiv.vs.uninfected.rr = Lognormal.Distribution(0, log(2)/2, lower = 1),
    depression.remission.hiv.vs.uninfected.rr = Lognormal.Distribution(0, log(2)/2, upper = 1),
    
    depression.treatment.initiation.rr = Lognormal.Distribution(0, log(2)/2),
    depression.treatment.discontinuation.rr = Lognormal.Distribution(0, log(2)/2),
    
    depression.treatment.initiation.hiv.vs.uninfected.rr = Lognormal.Distribution(0, log(2)/2),
    depression.treatment.discontinuation.hiv.vs.uninfected.rr = Lognormal.Distribution(0, log(2)/2),
    #From here continue
    sexual.susceptibility.untreated.vs.no.depression.male.rr = Lognormal.Distribution((log(.69)+log(1.11))/2, (log(1.11)-log(.69))/4),
    sexual.susceptibility.untreated.vs.no.depression.female.rr = Lognormal.Distribution((log(.93)+log(.63))/2, (log(.93)-log(.63))/4),
    sexual.susceptibility.treated.vs.untreated.depression.rr = Lognormal.Distribution(0, 1), #Unable to find
    
    idu.susceptibility.untreated.vs.no.depression.male.rr = Lognormal.Distribution((log(.69)+log(1.11))/2, (log(1.11)-log(.69))/4),
    idu.susceptibility.untreated.vs.no.depression.female.rr = Lognormal.Distribution((log(.93)+log(.63))/2, (log(.93)-log(.63))/4),
    idu.susceptibility.treated.vs.untreated.depression.rr = Lognormal.Distribution(0, 1), #Unable to find
    
    testing.untreated.vs.no.depression.or = Lognormal.Distribution(log(1.34), .1),
    testing.treated.vs.untreated.depression.or = Lognormal.Distribution(0, 1), #Unable tofind
    
    idu.incidence.untreated.vs.no.depression.rr= Lognormal.Distribution(0, log(2)/2),
    idu.remission.untreated.vs.no.depression.rr= Lognormal.Distribution(0, log(2)/2),
    idu.relapse.untreated.vs.no.depression.rr= Lognormal.Distribution(0, log(2)/2),
    
    idu.incidence.treated.vs.untreated.depression.rr= Lognormal.Distribution(0, log(2)/2),
    idu.remission.treated.vs.untreated.depression.rr= Lognormal.Distribution(0, log(2)/2),
    idu.relapse.treated.vs.untreated.depression.rr= Lognormal.Distribution(0, log(2)/2)
)

VERSION.MANAGER = register.parameters.prior('depression_1.0',
                                            prior=DEPRESSION.PARAMETERS.PRIOR,
                                            join.with.previous.version.prior = T)

# parameters is a vector, with names the same as the distribution above
# this function needs to tell the components object about each of the parameters
DEPRESSION.GET.COMPONENTS.FOR.PARAMETERS <- function(parameters, components,
                                                     data.managers = ALL.DATA.MANAGERS)
{
    age1.name = get.components.settings(components)$AGES$labels[1]
    
    #-- Parameters that affect the HIV continuum of care --#
    
    # Suppression
    suppression.ors = c('untreated_depression' = parameters['suppression.untreated.vs.no.depression.or'],
                        'treated_depression' = parameters['suppression.treated.vs.untreated.depression.or'] * 
                            parameters['suppression.untreated.vs.no.depression.or'])
    names(suppression.ors) = c('untreated_depression', 'treated_depression')
    
    components = set.transition.intercept.alphas(components,
                                                 type='failing.to.suppressed',
                                                 values=suppression.ors,
                                                 dimensions='subpopulation')
    components = set.transition.intercept.alphas(components,
                                                 type='recently.suppressed.to.failing',
                                                 values=1/suppression.ors,
                                                 dimensions='subpopulation')
    components = set.transition.intercept.alphas(components,
                                                 type='durably.suppressed.to.failing',
                                                 values=1/suppression.ors,
                                                 dimensions='subpopulation')
    components = set.transition.intercept.alphas(components,
                                                 type='naive.to.suppressed',
                                                 values=suppression.ors,
                                                 dimensions='subpopulation')
    
    # Retention / Reengagement
    lost.ors = c('untreated_depression' = parameters['lost.untreated.vs.no.depression.or'],
                 'treated_depression' = parameters['lost.treated.vs.untreated.depression.or'] * 
                     parameters['lost.untreated.vs.no.depression.or'])
    names(lost.ors) = c('untreated_depression', 'treated_depression')
    
    components = set.transition.intercept.alphas(components,
                                                 type='recently.suppressed.to.disengaged',
                                                 values=lost.ors,
                                                 dimensions='subpopulation')
    components = set.transition.intercept.alphas(components,
                                                 type='durably.suppressed.to.disengaged',
                                                 values=lost.ors,
                                                 dimensions='subpopulation')
    components = set.transition.intercept.alphas(components,
                                                 type='failing.to.disengaged',
                                                 values=lost.ors,
                                                 dimensions='subpopulation')
    components = set.transition.intercept.alphas(components,
                                                 type='naive.to.disengaged',
                                                 values=lost.ors,
                                                 dimensions='subpopulation')
    
    components = set.transition.intercept.alphas(components,
                                                 type='reengagement',
                                                 values=1/lost.ors,
                                                 dimensions='subpopulation')
    
    # Parameters that affect moving between depression states
    
    incidence.values =c(all=1,
                        age1=parameters['depression.rr.age1']) * parameters['depression.incidence.rr']
    names(incidence.values) = c('all', age1.name)
    components = set.transition.intercept.alphas(components,
                                                 type='depression.incidence',
                                                 values=incidence.values,
                                                 dimensions=c('all','age'))
    
    components = set.transition.intercept.alphas(components,
                                                 type='depression.remission.rate',
                                                 values=parameters['depression.remission.rr'],
                                                 dimensions='all')
    
    components = set.transition.intercept.alphas(components,
                                                 type='depression.treatment.rate',
                                                 values=parameters['depression.treatment.initiation.rr'],
                                                 dimensions='all')
    
    components = set.transition.intercept.alphas(components,
                                                 type='depression.treatment.discontinuation.rate',
                                                 values=parameters['depression.treatment.discontinuation.rr'],
                                                 dimensions='all')
    
    
    components = set.static.parameter(components, 
                                      parameter.name = 'hiv.vs.nonhiv.depression.incidence.rr',
                                      parameter.value = parameters['depression.incidence.hiv.vs.uninfected.rr'])
    
    components = set.static.parameter(components, 
                                      parameter.name = 'hiv.vs.nonhiv.depression.remission.rate.rr',
                                      parameter.value = parameters['depression.remission.hiv.vs.uninfected.rr'])
    
    components = set.static.parameter(components, 
                                      parameter.name = 'hiv.vs.nonhiv.depression.treatment.rr',
                                      parameter.value = parameters['depression.treatment.initiation.rr'])
    
    components = set.static.parameter(components, 
                                      parameter.name = 'hiv.vs.nonhiv.depression.treatment.discontinuation.rate.rr',
                                      parameter.value = parameters['depression.treatment.discontinuation.rr'])
    
    # Parameters that affect HIV transmission/susceptibility
    
    components = set.susceptibility.alphas(components,
                                           mode='sexual',
                                           values=parameters['sexual.susceptibility.untreated.vs.no.depression.male.rr'], 
                                           dim.values=c('untreated_depression','msm','heterosexual_male'),
                                           dimensions=c('subpopulation', 'sex', 'sex'),
                                           as.interaction = T,
                                           interact.sex.risk = F)
    components = set.susceptibility.alphas(components,
                                           mode='sexual',
                                           values=parameters['sexual.susceptibility.untreated.vs.no.depression.male.rr'] *
                                               parameters['sexual.susceptibility.treated.vs.untreated.depression.rr'], 
                                           dim.values=c('treated_depression','msm','heterosexual_male'),
                                           dimensions=c('subpopulation', 'sex', 'sex'),
                                           as.interaction = T,
                                           interact.sex.risk = F)
    
    components = set.susceptibility.alphas(components,
                                           mode='sexual',
                                           values=parameters['sexual.susceptibility.untreated.vs.no.depression.female.rr'], 
                                           dim.values=c('untreated_depression','female'),
                                           dimensions=c('subpopulation', 'sex'),
                                           as.interaction = T,
                                           interact.sex.risk = F)
    components = set.susceptibility.alphas(components,
                                           mode='sexual',
                                           values=parameters['sexual.susceptibility.untreated.vs.no.depression.female.rr'] *
                                               parameters['sexual.susceptibility.treated.vs.untreated.depression.rr'], 
                                           dim.values=c('treated_depression','female'),
                                           dimensions=c('subpopulation', 'sex'),
                                           as.interaction = T,
                                           interact.sex.risk = F)
    
    
    components = set.susceptibility.alphas(components,
                                           mode='idu',
                                           values=parameters['idu.susceptibility.untreated.vs.no.depression.male.rr'], 
                                           dim.values=c('untreated_depression','msm','heterosexual_male'),
                                           dimensions=c('subpopulation', 'sex', 'sex'),
                                           as.interaction = T,
                                           interact.sex.risk = F)
    components = set.susceptibility.alphas(components,
                                           mode='idu',
                                           values=parameters['idu.susceptibility.untreated.vs.no.depression.male.rr'] *
                                               parameters['idu.susceptibility.treated.vs.untreated.depression.rr'], 
                                           dim.values=c('treated_depression','msm','heterosexual_male'),
                                           dimensions=c('subpopulation', 'sex', 'sex'),
                                           as.interaction = T,
                                           interact.sex.risk = F)
    
    components = set.susceptibility.alphas(components,
                                           mode='idu',
                                           values=parameters['idu.susceptibility.untreated.vs.no.depression.female.rr'], 
                                           dim.values=c('untreated_depression','female'),
                                           dimensions=c('subpopulation', 'sex'),
                                           as.interaction = T,
                                           interact.sex.risk = F)
    components = set.susceptibility.alphas(components,
                                           mode='idu',
                                           values=parameters['idu.susceptibility.untreated.vs.no.depression.female.rr'] *
                                               parameters['idu.susceptibility.treated.vs.untreated.depression.rr'], 
                                           dim.values=c('treated_depression','female'),
                                           dimensions=c('subpopulation', 'sex'),
                                           as.interaction = T,
                                           interact.sex.risk = F)
    
    
    # Parameters that affect testing
    testing.alphas = c(untreated_depression = parameters['testing.untreated.vs.no.depression.or'],
                       treated_depression = parameters['testing.untreated.vs.no.depression.or'] *
                           parameters['testing.treated.vs.untreated.depression.or'])
    names(testing.alphas) = c('untreated_depression', 'treated_depression')
    components = set.transition.intercept.alphas(components,
                                                 type='testing',
                                                 values=testing.alphas,
                                                 dimension='subpopulation')
    
    # Parameters that affect incidence/remission of IDU
    #-- still need to write the back-end code for this --#
    
    # Return
    components
}

VERSION.MANAGER = register.get.components.function('depression_1.0',
                                                   fn = DEPRESSION.GET.COMPONENTS.FOR.PARAMETERS,
                                                   join.with.previous.version.function = T)