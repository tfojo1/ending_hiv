
EHE.TRANSITION.MAPPING = create.transition.mapping(version='collapsed_1.0')
EHE.TRANSITION.MAPPING = register.transition(EHE.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='undiagnosed_from_prep',
                                                         to.state='diagnosed',
                                                         rate = expression( 1 / prep.screening.frequency),
                                                         label = 'diagnosis')
EHE.TRANSITION.MAPPING = register.transition(EHE.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='undiagnosed_from_prep',
                                                         to.state='undiagnosed',
                                                         rate = expression(-log(1-prep.persistence)),
                                                         label = 'prep.drop.out')
EHE.TRANSITION.MAPPING = register.transition(EHE.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='undiagnosed',
                                                         to.state='diagnosed',
                                                         rate = 'testing',
                                                         label = 'diagnosis')

EHE.TRANSITION.MAPPING = register.transition.element(EHE.TRANSITION.MAPPING,
                                                      name='prep.screening.frequency',
                                                      type='time')
EHE.TRANSITION.MAPPING = register.transition.element(EHE.TRANSITION.MAPPING,
                                                      name='prep.persistence',
                                                      type='proportion')

EHE.TRANSITION.MAPPING = register.transition.element(EHE.TRANSITION.MAPPING,
                                                      name='testing',
                                                      background.model.type = 'proportion',
                                                     ramp.type = 'proportion',
                                                     return.type = 'rate',
                                                     ramp.times = c(1981,1982,1993),
                                                     ramp.multipliers = c(0,NA,NA),
                                                     ramp.interpolate.scales = c('identity','log','identity'),
                                                     model.source = 'continuum.manager')


VERSION.MANAGER = create.jheem.settings(
    version='collapsed_1.0',
    prior.versions=character(),
    directory.suffix='_collapsed',
    
    age.cutoffs=c(13,25,35,45,55,Inf),
    races=c('black','hispanic','other'),
    locations='all_locations',
    subpopulations='all_subpopulations',
    sexes= c('heterosexual_male', 'msm', 'female'),
    risks = c('never_IDU', 'active_IDU', 'IDU_in_remission'),
    non.hiv.subsets='all_hiv_negative',
    continuum.of.care = c('undiagnosed', 'undiagnosed_from_prep', 'diagnosed'), # 'unsuppressed', 'suppressed'),
    cd4.strata = c('acute', 'chronic'),
    hiv.subsets='all_hiv_positive',
    
    first.diagnosed.continuum.states='diagnosed',
    diagnosed.continuum.states='diagnosed',
    undiagnosed.from.prep.continuum.states='undiagnosed_from_prep',
    undiagnosed.no.prep.continuum.states='undiagnosed',
    engaged.continuum.states=NA,
    suppressed.continuum.states=NA,
    
    acute.cd4.strata = 'acute',
    chronic.cd4.strata = 'chronic',
    
    active.idu.risk.states = 'active_IDU',
    
    transition.mapping = EHE.TRANSITION.MAPPING,
    
    is.continuum.collapsed=T
)
