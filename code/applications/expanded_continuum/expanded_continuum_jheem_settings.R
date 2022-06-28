
# depends on 
# jheem_settings.R
# ehe_jheem_settings.R

EXPANDED.CONTINUUM = c('undiagnosed', 
                       'undiagnosed_from_prep', 
                       'unengaged', 
                       'engaged_unsuppressed_naive',
                       'engaged_unsuppressed_failing', 
                       'engaged_suppressed', 
                       #'engaged_recently_suppressed',
                       #'engaged_durably_suppressed',
                       'disengaged_naive',
                       'disengaged_failing')

EXPANDED.CONTINUUM.TRANSITION.MAPPING = copy.transition.mapping(template=EHE.TRANSITION.MAPPING,
                                                                version='expanded_1.0',
                                                                rename.states = c('diagnosed'='unengaged')
)

EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                            dimension='continuum',
                                                            from.state='unengaged',
                                                            to.state='engaged_unsuppressed_naive',
                                                            rate = expression(linkage / time.to.link.vs.disengage),
                                                            label = 'engagement')
EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                            dimension='continuum',
                                                            from.state='unengaged',
                                                            to.state='disengaged_naive',
                                                            rate = expression((1-linkage) / time.to.link.vs.disengage),
                                                            label='failure.to.link')

EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                            dimension='continuum',
                                                            from.state='engaged_unsuppressed_naive',
                                                            to.state='engaged_unsuppressed_failing',
                                                            rate = expression((1-naive.to.suppressed) / (time.to.suppression.on.art + start.art)),
#                                                            rate = expression((1-naive.to.suppressed) / (time.to.suppression.on.art - 1/log(1-start.art))),
                                                            label = 'failure.to.suppress')
EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                            dimension='continuum',
                                                            from.state='engaged_unsuppressed_naive',
                                                            to.state='engaged_suppressed',
                                                            rate = expression(naive.to.suppressed / (time.to.suppression.on.art + start.art)),
#                                                            rate = expression(naive.to.suppressed / (time.to.suppression.on.art -1/log(1-start.art))),
                                                            label = 'gain.of.suppression')
#EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
#                                                                        dimension='continuum',
#                                                                        from.state='engaged_unsuppressed_naive',
#                                                                        to.state='engaged_recently_suppressed',
#                                                                        rate = expression(new.art.suppressed.proportions / (time.to.suppression.on.art + start.art)),
#                                                                        label = 'gain.of.suppression')
EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                            dimension='continuum',
                                                            from.state='engaged_unsuppressed_naive',
                                                            to.state='disengaged_naive',
                                                            rate = 'naive.to.disengaged',
                                                            label = 'loss.to.care')

EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                            dimension='continuum',
                                                            from.state='engaged_unsuppressed_failing',
                                                            to.state='engaged_suppressed',
                                                            rate = 'failing.to.suppressed',
                                                            label = 'gain.of.suppression')
#EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
#                                                                        dimension='continuum',
#                                                                        from.state='engaged_unsuppressed_failing',
#                                                                        to.state='engaged_recently_suppressed',
#                                                                        rate = 'failing.to.suppressed',
#                                                                        label = 'gain.of.suppression')
EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                            dimension='continuum',
                                                            from.state='engaged_unsuppressed_failing',
                                                            to.state='disengaged_failing',
                                                            rate = 'failing.to.disengaged',
                                                            label = 'loss.to.care')

#EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
#                                                                        dimension='continuum',
#                                                                        from.state='engaged_recently_suppressed',
#                                                                        to.state='engaged_durably_suppressed',
#                                                                        rate = expression(1/time.to.durable.suppression),
#                                                                        label = 'continuation.of.suppression')
EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                            dimension='continuum',
                                                            from.state='engaged_suppressed',
                                                            to.state='engaged_unsuppressed_failing',
                                                            rate = 'suppressed.to.failing',
                                                            label = 'loss.of.suppression')
#EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
#                                                                        dimension='continuum',
#                                                                        from.state='engaged_recently_suppressed',
#                                                                        to.state='engaged_unsuppressed_failing',
#                                                                        rate = 'recently.suppressed.to.failing',
#                                                                        label = 'loss.of.suppression')
EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                            dimension='continuum',
                                                            from.state='engaged_suppressed',
                                                            to.state='disengaged_failing',
                                                            rate = 'suppressed.to.disengaged',
                                                            label = 'loss.to.care')
#EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
#                                                                        dimension='continuum',
#                                                                        from.state='engaged_recently_suppressed',
#                                                                        to.state='disengaged_failing',
#                                                                        rate = 'recently.suppressed.to.disengaged',
#                                                                        label = 'loss.to.care')

#EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
#                                                                        dimension='continuum',
#                                                                        from.state='engaged_durably_suppressed',
#                                                                        to.state='engaged_unsuppressed_failing',
#                                                                        rate = 'durably.suppressed.to.failing',
#                                                                        label = 'loss.of.suppression')
#EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
#                                                                        dimension='continuum',
#                                                                        from.state='engaged_durably_suppressed',
#                                                                        to.state='disengaged_failing',
#                                                                        rate = 'durably.suppressed.to.disengaged',
#                                                                        label = 'loss.to.care')

EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                            dimension='continuum',
                                                            from.state='disengaged_naive',
                                                            to.state='engaged_unsuppressed_naive',
                                                            rate = 'reengagement',
                                                            label = 'reengagement')

EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                            dimension='continuum',
                                                            from.state='disengaged_failing',
                                                            to.state='engaged_unsuppressed_failing',
                                                            rate = 'reengagement',
                                                            label = 'reengagement')



EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition.element(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                                    name='time.to.link.vs.disengage',
                                                                    type='time',
                                                                    default.value = 0.25,
                                                                    required=F)
EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition.element(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                                    name='time.to.suppression.on.art',
                                                                    type='time',
                                                                    default.value = 0.25,
                                                                    required=F)

EXPANDED.RAMP.YEAR = 1996
EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition.element(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                                    name='linkage',
                                                                    type = 'proportion',
                                                                    ramp.times = EXPANDED.RAMP.YEAR,
                                                                    ramp.multipliers = 1,
                                                                    model.source = 'continuum.manager')

EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition.element(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                                    name='start.art',
                                                                    background.model.type = 'proportion',
                                                                    ramp.type = 'proportion',
                                                                    return.type = 'time',
                                                                    ramp.times = c(EXPANDED.RAMP.YEAR,2002),
                                                                    ramp.multipliers = c(0,1),
                                                                    model.source = 'continuum.manager')

EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition.element(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                                    name='naive.to.suppressed',
                                                                    type = 'proportion',
                                                                    ramp.times = EXPANDED.RAMP.YEAR,
                                                                    ramp.multipliers = 1,
                                                                    model.source = 'continuum.manager')
EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition.element(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                                    name='naive.to.disengaged',
                                                                    background.model.type = 'proportion',
                                                                    ramp.type = 'proportion',
                                                                    return.type = 'rate',
                                                                    ramp.times = EXPANDED.RAMP.YEAR,
                                                                    ramp.multipliers = 1,
                                                                    model.source = 'continuum.manager')

EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition.element(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                                    name='failing.to.suppressed',
                                                                    background.model.type = 'proportion',
                                                                    ramp.type = 'proportion',
                                                                    return.type = 'rate',
                                                                    ramp.times = EXPANDED.RAMP.YEAR,
                                                                    ramp.multipliers = 1,
                                                                    model.source = 'continuum.manager')
EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition.element(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                                    name='failing.to.disengaged',
                                                                    background.model.type = 'proportion',
                                                                    ramp.type = 'proportion',
                                                                    return.type = 'rate',
                                                                    ramp.times = EXPANDED.RAMP.YEAR,
                                                                    ramp.multipliers = 1,
                                                                    model.source = 'continuum.manager')

EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition.element(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                                    name='suppressed.to.failing',
                                                                    background.model.type = 'proportion',
                                                                    ramp.type = 'proportion',
                                                                    return.type = 'rate',
                                                                    ramp.times = EXPANDED.RAMP.YEAR,
                                                                    ramp.multipliers = 1,
                                                                    model.source = 'continuum.manager')
EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition.element(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                                    name='suppressed.to.disengaged',
                                                                    background.model.type = 'proportion',
                                                                    ramp.type = 'proportion',
                                                                    return.type = 'rate',
                                                                    ramp.times = EXPANDED.RAMP.YEAR,
                                                                    ramp.multipliers = 1,
                                                                    model.source = 'continuum.manager')

EXPANDED.CONTINUUM.TRANSITION.MAPPING = register.transition.element(EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                                    name='reengagement',
                                                                    background.model.type = 'proportion',
                                                                    ramp.type = 'proportion',
                                                                    return.type = 'rate',
                                                                    ramp.times = EXPANDED.RAMP.YEAR,
                                                                    ramp.multipliers = 1,
                                                                    model.source = 'continuum.manager')


VERSION.MANAGER = copy.and.modify.jheem.settings(
    template.settings = get.settings.for.version('collapsed_1.0'),
    
    version='expanded_1.0',
    directory.suffix='_expanded',
    
    continuum.of.care = EXPANDED.CONTINUUM,
    
    first.diagnosed.continuum.states='unengaged',
    diagnosed.continuum.states=setdiff(EXPANDED.CONTINUUM, 
                                       c('undiagnosed', 'undiagnosed_from_prep')),
    undiagnosed.from.prep.continuum.states='undiagnosed_from_prep',
    undiagnosed.no.prep.continuum.states='undiagnosed',
    engaged.continuum.states=c('engaged_unsuppressed_naive',
                               'engaged_unsuppressed_failing', 
                               'engaged_suppressed'
                               #'engaged_recently_suppressed',
                               #'engaged_durably_suppressed'
    ),
    suppressed.continuum.states=c('engaged_suppressed' 
                                  #'engaged_recently_suppressed',
                                  #'engaged_durably_suppressed'
    ),
    
    is.continuum.collapsed=F,
    
    transition.mapping = EXPANDED.CONTINUUM.TRANSITION.MAPPING
)