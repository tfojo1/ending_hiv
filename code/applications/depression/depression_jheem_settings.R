
# depends on 
# jheem_settings.R
# ehe_jheem_settings.R

DEPRESSION.SUBPOPULATIONS = c('no_depression',
                              'untreated_depression',
                              'treated_depression')

DEPRESSION.TRANSITION.MAPPING = copy.transition.mapping(template=EXPANDED.CONTINUUM.TRANSITION.MAPPING,
                                                                version='depression_1.0')


DEPRESSION.TRANSITION.MAPPING = register.transition(DEPRESSION.TRANSITION.MAPPING,
                                                    dimension = 'subpopulation',
                                                    subgroups = 'hiv.negative',
                                                    from.state = 'no_depression',
                                                    to.state = 'untreated_depression',
                                                    rate = 'depression.incidence',
                                                    label = 'incident.depression')

DEPRESSION.TRANSITION.MAPPING = register.transition(DEPRESSION.TRANSITION.MAPPING,
                                                    dimension = 'subpopulation',
                                                    subgroups = 'hiv.positive',
                                                    from.state = 'no_depression',
                                                    to.state = 'untreated_depression',
                                                    rate = expression(depression.incidence * hiv.vs.nonhiv.depression.incidence.rr),
                                                    label = 'incident.depression')



DEPRESSION.TRANSITION.MAPPING = register.transition(DEPRESSION.TRANSITION.MAPPING,
                                                    dimension = 'subpopulation',
                                                    subgroups = 'hiv.negative',
                                                    from.state = 'untreated_depression',
                                                    to.state = 'treated_depression',
                                                    rate = 'depression.treatment.rate',
                                                    label = 'treated.depression')


DEPRESSION.TRANSITION.MAPPING = register.transition(DEPRESSION.TRANSITION.MAPPING,
                                                    dimension = 'subpopulation',
                                                    subgroups = 'hiv.positive',
                                                    from.state = 'untreated_depression',
                                                    to.state = 'treated_depression',
                                                    rate = expression(depression.treatment.rate*hiv.vs.nonhiv.depression.treatment.rr),
                                                    label = 'treated.depression')


DEPRESSION.TRANSITION.MAPPING = register.transition(DEPRESSION.TRANSITION.MAPPING,
                                                    dimension = 'subpopulation',
                                                    subgroups = 'hiv.negative',
                                                    from.state = 'treated_depression',
                                                    to.state = 'untreated_depression',
                                                    rate = 'depression.treatment.discontinuation.rate',
                                                    label = 'discontinued.depression')


DEPRESSION.TRANSITION.MAPPING = register.transition(DEPRESSION.TRANSITION.MAPPING,
                                                    dimension = 'subpopulation',
                                                    subgroups = 'hiv.positive',
                                                    from.state = 'treated_depression',
                                                    to.state = 'untreated_depression',
                                                    rate = expression(depression.treatment.discontinuation.rate*hiv.vs.nonhiv.depression.treatment.discontinuation.rate.rr),
                                                    label = 'discontinued.depression')

DEPRESSION.TRANSITION.MAPPING = register.transition(DEPRESSION.TRANSITION.MAPPING,
                                                    dimension = 'subpopulation',
                                                    subgroups = 'hiv.negative',
                                                    from.state = 'treated_depression',
                                                    to.state = 'no_depression',
                                                    rate = 'depression.remission.rate',
                                                    label = 'treated.depression.remission')


DEPRESSION.TRANSITION.MAPPING = register.transition(DEPRESSION.TRANSITION.MAPPING,
                                                    dimension = 'subpopulation',
                                                    subgroups = 'hiv.positive',
                                                    from.state = 'treated_depression',
                                                    to.state = 'no_depression',
                                                    rate = expression(depression.remission.rate*hiv.vs.nonhiv.depression.remission.rate.rr),
                                                    label = 'treated.depression.remission')



DEPRESSION.TRANSITION.MAPPING = register.transition(DEPRESSION.TRANSITION.MAPPING,
                                                    dimension = 'subpopulation',
                                                    subgroups = 'hiv.negative',
                                                    from.state = 'untreated_depression',
                                                    to.state = 'no_depression',
                                                    rate = 'depression.remission.rate',
                                                    label = 'untreated.depression.remission')


DEPRESSION.TRANSITION.MAPPING = register.transition(DEPRESSION.TRANSITION.MAPPING,
                                                    dimension = 'subpopulation',
                                                    subgroups = 'hiv.positive',
                                                    from.state = 'untreated_depression',
                                                    to.state = 'no_depression', #add multiplier
                                                    rate = expression(depression.remission.rate* hiv.vs.nonhiv.depression.remission.rate.rr),
                                                    label = 'untreated.depression.remission')





# Need to register the transition elements
DEPRESSION.TRANSITION.MAPPING = register.transition.element(DEPRESSION.TRANSITION.MAPPING,
                                                            name='depression.incidence',
                                                            type = 'rate',
                                                            model.source = 'comorbidities.manager')

DEPRESSION.TRANSITION.MAPPING = register.transition.element(DEPRESSION.TRANSITION.MAPPING,
                                                            name='hiv.vs.nonhiv.depression.incidence.rr',
                                                            type='rate')

DEPRESSION.TRANSITION.MAPPING = register.transition.element(DEPRESSION.TRANSITION.MAPPING,
                                                            name='depression.treatment.rate',
                                                            type='rate',
                                                            model.source = 'comorbidities.manager')

DEPRESSION.TRANSITION.MAPPING = register.transition.element(DEPRESSION.TRANSITION.MAPPING,
                                                            name='hiv.vs.nonhiv.depression.treatment.rr',
                                                            type='rate')

DEPRESSION.TRANSITION.MAPPING = register.transition.element(DEPRESSION.TRANSITION.MAPPING,
                                                            name='depression.treatment.discontinuation.rate',
                                                            type='rate',
                                                            model.source = 'comorbidities.manager')

DEPRESSION.TRANSITION.MAPPING = register.transition.element(DEPRESSION.TRANSITION.MAPPING,
                                                            name='hiv.vs.nonhiv.depression.treatment.discontinuation.rate.rr',
                                                            type='rate')

DEPRESSION.TRANSITION.MAPPING = register.transition.element(DEPRESSION.TRANSITION.MAPPING,
                                                            name='depression.remission.rate',
                                                            type='rate',
                                                            model.source = 'comorbidities.manager')

DEPRESSION.TRANSITION.MAPPING = register.transition.element(DEPRESSION.TRANSITION.MAPPING,
                                                            name='hiv.vs.nonhiv.depression.remission.rate.rr',
                                                            type='rate')




# Put it all together
VERSION.MANAGER = copy.and.modify.jheem.settings(
    template.settings = get.settings.for.version('expanded_1.0'),
    
    version='depression_1.0',
    directory.suffix='_depression',
    file.version='dep1.0',
    
    subpopulations = DEPRESSION.SUBPOPULATIONS,
    subpopulation.birth.proportions = 'no_depression',
    
    transition.mapping = DEPRESSION.TRANSITION.MAPPING
)