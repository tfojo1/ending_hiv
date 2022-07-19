
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
                                                    rate = 'depression.treatment.rate',
                                                    label = 'treated.depression')


DEPRESSION.TRANSITION.MAPPING = register.transition(DEPRESSION.TRANSITION.MAPPING,
                                                    dimension = 'subpopulation',
                                                    subgroups = 'hiv.negative',
                                                    from.state = 'treated_depression',
                                                    to.state = 'untreated_depression',
                                                    rate = 'discontinuation.rate',
                                                    label = 'discontinued.depression')


DEPRESSION.TRANSITION.MAPPING = register.transition(DEPRESSION.TRANSITION.MAPPING,
                                                    dimension = 'subpopulation',
                                                    subgroups = 'hiv.positive',
                                                    from.state = 'treated_depression',
                                                    to.state = 'untreated_depression',
                                                    rate = 'discontinuation.rate',
                                                    label = 'discontinued.depression')

DEPRESSION.TRANSITION.MAPPING = register.transition(DEPRESSION.TRANSITION.MAPPING,
                                                    dimension = 'subpopulation',
                                                    subgroups = 'hiv.negative',
                                                    from.state = 'treated_depression',
                                                    to.state = 'no_depression',
                                                    rate = 'remission.rate',
                                                    label = 'treated.depression.remission')


DEPRESSION.TRANSITION.MAPPING = register.transition(DEPRESSION.TRANSITION.MAPPING,
                                                    dimension = 'subpopulation',
                                                    subgroups = 'hiv.positive',
                                                    from.state = 'treated_depression',
                                                    to.state = 'no_depression',
                                                    rate = 'remission.rate',
                                                    label = 'treated.depression.remission')



DEPRESSION.TRANSITION.MAPPING = register.transition(DEPRESSION.TRANSITION.MAPPING,
                                                    dimension = 'subpopulation',
                                                    subgroups = 'hiv.negative',
                                                    from.state = 'untreated_depression',
                                                    to.state = 'no_depression',
                                                    rate = 'remission.rate',
                                                      label = 'untreated.depression.remission')


DEPRESSION.TRANSITION.MAPPING = register.transition(DEPRESSION.TRANSITION.MAPPING,
                                                    dimension = 'subpopulation',
                                                    subgroups = 'hiv.positive',
                                                    from.state = 'untreated_depression',
                                                    to.state = 'no_depression', #add multiplier
                                                    rate = 'remission.rate',
                                                    label = 'untreated.depression.remission')





# Need to register the transition elements
DEPRESSION.TRANSITION.MAPPING = register.transition.element(DEPRESSION.TRANSITION.MAPPING,
                                                            name='depression.incidence',
                                                            type = 'rate',
                                                            model.source = 'comorbidities.manager')

DEPRESSION.TRANSITION.MAPPING = register.transition.element(DEPRESSION.TRANSITION.MAPPING,
                                                            name='hiv.vs.nonhiv.depression.incidence.rr',
                                                            type='rate')




#@ Ruchita - fill in all you other parameters


# Put it all together
VERSION.MANAGER = copy.and.modify.jheem.settings(
    template.settings = get.settings.for.version('expanded_1.0'),
    
    version='depression_1.0',
    directory.suffix='_expanded',
    
    subpopulations = DEPRESSION.SUBPOPULATIONS,
    
    transition.mapping = DEPRESSION.TRANSITION.MAPPING
)