
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


# Need to register the transition elements

VERSION.MANAGER = copy.and.modify.jheem.settings(
    template.settings = get.settings.for.version('expanded_1.0'),
    
    version='depression_1.0',
    directory.suffix='_expanded',
    
    subpopulations = DEPRESSION.SUBPOPULATIONS,
    
    transition.mapping = DEPRESSION.TRANSITION.MAPPING
)