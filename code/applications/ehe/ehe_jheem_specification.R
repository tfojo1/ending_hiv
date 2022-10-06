
#-- THE TRANSITIONS MAPPING --#
EHE.TRANSITION.MAPPING = create.transition.mapping(version='collapsed_1.0')


#-- PrEP Model Elements and Combos --#

# Standard PrEP
EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                name = 'prep',
                                                scale = 'proportion',
                                                get.model.function = get.prep.model,
                                                model.from.time = 2010,
                                                prep.manager = ALL.DATA.MANAGERS$prep,
                                                ramp.times = 2011,
                                                ramp.multipliers = 0)

EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                name = 'prep.rr',
                                                scale = 'ratio',
                                                model = create.prep.rr.model(rr.msm = BASE_PARAMETER_VALUES['prep.rr.msm'],
                                                                             rr.heterosexual = BASE_PARAMETER_VALUES['prep.rr.heterosexual'],
                                                                             rr.idu = BASE_PARAMETER_VALUES['prep.rr.idu']),
                                                model.from.time = 2010)

EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                     name='prep.persistence',
                                                     scale='proportion',
                                                     value = BASE_PARAMETER_VALUES['prep.persistence'])

# LAI PrEP
EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING, 
                                                     name = 'lai.prep',
                                                     scale = 'proportion',
                                                     value = 0)

EHE.TRANSITION.MAPPING = register.model.quantity(EHE.TRANSITION.MAPPING,
                                                 name = 'lai.prep.rr',
                                                 value = expression(prep.rr * lai.vs.oral.prep.hr))

EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                     name = 'lai.vs.oral.prep.hr',
                                                     scale = 'ratio',
                                                     value = 0.34)

EHE.TRANSITION.MAPPING = register.model.quantity(EHE.TRANSITION.MAPPING,
                                                 name = 'lai.prep.persistence',
                                                 value = expression(1 - (1-prep.persistence) * lai.vs.oral.prep.discontinuation.rr))

EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                     name = 'lai.vs.oral.prep.discontinuation.rr',
                                                     scale = 'ratio',
                                                     value = 1)


# Common to All PrEP / Combinations of PrEP modalities
EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                     name='prep.screening.frequency',
                                                     scale='time',
                                                     value = 0.25) #3 months

EHE.TRANSITION.MAPPING = register.model.quantity(EHE.TRANSITION.MAPPING,
                                                 name = 'all.prep.coverage',
                                                 value = expression(prep + lai.prep))

EHE.TRANSITION.MAPPING = register.model.quantity(EHE.TRANSITION.MAPPING,
                                                 name = 'all.prep.risk',
                                                 value = expression(prep * prep.rr + lai.prep * lai.prep.rr))

EHE.TRANSITION.MAPPING = register.model.quantity(EHE.TRANSITION.MAPPING,
                                                 name = 'all.prep.discontinuation',
                                                 value = expression( (-log(1-prep.persistence) * prep +
                                                                          -log(1-lai.prep.persistence) * lai.prep) /
                                                                         (prep + lai.prep) ))




#-- Continuum Transitions --# 
EHE.TRANSITION.MAPPING = register.transition(EHE.TRANSITION.MAPPING,
                                             dimension='continuum',
                                             from.state='undiagnosed_from_prep',
                                             to.state='diagnosed',
                                             subgroups = 'hiv.positive',
                                             value = expression( 1 / prep.screening.frequency))
EHE.TRANSITION.MAPPING = register.transition(EHE.TRANSITION.MAPPING,
                                             dimension='continuum',
                                             from.state='undiagnosed_from_prep',
                                             to.state='undiagnosed',
                                             subgroups = 'hiv.positive',
                                             value = 'all.prep.discontinuation')
EHE.TRANSITION.MAPPING = register.transition(EHE.TRANSITION.MAPPING,
                                             dimension='continuum',
                                             from.state='undiagnosed',
                                             to.state='diagnosed',
                                             subgroups = 'hiv.positive',
                                             value = 'testing')

#-- Susceptibility --#
EHE.TRANSITION.MAPPING = register.model.quantity(EHE.TRANSITION.MAPPING,
                                                 name='sexual.susceptibility',
                                                 value = expression( base.sexual.susceptibility *
                                                                        (all.prep.risk + 1-all.prep.coverage) )
)

EHE.TRANSITION.MAPPING = register.model.quantity(EHE.TRANSITION.MAPPING,
                                                 name='idu.susceptibility',
                                                 value = 0)
EHE.TRANSITION.MAPPING = register.model.quantity.subset(EHE.TRANSITION.MAPPING, 
                                                           quantity.name='idu.susceptibility',
                                                           value = expression( base.idu.susceptibility *
                                                                                   (all.prep.risk + 1-all.prep.coverage) *
                                                                                   (1 + needle.exchange*(needle.exchange.rr-1)) ),
                                                           applies.to = list(risk='active_IDU')
)

EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                name = 'base.sexual.susceptibility',
                                                scale = 'rate',
                                                model = create.log.rate.model(0,0,2010),
                                                model.from.time = 2010)

EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                name = 'base.idu.susceptibility',
                                                scale = 'rate',
                                                model = create.log.rate.model(0,0,2010),
                                                model.from.time = 2010)

#-- New Infection Proportions --#
EHE.TRANSITION.MAPPING = register.model.quantity(EHE.TRANSITION.MAPPING,
                                                 name = 'new.infection.proportions',
                                                 value = 0)


EHE.TRANSITION.MAPPING = register.model.quantity.subset(EHE.TRANSITION.MAPPING,
                                                           quantity.name = 'new.infection.proportions',
                                                           value = expression( (1-all.prep.coverage) / (all.prep.risk + 1-all.prep.coverage)),
                                                           applies.to = list(continuum='undiagnosed',
                                                                             cd4='acute')
)

EHE.TRANSITION.MAPPING = register.model.quantity.subset(EHE.TRANSITION.MAPPING,
                                                           quantity.name = 'new.infection.proportions',
                                                           value = expression(all.prep.risk / (all.prep.risk + 1-all.prep.coverage)),
                                                           applies.to = list(continuum='undiagnosed_from_prep',
                                                                             cd4='acute')
)

#-- Testing --#
EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                name='testing',
                                                scale = 'rate',
                                                
                                                get.model.function = get.testing.model,
                                                continuum.manager = ALL.DATA.MANAGERS$continuum,
                                                model.scale = 'proportion',
                                                model.from.time = 2010,
                                                
                                                ramp.scale = 'proportion',
                                                ramp.times = c(1981,1982,1993),
                                                ramp.multipliers = c(0,NA,NA),
                                                ramp.interpolate.scales = c('identity','log','identity'))


#-- Needle Exchange --#

EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING, 
                                                name = 'needle.exchange',
                                                scale = 'proportion',
                                                value = 0)

EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                name='needle.exchange.rr',
                                                scale = 'ratio',
                                                value = BASE_PARAMETER_VALUES['needle.exchange.rr'])




#-- Transmissibility --#


# Acute transmissibility
EHE.TRANSITION.MAPPING = register.model.quantity(EHE.TRANSITION.MAPPING,
                                                 name = 'acute.vs.chronic.transmissibility',
                                                 value = 1)
EHE.TRANSITION.MAPPING = register.model.quantity.subset(EHE.TRANSITION.MAPPING,
                                                        quantity.name = 'acute.vs.chronic.transmissibility',
                                                        value = 'acute.transmissibility.rr',
                                                        applies.to = list(cd4='acute'))

EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                name = 'acute.transmissibility.rr',
                                                value = BASE_PARAMETER_VALUES['acute.transmissibility.rr'],
                                                scale = 'ratio')

# IDU Transmissibility
EHE.TRANSITION.MAPPING = register.model.quantity(EHE.TRANSITION.MAPPING,
                                                 name = 'idu.transmissibility',
                                                 value = expression(suppression * diagnosed.needle.sharing.rr * acute.vs.chronic.transmissibility))

EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                name = 'diagnosed.needle.sharing.rr',
                                                scale = 'ratio',
                                                model = create.log.rate.model(intercept=log(BASE_PARAMETER_VALUES['diagnosed.needle.sharing.rr']),
                                                                                         slope=0,
                                                                                         anchor.year = 2010),
                                                model.from.time = 2010)


# Sexual Transmissibility

EHE.TRANSITION.MAPPING = register.model.quantity(EHE.TRANSITION.MAPPING,
                                                 name = 'sexual.transmissibility',
                                                 value = expression(suppression * diagnosed.sexual.transmission.rr * acute.vs.chronic.transmissibility))

EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                name = 'diagnosed.sexual.transmission.rr',
                                                scale = 'ratio',
                                                model = create.single.dimension.log.rate.model(dimension='sex',
                                                                                intercept.values=log(c(heterosexual_male = as.numeric(BASE_PARAMETER_VALUES['diagnosed.het.male.condomless.rr']),
                                                                                                       msm = as.numeric(BASE_PARAMETER_VALUES['diagnosed.msm.condomless.rr']),
                                                                                                       female = as.numeric(BASE_PARAMETER_VALUES['diagnosed.female.condomless.rr'])))),
                                                model.from.time = 2010
                                                )

#-- Suppression --#
EHE.TRANSITION.MAPPING = register.model.quantity(EHE.TRANSITION.MAPPING,
                                                 name = 'suppression',
                                                 value = 0)

EHE.TRANSITION.MAPPING = register.model.quantity.subset(EHE.TRANSITION.MAPPING,
                                                        quantity.name = 'suppression',
                                                        value = 'suppression.of.diagnosed',
                                                        applies.to = list(continuum='diagnosed'))

EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                name = 'suppression.of.diagnosed',
                                                scale = 'proportion',
                                                
                                                get.model.function = get.suppression.model,
                                                continuum.manager = ALL.DATA.MANAGERS$continuum,
                                                model.from.time = 2010,     
                                                
                                                ramp.times = 1996,
                                                ramp.multipliers = 0)


#-- IDU Transitions --#

EHE.TRANSITION.MAPPING = register.transition(EHE.TRANSITION.MAPPING,
                                             dimension = 'risk',
                                             subgroups = c('hiv.negative', 'hiv.positive'),
                                             from.state = 'never_IDU',
                                             to.state = 'active_IDU',
                                             value = 'idu.incidence')

EHE.TRANSITION.MAPPING = register.transition(EHE.TRANSITION.MAPPING,
                                             dimension = 'risk',
                                             subgroups = c('hiv.negative', 'hiv.positive'),
                                             from.state = 'active_IDU',
                                             to.state = 'IDU_in_remission',
                                             value = expression(idu.remission * 
                                                                    (1 + needle.exchange * 
                                                                         (needle.exchange.remission.rate.ratio - 1))))

EHE.TRANSITION.MAPPING = register.transition(EHE.TRANSITION.MAPPING,
                                             dimension = 'risk',
                                             subgroups = c('hiv.negative', 'hiv.positive'),
                                             from.state = 'IDU_in_remission',
                                             to.state = 'active_IDU',
                                             value = 'idu.relapse')

EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                name = 'idu.incidence',
                                                scale = 'rate',
                                                get.model.function = get.incident.idu.model,
                                                model.from.time = 1980,
                                                idu.manager = ALL.DATA.MANAGERS$idu,
                                                census = ALL.DATA.MANAGERS$census.full.msm)

EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                name = 'idu.remission',
                                                scale = 'rate',
                                                get.model.function = get.idu.remission.model,
                                                model.from.time = 1980,
                                                idu.manager = ALL.DATA.MANAGERS$idu,
                                                census = ALL.DATA.MANAGERS$census.full.msm)

EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                name = 'idu.relapse',
                                                scale = 'rate',
                                                get.model.function = get.idu.relapse.model,
                                                model.from.time = 1980,
                                                idu.manager = ALL.DATA.MANAGERS$idu,
                                                census = ALL.DATA.MANAGERS$census.full.msm)

EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                name = 'needle.exchange.remission.rate.ratio',
                                                scale = 'ratio',
                                                value = BASE_PARAMETER_VALUES['needle.exchange.remission.rate.ratio'])

#-- HIV Mortality --#


EHE.TRANSITION.MAPPING = register.model.quantity(EHE.TRANSITION.MAPPING,
                                                 name = 'hiv.mortality',
                                                 value = expression(suppression * unsuppressed.hiv.mortality.rate))

EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                name = 'unsuppressed.hiv.mortality.rate',
                                                scale = 'rate',
                                                model = create.logistic.spline.model(knot.values = c(1,1,1), #placeholder
                                                                                     knot.times = c(2000,2010,2016),
                                                                                     fraction.of.asymptote.after.end=0.05,
                                                                                     fraction.of.asymptote.before.start=0.025,
                                                                                     fraction.of.asymptote.for.change.dir=0.02,
                                                                                     alphas.are.multipliers = T),
                                                model.from.time = 2000,
                                                ramp.times = c(1970,1980,1996),
                                                ramp.multipliers = c(1,1,1)) #placeholder

#-- CD4 Transitions --#

EHE.TRANSITION.MAPPING = register.transition(EHE.TRANSITION.MAPPING,
                                             dimension = 'cd4',
                                             subgroups = 'hiv.positive',
                                             from.state = 'acute',
                                             to.state = 'chronic',
                                             value = expression(1/acute.hiv.duration))

EHE.TRANSITION.MAPPING = register.model.element(EHE.TRANSITION.MAPPING,
                                                     name = 'acute.hiv.duration',
                                                     scale = 'time',
                                                     value = BASE_PARAMETER_VALUES['acute.infection.duration'])

# Remaining hard-coded for now
# - Contact Arrays
# - Birth Proportions
# - General Mortality
#-- Aging? --#

#-- CREATE AND REGISTER THE SETTINGS --#

VERSION.MANAGER = create.jheem.specification(
    version='collapsed_1.0',
    prior.versions=character(),
    directory.suffix='_collapsed',
    file.version='1.0',
    
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
    
    state.name.mapping = create.state.name.mapping(
        first.diagnosed.states='diagnosed',
        diagnosed.states='diagnosed',
        undiagnosed.from.prep.states='undiagnosed_from_prep',
        undiagnosed.no.prep.states='undiagnosed',
        engaged.states=NA,
        suppressed.states=NA,
        
        acute.cd4.states = 'acute',
        chronic.cd4.states = 'chronic',
        
        active.idu.states = 'active_IDU'
    ),
    
    transition.mapping = EHE.TRANSITION.MAPPING,
    
    is.continuum.collapsed=T
)
