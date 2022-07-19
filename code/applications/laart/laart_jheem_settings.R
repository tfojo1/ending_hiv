

LAART.CONTINUUM = c('undiagnosed', 
                    'undiagnosed_from_prep', 
                    'unengaged', 
                    'engaged_unsuppressed_naive',
                    'engaged_unsuppressed_failing', 
                    'engaged_recently_suppressed',
                    'engaged_durably_suppressed',
                    'disengaged_naive',
                    'disengaged_failing', 
                    'laart_unsuppressed', 
                    'laart_recently_suppressed', 
                    'laart_durably_suppressed', 
                    'resistant_unsuppressed', 
                    'resistant_recently_suppressed', 
                    'resistant_durably_suppressed', 
                    'resistant_disengaged')

LAART.CONTINUUM.TRANSITION.MAPPING = copy.transition.mapping(template=get.settings.for.version('expanded_1.0')$transition.mapping,
                                                             version='laart')
#purpose?
# LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
#                                                          dimension='continuum',
#                                                          from.state='unengaged',
#                                                          to.state='engaged_unsuppressed_naive',
#                                                          rate = expression(linkage / time.to.link.vs.disengage),
#                                                          label = 'engagement')
#purpose?
# LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
#                                                          dimension='continuum',
#                                                          from.state='unengaged',
#                                                          to.state='disengaged_naive',
#                                                          rate = expression((1-linkage) / time.to.link.vs.disengage),
#                                                          label='failure.to.link')

# LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
#                                                          dimension='continuum',
#                                                          from.state='engaged_unsuppressed_naive',
#                                                          to.state='disengaged_naive',
#                                                          rate = 'naive.to.disengaged',
#                                                          label = 'loss.to.care')

# LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
#                                                          dimension='continuum',
#                                                          from.state='disengaged_naive',
#                                                          to.state='engaged_unsuppressed_naive',
#                                                          rate = 'reengagement',
#                                                          label = 'reengagement')

# LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
#                                                          dimension='continuum',
#                                                          from.state='engaged_unsuppressed_naive',
#                                                          to.state='engaged_unsuppressed_failing',
#                                                          rate = expression((1-naive.to.suppressed) / (time.to.suppression.on.art + start.art)),
#                                                          #rate = expression((1-naive.to.suppressed) / (time.to.suppression.on.art - 1/log(1-start.art))),
#                                                          label = 'failure.to.suppress')

# LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
#                                                          dimension='continuum',
#                                                          from.state='engaged_unsuppressed_naive',
#                                                          to.state='engaged_recently_suppressed',
#                                                          rate = expression(naive.to.suppressed / (time.to.suppression.on.art + start.art)),
#                                                          #rate = expression(naive.to.suppressed / (time.to.suppression.on.art -1/log(1-start.art))),
#                                                          label = 'gain.of.suppression')

# LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
#                                                          dimension='continuum',
#                                                          from.state='engaged_recently_suppressed',
#                                                          to.state='engaged_durably_suppressed',
#                                                          rate = expression(1/time.to.durable.suppression),
#                                                          label = 'continuation.of.suppression')

# LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
#                                                          dimension='continuum',
#                                                          from.state='engaged_recently_suppressed',
#                                                          to.state='engaged_unsuppressed_failing',
#                                                          rate = 'recently.suppressed.to.failing',
#                                                          label = 'loss.of.suppression')

# LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
#                                                          dimension='continuum',
#                                                          from.state='engaged_recently_suppressed',
#                                                          to.state='disengaged_failing',
#                                                          rate = 'recently.suppressed.to.disengaged',
#                                                          label = 'loss.to.care')

# LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
#                                                          dimension='continuum',
#                                                          from.state='engaged_durably_suppressed',
#                                                          to.state='engaged_unsuppressed_failing',
#                                                          rate = 'durably.suppressed.to.failing',
#                                                          label = 'loss.of.suppression')

# LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
#                                                          dimension='continuum',
#                                                          from.state='engaged_durably_suppressed',
#                                                          to.state='disengaged_failing',
#                                                          rate = 'durably.suppressed.to.disengaged',
#                                                          label = 'loss.to.care')

# LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
#                                                          dimension='continuum',
#                                                          from.state='engaged_unsuppressed_failing',
#                                                          to.state='disengaged_failing',
#                                                          rate = 'failing.to.disengaged',
#                                                          label = 'loss.to.care')

# LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
#                                                          dimension='continuum',
#                                                          from.state='engaged_unsuppressed_failing',
#                                                          to.state='engaged_recently_suppressed',
#                                                          rate = 'failing.to.suppressed',
#                                                          label = 'gain.of.suppression')

# LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
#                                                          dimension='continuum',
#                                                          from.state='disengaged_failing',
#                                                          to.state='engaged_unsuppressed_failing',
#                                                          rate = 'reengagement',
#                                                          label = 'reengagement')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='laart_recently_suppressed',
                                                         to.state='disengaged_failing',
                                                         rate = expression((1-laart.recently.suppressed.to.resistant.disengaged)*recently.suppressed.to.disengaged*laart.versus.oral.disengagement.rr),
                                                         label = 'loss.to.care')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='laart_recently_suppressed',
                                                         to.state='resistant_disengaged',
                                                         rate = expression((laart.recently.suppressed.to.resistant.disengaged)*recently.suppressed.to.disengaged*laart.versus.oral.disengagement.rr),
                                                         label = 'loss.to.care')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='laart_recently_suppressed',
                                                         to.state='engaged_recently_suppressed',
                                                         rate = 'laart.recently.suppressed.to.engaged.recently.suppressed',
                                                         label = 'switch.to.art')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='laart_recently_suppressed',
                                                         to.state='resistant_unsuppressed',
                                                         rate = 'laart.recently.suppressed.to.resistant.unsuppressed',
                                                         label = 'gain.of.resistance')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='laart_recently_suppressed',
                                                         to.state='laart_durably_suppressed',
                                                         rate = expression(1/time.to.laart.durable.suppression),
                                                         label = 'continuation.of.suppression')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='laart_durably_suppressed',
                                                         to.state='disengaged_failing',
                                                         rate = expression((1-laart.durably.suppressed.to.resistant.disengaged)*durably.suppressed.to.disengaged*laart.versus.oral.disengagement.rr),
                                                         label = 'loss.to.care')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='laart_durably_suppressed',
                                                         to.state='resistant_disengaged',
                                                         rate = expression((laart.durably.suppressed.to.resistant.disengaged)*durably.suppressed.to.disengaged*laart.versus.oral.disengagement.rr),
                                                         label = 'loss.to.care')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='laart_durably_suppressed',
                                                         to.state='resistant_unsuppressed',
                                                         rate = 'laart.durably.suppressed.to.resistant.unsuppressed',
                                                         label = 'gain.of.resistance')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='laart_durably_suppressed',
                                                         to.state='engaged_durably_suppressed',
                                                         rate = 'laart.durably.suppressed.to.engaged.durably.suppressed',
                                                         label = 'switch.to.art')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='laart_unsuppressed', 
                                                         to.state='disengaged_failing',
                                                         rate = expression((1-laart.unsuppressed.to.resistant.disengaged)*failing.to.disengaged*laart.versus.oral.disengagement.rr),
                                                         label = 'loss.to.care')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='laart_unsuppressed', 
                                                         to.state='resistant_disengaged',
                                                         rate = expression(laart.unsuppressed.to.resistant.disengaged*failing.to.disengaged*laart.versus.oral.disengagement.rr),
                                                         label = 'loss.to.care')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='laart_unsuppressed', #1/time*proportion
                                                         to.state='resistant_unsuppressed',
                                                         rate = expression((1/time.to.laart.gain.suppression)*(1-laart.unsuppressed.to.laart.recently.suppressed)),
                                                         label = 'gain.of.resistance')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='laart_unsuppressed', #1/time*proportion
                                                         to.state='laart_recently_suppressed',
                                                         rate = expression((1/time.to.laart.gain.suppression)*(laart.unsuppressed.to.laart.recently.suppressed)), #fix
                                                         label = 'gain.of.suppression')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='laart_unsuppressed',
                                                         to.state='engaged_unsuppressed_failing', #1/time (background = proportion, return = rate)
                                                         rate = 'laart.unsuppressed.to.engaged.unsuppressed.failing',
                                                         label = 'switch.to.art')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='resistant_recently_suppressed',
                                                         to.state='resistant_unsuppressed', 
                                                         #rate = 'resistant.recently.suppressed.to.resistant.unsuppressed', #multiple by ratio of oral vs resistant
                                                         rate = expression(recently.suppressed.to.failing*resistant.versus.oral.loss.of.suppression.rr), 
                                                         label = 'loss.of.suppression')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='resistant_recently_suppressed',
                                                         to.state='resistant_disengaged',
                                                         rate = expression(recently.suppressed.to.disengaged*resistant.versus.oral.disengagement.rr),
                                                         label = 'loss.to.care')


LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='resistant_recently_suppressed',
                                                         to.state='resistant_durably_suppressed', #1/time; time = time.to.suppression look at expanded jheem
                                                         rate = expression(1/time.to.durable.suppression),
                                                         label = 'continuation.of.suppression')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='resistant_durably_suppressed',
                                                         to.state='resistant_disengaged',
                                                         rate = expression(durably.suppressed.to.disengaged*resistant.versus.oral.disengagement.rr),
                                                         label = 'loss.to.care')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='resistant_durably_suppressed',
                                                         to.state='resistant_unsuppressed',
                                                         rate = expression(durably.suppressed.to.failing*resistant.versus.oral.loss.of.suppression.rr),
                                                         label = 'loss.of.suppression')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='resistant_unsuppressed',
                                                         to.state='resistant_disengaged',
                                                         rate = expression(failing.to.disengaged*resistant.versus.oral.disengagement.rr),
                                                         label = 'loss.to.care')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='resistant_unsuppressed',
                                                         to.state='resistant_recently_suppressed',
                                                         rate = expression(failing.to.suppressed*resistant.versus.oral.gain.of.suppression.rr),
                                                         label = 'gain.of.suppression')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='resistant_disengaged',
                                                         to.state='resistant_unsuppressed',
                                                         rate = expression(reengagement*resistant.versus.reengagement.rr),
                                                         label = 'reengagement')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='engaged_durably_suppressed',
                                                         to.state='laart_durably_suppressed',
                                                         rate = expression((engaged.durably.suppressed.switch.to.laart.successful)*engaged.durably.suppressed.switch.to.laart),
                                                         label = 'switch.to.laart')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='engaged_durably_suppressed',
                                                         to.state='resistant_unsuppressed',
                                                         rate = expression((1-engaged.durably.suppressed.switch.to.laart.successful)*engaged.durably.suppressed.switch.to.laart),
                                                         label = 'switch.to.laart')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='engaged_unsuppressed_failing',
                                                         to.state='laart_unsuppressed', 
                                                         rate = expression((engaged.unsuppressed.switch.to.laart.successful)*engaged.unsuppressed.switch.to.laart),
                                                         label = 'switch.to.laart')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                         dimension='continuum',
                                                         from.state='engaged_unsuppressed_failing',
                                                         to.state='resistant_unsuppressed', 
                                                         rate = expression((1-engaged.unsuppressed.switch.to.laart.successful)*engaged.unsuppressed.switch.to.laart),
                                                         label = 'switch.to.laart')






#ignore ramp; should be optional
LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='laart.recently.suppressed.to.resistant.disengaged',
                                                                 type = 'proportion', 
                                                                 model.source = 'continuum.manager')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='recently.suppressed.to.disengaged',
                                                                 background.model.type = 'proportion',
                                                                 ramp.type = 'proportion', 
                                                                 return.type = 'rate',
                                                                 model.source = 'continuum.manager')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='laart.versus.oral.disengagement.rr',
                                                                 type = 'rate', 
                                                                 model.source = 'continuum.manager')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='laart.recently.suppressed.to.engaged.recently.suppressed',
                                                                 background.model.type = 'proportion',
                                                                 ramp.type = 'proportion',
                                                                 return.type = 'rate',
                                                                 model.source = 'continuum.manager')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='laart.recently.suppressed.to.resistant.unsuppressed',
                                                                 background.model.type = 'proportion',
                                                                 ramp.type = 'proportion',
                                                                 return.type = 'rate',
                                                                 model.source = 'continuum.manager')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='time.to.laart.durable.suppression',
                                                                 type='time',
                                                                 default.value = 0.25,
                                                                 required=F)

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='laart.durably.suppressed.to.resistant.disengaged',
                                                                 type = 'proportion',
                                                                 model.source = 'continuum.manager')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='laart.durably.suppressed.to.resistant.unsuppressed',
                                                                 background.model.type = 'proportion',
                                                                 ramp.type = 'proportion',
                                                                 return.type = 'rate',
                                                                 model.source = 'continuum.manager')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='laart.durably.suppressed.to.engaged.durably.suppressed',
                                                                 background.model.type = 'proportion',
                                                                 ramp.type = 'proportion',
                                                                 return.type = 'rate',
                                                                 model.source = 'continuum.manager')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='laart.unsuppressed.to.resistant.disengaged',
                                                                 type = 'proportion', 
                                                                 model.source = 'continuum.manager')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='time.to.laart.gain.suppression',
                                                                 type='time',
                                                                 default.value = 0.25,
                                                                 required=F)

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='laart.unsuppressed.to.laart.recently.suppressed',
                                                                 type = 'proportion', 
                                                                 model.source = 'continuum.manager')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='laart.unsuppressed.to.engaged.unsuppressed.failing',
                                                                 background.model.type = 'proportion',
                                                                 ramp.type = 'proportion',
                                                                 return.type = 'rate',
                                                                 model.source = 'continuum.manager')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='resistant.versus.oral.loss.of.suppression.rr',
                                                                 type = 'rate', 
                                                                 model.source = 'continuum.manager')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='resistant.versus.oral.disengagement.rr',
                                                                 type = 'rate', 
                                                                 model.source = 'continuum.manager')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='resistant.versus.oral.gain.of.suppression.rr',
                                                                 type = 'rate', 
                                                                 model.source = 'continuum.manager')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='resistant.versus.reengagement.rr',
                                                                 type = 'rate', 
                                                                 model.source = 'continuum.manager')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='engaged.durably.suppressed.switch.to.laart.successful',
                                                                 type = 'proportion',
                                                                 model.source = 'continuum.manager')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='engaged.durably.suppressed.switch.to.laart',
                                                                 background.model.type = 'proportion',
                                                                 ramp.type = 'proportion',
                                                                 return.type = 'rate',
                                                                 model.source = 'continuum.manager')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='engaged.unsuppressed.switch.to.laart.successful',
                                                                 type = 'proportion',
                                                                 model.source = 'continuum.manager')

LAART.CONTINUUM.TRANSITION.MAPPING = register.transition.element(LAART.CONTINUUM.TRANSITION.MAPPING,
                                                                 name='engaged.unsuppressed.switch.to.laart',
                                                                 background.model.type = 'proportion',
                                                                 ramp.type = 'proportion',
                                                                 return.type = 'rate',
                                                                 model.source = 'continuum.manager')


VERSION.MANAGER = copy.and.modify.jheem.settings(
  template.settings = get.settings.for.version('expanded_1.0'),
  
  version='laart',
  directory.suffix='laart',
  
  continuum.of.care = LAART.CONTINUUM,
  
  first.diagnosed.continuum.states='unengaged',
  diagnosed.continuum.states=setdiff(LAART.CONTINUUM, 
                                     c('undiagnosed', 'undiagnosed_from_prep')),
  undiagnosed.from.prep.continuum.states='undiagnosed_from_prep',
  undiagnosed.no.prep.continuum.states='undiagnosed',
  engaged.continuum.states=c('engaged_unsuppressed_naive',
                             'engaged_unsuppressed_failing', 
                             'engaged_recently_suppressed',
                             'engaged_durably_suppressed'
  ),
  suppressed.continuum.states=c('engaged_recently_suppressed',
                                'engaged_durably_suppressed'
  ),
  
  is.continuum.collapsed=F,
  
  transition.mapping = LAART.CONTINUUM.TRANSITION.MAPPING
)

