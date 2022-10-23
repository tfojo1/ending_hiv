GET.COMPONENTS.FOR.LAART.PARAMETERS <- join.get.components.functions(
  get.components.function.for.version(version='expanded_1.0'),
  function(parameters, components,
           data.managers = ALL.DATA.MANAGERS)
  {
    #@Preetham fill in here
    #parameters is a named numeric vector which has the same names as in your prior distributions
    #components is the object you need to update
    
    components = set.static.parameter(components,
                                      parameter.name='laart.versus.oral.disengagement.rr', # the name you used in the settings
                                      parameter.value = parameters['laart.versus.oral.disengagement.rr']) # the name you used in the prior
    
    components = set.static.parameter(components,
                                      parameter.name='laart.recently.suppressed.to.engaged.recently.suppressed',
                                      parameter.value = parameters['laart.discontinuation'])
    
    components = set.static.parameter(components,
                                      parameter.name='laart.durably.suppressed.to.engaged.durably.suppressed',
                                      parameter.value = parameters['laart.discontinuation'])
    
    components = set.static.parameter(components,
                                      parameter.name='laart.unsuppressed.to.engaged.unsuppressed.failing',
                                      parameter.value = parameters['laart.discontinuation'])
    
    # components = set.static.parameter(components,
    #                                   parameter.name='laart.recently.suppressed.to.resistant.disengaged',
    #                                   parameter.value = parameters['laart.recently.suppressed.to.resistant.disengaged'])
    
    # components = set.static.parameter(components,
    #                                   parameter.name='laart.recently.suppressed.to.resistant.unsuppressed',
    #                                   parameter.value = parameters['laart.recently.suppressed.to.resistant.unsuppressed'])
    
    # components = set.static.parameter(components,
    #                                   parameter.name='laart.durably.suppressed.to.resistant.disengaged',
    #                                   parameter.value = parameters['laart.durably.suppressed.to.resistant.disengaged'])
     
    # components = set.static.parameter(components,
    #                                   parameter.name='laart.durably.suppressed.to.resistant.unsuppressed',
    #                                   parameter.value = parameters['laart.durably.suppressed.to.resistant.unsuppressed'])
    
    
    p.resistance = parameters['laart.recently.suppressed.to.resistant.disengaged']
    o.resistance = p.resistance / (1-p.resistance)
    o.resistance = o.resistance * parameters['unsuppressed.vs.recently.suppressed.resistance.disengaged.or']
    p.resistance = o.resistance / (1+o.resistance)
    # components = set.static.parameter(components,
    #                                   parameter.name='laart.unsuppressed.to.resistant.disengaged',
    #                                   parameter.value = p.resistance)
    
    components = set.static.parameter(components,
                                      parameter.name='resistant.versus.oral.loss.of.suppression.rr',
                                      parameter.value = parameters['resistant.versus.oral.loss.of.suppression.rr'])
    
    components = set.static.parameter(components,
                                      parameter.name='resistant.versus.oral.disengagement.rr',
                                      parameter.value = parameters['resistant.versus.oral.disengagement.rr'])
    
    components = set.static.parameter(components,
                                      parameter.name='laart.unsuppressed.to.laart.recently.suppressed',
                                      parameter.value = parameters['laart.unsuppressed.to.laart.recently.suppressed'])
    
    components = set.static.parameter(components,
                                      parameter.name='resistant.versus.oral.gain.of.suppression.rr',
                                      parameter.value = parameters['resistant.versus.oral.gain.of.suppression.rr'])
    
    components = set.static.parameter(components,
                                      parameter.name='resistant.versus.oral.reengagement.rr',
                                      parameter.value = parameters['resistant.versus.oral.reengagement.rr'])
    
    # p.resistance = parameters['laart.recently.suppressed.to.resistant.unsuppressed']
    # o.resistance = p.resistance / (1-p.resistance)
    # o.resistance = o.resistance = parameters['unsuppressed.vs.recently.suppressed.resistance.disengaged.or']
    # p.resistance = o.resistance / (1+o.resistance)
    # components = set.static.parameter(components,
    #                                   parameter.name='engaged.unsuppressed.switch.to.laart.successful',
    #                                   parameter.value = 1-p.resistance)
    
    # components = set.static.parameter(components,
    #                                   parameter.name='engaged.durably.suppressed.switch.to.laart.successful',
    #                                   parameter.value = 1-parameters['laart.durably.suppressed.to.resistant.unsuppressed'])
     
    components = set.static.parameter(components,
                                      parameter.name='engaged.suppressed.to.resistant.unsuppressed',
                                      parameter.value = parameters['engaged.suppressed.to.resistant.unsuppressed'])
    
    components = set.static.parameter(components,
                                      parameter.name='engaged.unsuppressed.vs.suppressed.resistant.unsuppressed.or',
                                      parameter.value = parameters['engaged.unsuppressed.vs.suppressed.resistant.unsuppressed.or'])
    
    components = set.static.parameter(components,
                                      parameter.name='laart.suppressed.to.resistant.unsuppressed',
                                      parameter.value = parameters['laart.suppressed.to.resistant.unsuppressed'])
    
    components = set.static.parameter(components,
                                      parameter.name='laart.suppressed.to.resistant.disengaged',
                                      parameter.value = parameters['laart.suppressed.to.resistant.disengaged'])
    
    
    
    # Return
    components
  }
)

VERSION.MANAGER = register.get.components.function(VERSION.MANAGER,
                                                   version='laart',
                                                   fn = GET.COMPONENTS.FOR.LAART.PARAMETERS,
                                                   join.with.previous.version.function = T)

VERSION.MANAGER = register.projection.update.components.function(VERSION.MANAGER,
                                                                 version='laart',
                                                                 fn = get.components.function.for.version('expanded_1.0'),
                                                                 join.with.previous.version.function = F)
