

GET.COMPONENTS.FOR.LAART.PARAMETERS <- join.get.components.functions(
    get.components.function.for.version(version='expanded_1.0'),
    function(parameters, components,
             data.managers = ALL.DATA.MANAGERS)
    {
        #@Preetham fill in here
        #parameters is a named numeric vector which has the same names as in your prior distributions
        #components is the object you need to update
        
        components = set.static.parameter(components,
                                          parameter.name='laart.vs.oral.disengagement.rr', # the name you used in the settings
                                          parameter.value = parameters['laart.vs.oral.disengagement.rr']) # the name you used in the prior
        
        components = set.static.parameter(components,
                                          parameter.name='laart.recently.suppressed.to.engaged.recently.suppressed',
                                          parameter.value = parameters['laart.recently.suppressed.to.engaged.recently.suppressed'])
        
        components = set.static.parameter(components,
                                          parameter.name='laart.durably.suppressed.to.engaged.recently.suppressed',
                                          parameter.value = parameters['laart.recently.suppressed.to.engaged.recently.suppressed'])
        
        components = set.static.parameter(components,
                                          parameter.name='laart.unsuppressed.to.engaged.unsuppressed.failing',
                                          parameter.value = parameters['laart.recently.suppressed.to.engaged.recently.suppressed'])
        
        components = set.static.parameter(components,
                                          parameter.name='laart.recently.suppressed.to.resistant.disengaged',
                                          parameter.value = parameters['laart.recently.suppressed.to.resistant.disengaged'])
        
        components = set.static.parameter(components,
                                          parameter.name='laart.recently.suppressed.to.resistant.unsuppressed',
                                          parameter.value = parameters['laart.recently.suppressed.to.resistant.unsuppressed'])
        
        components = set.static.parameter(components,
                                          parameter.name='laart.durably.suppressed.to.resistant.disengaged',
                                          parameter.value = parameters['laart.durably.suppressed.to.resistant.disengaged'])
        
        components = set.static.parameter(components,
                                          parameter.name='laart.durably.suppressed.to.resistant.unsuppressed',
                                          parameter.value = parameters['laart.durably.suppressed.to.resistant.unsuppressed'])
        
      
        p.resistance = parameters['laart.recently.suppressed.to.resistant.disengaged']
        o.resistance = p.resistance / (1-p.resistance)
        o.resistance = o.resistance * parameters['unsuppressed.vs.recently.suppressed.resistance.disengaged.or']
        p.resistance = o.resistance / (1+o.resistance)
        components = set.static.parameter(components,
                                          parameter.name='laart.unsuppressed.to.resistant.disengaged',
                                          parameter.value = p.resistance)
        
        components = set.static.parameter(components,
                                          parameter.name='resistant.versus.oral.loss.of.suppression.rr',
                                          parameter.value = 'resistant.versus.oral.loss.of.suppression.rr')
        
        components = set.static.parameter(components,
                                          parameter.name='resistant.versus.oral.disengagement.rr',
                                          parameter.value = 'resistant.versus.oral.disengagement.rr')
        
        components = set.static.parameter(components,
                                          parameter.name='laart.unsuppressed.to.laart.recently.suppressed',
                                          parameter.value = 'laart.unsuppressed.to.laart.recently.suppressed')
        
        components = set.static.parameter(components,
                                          parameter.name='resistant.versus.gain.of.suppression.rr',
                                          parameter.value = 'resistant.versus.gain.of.suppression.rr')
        
        components = set.static.parameter(components,
                                          parameter.name='resistant.versus.reengagement.rr',
                                          parameter.value = 'resistant.versus.reengagement.rr')
        
        # Return
        components
    }
)
