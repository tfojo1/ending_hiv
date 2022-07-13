

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
                                          parameter.value = parameters['rate.of.switching.back.to.oral'])
        components = set.static.parameter(components,
                                          parameter.name='laart.durably.suppressed.to.engaged.recently.suppressed',
                                          parameter.value = parameters['rate.of.switching.back.to.oral'])
        
        
        p.resistance = parameters['laart.recently.suppressed.to.resistant.disengaged']
        o.resistance = p.resistance / (1-p.resistance)
        o.resistance = o.resistance * parameters['unsuppressed.vs.recently.suppressed.resistance.disengaged.or']
        p.resistance = o.resistance / (1+o.resistance)
        components = set.static.parameter(components,
                                          parameter.name='laart.unsuppressed.to.resistant.disengaged',
                                          parameter.value = p.resistance)
        
        # Return
        components
    }
)