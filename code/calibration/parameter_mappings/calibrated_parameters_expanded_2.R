#113 - like 108 but with peak multiplier 
library(distributions)

MSM.BASE.TRATE.MEAN = 1
HET.BASE.TRATE.MEAN = 1
IDU.BASE.TRATE.MEAN = 1
BASE.TRATE.CV = 20

TRATE.RR.1.2.SPAN = 2#1.5
TRATE.RR.0.1.SPAN = 4#2#1.5
TRATE.RR.0.PEAK.SPAN = 8#3



EXPANDED.CONTINUUM.PARAMETERS.PRIOR = join.distributions(
    
    
    #-- LINKAGE --#
    heterosexual.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    msm.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    idu.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    msm.idu.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    
    black.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    hispanic.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    
    age1.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    age2.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    age4.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    age5.proportion.linked.or = Lognormal.Distribution(0, log(2)),
    
    heterosexual.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    msm.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    idu.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    msm.idu.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    black.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    hispanic.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    age1.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    age2.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    age4.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    age5.proportion.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    
    #-- STARTING ART --#
    heterosexual.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    msm.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    idu.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    msm.idu.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    age1.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    age2.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    age4.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    age5.start.art.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    full.art.year = Uniform.Distribution(2015, 2018),
    
    #-- ADHERENCE --#
    heterosexual.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    msm.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    idu.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    msm.idu.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    age1.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    age2.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    age4.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    age5.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    heterosexual.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    msm.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    idu.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    msm.idu.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    black.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    hispanic.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    age1.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    age2.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    age4.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    age5.proportion.adherent.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    # (plus a term for which direction suppression/loss of suppression)
    recently.suppressed.vs.failing.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    durably.suppressed.vs.failing.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    naive.vs.failing.proportion.adherent.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    #-- DISENGAGEMENT/REENGAGEMENT --#
    heterosexual.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    msm.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    idu.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    msm.idu.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    age1.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    age2.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    age4.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    age5.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    heterosexual.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    msm.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    idu.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    msm.idu.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    black.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    hispanic.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    age1.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    age2.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    age4.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    age5.proportion.lost.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    # (plus a term for from suppressed or from unsuppressed) 
    recently.suppressed.vs.failing.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(4)),
    durably.suppressed.vs.failing.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(4)),
    naive.vs.failing.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(4)),
    already.lost.vs.failing.proportion.lost.or = Lognormal.Distribution(0, 0.5*log(4))
)
VERSION.MANAGER = register.parameters.prior(VERSION.MANAGER,
                                            prior=join.distributions(BASE.PARAMETERS.PRIOR, EXPANDED.CONTINUUM.PARAMETERS.PRIOR),
                                            version='expanded_1.0')

EXPANDED.CONTINUUM.PARAMETER.VAR.BLOCKS = list(
    
    msm.linkage = c('msm.proportion.linked.or',
                        'msm.proportion.linked.slope.or',
                        'msm.idu.proportion.linked.or',
                        'msm.idu.proportion.linked.slope.or'),
    
    idu.heterosexual.linkage = c('idu.proportion.linked.or',
                                     'idu.proportion.linked.slope.or',
                                     'heterosexual.proportion.linked.or',
                                     'heterosexual.proportion.linked.slope.or'),
    
    linked.by.race = c('black.proportion.linked.or',
                       'black.proportion.linked.slope.or',
                       'hispanic.proportion.linked.or',
                       'hispanic.proportion.linked.slope.or'),
    
    young.linked = c('age1.proportion.linked.or',
                     'age1.proportion.linked.slope.or',
                     'age2.proportion.linked.or',
                     'age2.proportion.linked.slope.or'),
    
    old.linked = c('age4.proportion.linked.or',
                   'age4.proportion.linked.slope.or',
                   'age5.proportion.linked.or',
                   'age5.proportion.linked.slope.or'),
    
    start.art.by.risk = c('heterosexual.start.art.or',
                          'msm.start.art.or',
                          'idu.start.art.or',
                          'msm.idu.start.art.or'), 
    
    start.art.by.race = c('black.start.art.or',
                          'hispanic.start.art.or',
                          'full.art.year'),  
    
    start.art.by.age = c('age1.start.art.or',
                         'age2.start.art.or',
                         'age4.start.art.or',
                         'age5.start.art.or'), 
    
    msm.adherence = c('msm.proportion.adherent.or',
                      'msm.proportion.adherent.slope.or',
                      'msm.idu.proportion.adherent.or',
                      'msm.idu.proportion.adherent.slope.or'),
    
    idu.heterosexual.adherence = c('idu.proportion.adherent.or',
                                   'idu.proportion.adherent.slope.or',
                                   'heterosexual.proportion.adherent.or',
                                   'heterosexual.proportion.adherent.slope.or'),
    
    
    adherence.by.race = c('black.proportion.adherent.or',
                          'black.proportion.adherent.slope.or',
                          'hispanic.proportion.adherent.or',
                          'hispanic.proportion.adherent.slope.or'),
    
    young.adherence = c('age1.proportion.adherent.or',
                        'age1.proportion.adherent.slope.or',
                        'age2.proportion.adherent.or',
                        'age2.proportion.adherent.slope.or'),
    
    old.adherence = c('age4.proportion.adherent.or',
                      'age4.proportion.adherent.slope.or',
                      'age5.proportion.adherent.or',
                      'age5.proportion.adherent.slope.or'),
    
    
    msm.loss = c('msm.proportion.lost.or',
                 'msm.proportion.lost.slope.or',
                 'msm.idu.proportion.lost.or',
                 'msm.idu.proportion.lost.slope.or'),
    
    idu.heterosexual.loss = c('idu.proportion.lost.or',
                              'idu.proportion.lost.slope.or',
                              'heterosexual.proportion.lost.or',
                              'heterosexual.proportion.lost.slope.or'),
    
    loss.by.race = c('black.proportion.lost.or',
                     'black.proportion.lost.slope.or',
                     'hispanic.proportion.lost.or',
                     'hispanic.proportion.lost.slope.or'),
    
    young.loss = c('age1.proportion.lost.or',
                   'age1.proportion.lost.slope.or',
                   'age2.proportion.lost.or',
                   'age2.proportion.lost.slope.or'),
    
    old.loss = c('age4.proportion.lost.or',
                 'age4.proportion.lost.slope.or',
                 'age5.proportion.lost.or',
                 'age5.proportion.lost.slope.or'),
    
    suppressed.vs.nonsuppressed.adherence = c('recently.suppressed.vs.failing.proportion.adherent.or',
                                              'durably.suppressed.vs.failing.proportion.adherent.or',
                                              'naive.vs.failing.proportion.adherent.or'),
    
    suppressed.vs.nonsuppressed.retention = c('recently.suppressed.vs.failing.proportion.lost.or',
                                              'durably.suppressed.vs.failing.proportion.lost.or',
                                              'naive.vs.failing.proportion.lost.or',
                                              'already.lost.vs.failing.proportion.lost.or')
)
VERSION.MANAGER = register.parameter.sampling.blocks(VERSION.MANAGER,
                                                     blocks=c(BASE.PARAMETER.VAR.BLOCKS, EXPANDED.CONTINUUM.PARAMETER.VAR.BLOCKS),
                                                     version='expanded_1.0')

if (1==2)
{
    length(EXPANDED.CONTINUUM.PARAMETER.VAR.BLOCKS.1)
    setdiff(unique(unlist(EXPANDED.CONTINUUM.PARAMETER.VAR.BLOCKS.1)), EXPANDED.CONTINUUM.PARAMETERS.PRIOR@var.names)    
    setdiff(EXPANDED.CONTINUUM.PARAMETERS.PRIOR@var.names, unique(unlist(EXPANDED.CONTINUUM.PARAMETER.VAR.BLOCKS.1)))    
}



##---------------------------------##
##-- The Get Components Function --##
##---------------------------------##
##
EXPANDED.CONTINUUM.GET.COMPONENTS.FUNCTION = join.get.components.functions(
    BASE.GET.COMPONENTS.FUNCTION,
function(parameters, components,
         data.managers = ALL.DATA.MANAGERS)
{
    exclude.from.check = c('recently.suppressed.vs.failing.proportion.adherent.or', 
                           'durably.suppressed.vs.failing.proportion.adherent.or', 
                           'naive.vs.failing.proportion.adherent.or',
                           'recently.suppressed.vs.failing.proportion.lost.or', 
                           'durably.suppressed.vs.failing.proportion.lost.or', 
                           'naive.vs.failing.proportion.lost.or',
                           'already.lost.vs.failing.proportion.lost.or')
    
    # Linkage
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='linkage',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.linked.or',
                                                                slope.parameter.suffix = 'proportion.linked.slope.or',
                                                                idu.applies.to.in.remission = T)
    # Failing to Recently Suppressed
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='failing.to.suppressed',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.adherent.or',
                                                                slope.parameter.suffix = 'proportion.adherent.slope.or',
                                                                idu.applies.to.in.remission = T,
                                                                exclude.from.check = exclude.from.check)

    # Recently Suppressed to Failing
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='recently.suppressed.to.failing',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.adherent.or',
                                                                slope.parameter.suffix = 'proportion.adherent.slope.or',
                                                                idu.applies.to.in.remission = T,
                                                                transformation = 'reciprocal',
                                                                sex.risk.intercept.multiplier = parameters['recently.suppressed.vs.failing.proportion.adherent.or'],
                                                                exclude.from.check = exclude.from.check)
    
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='durably.suppressed.to.failing',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.adherent.or',
                                                                slope.parameter.suffix = 'proportion.adherent.slope.or',
                                                                idu.applies.to.in.remission = T,
                                                                transformation = 'reciprocal',
                                                                sex.risk.intercept.multiplier = parameters['durably.suppressed.vs.failing.proportion.adherent.or'],
                                                                exclude.from.check = exclude.from.check)
    

    # Naive - Recently suppressed
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='naive.to.suppressed',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.adherent.or',
                                                                slope.parameter.suffix = 'proportion.adherent.slope.or',
                                                                idu.applies.to.in.remission = T,
                                                                transformation = 'identity',
                                                                sex.risk.intercept.multiplier = parameters['naive.vs.failing.proportion.adherent.or'],
                                                                exclude.from.check = exclude.from.check)
    
    # Recently Suppressed to Disengaged
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='recently.suppressed.to.disengaged',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.lost.or',
                                                                slope.parameter.suffix = 'proportion.lost.slope.or',
                                                                idu.applies.to.in.remission = T,
                                                                transformation = 'identity',
                                                                sex.risk.intercept.multiplier = parameters['recently.suppressed.vs.failing.proportion.lost.or'],
                                                                exclude.from.check = exclude.from.check)
    
    # Durably Suppressed to Disengaged
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='durably.suppressed.to.disengaged',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.lost.or',
                                                                slope.parameter.suffix = 'proportion.lost.slope.or',
                                                                idu.applies.to.in.remission = T,
                                                                transformation = 'identity',
                                                                sex.risk.intercept.multiplier = parameters['durably.suppressed.vs.failing.proportion.lost.or'],
                                                                exclude.from.check = exclude.from.check)
    
    # Failing to Disengaged
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='failing.to.disengaged',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.lost.or',
                                                                slope.parameter.suffix = 'proportion.lost.slope.or',
                                                                idu.applies.to.in.remission = T,
                                                                transformation = 'identity',
                                                                sex.risk.intercept.multiplier = 1,
                                                                exclude.from.check = exclude.from.check)
    
    # Naive to Disengaged
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='naive.to.disengaged',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.lost.or',
                                                                slope.parameter.suffix = 'proportion.lost.slope.or',
                                                                idu.applies.to.in.remission = T,
                                                                transformation = 'identity',
                                                                sex.risk.intercept.multiplier = parameters['naive.vs.failing.proportion.lost.or'],
                                                                exclude.from.check = exclude.from.check)

    # Reengagement
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='reengagement',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'proportion.lost.or',
                                                                slope.parameter.suffix = 'proportion.lost.slope.or',
                                                                idu.applies.to.in.remission = T,
                                                                transformation = 'reciprocal',
                                                                sex.risk.intercept.multiplier = parameters['already.lost.vs.failing.proportion.lost.or'],
                                                                exclude.from.check = exclude.from.check)
 
    # Time to start art
    components = set.intercept.and.slope.alphas.from.parameters(components,
                                                                type='start.art',
                                                                parameters=parameters,
                                                                intercept.parameter.suffix = 'start.art.or',
                                                                slope.parameter.suffix = NULL, #no slope
                                                                idu.applies.to.in.remission = T)

 
    
    #-- Return --#
    components
})

VERSION.MANAGER = register.get.components.function(VERSION.MANAGER, 
                                                   fn=EXPANDED.CONTINUUM.GET.COMPONENTS.FUNCTION,
                                                   version = 'expanded_1.0')





EXPANDED.CONTINUUM.PROJECTION.PARAMETERS.DISTRIBUTION = join.distributions(
    
    continuum.gains.end.by.year = CURRENT.GAINS.END.BY.YEAR,
    
    total.future.suppression.slope.or = TOTAL.FUTURE.SLOPE.OR.DIST,
    total.future.unsuppression.slope.or = TOTAL.FUTURE.SLOPE.OR.DIST,
    total.future.linkage.slope.or = TOTAL.FUTURE.SLOPE.OR.DIST,
    total.future.disengagement.slope.or = TOTAL.FUTURE.SLOPE.OR.DIST,
    total.future.reengagement.slope.or = TOTAL.FUTURE.SLOPE.OR.DIST
)

VERSION.MANAGER = register.projection.parameters.distribution(
    VERSION.MANAGER,
    distribution = join.distributions(BASE.PROJECTION.PARAMETERS.DISTRIBUTION,
                                      EXPANDED.CONTINUUM.PROJECTION.PARAMETERS.DISTRIBUTION),
    version = 'expanded_1.0'
)

EXPANDED.CONTINUUM.UPDATE.COMPONENTS.FUNCTION <- function(parameters, components,
                                                          data.managers=ALL.DATA.MANAGERS)
{
    components = set.background.change.to.years(components,
                                                
                                                linkage = parameters['continuum.gains.end.by.year'],
                                                
                                                naive.to.suppressed = parameters['continuum.gains.end.by.year'],
                                                naive.to.disengaged = parameters['continuum.gains.end.by.year'],
                                                
                                                failing.to.disengaged = parameters['continuum.gains.end.by.year'],
                                                failing.to.suppressed = parameters['continuum.gains.end.by.year'],
                                                
                                                suppressed.to.disengaged = parameters['continuum.gains.end.by.year'],
                                                suppressed.to.failing = parameters['continuum.gains.end.by.year'],
                                                
                                                reengagement = parameters['continuum.gains.end.by.year']
    )
    
    components = set.future.background.slopes(components,
                                              
                                              linkage = parameters['total.future.linkage.slope.or'],
                                              
                                              # to suppressed
                                              naive.to.suppressed = parameters['total.future.suppression.slope.or'],
                                              failing.to.suppressed = parameters['total.future.suppression.slope.or'],
                                              
                                              # to failing
                                              recently.suppressed.to.failing = parameters['total.future.unsuppression.slope.or'],
                                              durably.suppressed.to.failing = parameters['total.future.unsuppression.slope.or'],
                                              
                                              # to disengaged
                                              failing.to.disengaged = parameters['total.future.disengagement.slope.or'],
                                              naive.to.disengaged = parameters['total.future.disengagement.slope.or'],
                                              recently.suppressed.to.disengaged = parameters['total.future.disengagement.slope.or'],
                                              durably.suppressed.to.disengaged = parameters['total.future.disengagement.slope.or'],
                                              
                                              # reengagement
                                              reengagement = parameters['total.future.reengagement.slope.or'],
                                              
                                              after.year = parameters['future.slope.after.year'])
    
    components
}

VERSION.MANAGER = register.projection.update.components.function(
        VERSION.MANAGER,
        fn = join.get.components.functions(BASE.GET.COMPONENTS.FUNCTION,
                                           EXPANDED.CONTINUUM.UPDATE.COMPONENTS.FUNCTION),
        version = 'expanded_1.0'
)

