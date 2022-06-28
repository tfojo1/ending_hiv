
# for now, we're going to use expanded_0.1 as our template since that is working
source('code/applications/expanded_continuum/expanded_continuum_01_jheem_settings.R')
source('code/calibration/parameter_mappings/calibrated_parameters_expanded_01.R')

LAART.CONTINUUM = c('undiagnosed', 
                    'undiagnosed_from_prep', 
                    'unengaged', 
                    'engaged_unsuppressed_naive',
                    'engaged_unsuppressed_failing', 
                    'engaged_suppressed', 
                    #'engaged_recently_suppressed',
                    #'engaged_durably_suppressed',
                    'disengaged_naive',
                    'disengaged_failing')

LAART.TRANSITION.MAPPING = copy.transition.mapping(template=get.settings.for.version('expanded_0.1')$transition.mapping,
                                                                version='laart')



VERSION.MANAGER = copy.and.modify.jheem.settings(
    template.settings = get.settings.for.version('expanded_0.1'),
    
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
                               'engaged_suppressed'
                               #'engaged_recently_suppressed',
                               #'engaged_durably_suppressed'
    ),
    suppressed.continuum.states=c('engaged_suppressed' 
                                  #'engaged_recently_suppressed',
                                  #'engaged_durably_suppressed'
    ),
    
    is.continuum.collapsed=F,
    
    transition.mapping = LAART.TRANSITION.MAPPING
)