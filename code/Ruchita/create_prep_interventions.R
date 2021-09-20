

# This file defines the interventions we will use for PrEP 
#  and registers them to an 'intervention manager'
# Registering them lets the manager know they exist and
#  allows them to be retrieved by name. This is going to
#  help us when we push it to the website

create.prep.interventions <- function(start.year,
                                      implemented.year,
                                      suffix='',
                                      INTERVENTION.MANAGER=INTERVENTION.MANAGER.1.0)
{
    if (suffix != '' && substr(suffix, 1,1)!='_')
        suffix = paste0("_", suffix)

    #-- DEFINE THE INTERVENTION UNITS --#
    
    PREP.10 = create.intervention.unit(type = "prep", start.year = start.year, 
                                       rates = .1, years = implemented.year)
    # RUCHITA - What other coverage levels do we want to use
    
    INJECTABLE.PREP = create.intervention.unit(type = "rr.prep", start.year = start.year, 
                                               rates = .34, years = implemented.year, 
                                               apply.function = "multiplier")

    
    #-- COMBINE TO MAKE SPECIFIC INTERVENTIONS --#
    MSM.P10 = create.intervention(ALL.MSM, PREP.10)
    INTERVENTION.MANAGER = register.intervention(MSM.P10, code=paste0('msm.p10.oral', suffix),
                                                 name='10% of MSM on oral PrEP',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    MSM.IP10 = create.intervention(ALL.MSM, PREP.10, INJECTABLE.PREP)
    INTERVENTION.MANAGER = register.intervention(MSM.IP10, code=paste0('msm.p10.inj', suffix),
                                                 name='10% of MSM on long-acting PrEP',
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
    
    # RUCHITA - what other interventions based off of PrEP levels do we want
    
    #-- RETURN THE INTERVENTION MANAGER --#
    INTERVENTION.MANAGER
}


##-- ACTUALLY CREATE INTERVENTIONS FOR A 2023-2027 time frame --##

INTERVENTION.MANAGER.1.0 = create.prep.interventions(start.year=2023,
                                                     implemented.year=2027,
                                                     suffix='23_27',
                                                     INTERVENTION.MANAGER=INTERVENTION.MANAGER.1.0)