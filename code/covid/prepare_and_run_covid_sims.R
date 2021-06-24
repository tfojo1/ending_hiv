
##--------------------##
##-- PREPARE SIMSET --##
##--------------------##

COVID.VERSION = '4.0'

prepare.simset.for.covid <- function(simset,
                                     parameters.to.add = NULL)
{
    simset = flatten.simset(simset)
    # A check here
    if (simset@n.sim != 1000)
        stop("not running with 1000 simulations in simset")
    
    bounds = attr(parameters.to.add, 'bounds')
    

    if (is.null(parameters.to.add))
        parameters.to.add = generate.covid.parameters(simset@n.sim)        
    
    simset = add.parameters(simset, 
                            parameters = parameters.to.add[1:simset@n.sim,],
                            parameter.names = dimnames(parameters.to.add)[[2]],
                            parameter.lower.bounds = bounds[1,],
                            parameter.upper.bounds = bounds[2,])
    
    simset
}

##--------------------##
##-- RUN SIMULATION --##
##--------------------##

TPOP.ALL = create.target.population()
COVID.TIME.OFFSET = 2020.25
COVID.EFFECT.ONSET.TIME = 1/365
run.covid.sim <- function(sim, 
                          parameters,
                          run.to=2030,
                          suppression.start.normalize.time=NULL,
                          suppression.normal.time=NULL,
                          prep.start.normalize.time=NULL,
                          prep.normal.time=NULL,
                          testing.start.normalize.time=NULL,
                          testing.normal.time=NULL,
                          sexual.transmission.start.normalize.time=NULL,
                          sexual.transmission.normal.time=NULL,
                          model.sexual.transmission.increase=F,
                          sexual.transmission.start.increase.time=NULL,
                          sexual.transmission.increase.maximal.time=NULL,
                          intervention=NULL)
{
    #-- Set up Intervention Units --#
    
    # Suppression
    if (is.null(suppression.start.normalize.time))
        suppression.start.normalize.time = COVID.TIME.OFFSET + parameters['suppression.start.normalize.time']
    if (is.null(suppression.normal.time))
        suppression.normal.time = COVID.TIME.OFFSET + parameters['suppression.normal.time']
    
    supp.rr = 1 - parameters['suppression.reduction']
    supp = create.intervention.unit('suppression', 
                                    start.year=COVID.TIME.OFFSET, 
                                    end.year=suppression.normal.time, 
                                    rates=c(supp.rr, supp.rr), 
                                    years=c(COVID.TIME.OFFSET + COVID.EFFECT.ONSET.TIME, 
                                                                suppression.start.normalize.time),
                                    apply.function = 'multiplier',
                                    allow.less.than.otherwise = T)

    # PrEP
    if (is.null(prep.start.normalize.time))
        prep.start.normalize.time = COVID.TIME.OFFSET + parameters['prep.start.normalize.time']
    if (is.null(prep.normal.time))
        prep.normal.time = COVID.TIME.OFFSET + parameters['prep.normal.time']
    
    prep.rr = 1 - parameters['prep.reduction']
    prep = create.intervention.unit('prep', 
                                    start.year=COVID.TIME.OFFSET, 
                                    end.year=prep.normal.time, 
                                    rates=c(prep.rr, prep.rr), 
                                    years=c(COVID.TIME.OFFSET + COVID.EFFECT.ONSET.TIME,
                                            prep.start.normalize.time),
                                    apply.function = 'multiplier',
                                    allow.less.than.otherwise = T)
    
    # Testing
    if (is.null(testing.start.normalize.time))
        testing.start.normalize.time = COVID.TIME.OFFSET + parameters['testing.start.normalize.time']
    if (is.null(testing.normal.time))
        testing.normal.time = COVID.TIME.OFFSET + parameters['testing.normal.time']
    
    testing.rr = 1 - parameters['testing.reduction']
    testing = create.intervention.unit('testing', 
                                       start.year=COVID.TIME.OFFSET, 
                                       end.year=testing.normal.time, 
                                       rates=c(testing.rr, testing.rr), 
                                       years=c(COVID.TIME.OFFSET + COVID.EFFECT.ONSET.TIME,
                                               testing.start.normalize.time),
                                       apply.function = 'multiplier',
                                       allow.less.than.otherwise = T)
    
    # Sexual Transmission
    if (is.null(sexual.transmission.start.normalize.time))
        sexual.transmission.start.normalize.time = COVID.TIME.OFFSET + parameters['sexual.transmission.start.normalize.time']
    if (is.null(sexual.transmission.normal.time))
        sexual.transmission.normal.time = COVID.TIME.OFFSET + parameters['sexual.transmission.normal.time']
    
    sexual.transmission.rr = 1 - parameters['sexual.transmission.reduction']
    
    if (model.sexual.transmission.increase)
    {
        if (is.null(sexual.transmission.start.increase.time))
            sexual.transmission.start.increase.time = COVID.TIME.OFFSET + parameters['sexual.transmission.start.increase.time']
        if (is.null(sexual.transmission.increase.maximal.time))
            sexual.transmission.increase.maximal.time = COVID.TIME.OFFSET + parameters['sexual.transmission.increase.maximal.time']
        sexual.transmission.increase.rr = 1 + parameters['sexual.transmission.increase']
        
        rates = c(sexual.transmission.rr,
                  sexual.transmission.rr,
                  sexual.transmission.increase.rr,
                  sexual.transmission.increase.rr)
        years = c(COVID.TIME.OFFSET + COVID.EFFECT.ONSET.TIME,
                  sexual.transmission.start.increase.time,
                  sexual.transmission.increase.maximal.time,
                  sexual.transmission.start.normalize.time)
        
        if (sexual.transmission.start.normalize.time==sexual.transmission.increase.maximal.time)
        {
            rates = rates[-length(rates)]
            years = years[-length(years)]
            
        }
    }
    else
    {
        rates = c(sexual.transmission.rr, sexual.transmission.rr)
        years = c(COVID.TIME.OFFSET + COVID.EFFECT.ONSET.TIME,
                  sexual.transmission.start.normalize.time)
    }
    
    het = create.intervention.unit('heterosexual.transmission', 
                                   start.year=COVID.TIME.OFFSET, 
                                   end.year=sexual.transmission.normal.time, 
                                   rates=rates, 
                                   years=years,
                                   apply.function = 'multiplier',
                                   allow.less.than.otherwise = T)
    msm = create.intervention.unit('msm.transmission', 
                                   start.year=COVID.TIME.OFFSET, 
                                   end.year=sexual.transmission.normal.time, 
                                   rates=rates, 
                                   years=years,
                                   apply.function = 'multiplier',
                                   allow.less.than.otherwise = T)
    
    # Create Intervention from Units
    ints = list(create.intervention(TPOP.ALL,
                                             testing, supp, prep, het, msm))
    
    if (!is.null(intervention))
        ints = c(ints, list(intervention))
    
    # Run Sim
    run.from.year = floor(COVID.TIME.OFFSET)
    run.sim.intervention(sim,
                         intervention=ints,
                         run.from.year=run.from.year,
                         run.to.year=run.to, 
                         keep.years = min(2018, (run.from.year-1)):run.to
                         )
}

##-------------##
##-- HELPERS --##
##-------------##

get.incidence.change <- function(simset, noint,
                                 year1=2020, year2=2021,
                                 mask=T)
{
    simset.inc = sapply(simset@simulations[mask], function(sim){
        extract.incidence(sim, years=year1:year2, keep.dimensions=character(), per.population = NA)
    })
    
    noint.inc = sapply(noint@simulations[mask], function(sim){
        extract.incidence(sim, years=year1:year2, keep.dimensions=character(), per.population = NA)
    })
    
    simset.inc - noint.inc
}

get.covid.simset.name <- function(msa,
                                  scenario=NULL,
                                  intervention.name=NA,
                                  version=COVID.VERSION)
{
    paste0("covid_", version, "_", msa, 
           ifelse(is.null(scenario), "", paste0("_", scenario)),
           ifelse(is.null(intervention.name) || is.na(intervention.name), '', paste0("_", intervention.name)),
           ".Rdata")
}