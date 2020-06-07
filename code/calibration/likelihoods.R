
#source('../code/data_managers/hiv_surveillance_manager.R')
#source('../code/estimating_errors/estimate_cdc_errors.R')

library(mvtnorm)
library(reshape2)
library(jheem)

##----------------------------------------------##
##-- THE COMBINED LIKELIHOOD CREATOR FUNCTION --##
##----------------------------------------------##
## This function creates and returns a function to calculate the likelihood
## The returned function takes as input two parameters
##  (1) jheem.result
##  (2) log (optional, default value=T)
## And returns either a likelihood or log-likelihood based on the result

create.full.likelihood <- function(location=BALTIMORE.MSA,
                                   surv=msa.surveillance,
                                   state.surveillance=state.surveillance,
                                   population=get.census.totals(ALL.DATA.MANAGERS$census.totals, location),
                                   use.simulated.denominators=F,
                                   use.sim.msm.proportions=T,
                                   denominator.dimensions='year',
                                   aggregate.denominator.males=T,
                                   msm.cv=0,
                                   idu.cv=0.25,
                                   new.years=2010:2017,
                                   prevalence.years=2009:2016,
                                   mortality.years=2009:2016,
                                   diagnosed.years=2010:2016,
                                   total.new.years=2008:2009,#1993:2010,
                                   total.prevalence.years=2007:2008,
                                   year.to.year.new.correlation=0.0,
                                   year.to.year.prevalence.correlation=0.0,
                                   year.to.year.mortality.correlation=0.0,

                                   new.numerator.year.to.year.chunk.correlation=0.65,
                                   new.numerator.year.to.year.off.correlation=0.25,
                                   new.numerator.chunk.years=list(2011:2014,2015:2017),
                                   prev.numerator.year.to.year.chunk.correlation=0.65,
                                   prev.numerator.year.to.year.off.correlation=0.25,
                                   prev.numerator.chunk.years=list(2010:2013,2014:2016),
                                   mort.numerator.year.to.year.chunk.correlation=0.65,
                                   mort.numerator.year.to.year.off.correlation=0.25,
                                   mort.numerator.chunk.years=list(2010:2013,2014:2016),

                                   new.numerator.sd=function(...){0},
                                   prevalence.numerator.sd=function(...){0},
                                   mortality.numerator.sd=function(...){0},
                                   mortality.bias.fn = function(...){0},
                                   mortality.bias.sd = function(...){0},
                                   diagnosed.numerator.sd=function(...){0},
                                   new.sd.inflation=1,
                                   prevalence.sd.inflation=1,
                                   mortality.sd.inflation=1,
                                   diagnosed.sd.inflation=1,
                                   include.total.new.prev=T,
                                   total.new.sd.inflation=1,
                                   cumulative.mortality.sd.inflation=1,
                                   total.new.numerator.sd=function(...){0},
                                   total.prevalence.sd.inflation=1,
                                   total.prevalence.numerator.sd=function(...){0},
                                   cumulative.mortality.numerator.sd=function(...){0},
                                   
                                   aids.diagnoses.numerator.sd=function(...){0},
                                   aids.diagnoses.sd.inflation=1,
                                   aids.diagnoses.to.hiv.ratio.log.sd=0.5*log(1.1),
                                   
                                   use.total.prevalence=F,
                                   idu.manager=ALL.DATA.MANAGERS$idu,
                                   census.full=ALL.DATA.MANAGERS$census.full,
                                   idu.years=2014:2016,
                                   idu.log.sd=log(2)/2,
                                   verbose=F)
{
    create.combined.likelihood(location=location,
                               surv=surv,
                               population=population,
                               use.2d.targets = T,
                               use.all.targets = T,
                               include.total.new.prev = include.total.new.prev,
                               use.simulated.denominators = use.simulated.denominators,
                               use.sim.msm.proportions = use.sim.msm.proportions,
                               denominator.dimensions = denominator.dimensions,
                               aggregate.denominator.males = aggregate.denominator.males,
                               msm.cv=msm.cv,
                               idu.cv=idu.cv,
                               new.years=new.years,
                               prevalence.years=prevalence.years,
                               mortality.years=mortality.years,
                               diagnosed.years=diagnosed.years,
                               total.new.years=total.new.years,
                               total.prevalence.years=total.prevalence.years,
                               year.to.year.new.correlation = year.to.year.new.correlation,
                               year.to.year.prevalence.correlation = year.to.year.prevalence.correlation,
                               year.to.year.mortality.correlation = year.to.year.mortality.correlation,

                               new.numerator.year.to.year.chunk.correlation=new.numerator.year.to.year.chunk.correlation,
                               new.numerator.year.to.year.off.correlation=new.numerator.year.to.year.off.correlation,
                               new.numerator.chunk.years=new.numerator.chunk.years,
                               prev.numerator.year.to.year.chunk.correlation=prev.numerator.year.to.year.chunk.correlation,
                               prev.numerator.year.to.year.off.correlation=prev.numerator.year.to.year.off.correlation,
                               prev.numerator.chunk.years=prev.numerator.chunk.years,
                               mort.numerator.year.to.year.chunk.correlation=mort.numerator.year.to.year.chunk.correlation,
                               mort.numerator.year.to.year.off.correlation=mort.numerator.year.to.year.off.correlation,
                               mort.numerator.chunk.years=mort.numerator.chunk.years,

                               new.numerator.sd=new.numerator.sd,
                               prevalence.numerator.sd=prevalence.numerator.sd,
                               mortality.numerator.sd=mortality.numerator.sd,
                               mortality.bias.fn=mortality.bias.fn,
                               mortality.bias.sd=mortality.bias.sd,
                               diagnosed.numerator.sd=diagnosed.numerator.sd,
                               new.sd.inflation=new.sd.inflation,
                               prevalence.sd.inflation=prevalence.sd.inflation,
                               mortality.sd.inflation=mortality.sd.inflation,
                               diagnosed.sd.inflation=diagnosed.sd.inflation,
                               total.new.sd.inflation=total.new.sd.inflation,
                               cumulative.mortality.sd.inflation=cumulative.mortality.sd.inflation,
                               total.new.numerator.sd=total.new.numerator.sd,
                               total.prevalence.sd.inflation=total.prevalence.sd.inflation,
                               total.prevalence.numerator.sd=total.prevalence.numerator.sd,
                               cumulative.mortality.numerator.sd=cumulative.mortality.numerator.sd,
                               
                               aids.diagnoses.numerator.sd=aids.diagnoses.numerator.sd,
                               aids.diagnoses.sd.inflation=aids.diagnoses.sd.inflation,
                               aids.diagnoses.to.hiv.ratio.log.sd=aids.diagnoses.to.hiv.ratio.log.sd,
                               
                               use.total.prevalence=use.total.prevalence,
                               idu.manager=idu.manager,
                               census.full=census.full,
                               idu.years=idu.years,
                               idu.log.sd=idu.log.sd,
                               verbose=verbose
                               )
}

create.combined.likelihood <- function(location=BALTIMORE.MSA,
                                   surv=msa.surveillance,
                                   population=get.census.totals(ALL.DATA.MANAGERS$census.totals, location),
                                   use.2d.targets=T,
                                   include.total.new.prev=T,
                                   use.all.targets=T,
                                   use.sex=use.all.targets,
                                   use.race=use.all.targets,
                                   use.risk=use.all.targets,
                                   use.age=use.all.targets,
                                   use.simulated.denominators=F,
                                   use.sim.msm.proportions=T,
                                   denominator.dimensions='year',
                                   aggregate.denominator.males=T,
                                   msm.cv=0,
                                   idu.cv=0,
                                   new.years=2011:2017,
                                   prevalence.years=2010:2016,
                                   mortality.years=2010:2016,
                                   diagnosed.years=2010:2016,
                                   total.new.years=1993:2010,
                                   total.prevalence.years=2007:2009,
                                   year.to.year.new.correlation=0.5,
                                   year.to.year.prevalence.correlation=0.5,
                                   year.to.year.mortality.correlation=0.5,

                                   new.numerator.year.to.year.chunk.correlation=0.65,
                                   new.numerator.year.to.year.off.correlation=0.25,
                                   new.numerator.chunk.years=list(2011:2014,2015:2017),
                                   prev.numerator.year.to.year.chunk.correlation=0.65,
                                   prev.numerator.year.to.year.off.correlation=0.25,
                                   prev.numerator.chunk.years=list(2010:2013,2014:2016),
                                   mort.numerator.year.to.year.chunk.correlation=0.65,
                                   mort.numerator.year.to.year.off.correlation=0.25,
                                   mort.numerator.chunk.years=list(2010:2013,2014:2016),
                                   
                                   aids.diagnoses.numerator.sd=function(...){0},
                                   aids.diagnoses.sd.inflation=1,
                                   aids.diagnoses.to.hiv.ratio.log.sd=0.5*log(1.1),
                                   
                                   new.numerator.sd=function(...){0},
                                   prevalence.numerator.sd=function(...){0},
                                   mortality.numerator.sd=function(...){0},
                                   mortality.bias.fn = function(...){0},
                                   mortality.bias.sd = function(...){0},
                                   diagnosed.numerator.sd=function(...){0},
                                   new.sd.inflation=1,
                                   prevalence.sd.inflation=1,
                                   mortality.sd.inflation=1,
                                   diagnosed.sd.inflation=1,
                                   total.new.sd.inflation=1,
                                   cumulative.mortality.sd.inflation=1,
                                   total.new.numerator.sd=function(...){0},
                                   total.prevalence.sd.inflation=1,
                                   total.prevalence.numerator.sd=function(...){0},
                                   cumulative.mortality.numerator.sd=function(...){0},
                                   use.total.prevalence=F,
                                   idu.manager=ALL.DATA.MANAGERS$idu,
                                   census.full=ALL.DATA.MANAGERS$census.full,
                                   idu.years=2014:2016,
                                   idu.log.sd=log(2)/2,
                                   verbose=F)
{
    new.lik = create.likelihood.function(data.type='new',
                                         years = new.years,
                                        surv=surv,
                                        location=location,
                                        by.total = include.total.new.prev,
                                        by.sex.age=use.2d.targets && use.sex && use.age,
                                        by.sex.race=use.2d.targets && use.sex && use.race,
                                        by.sex.risk=use.2d.targets && use.sex && use.risk,
                                        by.race.risk=use.2d.targets && use.race && use.risk,
                                        by.sex=!use.2d.targets && use.sex,
                                        by.race=!use.2d.targets && use.race,
                                        by.age=!use.2d.targets && use.age,
                                        by.risk=!use.2d.targets && use.risk,
                                        population=population,
                                        use.sim.msm.proportions=use.sim.msm.proportions,
                                        denominator.dimensions=intersect(denominator.dimensions, c('year','age','race','sex','risk')),
                                        aggregate.denominator.males=aggregate.denominator.males,
                                        msm.cv=msm.cv,
                                        idu.cv=idu.cv,
                                        sd.inflation=new.sd.inflation,
                                        year.to.year.correlation = year.to.year.new.correlation,

                                        numerator.year.to.year.chunk.correlation=new.numerator.year.to.year.chunk.correlation,
                                        numerator.year.to.year.off.correlation=new.numerator.year.to.year.off.correlation,
                                        numerator.chunk.years=new.numerator.chunk.years,
                                        numerator.sd = new.numerator.sd
                                        )

    prev.lik = create.likelihood.function(data.type='prevalence',
                                          years=prevalence.years,
                                         surv=surv,
                                         location=location,
                                         by.total = include.total.new.prev,
                                         by.sex.age=use.2d.targets && use.sex && use.age,
                                         by.sex.race=use.2d.targets && use.sex && use.race,
                                         by.sex.risk=use.2d.targets && use.sex && use.risk,
                                         by.race.risk=use.2d.targets && use.race && use.risk,
                                         by.sex=!use.2d.targets && use.sex,
                                         by.race=!use.2d.targets && use.race,
                                         by.age=!use.2d.targets && use.age,
                                         by.risk=!use.2d.targets && use.risk,
                                         population=population,
                                         use.sim.msm.proportions=use.sim.msm.proportions,
                                         denominator.dimensions=intersect(denominator.dimensions, c('year','age','race','sex','risk')),
                                         aggregate.denominator.males=aggregate.denominator.males,
                                         msm.cv=msm.cv,
                                         idu.cv=idu.cv,
                                         sd.inflation=prevalence.sd.inflation,
                                         year.to.year.correlation = year.to.year.prevalence.correlation,

                                         numerator.year.to.year.chunk.correlation=prev.numerator.year.to.year.chunk.correlation,
                                         numerator.year.to.year.off.correlation=prev.numerator.year.to.year.off.correlation,
                                         numerator.chunk.years=prev.numerator.chunk.years,
                                         numerator.sd = prevalence.numerator.sd
    )

    mort.lik = create.likelihood.function(data.type='mortality',
                                          years=mortality.years,
                                         surv=surv,
                                         location=location,
                                         by.sex=use.sex,
                                         by.total = !use.sex,
                                         population=population,
                                         denominator.dimensions=intersect(denominator.dimensions, c('year','sex')),
                                         aggregate.denominator.males=aggregate.denominator.males,
                                         msm.cv=msm.cv,
                                         idu.cv=idu.cv,
                                         sd.inflation = mortality.sd.inflation,
                                         year.to.year.correlation = year.to.year.mortality.correlation,

                                         numerator.year.to.year.chunk.correlation=mort.numerator.year.to.year.chunk.correlation,
                                         numerator.year.to.year.off.correlation=mort.numerator.year.to.year.off.correlation,
                                         numerator.chunk.years=mort.numerator.chunk.years,
                                         numerator.sd = mortality.numerator.sd,
                                         bias.fn = mortality.bias.fn,
                                         bias.sd = mortality.bias.sd
    )

    dx.lik = create.diagnosed.likelihood(surv=state.surveillance, location=location,
                                         years = diagnosed.years,
                                         sd.inflation=diagnosed.sd.inflation, numerator.sd=diagnosed.numerator.sd)

    total.new.lik = create.total.likelihood(data.type='new',
                                            years=total.new.years,
                                            surv=surv,
                                            location=location,
                                            population = population,
                                            sd.inflation = total.new.sd.inflation,
                                            numerator.sd = total.new.numerator.sd)

    total.prev.lik = create.total.likelihood(data.type='prevalence',
                                            years=total.prevalence.years,
                                            surv=surv,
                                            location=location,
                                            population = population,
                                            sd.inflation = total.prevalence.sd.inflation,
                                            numerator.sd = total.prevalence.numerator.sd)

    idu.lik = create.idu.likelihood(idu.manager=idu.manager,
                                    census=census.full,
                                    location=location,
                                    years=idu.years,
                                    log.sd = idu.log.sd)
    
    cum.mort.lik = create.cumulative.mortality.likelihood(surv = surv,
                                                          location = location,
                                                          numerator.sd = cumulative.mortality.numerator.sd,
                                                          sd.inflation = cumulative.mortality.sd.inflation)
    
    aids.lik = create.aids.diagnoses.likelihood(surv=surv,
                                                location=location,
                                                numerator.sd=aids.diagnoses.numerator.sd,
                                                sd.inflation=aids.diagnoses.sd.inflation,
                                                hiv.to.aids.diagnoses.ratio.log.sd=aids.diagnoses.to.hiv.ratio.log.sd)

    if (use.total.prevalence)
        create.joint.likelihood.function(new=new.lik, prev=prev.lik, mort=mort.lik,
                                         dx=dx.lik, total.new=total.new.lik, total.prev=total.prev.lik,
                                         idu=idu.lik, cum.mort=cum.mort.lik, aids=aids.lik,
                                         verbose=verbose)
    else
        create.joint.likelihood.function(new=new.lik, prev=prev.lik, mort=mort.lik,
                                         dx=dx.lik, total.new=total.new.lik, 
                                         idu=idu.lik, cum.mort=cum.mort.lik, aids=aids.lik,
                                         verbose=verbose)
}

create.marginalized.combined.likelihood <- function(location=BALTIMORE.MSA,
                                       surv=msa.surveillance,
                                       state.surveillance=state.surveillance,
                                       population=get.census.totals(ALL.DATA.MANAGERS$census.totals, location),
                                       use.2d.targets=T,
                                       use.all.targets=T,
                                       use.sex=use.all.targets,
                                       use.race=use.all.targets,
                                       use.risk=use.all.targets,
                                       use.age=use.all.targets,
                                       use.simulated.denominators=F,
                                       use.sim.msm.proportions=T,
                                       denominator.dimensions='year',
                                       aggregate.denominator.males=T,
                                       msm.cv=0,
                                       idu.cv=0,
                                       year.to.year.new.correlation=0.5,
                                       year.to.year.prevalence.correlation=0.5,
                                       year.to.year.mortality.correlation=0.5,
                                       numerator.cv.on.log.scale=T,
                                       new.cv=0.5,
                                       prevalence.cv=0.5,
                                       mortality.cv=0.5,
                                       new.fixed.sd=0,
                                       prevalence.fixed.sd=0,
                                       mortality.fixed.sd=0,
                                       new.sd.inflation=1,
                                       prevalence.sd.inflation=1,
                                       mortality.sd.inflation=1,
                                       verbose=F)
{
    stop("need to redo numerator sds")
    lik.components = list()

    year.to.year.correlations = c(new=year.to.year.new.correlation, prevalence=year.to.year.prevalence.correlation)
    numerator.cvs = c(new=new.cv, prevalence=prevalence.cv)
    sd.inflation = c(new=new.sd.inflation, prevalence=prevalence.sd.inflation)

    for (data.type in c('new','prevalence'))
    {
        if (use.2d.targets && use.sex && use.age)
        {
            one.lik = create.likelihood.function(data.type=data.type,
                                                 surv=surv,
                                                 location=location,
                                                 by.sex.age=T,
                                                 population=population,
                                                 use.sim.msm.proportions=use.sim.msm.proportions,
                                                 denominator.dimensions=intersect(denominator.dimensions, c('year','age','race','sex','risk')),
                                                 aggregate.denominator.males=aggregate.denominator.males,
                                                 msm.cv=msm.cv,
                                                 idu.cv=idu.cv,
                                                 sd.inflation=sd.inflation[data.type],
                                                 year.to.year.correlation = year.to.year.correlations[data.type],
                                                 numerator.cv.on.log.scale=numerator.cv.on.log.scale,
                                                 numerator.cv = numerator.cvs[data.type],
                                                 numerator.fixed.sd = numerator.fixed.sds[data.type]
            )
            lik.components = c(lik.components, list(one.lik))
        }

        if (use.2d.targets && use.sex && use.race)
        {
            one.lik = create.likelihood.function(data.type=data.type,
                                                 surv=surv,
                                                 location=location,
                                                 by.sex.race=T,
                                                 population=population,
                                                 use.sim.msm.proportions=use.sim.msm.proportions,
                                                 denominator.dimensions=intersect(denominator.dimensions, c('year','age','race','sex','risk')),
                                                 aggregate.denominator.males=aggregate.denominator.males,
                                                 msm.cv=msm.cv,
                                                 idu.cv=idu.cv,
                                                 sd.inflation=sd.inflation[data.type],
                                                 year.to.year.correlation = year.to.year.correlations[data.type],
                                                 numerator.cv.on.log.scale=numerator.cv.on.log.scale,
                                                 numerator.cv = numerator.cvs[data.type],
                                                 numerator.fixed.sd = numerator.fixed.sds[data.type]
            )
            lik.components = c(lik.components, list(one.lik))
        }

        if (use.2d.targets && use.sex && use.risk)
        {
            one.lik = create.likelihood.function(data.type=data.type,
                                                 surv=surv,
                                                 location=location,
                                                 by.sex.risk=T,
                                                 population=population,
                                                 use.sim.msm.proportions=use.sim.msm.proportions,
                                                 denominator.dimensions=intersect(denominator.dimensions, c('year','age','race','sex','risk')),
                                                 aggregate.denominator.males=aggregate.denominator.males,
                                                 msm.cv=msm.cv,
                                                 idu.cv=idu.cv,
                                                 sd.inflation=sd.inflation[data.type],
                                                 year.to.year.correlation = year.to.year.correlations[data.type],
                                                 numerator.cv.on.log.scale=numerator.cv.on.log.scale,
                                                 numerator.cv = numerator.cvs[data.type],
                                                 numerator.fixed.sd = numerator.fixed.sds[data.type]
            )
            lik.components = c(lik.components, list(one.lik))
        }

        if (use.2d.targets && use.race && use.risk)
        {
            one.lik = create.likelihood.function(data.type=data.type,
                                                 surv=surv,
                                                 location=location,
                                                 by.race.risk=T,
                                                 population=population,
                                                 use.sim.msm.proportions=use.sim.msm.proportions,
                                                 denominator.dimensions=intersect(denominator.dimensions, c('year','age','race','sex','risk')),
                                                 aggregate.denominator.males=aggregate.denominator.males,
                                                 msm.cv=msm.cv,
                                                 idu.cv=idu.cv,
                                                 year.to.year.correlation = year.to.year.correlations[data.type],
                                                 numerator.cv.on.log.scale=numerator.cv.on.log.scale,
                                                 numerator.cv = numerator.cvs[data.type],
                                                 numerator.fixed.sd = numerator.fixed.sds[data.type]
            )
            lik.components = c(lik.components, list(one.lik))
        }

        if (!use.2d.targets && use.sex)
        {
            one.lik = create.likelihood.function(data.type=data.type,
                                                 surv=surv,
                                                 location=location,
                                                 by.sex=T,
                                                 population=population,
                                                 use.sim.msm.proportions=use.sim.msm.proportions,
                                                 denominator.dimensions=intersect(denominator.dimensions, c('year','age','race','sex','risk')),
                                                 aggregate.denominator.males=aggregate.denominator.males,
                                                 msm.cv=msm.cv,
                                                 idu.cv=idu.cv,
                                                 sd.inflation=sd.inflation[data.type],
                                                 year.to.year.correlation = year.to.year.correlations[data.type],
                                                 numerator.cv.on.log.scale=numerator.cv.on.log.scale,
                                                 numerator.cv = numerator.cvs[data.type],
                                                 numerator.fixed.sd = numerator.fixed.sds[data.type]
            )
            lik.components = c(lik.components, list(one.lik))
        }

        if (!use.2d.targets && use.race)
        {
            one.lik = create.likelihood.function(data.type=data.type,
                                                 surv=surv,
                                                 location=location,
                                                 by.race=T,
                                                 population=population,
                                                 use.sim.msm.proportions=use.sim.msm.proportions,
                                                 denominator.dimensions=intersect(denominator.dimensions, c('year','age','race','sex','risk')),
                                                 aggregate.denominator.males=aggregate.denominator.males,
                                                 msm.cv=msm.cv,
                                                 idu.cv=idu.cv,
                                                 sd.inflation=sd.inflation[data.type],
                                                 year.to.year.correlation = year.to.year.correlations[data.type],
                                                 numerator.cv.on.log.scale=numerator.cv.on.log.scale,
                                                 numerator.cv = numerator.cvs[data.type],
                                                 numerator.fixed.sd = numerator.fixed.sds[data.type]
            )
            lik.components = c(lik.components, list(one.lik))
        }

        if (!use.2d.targets && use.age)
        {
            one.lik = create.likelihood.function(data.type=data.type,
                                                 surv=surv,
                                                 location=location,
                                                 by.age=T,
                                                 population=population,
                                                 use.sim.msm.proportions=use.sim.msm.proportions,
                                                 denominator.dimensions=intersect(denominator.dimensions, c('year','age','race','sex','risk')),
                                                 aggregate.denominator.males=aggregate.denominator.males,
                                                 msm.cv=msm.cv,
                                                 idu.cv=idu.cv,
                                                 sd.inflation=sd.inflation[data.type],
                                                 year.to.year.correlation = year.to.year.correlations[data.type],
                                                 numerator.cv.on.log.scale=numerator.cv.on.log.scale,
                                                 numerator.cv = numerator.cvs[data.type],
                                                 numerator.fixed.sd = numerator.fixed.sds[data.type]
            )
            lik.components = c(lik.components, list(one.lik))
        }

        if (!use.2d.targets && use.risk)
        {
            one.lik = create.likelihood.function(data.type=data.type,
                                                 surv=surv,
                                                 location=location,
                                                 by.risk=T,
                                                 population=population,
                                                 use.sim.msm.proportions=use.sim.msm.proportions,
                                                 denominator.dimensions=intersect(denominator.dimensions, c('year','age','race','sex','risk')),
                                                 aggregate.denominator.males=aggregate.denominator.males,
                                                 msm.cv=msm.cv,
                                                 idu.cv=idu.cv,
                                                 sd.inflation=sd.inflation[data.type],
                                                 year.to.year.correlation = year.to.year.correlations[data.type],
                                                 numerator.cv.on.log.scale=numerator.cv.on.log.scale,
                                                 numerator.cv = numerator.cvs[data.type],
                                                 numerator.fixed.sd = numerator.fixed.sds[data.type]
            )
            lik.components = c(lik.components, list(one.lik))
        }
    }

    mort.lik = create.likelihood.function(data.type='mortality',
                                          surv=surv,
                                          location=location,
                                          by.sex=use.sex,
                                          by.total = !use.sex,
                                          population=population,
                                          denominator.dimensions=intersect(denominator.dimensions, c('year','sex')),
                                          aggregate.denominator.males=aggregate.denominator.males,
                                          msm.cv=msm.cv,
                                          idu.cv=idu.cv,
                                          sd.inflation=mortality.sd.inflation,
                                          year.to.year.correlation = year.to.year.mortality.correlation,
                                          numerator.cv.on.log.scale=numerator.cv.on.log.scale,
                                          numerator.cv = mortality.cv,
                                          numerator.fixed.sd = mortality.fixed.sd
    )
    lik.components = c(lik.components, list(mort.lik))

    dx.lik = create.diagnosed.likelihood(surv=state.surveillance, location=location)
    lik.components = c(lik.components, list(dx.lik))

    print(paste0("Combining ", length(lik.components), " components (marginally) into the likelihood"))
    create.joint.likelihood.function(lik.components, verbose=verbose)
}

##-----------------------------------------------------##
##-- THE MAIN LIKELIHOOD COMPONENT CREATOR FUNCTIONS --##
##-----------------------------------------------------##
## These functions create and return functions to calculate the likelihood
## The returned function takes as input two parameters
##  (1) jheem.result
##  (2) log (optional, default value=T)
## And returns either a likelihood or log-likelihood based on the result

create.likelihood.function <- function(data.type=c('new','prevalence','mortality')[1],
                                       years=NULL,
                                       surv=msa.surveillance,
                                       location=BALTIMORE.MSA,
                                       by.total=F,
                                       by.sex=F,
                                       by.race=F,
                                       by.age=F,
                                       by.risk=F,
                                       by.sex.age=F,
                                       by.sex.race=F,
                                       by.sex.risk=F,
                                       by.race.risk=F,
                                       ages=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
                                       races=c('black','hispanic','other'),
                                       sexes=c('heterosexual_male','msm','female'),
                                       risks=c('never_IDU','active_IDU','IDU_in_remission'),
                                       sd.inflation=1,
                                       sd.inflation.for.observations=1,
                                       population=get.census.totals(ALL.DATA.MANAGERS$census.totals, location),
                                       use.simulated.denominators=F,
                                       use.sim.msm.proportions=F,
                                       denominator.dimensions='year',
                                       aggregate.denominator.males=F,
                                       msm.cv=0,
                                       idu.cv=0.25,
                                       year.to.year.correlation=0,
                                       numerator.year.to.year.chunk.correlation=0,
                                       numerator.year.to.year.off.correlation=0,
                                       numerator.chunk.years=numeric(),
                                       numerator.sd = function(...){0},
                                       bias.fn = function(...){0},
                                       bias.sd = function(...){0})
{
#    print(denominator.dimensions)

    #If years parameter is null, use all years for which there are data
    data.for.all = get.surveillance.data(surv, location, data.type=data.type)
    available.years = attr(data.for.all, 'years')
    if (is.null(years))
        years = available.years
    else
        years = intersect(years, available.years)

    #Set up the population to use as denominators
#    population = apply(population, c('year','age','race','sex','risk'), sum)[as.character(years),,,,]
    population = access(population, year=as.character(years))
    target.dim.names = list(year=years,
                            age=SETTINGS$AGES$labels,
                            race=SETTINGS$RACES,
                            sex=SETTINGS$SEXES,
                            risk=SETTINGS$RISK_STRATA)#dimnames(population)
    population = apply(population, denominator.dimensions, sum)
    population = expand.population(population, target.dim.names = target.dim.names)
    if ((aggregate.denominator.males || use.sim.msm.proportions) && any(denominator.dimensions=='sex'))
    {
        males = population[,,,'msm',] + population[,,,'heterosexual_male',]
        population[,,,'msm',] = males
        population[,,,'heterosexual_male',] = males
    }

    #Set up denominator elements
    if (all(denominator.dimensions!='risk'))
        idu.cv = 0
    if (all(denominator.dimensions!='sex') || aggregate.denominator.males || use.sim.msm.proportions)
        msm.cv = 0

    fixed.denominator.elements = create.simple.denominator.elements(population,
                                                                    msm.cv = msm.cv, idu.cv = idu.cv)

    population.sums.by.year = apply(population, 'year', sum)

    ##---------------------------------------------##
    ##-- Set up the likelihood elements/indexing --##
    ##---------------------------------------------##

    likelihood.elements = create.likelihood.elements.for.data.type(data.type=data.type, years=years, surv=surv, location=location,
                                                                   by.total=by.total,
                                                                   by.sex=by.sex, by.race=by.race, by.age=by.age, by.risk=by.risk,
                                                                   by.sex.age=by.sex.age, by.sex.race=by.sex.race,
                                                                   by.sex.risk=by.sex.risk, by.race.risk=by.race.risk,
                                                                   ages=ages, races=races, sexes=sexes, risks=risks,
                                                                   numerator.sd = numerator.sd,
                                                                   bias.fn = bias.fn,
                                                                   bias.sd = bias.sd,
                                                                   numerator.year.to.year.chunk.correlation=numerator.year.to.year.chunk.correlation,
                                                                   numerator.year.to.year.off.correlation=numerator.year.to.year.off.correlation,
                                                                   numerator.chunk.years=numerator.chunk.years
    )

    if (is.null(likelihood.elements$numerator.covar.mat))
        numerator.covar.mat = NULL
    else
        numerator.covar.mat = likelihood.elements$numerator.covar.mat * sd.inflation.for.observations^2

    # Set up the correlation matrix, if needed
    if (year.to.year.correlation == 0)
        corr.mat = NULL
    else
        corr.mat = make.compound.symmetry.by.year.matrix(access(population, year=as.character(years)), correlation=year.to.year.correlation)

    ##-------------------------------------------------------##
    ##-- Set up SD Inflation (if dependent on a data.type) --##
    ##-------------------------------------------------------##

    if (is(sd.inflation, 'character'))
    {
        data.for.all.for.sd.inflation = get.surveillance.data(surv, location, data.type=sd.inflation)
        sd.inflation.years = attr(data.for.all, 'years')
        sd.inflation.elements = create.likelihood.elements.for.data.type(data.type=sd.inflation,
                                                                         years=sd.inflation.years, surv=surv, location=location,
                                                                         by.total=by.total,
                                                                         by.sex=by.sex, by.race=by.race, by.age=by.age, by.risk=by.risk,
                                                                         by.sex.age=by.sex.age, by.sex.race=by.sex.race,
                                                                         by.sex.risk=by.sex.risk, by.race.risk=by.race.risk,
                                                                         ages=ages, races=races, sexes=sexes, risks=risks,
                                                                         numerator.sd = numerator.sd,
                                                                         numerator.year.to.year.chunk.correlation=numerator.year.to.year.chunk.correlation,
                                                                         numerator.year.to.year.off.correlation=numerator.year.to.year.off.correlation,
                                                                         numerator.chunk.years=numerator.chunk.years
                                                                         )

        sd.inflation = get.sd.inflation(match.to.response = likelihood.elements$response.vector,
                                        match.to.descriptions = likelihood.elements$descriptions,
                                        response.2 = sd.inflation.elements$response.vector,
                                        descriptions.2 = sd.inflation.elements$descriptions)

    }

    ##------------------------------------------------------------------------------------##
    ##-- Set up a mask to pull out components we don't use in the transformation matrix --##
    ##------------------------------------------------------------------------------------##

    denominators.are.totals = length(denominator.dimensions)==1 & denominator.dimensions=='year'
    if (is.null(corr.mat) &&
        !use.simulated.denominators &&
        !(use.sim.msm.proportions && !aggregate.denominator.males && any(denominator.dimensions=='sex')))
    {
        transformation.mapping = make.transformation.mapping(likelihood.elements$transformation.matrix,
                                                             likelihood.elements$denominator.vector)

        likelihood.elements$transformation.matrix = likelihood.elements$transformation.matrix[,transformation.mapping$first.in.signature]
        fixed.denominator.elements$denominator.vector = fixed.denominator.elements$denominator.vector[transformation.mapping$first.in.signature]

        if (!is.null(fixed.denominator.elements$denominator.covar.mat))
            fixed.denominator.elements$denominator.covar.mat = fixed.denominator.elements$denominator.covar.mat[transformation.mapping$first.in.signature,transformation.mapping$first.in.signature]
    }
    else
        transformation.mapping = NULL

    ##----------------------------------------------------##
    ##-- Make and return the actual likelihood function --##
    ##----------------------------------------------------##

    function(jheem.results, log=T){

        # Check if we terminated early
        if (jheem.results$terminated)
        {
            if (log)
                return (-Inf)
            else
                return (0)
        }

        # Set up the denominators
        if (denominators.are.totals)
            denominator.elements = fixed.denominator.elements
        else if (use.simulated.denominators)
        {
            stop("Need to update for denominator dimensions")
            sim.pop = extract.population.subset(jheem.results, years=years,
                                                per.population=NA,
                                                keep.dimensions = c('year','age','race','sex','risk'))

            sim.pop.sums.by.year = apply(sim.pop, 'year', sum)
            sim.pop = sim.pop * population.sums.by.year / sim.pop.sums.by.year

            denominator.elements = create.simple.denominator.elements(sim.pop,
                                                                      msm.cv = msm.cv, idu.cv = idu.cv)
        }
        else if (use.sim.msm.proportions && !aggregate.denominator.males && any(denominator.dimensions=='sex'))
            denominator.elements = create.simple.denominator.elements.from.msm.proportions(population,
                                                                                           msm.proportions=attr(jheem.results, 'msm.proportions.by.race'),
                                                                                           idu.cv = idu.cv)
        else
            denominator.elements = fixed.denominator.elements


        rates = pull.simulations.rates(jheem.results,
                                       data.type=data.type,
                                       years=years,
                                       denominator.dimensions=denominator.dimensions,
                                       aggregate.denominator.males=aggregate.denominator.males)

        # Pass it all to the sub-function to crunch
        likelihood.sub(rates,
                       transformation.matrix = likelihood.elements$transformation.matrix,
                       response.vector = likelihood.elements$response.vector,
                       denominator.vector = denominator.elements$denominator.vector,
                       denominator.covar.mat = denominator.elements$denominator.covar.mat,
                       numerator.covar.mat = numerator.covar.mat,
                       corr.mat = corr.mat,
                       sd.inflation=sd.inflation,
                       log=log,
                       sim=jheem.results,
                       transformation.mapping=transformation.mapping,
                       description= likelihood.elements$descriptions)
    }
}

create.diagnosed.likelihood <- function(years=NULL,
                                        surv=state.surveillance,
                                        location=BALTIMORE.MSA,
                                        numerator.sd=function(...){0},
                                        sd.inflation=1,
                                        ar=F,
                                        rho=0.55)
{
    means.and.sds = get.diagnosed.means.and.sds(location=location, surv=surv, years=years)
    mean.diagnosed = means.and.sds$means
    sds = means.and.sds$sds * sd.inflation
    years = means.and.sds$years

    not.na.mask = !is.na(mean.diagnosed)
    mean.diagnosed = mean.diagnosed[not.na.mask]

    if (ar)
    {
        year.diff = matrix(abs(rep(years, length(years)) - rep(years, each=length(years))),
                           nrow=length(years), ncol=length(years))
        cov.mat = sds %*% t(sds) * rho^year.diff
    }
    else
    {
        cov.mat = matrix(rho, nrow=length(years), ncol=length(years))
        diag(cov.mat) = 1
        cov.mat = sds %*% t(sds) * cov.mat
    }

    numerator.sds = numerator.sd(year=years, num=mean.diagnosed)
    if (length(numerator.sds)==1)
        numerator.sds = rep(numerator.sds, length(years))
    cov.mat = cov.mat + diag(numerator.sds^2)

    function(sim, log=T)
    {
        sim.diagnosed = extract.diagnosed.hiv(sim, years=years, keep.dimensions = 'year')
        dmvnorm(sim.diagnosed, mean=mean.diagnosed, sigma=cov.mat, log=log)
    }
}


create.total.likelihood <- function(data.type,
                                    years=NULL,
                                    surv=msa.surveillance,
                                    location=BALTIMORE.MSA,
                                    population=get.census.totals(ALL.DATA.MANAGERS$census.totals, location),
                                    numerator.sd=function(...){0},
                                    sd.inflation=1,
                                    ar=T,
                                    rho=0.7)
{
    if (data.type!='new' && data.type!='prevalence' && data.type!='mortality')
        stop("data.type must be either 'new', 'prevalence', or 'diagnosed'")

    observed = get.surveillance.data(msa.surveillance, location=location, data.type=data.type)
    available.years = attr(observed, 'years')[!is.na(observed)]
    if (is.null(years))
        years = available.years
    else
        years = intersect(years, available.years)
    observed = as.numeric(observed[as.character(years)])

    if (is.null(dim(population)))
        denominators = population[as.character(years)]
    else
        denominators = apply(population, 'year', sum)[as.character(years)]

    if (ar)
    {
        year.diff = matrix(abs(rep(years, length(years)) - rep(years, each=length(years))),
                           nrow=length(years), ncol=length(years))
        cor.mat = rho^year.diff
    }
    else
    {
        cor.mat = matrix(rho, nrow=length(years), ncol=length(years))
        diag(cor.mat) = 1
    }

    numerator.sds = numerator.sd(year=years, num=observed)
    if (length(numerator.sds)==1)
        numerator.sds = rep(numerator.sds, length(years))

    function(sim, log=T)
    {
        if (data.type=='new')
            sim.rates = extract.new.diagnoses(sim,
                                               years=years,
                                               keep.dimensions = 'year',
                                               per.population = 1)
        else if (data.type=='prevalence')
            sim.rates = extract.prevalence(sim, continuum='diagnosed',
                                            years=years,
                                           keep.dimensions = 'year',
                                           per.population = 1)
        else if (data.type=='mortality')
            sim.rates = extract.overall.hiv.mortality(sim, continuum='diagnosed',
                                                       years=years,
                                                      keep.dimensions = 'year',
                                                      per.population = 1)
        else
            stop("data.type must be either 'new', 'prevalence', or 'diagnosed'")

        sds = sqrt(denominators * sim.rates * (1-sim.rates))
        mean.vector = denominators * sim.rates

        cov.mat = sds %*% t(sds) * cor.mat * sd.inflation + diag(numerator.sds^2)
        dmvnorm(x=observed, mean=mean.vector, sigma = cov.mat, log=log)
    }
}

create.aids.diagnoses.likelihood <- function(surv=msa.surveillance,
                                             location=BALTIMORE.MSA,
                                             numerator.sd=function(years,num){rep(0,length(num))},
                                             sd.inflation=1,
                                             years=1999:2002,
                                             population=get.census.totals(ALL.DATA.MANAGERS$census.totals,
                                                                          location, years=years, flatten.single.dim.array = T),
                                             hiv.to.aids.diagnoses.ratio=c('1999'=1.45,
                                                                           '2000'=1.56,
                                                                           '2001'=1.51,
                                                                           '2002'=1.39,
                                                                           '2003'=1.35,
                                                                           '2004'=1.25)[as.character(years)],
                                             hiv.to.aids.diagnoses.ratio.log.sd=0.5*log(1.1),
                                             rho=0.5,
                                             verbose=F
                                             )
{
    observed.aids = get.surveillance.data(location.codes = location, data.type='aids.diagnoses', years=years)
    obs.sds = numerator.sd(years, observed.aids)
    
    #transform to lognormal based on mean and sd
    cv = obs.sds / observed.aids
    log.var = log(1 + cv^2)
    log.mean = log(observed.aids) - log.var/2
  
    #add log ratio
    log.ratios = log(hiv.to.aids.diagnoses.ratio) - hiv.to.aids.diagnoses.ratio.log.sd^2/2
    log.mean = log.mean + log.ratios
    log.var = log.var + hiv.to.aids.diagnoses.ratio.log.sd^2

    #put back on exp scale
    obs = exp(log.mean + log.var/2)
    obs.var = (exp(log.var)-1) * exp(2*log.mean + log.var)
    
    function(sim, log=T)
    {
        numerators = extract.new.diagnoses(sim, years=years, keep.dimensions = 'year', per.population = NA)
        denominators = extract.population.subset(sim, years=years, keep.dimensions = 'year',
                                                per.population = NA)
        
        rates = numerators / denominators
        
      #  sds = sqrt(population * rates * (1-rates) * sd.inflation^2 + obs.var)

        
        cov.mat = make.compound.symmetry.matrix(sqrt(obs.var), rho) + 
            diag(population * rates * (1-rates) * sd.inflation^2)
        
        if (verbose)
            print(cbind(obs=obs, sim=rates*population))
        
        dmvnorm(x=as.numeric(obs),
                mean=as.numeric(rates*population),
                sigma=cov.mat, 
                log=log)
        
    }
}

#.9 captured from https://jamanetwork.com/journals/jama/fullarticle/405140 (cited in 2001-2002 surveillance report)
create.cumulative.mortality.likelihood <- function(years=1981:2000,
                                                   surv=msa.surveillance,
                                                   location=BALTIMORE.MSA,
                                                   population=sum(get.census.totals(ALL.DATA.MANAGERS$census.totals,
                                                                                    location, years=years)),
                                                   rho=0.5,
                                                   numerator.sd=function(...){0},
                                                   sd.inflation=1,
                                                   fraction.captured=0.9,
                                                   verbose=F) #.85 and .9 are AIDS reported and deaths captured from techcnical appendix of volume 14 report - 2001-2002. .95 is my guess for how many HIV deaths are in people with AIDS
{
    observed = get.surveillance.data(location.codes = location, data.type = 'cumulative.aids.mortality', sex=T,race=T,risk=T)[1,,,]
    observed = apply(observed, c('race','sex','risk'), function(x){x})
    observed[,'female',c('msm','msm_idu')] = NA
    mask = !is.na(observed)
    observed = observed[mask]
    
    numerator.sds = numerator.sd(year=rep(max(years), length(observed)), num=observed)
    
    function(sim, log=T)
    {
        numerators = extract.overall.hiv.mortality(sim, years=years, keep.dimensions = c('race','sex','risk'),
                                                   continuum = 'diagnosed',
                                                   use.cdc.categorizations = T, per.population = NA)
        denominator = extract.population.subset(sim, years=years, keep.dimensions = character(),
                                                 use.cdc.categorizations = T, per.population = NA)
        
        rates = numerators[mask] / denominator * fraction.captured
        
        sds = sqrt(population * rates * (1-rates) * sd.inflation^2 + numerator.sds^2)
        
 
#        cov.mat = make.compound.symmetry.matrix(numerator.sds, rho) +
 #           diag( population * rates * (1-rates) * sd.inflation^2 )
        
#        cov.mat = make.compound.symmetry.matrix(sqrt(population * rates * (1-rates) * sd.inflation^2), rho) +
        #   diag(numerator.sds^2)   
     #   cbind(observed, sim=rates*population, sd=sqrt(diag(cov.mat)), model=sqrt(population*rates*(1-rates)*sd.inflation^2),
      #        obs=numerator.sds)
      
        piecewise = dnorm(x = observed,
                          mean = rates * population, 
                          sd = sds,
                          log = log)

    if (verbose)
    {
        print(cbind(melt(numerators/denominator*population)[mask,], observed, d=piecewise))
        print(c(sim=sum(rates*population), obs=sum(observed)))
    }

        if (log)
            sum(piecewise)
        else
            prod(piecewise)
        
    #    dmvnorm(x = observed,
    #            mean = rates * population,
    #            sigma = cov.mat,
    #            log=log)
    }
}

create.idu.likelihood <- function(idu.manager=ALL.DATA.MANAGERS$idu,
                                  census=ALL.DATA.MANAGERS$census.full,
                                  location=BALTIMORE.MSA,
                                  years=2014:2016,
                                  log.sd=log(2)/2,
                                  verbose=F)
{
    fips = counties.for.msa(location)
    if (length(fips)==0)
        fips = counties.for.state(location)
    if (length(fips)==0)
        fips = location
    if (any(is.na(county.names(fips))))
        stop("Invalid location - does not match a state, MSA, or county")

    idu.abs.proportions = get.aggregate.idu.proportions(idu.manager=idu.manager,
                                                    census=census,
                                                    fips=fips,
                                                    years=years)

    idu.relative.proportions = get.relative.idu.proportions(idu.manager=idu.manager,
                                                           census=census,
                                                           fips=fips,
                                                           years=years)

    names(idu.relative.proportions$idu.30d.by.age) = paste0('active_', names(idu.relative.proportions$idu.30d.by.age))
    names(idu.relative.proportions$idu.prior.by.age) = paste0('remission_', names(idu.relative.proportions$idu.30d.by.age))

    names(idu.relative.proportions$idu.30d.by.race) = paste0('active_', names(idu.relative.proportions$idu.30d.by.race))
    names(idu.relative.proportions$idu.prior.by.race) = paste0('remission_', names(idu.relative.proportions$idu.prior.by.race))

    names(idu.relative.proportions$idu.30d.by.sex) = paste0('active_', names(idu.relative.proportions$idu.30d.by.sex))
    names(idu.relative.proportions$idu.prior.by.sex) = paste0('remission_', names(idu.relative.proportions$idu.prior.by.sex))

    function(sim, log=T)
    {
        MIN = 0.000000001
        #-- Total Prevalence --#
        sim.active.idu.prevalence = max(MIN,extract.population.subset(sim, years=years, risk='active_IDU', keep.dimensions=character()) /
            extract.population.subset(sim, years=years, keep.dimensions=character()))
        sim.idu.in.remission.prevalence = max(MIN, extract.population.subset(sim, years=years, risk='IDU_in_remission', keep.dimensions=character()) /
            extract.population.subset(sim, years=years, keep.dimensions=character()))

        likelihood.components = c(active_prevalence=dnorm(log(idu.abs.proportions$idu.30d), mean=log(sim.active.idu.prevalence), sd=log.sd, log=log),
                                  remission_prevalence=dnorm(log(idu.abs.proportions$idu.prior), mean=log(sim.idu.in.remission.prevalence), sd=log.sd, log=log))

        #-- Age Ratios --#
        sim.active.idu.by.age = pmax(MIN,extract.population.subset(sim, years=years, risk='active_IDU', keep.dimensions='age') /
            extract.population.subset(sim, years=years, keep.dimensions='age'))
        sim.idu.in.remission.by.age = pmax(MIN, extract.population.subset(sim, years=years, risk='IDU_in_remission', keep.dimensions='age') /
            extract.population.subset(sim, years=years, keep.dimensions='age'))

        mask = idu.relative.proportions$idu.30d.by.age != 1
        likelihood.components = c(likelihood.components, dnorm(log(idu.relative.proportions$idu.30d.by.age[mask]),
                                                               mean=log(sim.active.idu.by.age[mask]) - log(sim.active.idu.by.age[!mask]),
                                                               sd = log.sd, log=log))
        likelihood.components = c(likelihood.components, dnorm(log(idu.relative.proportions$idu.prior.by.age[mask]),
                                                               mean=log(sim.idu.in.remission.by.age[mask]) - log(sim.idu.in.remission.by.age[!mask]),
                                                               sd = log.sd, log=log))

        #-- Sex Ratios --#
        sim.active.idu.by.sex = pmax(MIN,extract.population.subset(sim, years=years, risk='active_IDU', keep.dimensions='sex') /
            extract.population.subset(sim, years=years, keep.dimensions='sex'))
        sim.idu.in.remission.by.sex = pmax(MIN,extract.population.subset(sim, years=years, risk='IDU_in_remission', keep.dimensions='sex') /
            extract.population.subset(sim, years=years, keep.dimensions='sex'))

        mask = idu.relative.proportions$idu.30d.by.sex != 1
        likelihood.components = c(likelihood.components, dnorm(log(idu.relative.proportions$idu.30d.by.sex[mask]),
                                                               mean=log(sim.active.idu.by.sex[mask]) - log(sim.active.idu.by.sex[!mask]),
                                                               sd = log.sd, log=log))
        likelihood.components = c(likelihood.components, dnorm(log(idu.relative.proportions$idu.prior.by.sex[mask]),
                                                               mean=log(sim.idu.in.remission.by.sex[mask] - log(sim.idu.in.remission.by.sex[!mask])),
                                                               sd = log.sd, log=log))

        #-- Race Ratios --#
        sim.active.idu.by.race = pmax(MIN,extract.population.subset(sim, years=years, risk='active_IDU', keep.dimensions='race') /
            extract.population.subset(sim, years=years, keep.dimensions='race'))
        sim.idu.in.remission.by.race = pmax(MIN,extract.population.subset(sim, years=years, risk='IDU_in_remission', keep.dimensions='race') /
            extract.population.subset(sim, years=years, keep.dimensions='race'))

        mask = idu.relative.proportions$idu.30d.by.race != 1
        likelihood.components = c(likelihood.components, dnorm(log(idu.relative.proportions$idu.30d.by.race[mask]),
                                                               mean=log(sim.active.idu.by.race[mask]) - log(sim.active.idu.by.race[!mask]),
                                                               sd = log.sd, log=log))
        likelihood.components = c(likelihood.components, dnorm(log(idu.relative.proportions$idu.prior.by.race[mask]),
                                                               mean=log(sim.idu.in.remission.by.race[mask]) - log(sim.idu.in.remission.by.race[!mask]),
                                                               sd = log.sd, log=log))

        if (verbose)
            print(likelihood.components)

        if (log)
            sum(likelihood.components)
        else
            prod(likelihood.components)
    }
}

create.joint.likelihood.function <- function(..., verbose=F)
{
    args = list(...)
    sub.likelihoods = list()
    for (i in 1:length(args))
    {
        if (is(args[[i]], 'list'))
            sub.likelihoods = c(sub.likelihoods, args[[i]])
        else
            sub.likelihoods = c(sub.likelihoods, args[i])
    }

    function(jheem.results, log=T){
        if (jheem.results$terminated)
        {
            if (log)
                return (-Inf)
            else
                return (0)
        }


        sub.values = sapply(sub.likelihoods, function(lik){
            lik(jheem.results, log=log)
        })

        if (verbose)
            print(paste0("Likelihood components: ",
                         paste0(names(sub.likelihoods), '=', sub.values, collapse = ', ')))

        if (log)
            sum(sub.values)
        else
            prod(sub.values)
    }
}

##------------------------------------------------##
##-- HELPERS TO REDUCE COMPUTATION FOR MATRICES --##
##------------------------------------------------##

make.transformation.mapping <- function(transformation.matrix, denominators)
{
    n.col = ncol(transformation.matrix)
    col.signatures = paste0(denominators, '-', apply(transformation.matrix, 2, paste0, collapse=','))
    unique.col.signatures = unique(col.signatures)
    n.signatures = length(unique.col.signatures)

    rv = list()
    rv$index.to.signature = sapply(col.signatures, function(signature){
        (1:n.signatures)[signature==unique.col.signatures]
    })

    rv$signature.to.index = lapply(1:n.signatures, function(i){
        (1:n.col)[rv$index.to.signature==i]
    })

    rv$collapsing.matrix = sapply(1:n.signatures, function(i){
        row = rep(0, n.col)
        row[rv$signature.to.index[[i]]] = 1
        row
    })

    rv$first.in.signature = sapply(rv$signature.to.index, function(s2i){s2i[1]})

    rv
}

sum.for.matrix.mapping <- function(values, mapping)
{
    sapply(mapping$signature.to.index, function(indices){
        sum(values[indices])
    })
}

collapse.for.matrix.mapping <- function(M, mapping)
{
    sapply(1:length(mapping$signature.to.index), function(j){
        sapply(1:length(mapping$signature.to.index), function(i){
            sum(M[mapping$signature.to.index[[i]],
                  mapping$signature.to.index[[j]]])
        })
    })
}

##-------------------------------------##
##-- HELPERS TO *EXECUTE* LIKELIHOOD --##
##-------------------------------------##


pull.simulations.rates <- function(jheem.results,
                                   data.type=c('new','prevalence','mortality'),
                                   years,
                                   denominator.dimensions,
                                   aggregate.denominator.males=T)
{
    if (data.type=='new')
        numerators = extract.new.diagnoses(jheem.results,
                                      years=years,
                                      keep.dimensions = c('year', 'age','race','sex','risk'),
                                      per.population = NA)
    else if (data.type=='prevalence')
        numerators = extract.prevalence(jheem.results, continuum='diagnosed',
                                   years=years,
                                   keep.dimensions = c('year', 'age','race','sex','risk'),
                                   per.population = NA)
    else if (data.type=='mortality')
        numerators = extract.overall.hiv.mortality(jheem.results,
                                              years=years,
                                              keep.dimensions = c('year', 'age','race','sex','risk'),
                                              per.population = NA)
    else
        stop("data.type must be one of 'new', 'prevalence', or 'mortality")

    denominators = extract.population.subset(jheem.results, years=years,
                                             keep.dimensions = denominator.dimensions)

    denominators = expand.population(denominators, target.dim.names = dimnames(numerators))

    if (aggregate.denominator.males && any(denominator.dimensions=='sex'))
    {
        males = denominators[,,,'msm',] + denominators[,,,'heterosexual_male',]
        denominators[,,,'msm',] = males
        denominators[,,,'heterosexual_male',] = males
    }

    numerators / denominators
}

pull.simulations.rates.per.total.population <- function(jheem.results,
                                   data.type=c('new','prevalence','mortality'),
                                   mapping=NULL,
                                   years)
{
    if (data.type=='new')
        numerators = extract.new.diagnoses(jheem.results,
                                           years=years,
                                           keep.dimensions = c('year', 'age','race','sex','risk'),
                                           per.population = NA)
    else if (data.type=='prevalence')
        numerators = extract.prevalence(jheem.results, continuum='diagnosed',
                                        years=years,
                                        keep.dimensions = c('year', 'age','race','sex','risk'),
                                        per.population = NA)
    else if (data.type=='mortality')
        numerators = extract.overall.hiv.mortality(jheem.results,
                                                   years=years,
                                                   keep.dimensions = c('year', 'age','race','sex','risk'),
                                                   per.population = NA)
    else
        stop("data.type must be one of 'new', 'prevalence', or 'mortality")

    denominators = extract.population.subset(jheem.results, years=years,
                                             keep.dimensions = 'year')

    sum.for.matrix.mapping(numerators / denominators, mapping)
}

#Once we have numerators and rates, run a multivariate normal that is summed up
likelihood.sub <- function(pre.transformation.rates,
                           transformation.matrix,
                           response.vector,
                           denominator.vector,
                           denominator.covar.mat=NULL,
                           numerator.covar.mat=NULL,
                           corr.mat=NULL,
                           transformation.mapping=NULL,
                           sd.inflation=1,
                           log=T,
                           sim=NULL, #these last two arguments are for debugging purposes
                           description=NULL)
{
    pre.transformation.rates = as.numeric(pre.transformation.rates)

    pre.transformation.rates = pmin(1,pmax(0, pre.transformation.rates))

    if (is.null(transformation.mapping))
    {
        p = pre.transformation.rates
        p.1mp = pre.transformation.rates * (1 - pre.transformation.rates)
    }
    else
    {
        p = sum.for.matrix.mapping(pre.transformation.rates, transformation.mapping)
        p.1mp = sum.for.matrix.mapping(pre.transformation.rates * (1 - pre.transformation.rates),
                                       transformation.mapping)
    }

    binomial.variance.component = denominator.vector * p.1mp

    if (is.null(corr.mat))
        binomial.variance.component = diag(binomial.variance.component)
    else
    {
        sds = sqrt(binomial.variance.component)
        binomial.variance.component = sds %*% t(sds) * corr.mat
    }

    mean.vector = transformation.matrix %*% (denominator.vector * p)

    if (is.null(denominator.covar.mat))
    {
        covar.mat = transformation.matrix %*%
            binomial.variance.component %*%
            t(transformation.matrix)
    }
    else
    {
        denominator.variance.component = denominator.covar.mat *
            pre.transformation.rates %*% t(pre.transformation.rates)

        covar.mat = transformation.matrix %*%
            (binomial.variance.component + denominator.variance.component) %*%
            t(transformation.matrix)
    }

    if (is(sd.inflation, 'function'))
        sd.inflation = sd.inflation(description)

    if (length(sd.inflation)==1)
        covar.mat = covar.mat * sd.inflation^2
    else
        covar.mat = covar.mat * (sd.inflation %*% t(sd.inflation))

    if (!is.null(numerator.covar.mat))
        covar.mat = covar.mat + numerator.covar.mat

    #for debugging
    if (1==2)
    {
        catg.minus.year = unique(gsub('[0-9][0-9][0-9][0-9], ', '', description))
        sapply(catg.minus.year, function(catg){
            regex = gsub('\\+', '\\\\+', catg)
            mask = grepl(regex, description)
            dmvnorm(x=response.vector[mask],
                    mean=mean.vector[mask],
                    sigma=covar.mat[mask,mask],
                    log=log)
        })
        sapply(catg.minus.year, function(catg){
            regex = gsub('\\+', '\\\\+', catg)
            mask = grepl(regex, description)
            mean(mean.vector[mask])
        })

        cbind(mean=sapply(catg.minus.year, function(catg){
            regex = gsub('\\+', '\\\\+', catg)
            mask = grepl(regex, description)
            mean(mean.vector[mask])
        }),
        density=sapply(catg.minus.year, function(catg){
            regex = gsub('\\+', '\\\\+', catg)
            mask = grepl(regex, description)
            dmvnorm(x=response.vector[mask],
                    mean=mean.vector[mask],
                    sigma=covar.mat[mask,mask],
                    log=log)
        }))
    }

    if (1==2)
    {
        mask = grepl('idu', description) & !grepl('msm', description)
        print(paste0('idu: ', dmvnorm(x=response.vector[mask],
                                      mean=mean.vector[mask],
                                      sigma=covar.mat[mask,mask],
                                      log=log)))
        mask = grepl('msm', description) & !grepl('idu', description)
        print(paste0('msm: ', dmvnorm(x=response.vector[mask],
                                      mean=mean.vector[mask],
                                      sigma=covar.mat[mask,mask],
                                      log=log)))
        mask = grepl('idu', description) & grepl('msm', description)
        print(paste0('msm+idu: ', dmvnorm(x=response.vector[mask],
                                      mean=mean.vector[mask],
                                      sigma=covar.mat[mask,mask],
                                      log=log)))
        mask = grepl('het', description)
        print(paste0('het: ', dmvnorm(x=response.vector[mask],
                                      mean=mean.vector[mask],
                                      sigma=covar.mat[mask,mask],
                                      log=log)))
    }
    if (1==2)
    {
        mask = grepl('idu', description) & !grepl('msm', description) & grepl('black', description)
        print(paste0('black idu: ', dmvnorm(x=response.vector[mask],
                                            mean=mean.vector[mask],
                                            sigma=covar.mat[mask,mask],
                                            log=log)))
        mask = grepl('idu', description) & !grepl('msm', description) & grepl('hispanic', description)
        print(paste0('hispanic idu: ', dmvnorm(x=response.vector[mask],
                                            mean=mean.vector[mask],
                                            sigma=covar.mat[mask,mask],
                                            log=log)))
        mask = grepl('idu', description) & !grepl('msm', description) & grepl('other', description)
        print(paste0('other idu: ', dmvnorm(x=response.vector[mask],
                                            mean=mean.vector[mask],
                                            sigma=covar.mat[mask,mask],
                                            log=log)))
    }
    dmvnorm(x=response.vector,
            mean=mean.vector,
            sigma=covar.mat,
            log=log)
}



##-----------------------------------------------##
##-- HIGH-LEVEL HELPERS TO *CREATE* LIKELIHOOD --##
##-----------------------------------------------##

create.likelihood.elements.for.data.type <- function(data.type=c('new','prevalence', 'mortality'),
                                                     years,
                                                     surv,
                                                     location,
                                                     by.total,
                                                     by.sex,
                                                     by.race,
                                                     by.age,
                                                     by.risk,
                                                     by.sex.age,
                                                     by.sex.race,
                                                     by.sex.risk,
                                                     by.race.risk,
                                                     numerator.sd=function(...){0},
                                                     bias.fn=function(...){0},
                                                     bias.sd=function(...){0},
                                                     numerator.year.to.year.chunk.correlation=0,
                                                     numerator.year.to.year.off.correlation=0,
                                                     numerator.chunk.years=numeric(),
                                                     ages=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
                                                     races=c('black','hispanic','other'),
                                                     sexes=c('heterosexual_male','msm','female'),
                                                     risks=c('never_IDU','active_IDU','IDU_in_remission')
)
{
    #-- Set Up Indexing --#
    dim.names = list(year=as.character(years), age=ages, race=races, sex=sexes, risk=risks)
    jheem.skeleton = array(0,
                           dim=sapply(dim.names, length),
                           dimnames = dim.names)

    #-- Set Up Full Response Vector and Transformation Matrix --#

    response.vector = numeric()
    transformation.matrix = NULL
    numerator.covar.mat = NULL
    descriptions = character()

    if (by.total)
    {
        cdc.arr = get.surveillance.data(surv, location, data.type=data.type, years=years)
        tmrv = make.transformation.matrix.and.response.vector(cdc.arr=cdc.arr, jheem.skeleton=jheem.skeleton)

        sds = numerator.sd(years=tmrv$year, num=tmrv$response.vector)
        if (length(sds)==1)
            sds = rep(sds, length(tmrv$response.vector))
        bias = bias.fn(years=tmrv$year, num=tmrv$response.vector)
        if (length(bias)==1)
            bias = rep(bias, length(tmrv$response.vector))
        bias.sds = bias.sd(years=tmrv$year, num=tmrv$response.vector)
        if (length(bias.sds)==1)
            bias.sds = rep(bias.sds, length(tmrv$response.vector))
        
        sds = sqrt(sds^2 + bias.sds^2)
        response.vector = c(response.vector, tmrv$response.vector - bias)
        transformation.matrix = rbind(transformation.matrix, tmrv$transformation.matrix)
        
        one.numerator.covar.mat = make.chunked.compound.symmetry.matrix(sds=sds,
                                                                        chunk.rho=numerator.year.to.year.chunk.correlation,
                                                                        non.chunk.rho=numerator.year.to.year.off.correlation,
                                                                        years=tmrv$year,
                                                                        chunk.years=numerator.chunk.years)
#        one.numerator.covar.mat = make.compound.symmetry.matrix(sds=sds,
#                                                                rho=numerator.year.to.year.correlation)

        numerator.covar.mat = join.independent.covariance.matrices(numerator.covar.mat,
                                                                   one.numerator.covar.mat)
        descriptions = c(descriptions, tmrv$descriptions)
    }

    if (by.sex)
    {
        cdc.arr = get.surveillance.data(surv, location, data.type=data.type, years=years, sex=T)
        tmrv = make.transformation.matrix.and.response.vector(cdc.arr=cdc.arr, jheem.skeleton=jheem.skeleton)
        
        sds = numerator.sd(years=tmrv$year, num=tmrv$response.vector)
        if (length(sds)==1)
            sds = rep(sds, length(tmrv$response.vector))
        bias = bias.fn(years=tmrv$year, num=tmrv$response.vector)
        if (length(bias)==1)
            bias = rep(bias, length(tmrv$response.vector))
        bias.sds = bias.sd(years=tmrv$year, num=tmrv$response.vector)
        if (length(bias.sds)==1)
            bias.sds = rep(bias.sds, length(tmrv$response.vector))
        
        sds = sqrt(sds^2 + bias.sds^2)
        response.vector = c(response.vector, tmrv$response.vector - bias)
        transformation.matrix = rbind(transformation.matrix, tmrv$transformation.matrix)
        
        one.numerator.covar.mat = make.chunked.compound.symmetry.matrix(sds=sds,
                                                                        chunk.rho=numerator.year.to.year.chunk.correlation,
                                                                        non.chunk.rho=numerator.year.to.year.off.correlation,
                                                                        years=tmrv$year,
                                                                        chunk.years=numerator.chunk.years)
#        one.numerator.covar.mat = make.compound.symmetry.matrix(sds=sds,
#                                                                rho=numerator.year.to.year.correlation)

        numerator.covar.mat = join.independent.covariance.matrices(numerator.covar.mat,
                                                                   one.numerator.covar.mat)
        descriptions = c(descriptions, tmrv$descriptions)
    }

    if (by.race)
    {
        cdc.arr = get.surveillance.data(surv, location, data.type=data.type, years=years, race=T)
        tmrv = make.transformation.matrix.and.response.vector(cdc.arr=cdc.arr, jheem.skeleton=jheem.skeleton)
        
        sds = numerator.sd(years=tmrv$year, num=tmrv$response.vector)
        if (length(sds)==1)
            sds = rep(sds, length(tmrv$response.vector))
        bias = bias.fn(years=tmrv$year, num=tmrv$response.vector)
        if (length(bias)==1)
            bias = rep(bias, length(tmrv$response.vector))
        bias.sds = bias.sd(years=tmrv$year, num=tmrv$response.vector)
        if (length(bias.sds)==1)
            bias.sds = rep(bias.sds, length(tmrv$response.vector))
        
        sds = sqrt(sds^2 + bias.sds^2)
        response.vector = c(response.vector, tmrv$response.vector - bias)
        transformation.matrix = rbind(transformation.matrix, tmrv$transformation.matrix)
        
        one.numerator.covar.mat = make.chunked.compound.symmetry.matrix(sds=sds,
                                                                        chunk.rho=numerator.year.to.year.chunk.correlation,
                                                                        non.chunk.rho=numerator.year.to.year.off.correlation,
                                                                        years=tmrv$year,
                                                                        chunk.years=numerator.chunk.years)
#        one.numerator.covar.mat = make.compound.symmetry.matrix(sds=sds,
#                                                                rho=numerator.year.to.year.correlation)

        numerator.covar.mat = join.independent.covariance.matrices(numerator.covar.mat,
                                                                   one.numerator.covar.mat)
        descriptions = c(descriptions, tmrv$descriptions)
    }

    if (by.age)
    {
        cdc.arr = get.surveillance.data(surv, location, data.type=data.type, years=years, age=T)
        tmrv = make.transformation.matrix.and.response.vector(cdc.arr=cdc.arr, jheem.skeleton=jheem.skeleton)
        
        sds = numerator.sd(years=tmrv$year, num=tmrv$response.vector)
        if (length(sds)==1)
            sds = rep(sds, length(tmrv$response.vector))
        bias = bias.fn(years=tmrv$year, num=tmrv$response.vector)
        if (length(bias)==1)
            bias = rep(bias, length(tmrv$response.vector))
        bias.sds = bias.sd(years=tmrv$year, num=tmrv$response.vector)
        if (length(bias.sds)==1)
            bias.sds = rep(bias.sds, length(tmrv$response.vector))
        
        sds = sqrt(sds^2 + bias.sds^2)
        response.vector = c(response.vector, tmrv$response.vector - bias)
        transformation.matrix = rbind(transformation.matrix, tmrv$transformation.matrix)
        
        one.numerator.covar.mat = make.chunked.compound.symmetry.matrix(sds=sds,
                                                                        chunk.rho=numerator.year.to.year.chunk.correlation,
                                                                        non.chunk.rho=numerator.year.to.year.off.correlation,
                                                                        years=tmrv$year,
                                                                        chunk.years=numerator.chunk.years)
#        one.numerator.covar.mat = make.compound.symmetry.matrix(sds=sds,
#                                                                rho=numerator.year.to.year.correlation)

        numerator.covar.mat = join.independent.covariance.matrices(numerator.covar.mat,
                                                                   one.numerator.covar.mat)
        descriptions = c(descriptions, tmrv$descriptions)
    }

    if (by.risk)
    {
        cdc.arr = get.surveillance.data(surv, location, data.type=data.type, years=years, risk=T)
        tmrv = make.transformation.matrix.and.response.vector(cdc.arr=cdc.arr, jheem.skeleton=jheem.skeleton)
        
        sds = numerator.sd(years=tmrv$year, num=tmrv$response.vector)
        if (length(sds)==1)
            sds = rep(sds, length(tmrv$response.vector))
        bias = bias.fn(years=tmrv$year, num=tmrv$response.vector)
        if (length(bias)==1)
            bias = rep(bias, length(tmrv$response.vector))
        bias.sds = bias.sd(years=tmrv$year, num=tmrv$response.vector)
        if (length(bias.sds)==1)
            bias.sds = rep(bias.sds, length(tmrv$response.vector))
        
        sds = sqrt(sds^2 + bias.sds^2)
        response.vector = c(response.vector, tmrv$response.vector - bias)
        transformation.matrix = rbind(transformation.matrix, tmrv$transformation.matrix)
        
        one.numerator.covar.mat = make.chunked.compound.symmetry.matrix(sds=sds,
                                                                        chunk.rho=numerator.year.to.year.chunk.correlation,
                                                                        non.chunk.rho=numerator.year.to.year.off.correlation,
                                                                        years=tmrv$year,
                                                                        chunk.years=numerator.chunk.years)
#        one.numerator.covar.mat = make.compound.symmetry.matrix(sds=sds,
#                                                                rho=numerator.year.to.year.correlation)

        numerator.covar.mat = join.independent.covariance.matrices(numerator.covar.mat,
                                                                   one.numerator.covar.mat)
        descriptions = c(descriptions, tmrv$descriptions)
    }

    if (by.sex.age)
    {
        cdc.arr = get.surveillance.data(surv, location, data.type=data.type, years=years, sex=T, age=T)
        tmrv = make.transformation.matrix.and.response.vector(cdc.arr=cdc.arr, jheem.skeleton=jheem.skeleton)
        
        sds = numerator.sd(years=tmrv$year, num=tmrv$response.vector)
        if (length(sds)==1)
            sds = rep(sds, length(tmrv$response.vector))
        bias = bias.fn(years=tmrv$year, num=tmrv$response.vector)
        if (length(bias)==1)
            bias = rep(bias, length(tmrv$response.vector))
        bias.sds = bias.sd(years=tmrv$year, num=tmrv$response.vector)
        if (length(bias.sds)==1)
            bias.sds = rep(bias.sds, length(tmrv$response.vector))
        
        sds = sqrt(sds^2 + bias.sds^2)
        response.vector = c(response.vector, tmrv$response.vector - bias)
        transformation.matrix = rbind(transformation.matrix, tmrv$transformation.matrix)
        
        one.numerator.covar.mat = make.chunked.compound.symmetry.matrix(sds=sds,
                                                                        chunk.rho=numerator.year.to.year.chunk.correlation,
                                                                        non.chunk.rho=numerator.year.to.year.off.correlation,
                                                                        years=tmrv$year,
                                                                        chunk.years=numerator.chunk.years)
#        one.numerator.covar.mat = make.compound.symmetry.matrix(sds=sds,
#                                                                rho=numerator.year.to.year.correlation)

        numerator.covar.mat = join.independent.covariance.matrices(numerator.covar.mat,
                                                                   one.numerator.covar.mat)
        descriptions = c(descriptions, tmrv$descriptions)
    }

    if (by.sex.race)
    {
        cdc.arr = get.surveillance.data(surv, location, data.type=data.type, years=years, sex=T, race=T)
        tmrv = make.transformation.matrix.and.response.vector(cdc.arr=cdc.arr, jheem.skeleton=jheem.skeleton)
        
        sds = numerator.sd(years=tmrv$year, num=tmrv$response.vector)
        if (length(sds)==1)
            sds = rep(sds, length(tmrv$response.vector))
        bias = bias.fn(years=tmrv$year, num=tmrv$response.vector)
        if (length(bias)==1)
            bias = rep(bias, length(tmrv$response.vector))
        bias.sds = bias.sd(years=tmrv$year, num=tmrv$response.vector)
        if (length(bias.sds)==1)
            bias.sds = rep(bias.sds, length(tmrv$response.vector))
        
        sds = sqrt(sds^2 + bias.sds^2)
        response.vector = c(response.vector, tmrv$response.vector - bias)
        transformation.matrix = rbind(transformation.matrix, tmrv$transformation.matrix)
        
        one.numerator.covar.mat = make.chunked.compound.symmetry.matrix(sds=sds,
                                                                        chunk.rho=numerator.year.to.year.chunk.correlation,
                                                                        non.chunk.rho=numerator.year.to.year.off.correlation,
                                                                        years=tmrv$year,
                                                                        chunk.years=numerator.chunk.years)
#        one.numerator.covar.mat = make.compound.symmetry.matrix(sds=sds,
#                                                                rho=numerator.year.to.year.correlation)

        numerator.covar.mat = join.independent.covariance.matrices(numerator.covar.mat,
                                                                   one.numerator.covar.mat)
        descriptions = c(descriptions, tmrv$descriptions)
    }

    if (by.sex.risk)
    {
        cdc.arr = get.surveillance.data(surv, location, data.type=data.type, years=years, sex=T, risk=T)
        tmrv = make.transformation.matrix.and.response.vector(cdc.arr=cdc.arr, jheem.skeleton=jheem.skeleton)
        
        sds = numerator.sd(years=tmrv$year, num=tmrv$response.vector)
        if (length(sds)==1)
            sds = rep(sds, length(tmrv$response.vector))
        bias = bias.fn(years=tmrv$year, num=tmrv$response.vector)
        if (length(bias)==1)
            bias = rep(bias, length(tmrv$response.vector))
        bias.sds = bias.sd(years=tmrv$year, num=tmrv$response.vector)
        if (length(bias.sds)==1)
            bias.sds = rep(bias.sds, length(tmrv$response.vector))
        
        sds = sqrt(sds^2 + bias.sds^2)
        response.vector = c(response.vector, tmrv$response.vector - bias)
        transformation.matrix = rbind(transformation.matrix, tmrv$transformation.matrix)
        
        one.numerator.covar.mat = make.chunked.compound.symmetry.matrix(sds=sds,
                                                                        chunk.rho=numerator.year.to.year.chunk.correlation,
                                                                        non.chunk.rho=numerator.year.to.year.off.correlation,
                                                                        years=tmrv$year,
                                                                        chunk.years=numerator.chunk.years)
#        one.numerator.covar.mat = make.compound.symmetry.matrix(sds=sds,
#                                                                rho=numerator.year.to.year.correlation)

        numerator.covar.mat = join.independent.covariance.matrices(numerator.covar.mat,
                                                                   one.numerator.covar.mat)
        descriptions = c(descriptions, tmrv$descriptions)
    }

    if (by.race.risk)
    {
        cdc.arr = get.surveillance.data(surv, location, data.type=data.type, years=years, race=T, risk=T)
        tmrv = make.transformation.matrix.and.response.vector(cdc.arr=cdc.arr, jheem.skeleton=jheem.skeleton)
        
        sds = numerator.sd(years=tmrv$year, num=tmrv$response.vector)
        if (length(sds)==1)
            sds = rep(sds, length(tmrv$response.vector))
        bias = bias.fn(years=tmrv$year, num=tmrv$response.vector)
        if (length(bias)==1)
            bias = rep(bias, length(tmrv$response.vector))
        bias.sds = bias.sd(years=tmrv$year, num=tmrv$response.vector)
        if (length(bias.sds)==1)
            bias.sds = rep(bias.sds, length(tmrv$response.vector))
        
        sds = sqrt(sds^2 + bias.sds^2)
        response.vector = c(response.vector, tmrv$response.vector - bias)
        transformation.matrix = rbind(transformation.matrix, tmrv$transformation.matrix)
        
        one.numerator.covar.mat = make.chunked.compound.symmetry.matrix(sds=sds,
                                                                        chunk.rho=numerator.year.to.year.chunk.correlation,
                                                                        non.chunk.rho=numerator.year.to.year.off.correlation,
                                                                        years=tmrv$year,
                                                                        chunk.years=numerator.chunk.years)
#        one.numerator.covar.mat = make.compound.symmetry.matrix(sds=sds,
#                                                                rho=numerator.year.to.year.correlation)

        numerator.covar.mat = join.independent.covariance.matrices(numerator.covar.mat,
                                                                   one.numerator.covar.mat)
        descriptions = c(descriptions, tmrv$descriptions)
    }

    missing.response = is.na(response.vector)
    response.vector = response.vector[!missing.response]
    transformation.matrix = transformation.matrix[!missing.response,]
    descriptions = descriptions[!missing.response]
    numerator.covar.mat = numerator.covar.mat[!missing.response,!missing.response]

    if (all(numerator.covar.mat==0))
        numerator.covar.mat = NULL

    list(transformation.matrix = transformation.matrix,
         response.vector = response.vector,
         numerator.covar.mat = numerator.covar.mat,
         descriptions = descriptions)
}

##---------------------------##
##-- PUBLIC-FACING HELPERS --##
##---------------------------##

get.data.cvs <- function(location=BALTIMORE.MSA, surv=msa.surveillance, years=NULL)
{
    rv = list()
    rv$all.new = 1/sqrt(get.surveillance.data(surv, location, years=years, data.type='new'))
    rv$all.prev = 1/sqrt(get.surveillance.data(surv, location, years=years, data.type='prevalence'))
    rv$all.mort = 1/sqrt(get.surveillance.data(surv, location, years=years, data.type='mortality'))
    rv
}

get.equalizing.cv.multipliers <- function(location, surv=msa.surveillance, years=NULL)
{
    cvs = get.data.cvs(location=location, surv=surv, years=years)
    mean.cvs = sapply(cvs, mean, na.rm=T)
    1/(mean.cvs/max(mean.cvs))
}


##----------------------------------------------##
##-- MID-LEVEL HELPERS TO *CREATE* LIKELIHOOD --##
##----------------------------------------------##

make.transformation.matrix.and.response.vector <- function(cdc.arr, jheem.skeleton,
                                                           ages=NULL, races=NULL,
                                                           sexes=NULL, risks=NULL)
{
    melted = melt(cdc.arr, as.is=T)

    if (!is.null(ages) && !is.null(melted$age))
        melted = melted[sapply(melted$age, function(age){any(age==ages)}),]
    if (!is.null(races) && !is.null(melted$race))
        melted = melted[sapply(melted$race, function(race){any(race==races)}),]
    if (!is.null(sexes) && !is.null(melted$sex))
        melted = melted[sapply(melted$sex, function(sex){any(sex==sexes)}),]
    if (!is.null(risks) && !is.null(melted$risk))
        melted = melted[sapply(melted$risk, function(risk){any(risk==risks)}),]

    transformation.matrix = NULL

    if (!is.null(melted$sex) && !is.null(melted$risk))
        melted = melted[melted$sex != 'female' | (melted$risk != 'msm' & melted$risk != 'msm_idu'),]

    for (row in 1:dim(melted)[1])
    {
        one.transformation = jheem.skeleton

        access(one.transformation,
               year=melted$year[row],
               age=melted$age[row],
               race=melted$race[row],
               sex=map.cdc.to.jheem.sex(melted$sex[row], melted$risk[row]),
               risk=map.cdc.to.jheem.risk(melted$risk[row])) = 1

        transformation.matrix = rbind(transformation.matrix, as.numeric(one.transformation))
    }

    if (dim(melted)[2]==1)
        descriptions = rep(all, dim(melted)[1])
    else if (dim(melted)[2]==2)
        descriptions = melted[,dimnames(melted)[[2]]!='value']
    else
        descriptions = apply(melted[,dimnames(melted)[[2]]!='value'], 1, function(row){
            paste0(row, collapse=', ')
        })
#print(paste0(dimnames(melted)[[2]], collapse=', '))

    list(transformation.matrix = transformation.matrix,
         response.vector = as.matrix(melted$value, ncol=1),
         descriptions=descriptions,
         year=melted$year)
}

create.simple.denominator.elements <- function(population, msm.cv = msm.cv, idu.cv = idu.cv)
{
    diagonal = array(0, dim=dim(population), dimnames=dimnames(population))

    if (msm.cv!=0)
        access(diagonal, sex='msm') = access(diagonal, sex='msm') +
            access(population, sex='msm') * msm.cv

    if (idu.cv!=0)
    {
        access(diagonal, risk='active_IDU') = access(diagonal, risk='active_IDU') +
            access(population, risk='active_IDU') * idu.cv

        access(diagonal, risk='IDU_in_remission') = access(diagonal, risk='IDU_in_remission') +
            access(population, risk='IDU_in_remission') * idu.cv
    }

    denominator.covar.mat = diag(as.numeric(diagonal))
    if (all(denominator.covar.mat==0))
        denominator.covar.mat = NULL

    list(denominator.vector = as.numeric(population),
         denominator.covar.mat = denominator.covar.mat)
}

create.simple.denominator.elements.from.msm.proportions <- function(pre.msm.population, msm.proportions, idu.cv = 0.25)
{
    diagonal = array(0, dim=dim(pre.msm.population), dimnames=dimnames(pre.msm.population))

    for (race in names(msm.proportions))
    {
        pre.msm.population[,,race,'msm',] = msm.proportions[race] *
            pre.msm.population[,,race,'msm',]

        pre.msm.population[,,race,'heterosexual_male',] = msm.proportions[race] *
            pre.msm.population[,,race,'heterosexual_male',]
    }

    if (idu.cv!=0)
    {
        diagonal[,,,,'active_IDU'] = diagonal[,,,,'active_IDU'] +
            pre.msm.population[,,,,'active_IDU'] * idu.cv

        diagonal[,,,,'IDU_in_remission'] = diagonal[,,,,'IDU_in_remission'] +
            pre.msm.population[,,,,'IDU_in_remission'] * idu.cv
    }

    denominator.covar.mat = diag(as.numeric(diagonal))
    if (all(denominator.covar.mat==0))
        denominator.covar.mat = NULL

    list(denominator.vector = as.numeric(population),
         denominator.covar.mat = denominator.covar.mat)
}


make.compound.symmetry.by.year.matrix <- function(population,
                                                  correlation)
{
    N = prod(dim(population))
    N.year = dim(population)['year']
    N.other = N/N.year

    rv = array(0, dim=c(N.year, N.other, N.year, N.other))
    for (i in 1:N.other)
        rv[,i,,i] = correlation

    dim(rv) = c(N,N)
    diag(rv) = 1

    rv
}

#Base population age x race x sex(M/F)
make.denominator.elements <- function(base.population,
                                      msm.proportions.by.race,
                                      active.idu.prevalence,
                                      idu.ever.prevalence,
                                      target.population.dim.names,
                                      msm.cv,
                                      idu.cv)
{
    #-- Check arguments --#
    if (any(active.idu.prevalence<0) || any(active.idu.prevalence>1))
        stop('All values of active.idu.prevalence must be between 0 and 1')

    if (any(idu.ever.prevalence<0) || any(idu.ever.prevalence>1))
        stop('All values of idu.ever.prevalence must be between 0 and 1')

    if (any(active.idu.prevalence > idu.ever.prevalence))
        stop('All values of active.idu.prevalence must be less than or equal to the corresponding values in idu.ever.prevalence')

    if (any(components$proportions.msm.of.male>1) || any(components$proportions.msm.of.male<0))
        stop("MSM proportions must be between 0 and 1")

    if (any(components$proportions.msm.of.male>0.1))
        warning("Some MSM proportions have been set to be greater than 10% - is this intended?")


    prior.idu.prevalence = idu.ever.prevalence - prior.idu.prevalence

    #-- Set Up Hydrated Arrays --#
    target.from.dim.names = target.to.dim.names = target.population.dim.names
    names(target.from.dim.names) = paste0(names(target.from.dim.names), '.from')
    names(target.to.dim.names) = paste0(names(target.to.dim.names), '.to')
    hydrated.matrix.dim.names = c(target.from.dim.names, target.to.dim.names)

    hydrated.vector = array(0, dim=sapply(target.population.dim.names, length), dimnames=target.population.dim.names)
    hydrated.matrix = array(0, dim=sapply(hydrated.matrix.dim.names, length), dimnames=hydrated.matrix.dim.names)

    for (age in target.population.dim.names[['age']])
    {
        for (race in target.population.dim.names[['race']])
        {
            #-- Female --#
            p.active = access(active.idu.prevalence, age=age, race=race, sex='female')
            p.prior = access(prior.idu.prevalence, age=age, race=race, sex='female')
            base = base.population[age, race, 'female']

            hydrated.vector[age,race,'female','never_IDU'] = base * (1 - p.active - p.prior)
            hydrated.vector[age,race,'female','active_IDU'] = base * p.active
            hydrated.vector[age,race,'female','IDU_in_remission'] = base * p.prior

            hydrated.matrix[age,race,'female','never_IDU',age,race,'female','never_IDU'] = (base * idu.cv)^2 * (p.active^2 + p.prior^2)
            hydrated.matrix[age,race,'female','active_IDU',age,race,'female','active_IDU'] = (base * idu.cv)^2 * p.active^2
            hydrated.matrix[age,race,'female','IDU_in_remission',age,race,'female','IDU_in_remission'] = (base * idu.cv)^2 * p.prior^2
            hydrated.matrix[age,race,'female','never_IDU',age,race,'female','active_IDU'] =
                hydrated.matrix[age,race,'female','active_IDU',age,race,'female','never_IDU'] = -(base * idu.cv)^2 * p.active^2
            hydrated.matrix[age,race,'female','never_IDU',age,race,'female','IDU_in_remission'] =
                hydrated.matrix[age,race,'female','IDU_in_remission',age,race,'female','never_IDU'] = -(base * idu.cv)^2 * p.prior^2

            #-- Male --#
            p.msm = msm.proportions.by.race[race]
            base = base.population[age, race, 'male']
            p.active.het = access(active.idu.prevalence, age=age, race=race, sex='heterosexual_male')
            p.prior.het = access(prior.idu.prevalence, age=age, race=race, sex='heterosexual_male')
            p.active.msm = access(active.idu.prevalence, age=age, race=race, sex='msm')
            p.prior.msm = access(prior.idu.prevalence, age=age, race=race, sex='msm')

            hydrated.vector[age,race,'heterosexual_male','never_IDU'] = base * (1 - p.msm) * (1 - p.active.het - p.prior.het)
            hydrated.vector[age,race,'heterosexual_male','active_IDU'] = base * (1 - p.msm) * p.active.het
            hydrated.vector[age,race,'heterosexual_male','IDU_in_remission'] = base * (1 - p.msm) * p.prior.het

            hydrated.vector[age,race,'msm','never_IDU'] = base * p.msm * (1 - p.active.msm - p.prior.msm)
            hydrated.vector[age,race,'msm','active_IDU'] = base * p.msm * p.active.msm
            hydrated.vector[age,race,'msm','IDU_in_remission'] = base * p.msm * p.prior.msm

            #- The diagonals (variances) -#
            hydrated.matrix[age,race,'heterosexual_male','never_IDU',age,race,'female','never_IDU'] =
                base^2 * ( (p.msm * msm.cv)^2 + (1-p.msm)^2 * idu.cv^2 * (p.active.het^2 + p.prior.het^2) )
            hydrated.matrix[age,race,'heterosexual_male','active_IDU',age,race,'female','active_IDU'] =
                base^2 * ( (p.msm * msm.cv)^2 + (1-p.msm)^2 * idu.cv^2 * p.active.het^2 )
            hydrated.matrix[age,race,'heterosexual_male','IDU_in_remission',age,race,'female','IDU_in_remission'] =
                base^2 * ( (p.msm * msm.cv)^2 + (1-p.msm)^2 * idu.cv^2 * p.prior.het^2 )

            hydrated.matrix[age,race,'msm','never_IDU',age,race,'female','never_IDU'] =
                base^2 * ( (p.msm * msm.cv)^2 + p.msm^2 * idu.cv^2 * (p.active.msm^2 + p.prior.msm^2) )
            hydrated.matrix[age,race,'msm','active_IDU',age,race,'female','active_IDU'] =
                base^2 * ( (p.msm * msm.cv)^2 + p.msm^2 * idu.cv^2 * p.active.msm^2 )
            hydrated.matrix[age,race,'msm','IDU_in_remission',age,race,'female','IDU_in_remission'] =
                base^2 * ( (p.msm * msm.cv)^2 + p.msm^2 * idu.cv^2 * p.prior.msm^2 )

            #- Covariances by MSM -#

            #het with msm
                #never with x
            hydrated.matrix[age,race,'heterosexual_male','never_IDU',age,race,'msm','never_IDU'] =
                hydrated.matrix[age,race,'heterosexual_male','never_IDU',age,race,'msm','active_IDU'] =
                hydrated.matrix[age,race,'heterosexual_male','never_IDU',age,race,'msm','IDU_in_remission'] =
                #active with x
                hydrated.matrix[age,race,'heterosexual_male','active_IDU',age,race,'msm','never_IDU'] =
                hydrated.matrix[age,race,'heterosexual_male','active_IDU',age,race,'msm','active_IDU'] =
                hydrated.matrix[age,race,'heterosexual_male','active_IDU',age,race,'msm','IDU_in_remission'] =
                #remission with x
                hydrated.matrix[age,race,'msm','IDU_in_remission',age,race,'msm','never_IDU'] =
                hydrated.matrix[age,race,'msm','IDU_in_remission',age,race,'msm','active_IDU'] =
                hydrated.matrix[age,race,'msm','IDU_in_remission',age,race,'msm','IDU_in_remission'] =
            #msm with het
                hydrated.matrix[age,race,'msm','never_IDU',age,race,'heterosexual_male','never_IDU'] =
                hydrated.matrix[age,race,'msm','never_IDU',age,race,'heterosexual_male','active_IDU'] =
                hydrated.matrix[age,race,'msm','never_IDU',age,race,'heterosexual_male','IDU_in_remission'] =
                #active with x
                hydrated.matrix[age,race,'msm','active_IDU',age,race,'heterosexual_male','never_IDU'] =
                hydrated.matrix[age,race,'msm','active_IDU',age,race,'heterosexual_male','active_IDU'] =
                hydrated.matrix[age,race,'msm','active_IDU',age,race,'heterosexual_male','IDU_in_remission'] =
                #remission with x
                hydrated.matrix[age,race,'msm','IDU_in_remission',age,race,'heterosexual_male','never_IDU'] =
                hydrated.matrix[age,race,'msm','IDU_in_remission',age,race,'heterosexual_male','active_IDU'] =
                hydrated.matrix[age,race,'msm','IDU_in_remission',age,race,'heterosexual_male','IDU_in_remission'] =
            #the actual value
                -(base * p.msm * msm.cv)^2

            #- Covariances by IDU -#

            hydrated.matrix[age,race,'heterosexual_male','never_IDU',age,race,'heterosexual_male','active_IDU'] =
                hydrated.matrix[age,race,'heterosexual_male','active_IDU',age,race,'heterosexual_male','never_IDU'] =
                (base * p.msm * msm.cv)^2 - (base * (1-p.msm) * idu.cv * p.active.het)^2

            hydrated.matrix[age,race,'heterosexual_male','never_IDU',age,race,'heterosexual_male','IDU_in_remission'] =
                hydrated.matrix[age,race,'heterosexual_male','IDU_in_remission',age,race,'heterosexual_male','never_IDU'] =
                (base * p.msm * msm.cv)^2 - (base * (1-p.msm) * idu.cv * p.prior.het)^2

            hydrated.matrix[age,race,'heterosexual_male','active_IDU',age,race,'heterosexual_male','IDU_in_remission'] =
                hydrated.matrix[age,race,'heterosexual_male','IDU_in_remission',age,race,'heterosexual_male','active_IDU'] =
                (base * p.msm * msm.cv)^2


            hydrated.matrix[age,race,'msm','never_IDU',age,race,'msm','active_IDU'] =
                hydrated.matrix[age,race,'msm','active_IDU',age,race,'msm','never_IDU'] =
                (base * p.msm * msm.cv)^2 - (base * p.msm * idu.cv * p.active.msm)^2

            hydrated.matrix[age,race,'msm','never_IDU',age,race,'msm','IDU_in_remission'] =
                hydrated.matrix[age,race,'msm','IDU_in_remission',age,race,'msm','never_IDU'] =
                (base * p.msm * msm.cv)^2 - (base * p.msm * idu.cv * p.prior.msm)^2

            hydrated.matrix[age,race,'msm','active_IDU',age,race,'msm','IDU_in_remission'] =
                hydrated.matrix[age,race,'msm','IDU_in_remission',age,race,'msm','active_IDU'] =
                (base * p.msm * msm.cv)^2

        }
    }
}

get.sd.inflation <- function(match.to.response = likelihood.elements$response.vector,
                             match.to.descriptions = likelihood.elements$descriptions,
                             response.2 = sd.inflation.elements$response.vector,
                             descriptions.2 = sd.inflation.elements$descriptions)
{
    stop('we are not using this')
    match.to.year = as.numeric(substr(match.to.descriptions, 1,4))
    year.2 = as.numeric(substr(descriptions.2, 1,4))

    match.to.catg = substr(match.to.descriptions, 7,1000)
    catg.2 = substr(descriptions.2, 7,1000)

    match.to.cv = sqrt(match.to.response)
    cv.2 = sqrt(response.2)

    exact.match.indices = sapply(match.to.descriptions, function(match.to){
        (1:length(descriptions.2))[descriptions.2==match.to][1]
    })
    sd.inflation = cv.2[exact.match.indices] / match.to.cv

    no.exact.match = is.na(exact.match.indices)
    match.catg = lapply(match.to.catg[no.exact.match], function(match.to){

    })

    inexact.sd.inflation = sapply((1:length(match.to.descriptions))[no.exact.match], function(i){
        match.catg.indices = (1:length(catg.2))[catg.2==match.to.catg[i]]
        match.catg.years = year.2[match.catg.indices]
        year = match.to.year[i]

        min.year.dist = min(abs(year-match.catg.years))
        min.year.mask = abs(year-match.catg.years)==min.year.dist

        matched.cvs = cv.2[match.catg.indices][]
    })
    browser()
}

map.cdc.to.jheem.sex <- function(sex, risk)
{
    if (is.null(risk))
    {
        if (is.null(sex))
            NULL
        else
        {
            if (sex=='male')
                c('msm','heterosexual_male')
            else if (sex=='female')
                c('female')
        }
    }
    else
    {
        if (is.null(sex))
        {
            if (risk=='msm' || risk=='msm_idu')
                'msm'
            else if (risk=='idu' || risk=='heterosexual')
                c('heterosexual_male', 'female')
        }
        else
        {
            if (risk=='msm' || risk=='msm_idu')
                'msm'
            else if (sex=='male')
                'heterosexual_male'
            else if (sex=='female')
                'female'
        }
    }
}

map.cdc.to.jheem.risk <- function(risk)
{
    if (is.null(risk))
        NULL
    else
    {
        if (risk=='msm' || risk=='heterosexual')
            'never_IDU'
        else if (risk=='idu' || risk=='msm_idu')
            c('active_IDU', 'IDU_in_remission')
        else
            stop("Risk must be 'msm', 'idu', 'msm_idu', or 'heterosexual'")
    }
}



##----------------------------------------------##
##-- LOW-LEVEL HELPERS TO *CREATE* LIKELIHOOD --##
##----------------------------------------------##

make.compound.symmetry.matrix <- function(sds, rho)
{
    rho.mat = matrix(rho, nrow=length(sds), ncol=length(sds))
    diag(rho.mat) = 1
    sds %*% t(sds) * rho.mat
}

make.chunked.compound.symmetry.matrix <- function(sds,
                                                  chunk.rho,
                                                  non.chunk.rho,
                                                  years,
                                                  chunk.years)
{
    rho.mat = matrix(non.chunk.rho, nrow=length(years), ncol=length(years))

    for (chunk in chunk.years)
    {
        mask = sapply(years, function(year){any(year==chunk)})
        rho.mat[mask,mask] = chunk.rho
    }

    diag(rho.mat) = 1
    sds %*% t(sds) * rho.mat
}

join.independent.covariance.matrices <- function(m1, m2)
{
    if (is.null(m1))
        return (m2)
    if (is.null(m2))
        return (m1)

    n1 = dim(m1)[1]
    n2 = dim(m2)[1]
    rv = matrix(0, nrow=n1+n2, ncol=n1+n2)

    rv[1:n1,1:n1] = m1
    rv[n1+1:n2,n1+1:n2] = m2

    rv
}
