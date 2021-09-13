
##-- EXTRACT RESULTS (with option for CDC categorization) --##

CDC.SEXES = c('male','female')
CDC.RISKS = c('msm', 'idu', 'msm_idu', 'heterosexual')

##-----------------------##
##-- PROJECT FUNCTIONS --##
##-----------------------##

#an alias for backward compatibility
get.sim.absolute.incidence <- function(sim,
                                       keep.dimensions = 'year',
                                       years=sim$years,
                                       ages=NULL,
                                       races=NULL,
                                       subpopulations=NULL,
                                       sexes=NULL,
                                       risks=NULL,
                                       census.totals = if (exists('ALL.DATA.MANAGERS')) ALL.DATA.MANAGERS$census.totals else CENSUS.TOTALS,
                                       use.cdc.categorizations=T)
{
    project.absolute.incidence(sim=sim,
                               keep.dimensions = keep.dimensions,
                               years=years,
                               ages=ages,
                               races=races,
                               subpopulations=subpopulations,
                               sexes=sexes,
                               risks=risks,
                               census.totals = census.totals,
                               use.cdc.categorizations=use.cdc.categorizations)
}

project.absolute.incidence <- function(sim,
                                       keep.dimensions = 'year',
                                       years=sim$years,
                                       ages=NULL,
                                       races=NULL,
                                       subpopulations=NULL,
                                       sexes=NULL,
                                       risks=NULL,
                                       census.totals = if (exists('ALL.DATA.MANAGERS')) ALL.DATA.MANAGERS$census.totals else CENSUS.TOTALS,
                                       use.cdc.categorizations=T)
{
    
    numerators = do.extract.incidence(sim,
                                      years=years, 
                                      keep.dimensions=keep.dimensions,
                                      per.population=NA,
                                      ages=ages,
                                      races=races,
                                      subpopulations=subpopulations,
                                      sexes=sexes,
                                      risks=risks,
                                      non.hiv.subsets = NULL,
                                      continuum=NULL,
                                      cd4=NULL,
                                      hiv.subsets=NULL,
                                      use.cdc.categorizations=use.cdc.categorizations)
    
    do.project.absolute(sim=sim,
                        numerators=numerators,
                        years=years,
                        census.totals = census.totals)


}

project.absolute.new.diagnoses <- function(sim,
                                           keep.dimensions = 'year',
                                           years=sim$years,
                                           ages=NULL,
                                           races=NULL,
                                           subpopulations=NULL,
                                           sexes=NULL,
                                           risks=NULL,
                                           continuum.from=NULL,
                                           cd4=NULL,
                                           hiv.subsets = NULL,
                                           census.totals = if (exists('ALL.DATA.MANAGERS')) ALL.DATA.MANAGERS$census.totals else CENSUS.TOTALS,
                                           use.cdc.categorizations=T)
{
    numerators = do.extract.new.diagnoses(sim,
                                          per.population=NA,
                                          years=years,
                                          ages=ages,
                                          races=races,
                                          subpopulations=subpopulations,
                                          sexes=sexes,
                                          risks=risks,
                                          continuum.from=continuum.from,
                                          cd4s=cd4,
                                          hiv.subsets=hiv.subsets,
                                          keep.dimensions=keep.dimensions,
                                          include.hiv.positive.in.denominator=T,
                                          use.cdc.categorizations=use.cdc.categorizations)
    
    do.project.absolute(sim=sim,
                        numerators=numerators,
                        years=years,
                        census.totals = census.totals)
}

project.absolute.prevalence <- function(sim,
                                        keep.dimensions = 'year',
                                        years=sim$years,
                                        ages=NULL,
                                        races=NULL,
                                        subpopulations=NULL,
                                        sexes=NULL,
                                        risks=NULL,
                                        continuum=NULL,
                                        cd4=NULL,
                                        hiv.subsets = NULL,
                                        census.totals = if (exists('ALL.DATA.MANAGERS')) ALL.DATA.MANAGERS$census.totals else CENSUS.TOTALS,
                                        use.cdc.categorizations=T)
{   
    numerators = do.extract.prevalence(sim,
                                       per.population=NA,
                                       years=years,
                                       ages=ages,
                                       races=races,
                                       subpopulations=subpopulations,
                                       sexes=sexes,
                                       risks=risks,
                                       continuum=continuum,
                                       cd4s=cd4,
                                       hiv.subsets=hiv.subsets,
                                       keep.dimensions=keep.dimensions,
                                       use.cdc.categorizations=use.cdc.categorizations)
    
    do.project.absolute(sim=sim,
                        numerators=numerators,
                        years=years,
                        census.totals = census.totals)
}

project.absolute.prevalence.aware <- function(sim,
                                              keep.dimensions = 'year',
                                              years=sim$years,
                                              ages=NULL,
                                              races=NULL,
                                              subpopulations=NULL,
                                              sexes=NULL,
                                              risks=NULL,
                                              cd4=NULL,
                                              hiv.subsets = NULL,
                                              census.totals = if (exists('ALL.DATA.MANAGERS')) ALL.DATA.MANAGERS$census.totals else CENSUS.TOTALS,
                                              use.cdc.categorizations=T)
{   
    project.absolute.prevalence(sim,
                                keep.dimensions = keep.dimensions,
                                years=years,
                                ages=ages,
                                races=races,
                                subpopulations=subpopulations,
                                sexes=sexes,
                                risks=risks,
                                continuum=sim$diagnosed.continuum.states,
                                cd4=cd4,
                                hiv.subsets = hiv.subsets,
                                census.totals = census.totals,
                                use.cdc.categorizations=T)
}

project.population.subset <- function(sim,
                                      years=sim$years,
                                      keep.dimensions='year',
                                      ages=NULL,
                                      races=NULL,
                                      subpopulations=NULL,
                                      sexes=NULL,
                                      risks=NULL,
                                      non.hiv.subsets=NULL,
                                      continuum=NULL,
                                      cd4s=NULL,
                                      hiv.subsets=NULL,
                                      include.hiv.positive=T,
                                      include.hiv.negative=T,
                                      census.totals = if (exists('ALL.DATA.MANAGERS')) ALL.DATA.MANAGERS$census.totals else CENSUS.TOTALS,
                                      use.cdc.categorizations=F)
{
    numerators = do.extract.population.subset(sim,
                                              per.population=NA,
                                             years=years,
                                             ages=ages,
                                             races=races,
                                             subpopulations=subpopulations,
                                             sexes=sexes,
                                             risks=risks,
                                             non.hiv.subsets=non.hiv.subsets,
                                             continuum=continuum,
                                             cd4s=cd4s,
                                             hiv.subsets=hiv.subsets,
                                             include.hiv.positive=include.hiv.positive,
                                             include.hiv.negative=include.hiv.negative,
                                             keep.dimensions=keep.dimensions,
                                             use.cdc.categorizations=use.cdc.categorizations)
    
    do.project.absolute(sim=sim,
                        numerators=numerators,
                        years=years,
                        census.totals = census.totals)
}

project.absolute.n.suppressed <- function(sim,
                                          keep.dimensions = 'year',
                                          years=sim$years,
                                          ages=NULL,
                                          races=NULL,
                                          subpopulations=NULL,
                                          sexes=NULL,
                                          risks=NULL,
                                          continuum=NULL,
                                          cd4=NULL,
                                          hiv.subsets = NULL,
                                          census.totals = if (exists('ALL.DATA.MANAGERS')) ALL.DATA.MANAGERS$census.totals else CENSUS.TOTALS,
                                          use.cdc.categorizations=T)
{
    numerators = extract.suppression(sim,
                                     per.population = NA,
                                     years=years,
                                     keep.dimensions=keep.dimensions,
                                     ages=ages,
                                     races=races,
                                     subpopulations=subpopulations,
                                     sexes=sexes,
                                     risks=risks,
                                     continuum=continuum,
                                     cd4=cd4,
                                     hiv.subsets=hiv.subsets,
                                     use.cdc.categorizations=use.cdc.categorizations)
    
    
    do.project.absolute(sim=sim,
                        numerators=numerators,
                        years=years,
                        census.totals = census.totals)
}

project.absolute.overall.hiv.mortality <- function(sim,
                                                   keep.dimensions = 'year',
                                                   years=sim$years,
                                                   ages=NULL,
                                                   races=NULL,
                                                   subpopulations=NULL,
                                                   sexes=NULL,
                                                   risks=NULL,
                                                   continuum=NULL,
                                                   cd4=NULL,
                                                   hiv.subsets = NULL,
                                                   census.totals = if (exists('ALL.DATA.MANAGERS')) ALL.DATA.MANAGERS$census.totals else CENSUS.TOTALS,
                                                   use.cdc.categorizations=T)
{
    numerators = do.extract.overall.hiv.mortality(sim,
                                                  per.population = NA,
                                                  years=years,
                                                  ages=ages,
                                                  races=races,
                                                  subpopulations=subpopulations,
                                                  sexes=sexes,
                                                  risks=risks,
                                                  continuum=continuum,
                                                  cd4s=cd4,
                                                  hiv.subsets=hiv.subsets,
                                                  keep.dimensions=keep.dimensions,
                                                  use.cdc.categorizations=use.cdc.categorizations)
    
    
    do.project.absolute(sim=sim,
                        numerators=numerators,
                        years=years,
                        census.totals = census.totals)
}

do.project.absolute <- function(sim,
                                numerators,
                                years,
                                census.totals)
{
    if (length(intersect(sim$years, census.totals$years))==0)
        stop(paste0("Cannot project absolute trends unless the simulation contains at least one year in the census totals (",
                    min(census.totals$years), "-", max(census.totals$years), ')'))
    
    total.population = get.total.population(sim=sim, years=years, census.totals = census.totals)
    denominators = do.extract.population.subset(sim, years=years, 
                                                keep.dimensions = 'year', 
                                                use.cdc.categorizations = F)
    
    rv = as.numeric(numerators) / as.numeric(denominators) * total.population
    
    if (is.null(dim(numerators)))
        names(rv) = names(numerators)
    else
    {
        dim(rv) = dim(numerators)
        dimnames(rv) = dimnames(numerators)
    }
    
    rv
}

get.total.population <- function(sim, 
                                 years=sim$years,
                                 census.totals = if (exists('ALL.DATA.MANAGERS')) ALL.DATA.MANAGERS$census.totals else CENSUS.TOTALS)
{
    last.census.year = max(census.totals$years)
    years.after.census = years[years>last.census.year]
    years.with.or.before.census = setdiff(years, years.after.census)
    
    if (length(years.with.or.before.census)==0)
        rv.with.or.before.census = numeric()
    else
        rv.with.or.before.census = get.census.totals(census.totals, 
                                                     location=attr(sim, 'location'), 
                                                     years = years.with.or.before.census,
                                                     interpolate.missing.years = T,
                                                     flatten.single.dim.array = T)
    
    if (length(years.after.census)==0)
        rv.after.census = numeric()
    else
    {
        sim.pop.after.census = extract.population.subset(sim, keep.dimensions = 'year', years=years.after.census)
        sim.pop.last.with.census = extract.population.subset(sim, keep.dimensions = 'year', years=last.census.year)
        cen.pop.last.with.census = get.census.totals(census.totals, 
                                                     location=attr(sim, 'location'), 
                                                     years = last.census.year,
                                                     interpolate.missing.years = F,
                                                     flatten.single.dim.array = T)
        
        rv.after.census = sim.pop.after.census / sim.pop.last.with.census * cen.pop.last.with.census
    }
    
    rv = c(rv.with.or.before.census, rv.after.census)
    names(rv) = as.character(years)
    rv
}
    

##-----------------------##
##-- EXTRACT FUNCTIONS --##
##-----------------------##
    
do.extract.population.subset <- function(results,
                                         years=NULL,
                                         ages=NULL,
                                         races=NULL,
                                         subpopulations=NULL,
                                         sexes=NULL,
                                         risks=NULL,
                                         non.hiv.subsets=NULL,
                                         continuum=NULL,
                                         cd4s=NULL,
                                         hiv.subsets=NULL,
                                         include.hiv.positive=T,
                                         include.hiv.negative=T,
                                         keep.dimensions=NULL,
                                         denominator.dimensions='year',
                                         per.population=NA,
                                         transformation.fn=NULL,
                                         use.cdc.categorizations=F)
{
    if (setequal(risks, CDC.RISKS))
        risks = NULL
    if (setequal(sexes, CDC.SEXES))
        sexes = NULL
    
    if (use.cdc.categorizations &&
        (any(keep.dimensions=='sex') ||
         any(keep.dimensions=='risk') ||
         !is.null(risks) ||
         !is.null(sexes)))
    {
        numerators = extract.population.subset(results=results,
                                               years=years,
                                               ages=ages,
                                               races=races,
                                               subpopulations=subpopulations,
                                               sexes=NULL,
                                               risks=NULL,
                                               non.hiv.subsets=non.hiv.subsets,
                                               continuum=continuum,
                                               cd4s=cd4s,
                                               hiv.subsets=hiv.subsets,
                                               include.hiv.positive=include.hiv.positive,
                                               include.hiv.negative=include.hiv.negative,
                                               keep.dimensions=union(keep.dimensions, c('sex','risk')),
                                               per.population=NA,)
        numerators = sum.arr.to.cdc(numerators,
                                    keep.dimensions=keep.dimensions,
                                    sexes=sexes,
                                    risks=risks,
                                    female.msm.value = 0)
        
        if (is.na(per.population))
            numerators
        else
        {
            denominators = do.extract.population.subset(results=results,
                                                        years=years,
                                                        ages=ages,
                                                        races=races,
                                                        subpopulations=subpopulations,
                                                        sexes=NULL,
                                                        risks=NULL,
                                                        non.hiv.subsets=non.hiv.subsets,
                                                        continuum=continuum,
                                                        cd4s=cd4s,
                                                        hiv.subsets=hiv.subsets,
                                                        include.hiv.positive=include.hiv.positive,
                                                        include.hiv.negative=include.hiv.negative,
                                                        keep.dimensions=denominator.dimensions,
                                                        use.cdc.categorizations = use.cdc.categorizations,
                                                        per.population=NA)
            
            denominators = expand.population(denominators,
                                             target.dim.names = dimnames(numerators))
            
            numerators / denominators * per.population
        }
    }
    else
        extract.population.subset(results=results,
                                  years=years,
                                  ages=ages,
                                  races=races,
                                  subpopulations=subpopulations,
                                  sexes=sexes,
                                  risks=risks,
                                  non.hiv.subsets=non.hiv.subsets,
                                  continuum=continuum,
                                  cd4s=cd4s,
                                  hiv.subsets=hiv.subsets,
                                  include.hiv.positive=include.hiv.positive,
                                  include.hiv.negative=include.hiv.negative,
                                  keep.dimensions=keep.dimensions,
                                  denominator.dimensions=denominator.dimensions,
                                  per.population=per.population)
}

do.extract.diagnosed.hiv <- function(results,
                                     years=NULL,
                                     ages=NULL,
                                     races=NULL,
                                     subpopulations=NULL,
                                     sexes=NULL,
                                     risks=NULL,
                                     cd4s=NULL,
                                     hiv.subsets=NULL,
                                     keep.dimensions='year',
                                     per.population=1,
                                     use.cdc.categorizations=F
)
{
    if (setequal(risks, CDC.RISKS))
        risks = NULL
    if (setequal(sexes, CDC.SEXES))
        sexes = NULL
    
    if (use.cdc.categorizations &&
        (any(keep.dimensions=='sex') ||
         any(keep.dimensions=='risk') ||
         !is.null(risks) ||
         !is.null(sexes)))
    {
        numerators = extract.diagnosed.hiv(results=results,
                                           years=years,
                                           ages=ages,
                                           races=races,
                                           subpopulations=subpopulations,
                                           sexes=NULL,
                                           risks=NULL,
                                           hiv.subsets=hiv.subsets,
                                           keep.dimensions=union(keep.dimensions, c('sex','risk')),
                                           per.population=NA)
        numerators = sum.arr.to.cdc(numerators,
                                    keep.dimensions=keep.dimensions,
                                    sexes=sexes,
                                    risks=risks)
        
        if (is.na(per.population))
            numerators
        else
        {
            denominators = do.extract.population.subset(results=results,
                                                        continuum=NULL,
                                                        include.hiv.negative=F,
                                                        keep.dimensions=keep.dimensions,
                                                        years=years,
                                                        ages=ages,
                                                        races=races,
                                                        subpopulations=subpopulations,
                                                        sexes=sexes,
                                                        risks=risks,
                                                        cd4s=cd4s,
                                                        hiv.subsets=hiv.subsets,
                                                        per.population=NA,
                                                        use.cdc.categorizations=use.cdc.categorizations)
            
            numerators / denominators * per.population
        }
    }
    else
        extract.diagnosed.hiv(results=results,
                              years=years,
                              ages=ages,
                              races=races,
                              subpopulations=subpopulations,
                              sexes=sexes,
                              risks=risks,
                              hiv.subsets=hiv.subsets,
                              keep.dimensions=keep.dimensions,
                              per.population=per.population)
}

do.extract.incidence <- function(results,
                                 years=NULL,
                                 ages=NULL,
                                 races=NULL,
                                 subpopulations=NULL,
                                 sexes=NULL,
                                 risks=NULL,
                                 non.hiv.subsets=NULL,
                                 continuum=NULL,
                                 cd4s=NULL,
                                 hiv.subsets=NULL,
                                 keep.dimensions=NULL,
                                 include.hiv.positive.in.denominator=T,
                                 per.population=100000,
                                 use.cdc.categorizations=F
)
{
    if (setequal(risks, CDC.RISKS))
        risks = NULL
    if (setequal(sexes, CDC.SEXES))
        sexes = NULL
    
    if (use.cdc.categorizations &&
        (any(keep.dimensions=='sex') ||
         any(keep.dimensions=='risk') ||
         !is.null(risks) ||
         !is.null(sexes)))
    {
        if (!is.na(per.population))
            stop("collapsing to CDC not implemented for non-NA per.population")
        
        non.cdc = extract.incidence(results=results,
                                    years=years,
                                    ages=ages,
                                    races=races,
                                    subpopulations=subpopulations,
                                    sexes=NULL,
                                    risks=NULL,
                                    non.hiv.subsets=non.hiv.subsets,
                                    continuum=continuum,
                                    cd4s=cd4s,
                                    hiv.subsets=hiv.subsets,
                                    include.hiv.positive.in.denominator=T,
                                    keep.dimensions=union(keep.dimensions, c('sex','risk')),
                                    per.population=per.population)
        sum.arr.to.cdc(non.cdc,
                       keep.dimensions=keep.dimensions,
                       sexes=sexes,
                       risks=risks)
    }
    else
        extract.incidence(results=results,
                          years=years,
                          ages=ages,
                          races=races,
                          subpopulations=subpopulations,
                          sexes=sexes,
                          risks=risks,
                          non.hiv.subsets=non.hiv.subsets,
                          continuum=continuum,
                          cd4s=cd4s,
                          hiv.subsets=hiv.subsets,
                          include.hiv.positive.in.denominator=T,
                          keep.dimensions=keep.dimensions,
                          per.population=per.population)
}

do.extract.prevalence <- function(results,
                                  years=NULL,
                                  ages=NULL,
                                  races=NULL,
                                  subpopulations=NULL,
                                  sexes=NULL,
                                  risks=NULL,
                                  continuum=NULL,
                                  cd4s=NULL,
                                  hiv.subsets=NULL,
                                  keep.dimensions=NULL,
                                  per.population=100,
                                  transformation.fn=NULL,
                                  use.cdc.categorizations=F
)
{
    if (setequal(risks, CDC.RISKS))
        risks = NULL
    if (setequal(sexes, CDC.SEXES))
        sexes = NULL
    
    if (use.cdc.categorizations &&
        (any(keep.dimensions=='sex') ||
         any(keep.dimensions=='risk') ||
         !is.null(risks) ||
         !is.null(sexes)))
    {
        if (!is.na(per.population))
            stop("collapsing to CDC not implemented for non-NA per.population")
        
        non.cdc = extract.prevalence(results=results,
                                     years=years,
                                     ages=ages,
                                     races=races,
                                     subpopulations=subpopulations,
                                     sexes=NULL,
                                     risks=NULL,
                                     continuum=continuum,
                                     cd4s=cd4s,
                                     hiv.subsets=hiv.subsets,
                                     keep.dimensions=union(keep.dimensions, c('sex','risk')),
                                     per.population=per.population)
        sum.arr.to.cdc(non.cdc,
                       keep.dimensions=keep.dimensions,
                       sexes=sexes,
                       risks=risks)
    }
    else
        extract.prevalence(results=results,
                           years=years,
                           ages=ages,
                           races=races,
                           subpopulations=subpopulations,
                           sexes=sexes,
                           risks=risks,
                           continuum=continuum,
                           cd4s=cd4s,
                           hiv.subsets=hiv.subsets,
                           keep.dimensions=keep.dimensions,
                           per.population=per.population)
}

do.extract.new.diagnoses <- function(results,
                                     years=NULL,
                                     ages=NULL,
                                     races=NULL,
                                     subpopulations=NULL,
                                     sexes=NULL,
                                     risks=NULL,
                                     continuum.from=NULL,
                                     cd4s=NULL,
                                     hiv.subsets=NULL,
                                     keep.dimensions=NULL,
                                     continuum.to=NULL,
                                     include.hiv.positive.in.denominator=T,
                                     per.population=100000,
                                     transformation.fn=NULL,
                                     use.cdc.categorizations=F
)
{
    if (setequal(risks, CDC.RISKS))
        risks = NULL
    if (setequal(sexes, CDC.SEXES))
        sexes = NULL
    
    if (use.cdc.categorizations &&
        (any(keep.dimensions=='sex') ||
         any(keep.dimensions=='risk') ||
         !is.null(risks) ||
         !is.null(sexes)))
    {
        if (!is.na(per.population))
            stop("collapsing to CDC not implemented for non-NA per.population")
        
        non.cdc = extract.new.diagnoses(results=results,
                                        years=years,
                                        ages=ages,
                                        races=races,
                                        subpopulations=subpopulations,
                                        sexes=NULL,
                                        risks=NULL,
                                        continuum.to=continuum.to,
                                        hiv.subsets=hiv.subsets,
                                        include.hiv.positive.in.denominator=T,
                                        keep.dimensions=union(keep.dimensions, c('sex','risk')),
                                        per.population=per.population)
        sum.arr.to.cdc(non.cdc,
                       keep.dimensions=keep.dimensions,
                       sexes=sexes,
                       risks=risks)
    }
    else
        extract.new.diagnoses(results=results,
                              years=years,
                              ages=ages,
                              races=races,
                              subpopulations=subpopulations,
                              sexes=sexes,
                              risks=risks,
                              continuum.to=continuum.to,
                              hiv.subsets=hiv.subsets,
                              include.hiv.positive.in.denominator=T,
                              keep.dimensions=keep.dimensions,
                              per.population=per.population)   
}

do.extract.overall.hiv.mortality <- function(results,
                                             years=NULL,
                                             ages=NULL,
                                             races=NULL,
                                             subpopulations=NULL,
                                             sexes=NULL,
                                             risks=NULL,
                                             continuum=NULL,
                                             cd4s=NULL,
                                             hiv.subsets=NULL,
                                             keep.dimensions=NULL,
                                             include.hiv.negative.in.denominator=T,
                                             per.population=100000,
                                             transformation.fn=NULL,
                                             use.cdc.categorizations=F
)
{
    if (setequal(risks, CDC.RISKS))
        risks = NULL
    if (setequal(sexes, CDC.SEXES))
        sexes = NULL
    
    if (use.cdc.categorizations &&
        (any(keep.dimensions=='sex') ||
         any(keep.dimensions=='risk') ||
         !is.null(risks) ||
         !is.null(sexes)))
    {
        if (!is.na(per.population))
            stop("collapsing to CDC not implemented for non-NA per.population")
        
        non.cdc = extract.overall.hiv.mortality(results=results,
                                                years=years,
                                                ages=ages,
                                                races=races,
                                                subpopulations=subpopulations,
                                                sexes=NULL,
                                                risks=NULL,
                                                continuum=continuum,
                                                cd4s=cd4s,
                                                hiv.subsets=hiv.subsets,
                                                keep.dimensions=union(keep.dimensions, c('sex','risk')),
                                                include.hiv.negative.in.denominator=include.hiv.negative.in.denominator,
                                                per.population=per.population)
        sum.arr.to.cdc(non.cdc,
                       keep.dimensions=keep.dimensions,
                       sexes=sexes,
                       risks=risks)
    }
    else
        extract.overall.hiv.mortality(results=results,
                                      years=years,
                                      ages=ages,
                                      races=races,
                                      subpopulations=subpopulations,
                                      sexes=sexes,
                                      risks=risks,
                                      continuum=continuum,
                                      cd4s=cd4s,
                                      hiv.subsets=hiv.subsets,
                                      keep.dimensions=keep.dimensions,
                                      include.hiv.negative.in.denominator=include.hiv.negative.in.denominator,
                                      per.population=per.population)
}

##----------------------------------------------##
##-- EXTRACTING SUPPRESSION and TESTING RATES --##
##----------------------------------------------##

extract.suppression <- function(sim,
                                years=sim$years,
                                keep.dimensions='year',
                                per.population=1,
                                ages=NULL,
                                races=NULL,
                                subpopulations=NULL,
                                sexes=NULL,
                                risks=NULL,
                                continuum=sim$diagnosed.continuum.states,
                                cd4=NULL,
                                hiv.subsets=NULL,
                                use.cdc.categorizations=F,
                                year.anchor=c('start','mid','end')[2])
{
    if (any(keep.dimensions=='continuum'))
        stop("Cannot keep the continuum dimension in calculating suppression")
    
    components = attr(sim, 'components')
    if (components$settings$IS_CONTINUUM_COLLAPSED)
    {
        if (year.anchor=='start')
        {
            raw.suppression.rates = calculate.suppression(components)
            do.extract.rates(raw.rates = raw.suppression.rates,
                             sim=sim,
                             years=years,
                             population.years.offset = -1,
                             keep.dimensions=keep.dimensions,
                             per.population=per.population,
                             ages=ages,
                             races=races,
                             subpopulations=subpopulations,
                             sexes=sexes,
                             risks=risks,
                             continuum=continuum,
                             cd4=cd4,
                             hiv.subsets=hiv.subsets,
                             use.cdc.categorizations=use.cdc.categorizations,
                             include.hiv.negative = F)
        }
        else if (year.anchor=='end')
        {
            #year-end yyyy = year-start yyyy+1
            rename.year.dim.with.offset(extract.suppression(sim,
                                                            years=years+1,
                                                            keep.dimensions=keep.dimensions,
                                                            per.population=per.population,
                                                            ages=ages,
                                                            races=races,
                                                            subpopulations=subpopulations,
                                                            sexes=sexes,
                                                            risks=risks,
                                                            continuum=sim$diagnosed.continuum.states,
                                                            cd4=cd4,
                                                            hiv.subsets=hiv.subsets,
                                                            use.cdc.categorizations=use.cdc.categorizations,
                                                            year.anchor='start'),
                                        keep.dimensions = keep.dimensions,
                                        offset=-1)
        }
        else #mid
        {
            (extract.suppression(sim,
                                years=years,
                                keep.dimensions=keep.dimensions,
                                per.population=per.population,
                                ages=ages,
                                races=races,
                                subpopulations=subpopulations,
                                sexes=sexes,
                                risks=risks,
                                continuum=sim$diagnosed.continuum.states,
                                cd4=cd4,
                                hiv.subsets=hiv.subsets,
                                use.cdc.categorizations=use.cdc.categorizations,
                                year.anchor='start') +
                 extract.suppression(sim,
                                     years=years,
                                     keep.dimensions=keep.dimensions,
                                     per.population=per.population,
                                     ages=ages,
                                     races=races,
                                     subpopulations=subpopulations,
                                     sexes=sexes,
                                     risks=risks,
                                     continuum=sim$diagnosed.continuum.states,
                                     cd4=cd4,
                                     hiv.subsets=hiv.subsets,
                                     use.cdc.categorizations=use.cdc.categorizations,
                                     year.anchor='end')) / 2
        }
    }
    else
    {
        if (is.null(continuum))
            continuum = sim$continuum
        suppressed.states = intersect(components$settings$SUPPRESSED_STATES, continuum)
        if (length(suppressed.states)==0)
            stop(paste0("None of the specified continuum states (",
                        paste0("'", continuum, "'", collapse=', '),
                        ") are suppressed states"))
        
        numerators = do.extract.population.subset(results=sim,
                                                  years=years,
                                                  ages=ages,
                                                  races=races,
                                                  subpopulations=subpopulations,
                                                  sexes=sexes,
                                                  risks=risks,
                                                  continuum=suppressed.states,
                                                  cd4s=cd4,
                                                  hiv.subsets=hiv.subsets,
                                                  include.hiv.positive=T,
                                                  include.hiv.negative=F,
                                                  keep.dimensions=keep.dimensions,
                                                  per.population=NA,
                                                  use.cdc.categorizations=use.cdc.categorizations)
        
        denominators = do.extract.population.subset(results=sim,
                                                    years=years,
                                                    ages=ages,
                                                    races=races,
                                                    subpopulations=subpopulations,
                                                    sexes=sexes,
                                                    risks=risks,
                                                    continuum=continuum,
                                                    cd4s=cd4,
                                                    hiv.subsets=hiv.subsets,
                                                    include.hiv.positive=T,
                                                    include.hiv.negative=F,
                                                    keep.dimensions=keep.dimensions,
                                                    per.population=NA,
                                                    use.cdc.categorizations=use.cdc.categorizations)
        
        numerators / denominators
    }
}

extract.linkage <- function(sim,
                            years=sim$years,
                            keep.dimensions='year',
                            per.population=1,
                            ages=NULL,
                            races=NULL,
                            subpopulations=NULL,
                            sexes=NULL,
                            risks=NULL,
                            continuum='unengaged',
                            cd4=NULL,
                            hiv.subsets=NULL,
                            use.cdc.categorizations=F)
{
    components = attr(sim, 'components')
    if (components$settings$IS_CONTINUUM_COLLAPSED)
        stop("Simulation uses a collapsed continuum - cannot extract linkage")
    
    raw.linkage.rates = calculate.linkage(components)
    do.extract.rates(raw.rates = raw.linkage.rates,
                     sim=sim,
                     years=years,
                     keep.dimensions=keep.dimensions,
                     per.population=per.population,
                     ages=ages,
                     races=races,
                     subpopulations=subpopulations,
                     sexes=sexes,
                     risks=risks,
                     continuum=continuum,
                     cd4=cd4,
                     hiv.subsets=hiv.subsets,
                     use.cdc.categorizations=use.cdc.categorizations,
                     include.hiv.negative = F)
}

do.extract.engagement <- function(sim,
                                years=sim$years,
                                keep.dimensions='year',
                                per.population=1,
                                ages=NULL,
                                races=NULL,
                                subpopulations=NULL,
                                sexes=NULL,
                                risks=NULL,
                                continuum=sim$diagnosed.continuum.states,
                                cd4=NULL,
                                hiv.subsets=NULL,
                                use.cdc.categorizations=F)
{
    if (any(keep.dimensions=='continuum'))
        stop("Cannot keep the continuum dimension in calculating suppression")
    
    components = attr(sim, 'components')
    if (components$settings$IS_CONTINUUM_COLLAPSED)
        stop("Simulation uses a collapsed continuum - cannot extract linkage")
    
    engaged.states = intersect(components$settings$ENGAGED_STATES, continuum)
    if (length(engaged.states)==0)
        stop(paste0("None of the specified continuum states (",
                    paste0("'", continuum, "'", collapse=', '),
                    ") are engaged states"))
    
    numerators = do.extract.population.subset(results=sim,
                                              years=years,
                                              ages=ages,
                                              races=races,
                                              subpopulations=subpopulations,
                                              sexes=sexes,
                                              risks=risks,
                                              continuum=engaged.states,
                                              cd4s=cd4,
                                              hiv.subsets=hiv.subsets,
                                              include.hiv.positive=T,
                                              include.hiv.negative=F,
                                              keep.dimensions=keep.dimensions,
                                              per.population=NA,
                                              use.cdc.categorizations=use.cdc.categorizations)
    
    denominators = do.extract.population.subset(results=sim,
                                                years=years,
                                                ages=ages,
                                                races=races,
                                                subpopulations=subpopulations,
                                                sexes=sexes,
                                                risks=risks,
                                                continuum=continuum,
                                                cd4s=cd4,
                                                hiv.subsets=hiv.subsets,
                                                include.hiv.positive=T,
                                                include.hiv.negative=F,
                                                keep.dimensions=keep.dimensions,
                                                per.population=NA,
                                                use.cdc.categorizations=use.cdc.categorizations)
    
    numerators / denominators
}

extract.prep.coverage <- function(sim,
                                  years=sim$years,
                                  keep.dimensions='year',
                                  per.population=1,
                                  ages=NULL,
                                  races=NULL,
                                  subpopulations=NULL,
                                  sexes=NULL,
                                  risks=NULL,
                                  use.cdc.categorizations=F,
                                  indications.multiplier = get.prep.indications.estimate(),
                                  year.anchor=c('start','mid','end')[2])
{
    if (year.anchor=='start')
    {
        # it is more efficient to multiply into the numerator (for technical reasons)
        # so if we just need the numerator, then apply the estimates for the numerator
        # otherwise, apply them to the denominator
        numerator.multiplier = denominator.multiplier = NULL
        if (is.na(per.population))
            numerator.multiplier = indications.multiplier
        else
            denominator.multiplier = indications.multiplier
        
        raw.prep.coverage = calculate.prep.coverage(attr(sim, 'components'))
        do.extract.rates(raw.rates = raw.prep.coverage,
                         sim=sim,
                         years=years,
                         population.years.offset = -1,
                         keep.dimensions=keep.dimensions,
                         per.population=per.population,
                         ages=ages,
                         races=races,
                         subpopulations=subpopulations,
                         sexes=sexes,
                         risks=risks,
                         use.cdc.categorizations=use.cdc.categorizations,
                         include.hiv.negative = T,
                         include.hiv.positive = F,
                         multiplier=numerator.multiplier,
                         denominator.multiplier = denominator.multiplier)
    }
    else if (year.anchor=='end')
    {
        rename.year.dim.with.offset(extract.prep.coverage(sim,
                                                          years=years+1,
                                                          keep.dimensions=keep.dimensions,
                                                          per.population=per.population,
                                                          ages=ages,
                                                          races=races,
                                                          subpopulations=subpopulations,
                                                          sexes=sexes,
                                                          risks=risks,
                                                          use.cdc.categorizations=use.cdc.categorizations,
                                                          indications.multiplier = indications.multiplier,
                                                          year.anchor='start'),
                                    keep.dimensions = keep.dimensions,
                                    offset=-1)
    }
    else # year.anchor=='mid'
    {
        (extract.prep.coverage(sim,
                              years=years,
                              keep.dimensions=keep.dimensions,
                              per.population=per.population,
                              ages=ages,
                              races=races,
                              subpopulations=subpopulations,
                              sexes=sexes,
                              risks=risks,
                              use.cdc.categorizations=use.cdc.categorizations,
                              indications.multiplier = indications.multiplier,
                              year.anchor='start') +
             extract.prep.coverage(sim,
                                   years=years,
                                   keep.dimensions=keep.dimensions,
                                   per.population=per.population,
                                   ages=ages,
                                   races=races,
                                   subpopulations=subpopulations,
                                   sexes=sexes,
                                   risks=risks,
                                   use.cdc.categorizations=use.cdc.categorizations,
                                   indications.multiplier = indications.multiplier,
                                   year.anchor='end')) / 2
    }

}

extract.n.prep.indicated <- function(sim,
                                     years=sim$years,
                                     keep.dimensions='year',
                                     per.population=1,
                                     ages=NULL,
                                     races=NULL,
                                     subpopulations=NULL,
                                     sexes=NULL,
                                     risks=NULL,
                                     non.hiv.subsets=NULL,
                                     use.cdc.categorizations=F,
                                     indications.estimate = get.prep.indications.estimate())
{
    #-- Pull the pop and multiply by indications --#
    indications.dim.names = dimnames(indications.estimate)
    
    pop = extract.population.subset(sim, 
                                    years=years, 
                                    keep.dimensions = c('year',names(indications.dim.names)),
                                    ages=indications.dim.names[['age']],
                                    races=indications.dim.names[['race']],
                                    subpopulations=indications.dim.names[['subpopulation']],
                                    sexes=indications.dim.names[['sex']],
                                    risks=indications.dim.names[['risk']], 
                                    non.hiv.subsets = indications.dim.names[['non.hiv.subsets']],
                                    include.hiv.negative = T,
                                    include.hiv.positive = F)
    
    pop = pop * expand.population(indications.estimate, dimnames(pop))

    #-- Aggregate (for CDC categorizations if needed) --#
    
    if (setequal(risks, CDC.RISKS))
        risks = NULL
    if (setequal(sexes, CDC.SEXES))
        sexes = NULL
    
    if (use.cdc.categorizations &&
        (any(keep.dimensions=='sex') ||
         any(keep.dimensions=='risk') ||
         !is.null(risks) ||
         !is.null(sexes)))
    {
        pop = access(pop,
                     age=ages,
                     race=races,
                     subpopulation=subpopulations,
                     non.hiv.subset = non.hiv.subsets,
                     collapse.length.one.dimensions = F)
        
        pop = sum.arr.to.cdc(pop,
                             keep.dimensions=keep.dimensions,
                             sexes=sexes,
                             risks=risks,
                             female.msm.value = 0)
    }
    else
    {
        pop = access(pop,
                     age=ages,
                     race=races,
                     subpopulation=subpopulations,
                     sex=sexes,
                     risk=risks,
                     non.hiv.subset = non.hiv.subsets,
                     collapse.length.one.dimensions = F)
        
    }

    pop = apply(pop, keep.dimensions, sum)
    pop
}

extract.testing.period <- function(sim,
                                   years=sim$years,
                                   keep.dimensions='year',
                                   per.population=1,
                                   ages=NULL,
                                   races=NULL,
                                   subpopulations=NULL,
                                   sexes=NULL,
                                   risks=NULL,
                                   use.cdc.categorizations=F,
                                   year.anchor=c('start','mid','end')[2])
{
    1 / extract.testing.rates(sim,
                              years=years,
                              keep.dimension=keep.dimensions,
                              per.population=per.population,
                              ages=ages,
                              races=races,
                              subpopulations=subpopulations,
                              sexes=sexes,
                              risks=risks,
                              use.cdc.categorizations = use.cdc.categorizations,
                              year.anchor = year.anchor)
}

extract.testing.rates <- function(sim,
                                  years=sim$years,
                                  keep.dimensions='year',
                                  per.population=1,
                                  ages=NULL,
                                  races=NULL,
                                  subpopulations=NULL,
                                  sexes=NULL,
                                  risks=NULL,
                                  use.cdc.categorizations=F,
                                  year.anchor=c('start','mid','end')[2])
{
    if (year.anchor=='start')
    {
        raw.testing.rates = calculate.testing.rates(attr(sim, 'components'))
        raw.testing.rates$rates = lapply(raw.testing.rates$rates, function(r){
            dim.names = dimnames(r)[1:5]
            r = r[,,,,,'undiagnosed',1,1]
            dim(r) = sapply(dim.names, length)
            dimnames(r) = dim.names
            r
        })
        do.extract.rates(raw.rates = raw.testing.rates,
                         sim=sim,
                         years=years,
                         population.years.offset = -1,
                         keep.dimensions=keep.dimensions,
                         per.population=per.population,
                         ages=ages,
                         races=races,
                         subpopulations=subpopulations,
                         sexes=sexes,
                         risks=risks,
                         use.cdc.categorizations=use.cdc.categorizations,
                         include.hiv.negative = T,
                         include.hiv.positive = F)
    }
    else if (year.anchor=='end')
    {
        rename.year.dim.with.offset(extract.testing.rates(sim,
                                                          years=years+1,
                                                          keep.dimensions=keep.dimensions,
                                                          per.population=per.population,
                                                          ages=ages,
                                                          races=races,
                                                          subpopulations=subpopulations,
                                                          sexes=sexes,
                                                          risks=risks,
                                                          use.cdc.categorizations=use.cdc.categorizations,
                                                          year.anchor='start'),
                                    keep.dimensions = keep.dimensions,
                                    offset = -1)
    }
    else #year.anchor == 'mid'
    {
        (extract.testing.rates(sim,
                               years=years,
                               keep.dimensions=keep.dimensions,
                               per.population=per.population,
                               ages=ages,
                               races=races,
                               subpopulations=subpopulations,
                               sexes=sexes,
                               risks=risks,
                               use.cdc.categorizations=use.cdc.categorizations,
                               year.anchor='start') +
            extract.testing.rates(sim,
                                  years=years,
                                  keep.dimensions=keep.dimensions,
                                  per.population=per.population,
                                  ages=ages,
                                  races=races,
                                  subpopulations=subpopulations,
                                  sexes=sexes,
                                  risks=risks,
                                  use.cdc.categorizations=use.cdc.categorizations,
                                  year.anchor='end')) / 2
    }
}

do.extract.rates <- function(raw.rates,
                             sim,
                             years=sim$years,
                             population.years.offset=0,
                             keep.dimensions='year',
                             per.population=1,
                             ages=NULL,
                             races=NULL,
                             subpopulations=NULL,
                             sexes=NULL,
                             risks=NULL,
                             continuum=sim$diagnosed.continuum.states,
                             cd4=NULL,
                             hiv.subsets=NULL,
                             use.cdc.categorizations=F,
                             include.hiv.negative=F,
                             include.hiv.positive=T,
                             multiplier=NULL,
                             denominator.multiplier=NULL)
{
    interpolated.rates = interpolate.parameters(values=raw.rates$rates,
                                                value.times=raw.rates$times,
                                                desired.times = years)
    
    if (!is.null(multiplier))
    {
        multiplier = expand.population(multiplier, dimnames(interpolated.rates[[1]]))
        interpolated.rates = lapply(interpolated.rates, function(r){r*multiplier})
    }
    
    stratified.dim.names = c(list(year=as.character(years)), dimnames(interpolated.rates[[1]]))
    
    stratified.arr = t(sapply(interpolated.rates, function(r){r}))
    dim(stratified.arr) = sapply(stratified.dim.names, length)
    dimnames(stratified.arr) = stratified.dim.names
    
    all.dimension.names = names(stratified.dim.names)
    #    dimensions.length.geq.1.names = all.dimension.names[sapply(stratified.dim.names, length)>1]
    #    if (setequal()
    
    all.dimension.names = setdiff(all.dimension.names, 'non.hiv.subset')
    
    if (include.hiv.negative)
        prevalence = rename.year.dim.with.offset(extract.population.subset(sim, years=years+population.years.offset,
                                                                           keep.dimensions = all.dimension.names,
                                                                           ages=ages, races=races, subpopulations=subpopulations,
                                                                           sexes=NULL, risks=NULL, 
                                                                           include.hiv.negative = T,
                                                                           include.hiv.positive = include.hiv.positive),
                                                 keep.dimensions = all.dimension.names,
                                                 offset = -population.years.offset)
    else    
        prevalence = rename.year.dim.with.offset(extract.population.subset(sim, years=years+population.years.offset, 
                                                                           keep.dimensions = all.dimension.names,
                                                                           ages=ages, races=races, subpopulations=subpopulations,
                                                                           sexes=NULL, risks=NULL,
                                                                           continuum=continuum, cd4=cd4, hiv.subsets=hiv.subsets,
                                                                           include.hiv.negative = F),
                                                 keep.dimensions = all.dimension.names,
                                                 offset = -population.years.offset)

    if (!is.null(denominator.multiplier))
    {
        denominator.to.access = dimnames(prevalence)[names(dimnames(denominator.multiplier))]
        
        denominator.multiplier = access(denominator.multiplier,
                                        year=denominator.to.access$year,
                                        age=denominator.to.access$age,
                                        race=denominator.to.access$race,
                                        subpopulation=denominator.to.access$subpopulation,
                                        sex=denominator.to.access$sex,
                                        risk=denominator.to.access$risk,
                                        continuum=denominator.to.access$continuum,
                                        cd4=denominator.to.access$cd4,
                                        hiv.subset=denominator.to.access$hiv.subset)
        
        denominator.multiplier = expand.population(denominator.multiplier, dimnames(prevalence))
        prevalence = prevalence * denominator.multiplier
    }
    
    
    n.dim = length(dim(stratified.arr))
    if (include.hiv.negative)
        n.dim = n.dim-1
    not.subset.dimension.mask = sapply(1:n.dim, function(d){
        length(setdiff(dimnames(prevalence)[[d]], dimnames(stratified.arr)[[d]]))>0
    })
    
   
    if (sum(not.subset.dimension.mask)==1 && 
        names(dimnames(prevalence))[not.subset.dimension.mask]=='cd4' &&
        length(dim(prevalence)['cd4'])==1) #a faster hack for the common use
    {
        stratified.dim.names[['cd4']] = dimnames(prevalence)[['cd4']]
        stratified.arr = stratified.arr[,,,,,,,1,]
        
        dim(stratified.arr) = sapply(stratified.dim.names, length)
        dimnames(stratified.arr) = stratified.dim.names
    }
    else if (any(not.subset.dimension.mask))
    {
        if (any(dim(prevalence)[not.subset.dimension.mask] != 1))
            stop("Dimensions of stratified.arr and prevalence cannot be reconciled")
        
        if (any(sapply( (1:length(dim(prevalence)))[not.subset.dimension.mask], function(d){
            any(apply(stratified.arr, names(dim(stratified.arr)[-d]), sd) > 0)
        })))
            stop("Dimensions of stratified.arr and prevalence cannot be reconciled")
        
        stratified.dim.names[not.subset.dimension.mask] = dimnames(prevalence)[not.subset.dimension.mask]
        
        stratified.arr = apply(stratified.arr, (1:length(dim(prevalence)))[!not.subset.dimension.mask], mean) 
        dim(stratified.arr) = sapply(stratified.dim.names, length)
        dimnames(stratified.arr) = stratified.dim.names
    }
    
    if (include.hiv.negative)
    {
        if (length(dim(stratified.arr))==6)
            numerators = as.numeric(prevalence) *
                as.numeric(stratified.arr[,dimnames(prevalence)[['age']],dimnames(prevalence)[['race']],
                                          dimnames(prevalence)[['subpopulation']],dimnames(prevalence)[['sex']],
                                          dimnames(prevalence)[['risk']] ])
        else
            numerators = as.numeric(prevalence) *
                as.numeric(stratified.arr[,dimnames(prevalence)[['age']],dimnames(prevalence)[['race']],
                                          dimnames(prevalence)[['subpopulation']],dimnames(prevalence)[['sex']],
                                          dimnames(prevalence)[['risk']], ])
    }
    else
        numerators = as.numeric(prevalence) *
        as.numeric(stratified.arr[,dimnames(prevalence)[['age']],dimnames(prevalence)[['race']],
                                  dimnames(prevalence)[['subpopulation']],dimnames(prevalence)[['sex']],
                                  dimnames(prevalence)[['risk']],dimnames(prevalence)[['continuum']],
                                  dimnames(prevalence)[['cd4']],dimnames(prevalence)[['hiv.subset']]])
    dim(numerators) = dim(prevalence)
    dimnames(numerators) = dimnames(prevalence)
    
    if (setequal(risks, CDC.RISKS))
        risks = NULL
    if (setequal(sexes, CDC.SEXES))
        sexes = NULL
    
    if (use.cdc.categorizations && 
        (any(keep.dimensions=='risk') || any(keep.dimensions=='sex') ||
         !is.null(risks) || !is.null(sexes)))
    {
        numerators = sum.suppression.arr.to.cdc(numerators)
        prevalence = sum.suppression.arr.to.cdc(prevalence)
    }
    
    if (!is.null(sexes) && !is.null(risks))
    {
        dim.names = dimnames(numerators)
        dim.names[['sex']] = sexes
        dim.names[['risk']] = risks
        
        if (include.hiv.negative)
        {
            numerators = numerators[,,,,sexes,risks]
            prevalence = prevalence[,,,,sexes,risks]
        }
        else
        {
            numerators = numerators[,,,,sexes,risks,,,]
            prevalence = prevalence[,,,,sexes,risks,,,]
        }
        
        dim(numerators) = sapply(dim.names, length)
        dimnames(numerators) = dim.names
        dim(prevalence) = sapply(dim.names, length)
        dimnames(prevalence) = dim.names
    }
    else if (!is.null(sexes))
    {
        dim.names = dimnames(numerators)
        dim.names[['sex']] = sexes
        
        if (include.hiv.negative)
        {
            numerators = numerators[,,,,sexes,]
            prevalence = prevalence[,,,,sexes,]
        }
        else
        {
            numerators = numerators[,,,,sexes,,,,]
            prevalence = prevalence[,,,,sexes,,,,]
        }
        
        dim(numerators) = sapply(dim.names, length)
        dimnames(numerators) = dim.names
        dim(prevalence) = sapply(dim.names, length)
        dimnames(prevalence) = dim.names
    }
    else if (!is.null(risks))
    {
        dim.names = dimnames(numerators)
        dim.names[['risk']] = risks
        
        if (include.hiv.negative)
        {
            numerators = numerators[,,,,,risks]
            prevalence = prevalence[,,,,,risks]
        }
        else
        {
            numerators = numerators[,,,,,risks,,,]
            prevalence = prevalence[,,,,,risks,,,]
        }
        
        dim(numerators) = sapply(dim.names, length)
        dimnames(numerators) = dim.names
        dim(prevalence) = sapply(dim.names, length)
        dimnames(prevalence) = dim.names
    }
    
    numerators = apply(numerators, keep.dimensions, sum)
    
    if (is.na(per.population))
        numerators
    else
    {
        prevalence = apply(prevalence, keep.dimensions, sum)
        rv = numerators / prevalence * per.population
        rv[numerators==0 & prevalence==0] = 0
    
        rv
    }
}

##-------------##
##-- HELPERS --##
##-------------##

sum.suppression.arr.to.cdc <- function(arr)
{
    dim.names = dimnames(arr)
    
    non.idu.states = 'never_IDU'
    idu.states = setdiff(dim.names[['risk']], non.idu.states)
    collapse.idu.dimensions = setdiff(names(dim.names), c('risk','sex'))
    
    dim.names[['sex']] = c('female','male')
    dim.names[['risk']] = c('msm','idu','msm_idu','heterosexual')
    
    rv = array(0, dim=sapply(dim.names,length), dimnames=dim.names)
    
    if (length(dim.names)==6)
    {
        rv[,,,,'male','msm'] = arr[,,,,'msm','never_IDU']
        rv[,,,,'male','msm_idu'] = arr[,,,,'msm','active_IDU'] + arr[,,,,'msm','IDU_in_remission']
        rv[,,,,'male','idu'] = arr[,,,,'heterosexual_male','active_IDU'] + arr[,,,,'heterosexual_male','IDU_in_remission']
        rv[,,,,'male','heterosexual'] = arr[,,,,'heterosexual_male','never_IDU']
        rv[,,,,'female','idu'] = arr[,,,,'female','active_IDU'] + arr[,,,,'female','IDU_in_remission']
        rv[,,,,'female','heterosexual'] = arr[,,,,'female','never_IDU']
    }
    else
    {
        rv[,,,,'male','msm',,,] = arr[,,,,'msm','never_IDU',,,]
        rv[,,,,'male','msm_idu',,,] = arr[,,,,'msm','active_IDU',,,] + arr[,,,,'msm','IDU_in_remission',,,]
        rv[,,,,'male','idu',,,] = arr[,,,,'heterosexual_male','active_IDU',,,] + arr[,,,,'heterosexual_male','IDU_in_remission',,,]
        rv[,,,,'male','heterosexual',,,] = arr[,,,,'heterosexual_male','never_IDU',,,]
        rv[,,,,'female','idu',,,] = arr[,,,,'female','active_IDU',,,] + arr[,,,,'female','IDU_in_remission',,,]
        rv[,,,,'female','heterosexual',,,] = arr[,,,,'female','never_IDU',,,]
    }
    
    rv
}

#A slower, general function
sum.arr.to.cdc <- function(arr,
                           keep.dimensions=NULL,
                           sexes=NULL,
                           risks=NULL,
                           female.msm.value=as.numeric(NA))
{
    # Initial set up
    dim.names = dimnames(arr)
    if (is.null(keep.dimensions))
        keep.dimensions = names(dim.names)
    
    non.idu.states = 'never_IDU'
    idu.states = setdiff(dim.names[['risk']], non.idu.states)
    
    # Set up the new dim names
    if (is.null(sexes))
        sexes = CDC.SEXES
    else
        sexes = intersect(sexes, CDC.SEXES)
    
    if (any(keep.dimensions=='sex'))
        dim.names[['sex']] = intersect(CDC.SEXES, sexes)
    else
        dim.names = dim.names[names(dim.names)!='sex']
    
    if (is.null(risks))
        risks = CDC.RISKS
    else
        risks = intersect(risks, CDC.RISKS)
    
    if (any(keep.dimensions=='risk'))
        dim.names[['risk']] = intersect(CDC.RISKS, risks)
    else
        dim.names = dim.names[names(dim.names)!='risk']
    
    if (any(sapply(sexes, function(sex){all(sex!=CDC.SEXES)})))
        stop(paste0("subscript out of bounds. sexes must be a subset of ",
                    paste0("'", CDC.SEXES, "'", collapse=', ')))
    if (any(sapply(risks, function(risk){all(risk!=CDC.RISKS)})))
        stop(paste0("subscript out of bounds. risks must be a subset of ",
                    paste0("'", CDC.RISKS, "'", collapse=', ')))
    
    # Set up the return array
    rv = array(female.msm.value, dim=sapply(dim.names,length), dimnames=dim.names)
    non.risk.non.sex.dimensions = setdiff(names(dim.names), c('risk','sex'))
    
    # Pull from the non-cdc categorizations into rv
    if (all(keep.dimensions!='sex') && all(keep.dimensions!='risk'))
    {
        if (setequal(risks, CDC.RISKS))
        {
            if (setequal(sexes, CDC.SEXES))
                rv = apply(arr, non.risk.non.sex.dimensions, sum)
            else if (any(sexes=='male'))
                rv = apply(access(arr, sex=c('heterosexual_male','msm')), non.risk.non.sex.dimensions, sum)
            else #sexes == 'female'
                rv = apply(access(arr, sex='female'), non.risk.non.sex.dimensions, sum)
        }
        else
        {
            if (setequal(sexes, CDC.SEXES) || any(sexes=='male'))
            {
                if (setequal(sexes, CDC.SEXES))
                    non.msm.sexes = c('heterosexual_male', 'female')
                else
                    non.msm.sexes = 'heterosexual_male'
                
                msm.keep.risks = character()
                if (any(risks=='msm'))
                    msm.keep.risks = c(msm.keep.risks, 'never_IDU')
                if (any(risks=='msm_idu'))
                    msm.keep.risks = c(msm.keep.risks, idu.states)
                
                non.msm.keep.risks = character()
                if (any(risks=='heterosexual'))
                    non.msm.keep.risks = c(non.msm.keep.risks, 'never_IDU')
                if (any(risks=='idu'))
                    non.msm.keep.risks = c(non.msm.keep.risks, idu.states)
                
                if (length(msm.keep.risks)>0 && length(non.msm.keep.risks)>0)
                    rv = apply(access(arr, sex='msm', risk=msm.keep.risks), non.risk.non.sex.dimensions, sum) +
                    apply(access(arr, sex=non.msm.sexes, risk=non.msm.keep.risks), non.risk.non.sex.dimensions, sum)
                else if (length(msm.keep.risks)>0)
                    rv = apply(access(arr, sex='msm', risk=msm.keep.risks), non.risk.non.sex.dimensions, sum)
                else if (length(non.msm.keep.risks)>0)
                    rv = apply(access(arr, sex=non.msm.sexes, risk=non.msm.keep.risks), non.risk.non.sex.dimensions, sum)
                
            }
            else #sexes == 'female
            {
                if (any(risks=='heterosexual') && any(risks=='idu'))
                    rv = apply(access(arr, sex='female'), non.risk.non.sex.dimensions, sum)
                else if (any(risks=='heterosexual'))
                    rv = access(arr, sex='female', risk='never_IDU')
                else #risks == idu
                    rv = apply(access(arr, sex='female', risk=idu.states), non.risk.non.sex.dimensions, sum)
            }
        }
    }
    if (all(keep.dimensions!='sex')) #risk only
    {
        if (any(sexes=='male'))
        {
            if (any(risks=='msm'))
                access(rv, risk='msm') = access(arr, sex='msm', risk='never_IDU')
            if (any(risks=='msm_idu'))
            {
                if (length(non.risk.non.sex.dimensions)==0)
                    access(rv, risk='msm_idu') = sum(access(arr, sex='msm', risk=idu.states, collapse.length.one.dimensions = F))
                else
                    access(rv, risk='msm_idu') = apply(access(arr, sex='msm', risk=idu.states, collapse.length.one.dimensions = F), non.risk.non.sex.dimensions, sum)
            }
        }
        
        if (setequal(sexes, CDC.SEXES))
        {
            if (any(risks=='idu'))
            {
                if (length(non.risk.non.sex.dimensions)==0)
                    access(rv, risk='idu') = sum(access(arr, sex=c('heterosexual_male','female'), risk=idu.states, collapse.length.one.dimensions = F))
                else
                    access(rv, risk='idu') = apply(access(arr, sex=c('heterosexual_male','female'), risk=idu.states, collapse.length.one.dimensions = F), non.risk.non.sex.dimensions, sum)
            }
            if (any(risks=='heterosexual'))
            {
                if (length(non.risk.non.sex.dimensions)==0)
                    access(rv, risk='heterosexual') = sum(access(arr, sex=c('heterosexual_male','female'), risk='never_IDU'))
                else
                    access(rv, risk='heterosexual') = apply(access(arr, sex=c('heterosexual_male','female'), risk='never_IDU'), non.risk.non.sex.dimensions, sum)
            }
        }
        else if (any(sexes=='male'))
        {
            if (any(risks=='idu'))
            {
                if (length(non.risk.non.sex.dimensions)==0)
                    access(rv, risk='idu') = sum(access(arr, sex='heterosexual_male', risk=idu.states, collapse.length.one.dimensions = F))
                else
                    access(rv, risk='idu') = apply(access(arr, sex='heterosexual_male', risk=idu.states, collapse.length.one.dimensions = F), non.risk.non.sex.dimensions, sum)
            }
            if (any(risks=='heterosexual'))
                access(rv, risk='heterosexual') = access(arr, sex='heterosexual_male', risk='never_IDU')
        }
        else #sexes == female
        {
            if (any(risks=='idu'))
            {
                if (length(non.risk.non.sex.dimensions)==0)
                    access(rv, risk='idu') = sum(access(arr, sex='female', risk=idu.states, collapse.length.one.dimensions = F))
                else
                    access(rv, risk='idu') = apply(access(arr, sex='female', risk=idu.states, collapse.length.one.dimensions = F), non.risk.non.sex.dimensions, sum)
            }
            if (any(risks=='heterosexual'))
                access(rv, risk='heterosexual') = access(arr, sex='female', risk='never_IDU')
        }
    }
    else if (all(keep.dimensions!='risk')) #sex only
    {
        if (any(sexes=='male'))
        {
            if (setequal(risks, CDC.RISKS))
                access(rv, sex='male') = apply(access(arr, sex=c('heterosexual_male','msm')), non.risk.non.sex.dimensions, sum)
            else
            {
                msm.keep.risks = character()
                if (any(risks=='msm'))
                    msm.keep.risks = c(msm.keep.risks, 'never_IDU')
                if (any(risks=='msm_idu'))
                    msm.keep.risks = c(msm.keep.risks, idu.states)
                
                het.male.keep.risks = character()
                if (any(risks=='heterosexual'))
                    het.male.keep.risks = c(het.male.keep.risks, 'never_IDU')
                if (any(risks=='idu'))
                    het.male.keep.risks = c(het.male.keep.risks, idu.states)
                
                if (length(msm.keep.risks)>0 && length(het.male.keep.risks)>0)
                    access(rv, sex='male') = apply(access(arr, sex='msm', risk=msm.keep.risks), non.risk.non.sex.dimensions, sum) +
                    apply(access(arr, sex='heterosexual_male', risk=het.male.keep.risks), non.risk.non.sex.dimensions, sum)
                else if (length(msm.keep.risks)>0)
                    access(rv, sex='male') = apply(access(arr, sex='msm', risk=msm.keep.risks), non.risk.non.sex.dimensions, sum)
                else if (length(het.male.keep.risks)>0)
                    access(rv, sex='male') = apply(access(arr, sex='heterosexual_male', risk=het.male.keep.risks), non.risk.non.sex.dimensions, sum)
            }
        }
        
        if (any(sexes=='female'))
        {
            if (any(risks=='idu') && any(risks=='heterosexual'))
                access(rv, sex='female') = apply(access(arr, sex='female'), non.risk.non.sex.dimensions, sum)
            else if (any(risks=='idu'))
                access(rv, sex='female') = apply(access(arr, sex='female', risk=idu.states), non.risk.non.sex.dimensions, sum)
            else if (any(risks=='heterosexual'))
                access(rv, sex='female') = access(arr, sex='female', risk='never_IDU')
        }
    }
    else #keeping both sex and risk
    {
        if (any(sexes=='male'))
        {
            if (any(risks=='msm'))
                access(rv, sex='male', risk='msm') = access(arr, sex='msm', risk='never_IDU')
            if (any(risks=='msm_idu'))
                access(rv, sex='male', risk='msm_idu') = apply(access(arr, sex='msm', risk=idu.states, collapse.length.one.dimensions = F), non.risk.non.sex.dimensions, sum)
            if (any(risks=='idu'))
                access(rv, sex='male', risk='idu') = apply(access(arr, sex='heterosexual_male', risk=idu.states, collapse.length.one.dimensions = F), non.risk.non.sex.dimensions, sum)
            if (any(risks=='heterosexual'))
                access(rv, sex='male', risk='heterosexual') = access(arr, sex='heterosexual_male', risk='never_IDU')
        }
        
        if (any(sexes=='female'))
        {
            if (any(risks=='idu'))
                access(rv, sex='female', risk='idu') = apply(access(arr, sex='female', risk=idu.states, collapse.length.one.dimensions = F), non.risk.non.sex.dimensions, sum)
            if (any(risks=='heterosexual'))
                access(rv, sex='female', risk='heterosexual') = access(arr, sex='female', risk='never_IDU')
            
            #            if (any(risks=='msm'))
            #            {
            #                if (any(risks=='msm_idu'))
            #                    access(rv, sex='female', risk=c('msm','msm_idu')) = NaN
            #                else
            #                    access(rv, sex='female', risk='msm') = NaN
            #            }
            #            else if (any(risks=='msm_idu'))
            #                access(rv, sex='female', risk='msm_idu') = NaN
            
        }
    }
    
    rv
}

rename.year.dim.with.offset <- function(arr,
                                        offset,
                                        keep.dimensions)
{
    if (any(keep.dimensions=='year') && offset!=0)
    {
        if (is.null(dim(arr)))
        {
            names(arr) = as.character(as.numeric(names(arr)) + offset)
        }
        else
        {
            dimnames(arr)$year = as.character(as.numeric(dimnames(arr)$year) + offset)
        }
        
        arr
    }
    else
        arr
}