
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
    if (length(setdiff(years, sim$years))>0)
        stop(paste0("The following years are not represented in the simulation: ",
                    paste0(setdiff(years, sim$years), collapse=', ')))
    
    keep.years = years
    years = sort(union(years, census.totals$years))
    
    census.data = get.census.totals(census.totals, 
                                    location=attr(sim, 'location'), 
                                    years = years,
                                    interpolate.missing.years = 'when.some.years.not.missing',
                                    flatten.single.dim.array = T)
    
    
    if (all(is.na(census.data[as.character(intersect(sim$years, years))])))
        stop("None of the simulation's years are represented in census totals data")
    
    
    
#    census.data = census.data[!is.na(census.data)]
#    if (length(census.data)==0)
#        stop(paste0("No census data available for location '", attr(sim, 'location'), "'"))
    
    last.census.year = max(as.numeric(names(census.data[!is.na(census.data)])))
    years.after.census = years[years>last.census.year]
    first.census.year = min(as.numeric(names(census.data[!is.na(census.data)])))
    years.before.census = years[years<first.census.year]
    years.with.census = setdiff(years, union(years.before.census, years.after.census))
    
    rv.with.census = census.data[as.character(years.with.census)]
    # interpolate if any are missing
    if (any(is.na(rv.with.census)))
        rv.with.census = interpolate.parameters(values=rv.with.census[!is.na(rv.with.census)],
                                                value.times = years.with.census[!is.na(rv.with.census)],
                                                desired.times = years.with.census)

    if (length(years.before.census)==0)
        rv.before.census = numeric()
    else
    {
        sim.pop.before.census = extract.population.subset(sim, keep.dimensions = 'year', years=years.before.census)
        sim.pop.first.with.census = extract.population.subset(sim, keep.dimensions = 'year', years=first.census.year)
        cen.pop.first.with.census = census.data[as.character(first.census.year)]
        
        rv.before.census = sim.pop.before.census / sim.pop.first.with.census * cen.pop.first.with.census
    }
    
    if (length(years.after.census)==0)
        rv.after.census = numeric()
    else
    {
        sim.pop.after.census = extract.population.subset(sim, keep.dimensions = 'year', years=years.after.census)
        sim.pop.last.with.census = extract.population.subset(sim, keep.dimensions = 'year', years=last.census.year)
        cen.pop.last.with.census = census.data[as.character(last.census.year)]
        
        rv.after.census = sim.pop.after.census / sim.pop.last.with.census * cen.pop.last.with.census
    }
   
    rv = c(rv.before.census, rv.with.census, rv.after.census)
    names(rv) = as.character(years)
    rv[as.character(keep.years)]
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
                                         keep.dimensions='year',
                                         denominator.dimensions='year',
                                         per.population=NA,
                                         transformation.fn=NULL,
                                         use.cdc.categorizations=F,
                                         throw.error.if.missing.years=T)
{
    years = check.postprocessing.years(sim=results,
                                       data.type='Population',
                                       years=years,
                                       throw.error=throw.error.if.missing.years)
    
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
                                     use.cdc.categorizations=F,
                                     throw.error.if.missing.years=T
)
{
    years = check.postprocessing.years(sim=results,
                                       data.type='Diagnosed HIV Prevalence',
                                       years=years,
                                       throw.error=throw.error.if.missing.years)
    
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
                                 use.cdc.categorizations=F,
                                 throw.error.if.missing.years=T
)
{
    years = check.postprocessing.years(sim=results,
                                       data.type='Incidence',
                                       years=years,
                                       throw.error=throw.error.if.missing.years)
    
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
                                  use.cdc.categorizations=F,
                                  throw.error.if.missing.years=T
)
{
    years = check.postprocessing.years(sim=results,
                                       data.type='Prevalence',
                                       years=years,
                                       throw.error=throw.error.if.missing.years)
    
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
                                     use.cdc.categorizations=F,
                                     throw.error.if.missing.years=T
)
{
    years = check.postprocessing.years(sim=results,
                                       data.type='New Diagnoses',
                                       years=years,
                                       throw.error=throw.error.if.missing.years)
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
                                        cd4s = cd4s,
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
                              cd4s = cd4s,
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
                                             use.cdc.categorizations=F,
                                             throw.error.if.missing.years=T
)
{
    years = check.postprocessing.years(sim=results,
                                       data.type='Mortality',
                                       years=years,
                                       throw.error=throw.error.if.missing.years)
    
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

##-------------------------------------------------------##
##-- A HELPER FUNCTION FOR EXTRACTING WITH YEAR ANCHOR --##
##-------------------------------------------------------##

do.extract.for.year.anchor <- function(years,
                                       year.anchor,
                                       raw.year.is.start, # a boolean - if T, then 2000 = Jan 1, 2000. If F, 2000 = Dec 31, 2000
                                       extract.fn, #takes arguments years, ...
                                       keep.dimensions
                                       )
{
    if (raw.year.is.start)
    {
        start.offset = 0
        end.offset = 1
    }
    else
    {
        start.offset = -1
        end.offset = 0
    }
    
    orig.years = years
    if (year.anchor=='mid')
    {
        if (any(keep.dimensions=='year'))
        {
            years = sort(union(years+start.offset, years+end.offset))
            
            raw = extract.fn(years)
            dim.names = dimnames(raw)
            dim.names$year = as.character(orig.years)
            
            start.mask = sapply(years, function(y){
                any(y==(orig.years+start.offset))
            })
            end.mask = sapply(years, function(y){
                any(y==(orig.years+end.offset))
            })
            
            #this depends on year being the first dimension
            rv = (raw[start.mask] + raw[end.mask]) / 2
            dim(rv) = sapply(dim.names, length)
            dimnames(rv) = dim.names
        }
        else
            rv = (extract.fn(years+start.offset) + extract.fn(years+end.offset))/2
    }
    else
    {
        if (year.anchor=='start')
            years = years + start.offset
        else
            years = years + end.offset
        
        rv = extract.fn(years)
    }
    
    # Make sure the year dimension is named according to the given years
    if (any(keep.dimensions=='year'))
    {
        if (is.null(dim(rv)))
            names(rv) = as.character(orig.years)
        else
            dimnames(rv)$year = as.character(orig.years)
    }
    
    rv
}

##----------------------------------------------##
##-- EXTRACTING SUPPRESSION and TESTING RATES --##
##----------------------------------------------##

extract.suppression <- function(sim,
                                years=NULL,
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
                                year.anchor=c('start','mid','end')[2],
                                throw.error.if.missing.years=T)
{
    years = check.postprocessing.years(sim,
                                       data.type='Suppression',
                                       years=years,
                                       year.anchor=year.anchor,
                                       throw.error=throw.error.if.missing.years)
    
    if (any(keep.dimensions=='continuum'))
        stop("Cannot keep the continuum dimension in calculating suppression")
    
    components = attr(sim, 'components')
    settings = get.components.settings(components)
    if (settings$IS_CONTINUUM_COLLAPSED)
    {
        if (is.null(years))
        {
            if (year.anchor=='start')
                years = sim$years
            else
                years = sim$years[-length(years)]
        }
        
        extract.fn = function(fn.years)
        {
            raw.suppression.rates = calculate.suppression(components)
            do.extract.rates(raw.rates = raw.suppression.rates,
                             sim=sim,
                             years=fn.years,
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
        
        raw.year.is.start = T
    }
    else
    {
        if (is.null(years))
        {
            if (year.anchor=='end')
                years = sim$years
            else
                years = sim$years[-1]
        }
        
        suppressed.states = intersect(settings$SUPPRESSED_STATES, continuum)
        if (length(suppressed.states)==0)
            stop(paste0("None of the specified continuum states (",
                        paste0("'", continuum, "'", collapse=', '),
                        ") are suppressed states"))

        extract.fn = function(fn.years)
        {
            numerators = do.extract.population.subset(results=sim,
                                                      years=fn.years,
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
                                                        years=fn.years,
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
        
        raw.year.is.start = F
    }
    
    do.extract.for.year.anchor(years=years,
                               year.anchor=year.anchor,
                               raw.year.is.start=raw.year.is.start,
                               extract.fn=extract.fn,
                               keep.dimensions=keep.dimensions)
        
}


extract.linkage <- function(sim,
                            years=NULL,
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
                            use.cdc.categorizations=F,
                            year.anchor=c('start','mid','end')[2],
                            throw.error.if.missing.years=T)
{
    years = check.postprocessing.years(sim,
                                       data.type='Linkage',
                                       years=years,
                                       year.anchor=year.anchor,
                                       throw.error=throw.error.if.missing.years)
    
    components = attr(sim, 'components')
    settings = get.components.settings(components)
    if (settings$IS_CONTINUUM_COLLAPSED)
        stop("Simulation uses a collapsed continuum - cannot extract linkage")
    
    extract.fn = function(fn.years)
    {
        raw.linkage.rates = calculate.linkage(components)
        do.extract.rates(raw.rates = raw.linkage.rates,
                         sim=sim,
                         years=fn.years,
                         keep.dimensions=keep.dimensions,
                         per.population=per.population,
                         population.years.offset = -1,
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
    
    do.extract.for.year.anchor(years=years,
                               year.anchor=year.anchor,
                               raw.year.is.start=T,
                               extract.fn=extract.fn,
                               keep.dimensions=keep.dimensions)
}

extract.retention <- function(sim,
                              years=NULL,
                              keep.dimensions='year',
                              per.population=1,
                              ages=NULL,
                              races=NULL,
                              subpopulations=NULL,
                              sexes=NULL,
                              risks=NULL,
                              continuum=get.sim.settings(sim)$ENGAGED_STATES,
                              cd4=NULL,
                              hiv.subsets=NULL,
                              use.cdc.categorizations=F,
                              year.anchor=c('start','mid','end')[3],
                              throw.error.if.missing.years=T)
{
    settings = get.sim.settings(sim)
    do.extract.transition.by.to.from(sim=sim,
                                     dimension='continuum',
                                     subgroup='hiv.positive',
                                     to=settings$DISENGAGED_STATES,
                                     from=settings$ENGAGED_STATES,
                                     scale='proportion.staying',
                                     group.by='from',
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
                                     year.anchor=year.anchor,
                                     throw.error.if.missing.years=throw.error.if.missing.years)
}

OLD.extract.retention <- function(sim,
                              years=NULL,
                              keep.dimensions='year',
                              per.population=1,
                              ages=NULL,
                              races=NULL,
                              subpopulations=NULL,
                              sexes=NULL,
                              risks=NULL,
                              continuum=get.sim.settings(sim)$ENGAGED_STATES,
                              cd4=NULL,
                              hiv.subsets=NULL,
                              use.cdc.categorizations=F,
                              year.anchor=c('start','mid','end')[3],
                              throw.error.if.missing.years=T)
{
    years = check.postprocessing.years(sim,
                                       data.type='Retention',
                                       years=years,
                                       year.anchor=year.anchor,
                                       throw.error=throw.error.if.missing.years)
    
    components = attr(sim, 'components')
    settings = get.components.settings(components)
    if (settings$IS_CONTINUUM_COLLAPSED)
        stop("Simulation uses a collapsed continuum - cannot extract retention")
    
    invalid.states = setdiff(continuum, settings$ENGAGED_STATES)
    if (length(invalid.states)>0)
        stop(paste0("The following are not engaged states in the continuum: ",
                    paste0(invalid.states, collapse=', ')))
    
  
    extract.fn = function(fn.years)
    {
        # pull the raw rates
        raw.disengagement.rates = NULL
        
        # Naive
        if (any(continuum=='engaged_unsuppressed_naive'))
        {
            naive.to.disengaged = calculate.rates(components, 'naive.to.disengaged')
            naive.to.disengaged = interpolate.parameters(
                values=naive.to.disengaged$rates,
                value.times = naive.to.disengaged$times,
                desired.times = fn.years)
            
            if (is.null(raw.disengagement.rates))
                raw.disengagement.rates = naive.to.disengaged
            else
            {
                raw.disengagement.rates = lapply(1:length(raw.disengagement.rates), function(i){
                    raw.disengagement.rates[[i]][,,,,,'engaged_unsuppressed_naive',,] = 
                        naive.to.disengaged[[i]][,,,,,'engaged_unsuppressed_naive',,]
                    raw.disengagement.rates[[i]]
                })
            }                
        }
        
        # Failing
        if (any(continuum=='engaged_unsuppressed_failing'))
        {
            failing.to.disengaged = calculate.rates(components, 'failing.to.disengaged')
            failing.to.disengaged = interpolate.parameters(
                values=failing.to.disengaged$rates,
                value.times = failing.to.disengaged$times,
                desired.times = fn.years)
            
            if (is.null(raw.disengagement.rates))
                raw.disengagement.rates = failing.to.disengaged
            else
            {
                raw.disengagement.rates = lapply(1:length(raw.disengagement.rates), function(i){
                    raw.disengagement.rates[[i]][,,,,,'engaged_unsuppressed_failing',,] = 
                        failing.to.disengaged[[i]][,,,,,'engaged_unsuppressed_failing',,]
                    raw.disengagement.rates[[i]]
                })
            }                
        }
        
        # Suppressed
        if (any(continuum=='engaged_suppressed'))
        {
            suppressed.to.disengaged = calculate.rates(components, 'suppressed.to.disengaged')
            suppressed.to.disengaged = interpolate.parameters(
                values=suppressed.to.disengaged$rates,
                value.times = suppressed.to.disengaged$times,
                desired.times = fn.years)
            
            if (is.null(raw.disengagement.rates))
                raw.disengagement.rates = suppressed.to.disengaged
            else
            {
                raw.disengagement.rates = lapply(1:length(raw.disengagement.rates), function(i){
                    raw.disengagement.rates[[i]][,,,,,'engaged_suppressed',,] = 
                        suppressed.to.disengaged[[i]][,,,,,'engaged_suppressed',,]
                    raw.disengagement.rates[[i]]
                })
            }                
        }
        
        # convert to p retained
        raw.retention.p = list(
            rates=lapply(raw.disengagement.rates, function(r){
                exp(-r)
            }),
            times = fn.years)
        
        # do extract    
        do.extract.rates(raw.rates = raw.retention.p,
                         sim=sim,
                         years=fn.years,
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
    
    do.extract.for.year.anchor(years=years,
                               year.anchor=year.anchor,
                               raw.year.is.start=T,
                               extract.fn=extract.fn,
                               keep.dimensions=keep.dimensions)
}

extract.gain.of.suppression <- function(sim,
                              years=NULL,
                              keep.dimensions='year',
                              per.population=1,
                              ages=NULL,
                              races=NULL,
                              subpopulations=NULL,
                              sexes=NULL,
                              risks=NULL,
                              continuum=setdiff(get.sim.settings(sim)$ENGAGED_STATES,
                                                get.sim.settings(sim)$SUPPRESSED_STATES),
                              cd4=NULL,
                              hiv.subsets=NULL,
                              use.cdc.categorizations=F,
                              year.anchor=c('start','mid','end')[2],
                              throw.error.if.missing.years=T)
{
    years = check.postprocessing.years(sim,
                                       data.type='Gain of Suppression',
                                       years=years,
                                       year.anchor=year.anchor,
                                       throw.error=throw.error.if.missing.years)
    
    components = attr(sim, 'components')
    settings = get.components.settings(components)
    if (settings$IS_CONTINUUM_COLLAPSED)
        stop("Simulation uses a collapsed continuum - cannot extract retention")
    
    allowed.states = setdiff(settings$ENGAGED_STATES,
                             settings$SUPPRESSED_STATES)
    invalid.states = setdiff(continuum, allowed.states)
    if (length(invalid.states)>0)
        stop(paste0("The following are not engaged/unsuppressed states in the continuum: ",
                    paste0(invalid.states, collapse=', ')))
    
    
    extract.fn = function(fn.years)
    {
        # pull the raw rates
        raw.gain.of.suppression.p = NULL
        
        # Naive
        if (any(continuum=='engaged_unsuppressed_naive'))
        {
            naive.to.suppressed = calculate.rates(components, 'naive.to.suppressed')
            naive.to.suppressed = interpolate.parameters(
                values=naive.to.suppressed$rates,
                value.times = naive.to.suppressed$times,
                desired.times = fn.years)
            
            if (is.null(raw.gain.of.suppression.p))
                raw.gain.of.suppression.p = naive.to.suppressed
            else
            {
                raw.gain.of.suppression.p = lapply(1:length(raw.gain.of.suppression.p), function(i){
                    raw.gain.of.suppression.p[[i]][,,,,,'engaged_unsuppressed_naive',,] = 
                        naive.to.suppressed[[i]][,,,,,'engaged_unsuppressed_naive',,]
                    raw.gain.of.suppression.p[[i]]
                })
            }                
        }
        
        # Failing
        
        if (any(continuum=='engaged_unsuppressed_failing'))
        {
            failing.to.suppressed = calculate.rates(components, 'failing.to.suppressed')
            failing.to.suppressed = interpolate.parameters(
                values=failing.to.suppressed$rates,
                value.times = failing.to.suppressed$times,
                desired.times = fn.years)
            
            failing.to.suppressed = lapply(failing.to.suppressed, function(r){
                exp(-r)
            })
            
            if (is.null(raw.gain.of.suppression.p))
                raw.gain.of.suppression.p = failing.to.suppressed
            else
            {
                raw.gain.of.suppression.p = lapply(1:length(raw.gain.of.suppression.p), function(i){
                    raw.gain.of.suppression.p[[i]][,,,,,'engaged_unsuppressed_failing',,] = 
                        failing.to.suppressed[[i]][,,,,,'engaged_unsuppressed_failing',,]
                    raw.gain.of.suppression.p[[i]]
                })
            }                
        }
        
        raw.gain.of.suppression.p = list(
            rates = raw.gain.of.suppression.p,
            times = fn.years
        )
        
        # do extract    
        do.extract.rates(raw.rates = raw.gain.of.suppression.p,
                         sim=sim,
                         years=fn.years,
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
    
    do.extract.for.year.anchor(years=years,
                               year.anchor=year.anchor,
                               raw.year.is.start=T,
                               extract.fn=extract.fn,
                               keep.dimensions=keep.dimensions)
}

do.extract.engagement <- function(sim,
                                years=NULL,
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
                                year.anchor=c('start','mid','end')[3],
                                throw.error.if.missing.years=T)
{
    years = check.postprocessing.years(sim,
                               data.type='Engagement',
                               years=years,
                               year.anchor=year.anchor,
                               throw.error=throw.error.if.missing.years)
    
    if (any(keep.dimensions=='continuum'))
        stop("Cannot keep the continuum dimension in calculating suppression")
    
    components = attr(sim, 'components')
    settings = get.components.settings(components)
    if (settings$IS_CONTINUUM_COLLAPSED)
        stop("Simulation uses a collapsed continuum - cannot extract linkage")
    
    engaged.states = intersect(settings$ENGAGED_STATES, continuum)
    if (length(engaged.states)==0)
        stop(paste0("None of the specified continuum states (",
                    paste0("'", continuum, "'", collapse=', '),
                    ") are engaged states"))
    
    extract.fn = function(fn.years){
        numerators = do.extract.population.subset(results=sim,
                                                  years=fn.years,
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
                                                    years=fn.years,
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
    
    do.extract.for.year.anchor(years=years,
                               year.anchor=year.anchor,
                               raw.year.is.start=F,
                               extract.fn=extract.fn,
                               keep.dimensions=keep.dimensions)
}

extract.prep.persistence <- function(sim)
{
    components = attr(sim, 'components')
    components$prep.persistence
}

extract.prep.coverage <- function(sim,
                                  years= NULL,
                                  keep.dimensions='year',
                                  per.population=1,
                                  ages=NULL,
                                  races=NULL,
                                  subpopulations=NULL,
                                  sexes=NULL,
                                  risks=NULL,
                                  use.cdc.categorizations=F,
                                  indications.multiplier = get.prep.indications.estimate(),
                                  year.anchor=c('start','mid','end')[2],
                                  throw.error.if.missing.years=T)
{
    years = check.postprocessing.years(sim,
                               data.type='PrEP Coverage',
                               years=years,
                               year.anchor=year.anchor,
                               throw.error=throw.error.if.missing.years)
    
    if (is.null(years))
    {
        if (year.anchor=='start') 
            years = sim$years[-1] 
        else if (year.anchor=='end')
            years = sim$years[-length(sim$years)] 
        else 
            years = sim$years[-c(1,length(sim$years))]
    }
    
   extract.fn <- function(fn.years){
       
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
                         years=fn.years,
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
   
   do.extract.for.year.anchor(years=years,
                              year.anchor=year.anchor,
                              raw.year.is.start=T,
                              extract.fn=extract.fn,
                              keep.dimensions=keep.dimensions)

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
                                     indications.estimate = get.prep.indications.estimate(),
                                     throw.error.if.missing.years=T)
{
    
    years = check.postprocessing.years(sim,
                               data.type='PrEP Indications',
                               years=years,
                               throw.error=throw.error.if.missing.years)
    
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
                                   years=NULL,
                                   keep.dimensions='year',
                                   per.population=1,
                                   ages=NULL,
                                   races=NULL,
                                   subpopulations=NULL,
                                   sexes=NULL,
                                   risks=NULL,
                                   use.cdc.categorizations=F,
                                   year.anchor=c('start','mid','end')[2],
                                   throw.error.if.missing.years=!is.null(years))
{
    
    years = check.postprocessing.years(sim,
                               data.type='testing rates',
                               years=years,
                               year.anchor=year.anchor,
                               throw.error=throw.error.if.missing.years)
    
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


new.extract.testing.rates <- function(sim,
                                      years=NULL,
                                      keep.dimensions='year',
                                      per.population=1,
                                      ages=NULL,
                                      races=NULL,
                                      subpopulations=NULL,
                                      sexes=NULL,
                                      risks=NULL,
                                      continuum=NULL,
                                      cd4=NULL,
                                      hiv.subsets=NULL,
                                      use.cdc.categorizations=F,
                                      year.anchor=c('start','mid','end')[3],
                                      throw.error.if.missing.years=T)
{
    settings = get.sim.settings(sim)
    do.extract.transition.by.to.from(sim=sim,
                                     dimension='continuum',
                                     subgroup='hiv.positive',
                                     to=settings$FIRST_DIAGNOSED_STATE,
                                     from=settings$UNDIAGNOSED_NO_PREP,
                                     scale='rate',
                                     group.by='from',
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
                                     year.anchor=year.anchor,
                                     throw.error.if.missing.years=throw.error.if.missing.years)
}


extract.testing.rates <- function(sim,
                                  years=NULL,
                                  keep.dimensions='year',
                                  per.population=1,
                                  ages=NULL,
                                  races=NULL,
                                  subpopulations=NULL,
                                  sexes=NULL,
                                  risks=NULL,
                                  use.cdc.categorizations=F,
                                  year.anchor=c('start','mid','end')[2],
                                  throw.error.if.missing.years=!is.null(years))
{
    years = check.postprocessing.years(sim,
                               data.type='testing rates',
                               years=years,
                               year.anchor=year.anchor,
                               throw.error=throw.error.if.missing.years)
        
    extract.fn = function(fn.years)
    {
        raw.testing.rates = calculate.testing.rates(attr(sim, 'components'))
        raw.testing.rates$rates = lapply(raw.testing.rates$rates, function(r){
            if (any(names(dim(r))=='continuum'))
                r = single.dim.access(r, dim='continuum', dim.value='undiagnosed')
            dim.names = dimnames(r)[1:5]
            r = r[,,,,,1,1]
            dim(r) = sapply(dim.names, length)
            dimnames(r) = dim.names
            r
        })
        do.extract.rates(raw.rates = raw.testing.rates,
                         sim=sim,
                         years=fn.years,
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
    
    do.extract.for.year.anchor(years=years,
                               year.anchor=year.anchor,
                               raw.year.is.start=T,
                               extract.fn=extract.fn,
                               keep.dimensions=keep.dimensions)
}

extract.testing.proportions <- function(sim,
                                        years=NULL,
                                        keep.dimensions='year',
                                        per.population=1,
                                        ages=NULL,
                                        races=NULL,
                                        subpopulations=NULL,
                                        sexes=NULL,
                                        risks=NULL,
                                        use.cdc.categorizations=F,
                                        year.anchor=c('start','mid','end')[2],
                                        throw.error.if.missing.years=!is.null(years))
{
    years = check.postprocessing.years(sim,
                                       data.type='testing rates',
                                       years=years,
                                       year.anchor=year.anchor,
                                       throw.error=throw.error.if.missing.years)
    
    extract.fn = function(fn.years)
    {
        raw.testing = calculate.testing.rates(attr(sim, 'components'))
        raw.testing$rates = lapply(raw.testing$rates, function(r){
            if (any(names(dim(r))=='continuum'))
                r = single.dim.access(r, dim='continuum', dim.value='undiagnosed')
            dim.names = dimnames(r)[1:5]
            r = r[,,,,,1,1]
            dim(r) = sapply(dim.names, length)
            dimnames(r) = dim.names
            1-exp(-r)
        })
        do.extract.rates(raw.rates = raw.testing,
                         sim=sim,
                         years=fn.years,
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
    
    do.extract.for.year.anchor(years=years,
                               year.anchor=year.anchor,
                               raw.year.is.start=T,
                               extract.fn=extract.fn,
                               keep.dimensions=keep.dimensions)
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


##-- SUB-FUNCTIONS THAT GENERICALLY ACCESS TRANSITIONS BASED ON MAPPINGS --##

do.extract.transition.by.label <- function(sim,
                                           label,
                                           scale,
                                           aggregate.on.scale=scale,
                                           group.by=c('from','to')[1],
                                           years=NULL,
                                           keep.dimensions='year',
                                           per.population=1,
                                           ages=NULL,
                                           races=NULL,
                                           subpopulations=NULL,
                                           sexes=NULL,
                                           risks=NULL,
                                           continuum=NULL,
                                           cd4=NULL,
                                           hiv.subsets=NULL,
                                           use.cdc.categorizations=F,
                                           year.anchor=c('start','mid','end')[3],
                                           throw.error.if.missing.years=T)
{
    # Pull the transitions for the label
    transition.mapping = get.sim.settings(sim)$transition.mapping
    transitions = get.transitions.by.labels(transition.mapping, labels=label)
    if (length(transitions)==0)
        stop(paste0("No transitions have been registered under the label '",
                    label, "'. Unable to extract transition."))
    
    do.extract.transition.from.transition.list(sim=sim,
                                               transitions=transitions,
                                               scale=scale,
                                               aggregate.on.scale=aggregate.on.scale,
                                               group.by=group.by,
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
                                               year.anchor=year.anchor,
                                               throw.error.if.missing.years=throw.error.if.missing.years
    )
}

do.extract.transition.by.to.from <- function(sim,
                                             dimension,
                                             subgroup,
                                             to,
                                             from,
                                             scale,
                                             aggregate.on.scale=scale,
                                             group.by=c('from','to')[1],
                                             years=NULL,
                                             keep.dimensions='year',
                                             per.population=1,
                                             ages=NULL,
                                             races=NULL,
                                             subpopulations=NULL,
                                             sexes=NULL,
                                             risks=NULL,
                                             continuum=NULL,
                                             cd4=NULL,
                                             hiv.subsets=NULL,
                                             use.cdc.categorizations=F,
                                             year.anchor=c('start','mid','end')[3],
                                             throw.error.if.missing.years=T)
{
    # Pull transitions by to-from
    transition.mapping = get.sim.settings(sim)$transition.mapping
    transitions = get.transitions.by.to.from(transition.mapping,
                                             dimension=dimension,
                                             subgroup=subgroup,
                                             to=to,
                                             from=from)
    
    if (length(transitions)==0)
        stop(paste0("No ", subgroup, " transitions have been registered from <",
                    paste0("'", from, "'", collapse=', '), 
                    "> to <",
                    paste0("'", to, "'", collapse=', '), 
                    " in the '", dimension,
                    "' dimension. Unable to extract transition."))
        
    do.extract.transition.from.transition.list(sim=sim,
                                               transitions=transitions,
                                               scale=scale,
                                               aggregate.on.scale=aggregate.on.scale,
                                               group.by=group.by,
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
                                               year.anchor=year.anchor,
                                               throw.error.if.missing.years=throw.error.if.missing.years
    )
}


do.extract.transition.from.transition.list <- function(sim,
                                                       transitions,
                                                       scale,
                                                       aggregate.on.scale=scale,
                                                       group.by=c('from','to')[1],
                                                       years=NULL,
                                                       keep.dimensions='year',
                                                       per.population=1,
                                                       ages=NULL,
                                                       races=NULL,
                                                       subpopulations=NULL,
                                                       sexes=NULL,
                                                       risks=NULL,
                                                       continuum=get.sim.settings(sim)$ENGAGED_STATES,
                                                       cd4=NULL,
                                                       hiv.subsets=NULL,
                                                       use.cdc.categorizations=F,
                                                       year.anchor=c('start','mid','end')[3],
                                                       throw.error.if.missing.years=T)
{
    # Check years argument
    years = check.postprocessing.years(sim,
                                       data.type='generic transition',
                                       years=years,
                                       year.anchor=year.anchor,
                                       throw.error=throw.error.if.missing.years)
    
    # Check that we are only looking at one dimension
    transition.dimension = unique(sapply(transitions, function(tr){tr$dimension}))
    if (length(transition.dimension)>1)
        stop(paste0("The label '", label, "' applies to transitions in more than one dimension (",
                    paste0("'", transition.dimension, "'", collapse=', '), 
                    "). Cannot extract transition across multiple dimensions"))
    
    # For now, only allow rates for a single subgroup (otherwise its going to get too tricky)
    subgroups = unique(sapply(transitions, function(tr){tr$subgroup}))
    if (length(subgroups)>1)
        stop("Unable to extract transition for '", label, 
             "' - for now, we can only extract transitions that span one subgroup")
    
    # A little prep work
    
    from.values = sapply(transitions, function(tr){tr$from})
    to.values = sapply(transitions, function(tr){tr$to})
    
    
    # check dimensions
    dim.values = list(
        ages=ages,
        races=races,
        subpopulations=subpopulations,
        sexes=sexes,
        risks=risks,
        continuum=continuum,
        cd4=cd4,
        hiv.subsets=hiv.subsets
    )
    
    if (group.by=='to')
        valid.dim.values = unique(to.values)
    else
        valid.dim.values = unique(from.values)
    
    if (is.null(dim.values[[transition.dimension]]))
        dim.values[[transition.dimension]] = valid.dim.values
    else if (length(setdiff(dim.values[[transition.dimension]], valid.dim.values))>0)
        stop(paste0("Invalid ", transition.dimension,
                    " value(s) for this transition: ",
                    paste0("'", setdiff(dim.values[[transition.dimension]], valid.dim.values), "'", collapse=', ')))
   
    extract.fn = function(fn.years)
    {
        # Pull the corresponding rates and times
        # This presupposes only one subgroup
        components = get.sim.components(sim)
        comps.names = get.transition.component.names(transition.dimension, subgroup=subgroups)
        if (is.null(components[[comps.names$name]]))
            components = do.setup.transitions(components, subgroup=subgroups, dimension=transition.dimension)
        
        full.values = components[[comps.names$name]]
        full.years = components[[comps.names$years.name]]
        
        if (length(setdiff(fn.years, full.years))==0) # we can do a quick subset and save ourselves some calculations
        {
            indices = sapply(fn.years, function(y){
                (1:length(full.years))[full.years==y]
            })
            full.values = full.values[indices]
        }
        else
            full.values = interpolate.parameters(values=full.values,
                                                 value.times = full.years,
                                                 desired.times = fn.years)
     
        # Pare down rates
        full.values = lapply(full.values, function(vv){
            pairwise.access.transitions(arr=vv,
                                        dimension=transition.dimension,
                                        from.values=from.values,
                                        to.values=to.values,
                                        allow.unoptimized=F,
                                        group.by=group.by)
        })
        
        
        # Convert to desired scale for aggregating
        full.values = convert.transition.element.type(full.values,
                                                      convert.from.type='rate',
                                                      convert.to.type=aggregate.on.scale)
        
        raw.rates = list(
            times = fn.years,
            rates = full.values
        )
        
        
        # do extract    
        do.extract.rates(raw.rates = raw.rates,
                         sim=sim,
                         years=fn.years,
                         population.years.offset = -1,
                         keep.dimensions=keep.dimensions,
                         per.population=per.population,
                         ages=dim.values$ages,
                         races=dim.values$races,
                         subpopulations=dim.values$subpopulations,
                         sexes=dim.values$sexes,
                         risks=dim.values$risks,
                         continuum=dim.values$continuum,
                         cd4=dim.values$cd4,
                         hiv.subsets=dim.values$hiv.subsets,
                         use.cdc.categorizations=use.cdc.categorizations,
                         include.hiv.negative = subgroups=='hiv.negative')
    }    
    
    do.extract.for.year.anchor(years=years,
                               year.anchor=year.anchor,
                               raw.year.is.start=T,
                               extract.fn=extract.fn,
                               keep.dimensions=keep.dimensions)
}

##-------------##
##-- HELPERS --##
##-------------##

check.postprocessing.years <- function(sim,
                                       data.type,
                                       years,
                                       year.anchor=NA,
                                       throw.error=T)
{
    years.available = do.get.years.available.for.sim.data(sim,
                                                          year.anchor=year.anchor)
    if (is.null(years))
        years = years.available
    
    missing.years = setdiff(years,years.available)
    if (length(missing.years)>0)
    {
        if (throw.error)
        {
            msg = paste0("The simulation does not have '", data.type, 
                         " 'data available for ",
                         paste0(missing.years, collapse=', '))
            if (!is.na(year.anchor))
                msg = paste0(msg, " for year.anchor = '", year.anchor, "'")
            
            stop(msg)
        }
        else
            intersect(years, years.available)
    }
    else
        years
}

do.get.years.available.for.sim.data <- function(sim,
                                                year.anchor)
{
    sim.years = sim$years
    
    years.available.for.anchor.end = sim.years
    
    if (is.na(year.anchor) || year.anchor=='end')
        years.available.for.anchor.end
    else if (year.anchor=='start')
        years.available.for.anchor.end+1
    else #year.anchor == 'mid'
        years.available.for.anchor.end[-1]
}

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
    {
        invalid.sexes = setdiff(sexes, CDC.SEXES)
        if (length(invalid.sexes)>0)
            stop(paste0(paste0("'", invalid.sexes, "' ", collapse=', '), " ", 
                        ifelse(length(invalid.sexes)==1, 'is not a valid value', 'are not valid values'),
                        " for 'sexes' - must be one of: ",
                        paste0("'", CDC.SEXES, "'", collapse=', ')))
      #  sexes = intersect(sexes, CDC.SEXES)
    }
    
    if (any(keep.dimensions=='sex'))
        dim.names[['sex']] = intersect(CDC.SEXES, sexes)
    else
        dim.names = dim.names[names(dim.names)!='sex']
    
    if (is.null(risks))
        risks = CDC.RISKS
    else
    {
        invalid.risks = setdiff(risks, CDC.RISKS)
        if (length(invalid.risks)>0)
            stop(paste0(paste0("'", invalid.risks, "' ", collapse=', '), " ", 
                        ifelse(length(invalid.risks)==1, 'is not a valid value', 'are not valid values'),
                        " for 'risks' - must be one of: ",
                        paste0("'", CDC.RISKS, "'", collapse=', ')))
      #  risks = intersect(risks, CDC.RISKS)
    }
    
    if (any(keep.dimensions=='risk'))
        dim.names[['risk']] = intersect(CDC.RISKS, risks)
    else
        dim.names = dim.names[names(dim.names)!='risk']
    
 #   if (any(sapply(sexes, function(sex){all(sex!=CDC.SEXES)})))
  #      stop(paste0("subscript out of bounds. sexes must be a subset of ",
   #                 paste0("'", CDC.SEXES, "'", collapse=', ')))
    #if (any(sapply(risks, function(risk){all(risk!=CDC.RISKS)})))
     #   stop(paste0("subscript out of bounds. risks must be a subset of ",
      #              paste0("'", CDC.RISKS, "'", collapse=', ')))
    
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
    else if (all(keep.dimensions!='sex')) #risk only
    {
        if (any(sexes=='male'))
        {
            if (any(risks=='msm'))
            {
                if (any(names(dimnames(rv))=='risk'))
                    access(rv, risk='msm') = access(arr, sex='msm', risk='never_IDU')
                else
                    rv = access(arr, sex='msm', risk='never_IDU')
            }
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