library(jheem)

#testing
if (1==2)
{
    intervention = create.one.intervention("All MSM or IDU",
                                  testing.frequency = 1,
                                  suppressed.proportion = 1,
                                  prep.coverage = .5,
                                  intervention.ramped.up.year = 2021
                                  )

    test = setup.components.for.intervention(components, intervention)

    source('../code/testing_calibration/manual_calibrating_for_calibrated_6.R')
    sim1 = run.jheem.from.components(components, end.year=2030)
    sim2 = run.jheem.from.components(test, end.year=2030)

    #-- PLOTS --#
    plot.calibration.risk(list(sim1,sim2), years=2010:2030)
}



setup.components.for.intervention <- function(components,
                                              intervention)
{
    if (is.null(intervention))
        return (components)

    # Testing
    if (!all(sapply(intervention$testing.frequency, function(x){all(is.na(x))})))
    {
        testing.rates = lapply(1:length(intervention$testing.years), function(y)
        {
            testing.mat = get.general.population.skeleton(components$jheem,
                                                          value=as.numeric(NA))
            for (j in 1:length(intervention$target.populations))
            {
                t.pop = intervention$target.populations[[j]]

#                print(paste0('Testing, j=', j, ', y=', y))
#                print(t.pop)
                testing.mat[t.pop$ages, t.pop$races, t.pop$subpopulations, t.pop$sexes, t.pop$risks] =
                    1 / intervention$testing.frequency[[j]][y]
            }

            testing.mat
        })

        components = set.foreground.hiv.testing.rates(components,
                                                      testing.rates = testing.rates,
                                                      years = intervention$testing.years)
    }

    # Suppression
    if (!all(sapply(intervention$suppressed.proportion, function(x){all(is.na(x))})))
    {
        suppressed.proportions = lapply(1:length(intervention$suppressed.years), function(y)
        {
            suppressed.mat = get.hiv.positive.population.skeleton(components$jheem,
                                                                  value=as.numeric(NA))
            for (j in 1:length(intervention$target.populations))
            {
                t.pop = intervention$target.populations[[j]]
#                print(paste0('Suppressed, j=', j, ', y=', y))
#                print(t.pop)
                suppressed.mat[t.pop$ages, t.pop$races, t.pop$subpopulations, t.pop$sexes, t.pop$risks,,,] =
                    intervention$suppressed.proportion[[j]][y]
            }

            suppressed.mat
        })

        components = set.foreground.suppression(components,
                                                suppressed.proportions = suppressed.proportions,
                                                years = intervention$suppressed.years)
    }

    # PrEP
    if (!all(sapply(intervention$prep.coverage, function(x){all(is.na(x))})))
    {
        prep.coverages = lapply(1:length(intervention$prep.years), function(y)
        {
            prep.mat = get.general.population.skeleton(components$jheem,
                                                       value=as.numeric(NA))

            for (j in 1:length(intervention$target.populations))
            {
                t.pop = intervention$target.populations[[j]]
#                print(paste0('Prep Coverage, j=', j, ', y=', y))
#                print(t.pop)
                prep.mat[t.pop$ages, t.pop$races, t.pop$subpopulations, t.pop$sexes, t.pop$risks] =
                    intervention$prep.coverage[[j]][y]
            }

            prep.mat
        })

        components = set.foreground.prep.coverage(components,
                                                  prep.coverage = prep.coverages,
                                                  years = intervention$prep.years)
    }
    
    components
}

create.multiple.intervention.for.targets <- function()
{

}

create.all.intervention.combinations <- function(target.populations,
                                                 testing.frequencies,
                                                 suppressed.proportions,
                                                 prep.coverages,
                                                 intervention.ramped.up.year=2022,
                                                 settings=SETTINGS)
{
    #Iterate all combinations
   
    target.populations = rep(target.populations, each = length(testing.frequencies))
    testing.frequencies = rep(testing.frequencies, length(target.populations)/length(testing.frequencies))
    
    target.populations = rep(target.populations, each=length(suppressed.proportions))
    testing.frequencies = rep(testing.frequencies, each=length(suppressed.proportions))
    suppressed.proportions = rep(suppressed.proportions, length(target.populations)/length(suppressed.proportions))
   
    
     
    target.populations = rep(target.populations, each=length(prep.coverages))
    testing.frequencies = rep(testing.frequencies, each=length(prep.coverages))
    suppressed.proportions = rep(suppressed.proportions, each=length(prep.coverages))
    prep.coverages = rep(prep.coverages, length(target.populations)/length(prep.coverages))
    
    rv = lapply(1:length(target.populations), function(i){
        create.one.intervention(target.populations = target.populations[i],
                                testing.frequency = testing.frequencies[i],
                                suppressed.proportion = suppressed.proportions[i],
                                prep.coverage = prep.coverages[i],
                                intervention.ramped.up.year = intervention.ramped.up.year,
                                settings=settings)
    })
    
    rv
}

create.one.intervention <- function(target.populations,
                                    testing.frequency,
                                    suppressed.proportion,
                                    prep.coverage,
                                    intervention.ramped.up.year=2022,
                                    settings=SETTINGS)

{
    if (length(testing.frequency)>1 ||
        length(suppressed.proportion)>1 ||
        length(prep.coverage)>1)
        stop("The interventions (testing, suppression, PrEP) can only have one value")

    rv = list(target.populations=list())

    for (t.pop in target.populations)
    {
        rv$target.populations = c(rv$target.populations,
                                  parse.target.populations(t.pop, settings=settings))
    }

    rv$target.description = paste0(target.populations, collapse=', ')

    rv$n.groups = length(rv$target.populations)

    rv$prep.coverage = lapply(1:rv$n.groups, function(i){prep.coverage})
    rv$testing.frequency = lapply(1:rv$n.groups, function(i){testing.frequency})
    rv$suppressed.proportion = lapply(1:rv$n.groups, function(i){suppressed.proportion})

    rv$prep.years = rv$testing.years = rv$suppressed.years = intervention.ramped.up.year

    rv$name = ''
    if (!is.na(testing.frequency))
    {
        if (rv$name != '')
            rv$name = paste0(rv$name, ", ")
        rv$name = paste0(rv$name, "testing ", 1/testing.frequency, "/yr")
    }

    if (!is.na(suppressed.proportion))
    {
        if (rv$name != '')
            rv$name = paste0(rv$name, ", ")
        rv$name = paste0(rv$name, suppressed.proportion, " suppressed")
    }

    if (!is.na(prep.coverage))
    {
        if (rv$name != '')
            rv$name = paste0(rv$name, ", ")
        rv$name = paste0(rv$name, prep.coverage, " PrEP")
    }

    rv$name = paste0(target.populations, ": ", rv$name)
    
    rv
}

make.target.population <- function(races='all',
                                   sexes='all',
                                   ages='all',
                                   idu=F,
                                   settings=SETTINGS
)
{
    if (length(races)==0 || any(races=='all'))
        races = settings$RACES
    if (length(ages)==0 || any(ages=='all'))
        ages = as.integer(1:length(settings$AGES$labels))
    if (length(sexes)==0 || any(sexes=='all'))
        sexes = settings$SEXES

    rv = list(races=races,
              sexes=sexes,
              ages=ages)

    if (idu)
        rv$risks = c('active_IDU', 'IDU_in_remission')
    else
        rv$risks = settings$RISK_STRATA

    #for now
    rv$subpopulations = 1

    rv
}

parse.target.populations <- function(str, settings=SETTINGS)
{
    races = character()
    ages = integer()
    sexes = character()
    risks = character()

    #-- Race --#
    if (grepl('black', str, ignore.case = T))
        races = c(races, 'black')
    if (grepl('hispanic', str, ignore.case = T))
        races = c(races, 'hispanic')
    if (grepl('other', str, ignore.case = T))
        races = c(races, 'other')

    #-- Ages --#
    if (grepl('young', str, ignore.case = T) || grepl('<35', str))
        ages = c(ages, 1:2)

    #-- Sex/Risk --#
    if (grepl('msm', str, ignore.case = T))
        sexes = c(sexes, 'msm')

    if (grepl('idu', str, ignore.case = T))
    {
        risks = c(risks, 'active_IDU', 'IDU_in_remission')
        if (grepl('[+/-_]idu', str, ignore.case=T) ||
            grepl('idu[+/-_]', str, ignore.case=T))
        {
            list(make.target.population(races=races, ages=ages, sexes=sexes, idu=T,
                                        settings=settings))
        }
        else
        {
            list(make.target.population(races=races, ages=ages, sexes=sexes, idu=F,
                                        settings=settings),
                 make.target.population(races=races, ages=ages, sexes='all', idu=T,
                                        settings=settings))
        }
    }
    else
        list(make.target.population(races=races, ages=ages, sexes=sexes, idu=F,
                                    settings=settings))

}

get.intervention.filename <- function(intervention)
{
    if (is.null(intervention))
        "No_Intervention"
    else
    {
        filename = gsub(" ", "_", intervention$name)
        filename = gsub(":", "", filename)
        filename = gsub("/", "p", filename)
        filename = gsub(",", "", filename)
        filename = gsub("<", "lt", filename)
    
        filename
    }
}
