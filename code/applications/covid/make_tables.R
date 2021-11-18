
if (1==2)
{
    load('results/covid/covid_4.1_results.Rdata')

    
}

make.supplement.table <- function(outcomes.arr,
                                  outcome='incidence',
                                  locations=dimnames(outcomes.arr)$location,
                                  years=c(2020,2021,2025),
                                  scenarios=c('Absent COVID' = 'baseline',
                                              'Prolonged Barriers to Care'='delayed.hiv.care',
                                              'Rapid Resumption of Care' = 'base'),
                                  ci.coverage=0.95,
                                  digits=0,
                                  pct=outcome=='suppression' || outcome=='diagnosed')
{
    alpha = (1-ci.coverage)/2
    
    data = lapply(scenarios, get.variable,
                  results=outcomes.arr,
                  params=parameters,
                  var.name=outcome,
                  years=years,
                  intervention.name=NA,
                  locations=locations,
                  aggregate.locations=F,
                  aggregate.years=F)
    
    total.data = lapply(scenarios, get.variable,
                        results=outcomes.arr,
                        params=parameters,
                        var.name=outcome,
                        years=years,
                        intervention.name=NA,
                        locations=locations,
                        aggregate.locations=T,
                        aggregate.years=F)
    data = lapply(1:length(data), function(i){
        dd = data[[i]]
        dim.names = dimnames(dd)
        dim.names$location = c(dim.names$location, 'Total')
        rv = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
        rv[locations,,] = dd
        rv['Total',,] = total.data[[i]]
        
        rv
    })
    
    names(data) = scenarios
    
    
    if (pct)
    {
        mult = 100
        unit = '%'
    }
    else
    {
        mult = 1
        unit = ''
    }
    
    rv = sapply(years, function(year){
        sapply(scenarios, function(scenario){
            values = data[[scenario]][,as.character(year),]
            est = rowMeans(values)
            ci.lower = apply(values, 'location', quantile, probs=alpha)
            ci.upper = apply(values, 'location', quantile, probs=1-alpha)
            
#            paste0(year, ' - ', scenario, ', ', locations)
            paste0(format(round(mult*est, digits=digits), big.mark=','), 
                   unit, " [", 
                   format(round(mult*ci.lower, digits=digits), big.mark=','),
                   " to ",
                   format(round(mult*ci.upper, digits=digits), big.mark=','),
                   unit, "]")
        })
    })
    
    dim(rv) = c(location=length(locations)+1,
                col=length(years)*length(scenarios))
    
    rv = data.frame(Location=c(location.names[locations], 'Total'),
                        as.data.frame(rv))
    
    
    if (is.null(names(scenarios)))
        names(scenarios) = scenarios
    
    dimnames(rv)[[2]] = c('Location', 
               paste0(rep(names(scenarios), length(years)),
                    ", ",
                    rep(years, each=length(scenarios))))
    
    rv
}

source('../../commoncode/latex_helpers.R')
if (1==2)
{
    # Incidence
    tab = make.supplement.table(outcomes.arr=outcomes.arr,
                                years=2020,
                                outcome='incidence')
    make.latex.table(tab, file = '../Manuscripts/covid_manuscript/supplement/tables/inc_tab_2020.txt')
    
    
    tab = make.supplement.table(outcomes.arr=outcomes.arr,
                                years=2021,
                                outcome='incidence')
    make.latex.table(tab, file = '../Manuscripts/covid_manuscript/supplement/tables/inc_tab_2021.txt')
    
    
    tab = make.supplement.table(outcomes.arr=outcomes.arr,
                                years=2025,
                                outcome='incidence')
    make.latex.table(tab, file = '../Manuscripts/covid_manuscript/supplement/tables/inc_tab_2025.txt')
    
    
    # Reported Diagnoses
    tab = make.supplement.table(outcomes.arr=outcomes.arr,
                                years=2020,
                                outcome='new')
    make.latex.table(tab, file = '../Manuscripts/covid_manuscript/supplement/tables/new_tab_2020.txt')
    
    
    tab = make.supplement.table(outcomes.arr=outcomes.arr,
                                years=2021,
                                outcome='new')
    make.latex.table(tab, file = '../Manuscripts/covid_manuscript/supplement/tables/new_tab_2021.txt')
    
    
    tab = make.supplement.table(outcomes.arr=outcomes.arr,
                                years=2025,
                                outcome='new')
    make.latex.table(tab, file = '../Manuscripts/covid_manuscript/supplement/tables/new_tab_2025.txt')
    
    
    # Prevalence
    tab = make.supplement.table(outcomes.arr=outcomes.arr,
                                years=2020,
                                outcome='prevalence')
    make.latex.table(tab, file = '../Manuscripts/covid_manuscript/supplement/tables/prev_tab_2020.txt')
    
    
    tab = make.supplement.table(outcomes.arr=outcomes.arr,
                                years=2021,
                                outcome='prevalence')
    make.latex.table(tab, file = '../Manuscripts/covid_manuscript/supplement/tables/prev_tab_2021.txt')
    
    
    tab = make.supplement.table(outcomes.arr=outcomes.arr,
                                years=2025,
                                outcome='prevalence')
    make.latex.table(tab, file = '../Manuscripts/covid_manuscript/supplement/tables/prev_tab_2025.txt')
    
    
    # Awareness
    tab = make.supplement.table(outcomes.arr=outcomes.arr,
                                years=2020,
                                outcome='diagnosed')
    make.latex.table(tab, file = '../Manuscripts/covid_manuscript/supplement/tables/dx_tab_2020.txt')
    
    
    tab = make.supplement.table(outcomes.arr=outcomes.arr,
                                years=2021,
                                outcome='diagnosed')
    make.latex.table(tab, file = '../Manuscripts/covid_manuscript/supplement/tables/dx_tab_2021.txt')
    
    
    tab = make.supplement.table(outcomes.arr=outcomes.arr,
                                years=2025,
                                outcome='diagnosed')
    make.latex.table(tab, file = '../Manuscripts/covid_manuscript/supplement/tables/dx_tab_2025.txt')
 
    # Suppression
    tab = make.supplement.table(outcomes.arr=outcomes.arr,
                                years=2020,
                                outcome='suppression')
    make.latex.table(tab, file = '../Manuscripts/covid_manuscript/supplement/tables/supp_tab_2020.txt')
    
    
    tab = make.supplement.table(outcomes.arr=outcomes.arr,
                                years=2021,
                                outcome='suppression')
    make.latex.table(tab, file = '../Manuscripts/covid_manuscript/supplement/tables/supp_tab_2021.txt')
    
    
    tab = make.supplement.table(outcomes.arr=outcomes.arr,
                                years=2025,
                                outcome='suppression')
    make.latex.table(tab, file = '../Manuscripts/covid_manuscript/supplement/tables/supp_tab_2025.txt')
    
}