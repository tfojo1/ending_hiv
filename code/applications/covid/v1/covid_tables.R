
if (1==2)
{
    x = prepare.timeline.df(aggregate.locations = F,
                            subtract.scenario = NA,
                            outcomes ='incidence',
                            years=c(2021,2025))
    
    x = x[order(x$location, x$scenario),]
    
    x = quick.covid.table(outcome='new')
    
    x = make.covid.summary.table()
    x = x[order(x$location, x$scenario, x$year),]
    write.csv(x, 'code/covid/results/summary_table.csv')
    
}

quick.covid.table <- function(outcome='incidence',
                              years=c(2021,2025))
{
    x = prepare.timeline.df(aggregate.locations = F,
                            subtract.scenario = NA,
                            outcomes=outcome,
                            years=years)
    
    x = rbind(x,
              prepare.timeline.df(aggregate.locations = T,
                                  subtract.scenario = NA,
                                  outcomes=outcome,
                                  years=years))
    
    
    x = x[order(x$location, x$scenario),]
    
    x$outcome = COVID.OUTCOME.NAMES[as.character(x$outcome)]
   
    
    names(x) = c("Location",
                 "Scenario",
                 "Intervention",
                 "Year",
                 "Outcome",
                 "Mean",
                 "CI_95_lower",
                 "CI_95_upper",
                 "CI_50_lower",
                 "CI_50_upper") 
    x = make.covid.summary.table()
}

make.covid.summary.table <- function(results=outcomes.arr,
                                     params=parameters,
                                     variables=c('incidence','new','prevalence','suppression','diagnosed'),
                                     scenarios = dimnames(outcomes.arr)[['scenario']],
                                     intervention.names = NA,
                                     locations = dimnames(outcomes.arr)[['location']],
                                     loc.names = location.names,
                                     aggregate.locations=F,
                                     years=c(2021,2025),
                                     summary.stat='mean',
                                     interval.coverage=0.95)
{
    summary.stat.fn = get(summary.stat)
    alpha = (1-interval.coverage)/2
    
    rv = NULL
     
    for (scenario in scenarios)
    {
        for (int.name in intervention.names)
        {   
            for (year in years)
            {  
                if (is.na(int.name))
                    name.for.int = 'none'
                else
                    name.for.int = secondary.intervention.name(int.name)
                
                if (aggregate.locations)
                    year.df = data.frame(
                        location='Total',
                        year=year,
                        scenario=COVID.SCENARIO.NAMES[scenario],
                        intervention.names=name.for.int)
                else
                    year.df = data.frame(
                        location=loc.names[locations],
                        year=year,
                        scenario=COVID.SCENARIO.NAMES[scenario],
                        intervention.names=name.for.int
                    )
                
                for (variable in variables)
                {
                    values = get.variable(results=results, params=params, 
                                          var.name=variable,
                                          years = year, 
                                          scenario = scenario,
                                          intervention.name = int.name)
                    
                    sub.df = data.frame(
                        est=apply(values, 'location', summary.stat.fn),
                        ci.lower=apply(values, 'location', quantile, probs=alpha),
                        ci.upper=apply(values, 'location', quantile, probs=1-alpha)
                    )
                    
                    names(sub.df) = paste0(COVID.OUTCOME.NAMES[variable], ": ",
                                           c(summary.stat, 
                                             paste0(round(100*interval.coverage), "_", c("lower","upper"))))
                    
                    year.df = cbind(year.df, sub.df)
                }
                
                
                rv = rbind(rv, year.df)
            }
        }
        
    }
    
    rv
}