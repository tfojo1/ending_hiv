
#source('code/visualization/plot_wrappers.R')
#source('code/targets/target_msas.R')
#source('code/covid/prepare_and_run_covid_sims.R')

library(ggsci)

if (1==2)
{
    memory.limit(64000)
    baltimore.covid.simsets = load.covid.simsets(BALTIMORE.MSA)
    make.covid.timeline.plot(baltimore.covid.simsets)
}


COVID.SCENARIO.NAMES = c(
    baseline = 'Absent COVID',
    noint = "Absent COVID",
    base = "COVID: Base Case",
    delayed.hiv.care = "COVID: Delayed Resumption of HIV Screening, Care, and PrEP",
    rebound.sexual.transmission = "COVID: Rebound Increase in Sexual Transmission",
    rebound.sex.delayed.hiv.care = "COVID: Rebound Sexual Transmission and Delayed HIV Care"
)

COVID.OUTCOME.NAMES = OUTCOME.LABELS

make.covid.timeline.plot <- function(df,
                                     outcomes='incidence',
                                     scenarios=c('base','delayed.hiv.care','rebound.sexual.transmission','rebound.sex.delayed.hiv.care'),
                                     intervention.names=c(NA, 'testing.75.6.6')[1],
                                     include.baseline=T,
                                     locations=unique(df$msa),
                                     years=2010:2025,
                                     cumulative=F,
                                     interval.coverage=0.95,
                                     summary.stat=mean,
                                     aggregate.locations = F,
                                     #style arguments
                                     palette=pal_jama(),
                                     ribbon.alpha=0.2,
                                     line.size=2,
                                     fixed.scale=F)
{
    suppressWarnings({
    if (include.baseline)
        scenario.names = 'baseline'
    else
        scenario.names = character()
    
    n.scenarios = length(scenarios)
    n.interventions = length(intervention.names)
    scenarios = rep(scenarios, each=n.interventions)
    intervention.names = rep(intervention.names, n.scenarios)
    
    scenario.names = c(scenario.names, paste0("COVID.", scenarios))
    if (include.baseline)
    {
        scenarios = c('noint', scenarios)
        intervention.names = c(NA, intervention.names)
    }
    scenario.names[!is.na(intervention.names)] = paste0(scenario.names[!is.na(intervention.names)],
                                                        ".", intervention.names[!is.na(intervention.names)])
    alpha = (1-interval.coverage)/2
    
    ##-- MAKE THE DATA FRAME --##
    plot.df = NULL
    for (outcome in outcomes)
    {
        for (sc.index in 1:length(scenario.names))
        {
            sc = scenario.names[sc.index]
           
            
            scenario.plus.intervention.name = COVID.SCENARIO.NAMES[scenarios[sc.index]]
            if (!is.na(intervention.names[sc.index]))
                scenario.plus.intervention.name = paste0(scenario.plus.intervention.name,
                                                         ", ",
                                                         secondary.intervention.name(intervention.names[sc.index]))
            
            col.names = paste0(sc, "_", outcome, "_", years)
            year.present = sapply(col.names, function(cn){any(cn==names(df))})
            
            col.names = col.names[year.present]
            sc.years = years[year.present]
            
            
            if (length(col.names)>0)
            {
                aggregated.values = 0
                for (location in locations)
                {
                    location.mask = df$msa == location
                    location.name = as.character(df$msa.name[location.mask][1])
                    
                    values = as.matrix(df[location.mask, col.names])
                    dim(values) = c(length(values)/length(col.names), length(col.names))
                    if (cumulative)
                        values = t(apply(values, 1, cumsum))
                    aggregated.values = values + aggregated.values
                    
                    if (!aggregate.locations)
                    {
                        estimate = apply(values, 2, summary.stat)
                        ci.lower = apply(values, 2, quantile, probs=alpha)
                        ci.upper = apply(values, 2, quantile, probs=1-alpha)
                        
                        plot.df = rbind(plot.df,
                                   data.frame(year=sc.years,
                                              estimate=estimate,
                                              ci.lower=ci.lower,
                                              ci.upper=ci.upper,
                                              location=location.name,
                                              outcome=COVID.OUTCOME.NAMES[outcome],
                                              scenario=scenario.plus.intervention.name
                                   ))
                        
                        
                    }
                }
                
                if (aggregate.locations)
                {
                    estimate = apply(aggregated.values, 2, summary.stat)
                    ci.lower = apply(aggregated.values, 2, quantile, probs=alpha)
                    ci.upper = apply(aggregated.values, 2, quantile, probs=1-alpha)
                    
                    plot.df = rbind(plot.df,
                               data.frame(year=sc.years,
                                          estimate=estimate,
                                          ci.lower=ci.lower,
                                          ci.upper=ci.upper,
                                          location='All',
                                          outcome=COVID.OUTCOME.NAMES[outcome],
                                          scenario=scenario.plus.intervention.name
                               ))
                }
                
            }
            
        }
    }
    
    ##-- MAKE THE PLOT --##
    colors = palette(length(scenarios))
    names(colors) = unique(df$scenario) #COVID.SCENARIO.NAMES[scenarios]
    
    
    rv = ggplot(plot.df) + 
        geom_ribbon(aes(year, ymin=ci.lower, ymax=ci.upper, fill=scenario),
                    color=NA, alpha=ribbon.alpha, line.size=0) + 
        geom_line(aes(year, estimate, color=scenario), size=line.size) +
        xlab("Year") +
        scale_color_manual(values=colors, name='Scenario') +
        scale_fill_manual(values=colors, name='Scenario') + 
        scale_y_continuous(labels=function(y){format(y, big.mark=',')})
    
    if (length(outcomes)==1)
    {
        rv = rv + ylab(COVID.OUTCOME.NAMES[outcomes])
        
        if (!aggregate.locations)
        {
            if (fixed.scale)
                rv = rv + facet_wrap(~location, scales='fixed')
            else
                rv = rv + facet_wrap(~location, scales='free_y')
        }
    }
    else
    {
        rv = rv + ylab("Outcome")
        
        if (aggregate.locations)
            rv = rv + facet_wrap(~outcome, scales='free_y')
        else
            rv = rv + facet_wrap(~location + outcome, scales='free_y')
    }
    
    rv = rv + theme(legend.position = 'bottom', legend.direction = 'vertical')
    
    rv
    })
}

