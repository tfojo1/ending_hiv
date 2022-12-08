source('code/applications/laart/do_files/do_prepare_simsets_for_laart.R')
source('code/source_code.R')
SRC.DIRECTORY = file.path(SIMULATIONS.DIR, 'baseline_quick_expanded')
DST.DIRECTORY = file.path(SIMULATIONS.DIR, 'baseline_quick_laart')

source('code/processing/visualization/sim_plots.R')
source('code/settings_and_files/setup_versions.R')
source('code/applications/laart/laart_jheem_settings.R')
source('code/applications/laart/laart_parameters.R')
source('code/applications/laart/laart_parameter_mapping.R')
source('code/applications/laart/laart_summary_functions.R')
source('code/applications/laart/laart_interventionsv2.R')
library('ramify')

load("atlanta.RData")

intervention_code_dictionary <-c(
  "noint"=1, 
  "durable.laart.25"=2, 
  "durable.laart.50"=3,
  "unsuppressed.laart.25" = 4,
  "unsuppressed.laart.50" = 5,
  "naive.laart.25" = 6,
  "naive.laart.50" = 7,
  "everything.laart.25" = 8,
  "everything.laart.50" = 9
)

### OBTAIN TOTAL % AND CI OF PEOPLE IN TREATMENT TYPES###
get.proportion.by.treatment.group<-function(all_city_simset)
{
  sapply(all_city_simset, function(simset){
    columns = c("2027_laart_percentage","2035_laart_percentage") 
    df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
    colnames(df) = columns
    for (i in 1:simset@n.sim){
      df[nrow(df) + 1,] = c(round(100*t(sapply(as.character(2024:2035), function(year){
        rowMeans(sapply(simset@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
      })),1)['2027', 'laart'], round(100*t(sapply(as.character(2024:2035), function(year){
        rowMeans(sapply(simset@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
      })),1)['2035', 'laart'])
    }
    df
  })
}
proportion.by.treatment = get.proportion.by.treatment.group(allsimsets)
print(paste0("25% Durable Intervention mean people on laart in 2027: ", mean(proportion.by.treatment[[3]])))
print(paste0("25% Durable Intervention CI people on laart in 2027: ", list(quantile(proportion.by.treatment[[3]], probs=c(0.025, 0.975)))))

print(paste0("25% Durable Intervention mean people on laart in 2035: ", mean(proportion.by.treatment[[4]])))
print(paste0("25% Durable Intervention CI people on laart in 2035: ", list(quantile(proportion.by.treatment[[4]], probs=c(0.025, 0.975)))))

print(paste0("50% Durable Intervention mean people on laart in 2027: ", mean(proportion.by.treatment[[5]])))
print(paste0("50% Durable Intervention CI people on laart in 2027: ", list(quantile(proportion.by.treatment[[5]], probs=c(0.025, 0.975)))))

print(paste0("50% Durable Intervention mean people on laart in 2035: ", mean(proportion.by.treatment[[6]])))
print(paste0("50% Durable Intervention CI people on laart in 2035: ", list(quantile(proportion.by.treatment[[6]], probs=c(0.025, 0.975)))))


###PLOT INTERVENTION EFFECTS###
get.incident.per.simulation.all.interventions<-function(all_city_simset)
{
  cityinc = sapply(allsimsets, function(simset){
    temp = sapply(simset@simulations, function(sim){
      inc = project.absolute.incidence(sim, years = c(2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033, 2034, 2035))
    })
  })
}

get.df.mean.inc.per.year.for.intervention<-function(all_city_simset, intervention_code)
{
  cityinc = get.incident.per.simulation.all.interventions(all_city_simset)
  interventioninc = resize(cityinc[,intervention_code_dictionary[intervention_code]], nrow = 11, ncol = 100)
  rownames(interventioninc)<-c('2025','2026', '2027', '2028', '2029', '2030', '2031', '2032', '2033', '2034', '2035')
  df = data.frame(t(interventioninc), check.names = FALSE)
  df<-tibble::rowid_to_column(df, 'Sim')
  mdf<-melt(df, id.vars='Sim', value.name="value", variable.name="Year")
  Year<- c('2025','2026', '2027', '2028', '2029', '2030', '2031', '2032', '2033', '2034', '2035')
  Mean_Inc<-rowMeans(interventioninc)
  SD<-apply(interventioninc, 1, sd)
  intdf<-data.frame(Year, Mean_Inc, SD)
}

nointdf = get.df.mean.inc.per.year.for.intervention(allsimsets, 'noint')
durable50intdf = get.df.mean.inc.per.year.for.intervention(allsimsets, 'durable.laart.50')

ggplot(data=durable50intdf) + 
  geom_ribbon(data=durable50intdf, aes(x=Year, ymin=Mean_Inc-SD, ymax = Mean_Inc+SD, group = 1), fill = 'blue', alpha = 0.2) +
  geom_ribbon(data = nointdf, aes(x=Year, ymin=Mean_Inc-SD, ymax = Mean_Inc+SD, group = 1), fill = 'red', alpha = 0.2) +
  geom_line(data=durable50intdf, aes(x=Year, y=Mean_Inc, group = 1, color = 'Durable 50% \nIntervention')) + 
  geom_line(data = nointdf, aes(x=Year, y=Mean_Inc, group = 1, color = 'No Intervention')) + 
  scale_color_manual(name='Intervention',
                     breaks=c('No Intervention', 'Durable 50% \nIntervention'),
                     values=c('No Intervention'='red', 'Durable 50% \nIntervention'='blue')) + 
  labs(x = "Year", y = "Model Estimated Incidence")  +
  coord_cartesian(ylim = c(0, NA)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(legend.position = c(0.8,0.8)) + 
  theme(legend.title=element_text(size=10),legend.text=element_text(size=8)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

###SENSITIVITY ANALYSIS###
get.percentage.point.incidence.reduction <-function(all_city_simsets, start_year=2025, end_year=2035)
{
  inc_red = sapply(all_city_simsets, function(simset){
    sapply(simset@simulations, function(sim){
      inc = project.absolute.incidence(sim, years = c(start_year, end_year))
      (inc[1]-inc[2])/inc[1]
    })
  })
  pp_inc_red = inc_red-inc_red[,1]
}

plot.parameter.sensitivity.per.intervention <-function(all_city_simsets, param_name, intervention_code, x_axis_name, start_year=2025, end_year=2035)
{
  pp_inc_red = get.percentage.point.incidence.reduction(all_city_simsets, start_year, end_year)
  simset = all_city_simsets[[intervention_code_dictionary[intervention_code]]]
  df<- data.frame(par = simset@parameters[, param_name], inc = pp_inc_red[,intervention_code_dictionary[intervention_code]])
  ggplot(df, aes(x = par, y = inc)) + 
    annotate("text",  x=Inf, y = Inf, label = paste("R²= ", round(cor(simset@parameters[, param_name], pp_inc_red[,intervention_code_dictionary[intervention_code]]), 2)), vjust=1, hjust=1, size = 15) + 
    geom_point() + 
    labs(x = x_axis_name, y = "Estimated Incidence Reduction") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme(legend.position = c(0.8,0.8)) + 
    theme(legend.title=element_text(size=10),legend.text=element_text(size=8)) 
}
plot.parameter.sensitivity.per.intervention(all_city_simsets = allsimsets, param_name = "laart.versus.oral.disengagement.rr", 
                                            intervention_code = "durable.laart.50", x_axis_name = "Risk Ratio of Care Disengagement using LAART versus Oral ART")
plot.parameter.sensitivity.per.intervention(all_city_simsets = allsimsets, param_name = "laart.versus.oral.disengagement.rr", 
                                            intervention_code = "naive.laart.50", x_axis_name = "Risk Ratio of Care Disengagement using LAART versus Oral ART")
plot.parameter.sensitivity.per.intervention(all_city_simsets = allsimsets, param_name = "laart.suppressed.to.resistant.unsuppressed", 
                                            intervention_code = "durable.laart.50", x_axis_name = "Proportion of Durably Suppressed Individuals on LAART\n Developing Resistance")
plot.parameter.sensitivity.per.intervention(all_city_simsets = allsimsets, param_name = "laart.suppressed.to.resistant.unsuppressed", 
                                            intervention_code = "naive.laart.50", x_axis_name = "Proportion of Durably Suppressed Individuals on LAART\n Developing Resistance")


get.correlation.coefficient.per.parameter<- function(all_city_simsets, start_year=2025, end_year=2035){
  pp_inc_red = get.percentage.point.incidence.reduction(all_city_simsets, start_year, end_year)
  correlations = 
    sapply(LAART.PARAMETER.DISTRIBUTION@var.names, function(varname){
      sapply(all_city_simsets, function(simset){
        cor(pp_inc_red, simset@parameters[, varname]) 
      })
    })
  correlations = correlations[1:9,]
  array(c(correlations[2,], correlations[3,], correlations[4,], correlations[5,], correlations[6,], correlations[7,], correlations[8,], correlations[9,]), dim=c(13,8), list(LAART.PARAMETER.DISTRIBUTION@var.names, c('25% Durable', '50% Durable', '25% Unsuppressed', '50% Unsuppressed', '25% Naive', '50% Naive', '25% Combined', '50% Combined')))
}
parameter.correlations = get.correlation.coefficient.per.parameter(allsimsets)
print(parameter.correlations)


get.correlation.coefficient.per.2035.treatment.type.proportion<-function(all_city_simsets, start_year=2025, end_year=2035)
{
  pp_inc_red = get.percentage.point.incidence.reduction(all_city_simsets, start_year, end_year)
  val = sapply(all_city_simsets, function(simset){
    sapply(simset@simulations, function(sim){
      get.proportions.engaged.by.laart(sim, end_year)
    })
  })
  val.oral = val[seq(1, nrow(val), 3), ]
  val.laart = val[seq(2, nrow(val), 3), ]
  val.resistant = val[seq(3, nrow(val), 3), ]
  columns = c("No int","25% Durable", "50% Durable", "25% Unsuppressed", "50% Unsuppressed", "25% Naive", "50% Naive", "25% Combined", "50% Combined") 
  row = c("Oral % 2035", "Laart % 2035", "Resistant % 2035")
  df = rbind(diag(cor(val.oral, pp_inc_red)),diag(cor(val.laart, pp_inc_red)))
  df = rbind(df, diag(cor(val.resistant, pp_inc_red)))
  colnames(df) = columns
  rownames(df) = row
  df
}

get.intervention.incidence.reduction<- function(allsimsets = allsimsets)
{
  values = sapply(allsimsets, function(simset){
    sapply(simset@simulations, function(sim){
      inc = project.absolute.incidence(sim, years = c(2025, 2035))
      (inc[1]-inc[2])/inc[1]
    })
  })
  print(apply(values, 2, mean))
  print(apply(values, 2, quantile, probs=c(0.025, 0.975)))
  
  print(apply(values-values[ ,1], 2, mean))
  print(apply(values-values[ ,1], 2, quantile, probs=c(0.025, 0.975)))
}
get.intervention.incidence.reduction(allsimsets)
percent.in.treatment.type.correlations = get.correlation.coefficient.per.2035.treatment.type.proportion(allsimsets)
print(percent.in.treatment.type.correlations)

