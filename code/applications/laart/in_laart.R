source('code/applications/laart/do_files/do_prepare_simsets_for_laart.R')
source('code/source_code.R')
SRC.DIRECTORY = file.path(SIMULATIONS.DIR, 'baseline_quick_expanded')
DST.DIRECTORY = file.path(SIMULATIONS.DIR, 'baseline_quick_laart')

source('code/processing/visualization/sim_plots.R')
source('code/settings_and_files/setup_versions.R')
source('code/applications/laart/laart_jheem_settings.R')
source('code/applications/laart/laart_parameters.R')
source('code/applications/laart/laart_parameter_mapping.R')
#source('code/applications/laart/laart_interventions.R')
load("baltimore.RData")


get.proportions.durable <- function(sim, years=2035)
{
  pop = do.extract.population.subset(sim, years=years, keep.dimensions=c('continuum'), include.hiv.negative = F)
  
  of.interest = pop[c('engaged_durably_suppressed','laart_durably_suppressed','resistant_durably_suppressed')]
  of.interest / sum(of.interest)
}

get.proportions.unsuppressed <- function(sim, years=2035)
{
  pop = do.extract.population.subset(sim, years=years, keep.dimensions=c('continuum'), include.hiv.negative = F)
  
  of.interest = pop[c('engaged_unsuppressed_failing','laart_unsuppressed','resistant_unsuppressed')]
  of.interest / sum(of.interest)
}

get.proportions.engaged <- function(sim, years=2035)
{
  pop = do.extract.population.subset(sim, years=years, keep.dimensions=c('continuum'), include.hiv.negative = F)
  
  of.interest = pop[get.settings.for.version('laart')$ENGAGED_STATES]
  of.interest / sum(of.interest)
}

get.proportions.engaged.by.laart <- function(sim, years=2035)
{
  pop = do.extract.population.subset(sim, years=years, keep.dimensions=c('continuum'), include.hiv.negative = F)
  
  engaged.states = get.settings.for.version('laart')$ENGAGED_STATES
  laart.states = c('laart_unsuppressed','laart_recently_suppressed', 'laart_durably_suppressed')
  resistant.states = c('resistant_unsuppressed','resistant_recently_suppressed', 'resistant_durably_suppressed')
  oral.states = setdiff(engaged.states, c(laart.states, resistant.states))
  
  c(oral=sum(pop[oral.states]), laart=sum(pop[laart.states]), resistant=sum(pop[resistant.states])) / sum(pop[engaged.states])
}

values = sapply(allsimsets, function(simset){
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
  print(df)
})

print(paste0("25% Durable Intervention mean people on laart in 2027: ", mean(values[[3]])))
print(paste0("25% Durable Intervention CI people on laart in 2027: ", list(quantile(values[[3]], probs=c(0.025, 0.975)))))

print(paste0("25% Durable Intervention mean people on laart in 2035: ", mean(values[[4]])))
print(paste0("25% Durable Intervention CI people on laart in 2035: ", list(quantile(values[[4]], probs=c(0.025, 0.975)))))

print(paste0("50% Durable Intervention mean people on laart in 2027: ", mean(values[[5]])))
print(paste0("50% Durable Intervention CI people on laart in 2027: ", list(quantile(values[[5]], probs=c(0.025, 0.975)))))

print(paste0("50% Durable Intervention mean people on laart in 2035: ", mean(values[[6]])))
print(paste0("50% Durable Intervention CI people on laart in 2035: ", list(quantile(values[[6]], probs=c(0.025, 0.975)))))
