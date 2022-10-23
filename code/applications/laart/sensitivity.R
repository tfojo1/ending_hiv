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
load("miami.RData")

values = sapply(allsimsets, function(simset){
  sapply(simset@simulations, function(sim){
    inc = project.absolute.incidence(sim, years = c(2025, 2035))
    (inc[1]-inc[2])/inc[1]
  })
})

newvalues = values-values[,1]

correlations = 
  sapply(LAART.PARAMETER.DISTRIBUTION@var.names, function(varname){
    sapply(allsimsets, function(simset){
      cor(newvalues, simset@parameters[, varname]) 
  })
})
correlations = correlations[1:9,]

print(array(c(correlations[3,],correlations[5,],correlations[7,],correlations[9,]), dim=c(13,4), list(LAART.PARAMETER.DISTRIBUTION@var.names, c('50% Durable', '50% Unsuppressed', '50% Naive', '50% Combined'))))


### SUMMARY FUNCTIONS ####
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

val = sapply(allsimsets, function(simset){
  sapply(simset@simulations, function(sim){
      get.proportions.engaged.by.laart(sim, 2035)
  })
})
val.oral = val[seq(1, nrow(val), 3), ]
val.laart = val[seq(2, nrow(val), 3), ]
val.resistant = val[seq(3, nrow(val), 3), ]
columns = c("No int","25% Durable", "50% Durable", "25% Unsuppressed", "50% Unsuppressed", "25% Naive", "50% Naive", "25% Combined", "50% Combined") 
row = c("Oral % 2035", "Laart % 2035", "Resistant % 2035")
df = rbind(diag(cor(val.oral, newvalues)),diag(cor(val.laart, newvalues)))
df = rbind(df, diag(cor(val.resistant, newvalues)))
colnames(df) = columns
rownames(df) = row
