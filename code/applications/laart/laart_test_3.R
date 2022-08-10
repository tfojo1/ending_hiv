
if (1==2)
    setwd('../../../Ending HIV/Ending_HIV/')


source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')
load('simulations/laart_test/12580.Rdata')
source('code/settings_and_files/setup_versions.R')
source('code/applications/laart/laart_jheem_settings.R')
source('code/applications/laart/laart_parameters.R')
source('code/applications/laart/laart_parameter_mapping.R')
source('code/applications/laart/laart_interventions.R')

simset = subset.simset(simset, 1:5)
#simplot(simset)

prepared = prepare.simset.for.intervention(simset, update.version = 'laart')


noint = run.simset.intervention(prepared, NO.INTERVENTION, run.to.year = 2035)

INTERVENTION.MANAGER.2 = create.laart.interventions()
int.durable.10 = run.simset.intervention(prepared, INTERVENTION.MANAGER.2$intervention$durable.laart.10py, run.to.year = 2035)
int.durable.25 = run.simset.intervention(prepared, INTERVENTION.MANAGER.2$intervention$durable.laart.25py, run.to.year = 2035)
int.durable.50 = run.simset.intervention(prepared, INTERVENTION.MANAGER.2$intervention$durable.laart.50py, run.to.year = 2035)

int.unsuppressed.10 = run.simset.intervention(prepared, INTERVENTION.MANAGER.2$intervention$unsuppressed.laart.10py, run.to.year = 2035)
int.unsuppressed.25 = run.simset.intervention(prepared, INTERVENTION.MANAGER.2$intervention$unsuppressed.laart.25py, run.to.year = 2035)
int.unsuppressed.50 = run.simset.intervention(prepared, INTERVENTION.MANAGER.2$intervention$unsuppressed.laart.50py, run.to.year = 2035)

comparesim<-function(noint, int){
  simplot(noint, int)
  simplot(noint, int, data.types='suppression')
  
  
  # the below is indexed [year, simulation]
  suppression.delta = sapply(1:int@n.sim, function(i){
    sim.noint = noint@simulations[[i]]
    sim.int = int@simulations[[i]]
    
    (extract.suppression(sim.int) - extract.suppression(sim.noint) ) / extract.suppression(sim.noint)
  })
  
  print(paste0("On average, the intervention changed suppression by ",
               round(mean(suppression.delta['2035',]), 3), '% from what it would have been in 2035'))
  
  #suppression.delta gives difference in suppression per year
  
  incidence.delta = sapply(1:int@n.sim, function(i){
    sim.noint = noint@simulations[[i]]
    sim.int = int@simulations[[i]]
    
    (project.absolute.incidence(sim.int) - project.absolute.incidence(sim.noint) ) / project.absolute.incidence(sim.noint)
  })
  
  print(paste0("On average, the intervention changed incidence by ",
               round(mean(incidence.delta['2035',]), 3), '% from what it would have been in 2035'))
  
  
  engagement.delta = sapply(1:int@n.sim, function(i){
    sim.noint = noint@simulations[[i]]
    sim.int = int@simulations[[i]]
    
    (do.extract.engagement(sim.int) - do.extract.engagement(sim.noint) ) / do.extract.engagement(sim.noint)
  })
  
  print(paste0("On average, the intervention changed engagement by ",
               round(mean(engagement.delta['2035',]), 3), '% from what it would have been in 2035'))
}

sensanalysis<-function (noint, int){
  suppression.delta = sapply(1:int@n.sim, function(i){
    sim.noint = noint@simulations[[i]]
    sim.int = int@simulations[[i]]
    (extract.suppression(sim.int) - extract.suppression(sim.noint) ) / extract.suppression(sim.noint)
  })
  
  outcome=suppression.delta["2035",]
  
  mat = matrix(, nrow = 11, ncol = 3)
  for (i in 1:11) {
    parameter = int@parameters[,int@parameter.names[202+i]]
    mat[i,] = c(int@parameter.names[202+i], cor(parameter, outcome), cor.test(parameter, outcome)$p.value)
  }
  return(mat)
}


comparesim(noint, int.durable.10)
mat.durable.10 = sensanalysis(noint, int.durable.10)


comparesim(noint, int.durable.25)
mat.durable.25 = sensanalysis(noint, int.durable.25)

comparesim(noint, int.durable.50)
mat.durable.50 = sensanalysis(noint, int.durable.50)


comparesim(noint, int.unsuppressed.10)
mat.unsuppressed.10 = sensanalysis(noint, int.unsuppressed.10)


comparesim(noint, int.unsuppressed.25)
mat.unsuppressed.25 = sensanalysis(noint, int.unsuppressed.25)

comparesim(noint, int.unsuppressed.50)
mat.unsuppressed.50 = sensanalysis(noint, int.unsuppressed.50)


# some interventions
# implemented.year = 2028
# start.year = 2025
# 
# rate.10 = -log(1-0.1)/(implemented.year-start.year)
# u.DURABLE.LAART.10 = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
#                                               start.year = start.year,
#                                               years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
#                                               rates = expression(c(rate.10, rate.10, 0.1 / (1-.1) * laart.discontinuation)),
#                                               scale = 'rate',
#                                               apply.function = 'absolute',
#                                               expression.parameters = list(rate.10=rate.10))
# 
# intervention = create.intervention(WHOLE.POPULATION, u.DURABLE.LAART.10)
# INTERVENTION.MANAGER.1.0 = register.intervention(intervention,
#                                                  code = 'durable.laart.10_25.28',
#                                                  name = 'start 10% of durably suppressed PWH on LAART',
#                                                  manager = INTERVENTION.MANAGER.1.0)

# now with 50%

# rate.50 = -log(1-0.5)/(implemented.year-start.year)
# u.DURABLE.LAART.50 = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
#                                               start.year = start.year,
#                                               years = c(start.year+0.0001, implemented.year, implemented.year+0.001),
#                                               rates = expression(c(rate.50, rate.50, 0.5 / (1-.5) * laart.discontinuation)),
#                                               scale = 'rate',
#                                               apply.function = 'absolute',
#                                               expression.parameters = list(rate.50=rate.50))
# 
# intervention = create.intervention(WHOLE.POPULATION, u.DURABLE.LAART.50)
# INTERVENTION.MANAGER.2 = register.intervention(intervention,
#                                                  code = 'durable.laart.50_25.28',
#                                                  name = 'start 50% of durably suppressed PWH on LAART',
#                                                 manager = INTERVENTION.MANAGER.2)
# int.durable.50 = run.simset.intervention(prepared, INTERVENTION.MANAGER.2$intervention$durable.laart.50_25.28, run.to.year = 2035)



# runsim<- function(sim_num){
#   intervention = INTERVENTION.MANAGER.2$intervention[c(sim_num)]
#   int = run.simset.intervention(prepared, intervention, run.to.year = 2035)
#   
#   simplot(noint, int)
#   simplot(noint, int, data.types='suppression')
#   
#   
#   # the below is indexed [year, simulation]
#   suppression.delta = sapply(1:int@n.sim, function(i){
#     sim.noint = noint@simulations[[i]]
#     sim.int = int@simulations[[i]]
#     
#     (extract.suppression(sim.int) - extract.suppression(sim.noint) ) / extract.suppression(sim.noint)
#   })
#   
#   print(paste0("On average, the intervention changed suppression by ",
#                round(mean(suppression.delta['2035',]), 3), '% from what it would have been in 2035'))
#   
#   #suppression.delta gives difference in suppression per year
#   
#   incidence.delta = sapply(1:int@n.sim, function(i){
#     sim.noint = noint@simulations[[i]]
#     sim.int = int@simulations[[i]]
#     
#     (project.absolute.incidence(sim.int) - project.absolute.incidence(sim.noint) ) / project.absolute.incidence(sim.noint)
#   })
#   
#   print(paste0("On average, the intervention changed incidence by ",
#                round(mean(incidence.delta['2035',]), 3), '% from what it would have been in 2035'))
#   
#   
#   engagement.delta = sapply(1:int@n.sim, function(i){
#     sim.noint = noint@simulations[[i]]
#     sim.int = int@simulations[[i]]
#     
#     (do.extract.engagement(sim.int) - do.extract.engagement(sim.noint) ) / do.extract.engagement(sim.noint)
#   })
#   
#   print(paste0("On average, the intervention changed engagement by ",
#                round(mean(engagement.delta['2035',]), 3), '% from what it would have been in 2035'))
# }

#run 7 interventions; construct table on reduction of incidence from 2025 to 2035 (mean and ci); baltimore will be row; 
#sensitivity analysis
#outcome=suppression.delta["2035",]
#int@parameter.names gives parameter of laart
# parameter = int@parameters[,"laart.discontinuation"]
# qplot(parameter,outcome)
#calculate correlation coefficient