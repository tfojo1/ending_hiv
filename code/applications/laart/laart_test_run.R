setwd("C:/Users/pree1/OneDrive - Johns Hopkins/Documents/ending_HIV/ending_hiv")
#-- LOAD UP STUFF --#
source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')
source('code/settings_and_files/setup_versions.R')
source('code/applications/laart/laart_jheem_settings.R')
source('code/applications/laart/laart_parameters.R')
source('code/applications/laart/laart_parameter_mapping.R')
source('code/applications/laart/laart_summary_functions.R')
source('code/applications/laart/laart_jheem_settings.R')
source('code/applications/laart/durable_intervention_optimizer.R')
source('code/applications/laart/laart_interventionsv2.R')


run.laart.interventions<- function(simset = simset, INTERVENTION.MANAGER = INTERVENTION.MANAGER.1.0, run.to.year = 2035, keep.years = 2015:2035)
{
  allsimsets = list()
  for (numint in 1:length(INTERVENTION.MANAGER[['intervention']])){
    int = run.simset.intervention(simset, 
                                  intervention=INTERVENTION.MANAGER[["intervention"]][[numint]],
                                  run.to.year=2035,
                                  keep.years=2015:2035)
    allsimsets = append(allsimsets, int)
    print(numint)
  }
  return(allsimsets)
}
get.final.treatment.group.distribution<-function(allsimsets, INTERVENTION.MANAGER = INTERVENTION.MANAGER.1.0, star.year = 2025, end.year = 2035, numint)
{
  int = allsimsets[[numint]]
  print(round(100*t(sapply(as.character((star.year-1):end.year), function(year){rowMeans(sapply(int@simulations, get.proportions.engaged.by.laart, years=as.numeric(year)))}))))
  columns = c("2027_laart_percentage","2035_laart_percentage") 
  df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
  colnames(df) = columns
  for (i in 1:int@n.sim){
    df[nrow(df) + 1,] = c(round(100*t(sapply(as.character((star.year-1):end.year), function(year){
      rowMeans(sapply(int@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
    })),1)['2027', 'laart'], round(100*t(sapply(as.character((star.year-1):end.year), function(year){
      rowMeans(sapply(int@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
    })),1)['2035', 'laart'])
  }
  hist(df$`2027_laart_percentage`)
  hist(df$`2035_laart_percentage`)
}

run.multiple.laart.interventions<- function(SRC.DIRECTORY = SRC.DIRECTORY,  DST.DIRECTORY = DST.DIRECTORY, INTERVENTION.MANAGER = INTERVENTION.MANAGER.1.0, run.to.year = 2035, keep.years = 2015:2035, LOCATIONS = LOCATIONS)
{
  for (loc in LOCATIONS)
  {
    print(paste0("Simulating ", msa.names(loc), " (", loc, ")"))
    src.filename = get.full.filename(location=loc, version='laart')
    load(file.path(SRC.DIRECTORY, src.filename))
    get.simset.file.version(simset)
    optim.simset = subset.simset(simset, simset@n.sim-9:0)
    durable.laart.25.parameters = get.durable.25.intervention.parameters(optim.simset = optim.simset)
    durable.laart.50.parameters = get.durable.50.intervention.parameters(optim.simset = optim.simset)
    INTERVENTION.MANAGER.1.0<- create.laart.interventions(durable.laart.25.parameters = durable.laart.25.parameters, durable.laart.50.parameters = durable.laart.50.parameters)
    allsimsets = run.laart.interventions(simset = simset, INTERVENTION.MANAGER = INTERVENTION.MANAGER.1.0, run.to.year = 2035, keep.years = 2015:2035)
    save(allsimsets, file = file.path(DST.DIRECTORY, paste0(loc, "_results.Rdata")))
  }
}

SRC.DIRECTORY = file.path(SIMULATIONS.DIR, 'baseline_quick_laart')
DST.DIRECTORY = file.path(SIMULATIONS.DIR, 'laart_results')
LOCATIONS = c(ATLANTA.MSA, BALTIMORE.MSA, LA.MSA, MIAMI.MSA)
run.multiple.laart.interventions(SRC.DIRECTORY = SRC.DIRECTORY,  DST.DIRECTORY = DST.DIRECTORY, INTERVENTION.MANAGER = INTERVENTION.MANAGER.1.0, run.to.year = 2035, keep.years = 2015:2035, LOCATIONS = LOCATIONS)