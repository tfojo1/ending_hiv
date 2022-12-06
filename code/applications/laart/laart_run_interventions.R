run.laart.interventions<- function(simset, INTERVENTION.MANAGER = INTERVENTION.MANAGER.1.0, run.to.year = 2035, keep.years = 2015:2035)
{
  allsimsets = list()
  for (numint in 1:length(INTERVENTION.MANAGER.1.0[['intervention']])){
    int = run.simset.intervention(simset, 
                                  intervention=INTERVENTION.MANAGER[["intervention"]][[numint]],
                                  run.to.year=2035,
                                  keep.years=2015:2035)
    allsimsets = list.append(allsimsets, int)
  }
  return(allsimsets)
}
