
source("code/source_code.R") 


prep_10 = create.intervention.unit(type = "prep", start.year = 2023, rates = .1, years = 2027)
prep_test = create.intervention.unit(type = "prep", start.year = 2023, rates = c(.1,.2), years = c(2027,2030))
prep_RR = create.intervention.unit(type = "rr.prep", start.year = 2023, rates = .34, years = 2027, apply.function = "multiplier")

#Create Interventions

#oral prep 10%
int_oral_10 = create.intervention(ALL.MSM, prep_10)
#injectible prep 10% 
int_inj_10 = create.intervention(ALL.MSM, prep_10, prep_RR)


directory = "mcmc_runs/quick_simsets/1.0_12420_full.Rdata"


prep_10_intervention <- function(directory){
  
  load(directory)
  
  num_sim = simset@n.sim #Number of simulations 
  
  simset = prepare.simset.for.interventions(simset)
  
  #incidence projection oral 10%
  simset_oral_10 = run.simset.intervention(simset, int_oral_10, run.to.year = 2030, keep.years = 2018:2030)
  #incidence projection injectible 10%
  simset_inj_10 = run.simset.intervention(simset, int_inj_10, run.to.year = 2030, keep.years = 2018:2030)
  
  
  delta_10_prep = matrix(nrow = 1, ncol = num_sim)
  
  for(a in 1:num_sim){
    sum_oral = sum(project.absolute.incidence(simset_oral_10@simulations[[a]], years = 2020:2030))
    sum_inj = sum(project.absolute.incidence(simset_inj_10@simulations[[a]], years = 2020:2030))
    delta_10_prep[1,a] = sum_inj-sum_oral
  }
  
  
  #mean difference in incidence 
  mean = mean(delta_10_prep)
  CI_high = sort(delta_10_prep)[.025*num_sim]
  CI_low = sort(delta_10_prep)[.975*num_sim]
  
  output = rbind(mean,CI_high,CI_low)
  
  return(output)
  
}


file.list = list.files(path = "mcmc_runs/quick_simsets/", full.names = TRUE, recursive = TRUE)

intervention_output = sapply(file.list,prep_10_intervention)


