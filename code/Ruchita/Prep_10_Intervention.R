#directory parameter 
directory = "mcmc_runs/quick_simsets/1.0_12420_full.Rdata"
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
CI_high = mean + 1.96*(sd(delta_10_prep)/(sqrt(num_sim))) 
CI_low = mean - 1.96*(sd(delta_10_prep)/(sqrt(num_sim))) 
