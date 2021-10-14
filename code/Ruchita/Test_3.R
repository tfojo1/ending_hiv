source("code/source_code.R") 


prep_10 = create.intervention.unit(type = "prep", start.year = 2023, rates = .1, years = 2027)
prep_test = create.intervention.unit(type = "prep", start.year = 2023, rates = c(.1,.2), years = c(2027,2030))
prep_RR = create.intervention.unit(type = "rr.prep", start.year = 2023, rates = .34, years = 2027, apply.function = "multiplier")

#Create Interventions

#oral prep 10%
int_oral_10 = create.intervention(ALL.MSM, prep_10)
#injectible prep 10% 
int_inj_10 = create.intervention(ALL.MSM, prep_10, prep_RR)

#intervention = create.intervention(WHOLE.POPULATION, prep_10, prep_RR)

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


#simset = subset.simset(simset, 1:50)
source("code/plots.R")
plot.calibration(simset)


#try a variable intervention

prep.variable.rr = create.intervention.unit(type = "rr.prep", start.year = 2023, rates = 'prep.rr', years = 2027, apply.function = "multiplier")
intervention.variable = create.intervention(WHOLE.POPULATION, prep_10, prep.variable.rr,
                                            Logitnormal.Distribution(var.name='prep.rr', meanlogit = 0, sdlogit = 1))
simset.from.variable = run.simset.intervention(simset, intervention.variable, run.to.year = 2030)
