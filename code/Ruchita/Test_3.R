source("code/source_code.R") 

#
prep_10 = create.intervention.unit(type = "prep", start.year = 2023, rates = .1, years = 2027)
prep_test = create.intervention.unit(type = "prep", start.year = 2023, rates = c(.1,.2), years = c(2027,2030))
prep_RR = create.intervention.unit(type = "rr.prep", start.year = 2023, rates = .5, years = 2027, apply.function = "multiplier")

intervention = create.intervention(WHOLE.POPULATION, prep_10, prep_RR)

load("mcmc_runs/full_simsets/1.0_12060_full.Rdata")
simset = subset.simset(simset, 1:50)
source("code/plots.R")
plot.calibration(simset)

simset = prepare.simset.for.interventions(simset)
simset_2 = run.simset.intervention(simset, intervention, run.to.year = 2030)


#try a variable intervention

prep.variable.rr = create.intervention.unit(type = "rr.prep", start.year = 2023, rates = 'prep.rr', years = 2027, apply.function = "multiplier")
intervention.variable = create.intervention(WHOLE.POPULATION, prep_10, prep.variable.rr,
                                            Logitnormal.Distribution(var.name='prep.rr', meanlogit = 0, sdlogit = 1))
simset.from.variable = run.simset.intervention(simset, intervention.variable, run.to.year = 2030)
