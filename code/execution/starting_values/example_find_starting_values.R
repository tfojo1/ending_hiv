
source('code/source_code.R')
source('code/execution/starting_values/systematic_find_starting_values.R')

test.fn = create.test.starting.values.function(location=LA.MSA, version='expanded_1.0')

# Original values 
test = test.fn(lost.or.mult = 1, reengagement.or.mult=1, lost.or.slope.mult = 1)
simplot(test$sim)
simplot(test$sim, facet.by='risk')
simplot(test$sim, data.types=c('engagement','suppression','suppression.of.engaged','retention'))


# Increasing lost.or.mult increases proportion lost; increasing reengagement.or.mult increases re-engagement
test = test.fn(msm.trate.1.mult = 0.97, msm.trate.2.mult = 0.91, 
               het.trate.mult = 1.1, het.trate.2.mult = 1.1,
               supp.or.mult = 1.3, lost.or.mult = 1.15, 
               reengagement.or.mult = 1.5) 
# time 0 2000; time 1 2010; time 2 2020

simplot(test$sim)
simplot(test$sim, facet.by='risk')
simplot(test$sim, data.types=c('engagement','suppression','suppression.of.engaged','retention'))


#saves in the ending hiv/start values/expanded 1.0 folder; don't push until I like the starting values
save.starting.values(sim.and.params = test) 

# Baltimore - needed to decrease retention; maybe decrease cases in later time period
# lost.or.mult = 2.25, reengagement.or.mult=4,supp.or.slope.mult = 1.2

# Miami - need to increase suppression; maybe decrease retention slope
# supp.or.mult = 1.4

# Atlanta - need to increase suppression; maybe decrease retention slope
# supp.or.mult = 1.35, supp.or.slope.mult = 1.1

# LA - need to decrease cases (msm in later period); increase suppression; decrease retention
# msm.trate.1.mult = 0.97, msm.trate.2.mult = 0.91, 
# het.trate.mult = 1.1, het.trate.2.mult = 1.1,
# supp.or.mult = 1.3, lost.or.mult = 1.15, 
# reengagement.or.mult = 1.5

