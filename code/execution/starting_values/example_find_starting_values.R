
source('code/source_code.R')
source('code/execution/starting_values/systematic_find_starting_values.R')

test.fn = create.test.starting.values.function(location=ATLANTA.MSA, version='expanded_1.0')

# Original values 
test = test.fn(lost.or.mult = 1, reengagement.or.mult=1, lost.or.slope.mult = 1)
simplot(test$sim, facet.by='risk')
simplot(test$sim, data.types=c('engagement','suppression','suppression.of.engaged','retention'))
simplot(test$sim)

# Increasing lost.or.mult increases proportion lost; increasing reengagement.or.mult increases re-engagement
test = test.fn(lost.or.mult = 1.15, 
               reengagement.or.mult=1.85, 
               supp.or.slope.mult = 1.1, het.trate.1.mult = 1.02,
               het.trate.2.mult = 0.97, 
               msm.trate.1.mult = 0.98, msm.trate.2.mult = 0.99) 
# time 0 2000; time 1 2010; time 2 2020

simplot(test$sim, facet.by='risk')
simplot(test$sim)
simplot(test$sim, data.types=c('engagement','suppression','suppression.of.engaged','retention'))


#saves in the ending hiv/start values/expanded 1.0 folder; don't push until I like the starting values
save.starting.values(sim.and.params = test) 

# Baltimore
# lost.or.mult = 3.25, reengagement.or.mult=4.25

# Miami
# lost.or.mult = 1.05, reengagement.or.mult=2 

# Atlanta
# lost.or.mult = 1.1, reengagement.or.mult=1.5

# LA
# lost.or.mult = 1.15, reengagement.or.mult=1.85, supp.or.slope.mult = 1.1, 
# het.trate.1.mult = 1.02,het.trate.2.mult = 0.97, msm.trate.1.mult = 0.98, msm.trate.2.mult = 0.99