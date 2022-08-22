
source('code/source_code.R')
source('code/execution/starting_values/systematic_find_starting_values.R')

test.fn = create.test.starting.values.function(location=CLEVELAND.MSA, version='expanded_1.0')

# Original values 
test = test.fn()
simplot(test$sim)
simplot(test$sim, facet.by='risk')
simplot(test$sim, data.types=c('engagement','suppression','suppression.of.engaged','retention'))


# Increasing lost.or.mult increases proportion lost; increasing reengagement.or.mult increases re-engagement
test = test.fn(het.trate.1.mult = 1.5, het.trate.2.mult = 1,
               msm.trate.1.mult = 1.12, msm.trate.2.mult = .92,
               idu.trate.1.mult = 3, idu.trate.2.mult = 3,
               supp.or.mult = 1, supp.or.slope.mult = 2,
               reengagement.or.mult = 4,
               lost.or.mult = 3, lost.or.slope.mult = 1)

# time 0 2000; time 1 2010; time 2 2020

simplot(test$sim)
simplot(test$sim, facet.by='risk')
simplot(test$sim, data.types=c('engagement','suppression','suppression.of.engaged','retention'))


#saves in the ending hiv/start values/expanded 1.0 folder; don't push until I like the starting values
save.starting.values(sim.and.params = test) 

