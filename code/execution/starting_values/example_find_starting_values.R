
source('code/source_code.R')
source('code/execution/starting_values/systematic_find_starting_values.R')

test.fn = create.test.starting.values.function(location=MIAMI.MSA, version='expanded_1.0')

test = test.fn(lost.or.mult = 0.75, reengagement.or.mult=3)
simplot(test$sim, data.types=c('engagement','suppression','suppression.of.engaged','retention'))
simplot(test$sim, facet.by='risk')
