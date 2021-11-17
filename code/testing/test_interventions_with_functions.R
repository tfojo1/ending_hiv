

unit.fn = create.intervention.unit('suppression',
                                   start.year=2020,
                                   end.year=Inf,
                                   years=c(2021,2022),
                                   rates=function(x){x[c('hi','yo')]})
resolved = resolve.intervention.unit(unit.fn, c('hi'=1,'yo'=2))


to.fail = create.intervention.unit('suppression',
                                       start.year=2020,
                                       end.year=Inf,
                                       years=c(2021,2022),
                                       rates=function(x){x['hi']})

resolved = resolve.intervention.unit(to.fail, c('hi'=1,'yo'=2))



unit.expr = create.intervention.unit('suppression',
                                   start.year=2020,
                                   end.year=Inf,
                                   years=c(2021,2022),
                                   rates=expression(c(hi,yo)))
resolved = resolve.intervention.unit(unit.expr, c('hi'=1,'yo'=2))
resolved$rates


unit.expr2 = create.intervention.unit('suppression',
                                     start.year=2020,
                                     end.year=Inf,
                                     years=c(2021,2022),
                                     rates=c(expression(hi), expression(yo)))
resolved = resolve.intervention.unit(unit.expr2, c('hi'=1,'yo'=2))
resolved$rates

unit.char = create.intervention.unit('suppression',
                                     start.year=2020,
                                     end.year=Inf,
                                     years=c(2021,2022),
                                     rates=c('hi',10))
resolved = resolve.intervention.unit(unit.char, c('hi'=1,'yo'=2))
intervention.unit.is.resolved(resolved)
resolved$rates
