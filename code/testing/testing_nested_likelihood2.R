

fn = create.nested.likelihood(data.type='diagnosed',
                              msa='12580',
                              msa.surveillance=msa.surveillance,
                              state.surveillance=state.surveillance,
                              county.surveillance=county.surveillance,
                              years=2008:2019,
                              observation.error.fn = function(years, num){num*.065})

fn(sim)

fn = create.nested.likelihood(data.type='suppression',
                                     msa='12580',
                                     msa.surveillance=msa.surveillance,
                                     state.surveillance=state.surveillance,
                                     county.surveillance=county.surveillance,
                                     years=2008:2019,
                         observation.error.fn = function(years, num){num*.065})

fn(sim, debug=T)


test.data.types = c('retention','diagnosed','suppression', 'engagement', 'linkage')
test.data.types.for.n = c('prevalence','prevalence','prevalence','prevalence','new')
names(test.data.types.for.n) = test.data.types

y=sapply(test.data.types, function(data.type){
    extra.county = get.extra.msa.county.p.bias.and.variance(data.type,
                                                            test.data.types.for.n[data.type],
                                                            msa.surveillance,
                                                            county.surveillance,
                                                            state.surveillance)
    in.county = get.in.msa.county.p.bias.and.variance(data.type,
                                                      test.data.types.for.n[data.type],
                                              msa.surveillance,
                                              county.surveillance)
    state = get.state.p.bias.and.variance(data.type,
                                          test.data.types.for.n[data.type],
                                          msa.surveillance,
                                          state.surveillance)
    
    c(in.county.sd = sqrt(in.county$variance),
      extra.county.sd = sqrt(extra.county$variance),
      state.sd = sqrt(state$variance),
      extra.county.bias = extra.county$bias,
      state.bias = state$bias,
      in.county.n = in.county$n,
      extra.county.n = extra.county$n,
      state.n = state$n
      )
    
})
round(y,3)



get.extra.msa.county.p.bias.and.variance('suppression',
                                         'prevalence',
                                         msa.surveillance,
                                         county.surveillance,
                                         state.surveillance)






