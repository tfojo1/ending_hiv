
msas = dimnames(msa.surveillance$new.all)$location

#Alaska, Colorado, North Dakota, South Dakota, Idaho, Montana, Oregon, Utah, Washington, and Wyoming.

states.of.interest = c('AK','CO','ND','SD','ID','MT','OR','UT','WA','WY')
state.fips.to.name(state.abbreviation.to.fips(states.of.interest))

states.with.surveillance = states.for.msa(msas)


overlap.msas = lapply(states.of.interest, function(st){
    msa.mask = sapply(msas, function(msa){
        any(states.for.msa(msa)==st)
    })
    msas[msa.mask]
})

overlap.msa.names = lapply(overlap.msas, function(ov){
    unlist(msa.names(ov))
})
names(overlap.msa.names) = states.of.interest
overlap.msa.names

states.of.interest[sapply(overlap.msa.names, length)>0]
