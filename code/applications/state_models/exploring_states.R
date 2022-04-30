

TARGET.STATES = c('AL','LA','MS')

msas = dimnames(msa.surveillance$new.all)$location
states = lapply(msas, states.for.msa)
mask = sapply(states, function(sts){
    any(sapply(sts, function(st){
        any(st==TARGET.STATES)
    }))
})
sum(mask)

matching.msas = msas[mask]
msa.names(matching.msas)

msa.names(intersect(matching.msas, TARGET.MSAS))
