

TARGET.STATES = c('AL','LA','MS')

if (1==2)
{

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
}


summarize.surveillance.by.urbanicity <- function(state, 
                                                 data.type,
                                                 years=2019,
                                                 report.fractions=T,
                                                 report.urban.suburban.rural=T)
{
    counties = counties.for.state(state)
    
    if (report.urban.suburban.rural)
        cty.urb = c('urban','urban','urban',
                    'suburban','suburban','suburban','suburban',
                    'rural','rural')[county.urbanization(counties, return.as.text = F)]
    else
        cty.urb = county.urbanization(counties)
    
    urbanicities = unique(cty.urb)
    
    rv = sapply(urbanicities, function(urb){
        sum(get.surveillance.data(county.surveillance,
                              location.codes = counties[cty.urb==urb], 
                              years = years,
                              aggregate.years=T,
                              aggregate.locations=F,
                              na.rm = T))
    })
    
    if (report.fractions)
        rv = rv / sum(rv)
    
    rv
}