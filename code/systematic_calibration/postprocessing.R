
extract.suppression <- function(sim,
                                years=sim$years,
                                keep.dimensions='year',
                                per.population=1,
                                ages=NULL,
                                races=NULL,
                                subpopulations=NULL,
                                sexes=NULL,
                                risks=NULL,
                                continuum='diagnosed',
                                cd4=NULL,
                                hiv.subsets=NULL,
                                use.cdc.categorizations=F)
{
    raw.suppression.rates = calculate.suppression(attr(sim, 'components'))
    
    interpolated.rates = interpolate.parameters(values=raw.suppression.rates$rates,
                                                value.times=raw.suppression.rates$times,
                                                desired.times = years)
    
    stratified.dim.names = c(list(year=as.character(years)), dimnames(interpolated.rates[[1]]))

    stratified.suppression.arr = t(sapply(interpolated.rates, function(r){r}))
    dim(stratified.suppression.arr) = sapply(stratified.dim.names, length)
    dimnames(stratified.suppression.arr) = stratified.dim.names
    
    all.dimension.names = names(stratified.dim.names)
#    dimensions.length.geq.1.names = all.dimension.names[sapply(stratified.dim.names, length)>1]
#    if (setequal()

    prevalence = extract.population.subset(sim, years=years, keep.dimensions = all.dimension.names,
                                    ages=ages, races=races, subpopulations=subpopulations,
                                    sexes=NULL, risks=NULL,
                                    continuum=continuum, cd4=cd4, hiv.subsets=hiv.subsets,
                                    include.hiv.negative = F)

    numerators = as.numeric(prevalence) *
        as.numeric(stratified.suppression.arr[,dimnames(prevalence)[['age']],dimnames(prevalence)[['race']],
                                   dimnames(prevalence)[['subpopulation']],dimnames(prevalence)[['sex']],
                                   dimnames(prevalence)[['risk']],dimnames(prevalence)[['continuum']],
                                   dimnames(prevalence)[['cd4']],dimnames(prevalence)[['hiv.subset']]])
    dim(numerators) = dim(prevalence)
    dimnames(numerators) = dimnames(prevalence)
    
    if (use.cdc.categorizations && (any(keep.dimensions=='risk') || any(keep.dimensions=='sex')))
    {
        numerators = sum.suppression.arr.to.cdc(numerators)
        prevalence = sum.suppression.arr.to.cdc(prevalence)
    }
    
    if (!is.null(sexes) && !is.null(risks))
    {
        dim.names = dimnames(numerators)
        dim.names[['sex']] = sexes
        dim.names[['risk']] = risks
        
        numerators = numerators[,,,,sexes,risks,,,]
        prevalence = prevalence[,,,,sexes,risks,,,]
        
        dim(numerators) = sapply(dim.names, length)
        dimnames(numerators) = dim.names
        dim(prevalence) = sapply(dim.names, length)
        dimnames(prevalence) = dim.names
    }
    else if (!is.null(sexes))
    {
        dim.names = dimnames(numerators)
        dim.names[['sex']] = sexes
        
        numerators = numerators[,,,,sexes,,,,]
        prevalence = prevalence[,,,,sexes,,,,]
        
        dim(numerators) = sapply(dim.names, length)
        dimnames(numerators) = dim.names
        dim(prevalence) = sapply(dim.names, length)
        dimnames(prevalence) = dim.names
    }
    else if (!is.null(risks))
    {
        dim.names = dimnames(numerators)
        dim.names[['risk']] = risks
        
        numerators = numerators[,,,,,risks,,,]
        prevalence = prevalence[,,,,,risks,,,]
        
        dim(numerators) = sapply(dim.names, length)
        dimnames(numerators) = dim.names
        dim(prevalence) = sapply(dim.names, length)
        dimnames(prevalence) = dim.names
    }
    
    rv = apply(numerators, keep.dimensions, sum) / apply(prevalence, keep.dimensions, sum)
    rv
}

sum.suppression.arr.to.cdc <- function(arr)
{
    dim.names = dimnames(arr)
  
    non.idu.states = 'never_IDU'
    idu.states = setdiff(dim.names[['risk']], non.idu.states)
    collapse.idu.dimensions = setdiff(names(dim.names), c('risk','sex'))
    
    dim.names[['sex']] = c('female','male')
    dim.names[['risk']] = c('msm','idu','msm_idu','heterosexual')
    
    rv = array(0, dim=sapply(dim.names,length), dimnames=dim.names)
    
    rv[,,,,'male','msm',,,] = arr[,,,,'msm','never_IDU',,,]
    rv[,,,,'male','msm_idu',,,] = arr[,,,,'msm','active_IDU',,,] + arr[,,,,'msm','IDU_in_remission',,,]
    rv[,,,,'male','idu',,,] = arr[,,,,'heterosexual_male','active_IDU',,,] + arr[,,,,'heterosexual_male','IDU_in_remission',,,]
    rv[,,,,'male','heterosexual',,,] = arr[,,,,'heterosexual_male','never_IDU',,,]
    rv[,,,,'female','idu',,,] = arr[,,,,'female','active_IDU',,,] + arr[,,,,'female','IDU_in_remission',,,]
    rv[,,,,'female','heterosexual',,,] = arr[,,,,'female','never_IDU',,,]
    
    rv
}

#A slow, general function
sum.arr.to.cdc <- function(arr)
{
    dim.names = dimnames(arr)
    
    non.idu.states = 'never_IDU'
    idu.states = setdiff(dim.names[['risk']], non.idu.states)
    collapse.idu.dimensions = setdiff(names(dim.names), c('risk','sex'))
    
    dim.names[['sex']] = c('female','male')
    dim.names[['risk']] = c('msm','idu','msm_idu','heterosexual')
    
    rv = array(0, dim=sapply(dim.names,length), dimnames=dim.names)

    access(rv, sex='male', risk='msm') = access(arr, sex='msm', risk='never_IDU')
    access(rv, sex='male', risk='msm_idu') = apply(access(arr, sex='msm', risk=idu.states, collapse.length.one.dimensions = F), collapse.idu.dimensions, sum)
    access(rv, sex='male', risk='idu') = apply(access(arr, sex='heterosexual_male', risk=idu.states, collapse.length.one.dimensions = F), collapse.idu.dimensions, sum)
    access(rv, sex='male', risk='heterosexual') = access(arr, sex='heterosexual_male', risk='never_IDU')
    access(rv, sex='female', risk='idu') = apply(access(arr, sex='female', risk=idu.states, collapse.length.one.dimensions = F), collapse.idu.dimensions, sum)
    access(rv, sex='female', risk='heterosexual') = access(arr, sex='female', risk='never_IDU')
    
    rv
}