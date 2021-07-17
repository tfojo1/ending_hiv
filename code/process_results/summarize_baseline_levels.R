
if (1==2)
{
    baseline.prep = get.baseline.levels('prep', dir='mcmc_runs/quick_simsets/')
    write.csv(make.pretty.baseline.table(baseline.prep, digits=1, pct=T), file='../Manuscripts/manuscript_1/Annals Submission/revision 1/tables/baseline_prep.csv')
    
    baseline.testing = get.baseline.levels('testing', dir='mcmc_runs/quick_simsets/')
    write.csv(make.pretty.baseline.table(baseline.testing, digits=2, pct=F), file='../Manuscripts/manuscript_1/Annals Submission/revision 1/tables/baseline_testing.csv')
    
    baseline.suppression = get.baseline.levels('suppression', dir='mcmc_runs/quick_simsets/')
    write.csv(make.pretty.baseline.table(baseline.suppression, digits=1, pct=T), file='../Manuscripts/manuscript_1/Annals Submission/revision 1/tables/baseline_suppression.csv')
    
}

make.extract.baseline.function <- function(name='prep',
                                           years=c(2020,2025))
{
    if (name=='prep')
        extract.fn = extract.prep.coverage
    else if (name=='suppression')
        extract.fn = function(sim,...){extract.suppression(sim, continuum=sim$diagnosed.continuum.states, ...)}
    else if (name=='testing')
        extract.fn = extract.testing.rates
    else
        stop("name must be either 'prep', 'suppression', or 'testing'")
    
    
    if (name=='suppression')
    {
        idu.risks = c('active_IDU','IDU_in_remission')
        non.idu.risks = c('never_IDU')
        include.hiv.positive=T
        include.hiv.negative=F
    }
    else
    {
        idu.risks = c('active_IDU')
        non.idu.risks = c('never_IDU','IDU_in_remission')
        include.hiv.positive=F
        include.hiv.negative=T
    }
    
    function(sim)
    {
        ybhm.numerator = rowSums(extract.fn(sim, years=years, races=c('black','hispanic'), sexes='msm', 
                                            keep.dimensions=c('year','age'), use.cdc.categorizations = F, per.population = NA)[,1:2])
        ybhm.denominator = extract.population.subset(sim, years=years, race=c('black','hispanic'), sexes='msm', ages=1:2, 
                                                     keep.dimensions='year',
                                                     continuum = sim$diagnosed.continuum.states,
                                                     include.hiv.negative = include.hiv.negative,
                                                     include.hiv.positive = include.hiv.positive)
        
        msm.numerator = extract.fn(sim, years=years, sexes='msm', risks=non.idu.risks,
                                  keep.dimensions='year', use.cdc.categorizations = F, per.population = NA)
        
        idu.numerator = extract.fn(sim, years=years, risks=idu.risks,
                       keep.dimensions='year', use.cdc.categorizations = F, per.population = NA)
        
        
        msm.denominator = extract.population.subset(sim, years=years, sexes='msm', risks=non.idu.risks,
                                                   keep.dimensions='year', use.cdc.categorizations = F,
                                                   continuum = sim$diagnosed.continuum.states,
                                                   include.hiv.negative = include.hiv.negative,
                                                   include.hiv.positive = include.hiv.positive)
        
        idu.denominator = extract.population.subset(sim, years=years, risks=idu.risks,
                                      keep.dimensions='year', use.cdc.categorizations = F,
                                      continuum = sim$diagnosed.continuum.states,
                                      include.hiv.negative = include.hiv.negative,
                                      include.hiv.positive = include.hiv.positive)
        
        
        het.numerator = extract.fn(sim, years=years, sexes=c('female','heterosexual_male'),
                         risks=non.idu.risks,
                         keep.dimensions='year', use.cdc.categorizations = F, per.population=NA)
        
        het.denominator = extract.population.subset(sim, years=years, sexes=c('female','heterosexual_male'),
                                   risks=non.idu.risks,
                                   keep.dimensions='year',
                                   include.hiv.negative = include.hiv.negative,
                                   include.hiv.positive = include.hiv.positive)
        
        
        c(ybhm.numerator = ybhm.numerator,
          non.ybh.mi.numerator = (msm.numerator + idu.numerator - ybhm.numerator),
          msm.numerator = msm.numerator,
          het.numerator = het.numerator,
              
          ybhm.denominator = ybhm.denominator, 
          non.ybh.mi.denominator = (msm.denominator + idu.denominator - ybhm.denominator),
          msm.denominator = msm.denominator,
          het.denominator = het.denominator)
    }
}

get.baseline.levels <- function(name=c('prep','testing','suppression')[1],
                                   dir='mcmc_runs/full_simsets/',
                                   msas=TARGET.MSAS,
                                   summary.stat = mean,
                                   interval.coverage=0.95,
                                   calculate.total = T,
                                n.sim=N.SIM)
{
    # Get raw numbers
    raw.baseline = get.raw.values.one.intervention(dir=dir,
                                                   msas=msas,
                                                   fn=make.extract.baseline.function(name),
                                                   n.values=8,
                                                   n.sim=n.sim)
    
    # Add total
    if (calculate.total)
    {
        dim.names = dimnames(raw.baseline)
        dim.names[['location']] = c(dim.names[['location']], 'Total')
        
        old.raw = raw.baseline
        raw.baseline = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
        raw.baseline[,,setdiff(dim.names$location, 'Total')] = old.raw
        raw.baseline[,,'Total'] = rowSums(old.raw, dims=2, na.rm = T)
    }
    
    dim.names = dimnames(raw.baseline)
    value.names = dim.names[['value']]
    value.years = gsub(".*\\.([0-9][0-9][0-9][0-9])$", "\\1", value.names)
    denominator.mask = grepl('denominator', value.names)
    catg = gsub("^(.*)\\.[a-z]+\\.[0-9]+$", "\\1", value.names)
    
    dim.names[['value']] = paste0(catg[denominator.mask], ".", value.years[denominator.mask])
    
    fraction.baseline = raw.baseline[!denominator.mask,,] / raw.baseline[denominator.mask,,]
    dim(fraction.baseline) = sapply(dim.names, length)
    dimnames(fraction.baseline) = dim.names
    
    alpha = (1-interval.coverage)/2
    rv=list(estimates=apply(fraction.baseline, c('location','value'), summary.stat, na.rm=T),
            ci.lower=apply(fraction.baseline, c('location','value'), quantile, probs=alpha, na.rm=T),
            ci.upper=apply(fraction.baseline, c('location','value'), quantile, probs=1-alpha, na.rm=T)
    )
    
    rv
}

make.pretty.baseline.table <- function(baseline.levels,
                                 digits=1,
                                 pct=T)
{
    if (pct)
    {
        mult = 100
        suffix = '%'
    }
    else
    {
        mult = 1
        suffix = ''
    }
    
    baseline.levels$estimates = format(round(mult * baseline.levels$estimates, digits=digits), nsmall=digits)
    baseline.levels$ci.lower = format(round(mult * baseline.levels$ci.lower, digits=digits), nsmall=digits)
    baseline.levels$ci.upper = format(round(mult * baseline.levels$ci.upper, digits=digits), nsmall=digits)
    
    rv = paste0(baseline.levels$estimates, suffix, " [",
                baseline.levels$ci.lower, " - ", baseline.levels$ci.upper, "]")
    
    dim(rv) = dim(baseline.levels$estimates)
    dimnames(rv) = dimnames(baseline.levels$estimates)
    
    rv
}